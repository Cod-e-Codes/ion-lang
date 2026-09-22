use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::{ParseError, Parser};
use crate::tc::ModuleExports;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

pub struct Compiler {
    modules: HashMap<PathBuf, Program>,
    visiting: HashSet<PathBuf>,
    module_exports: HashMap<String, ModuleExports>, // Maps import alias to exports
    stdlib_paths: Vec<PathBuf>,
    project_root: Option<PathBuf>,
    next_expr_id: u32,
}

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
    ImportCycle { path: PathBuf },
    FileNotFound { path: PathBuf },
    IoError(String),
    InvalidUtf8 { path: PathBuf, valid_up_to: usize },
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::ParseError(err) => write!(f, "{}", err),
            CompileError::ImportCycle { path } => {
                write!(f, "import cycle detected involving {}", path.display())
            }
            CompileError::FileNotFound { path } => {
                write!(f, "file not found: {}", path.display())
            }
            CompileError::IoError(msg) => write!(f, "IO error: {}", msg),
            CompileError::InvalidUtf8 { path, valid_up_to } => write!(
                f,
                "invalid UTF-8 in source file {} (at byte {})",
                path.display(),
                valid_up_to
            ),
        }
    }
}

/// True when the merged program defines an entry point named `main`.
pub fn program_has_entry_main(program: &Program) -> bool {
    program.functions.iter().any(|f| f.name == "main")
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            visiting: HashSet::new(),
            module_exports: HashMap::new(),
            stdlib_paths: Vec::new(),
            project_root: None,
            next_expr_id: 1,
        }
    }

    pub fn with_import_config(stdlib_paths: Vec<PathBuf>, project_root: Option<PathBuf>) -> Self {
        Self {
            modules: HashMap::new(),
            visiting: HashSet::new(),
            module_exports: HashMap::new(),
            stdlib_paths,
            project_root,
            next_expr_id: 1,
        }
    }

    pub fn set_import_config(&mut self, stdlib_paths: Vec<PathBuf>, project_root: Option<PathBuf>) {
        self.stdlib_paths = stdlib_paths;
        self.project_root = project_root;
    }

    /// Resolve an import path relative to the importing file and stdlib search paths.
    pub fn resolve_import_path(&self, import_path: &str, from_file: &Path) -> PathBuf {
        crate::build::resolve_import_path(
            import_path,
            from_file,
            &self.stdlib_paths,
            self.project_root.as_deref(),
        )
    }

    /// Parse a module file and recursively parse its imports
    pub fn parse_module(&mut self, path: &Path) -> Result<Program, CompileError> {
        let canonical_path = path
            .canonicalize()
            .map_err(|_| CompileError::FileNotFound {
                path: path.to_path_buf(),
            })?;

        // Check if already parsed
        if let Some(module) = self.modules.get(&canonical_path) {
            return Ok(module.clone());
        }

        // Check for cycles
        if self.visiting.contains(&canonical_path) {
            return Err(CompileError::ImportCycle {
                path: canonical_path,
            });
        }

        // Mark as visiting
        self.visiting.insert(canonical_path.clone());

        // Read file as bytes, then decode UTF-8 (clear error; avoid Debug path noise).
        let bytes = std::fs::read(&canonical_path).map_err(|e| {
            CompileError::IoError(format!(
                "Failed to read file {}: {}",
                canonical_path.display(),
                e
            ))
        })?;
        let content = String::from_utf8(bytes).map_err(|e| CompileError::InvalidUtf8 {
            path: path.to_path_buf(),
            valid_up_to: e.utf8_error().valid_up_to(),
        })?;

        // Lex
        let mut lexer = Lexer::new(&content);
        let tokens = lexer.tokenize().map_err(|e| {
            CompileError::ParseError(ParseError::Message(format!("Lexer error: {}", e)))
        })?;

        // Parse
        let mut parser = Parser::with_source(tokens, &content);
        let mut program = parser.parse().map_err(CompileError::ParseError)?;
        for imp in &program.impls {
            let known = program.structs.iter().any(|s| s.name == imp.type_name)
                || program.enums.iter().any(|e| e.name == imp.type_name);
            if !known {
                return Err(CompileError::ParseError(ParseError::Message(format!(
                    "impl {} for {} must be in the same module as that type",
                    imp.capability, imp.type_name
                ))));
            }
        }
        crate::ast::number_program(&mut program, &mut self.next_expr_id);

        // Recursively parse imports and build export maps
        self.register_imports(&canonical_path, &program.imports)?;

        // Remove from visiting set
        self.visiting.remove(&canonical_path);

        // Cache and return
        self.modules.insert(canonical_path, program.clone());
        Ok(program)
    }

    /// Resolve imports for a file and populate `module_exports`.
    /// Used when the root program is already parsed (e.g. LSP buffer text).
    pub fn register_imports(
        &mut self,
        from_file: &Path,
        imports: &[ImportStmt],
    ) -> Result<(), CompileError> {
        let errors = self.load_imports(from_file, imports);
        if let Some((_, err)) = errors.into_iter().next() {
            Err(err)
        } else {
            Ok(())
        }
    }

    /// Load imports one by one, recording per-import errors without stopping at the first failure.
    pub fn load_imports(
        &mut self,
        from_file: &Path,
        imports: &[ImportStmt],
    ) -> Vec<(Span, CompileError)> {
        let canonical_from = from_file
            .canonicalize()
            .unwrap_or_else(|_| from_file.to_path_buf());

        let mut errors = Vec::new();
        for import in imports {
            let import_path = self.resolve_import_path(&import.path, &canonical_from);
            let imported_module = match self.parse_module(&import_path) {
                Ok(module) => module,
                Err(err) => {
                    errors.push((import.span, err));
                    continue;
                }
            };

            let mut exports = ModuleExports {
                functions: HashMap::new(),
                structs: HashMap::new(),
                enums: HashMap::new(),
                all_functions: HashMap::new(),
            };

            for func in &imported_module.functions {
                exports.all_functions.insert(func.name.clone(), func.pub_);
                if func.pub_ {
                    exports.functions.insert(func.name.clone(), func.clone());
                }
            }

            for s in &imported_module.structs {
                if s.pub_ {
                    exports.structs.insert(s.name.clone(), s.clone());
                }
            }

            for e in &imported_module.enums {
                if e.pub_ {
                    exports.enums.insert(e.name.clone(), e.clone());
                }
            }

            self.module_exports.insert(import.alias.clone(), exports);
        }

        errors
    }

    /// Number expressions in an already-parsed program (LSP buffer AST).
    pub fn number_program(&mut self, program: &mut Program) {
        crate::ast::number_program(program, &mut self.next_expr_id);
    }

    /// Get all parsed modules
    pub fn get_modules(&self) -> &HashMap<PathBuf, Program> {
        &self.modules
    }

    /// Get module exports map (for type checker)
    pub fn get_module_exports(&self) -> &HashMap<String, ModuleExports> {
        &self.module_exports
    }

    /// Merge all modules into a single Program for codegen
    /// This collects all functions, structs, enums, and extern blocks from all modules
    pub fn merge_modules(&self, main_program: &Program, main_path: &Path) -> Program {
        let mut merged = Program {
            doc: main_program.doc.clone(),
            imports: Vec::new(), // Imports are not needed in merged program
            structs: main_program.structs.clone(),
            enums: main_program.enums.clone(),
            type_aliases: main_program.type_aliases.clone(),
            capabilities: main_program.capabilities.clone(),
            impls: main_program.impls.clone(),
            functions: main_program.functions.clone(),
            consts: main_program.consts.clone(),
            extern_blocks: main_program.extern_blocks.clone(),
        };

        let main_canonical = main_path
            .canonicalize()
            .unwrap_or_else(|_| main_path.to_path_buf());

        let mut module_aliases: HashMap<PathBuf, String> = HashMap::new();
        self.collect_module_aliases(&main_canonical, &main_program.imports, &mut module_aliases);

        // Merge all imported modules' public items (skip the main program to avoid duplicates)
        for (module_path, module_program) in &self.modules {
            // Skip the main program - we already have its items
            if module_path
                .canonicalize()
                .unwrap_or_else(|_| module_path.clone())
                == main_canonical
            {
                continue;
            }

            let canonical_module = module_path
                .canonicalize()
                .unwrap_or_else(|_| module_path.clone());
            let alias = module_aliases
                .get(&canonical_module)
                .or_else(|| module_aliases.get(module_path));

            // Include all structs/enums from imported modules so merged pub function
            // bodies type-check (private types used only inside a module).
            for s in &module_program.structs {
                if !merged
                    .structs
                    .iter()
                    .any(|existing| existing.name == s.name)
                {
                    merged.structs.push(s.clone());
                }
            }

            for e in &module_program.enums {
                if !merged.enums.iter().any(|existing| existing.name == e.name) {
                    merged.enums.push(e.clone());
                }
            }

            for cap in &module_program.capabilities {
                if !merged
                    .capabilities
                    .iter()
                    .any(|existing| existing.name == cap.name)
                {
                    merged.capabilities.push(cap.clone());
                }
            }
            for imp in &module_program.impls {
                merged.impls.push(imp.clone());
            }

            // Public functions are renamed `{alias}_{name}`. Calls inside this
            // module still use the bare name, so rewrite those callees to the
            // mangled symbol. Private names stay bare.
            let pub_names: HashSet<String> = module_program
                .functions
                .iter()
                .filter(|f| f.pub_)
                .map(|f| f.name.clone())
                .collect();

            // Private helpers referenced by merged pub functions must be present too.
            for f in &module_program.functions {
                if f.pub_ {
                    continue;
                }
                if merged
                    .functions
                    .iter()
                    .any(|existing| existing.name == f.name)
                {
                    continue;
                }
                let mut f_copy = f.clone();
                if let Some(alias) = alias {
                    rewrite_module_function_calls(&mut f_copy, alias, &pub_names);
                }
                merged.functions.push(f_copy);
            }

            // Add public functions, prefixed with import alias to avoid name collisions
            // (e.g. io::print_int and fmt::print_int both become distinct C symbols).
            if let Some(alias) = alias {
                for f in &module_program.functions {
                    if !f.pub_ {
                        continue;
                    }
                    let mangled_name = format!("{}_{}", alias, f.name);
                    if merged
                        .functions
                        .iter()
                        .any(|existing| existing.name == mangled_name)
                    {
                        continue;
                    }
                    let mut f_copy = f.clone();
                    rewrite_module_function_calls(&mut f_copy, alias, &pub_names);
                    f_copy.name = mangled_name;
                    merged.functions.push(f_copy);
                }
            }

            // Add extern blocks (all extern blocks are included, but deduplicate by function name)
            for extern_block in &module_program.extern_blocks {
                // Check if we already have functions from this extern block
                let mut block_to_add = extern_block.clone();
                block_to_add.functions.retain(|ext_fn| {
                    // Check if this function is already declared in any existing extern block
                    !merged.extern_blocks.iter().any(|existing_block| {
                        existing_block
                            .functions
                            .iter()
                            .any(|existing_fn| existing_fn.name == ext_fn.name)
                    })
                });
                if !block_to_add.functions.is_empty() {
                    merged.extern_blocks.push(block_to_add);
                }
            }
        }

        merged
    }

    /// Map canonical module paths to the import alias used from the main file's import tree.
    pub fn import_aliases_from_main(
        &self,
        main_path: &Path,
        main_program: &Program,
    ) -> HashMap<PathBuf, String> {
        let mut out = HashMap::new();
        let main_canonical = main_path
            .canonicalize()
            .unwrap_or_else(|_| main_path.to_path_buf());
        self.collect_module_aliases(&main_canonical, &main_program.imports, &mut out);
        out
    }

    /// Map canonical module paths to the import alias used from the main file's import tree.
    fn collect_module_aliases(
        &self,
        from_file: &Path,
        imports: &[ImportStmt],
        out: &mut HashMap<PathBuf, String>,
    ) {
        for import in imports {
            let import_path = self.resolve_import_path(&import.path, from_file);
            let canonical = import_path.canonicalize().unwrap_or(import_path);
            if out.contains_key(&canonical) {
                continue;
            }
            out.insert(canonical.clone(), import.alias.clone());
            if let Some(module) = self.modules.get(&canonical) {
                self.collect_module_aliases(&canonical, &module.imports, out);
            }
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

fn rewrite_module_function_calls(func: &mut FnDecl, alias: &str, pub_names: &HashSet<String>) {
    rewrite_block_calls(&mut func.body, alias, pub_names);
}

fn rewrite_block_calls(block: &mut Block, alias: &str, pub_names: &HashSet<String>) {
    for stmt in &mut block.statements {
        rewrite_stmt_calls(stmt, alias, pub_names);
    }
}

fn rewrite_stmt_calls(stmt: &mut Stmt, alias: &str, pub_names: &HashSet<String>) {
    match stmt {
        Stmt::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.init {
                rewrite_expr_calls(init, alias, pub_names);
            }
        }
        Stmt::Return(ret) => {
            if let Some(value) = &mut ret.value {
                rewrite_expr_calls(value, alias, pub_names);
            }
        }
        Stmt::Expr(expr_stmt) => rewrite_expr_calls(&mut expr_stmt.expr, alias, pub_names),
        Stmt::Defer(defer_stmt) => rewrite_expr_calls(&mut defer_stmt.expr, alias, pub_names),
        Stmt::Spawn(spawn) => rewrite_block_calls(&mut spawn.body, alias, pub_names),
        Stmt::Select(sel) => {
            for arm in &mut sel.recv_arms {
                rewrite_expr_calls(&mut arm.recv.channel, alias, pub_names);
                rewrite_block_calls(&mut arm.body, alias, pub_names);
            }
            if let Some(body) = &mut sel.default_body {
                rewrite_block_calls(body, alias, pub_names);
            }
            if let Some(timeout) = &mut sel.timeout_ms {
                rewrite_expr_calls(timeout, alias, pub_names);
            }
            if let Some(body) = &mut sel.timeout_body {
                rewrite_block_calls(body, alias, pub_names);
            }
        }
        Stmt::If(if_stmt) => {
            rewrite_expr_calls(&mut if_stmt.cond, alias, pub_names);
            rewrite_block_calls(&mut if_stmt.then_block, alias, pub_names);
            if let Some(else_block) = &mut if_stmt.else_block {
                rewrite_block_calls(else_block, alias, pub_names);
            }
        }
        Stmt::While(while_stmt) => {
            rewrite_expr_calls(&mut while_stmt.cond, alias, pub_names);
            rewrite_block_calls(&mut while_stmt.body, alias, pub_names);
        }
        Stmt::Loop(loop_stmt) => rewrite_block_calls(&mut loop_stmt.body, alias, pub_names),
        Stmt::For(for_stmt) => {
            rewrite_expr_calls(&mut for_stmt.iterable, alias, pub_names);
            rewrite_block_calls(&mut for_stmt.body, alias, pub_names);
        }
        Stmt::UnsafeBlock(block) => rewrite_block_calls(&mut block.body, alias, pub_names),
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

fn rewrite_expr_calls(expr: &mut Expr, alias: &str, pub_names: &HashSet<String>) {
    match expr {
        Expr::Call(call) => {
            if !call.callee.contains("::") && pub_names.contains(&call.callee) {
                call.callee = format!("{alias}_{}", call.callee);
            }
            for arg in &mut call.args {
                rewrite_expr_calls(arg, alias, pub_names);
            }
        }
        Expr::BinOp(binop) => {
            rewrite_expr_calls(&mut binop.left, alias, pub_names);
            rewrite_expr_calls(&mut binop.right, alias, pub_names);
        }
        Expr::UnOp(unop) => rewrite_expr_calls(&mut unop.operand, alias, pub_names),
        Expr::Ref(r) => rewrite_expr_calls(&mut r.inner, alias, pub_names),
        Expr::Send(send) => {
            rewrite_expr_calls(&mut send.channel, alias, pub_names);
            rewrite_expr_calls(&mut send.value, alias, pub_names);
        }
        Expr::Recv(recv) => rewrite_expr_calls(&mut recv.channel, alias, pub_names),
        Expr::Spawn(spawn) => rewrite_block_calls(&mut spawn.body, alias, pub_names),
        Expr::StructLit(lit) => {
            for field in &mut lit.fields {
                rewrite_expr_calls(&mut field.value, alias, pub_names);
            }
        }
        Expr::FieldAccess(acc) => rewrite_expr_calls(&mut acc.base, alias, pub_names),
        Expr::EnumLit(lit) => {
            for arg in &mut lit.args {
                rewrite_expr_calls(arg, alias, pub_names);
            }
            if let Some(fields) = &mut lit.named_fields {
                for (_, value) in fields {
                    rewrite_expr_calls(value, alias, pub_names);
                }
            }
        }
        Expr::Match(m) => {
            rewrite_expr_calls(&mut m.expr, alias, pub_names);
            for arm in &mut m.arms {
                if let Some(guard) = &mut arm.guard {
                    rewrite_expr_calls(guard, alias, pub_names);
                }
                rewrite_block_calls(&mut arm.body, alias, pub_names);
            }
        }
        Expr::Try(try_expr) => rewrite_expr_calls(&mut try_expr.operand, alias, pub_names),
        Expr::MethodCall(call) => {
            rewrite_expr_calls(&mut call.receiver, alias, pub_names);
            for arg in &mut call.args {
                rewrite_expr_calls(arg, alias, pub_names);
            }
        }
        Expr::ArrayLiteral(arr) => {
            for elem in &mut arr.elements {
                rewrite_expr_calls(elem, alias, pub_names);
            }
            if let Some((value, _)) = &mut arr.repeat {
                rewrite_expr_calls(value, alias, pub_names);
            }
        }
        Expr::TupleLit(tup) => {
            for elem in &mut tup.elements {
                rewrite_expr_calls(elem, alias, pub_names);
            }
        }
        Expr::Index(index) => {
            rewrite_expr_calls(&mut index.target, alias, pub_names);
            rewrite_expr_calls(&mut index.index, alias, pub_names);
        }
        Expr::Cast(cast) => rewrite_expr_calls(&mut cast.expr, alias, pub_names),
        Expr::Assign(assign) => {
            rewrite_expr_calls(&mut assign.target, alias, pub_names);
            rewrite_expr_calls(&mut assign.value, alias, pub_names);
        }
        Expr::FnLiteral(lit) => rewrite_block_calls(&mut lit.body, alias, pub_names),
        Expr::Lit(_)
        | Expr::BoolLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::Var(_)
        | Expr::StringLit(_)
        | Expr::TypeConst(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn merge_modules_includes_private_structs_and_functions_from_imports() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
        let main_path = root.join("test_multi_struct.ion");
        let mut compiler = Compiler::new();
        let main_program = compiler
            .parse_module(&main_path)
            .expect("parse test_multi_struct.ion");
        let merged = compiler.merge_modules(&main_program, &main_path);

        assert!(
            merged.structs.iter().any(|s| s.name == "Item"),
            "expected private struct Item from struct_lib.ion"
        );
        assert!(
            merged.functions.iter().any(|f| f.name == "lib_compute"),
            "expected mangled pub fn from struct_lib.ion, got: {:?}",
            merged.functions.iter().map(|f| &f.name).collect::<Vec<_>>()
        );
        assert!(
            merged.functions.iter().any(|f| f.name == "line_total"),
            "expected private helper fn from struct_lib.ion for type checking"
        );

        let mut checker = crate::tc::TypeChecker::new();
        checker.set_module_exports(compiler.get_module_exports().clone());
        checker
            .check_program(&merged)
            .expect("merged program should type-check");
    }
}
