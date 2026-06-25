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
}

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
    ImportCycle { path: PathBuf },
    FileNotFound { path: PathBuf },
    IoError(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::ParseError(err) => write!(f, "{}", err),
            CompileError::ImportCycle { path } => {
                write!(f, "import cycle detected involving {:?}", path)
            }
            CompileError::FileNotFound { path } => write!(f, "file not found: {:?}", path),
            CompileError::IoError(msg) => write!(f, "IO error: {}", msg),
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            visiting: HashSet::new(),
            module_exports: HashMap::new(),
            stdlib_paths: Vec::new(),
            project_root: None,
        }
    }

    pub fn with_import_config(stdlib_paths: Vec<PathBuf>, project_root: Option<PathBuf>) -> Self {
        Self {
            modules: HashMap::new(),
            visiting: HashSet::new(),
            module_exports: HashMap::new(),
            stdlib_paths,
            project_root,
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

        // Read file
        let content = std::fs::read_to_string(&canonical_path).map_err(|e| {
            CompileError::IoError(format!("Failed to read file {:?}: {}", canonical_path, e))
        })?;

        // Lex
        let mut lexer = Lexer::new(&content);
        let tokens = lexer.tokenize().map_err(|e| {
            CompileError::ParseError(ParseError::Message(format!("Lexer error: {}", e)))
        })?;

        // Parse
        let mut parser = Parser::with_source(tokens, &content);
        let program = parser.parse().map_err(CompileError::ParseError)?;

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
            functions: main_program.functions.clone(),
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
                merged.functions.push(f.clone());
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
