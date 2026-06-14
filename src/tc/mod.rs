mod builtins;
mod ownership;
mod types;

use crate::ast::*;
use std::collections::HashMap;
use std::path::PathBuf;
pub use types::type_to_string;
pub(crate) use types::{fn_type_from_signature, types_equal};

// Helper trait for getting span from expressions
trait HasSpan {
    fn span(&self) -> Span;
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Lit(e) => e.span,
            Expr::BoolLiteral(e) => e.span,
            Expr::FloatLiteral(e) => e.span,
            Expr::Var(e) => e.span,
            Expr::BinOp(e) => e.span,
            Expr::UnOp(e) => e.span,
            Expr::Ref(e) => e.span,
            Expr::Send(e) => e.span,
            Expr::Recv(e) => e.span,
            Expr::StructLit(e) => e.span,
            Expr::FieldAccess(e) => e.span,
            Expr::EnumLit(e) => e.span,
            Expr::Match(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::MethodCall(e) => e.span,
            Expr::StringLit(e) => e.span,
            Expr::ArrayLiteral(e) => e.span,
            Expr::TupleLit(e) => e.span,
            Expr::Index(e) => e.span,
            Expr::Cast(e) => e.span,
            Expr::Assign(e) => e.span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OwnershipState {
    Valid,
    Moved,
}

#[derive(Debug, Clone)]
struct VariableInfo {
    ty: Type,
    state: OwnershipState,
    definition_span: Span,
    shared_borrow_count: u32,
    mut_borrow_count: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LspSymbolKind {
    Function,
    Struct,
    Enum,
    Field,
    Variant,
    TypeAlias,
    Variable,
    Module,
    Builtin,
}

#[derive(Debug, Clone)]
pub struct LspCompletionItem {
    pub label: String,
    pub kind: LspSymbolKind,
    pub detail: Option<String>,
}

#[derive(Debug, Clone)]
pub struct LspDocumentSymbol {
    pub name: String,
    pub kind: LspSymbolKind,
    pub span: Span,
    pub children: Vec<LspDocumentSymbol>,
}

#[derive(Debug, Clone)]
pub struct LspSignatureHelp {
    pub label: String,
    pub active_parameter: Option<u32>,
}

#[derive(Debug, Clone, Default)]
pub struct LspInfo {
    pub references: HashMap<Span, LspTarget>,
    pub types: HashMap<Span, Type>,
    /// Hover text keyed by definition span (functions, structs, enums, aliases, extern fns, builtins).
    pub hover_docs: HashMap<Span, String>,
    pub completions: Vec<LspCompletionItem>,
    pub document_symbols: Vec<LspDocumentSymbol>,
    pub signatures: HashMap<Span, LspSignatureHelp>,
    /// Qualified items per import alias (e.g. `io` -> `io::print`).
    pub module_items: HashMap<String, Vec<LspCompletionItem>>,
    /// Struct field names for member completion.
    pub struct_members: HashMap<String, Vec<LspCompletionItem>>,
    /// Enum variant names for member completion.
    pub enum_variants: HashMap<String, Vec<LspCompletionItem>>,
}

/// Go-to-definition target: span and optional file (None = current file).
#[derive(Debug, Clone)]
pub struct LspTarget {
    pub span: Span,
    pub file: Option<PathBuf>,
}

#[derive(Debug)]
pub enum TypeCheckError {
    UndefinedVariable {
        name: String,
        span: Span,
    },
    TypeMismatch {
        expected: String,
        got: String,
        span: Span,
    },
    UseAfterMove {
        name: String,
        span: Span,
    },
    BorrowConflict {
        name: String,
        description: String,
        span: Span,
    },
    ReferenceEscape {
        description: String,
        span: Span,
    },
    Message(String),
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeCheckError::UndefinedVariable { name, span } => {
                write!(
                    f,
                    "line {}: UndefinedVariable: undefined variable '{}'",
                    span.line, name
                )
            }
            TypeCheckError::TypeMismatch {
                expected,
                got,
                span,
            } => write!(
                f,
                "line {}: TypeMismatch: expected {}, got {}",
                span.line, expected, got
            ),
            TypeCheckError::UseAfterMove { name, span } => write!(
                f,
                "line {}: UseAfterMove: variable '{}' was moved",
                span.line, name
            ),
            TypeCheckError::BorrowConflict {
                name,
                description,
                span,
            } => write!(
                f,
                "line {}: BorrowConflict: cannot borrow '{}' {}",
                span.line, name, description
            ),
            TypeCheckError::ReferenceEscape { description, span } => {
                write!(f, "line {}: ReferenceEscape: {}", span.line, description)
            }
            TypeCheckError::Message(msg) => write!(f, "{}", msg),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckResult {
    pub lsp_info: LspInfo,
}

pub struct TypeChecker {
    variables: HashMap<String, VariableInfo>,
    structs: HashMap<String, StructDecl>,
    enums: HashMap<String, EnumDecl>,
    type_aliases: HashMap<String, TypeAliasDecl>,
    functions: HashMap<String, FnDecl>,
    extern_functions: HashMap<String, ExternFnDecl>,
    // Module imports: maps import alias to module exports
    module_imports: HashMap<String, ModuleExports>,
    // Import alias to source file path (for cross-file go-to-definition)
    module_paths: HashMap<String, PathBuf>,
    // Unsafe context tracking (depth for nested unsafe blocks)
    unsafe_context_depth: usize,
    // Loop nesting depth (for break/continue validation)
    loop_depth: usize,
    // Current function's resolved return type (for return statement checking)
    current_return_type: Option<Type>,
    // LSP information collected during type checking
    pub lsp_info: LspInfo,
    // Active function generic type parameter names (innermost scope last)
    type_param_scopes: Vec<Vec<String>>,
    // Borrows registered in nested scopes (released on scope pop)
    borrow_scopes: Vec<Vec<(String, bool)>>,
    // When false, skip recording expression-level LSP data (avoids span collisions from merged imports).
    lsp_recording: bool,
}

#[derive(Debug, Clone)]
pub struct ModuleExports {
    pub functions: HashMap<String, FnDecl>, // Public functions only
    pub structs: HashMap<String, StructDecl>, // Public structs only
    pub enums: HashMap<String, EnumDecl>,   // Public enums only
    // All functions (public and private) for better error messages
    pub all_functions: HashMap<String, bool>, // Maps function name to is_public
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            type_aliases: HashMap::new(),
            functions: HashMap::new(),
            extern_functions: HashMap::new(),
            module_imports: HashMap::new(),
            module_paths: HashMap::new(),
            unsafe_context_depth: 0,
            loop_depth: 0,
            current_return_type: None,
            lsp_info: LspInfo::default(),
            type_param_scopes: Vec::new(),
            borrow_scopes: Vec::new(),
            lsp_recording: true,
        }
    }

    fn new_variable_info(ty: Type, definition_span: Span) -> VariableInfo {
        VariableInfo {
            ty,
            state: OwnershipState::Valid,
            definition_span,
            shared_borrow_count: 0,
            mut_borrow_count: 0,
        }
    }

    fn push_type_params(&mut self, params: &[String]) {
        self.type_param_scopes.push(params.to_vec());
    }

    fn pop_type_params(&mut self) {
        self.type_param_scopes.pop();
    }

    fn is_type_param(&self, name: &str) -> bool {
        self.type_param_scopes
            .iter()
            .any(|scope| scope.iter().any(|p| p == name))
    }

    fn record_hover_doc(&mut self, span: Span, doc: String) {
        if !self.lsp_recording {
            return;
        }
        self.lsp_info.hover_docs.insert(span, doc);
    }

    fn record_completion(&mut self, label: &str, kind: LspSymbolKind, detail: Option<String>) {
        let label = label.to_string();
        if self
            .lsp_info
            .completions
            .iter()
            .any(|item| item.label == label)
        {
            return;
        }
        self.lsp_info.completions.push(LspCompletionItem {
            label,
            kind,
            detail,
        });
    }

    fn record_expr_type(&mut self, span: Span, ty: &Type) {
        if !self.lsp_recording {
            return;
        }
        self.lsp_info.types.insert(span, ty.clone());
    }

    fn record_signature(&mut self, span: Span, label: String, active_parameter: Option<u32>) {
        if !self.lsp_recording {
            return;
        }
        self.lsp_info.signatures.insert(
            span,
            LspSignatureHelp {
                label,
                active_parameter,
            },
        );
    }

    fn fn_hover_doc(
        name: &str,
        generics: &[String],
        params: &[Param],
        ret: &Option<Type>,
    ) -> String {
        let params: Vec<String> = params
            .iter()
            .map(|p| format!("{}: {}", p.name, type_to_string(&p.ty)))
            .collect();
        let ret = ret
            .as_ref()
            .map(type_to_string)
            .unwrap_or_else(|| "void".to_string());
        let generics = if generics.is_empty() {
            String::new()
        } else {
            format!("<{}>", generics.join(", "))
        };
        format!("fn {}{}({}) -> {}", name, generics, params.join(", "), ret)
    }

    fn seed_lsp_symbols(&mut self, source: &Program) {
        for alias in &source.imports {
            self.record_completion(&alias.alias, LspSymbolKind::Module, None);
        }

        for alias in &source.type_aliases {
            let generics = if alias.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", alias.generics.join(", "))
            };
            let doc = format!(
                "type {}{} = {}",
                alias.name,
                generics,
                type_to_string(&alias.target)
            );
            self.record_hover_doc(alias.span, doc);
            self.record_completion(
                &alias.name,
                LspSymbolKind::TypeAlias,
                Some(type_to_string(&alias.target)),
            );
            self.lsp_info.document_symbols.push(LspDocumentSymbol {
                name: alias.name.clone(),
                kind: LspSymbolKind::TypeAlias,
                span: alias.span,
                children: Vec::new(),
            });
        }

        for s in &source.structs {
            let fields: Vec<String> = s.fields.iter().map(|f| f.name.clone()).collect();
            let generics = if s.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", s.generics.join(", "))
            };
            self.record_hover_doc(
                s.span,
                format!("struct {}{} {{ {} }}", s.name, generics, fields.join(", ")),
            );
            self.record_completion(&s.name, LspSymbolKind::Struct, None);
            let field_children: Vec<LspDocumentSymbol> = s
                .fields
                .iter()
                .map(|f| LspDocumentSymbol {
                    name: f.name.clone(),
                    kind: LspSymbolKind::Field,
                    span: f.span,
                    children: Vec::new(),
                })
                .collect();
            let member_items: Vec<LspCompletionItem> = s
                .fields
                .iter()
                .map(|f| LspCompletionItem {
                    label: f.name.clone(),
                    kind: LspSymbolKind::Field,
                    detail: Some(type_to_string(&f.ty)),
                })
                .collect();
            self.lsp_info
                .struct_members
                .insert(s.name.clone(), member_items);
            self.lsp_info.document_symbols.push(LspDocumentSymbol {
                name: s.name.clone(),
                kind: LspSymbolKind::Struct,
                span: s.span,
                children: field_children,
            });
        }

        for e in &source.enums {
            let variants: Vec<String> = e.variants.iter().map(|v| v.name.clone()).collect();
            let generics = if e.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", e.generics.join(", "))
            };
            self.record_hover_doc(
                e.span,
                format!("enum {}{} {{ {} }}", e.name, generics, variants.join(", ")),
            );
            self.record_completion(&e.name, LspSymbolKind::Enum, None);
            let variant_children: Vec<LspDocumentSymbol> = e
                .variants
                .iter()
                .map(|v| LspDocumentSymbol {
                    name: v.name.clone(),
                    kind: LspSymbolKind::Variant,
                    span: v.span,
                    children: Vec::new(),
                })
                .collect();
            let variant_items: Vec<LspCompletionItem> = e
                .variants
                .iter()
                .map(|v| LspCompletionItem {
                    label: v.name.clone(),
                    kind: LspSymbolKind::Variant,
                    detail: None,
                })
                .collect();
            self.lsp_info
                .enum_variants
                .insert(e.name.clone(), variant_items);
            for v in &e.variants {
                self.record_hover_doc(v.span, format!("{}::{}", e.name, v.name));
            }
            self.lsp_info.document_symbols.push(LspDocumentSymbol {
                name: e.name.clone(),
                kind: LspSymbolKind::Enum,
                span: e.span,
                children: variant_children,
            });
        }

        for function in &source.functions {
            let doc = Self::fn_hover_doc(
                &function.name,
                &function.generics,
                &function.params,
                &function.return_type,
            );
            self.record_hover_doc(function.span, doc);
            self.record_completion(
                &function.name,
                LspSymbolKind::Function,
                function.return_type.as_ref().map(type_to_string),
            );
            self.lsp_info.document_symbols.push(LspDocumentSymbol {
                name: function.name.clone(),
                kind: LspSymbolKind::Function,
                span: function.span,
                children: Vec::new(),
            });
        }

        for block in &source.extern_blocks {
            for extern_fn in &block.functions {
                let doc = Self::fn_hover_doc(
                    &extern_fn.name,
                    &[],
                    &extern_fn.params,
                    &extern_fn.return_type,
                );
                self.record_hover_doc(
                    extern_fn.span,
                    format!("extern \"{}\" {}", block.linkage, doc),
                );
                self.record_completion(
                    &extern_fn.name,
                    LspSymbolKind::Function,
                    extern_fn.return_type.as_ref().map(type_to_string),
                );
                self.lsp_info.document_symbols.push(LspDocumentSymbol {
                    name: extern_fn.name.clone(),
                    kind: LspSymbolKind::Function,
                    span: extern_fn.span,
                    children: Vec::new(),
                });
            }
        }
    }

    fn seed_module_items(&mut self) {
        for (alias, exports) in &self.module_imports {
            let mut items = Vec::new();
            for (name, func) in &exports.functions {
                let label = format!("{alias}::{name}");
                items.push(LspCompletionItem {
                    label,
                    kind: LspSymbolKind::Function,
                    detail: Some(Self::fn_hover_doc(
                        name,
                        &func.generics,
                        &func.params,
                        &func.return_type,
                    )),
                });
            }
            for name in exports.structs.keys() {
                items.push(LspCompletionItem {
                    label: format!("{alias}::{name}"),
                    kind: LspSymbolKind::Struct,
                    detail: None,
                });
            }
            for name in exports.enums.keys() {
                items.push(LspCompletionItem {
                    label: format!("{alias}::{name}"),
                    kind: LspSymbolKind::Enum,
                    detail: None,
                });
            }
            self.lsp_info.module_items.insert(alias.clone(), items);
        }
    }

    fn is_in_unsafe_context(&self) -> bool {
        self.unsafe_context_depth > 0
    }

    /// Set module exports for qualified name resolution
    pub fn set_module_exports(&mut self, exports: HashMap<String, ModuleExports>) {
        self.module_imports = exports;
    }

    /// Map import alias to absolute source path for cross-file LSP navigation.
    pub fn set_module_paths(&mut self, paths: HashMap<String, PathBuf>) {
        self.module_paths = paths;
    }

    fn record_reference(&mut self, use_span: Span, target: LspTarget) {
        if !self.lsp_recording {
            return;
        }
        self.lsp_info.references.insert(use_span, target);
    }

    fn lookup_type_target(&self, name: &str) -> Option<LspTarget> {
        if let Some(s) = self.structs.get(name) {
            return Some(LspTarget {
                span: s.span,
                file: None,
            });
        }
        if let Some(e) = self.enums.get(name) {
            return Some(LspTarget {
                span: e.span,
                file: None,
            });
        }
        if let Some(a) = self.type_aliases.get(name) {
            return Some(LspTarget {
                span: a.span,
                file: None,
            });
        }
        None
    }

    fn lookup_variant_target(&self, enum_name: &str, variant: &str) -> Option<LspTarget> {
        let enum_decl = self.enums.get(enum_name)?;
        let variant_decl = enum_decl.variants.iter().find(|v| v.name == variant)?;
        Some(LspTarget {
            span: variant_decl.span,
            file: None,
        })
    }

    fn lookup_field_target(&self, struct_name: &str, field: &str) -> Option<LspTarget> {
        let struct_decl = self.structs.get(struct_name)?;
        let field_decl = struct_decl.fields.iter().find(|f| f.name == field)?;
        Some(LspTarget {
            span: field_decl.span,
            file: None,
        })
    }

    fn builtin_signature(qualified: &str) -> Option<&'static str> {
        match qualified {
            "Vec::new" => Some("fn Vec::new() -> Vec<T>"),
            "Vec::with_capacity" => Some("fn Vec::with_capacity(cap: int) -> Vec<T>"),
            "Vec::push" => Some("fn Vec::push(vec: &mut Vec<T>, value: T)"),
            "Vec::pop" => Some("fn Vec::pop(vec: &mut Vec<T>) -> Option<T>"),
            "Vec::len" => Some("fn Vec::len(vec: &Vec<T>) -> int"),
            "Vec::capacity" => Some("fn Vec::capacity(vec: &Vec<T>) -> int"),
            "Vec::get" => Some("fn Vec::get(vec: &Vec<T>, index: int) -> Option<T>"),
            "Vec::set" => Some("fn Vec::set(vec: &mut Vec<T>, index: int, value: T) -> int"),
            "String::new" => Some("fn String::new() -> String"),
            "String::from" => Some("fn String::from(s: &str) -> String"),
            "String::len" => Some("fn String::len(s: &String) -> int"),
            "String::push_str" => Some("fn String::push_str(s: &mut String, other: &str)"),
            "String::push_byte" => Some("fn String::push_byte(s: &mut String, b: u8)"),
            "Box::new" => Some("fn Box::new<T>(value: T) -> Box<T>"),
            "Box::unwrap" => Some("fn Box::unwrap<T>(box: Box<T>) -> T"),
            "channel" => Some("fn channel<T>() -> (Sender<T>, Receiver<T>)"),
            "send" => Some("fn send(sender: &Sender<T>, value: T)"),
            "recv" => Some("fn recv(receiver: &mut Receiver<T>) -> T"),
            _ => None,
        }
    }

    fn lookup_fn_target(&self, callee: &str) -> Option<LspTarget> {
        if let Some(f) = self.functions.get(callee) {
            return Some(LspTarget {
                span: f.span,
                file: None,
            });
        }
        if let Some(f) = self.extern_functions.get(callee) {
            return Some(LspTarget {
                span: f.span,
                file: None,
            });
        }
        if let Some((module, func)) = callee.split_once("::") {
            let decl = self.module_imports.get(module)?.functions.get(func)?;
            return Some(LspTarget {
                span: decl.span,
                file: self.module_paths.get(module).cloned(),
            });
        }
        None
    }

    pub fn check_program(&mut self, program: &Program) -> Result<TypeCheckResult, TypeCheckError> {
        let (result, errors) = self.check_program_collecting(program);
        if let Some(err) = errors.into_iter().next() {
            Err(err)
        } else {
            Ok(result)
        }
    }

    /// Type-check the program and collect all independent errors (e.g. per-function).
    /// Used by the LSP to publish multiple diagnostics in one pass.
    pub fn check_program_collecting(
        &mut self,
        program: &Program,
    ) -> (TypeCheckResult, Vec<TypeCheckError>) {
        self.check_program_collecting_with_source(program, program)
    }

    pub fn check_program_collecting_with_source(
        &mut self,
        program: &Program,
        source: &Program,
    ) -> (TypeCheckResult, Vec<TypeCheckError>) {
        self.lsp_info = LspInfo::default();
        self.seed_lsp_symbols(source);
        self.seed_module_items();
        let mut errors = Vec::new();

        // Record struct declarations
        self.structs.clear();
        for s in &program.structs {
            self.structs.insert(s.name.clone(), s.clone());
        }

        // Record enum declarations
        self.enums.clear();
        for e in &program.enums {
            self.enums.insert(e.name.clone(), e.clone());
        }

        // Record type alias declarations
        self.type_aliases.clear();
        for alias in &program.type_aliases {
            self.type_aliases.insert(alias.name.clone(), alias.clone());
        }

        // Record function declarations
        self.functions.clear();
        for f in &program.functions {
            self.functions.insert(f.name.clone(), f.clone());
        }

        // Record extern function declarations
        self.extern_functions.clear();
        for extern_block in &program.extern_blocks {
            for extern_fn in &extern_block.functions {
                self.extern_functions
                    .insert(extern_fn.name.clone(), extern_fn.clone());
            }
        }

        // Enforce no-escape rule on struct fields: they cannot contain references.
        for s in &program.structs {
            for field in &s.fields {
                if self.is_reference_containing(&field.ty) {
                    errors.push(TypeCheckError::ReferenceEscape {
                        description: format!(
                            "Struct '{}' field '{}' cannot have reference type '{}' (violates no-escape rule)",
                            s.name,
                            field.name,
                            type_to_string(&field.ty)
                        ),
                        span: field.span,
                    });
                }
            }
        }

        // Enforce no-escape rule on enum variant payloads
        for e in &program.enums {
            for variant in &e.variants {
                for payload_ty in &variant.payload_types {
                    if self.is_reference_containing(payload_ty) {
                        errors.push(TypeCheckError::ReferenceEscape {
                            description: format!(
                                "Enum '{}' variant '{}' cannot have reference type '{}' in payload (violates no-escape rule)",
                                e.name,
                                variant.name,
                                type_to_string(payload_ty)
                            ),
                            span: variant.span,
                        });
                    }
                }
            }
        }

        let source_fn_names: std::collections::HashSet<String> =
            source.functions.iter().map(|f| f.name.clone()).collect();

        for function in &program.functions {
            self.lsp_recording = source_fn_names.contains(&function.name);
            if let Err(e) = self.check_function(function) {
                errors.push(e);
            }
        }
        self.lsp_recording = true;

        (
            TypeCheckResult {
                lsp_info: self.lsp_info.clone(),
            },
            errors,
        )
    }

    /// Get the required receiver type for a built-in method.
    /// Returns Some(receiver_type) if it's a built-in method, None otherwise.
    fn get_builtin_method_receiver_type(
        &self,
        qualified_name: &str,
        receiver_type: &Type,
        type_name: &str,
    ) -> Result<Option<Type>, TypeCheckError> {
        match (type_name, qualified_name) {
            ("Vec", "Vec::push") | ("Vec", "Vec::pop") | ("Vec", "Vec::set") => {
                // These methods require &mut Vec<T>
                let elem_type = match receiver_type {
                    Type::Vec { elem_type } => elem_type.clone(),
                    Type::Ref { inner, .. } => match **inner {
                        Type::Vec { ref elem_type } => elem_type.clone(),
                        _ => return Ok(None),
                    },
                    _ => return Ok(None),
                };
                Ok(Some(Type::Ref {
                    inner: Box::new(Type::Vec { elem_type }),
                    mutable: true,
                }))
            }
            ("Vec", "Vec::len") | ("Vec", "Vec::capacity") | ("Vec", "Vec::get") => {
                // These methods require &Vec<T>
                let elem_type = match receiver_type {
                    Type::Vec { elem_type } => elem_type.clone(),
                    Type::Ref { inner, .. } => match **inner {
                        Type::Vec { ref elem_type } => elem_type.clone(),
                        _ => return Ok(None),
                    },
                    _ => return Ok(None),
                };
                Ok(Some(Type::Ref {
                    inner: Box::new(Type::Vec { elem_type }),
                    mutable: false,
                }))
            }
            ("String", "String::len") => {
                // String::len requires &String
                Ok(Some(Type::Ref {
                    inner: Box::new(Type::String),
                    mutable: false,
                }))
            }
            ("String", "String::push_str") => {
                // String::push_str requires &mut String
                Ok(Some(Type::Ref {
                    inner: Box::new(Type::String),
                    mutable: true,
                }))
            }
            ("String", "String::push_byte") => {
                // String::push_byte requires &mut String
                Ok(Some(Type::Ref {
                    inner: Box::new(Type::String),
                    mutable: true,
                }))
            }
            _ => Ok(None),
        }
    }

    /// Look up a method function by qualified name.
    /// Returns the function declaration if found.
    fn lookup_method_function(
        &self,
        qualified_name: &str,
        type_name: &str,
    ) -> Result<FnDecl, TypeCheckError> {
        // Check built-in types first (Vec, String, Box)
        if let Some(func_decl) = self.functions.get(qualified_name) {
            return Ok(func_decl.clone());
        }

        // Check module imports
        if let Some(module_exports) = self.module_imports.get(type_name) {
            let parts: Vec<&str> = qualified_name.split("::").collect();
            if parts.len() == 2 {
                let func_name = parts[1];
                if let Some(&is_public) = module_exports.all_functions.get(func_name) {
                    if !is_public {
                        return Err(TypeCheckError::Message(format!(
                            "Cannot access non-public method '{}' from module '{}'. Add 'pub' before 'fn {}' in the module to make it accessible.",
                            qualified_name, type_name, func_name
                        )));
                    }
                    if let Some(func_decl) = module_exports.functions.get(func_name) {
                        return Ok(func_decl.clone());
                    }
                }
            }
        }

        // Check if it's a struct method
        if self.structs.contains_key(type_name) || self.enums.contains_key(type_name) {
            // Look for function with qualified name in current scope
            if let Some(func_decl) = self.functions.get(qualified_name) {
                return Ok(func_decl.clone());
            }
        }

        Err(TypeCheckError::Message(format!(
            "No method '{}' found for type '{}'",
            qualified_name.split("::").last().unwrap_or(""),
            type_name
        )))
    }

    /// Create a receiver argument with appropriate borrowing based on the required type.
    fn create_receiver_argument(
        &self,
        receiver: Box<Expr>,
        receiver_type: &Type,
        required_type: &Type,
        span: Span,
    ) -> Result<Expr, TypeCheckError> {
        // Check if the required type matches the receiver type
        let types_match = types_equal(receiver_type, required_type);

        // Check if required type is a reference
        if let Type::Ref {
            inner: required_inner,
            mutable: required_mut,
        } = required_type
        {
            // Need to create a reference
            // Check if receiver is already a reference
            if let Type::Ref {
                inner: receiver_inner,
                mutable: receiver_mut,
            } = receiver_type
            {
                // Already a reference - check if mutability matches
                if *required_mut && !receiver_mut {
                    return Err(TypeCheckError::Message(
                        "Cannot borrow receiver as mutable reference - receiver is immutable"
                            .to_string(),
                    ));
                }
                // Types should match
                if !types_equal(receiver_inner, required_inner) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: type_to_string(required_type),
                        got: type_to_string(receiver_type),
                        span,
                    });
                }
                // Already the right type, return as-is
                Ok(*receiver)
            } else {
                // Need to create a reference
                // Check if receiver is a mutable variable for &mut
                if *required_mut {
                    // For &mut, the receiver must be a mutable binding
                    // This is a simplified check - in practice, we'd need to track mutability
                    // For now, we'll create the &mut expression and let the type checker validate
                    Ok(Expr::Ref(RefExpr {
                        mutable: true,
                        inner: receiver,
                        span,
                    }))
                } else {
                    // Create immutable reference
                    Ok(Expr::Ref(RefExpr {
                        mutable: false,
                        inner: receiver,
                        span,
                    }))
                }
            }
        } else {
            // Required type is not a reference - pass by value
            if types_match {
                Ok(*receiver)
            } else {
                Err(TypeCheckError::TypeMismatch {
                    expected: type_to_string(required_type),
                    got: type_to_string(receiver_type),
                    span,
                })
            }
        }
    }

    /// Helper function to substitute type parameters (similar to the one in cgen)
    fn substitute_type_params(ty: &Type, substitutions: &HashMap<String, &Type>) -> Type {
        match ty {
            Type::Struct(name) | Type::Enum(name) => substitutions
                .get(name)
                .map(|&sub_ty| sub_ty.clone())
                .unwrap_or_else(|| ty.clone()),
            Type::Generic { name, params } => {
                if let Some(&sub_ty) = substitutions.get(name) {
                    sub_ty.clone()
                } else {
                    let substituted_params: Vec<Type> = params
                        .iter()
                        .map(|p| Self::substitute_type_params(p, substitutions))
                        .collect();
                    Type::Generic {
                        name: name.clone(),
                        params: substituted_params,
                    }
                }
            }
            Type::Ref { inner, mutable } => Type::Ref {
                inner: Box::new(Self::substitute_type_params(inner, substitutions)),
                mutable: *mutable,
            },
            Type::RawPtr { inner } => Type::RawPtr {
                inner: Box::new(Self::substitute_type_params(inner, substitutions)),
            },
            Type::Box { inner } => Type::Box {
                inner: Box::new(Self::substitute_type_params(inner, substitutions)),
            },
            Type::Vec { elem_type } => Type::Vec {
                elem_type: Box::new(Self::substitute_type_params(elem_type, substitutions)),
            },
            Type::Channel { elem_type } => Type::Channel {
                elem_type: Box::new(Self::substitute_type_params(elem_type, substitutions)),
            },
            Type::Array { inner, size } => Type::Array {
                inner: Box::new(Self::substitute_type_params(inner, substitutions)),
                size: *size,
            },
            Type::Slice { inner } => Type::Slice {
                inner: Box::new(Self::substitute_type_params(inner, substitutions)),
            },
            Type::Sender { elem_type } => Type::Sender {
                elem_type: Box::new(Self::substitute_type_params(elem_type, substitutions)),
            },
            Type::Receiver { elem_type } => Type::Receiver {
                elem_type: Box::new(Self::substitute_type_params(elem_type, substitutions)),
            },
            Type::Tuple { elements } => Type::Tuple {
                elements: elements
                    .iter()
                    .map(|e| Self::substitute_type_params(e, substitutions))
                    .collect(),
            },
            Type::Fn {
                params,
                return_type,
            } => Type::Fn {
                params: params
                    .iter()
                    .map(|p| Self::substitute_type_params(p, substitutions))
                    .collect(),
                return_type: Box::new(Self::substitute_type_params(return_type, substitutions)),
            },
            _ => ty.clone(),
        }
    }

    fn check_function(&mut self, function: &FnDecl) -> Result<(), TypeCheckError> {
        // Resolve return type alias if present
        let resolved_return_type = if let Some(ref ret_ty) = function.return_type {
            Some(self.resolve_type_name(ret_ty)?)
        } else {
            None
        };

        // Check no-escape rule: function return types cannot contain references
        if let Some(ref ret_ty) = resolved_return_type
            && self.is_reference_containing(ret_ty)
        {
            return Err(TypeCheckError::ReferenceEscape {
                description: format!(
                    "Function '{}' cannot return a reference type '{}' (violates no-escape rule)",
                    function.name,
                    type_to_string(ret_ty)
                ),
                span: function.span,
            });
        }

        // Save previous scope and return type
        let prev_vars = self.variables.clone();
        let prev_return_type = self.current_return_type.clone();

        // Set current return type for return statement checking
        self.current_return_type = resolved_return_type.clone();

        if !function.generics.is_empty() {
            self.push_type_params(&function.generics);
        }

        // Add parameters to scope (they start as Valid)
        // Resolve type aliases for parameters
        for param in &function.params {
            let resolved_param_ty = self.resolve_type_name(&param.ty)?;
            self.variables.insert(
                param.name.clone(),
                Self::new_variable_info(resolved_param_ty, function.span),
            );
        }

        // Check function body
        self.push_borrow_scope();
        for stmt in &function.body.statements {
            self.check_stmt(stmt)?;
        }
        self.pop_borrow_scope();

        // Restore previous scope and return type
        self.variables = prev_vars;
        self.current_return_type = prev_return_type;
        if !function.generics.is_empty() {
            self.pop_type_params();
        }

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), TypeCheckError> {
        match stmt {
            Stmt::Let(let_stmt) => {
                // Handle tuple destructuring: let (a, b) = expr
                if let Some(ref patterns) = let_stmt.patterns {
                    // channel() tuple destructuring: let (tx, rx): (Sender<T>, Receiver<T>) = channel<T>();
                    if patterns.len() == 2
                        && let Some(ref init) = let_stmt.init
                        && let Expr::Call(call_expr) = init
                        && call_expr.callee == "channel"
                    {
                        // channel<T>() returns (Sender<T>, Receiver<T>)
                        // Extract T from type annotation if present, or infer from context
                        let elem_type = if let Some(ref type_ann) = let_stmt.type_ann {
                            // Try to extract T from type annotation like (Sender<T>, Receiver<T>)
                            if let Type::Tuple { elements } = self.resolve_type_name(type_ann)? {
                                if elements.len() == 2 {
                                    // Try to extract T from Sender<T> and Receiver<T>
                                    if let Type::Sender { elem_type: tx_elem } = &elements[0] {
                                        if let Type::Receiver { elem_type: rx_elem } = &elements[1]
                                        {
                                            if types_equal(tx_elem, rx_elem) {
                                                Some((**tx_elem).clone())
                                            } else {
                                                return Err(TypeCheckError::Message(
                                                            "Sender and Receiver element types must match in channel tuple".to_string()
                                                        ));
                                            }
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            // No type annotation - we'll need to infer later
                            // For now, require type annotation
                            None
                        };

                        if let Some(elem_ty) = elem_type {
                            // Verify element type is Send
                            if !self.is_send(&elem_ty) {
                                return Err(TypeCheckError::TypeMismatch {
                                    expected: "Send element type for channel".to_string(),
                                    got: type_to_string(&elem_ty),
                                    span: let_stmt.span,
                                });
                            }

                            // Create Sender and Receiver types
                            let sender_ty = Type::Sender {
                                elem_type: Box::new(elem_ty.clone()),
                            };
                            let receiver_ty = Type::Receiver {
                                elem_type: Box::new(elem_ty),
                            };

                            // Bind patterns to their types
                            if patterns.len() == 2 {
                                // First pattern gets Sender<T>
                                match &patterns[0] {
                                    Pattern::Binding { name, .. } => {
                                        self.variables.insert(
                                            name.clone(),
                                            Self::new_variable_info(sender_ty, let_stmt.span),
                                        );
                                    }
                                    _ => {
                                        return Err(TypeCheckError::Message(
                                            "First pattern in channel tuple must be a binding"
                                                .to_string(),
                                        ));
                                    }
                                }

                                // Second pattern gets Receiver<T>
                                match &patterns[1] {
                                    Pattern::Binding { name, .. } => {
                                        self.variables.insert(
                                            name.clone(),
                                            Self::new_variable_info(receiver_ty, let_stmt.span),
                                        );
                                    }
                                    _ => {
                                        return Err(TypeCheckError::Message(
                                            "Second pattern in channel tuple must be a binding"
                                                .to_string(),
                                        ));
                                    }
                                }

                                // Mark the init expression as moved (though channel() creates new values)
                                self.check_expr_for_moves(init)?;

                                return Ok(());
                            }
                        } else {
                            return Err(TypeCheckError::Message(
                                        "channel() with tuple destructuring requires type annotation: let (tx, rx): (Sender<T>, Receiver<T>) = channel<T>()".to_string()
                                    ));
                        }
                    }

                    // General tuple destructuring: let (a, b, ...) = tuple_expr;
                    let init = let_stmt.init.as_ref().ok_or_else(|| {
                        TypeCheckError::Message(
                            "tuple destructuring requires an initializer".to_string(),
                        )
                    })?;
                    let init_type = self.check_expr(init)?;
                    let tuple_type = if let Some(ref type_ann) = let_stmt.type_ann {
                        let resolved = self.resolve_type_name(type_ann)?;
                        if !types_equal(&resolved, &init_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: type_to_string(&resolved),
                                got: type_to_string(&init_type),
                                span: let_stmt.span,
                            });
                        }
                        resolved
                    } else {
                        init_type
                    };
                    let Type::Tuple { elements } = tuple_type else {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "tuple type for destructuring".to_string(),
                            got: type_to_string(&tuple_type),
                            span: let_stmt.span,
                        });
                    };
                    if patterns.len() != elements.len() {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: format!("{} tuple patterns", elements.len()),
                            got: format!("{} patterns", patterns.len()),
                            span: let_stmt.span,
                        });
                    }
                    for (pattern, elem_ty) in patterns.iter().zip(elements.iter()) {
                        match pattern {
                            Pattern::Binding { name, .. } => {
                                self.variables.insert(
                                    name.clone(),
                                    Self::new_variable_info(elem_ty.clone(), let_stmt.span),
                                );
                            }
                            _ => {
                                return Err(TypeCheckError::Message(
                                    "tuple destructuring patterns must be variable bindings"
                                        .to_string(),
                                ));
                            }
                        }
                    }
                    self.check_expr_for_moves(init)?;
                    return Ok(());
                }

                if let Some(ref init) = let_stmt.init {
                    // First check the expression type (this verifies variables are Valid)
                    let init_type = self.check_expr(init)?;

                    // Resolve type annotation - convert Struct(name) to Enum(name) if it's actually an enum
                    // Also resolve type aliases
                    let resolved_type_ann = if let Some(ref type_ann) = let_stmt.type_ann {
                        Some(self.resolve_type_name(type_ann)?)
                    } else {
                        None
                    };

                    if let Some(ref type_ann) = resolved_type_ann {
                        // Resolve init_type aliases too
                        let resolved_init_type = self.resolve_type_name(&init_type)?;

                        // Check for array-to-slice coercion: &[T; N] can be coerced to &[]T
                        let coerced = if let Type::Ref {
                            inner: init_inner,
                            mutable: init_mut,
                        } = &resolved_init_type
                        {
                            if let Type::Ref {
                                inner: ann_inner,
                                mutable: ann_mut,
                            } = type_ann
                            {
                                if matches!(**init_inner, Type::Array { .. })
                                    && matches!(**ann_inner, Type::Slice { .. })
                                {
                                    if let Type::Array {
                                        inner: ref init_elem,
                                        ..
                                    } = **init_inner
                                    {
                                        if let Type::Slice {
                                            inner: ref ann_elem,
                                        } = **ann_inner
                                        {
                                            init_mut == ann_mut && types_equal(init_elem, ann_elem)
                                        } else {
                                            false
                                        }
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        // Check for numeric type coercion
                        let numeric_coerced =
                            Self::can_coerce_numeric(&resolved_init_type, type_ann);

                        // Check for array element type coercion: [int; N] can be coerced to [u8; N] if element types can be coerced
                        let array_elem_coerced = if let Type::Array {
                            inner: init_elem,
                            size: init_size,
                        } = &resolved_init_type
                        {
                            if let Type::Array {
                                inner: ann_elem,
                                size: ann_size,
                            } = type_ann
                            {
                                init_size == ann_size
                                    && Self::can_coerce_numeric(init_elem, ann_elem)
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        if !coerced
                            && !numeric_coerced
                            && !array_elem_coerced
                            && !types_equal(&resolved_init_type, type_ann)
                        {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: type_to_string(type_ann),
                                got: type_to_string(&init_type),
                                span: let_stmt.span,
                            });
                        }
                    }

                    // For minimal subset, infer type from init
                    let var_type = resolved_type_ann.as_ref().unwrap_or(&init_type).clone();

                    // Then mark moves in the initializer expression
                    // This handles cases like `let y = x;` where `x` is moved to `y`
                    self.check_expr_for_moves(init)?;

                    // Lasting borrows from `let r = &x` / `let r = &mut s.field`, etc.
                    if let Expr::Ref(ref_expr) = init
                        && let Some((owner, span)) = self.borrow_owner_from_expr(&ref_expr.inner)
                    {
                        self.register_borrow(&owner, ref_expr.mutable, span)?;
                    }

                    // New variable starts as Valid (it owns the value from init)
                    self.variables.insert(
                        let_stmt.name.clone(),
                        Self::new_variable_info(var_type.clone(), let_stmt.span),
                    );
                    if !let_stmt.name.is_empty() {
                        self.record_expr_type(let_stmt.name_span, &var_type);
                    }
                } else if let Some(ref type_ann) = let_stmt.type_ann {
                    // Resolve type annotation - convert Struct(name) to Enum(name) if it's actually an enum
                    let resolved_type = self.resolve_type_name(type_ann)?;

                    // If this is a channel type, enforce that its element type is Send.
                    if let Type::Channel { elem_type } = &resolved_type
                        && !self.is_send(elem_type)
                    {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "Send element type for channel".to_string(),
                            got: type_to_string(elem_type),
                            span: let_stmt.span,
                        });
                    }
                    // Variable declared without init - store the type, state is Valid but uninitialized
                    self.variables.insert(
                        let_stmt.name.clone(),
                        Self::new_variable_info(resolved_type.clone(), let_stmt.span),
                    );
                    if !let_stmt.name.is_empty() {
                        self.record_expr_type(let_stmt.name_span, &resolved_type);
                    }
                } else {
                    return Err(TypeCheckError::Message(format!(
                        "Variable '{}' must have either a type annotation or initializer",
                        let_stmt.name
                    )));
                }
            }
            Stmt::Return(return_stmt) => {
                if let Some(ref value) = return_stmt.value {
                    // First check the expression type (this verifies variables are Valid)
                    let value_type = self.check_expr(value)?;
                    let resolved_value_type = self.resolve_type_name(&value_type)?;

                    // Check no-escape rule: cannot return reference-containing types
                    if self.is_reference_containing(&resolved_value_type) {
                        return Err(TypeCheckError::ReferenceEscape {
                            description: "Cannot return a reference type (violates no-escape rule)"
                                .to_string(),
                            span: return_stmt.span,
                        });
                    }

                    // Check return type matches function return type
                    if let Some(ref expected_return_type) = self.current_return_type {
                        // Check for numeric coercion
                        let numeric_coerced =
                            Self::can_coerce_numeric(&resolved_value_type, expected_return_type);

                        if !numeric_coerced
                            && !types_equal(&resolved_value_type, expected_return_type)
                        {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: type_to_string(expected_return_type),
                                got: type_to_string(&value_type),
                                span: return_stmt.span,
                            });
                        }
                    }

                    // Then mark variables as moved (return consumes the value)
                    self.check_expr_for_moves(value)?;
                } else {
                    // Return without value - check that function expects no return type
                    if self.current_return_type.is_some() {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "return value".to_string(),
                            got: "no return value".to_string(),
                            span: return_stmt.span,
                        });
                    }
                }
            }
            Stmt::Break(_) => {
                if self.loop_depth == 0 {
                    return Err(TypeCheckError::Message(
                        "break statement outside of loop".to_string(),
                    ));
                }
            }
            Stmt::Continue(_) => {
                if self.loop_depth == 0 {
                    return Err(TypeCheckError::Message(
                        "continue statement outside of loop".to_string(),
                    ));
                }
            }
            Stmt::Expr(expr_stmt) => {
                self.check_expr(&expr_stmt.expr)?;
                self.check_expr_for_moves(&expr_stmt.expr)?;
            }
            Stmt::Defer(defer_stmt) => {
                // A defer just needs its expression to be well-typed; it
                // otherwise behaves like a normal expression for TC purposes.
                // Any reference/no-escape issues are already enforced by
                // `check_expr`, and move semantics are handled at the use site.
                self.check_expr(&defer_stmt.expr)?;
                self.check_expr_for_moves(&defer_stmt.expr)?;
            }
            Stmt::If(if_stmt) => {
                // Save environment before the condition: checking the condition must not
                // consume operands (e.g. `if x != 1` only borrows `x` for comparison).
                let before = self.variables.clone();
                let cond_ty = self.check_expr(&if_stmt.cond)?;
                if !types_equal(&cond_ty, &Type::Bool) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "bool (if condition)".to_string(),
                        got: type_to_string(&cond_ty),
                        span: if_stmt.span,
                    });
                }
                self.variables = before.clone();

                // Then-branch: type-check with its own copy of the environment
                self.variables = before.clone();
                self.push_borrow_scope();
                for inner in &if_stmt.then_block.statements {
                    self.check_stmt(inner)?;
                }
                self.pop_borrow_scope();
                let then_env = self.variables.clone();

                // Else-branch: same starting environment; if no else, treat as no-op.
                let else_env = if let Some(ref else_blk) = if_stmt.else_block {
                    self.variables = before.clone();
                    self.push_borrow_scope();
                    for inner in &else_blk.statements {
                        self.check_stmt(inner)?;
                    }
                    self.pop_borrow_scope();
                    self.variables.clone()
                } else {
                    before.clone()
                };

                // Merge environments: only branches that can fall through to code
                // after this `if` must agree on ownership (diverging branches are skipped).
                let mut merged = before.clone();
                for (name, prev_info) in before.iter() {
                    let then_state = then_env
                        .get(name)
                        .map(|info| info.state)
                        .unwrap_or(prev_info.state);
                    let else_state = else_env
                        .get(name)
                        .map(|info| info.state)
                        .unwrap_or(prev_info.state);

                    let mut reach_states: Vec<OwnershipState> = Vec::new();
                    if if_stmt.else_block.is_none() {
                        reach_states.push(prev_info.state);
                    }
                    if block_falls_through(&if_stmt.then_block) {
                        reach_states.push(then_state);
                    }
                    if let Some(ref else_blk) = if_stmt.else_block
                        && block_falls_through(else_blk)
                    {
                        reach_states.push(else_state);
                    }

                    let merged_state = if reach_states.is_empty() {
                        prev_info.state
                    } else if reach_states.iter().all(|s| *s == OwnershipState::Valid) {
                        OwnershipState::Valid
                    } else if reach_states.iter().all(|s| *s == OwnershipState::Moved) {
                        OwnershipState::Moved
                    } else {
                        return Err(TypeCheckError::UseAfterMove {
                            name: name.clone(),
                            span: if_stmt.span,
                        });
                    };

                    if let Some(info) = merged.get_mut(name) {
                        info.state = merged_state;
                        info.shared_borrow_count = prev_info.shared_borrow_count;
                        info.mut_borrow_count = prev_info.mut_borrow_count;
                    }
                }

                self.variables = merged;
            }
            Stmt::Spawn(spawn_stmt) => {
                // Discover captured variables: any variable from the current scope
                // that is referenced inside the spawn body.
                let captured_names = collect_captured_vars(&spawn_stmt.body);

                // Validate captures: must exist, be Valid, and be Send.
                let mut captured_infos = Vec::new();
                for name in captured_names {
                    if let Some(info) = self.variables.get(&name) {
                        if info.state == OwnershipState::Moved {
                            return Err(TypeCheckError::UseAfterMove {
                                name: name.clone(),
                                span: spawn_stmt.span,
                            });
                        }
                        if !self.is_send(&info.ty) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "Send value for spawn capture".to_string(),
                                got: type_to_string(&info.ty),
                                span: spawn_stmt.span,
                            });
                        }
                        self.check_owner_not_borrowed(&name, spawn_stmt.span)?;
                        captured_infos.push((name.clone(), info.ty.clone()));
                    } else {
                        return Err(TypeCheckError::UndefinedVariable {
                            name: name.clone(),
                            span: spawn_stmt.span,
                        });
                    }
                }

                // Mark captured variables as moved in the parent scope.
                for (name, _) in &captured_infos {
                    if let Some(info) = self.variables.get_mut(name) {
                        info.state = OwnershipState::Moved;
                    }
                }

                // Type-check the spawn body in an isolated scope seeded with the captured vars.
                let parent_vars = self.variables.clone();
                self.variables.clear();
                for (name, ty) in captured_infos {
                    self.variables
                        .insert(name, Self::new_variable_info(ty, spawn_stmt.span));
                }
                // Check the body of the spawn block
                self.push_borrow_scope();
                for inner in &spawn_stmt.body.statements {
                    self.check_stmt(inner)?;
                }
                self.pop_borrow_scope();
                // Restore parent scope (with captured variables already marked moved).
                self.variables = parent_vars;
            }
            Stmt::While(while_stmt) => {
                let before = self.variables.clone();
                let cond_ty = self.check_expr(&while_stmt.cond)?;
                if !types_equal(&cond_ty, &Type::Bool) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "bool (while condition)".to_string(),
                        got: type_to_string(&cond_ty),
                        span: while_stmt.span,
                    });
                }
                self.variables = before.clone();
                self.loop_depth += 1;
                self.push_borrow_scope();
                for inner in &while_stmt.body.statements {
                    self.check_stmt(inner)?;
                }
                self.pop_borrow_scope();
                self.loop_depth -= 1;
                let body_env = self.variables.clone();

                // Merge pessimistically for repeated iteration: a variable may only stay
                // Valid if it is Valid both before the loop and after one abstract pass
                // through the body. If it is moved in the body, later iterations or uses
                // after the loop would be invalid (same rule spirit as if/else merge).
                let mut merged = before.clone();
                for (name, prev_info) in before.iter() {
                    let body_state = body_env
                        .get(name)
                        .map(|info| info.state)
                        .unwrap_or(prev_info.state);

                    let merged_state = match (prev_info.state, body_state) {
                        (OwnershipState::Valid, OwnershipState::Valid) => OwnershipState::Valid,
                        (OwnershipState::Moved, OwnershipState::Moved) => OwnershipState::Moved,
                        _ => {
                            return Err(TypeCheckError::UseAfterMove {
                                name: name.clone(),
                                span: while_stmt.span,
                            });
                        }
                    };

                    if let Some(info) = merged.get_mut(name) {
                        info.state = merged_state;
                        info.shared_borrow_count = prev_info.shared_borrow_count;
                        info.mut_borrow_count = prev_info.mut_borrow_count;
                    }
                }

                self.variables = merged;
            }
            Stmt::Loop(loop_stmt) => {
                let before = self.variables.clone();
                self.loop_depth += 1;
                self.push_borrow_scope();
                for inner in &loop_stmt.body.statements {
                    self.check_stmt(inner)?;
                }
                self.pop_borrow_scope();
                self.loop_depth -= 1;
                let body_env = self.variables.clone();

                let mut merged = before.clone();
                for (name, prev_info) in before.iter() {
                    let body_state = body_env
                        .get(name)
                        .map(|info| info.state)
                        .unwrap_or(prev_info.state);

                    let merged_state = match (prev_info.state, body_state) {
                        (OwnershipState::Valid, OwnershipState::Valid) => OwnershipState::Valid,
                        (OwnershipState::Moved, OwnershipState::Moved) => OwnershipState::Moved,
                        _ => {
                            return Err(TypeCheckError::UseAfterMove {
                                name: name.clone(),
                                span: loop_stmt.span,
                            });
                        }
                    };

                    if let Some(info) = merged.get_mut(name) {
                        info.state = merged_state;
                        info.shared_borrow_count = prev_info.shared_borrow_count;
                        info.mut_borrow_count = prev_info.mut_borrow_count;
                    }
                }

                self.variables = merged;
            }
            Stmt::For(for_stmt) => {
                // Desugar: for x in container { body }
                // to:
                //   let mut __i = 0;
                //   while __i < container.len() {
                //       let x = container.get(__i).unwrap();
                //       body;
                //       __i = __i + 1;
                //   }

                // Check iterable type - must be Vec<T>, String, or Array<T>
                let iterable_type = self.check_expr(&for_stmt.iterable)?;
                let elem_type = match iterable_type {
                    Type::Vec { ref elem_type } => (**elem_type).clone(),
                    Type::String => Type::U8,
                    Type::Array { ref inner, .. } => (**inner).clone(),
                    other => {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "Vec<T>, String, or Array<T>".to_string(),
                            got: type_to_string(&other),
                            span: for_stmt.iterable.span(),
                        });
                    }
                };

                // Move the container into a temporary variable in the loop scope
                let container_var = format!("__for_container_{}", for_stmt.span.start);
                let container_expr = for_stmt.iterable.clone();

                // Create the index variable name (mangled to avoid conflicts)
                let index_var = format!("__for_i_{}", for_stmt.span.start);

                // Create spans for desugared code
                let index_init_span = for_stmt.span;

                // First, initialize the container variable: let container_var = container_expr;
                let container_init_stmt = Stmt::Let(LetStmt {
                    name: container_var.clone(),
                    name_span: index_init_span,
                    patterns: None,
                    mutable: false,
                    type_ann: None,
                    init: Some(container_expr.clone()),
                    span: index_init_span,
                });

                // Check the container initialization first (this moves the container)
                // check_stmt will handle the move checking internally
                self.check_stmt(&container_init_stmt)?;

                // Initialize the index variable: let mut __i = 0;
                let index_init_stmt = Stmt::Let(LetStmt {
                    name: index_var.clone(),
                    name_span: index_init_span,
                    patterns: None,
                    mutable: true,
                    type_ann: None,
                    init: Some(Expr::Lit(LitExpr {
                        value: 0,
                        span: index_init_span,
                    })),
                    span: index_init_span,
                });

                // Check the index initialization
                self.check_stmt(&index_init_stmt)?;

                // Now create expressions that reference the initialized variables
                // Create: container.len() - using the container variable
                let container_var_expr = Expr::Var(VarExpr {
                    name: container_var.clone(),
                    span: index_init_span,
                });

                let index_var_ref = Expr::Var(VarExpr {
                    name: index_var.clone(),
                    span: index_init_span,
                });

                let cond_expr = match &iterable_type {
                    Type::Array { size, .. } => Expr::BinOp(BinOpExpr {
                        op: BinOp::Lt,
                        left: Box::new(index_var_ref.clone()),
                        right: Box::new(Expr::Lit(LitExpr {
                            value: *size as i64,
                            span: index_init_span,
                        })),
                        span: index_init_span,
                    }),
                    Type::Vec { .. } | Type::String => {
                        let len_callee = match iterable_type {
                            Type::Vec { .. } => "Vec::len",
                            Type::String => "String::len",
                            _ => unreachable!(),
                        };
                        Expr::BinOp(BinOpExpr {
                            op: BinOp::Lt,
                            left: Box::new(index_var_ref.clone()),
                            right: Box::new(Expr::Call(CallExpr {
                                callee: len_callee.to_string(),
                                args: vec![Expr::Ref(RefExpr {
                                    mutable: false,
                                    inner: Box::new(container_var_expr.clone()),
                                    span: index_init_span,
                                })],
                                span: index_init_span,
                                callee_span: index_init_span,
                            })),
                            span: index_init_span,
                        })
                    }
                    _ => unreachable!(),
                };

                let mut desugared_body = Block { statements: vec![] };

                match iterable_type {
                    Type::Vec { .. } => {
                        let opt_var = format!("__for_opt_{}", for_stmt.span.start);
                        let get_expr = Expr::Call(CallExpr {
                            callee: "Vec::get".to_string(),
                            args: vec![
                                Expr::Ref(RefExpr {
                                    mutable: false,
                                    inner: Box::new(container_var_expr.clone()),
                                    span: index_init_span,
                                }),
                                index_var_ref.clone(),
                            ],
                            span: index_init_span,
                            callee_span: index_init_span,
                        });
                        desugared_body.statements.push(Stmt::Let(LetStmt {
                            name: opt_var.clone(),
                            name_span: index_init_span,
                            patterns: None,
                            mutable: false,
                            type_ann: Some(Type::Generic {
                                name: "Option".to_string(),
                                params: vec![elem_type.clone()],
                            }),
                            init: Some(get_expr),
                            span: index_init_span,
                        }));
                        let mut match_body = Block { statements: vec![] };
                        for stmt in &for_stmt.body.statements {
                            match_body.statements.push(stmt.clone());
                        }
                        match_body.statements.push(Stmt::Expr(ExprStmt {
                            expr: Expr::Assign(AssignExpr {
                                target: Box::new(Expr::Var(VarExpr {
                                    name: index_var.clone(),
                                    span: index_init_span,
                                })),
                                value: Box::new(Expr::BinOp(BinOpExpr {
                                    op: BinOp::Add,
                                    left: Box::new(index_var_ref.clone()),
                                    right: Box::new(Expr::Lit(LitExpr {
                                        value: 1,
                                        span: index_init_span,
                                    })),
                                    span: index_init_span,
                                })),
                                span: index_init_span,
                            }),
                        }));
                        desugared_body.statements.push(Stmt::Expr(ExprStmt {
                            expr: Expr::Match(MatchExpr {
                                expr: Box::new(Expr::Var(VarExpr {
                                    name: opt_var,
                                    span: index_init_span,
                                })),
                                arms: vec![
                                    MatchArm {
                                        pattern: Pattern::Variant {
                                            enum_name: "Option".to_string(),
                                            variant: "Some".to_string(),
                                            sub_patterns: vec![Pattern::Binding {
                                                name: for_stmt.var_name.clone(),
                                                span: index_init_span,
                                            }],
                                            named_fields: None,
                                            span: index_init_span,
                                        },
                                        guard: None,
                                        body: match_body,
                                        span: index_init_span,
                                    },
                                    MatchArm {
                                        pattern: Pattern::Variant {
                                            enum_name: "Option".to_string(),
                                            variant: "None".to_string(),
                                            sub_patterns: vec![],
                                            named_fields: None,
                                            span: index_init_span,
                                        },
                                        guard: None,
                                        body: Block { statements: vec![] },
                                        span: index_init_span,
                                    },
                                ],
                                span: index_init_span,
                            }),
                        }));
                    }
                    Type::Array { .. } | Type::String => {
                        desugared_body.statements.push(Stmt::Let(LetStmt {
                            name: for_stmt.var_name.clone(),
                            name_span: index_init_span,
                            patterns: None,
                            mutable: false,
                            type_ann: Some(elem_type.clone()),
                            init: Some(Expr::Index(IndexExpr {
                                target: Box::new(container_var_expr.clone()),
                                index: Box::new(index_var_ref.clone()),
                                span: index_init_span,
                            })),
                            span: index_init_span,
                        }));
                        for stmt in &for_stmt.body.statements {
                            desugared_body.statements.push(stmt.clone());
                        }
                        desugared_body.statements.push(Stmt::Expr(ExprStmt {
                            expr: Expr::Assign(AssignExpr {
                                target: Box::new(Expr::Var(VarExpr {
                                    name: index_var.clone(),
                                    span: index_init_span,
                                })),
                                value: Box::new(Expr::BinOp(BinOpExpr {
                                    op: BinOp::Add,
                                    left: Box::new(index_var_ref),
                                    right: Box::new(Expr::Lit(LitExpr {
                                        value: 1,
                                        span: index_init_span,
                                    })),
                                    span: index_init_span,
                                })),
                                span: index_init_span,
                            }),
                        }));
                    }
                    _ => unreachable!(),
                }

                // Create the while loop
                let while_stmt = Stmt::While(WhileStmt {
                    cond: cond_expr,
                    body: desugared_body,
                    span: for_stmt.span,
                });

                // Check the while loop (which will check the body)
                // Variables are already initialized above
                self.check_stmt(&while_stmt)?;
            }
            Stmt::UnsafeBlock(unsafe_stmt) => {
                // Enter unsafe context
                self.unsafe_context_depth += 1;

                // Check body
                self.push_borrow_scope();
                for inner in &unsafe_stmt.body.statements {
                    self.check_stmt(inner)?;
                }
                self.pop_borrow_scope();

                // Exit unsafe context
                self.unsafe_context_depth -= 1;
            }
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        let ty = self.check_expr_with_context(expr, false)?;
        self.record_expr_type(expr.span(), &ty);
        Ok(ty)
    }

    fn check_expr_borrow_operand(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        self.check_expr_with_context(expr, true)
    }

    fn check_expr_with_context(
        &mut self,
        expr: &Expr,
        borrow_operand: bool,
    ) -> Result<Type, TypeCheckError> {
        match expr {
            Expr::Lit(_) => Ok(Type::Int),
            Expr::BoolLiteral(_) => Ok(Type::Bool),
            Expr::FloatLiteral(_) => Ok(Type::F64), // Float literals default to f64
            Expr::StringLit(_) => Ok(Type::String),
            Expr::Var(var_expr) => {
                // Check if this is a qualified name (mod::item)
                if var_expr.name.contains("::") {
                    let parts: Vec<&str> = var_expr.name.split("::").collect();
                    if parts.len() != 2 {
                        return Err(TypeCheckError::Message(format!(
                            "Invalid qualified name: {}",
                            var_expr.name
                        )));
                    }
                    let module_name = parts[0];
                    let item_name = parts[1];

                    // Look up module in imports
                    let module_exports = self.module_imports.get(module_name).ok_or_else(|| {
                        TypeCheckError::UndefinedVariable {
                            name: format!("module '{}'", module_name),
                            span: var_expr.span,
                        }
                    })?;

                    // Look up item in module's exports
                    // For now, we only support functions from modules via VarExpr
                    // (This is unusual - typically functions are called, not referenced as variables)
                    // Structs and enums would be accessed via type names, not VarExpr
                    if let Some(&is_public) = module_exports.all_functions.get(item_name) {
                        if !is_public {
                            return Err(TypeCheckError::Message(format!(
                                "Cannot access non-public function '{}::{}' from module '{}'. Add 'pub' before 'fn {}' in the module to make it accessible.",
                                module_name, item_name, module_name, item_name
                            )));
                        }
                        // Return function type for module function references
                        let func_decl = module_exports
                            .functions
                            .get(item_name)
                            .ok_or_else(|| TypeCheckError::UndefinedVariable {
                                name: format!("{}::{}", module_name, item_name),
                                span: var_expr.span,
                            })?
                            .clone();
                        let module_file = self.module_paths.get(module_name).cloned();
                        self.record_reference(
                            var_expr.span,
                            LspTarget {
                                span: func_decl.span,
                                file: module_file,
                            },
                        );
                        Ok(fn_type_from_signature(
                            &func_decl.params,
                            &func_decl.return_type,
                        ))
                    } else {
                        Err(TypeCheckError::UndefinedVariable {
                            name: format!("{}::{}", module_name, item_name),
                            span: var_expr.span,
                        })
                    }
                } else if let Some(var_info) = self.variables.get(&var_expr.name) {
                    // Check for use-after-move
                    if var_info.state == OwnershipState::Moved {
                        return Err(TypeCheckError::UseAfterMove {
                            name: var_expr.name.clone(),
                            span: var_expr.span,
                        });
                    }

                    if !borrow_operand {
                        self.check_owner_not_borrowed(&var_expr.name, var_expr.span)?;
                    }

                    let def_span = var_info.definition_span;
                    let var_ty = var_info.ty.clone();

                    // Track this reference for LSP (goto definition)
                    self.record_reference(
                        var_expr.span,
                        LspTarget {
                            span: def_span,
                            file: None,
                        },
                    );
                    self.record_expr_type(var_expr.span, &var_ty);

                    Ok(var_ty)
                } else if let Some(func_decl) = self.functions.get(&var_expr.name).cloned() {
                    self.record_reference(
                        var_expr.span,
                        LspTarget {
                            span: func_decl.span,
                            file: None,
                        },
                    );
                    Ok(fn_type_from_signature(
                        &func_decl.params,
                        &func_decl.return_type,
                    ))
                } else if let Some(extern_fn) = self.extern_functions.get(&var_expr.name) {
                    Ok(fn_type_from_signature(
                        &extern_fn.params,
                        &extern_fn.return_type,
                    ))
                } else {
                    Err(TypeCheckError::UndefinedVariable {
                        name: var_expr.name.clone(),
                        span: var_expr.span,
                    })
                }
            }
            Expr::Ref(ref_expr) => {
                if let Some((owner, span)) = self.borrow_owner_from_expr(&ref_expr.inner) {
                    self.check_borrow_allowed(&owner, ref_expr.mutable, span)?;
                }
                let inner_type = self.check_expr_borrow_operand(&ref_expr.inner)?;

                Ok(Type::Ref {
                    inner: Box::new(inner_type),
                    mutable: ref_expr.mutable,
                })
            }
            Expr::StructLit(lit) => {
                // Look up struct declaration and clone the fields so we don't
                // hold any borrows into `self` while recursing.
                let (struct_name, struct_fields) =
                    if let Some(decl) = self.structs.get(&lit.type_name) {
                        (decl.name.clone(), decl.fields.clone())
                    } else {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "known struct type".to_string(),
                            got: lit.type_name.clone(),
                            span: lit.span,
                        });
                    };

                // Check if this struct has generic parameters
                let is_generic = if let Some(decl) = self.structs.get(&struct_name) {
                    !decl.generics.is_empty()
                } else {
                    false
                };
                let generics = if let Some(decl) = self.structs.get(&struct_name) {
                    decl.generics.clone()
                } else {
                    Vec::new()
                };

                // Check that all fields provided correspond to struct fields
                for field_expr in &lit.fields {
                    let field_decl_ty = struct_fields
                        .iter()
                        .find(|f| f.name == field_expr.name)
                        .map(|f| f.ty.clone())
                        .ok_or_else(|| TypeCheckError::TypeMismatch {
                            expected: format!("field of struct '{}'", struct_name),
                            got: field_expr.name.clone(),
                            span: field_expr.span,
                        })?;

                    let value_ty = self.check_expr(&field_expr.value)?;
                    let resolved_value_ty = self.resolve_type_name(&value_ty)?;
                    let resolved_field_ty = self.resolve_type_name(&field_decl_ty)?;

                    // If this is a generic struct and the field type is a generic parameter, allow any type
                    let types_match = if is_generic {
                        // Check if field_decl_ty is a generic parameter (e.g., Type::Struct("T"))
                        if let Type::Struct(param_name) = &field_decl_ty {
                            // If this is a generic parameter name, allow any type
                            generics.contains(param_name)
                        } else {
                            // Check for array element type coercion
                            let array_elem_coerced = if let Type::Array {
                                inner: value_elem,
                                size: value_size,
                            } = &resolved_value_ty
                            {
                                if let Type::Array {
                                    inner: field_elem,
                                    size: field_size,
                                } = &resolved_field_ty
                                {
                                    value_size == field_size
                                        && Self::can_coerce_numeric(value_elem, field_elem)
                                } else {
                                    false
                                }
                            } else {
                                false
                            };

                            // Check for numeric coercion
                            let numeric_coerced =
                                Self::can_coerce_numeric(&resolved_value_ty, &resolved_field_ty);

                            array_elem_coerced
                                || numeric_coerced
                                || types_equal(&resolved_value_ty, &resolved_field_ty)
                        }
                    } else {
                        // Check for array element type coercion
                        let array_elem_coerced = if let Type::Array {
                            inner: value_elem,
                            size: value_size,
                        } = &resolved_value_ty
                        {
                            if let Type::Array {
                                inner: field_elem,
                                size: field_size,
                            } = &resolved_field_ty
                            {
                                value_size == field_size
                                    && Self::can_coerce_numeric(value_elem, field_elem)
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        // Check for numeric coercion
                        let numeric_coerced =
                            Self::can_coerce_numeric(&resolved_value_ty, &resolved_field_ty);

                        array_elem_coerced
                            || numeric_coerced
                            || types_equal(&resolved_value_ty, &resolved_field_ty)
                    };

                    if !types_match {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: type_to_string(&field_decl_ty),
                            got: type_to_string(&value_ty),
                            span: field_expr.span,
                        });
                    }
                }

                // For now we don't enforce that all fields are initialized; minimal subset.

                Ok(Type::Struct(struct_name))
            }
            Expr::FieldAccess(acc) => {
                let base_ty = self.check_expr_with_context(&acc.base, borrow_operand)?;
                match base_ty {
                    Type::Struct(ref name) => {
                        if self.is_type_param(name) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "concrete struct value for field access".to_string(),
                                got: format!("type parameter '{}'", name),
                                span: acc.span,
                            });
                        }
                        let decl =
                            self.structs
                                .get(name)
                                .ok_or_else(|| TypeCheckError::TypeMismatch {
                                    expected: "known struct type".to_string(),
                                    got: name.clone(),
                                    span: acc.span,
                                })?;
                        let field = decl
                            .fields
                            .iter()
                            .find(|f| f.name == acc.field)
                            .ok_or_else(|| TypeCheckError::TypeMismatch {
                                expected: format!("field of struct '{}'", decl.name),
                                got: acc.field.clone(),
                                span: acc.span,
                            })?;
                        let field_ty = field.ty.clone();
                        let struct_name = name.clone();
                        let field_name = acc.field.clone();
                        if let Some(target) = self.lookup_field_target(&struct_name, &field_name) {
                            self.record_reference(acc.field_span, target);
                        }
                        Ok(field_ty)
                    }
                    Type::Generic { name, params } => {
                        let decl = self.structs.get(&name).ok_or_else(|| {
                            TypeCheckError::TypeMismatch {
                                expected: "known struct type".to_string(),
                                got: name.clone(),
                                span: acc.span,
                            }
                        })?;
                        if decl.generics.len() != params.len() {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: format!(
                                    "{} generic parameters for struct '{}'",
                                    decl.generics.len(),
                                    name
                                ),
                                got: format!("{} parameters", params.len()),
                                span: acc.span,
                            });
                        }
                        let substitutions = decl
                            .generics
                            .iter()
                            .zip(params.iter())
                            .map(|(param_name, concrete)| (param_name.clone(), concrete.clone()))
                            .collect::<std::collections::HashMap<_, _>>();
                        let field = decl
                            .fields
                            .iter()
                            .find(|f| f.name == acc.field)
                            .ok_or_else(|| TypeCheckError::TypeMismatch {
                                expected: format!("field of struct '{}'", decl.name),
                                got: acc.field.clone(),
                                span: acc.span,
                            })?;
                        let field_ty = substitute_generic_types_impl(&field.ty, &substitutions);
                        let struct_name = name.clone();
                        let field_name = acc.field.clone();
                        if let Some(target) = self.lookup_field_target(&struct_name, &field_name) {
                            self.record_reference(acc.field_span, target);
                        }
                        Ok(field_ty)
                    }
                    Type::String => match acc.field.as_str() {
                        "data" => Ok(Type::RawPtr {
                            inner: Box::new(Type::U8),
                        }),
                        "len" => Ok(Type::Int),
                        _ => Err(TypeCheckError::TypeMismatch {
                            expected: "field 'data' or 'len' on String".to_string(),
                            got: acc.field.clone(),
                            span: acc.span,
                        }),
                    },
                    Type::Tuple { elements } => {
                        let index: usize =
                            acc.field
                                .parse()
                                .map_err(|_| TypeCheckError::TypeMismatch {
                                    expected: "numeric tuple field index".to_string(),
                                    got: acc.field.clone(),
                                    span: acc.span,
                                })?;
                        if index >= elements.len() {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: format!(
                                    "tuple field index 0..{}",
                                    elements.len().saturating_sub(1)
                                ),
                                got: acc.field.clone(),
                                span: acc.span,
                            });
                        }
                        Ok(elements[index].clone())
                    }
                    other => Err(TypeCheckError::TypeMismatch {
                        expected: "struct value for field access".to_string(),
                        got: type_to_string(&other),
                        span: acc.span,
                    }),
                }
            }
            Expr::BinOp(bin_op_expr) => {
                let left_type = self.check_expr(&bin_op_expr.left)?;
                let right_type = self.check_expr(&bin_op_expr.right)?;

                match bin_op_expr.op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                        // Arithmetic operations
                        self.check_arithmetic_op(&left_type, &right_type, &bin_op_expr.span)
                    }
                    BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge | BinOp::Eq | BinOp::Ne => {
                        // Comparison operations - return bool
                        // Allow comparing numeric types (even if different) or equal non-numeric types
                        if (self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type))
                            || types_equal(&left_type, &right_type)
                        {
                            Ok(Type::Bool)
                        } else {
                            Err(TypeCheckError::TypeMismatch {
                                expected: "comparable types".to_string(),
                                got: format!(
                                    "{} and {}",
                                    type_to_string(&left_type),
                                    type_to_string(&right_type)
                                ),
                                span: bin_op_expr.span,
                            })
                        }
                    }
                    BinOp::And | BinOp::Or => {
                        // Logical AND and OR - require bool operands, return bool
                        if !types_equal(&left_type, &Type::Bool) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "bool".to_string(),
                                got: type_to_string(&left_type),
                                span: bin_op_expr.left.span(),
                            });
                        }
                        if !types_equal(&right_type, &Type::Bool) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "bool".to_string(),
                                got: type_to_string(&right_type),
                                span: bin_op_expr.right.span(),
                            });
                        }
                        Ok(Type::Bool)
                    }
                    BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor => {
                        // Bitwise operations - require integer operands
                        if !self.is_integer_type(&left_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "integer type".to_string(),
                                got: type_to_string(&left_type),
                                span: bin_op_expr.left.span(),
                            });
                        }
                        if !self.is_integer_type(&right_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "integer type".to_string(),
                                got: type_to_string(&right_type),
                                span: bin_op_expr.right.span(),
                            });
                        }
                        // Result type follows integer promotion rules (use the larger type)
                        self.promote_integer_types(&left_type, &right_type)
                    }
                    BinOp::ShiftLeft | BinOp::ShiftRight => {
                        // Shift operations - left operand must be integer, right must be unsigned integer
                        if !self.is_integer_type(&left_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "integer type".to_string(),
                                got: type_to_string(&left_type),
                                span: bin_op_expr.left.span(),
                            });
                        }
                        if !self.is_unsigned_integer_type(&right_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "unsigned integer type".to_string(),
                                got: type_to_string(&right_type),
                                span: bin_op_expr.right.span(),
                            });
                        }
                        // Result type is the type of the left operand
                        Ok(left_type)
                    }
                }
            }
            Expr::UnOp(un_op_expr) => {
                let operand_type = self.check_expr(&un_op_expr.operand)?;
                match un_op_expr.op {
                    UnOp::Not => {
                        // Logical NOT - requires bool operand, returns bool
                        if !types_equal(&operand_type, &Type::Bool) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "bool".to_string(),
                                got: type_to_string(&operand_type),
                                span: un_op_expr.operand.span(),
                            });
                        }
                        Ok(Type::Bool)
                    }
                    UnOp::Neg => {
                        // Unary minus - requires numeric operand, returns same type
                        if !self.is_numeric_type(&operand_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "numeric type".to_string(),
                                got: type_to_string(&operand_type),
                                span: un_op_expr.operand.span(),
                            });
                        }
                        Ok(operand_type)
                    }
                }
            }
            Expr::Send(send_expr) => {
                // send(sender, value): sender must be &Sender<T>, value must have type T,
                // and T must be Send. Returns int (status code).
                let sender_ref_type = self.check_expr(&send_expr.channel)?;
                let (elem_type, span) = match sender_ref_type {
                    Type::Ref {
                        inner,
                        mutable: false,
                    } => match *inner {
                        Type::Sender { ref elem_type } => ((*elem_type).clone(), send_expr.span),
                        other => {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "&Sender<T>".to_string(),
                                got: type_to_string(&other),
                                span: send_expr.span,
                            });
                        }
                    },
                    other => {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "&Sender<T>".to_string(),
                            got: type_to_string(&other),
                            span: send_expr.span,
                        });
                    }
                };

                let value_type = self.check_expr(&send_expr.value)?;
                if !types_equal(&value_type, &elem_type) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: type_to_string(&elem_type),
                        got: type_to_string(&value_type),
                        span,
                    });
                }

                // Element type must be Send
                if !self.is_send(&elem_type) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: format!(
                            "Send element type for channel, got {}",
                            type_to_string(&elem_type)
                        ),
                        got: type_to_string(&elem_type),
                        span,
                    });
                }

                Ok(Type::Int)
            }
            Expr::Recv(recv_expr) => {
                // recv(receiver): receiver must be &mut Receiver<T>, expression type is T.
                let receiver_mut_ref_type = self.check_expr(&recv_expr.channel)?;
                match receiver_mut_ref_type {
                    Type::Ref {
                        inner,
                        mutable: true,
                    } => match *inner {
                        Type::Receiver { ref elem_type } => Ok((**elem_type).clone()),
                        other => Err(TypeCheckError::TypeMismatch {
                            expected: "&mut Receiver<T>".to_string(),
                            got: type_to_string(&other),
                            span: recv_expr.span,
                        }),
                    },
                    other => Err(TypeCheckError::TypeMismatch {
                        expected: "&mut Receiver<T>".to_string(),
                        got: type_to_string(&other),
                        span: recv_expr.span,
                    }),
                }
            }
            Expr::EnumLit(enum_lit) => {
                // Check if this is actually a qualified function call that was mis-parsed
                // This happens when mod::func(...) is parsed as EnumLit instead of CallExpr
                if let Some(module_exports) = self.module_imports.get(&enum_lit.enum_name) {
                    // This is actually a qualified function call, not an enum literal
                    // First check if the function exists (public or private)
                    if let Some(&is_public) = module_exports.all_functions.get(&enum_lit.variant) {
                        if !is_public {
                            return Err(TypeCheckError::Message(format!(
                                "Cannot access non-public function '{}::{}' from module '{}'. Add 'pub' before 'fn {}' in the module to make it accessible.",
                                enum_lit.enum_name,
                                enum_lit.variant,
                                enum_lit.enum_name,
                                enum_lit.variant
                            )));
                        }
                    } else {
                        return Err(TypeCheckError::UndefinedVariable {
                            name: format!("{}::{}", enum_lit.enum_name, enum_lit.variant),
                            span: enum_lit.span,
                        });
                    }

                    // Get the function declaration (we know it's public now)
                    let func_decl = module_exports
                        .functions
                        .get(&enum_lit.variant)
                        .ok_or_else(|| {
                            // This shouldn't happen if all_functions is correct, but handle it gracefully
                            TypeCheckError::UndefinedVariable {
                                name: format!("{}::{}", enum_lit.enum_name, enum_lit.variant),
                                span: enum_lit.span,
                            }
                        })?
                        .clone();

                    // Check argument count
                    if enum_lit.args.len() != func_decl.params.len() {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: format!("{} arguments", func_decl.params.len()),
                            got: format!("{} arguments", enum_lit.args.len()),
                            span: enum_lit.span,
                        });
                    }

                    // Clone params to avoid borrow issues
                    let params = func_decl.params.clone();
                    let return_type = func_decl.return_type.clone();

                    // Check argument types (now we can call self.check_expr since we've dropped the module_exports borrow)
                    for (arg_expr, param) in enum_lit.args.iter().zip(params.iter()) {
                        let arg_ty = self.check_expr(arg_expr)?;
                        let resolved_arg_ty = self.resolve_type_name(&arg_ty)?;
                        let resolved_param_ty = self.resolve_type_name(&param.ty)?;
                        let numeric_coerced =
                            Self::can_coerce_numeric(&resolved_arg_ty, &resolved_param_ty);
                        if !numeric_coerced && !types_equal(&resolved_arg_ty, &resolved_param_ty) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: type_to_string(&resolved_param_ty),
                                got: type_to_string(&resolved_arg_ty),
                                span: arg_expr.span(),
                            });
                        }
                    }

                    // Return the function's return type
                    let callee = format!("{}::{}", enum_lit.enum_name, enum_lit.variant);
                    self.record_reference(
                        enum_lit.variant_span,
                        LspTarget {
                            span: func_decl.span,
                            file: self.module_paths.get(&enum_lit.enum_name).cloned(),
                        },
                    );
                    self.record_signature(
                        enum_lit.span,
                        Self::fn_hover_doc(
                            &callee,
                            &func_decl.generics,
                            &func_decl.params,
                            &func_decl.return_type,
                        ),
                        None,
                    );
                    return Ok(return_type.unwrap_or(Type::Int));
                }

                // Verify enum exists and clone to avoid borrow issues
                let enum_decl = self
                    .enums
                    .get(&enum_lit.enum_name)
                    .ok_or_else(|| TypeCheckError::TypeMismatch {
                        expected: "known enum type".to_string(),
                        got: enum_lit.enum_name.clone(),
                        span: enum_lit.span,
                    })?
                    .clone();

                // Verify variant exists
                let variant = enum_decl
                    .variants
                    .iter()
                    .find(|v| v.name == enum_lit.variant)
                    .ok_or_else(|| TypeCheckError::TypeMismatch {
                        expected: format!("variant of enum '{}'", enum_lit.enum_name),
                        got: enum_lit.variant.clone(),
                        span: enum_lit.span,
                    })?;

                if let Some(target) = self.lookup_type_target(&enum_lit.enum_name) {
                    self.record_reference(enum_lit.enum_name_span, target);
                }
                if let Some(target) =
                    self.lookup_variant_target(&enum_lit.enum_name, &enum_lit.variant)
                {
                    self.record_reference(enum_lit.variant_span, target);
                }

                // Check argument count matches
                if enum_lit.args.len() != variant.payload_types.len() {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: format!("{} arguments", variant.payload_types.len()),
                        got: format!("{} arguments", enum_lit.args.len()),
                        span: enum_lit.span,
                    });
                }

                // Check if this enum has generic parameters
                let is_generic = !enum_decl.generics.is_empty();

                // Check argument types match and infer generic parameters if needed
                let payload_types = variant.payload_types.clone();
                let mut inferred_params: Vec<Type> = Vec::new();

                for (arg_expr, expected_ty) in enum_lit.args.iter().zip(payload_types.iter()) {
                    let arg_ty = self.check_expr(arg_expr)?;
                    let resolved_arg_ty = self.resolve_type_name(&arg_ty)?;
                    let resolved_expected_ty = self.resolve_type_name(expected_ty)?;

                    // If this is a generic enum and the payload type is a generic parameter, infer it from the argument
                    let types_match = if is_generic {
                        // Check if expected_ty is a generic parameter (e.g., Type::Struct("T"))
                        if let Type::Struct(param_name) = &resolved_expected_ty {
                            // If this is a generic parameter name, infer it from the argument type
                            if enum_decl.generics.contains(param_name) {
                                // Find the index of this generic parameter
                                if let Some(index) =
                                    enum_decl.generics.iter().position(|g| g == param_name)
                                {
                                    // Ensure we have enough inferred params
                                    while inferred_params.len() <= index {
                                        inferred_params.push(Type::Int); // Default fallback
                                    }
                                    inferred_params[index] = resolved_arg_ty.clone();
                                }
                                true
                            } else {
                                let numeric_coerced = Self::can_coerce_numeric(
                                    &resolved_arg_ty,
                                    &resolved_expected_ty,
                                );
                                numeric_coerced
                                    || types_equal(&resolved_arg_ty, &resolved_expected_ty)
                            }
                        } else {
                            let numeric_coerced =
                                Self::can_coerce_numeric(&resolved_arg_ty, &resolved_expected_ty);
                            numeric_coerced || types_equal(&resolved_arg_ty, &resolved_expected_ty)
                        }
                    } else {
                        let numeric_coerced =
                            Self::can_coerce_numeric(&resolved_arg_ty, &resolved_expected_ty);
                        numeric_coerced || types_equal(&resolved_arg_ty, &resolved_expected_ty)
                    };

                    if !types_match {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: type_to_string(&resolved_expected_ty),
                            got: type_to_string(&resolved_arg_ty),
                            span: arg_expr.span(),
                        });
                    }
                }

                // Return the appropriate type based on whether it's generic
                if is_generic && inferred_params.len() == enum_decl.generics.len() {
                    Ok(Type::Generic {
                        name: enum_lit.enum_name.clone(),
                        params: inferred_params,
                    })
                } else {
                    Ok(Type::Enum(enum_lit.enum_name.clone()))
                }
            }
            Expr::Match(match_expr) => {
                // Check the match expression type
                let expr_ty = self.check_expr(&match_expr.expr)?;

                // Support matching on enums (both non-generic and generic)
                let (enum_name, enum_decl) = match &expr_ty {
                    Type::Enum(name) => {
                        let decl = self
                            .enums
                            .get(name)
                            .ok_or_else(|| TypeCheckError::TypeMismatch {
                                expected: "known enum type".to_string(),
                                got: name.clone(),
                                span: match_expr.span,
                            })?
                            .clone();
                        (name.clone(), decl)
                    }
                    Type::Generic { name, .. } => {
                        // Generic enums like Option<T> are represented as Generic
                        // Check if it's a known enum or a built-in generic enum
                        if self.enums.contains_key(name) {
                            let decl = self.enums.get(name).unwrap().clone();
                            (name.clone(), decl)
                        } else if name == "Option" {
                            // Option is a built-in generic enum type
                            // Create a synthetic enum declaration for Option<T>
                            let decl = EnumDecl {
                                pub_: false,
                                name: "Option".to_string(),
                                generics: vec!["T".to_string()],
                                variants: vec![
                                    EnumVariant {
                                        name: "Some".to_string(),
                                        payload_types: vec![Type::Generic {
                                            name: "T".to_string(),
                                            params: vec![],
                                        }],
                                        named_fields: None,
                                        span: match_expr.span,
                                    },
                                    EnumVariant {
                                        name: "None".to_string(),
                                        payload_types: vec![],
                                        named_fields: None,
                                        span: match_expr.span,
                                    },
                                ],
                                span: match_expr.span,
                            };
                            (name.clone(), decl)
                        } else {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "enum type for match".to_string(),
                                got: type_to_string(&expr_ty),
                                span: match_expr.span,
                            });
                        }
                    }
                    other => {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "enum type for match".to_string(),
                            got: type_to_string(other),
                            span: match_expr.span,
                        });
                    }
                };

                // Check exhaustiveness: all variants must be covered
                let mut covered_variants: Vec<String> = Vec::new();
                let mut match_result_type: Option<Type> = None;
                let expr_ty_clone = expr_ty.clone();

                for arm in &match_expr.arms {
                    // Save current scope
                    let prev_vars = self.variables.clone();

                    // Add pattern bindings to scope before checking the arm body
                    self.add_pattern_bindings(
                        &arm.pattern,
                        &enum_decl,
                        &expr_ty_clone,
                        &match_expr.expr,
                    )?;

                    if let Some(ref guard) = arm.guard {
                        let guard_ty = self.check_expr(guard)?;
                        if !types_equal(&guard_ty, &Type::Bool) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "bool (match guard)".to_string(),
                                got: type_to_string(&guard_ty),
                                span: guard.span(),
                            });
                        }
                    }

                    match &arm.pattern {
                        Pattern::Variant { variant, .. } => {
                            if !enum_decl.variants.iter().any(|v| v.name == *variant) {
                                return Err(TypeCheckError::TypeMismatch {
                                    expected: format!("variant of enum '{}'", enum_name),
                                    got: variant.clone(),
                                    span: arm.span,
                                });
                            }
                            covered_variants.push(variant.clone());
                        }
                        Pattern::Wildcard { .. } => {
                            // Wildcard covers all remaining variants
                            covered_variants
                                .extend(enum_decl.variants.iter().map(|v| v.name.clone()));
                        }
                        Pattern::Binding { .. } => {
                            // Binding pattern matches any variant
                            covered_variants
                                .extend(enum_decl.variants.iter().map(|v| v.name.clone()));
                        }
                    }

                    // Type-check the arm body and unify arm result types
                    for stmt in &arm.body.statements {
                        self.check_stmt(stmt)?;
                    }
                    let arm_ty = self.infer_block_result_type(&arm.body, arm.span)?;
                    match &match_result_type {
                        None => match_result_type = Some(arm_ty),
                        Some(prev) => {
                            match_result_type =
                                Some(self.unify_match_arm_types(prev, &arm_ty, arm.span)?);
                        }
                    }
                    self.variables = prev_vars;
                }

                // Check exhaustiveness
                for variant in &enum_decl.variants {
                    if !covered_variants.contains(&variant.name) {
                        return Err(TypeCheckError::Message(format!(
                            "Match expression is not exhaustive: variant '{}' of enum '{}' is not covered",
                            variant.name, enum_name
                        )));
                    }
                }

                Ok(match_result_type.ok_or_else(|| {
                    TypeCheckError::Message("Match expression has no arms".to_string())
                })?)
            }
            Expr::Call(call_expr) => {
                // Check if this is a built-in function first
                if let Some(return_type) = self.check_builtin_call(call_expr)? {
                    if let Some(sig) = Self::builtin_signature(&call_expr.callee) {
                        self.record_hover_doc(call_expr.callee_span, sig.to_string());
                        self.record_signature(call_expr.span, sig.to_string(), None);
                    }
                    return Ok(return_type);
                }

                // Check if this is a qualified name (mod::func)
                let (params, return_type_opt) = if call_expr.callee.contains("::") {
                    let parts: Vec<&str> = call_expr.callee.split("::").collect();
                    if parts.len() != 2 {
                        return Err(TypeCheckError::Message(format!(
                            "Invalid qualified function name: {}",
                            call_expr.callee
                        )));
                    }
                    let module_name = parts[0];
                    let func_name = parts[1];

                    // Look up module in imports or check if it's actually an enum literal that was mis-parsed
                    let module_exports = match self.module_imports.get(module_name) {
                        Some(exports) => exports,
                        None => {
                            // Not a module - check if it's an enum literal
                            // This handles cases where the parser treated Enum::Variant(...) as a qualified call
                            if let Some(enum_decl) = self.enums.get(module_name) {
                                // Clone the variant payload types to avoid borrow checker issues
                                let variant = enum_decl
                                    .variants
                                    .iter()
                                    .find(|v| v.name == func_name)
                                    .ok_or_else(|| TypeCheckError::TypeMismatch {
                                        expected: format!("variant of enum '{}'", module_name),
                                        got: func_name.to_string(),
                                        span: call_expr.span,
                                    })?;

                                let payload_types = variant.payload_types.clone();

                                // Check argument count matches variant
                                if call_expr.args.len() != payload_types.len() {
                                    return Err(TypeCheckError::TypeMismatch {
                                        expected: format!("{} arguments", payload_types.len()),
                                        got: format!("{} arguments", call_expr.args.len()),
                                        span: call_expr.span,
                                    });
                                }

                                // Check argument types (now we can call self.check_expr since we've dropped the enum_decl borrow)
                                for (arg_expr, expected_ty) in
                                    call_expr.args.iter().zip(payload_types.iter())
                                {
                                    let arg_ty = self.check_expr(arg_expr)?;
                                    let resolved_arg_ty = self.resolve_type_name(&arg_ty)?;
                                    let resolved_expected_ty =
                                        self.resolve_type_name(expected_ty)?;
                                    let numeric_coerced = Self::can_coerce_numeric(
                                        &resolved_arg_ty,
                                        &resolved_expected_ty,
                                    );
                                    if !numeric_coerced
                                        && !types_equal(&resolved_arg_ty, &resolved_expected_ty)
                                    {
                                        return Err(TypeCheckError::TypeMismatch {
                                            expected: type_to_string(&resolved_expected_ty),
                                            got: type_to_string(&resolved_arg_ty),
                                            span: arg_expr.span(),
                                        });
                                    }
                                }

                                // Return the enum type
                                return Ok(Type::Enum(module_name.to_string()));
                            } else {
                                return Err(TypeCheckError::UndefinedVariable {
                                    name: format!("module '{}'", module_name),
                                    span: call_expr.span,
                                });
                            }
                        }
                    };

                    // First check if the function exists (public or private)
                    if let Some(&is_public) = module_exports.all_functions.get(func_name) {
                        if !is_public {
                            return Err(TypeCheckError::Message(format!(
                                "Cannot access non-public function '{}::{}' from module '{}'. Add 'pub' before 'fn {}' in the module to make it accessible.",
                                module_name, func_name, module_name, func_name
                            )));
                        }
                    } else {
                        return Err(TypeCheckError::UndefinedVariable {
                            name: format!("{}::{}", module_name, func_name),
                            span: call_expr.span,
                        });
                    }

                    // Get the function declaration (we know it's public now)
                    let func_decl = module_exports.functions.get(func_name).ok_or_else(|| {
                        // This shouldn't happen if all_functions is correct, but handle it gracefully
                        TypeCheckError::UndefinedVariable {
                            name: format!("{}::{}", module_name, func_name),
                            span: call_expr.span,
                        }
                    })?;

                    (func_decl.params.clone(), func_decl.return_type.clone())
                } else if let Some(var_info) = self.variables.get(&call_expr.callee) {
                    let resolved_ty = self.resolve_type_name(&var_info.ty)?;
                    match resolved_ty {
                        Type::Fn {
                            params: fn_params,
                            return_type,
                        } => {
                            if call_expr.args.len() != fn_params.len() {
                                return Err(TypeCheckError::TypeMismatch {
                                    expected: format!("{} arguments", fn_params.len()),
                                    got: format!("{} arguments", call_expr.args.len()),
                                    span: call_expr.span,
                                });
                            }
                            for (arg_expr, param_ty) in call_expr.args.iter().zip(fn_params.iter())
                            {
                                let arg_ty = self.check_expr(arg_expr)?;
                                let resolved_arg_ty = self.resolve_type_name(&arg_ty)?;
                                let resolved_param_ty = self.resolve_type_name(param_ty)?;
                                let numeric_coerced =
                                    Self::can_coerce_numeric(&resolved_arg_ty, &resolved_param_ty);
                                if !numeric_coerced
                                    && !types_equal(&resolved_arg_ty, &resolved_param_ty)
                                {
                                    return Err(TypeCheckError::TypeMismatch {
                                        expected: type_to_string(&resolved_param_ty),
                                        got: type_to_string(&resolved_arg_ty),
                                        span: arg_expr.span(),
                                    });
                                }
                                self.check_expr_for_moves(arg_expr)?;
                            }
                            return Ok(*return_type);
                        }
                        _ => {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: "function type".to_string(),
                                got: type_to_string(&var_info.ty),
                                span: call_expr.span,
                            });
                        }
                    }
                } else {
                    // Look up function (regular or extern) and clone to avoid borrow issues
                    if let Some(func_decl) = self.functions.get(&call_expr.callee) {
                        (func_decl.params.clone(), func_decl.return_type.clone())
                    } else if let Some(extern_fn) = self.extern_functions.get(&call_expr.callee) {
                        (extern_fn.params.clone(), extern_fn.return_type.clone())
                    } else {
                        return Err(TypeCheckError::UndefinedVariable {
                            name: call_expr.callee.clone(),
                            span: call_expr.span,
                        });
                    }
                };

                let func_decl_params = params;

                let fn_generics: Vec<String> = if call_expr.callee.contains("::") {
                    let parts: Vec<&str> = call_expr.callee.split("::").collect();
                    if parts.len() == 2 {
                        self.module_imports
                            .get(parts[0])
                            .and_then(|m| m.functions.get(parts[1]))
                            .map(|f| f.generics.clone())
                            .unwrap_or_default()
                    } else {
                        Vec::new()
                    }
                } else {
                    self.functions
                        .get(&call_expr.callee)
                        .map(|f| f.generics.clone())
                        .unwrap_or_default()
                };

                let mut generic_substitutions = std::collections::HashMap::new();
                if !fn_generics.is_empty()
                    && !call_expr.args.is_empty()
                    && !func_decl_params.is_empty()
                {
                    let arg0_ty = self.check_expr(&call_expr.args[0])?;
                    let resolved_arg = self.resolve_type_name(&arg0_ty)?;
                    let param0_ty = self.resolve_type_name(&func_decl_params[0].ty)?;
                    generic_substitutions =
                        infer_generic_substitutions(&param0_ty, &resolved_arg, &fn_generics);
                }

                // Check argument count
                if call_expr.args.len() != func_decl_params.len() {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: format!("{} arguments", func_decl_params.len()),
                        got: format!("{} arguments", call_expr.args.len()),
                        span: call_expr.span,
                    });
                }

                // Check if this is an extern function call
                let is_extern = self.extern_functions.contains_key(&call_expr.callee);

                // Check argument types and mark as moved
                for (arg_expr, param) in call_expr.args.iter().zip(func_decl_params.iter()) {
                    let arg_ty = self.check_expr(arg_expr)?;
                    let resolved_arg_ty = self.resolve_type_name(&arg_ty)?;
                    let resolved_param_ty = substitute_generic_types_impl(
                        &self.resolve_type_name(&param.ty)?,
                        &generic_substitutions,
                    );

                    // Special case: allow string literals to be passed as *u8 for extern functions
                    // This enables calling C functions like printf with string literals
                    // In C, string literals are char* (which is *u8 in Ion)
                    let types_match = if is_extern
                        && matches!(arg_expr, Expr::StringLit(_))
                        && matches!(resolved_param_ty, Type::RawPtr { ref inner } if matches!(**inner, Type::U8))
                    {
                        // Allow String literal to be passed as *u8 (char* in C) for extern functions
                        true
                    } else if is_extern
                        && matches!(
                            resolved_param_ty,
                            Type::RawPtr { ref inner } if matches!(**inner, Type::U8)
                        )
                        && matches!(
                            resolved_arg_ty,
                            Type::Ref { ref inner, .. } if matches!(**inner, Type::U8)
                        )
                    {
                        // Allow &u8 (e.g. &buf[0]) for *u8 FFI parameters
                        true
                    } else {
                        // Check for array-to-slice coercion: &[T; N] can be coerced to &[]T
                        let coerced = if let Type::Ref {
                            inner: arg_inner,
                            mutable: arg_mut,
                        } = &resolved_arg_ty
                        {
                            if let Type::Ref {
                                inner: param_inner,
                                mutable: param_mut,
                            } = &resolved_param_ty
                            {
                                if matches!(**arg_inner, Type::Array { .. })
                                    && matches!(**param_inner, Type::Slice { .. })
                                {
                                    if let Type::Array {
                                        inner: ref arg_elem,
                                        ..
                                    } = **arg_inner
                                    {
                                        if let Type::Slice {
                                            inner: ref param_elem,
                                        } = **param_inner
                                        {
                                            arg_mut == param_mut
                                                && types_equal(arg_elem, param_elem)
                                        } else {
                                            false
                                        }
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        if coerced {
                            true
                        } else {
                            // Check for numeric type coercion
                            let numeric_coerced =
                                Self::can_coerce_numeric(&resolved_arg_ty, &resolved_param_ty);
                            // Normal type equality check
                            numeric_coerced || types_equal(&resolved_arg_ty, &resolved_param_ty)
                        }
                    };

                    if !types_match {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: type_to_string(&resolved_param_ty),
                            got: type_to_string(&resolved_arg_ty),
                            span: arg_expr.span(),
                        });
                    }
                }

                // Check if this is an extern function call - require unsafe context
                if is_extern && !self.is_in_unsafe_context() {
                    return Err(TypeCheckError::Message(format!(
                        "Extern function calls must be inside an unsafe block: {}",
                        call_expr.callee
                    )));
                }

                // Return the function's return type, or int if void
                if let Some(target) = self.lookup_fn_target(&call_expr.callee) {
                    self.record_reference(call_expr.callee_span, target);
                } else if let Some((module, func)) = call_expr.callee.split_once("::")
                    && let Some(enum_target) = self.lookup_variant_target(module, func)
                {
                    self.record_reference(call_expr.callee_span, enum_target);
                }

                self.record_signature(
                    call_expr.span,
                    Self::fn_hover_doc(
                        &call_expr.callee,
                        &fn_generics,
                        &func_decl_params,
                        &return_type_opt,
                    ),
                    None,
                );

                if let Some(ret) = return_type_opt {
                    Ok(substitute_generic_types_impl(
                        &self.resolve_type_name(&ret)?,
                        &generic_substitutions,
                    ))
                } else {
                    Ok(Type::Int)
                }
            }
            Expr::MethodCall(method_call) => {
                // Method call syntax: expr.method(args...)
                // This desugars to: Type::method(receiver, args...)

                // Step 1: Type-check the receiver to determine its type
                let receiver_type = self.check_expr(&method_call.receiver)?;

                // Step 2: Resolve type aliases
                let resolved_type = self.resolve_type_name(&receiver_type)?;

                // Step 3: Extract base type name for method lookup
                // Handle references, generics, etc.
                let (base_type_name, _is_reference, _is_mutable_ref) =
                    Self::extract_type_name_for_method(&resolved_type)?;

                // Step 4: Construct qualified function name: Type::method
                let qualified_name = format!("{}::{}", base_type_name, method_call.method);

                // Step 5: Determine required receiver type (check built-ins first)
                let required_receiver_type = match self.get_builtin_method_receiver_type(
                    &qualified_name,
                    &resolved_type,
                    &base_type_name,
                )? {
                    Some(ty) => ty,
                    None => {
                        // Not a built-in, look up regular function
                        let func_decl =
                            self.lookup_method_function(&qualified_name, &base_type_name)?;
                        if func_decl.params.is_empty() {
                            return Err(TypeCheckError::Message(format!(
                                "Method '{}' for type '{}' requires at least one parameter (the receiver)",
                                method_call.method, base_type_name
                            )));
                        }
                        let first_param = &func_decl.params[0];
                        self.resolve_type_name(&first_param.ty)?
                    }
                };

                // Step 6: Create receiver argument with appropriate borrowing
                let receiver_arg = self.create_receiver_argument(
                    method_call.receiver.clone(),
                    &resolved_type,
                    &required_receiver_type,
                    method_call.span,
                )?;

                // Step 9: Desugar to CallExpr
                let mut desugared_args = vec![receiver_arg];
                desugared_args.extend(method_call.args.clone());

                let desugared_call = CallExpr {
                    callee: qualified_name.clone(),
                    args: desugared_args,
                    span: method_call.span,
                    callee_span: method_call.method_span,
                };

                // Step 10: Type-check the desugared call and return its type
                // Check if this is a built-in function first
                if let Some(return_type) = self.check_builtin_call(&desugared_call)? {
                    if let Some(sig) = Self::builtin_signature(&qualified_name) {
                        self.record_hover_doc(method_call.method_span, sig.to_string());
                        self.record_signature(method_call.span, sig.to_string(), None);
                    }
                    return Ok(return_type);
                }

                // Regular function call type checking
                let (params, return_type_opt) = if desugared_call.callee.contains("::") {
                    let parts: Vec<&str> = desugared_call.callee.split("::").collect();
                    if parts.len() != 2 {
                        return Err(TypeCheckError::Message(format!(
                            "Invalid qualified function name: {}",
                            desugared_call.callee
                        )));
                    }
                    let module_name = parts[0];
                    let func_name = parts[1];

                    // Look up module in imports or check if it's a type with methods
                    if let Some(module_exports) = self.module_imports.get(module_name) {
                        // Module import case
                        if let Some(&is_public) = module_exports.all_functions.get(func_name) {
                            if !is_public {
                                return Err(TypeCheckError::Message(format!(
                                    "Cannot access non-public function '{}::{}' from module '{}'. Add 'pub' before 'fn {}' in the module to make it accessible.",
                                    module_name, func_name, module_name, func_name
                                )));
                            }
                        } else {
                            return Err(TypeCheckError::UndefinedVariable {
                                name: format!("{}::{}", module_name, func_name),
                                span: desugared_call.span,
                            });
                        }

                        let func_decl =
                            module_exports.functions.get(func_name).ok_or_else(|| {
                                TypeCheckError::UndefinedVariable {
                                    name: format!("{}::{}", module_name, func_name),
                                    span: desugared_call.span,
                                }
                            })?;

                        (func_decl.params.clone(), func_decl.return_type.clone())
                    } else {
                        // Type method case - look up the function again
                        let func_decl =
                            self.lookup_method_function(&desugared_call.callee, &base_type_name)?;
                        (func_decl.params.clone(), func_decl.return_type.clone())
                    }
                } else {
                    // Simple function call - shouldn't happen for methods
                    return Err(TypeCheckError::Message(format!(
                        "Method call '{}' must resolve to qualified function name",
                        method_call.method
                    )));
                };

                // Check argument count
                if desugared_call.args.len() != params.len() {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: format!("{} arguments", params.len()),
                        got: format!("{} arguments", desugared_call.args.len()),
                        span: desugared_call.span,
                    });
                }

                // Check argument types
                for (arg_expr, param) in desugared_call.args.iter().zip(params.iter()) {
                    let arg_ty = self.check_expr(arg_expr)?;
                    let resolved_arg_ty = self.resolve_type_name(&arg_ty)?;
                    let resolved_param_ty = self.resolve_type_name(&param.ty)?;

                    // Check for numeric coercion and type equality
                    let numeric_coerced =
                        Self::can_coerce_numeric(&resolved_arg_ty, &resolved_param_ty);
                    let types_match =
                        numeric_coerced || types_equal(&resolved_arg_ty, &resolved_param_ty);

                    if !types_match {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: type_to_string(&resolved_param_ty),
                            got: type_to_string(&resolved_arg_ty),
                            span: arg_expr.span(),
                        });
                    }
                }

                if let Some(target) = self.lookup_fn_target(&desugared_call.callee) {
                    self.record_reference(method_call.method_span, target);
                }

                self.record_signature(
                    method_call.span,
                    Self::fn_hover_doc(&desugared_call.callee, &[], &params, &return_type_opt),
                    None,
                );

                Ok(return_type_opt.unwrap_or(Type::Int))
            }
            Expr::TupleLit(tuple_lit) => {
                if tuple_lit.elements.is_empty() {
                    return Err(TypeCheckError::Message(
                        "empty tuple literal is not supported".to_string(),
                    ));
                }
                let mut elem_types = Vec::with_capacity(tuple_lit.elements.len());
                for elem in &tuple_lit.elements {
                    elem_types.push(self.check_expr(elem)?);
                }
                Ok(Type::Tuple {
                    elements: elem_types,
                })
            }
            Expr::ArrayLiteral(arr_lit) => {
                // Handle [value; count] syntax
                if let Some((ref value_expr, count)) = arr_lit.repeat {
                    // Type-check the repeated expression once
                    let elem_ty = self.check_expr(value_expr)?;

                    // For integer literals, default to int, but allow coercion later
                    // The actual element type will be determined by the type annotation if present
                    let inferred_elem_ty = if matches!(elem_ty, Type::Int) {
                        // Integer literals default to int, but can be coerced to smaller types
                        Type::Int
                    } else {
                        elem_ty
                    };

                    // Return array type with inferred element type and count
                    Ok(Type::Array {
                        inner: Box::new(inferred_elem_ty),
                        size: count,
                    })
                } else if arr_lit.elements.is_empty() {
                    // Empty array - can't infer type, require type annotation
                    Err(TypeCheckError::Message(
                        "Empty array literal requires explicit type annotation".to_string(),
                    ))
                } else {
                    // Regular array literal: [expr, expr, ...]
                    // Check all elements have the same type
                    let first_elem_ty = self.check_expr(&arr_lit.elements[0])?;
                    for elem in arr_lit.elements.iter().skip(1) {
                        let elem_ty = self.check_expr(elem)?;
                        if !types_equal(&elem_ty, &first_elem_ty) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: type_to_string(&first_elem_ty),
                                got: type_to_string(&elem_ty),
                                span: elem.span(),
                            });
                        }
                    }

                    // Return array type with inferred element type and size
                    Ok(Type::Array {
                        inner: Box::new(first_elem_ty),
                        size: arr_lit.elements.len(),
                    })
                }
            }
            Expr::Index(index_expr) => {
                // Check target expression
                let target_ty = self.check_expr_with_context(&index_expr.target, borrow_operand)?;

                // Check index expression is integer
                let index_ty = self.check_expr(&index_expr.index)?;
                if !types_equal(&index_ty, &Type::Int) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "int".to_string(),
                        got: type_to_string(&index_ty),
                        span: index_expr.index.span(),
                    });
                }

                // Determine result type based on target
                let target_ty_str = type_to_string(&target_ty);
                match target_ty {
                    Type::Array { inner, .. } => Ok(*inner),
                    Type::Slice { inner } => Ok(*inner),
                    Type::String => Ok(Type::U8),
                    Type::Ref { inner, .. } => match inner.as_ref() {
                        Type::Array { inner: elem_ty, .. } => Ok(*elem_ty.clone()),
                        Type::Slice { inner: elem_ty } => Ok(*elem_ty.clone()),
                        Type::String => Ok(Type::U8),
                        _ => Err(TypeCheckError::TypeMismatch {
                            expected: "array, slice, or String type for indexing".to_string(),
                            got: target_ty_str.clone(),
                            span: index_expr.span,
                        }),
                    },
                    _ => Err(TypeCheckError::TypeMismatch {
                        expected: "array, slice, or String type for indexing".to_string(),
                        got: target_ty_str,
                        span: index_expr.span,
                    }),
                }
            }
            Expr::Cast(cast_expr) => {
                // Check the expression being cast
                let expr_type = self.check_expr(&cast_expr.expr)?;
                let resolved_expr_type = self.resolve_type_name(&expr_type)?;
                let resolved_target_type = self.resolve_type_name(&cast_expr.target_type)?;

                // Only allow casts between numeric types
                if !self.is_numeric_type(&resolved_expr_type) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "numeric type for cast".to_string(),
                        got: type_to_string(&resolved_expr_type),
                        span: cast_expr.expr.span(),
                    });
                }

                if !self.is_numeric_type(&resolved_target_type) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "numeric type as cast target".to_string(),
                        got: type_to_string(&resolved_target_type),
                        span: cast_expr.span,
                    });
                }

                // Return the target type
                Ok(resolved_target_type)
            }
            Expr::Assign(assign_expr) => {
                // Check the value expression
                let value_ty = self.check_expr(&assign_expr.value)?;
                let resolved_value_ty = self.resolve_type_name(&value_ty)?;

                // Handle different assignment targets
                match &*assign_expr.target {
                    Expr::Var(var_expr) => {
                        // Variable assignment: x = value
                        // Check that variable exists and is mutable
                        let var_info = self.variables.get(&var_expr.name).ok_or_else(|| {
                            TypeCheckError::UndefinedVariable {
                                name: var_expr.name.clone(),
                                span: var_expr.span,
                            }
                        })?;

                        self.check_owner_not_borrowed(&var_expr.name, var_expr.span)?;

                        // Check type compatibility with coercion
                        let resolved_var_ty = self.resolve_type_name(&var_info.ty)?;
                        let numeric_coerced =
                            Self::can_coerce_numeric(&resolved_value_ty, &resolved_var_ty);

                        if !types_equal(&resolved_value_ty, &resolved_var_ty) && !numeric_coerced {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: type_to_string(&resolved_var_ty),
                                got: type_to_string(&resolved_value_ty),
                                span: assign_expr.value.span(),
                            });
                        }

                        // Assignment returns unit type (void) in Ion
                        Ok(Type::Int) // Using Int as placeholder for unit/void
                    }
                    Expr::Index(index_expr) => {
                        // Array element assignment: arr[i] = value
                        // Check the base of the index expression to get the array type
                        let base_ty = self.check_expr(&index_expr.target)?;
                        let target_ty_str = type_to_string(&base_ty);
                        match base_ty {
                            Type::Array { inner, .. } => {
                                // Check value type matches element type (with coercion)
                                let resolved_elem_ty = self.resolve_type_name(inner.as_ref())?;
                                let numeric_coerced =
                                    Self::can_coerce_numeric(&resolved_value_ty, &resolved_elem_ty);

                                if !types_equal(&resolved_value_ty, &resolved_elem_ty)
                                    && !numeric_coerced
                                {
                                    return Err(TypeCheckError::TypeMismatch {
                                        expected: type_to_string(&resolved_elem_ty),
                                        got: type_to_string(&resolved_value_ty),
                                        span: assign_expr.value.span(),
                                    });
                                }

                                // Assignment returns unit type
                                Ok(Type::Int) // Using Int as placeholder for unit/void
                            }
                            Type::Ref { inner, .. } => {
                                // Indexing a reference to an array
                                match inner.as_ref() {
                                    Type::Array { inner: elem_ty, .. } => {
                                        let resolved_elem_ty =
                                            self.resolve_type_name(elem_ty.as_ref())?;
                                        let numeric_coerced = Self::can_coerce_numeric(
                                            &resolved_value_ty,
                                            &resolved_elem_ty,
                                        );

                                        if !types_equal(&resolved_value_ty, &resolved_elem_ty)
                                            && !numeric_coerced
                                        {
                                            return Err(TypeCheckError::TypeMismatch {
                                                expected: type_to_string(&resolved_elem_ty),
                                                got: type_to_string(&resolved_value_ty),
                                                span: assign_expr.value.span(),
                                            });
                                        }

                                        Ok(Type::Int)
                                    }
                                    _ => Err(TypeCheckError::TypeMismatch {
                                        expected: "array type for indexed assignment".to_string(),
                                        got: target_ty_str,
                                        span: assign_expr.span,
                                    }),
                                }
                            }
                            _ => Err(TypeCheckError::TypeMismatch {
                                expected: "array type for indexed assignment".to_string(),
                                got: target_ty_str,
                                span: assign_expr.span,
                            }),
                        }
                    }
                    _ => Err(TypeCheckError::TypeMismatch {
                        expected: "variable or indexed expression".to_string(),
                        got: "invalid assignment target".to_string(),
                        span: assign_expr.span,
                    }),
                }
            }
        }
    }

    /// Check if a type is numeric (all integer and float types)
    fn is_numeric_type(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Int
                | Type::F32
                | Type::F64
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::UInt
        )
    }

    /// Check if a type is an integer type (signed or unsigned, excluding floats)
    fn is_integer_type(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Int
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::UInt
        )
    }

    /// Check if a type is an unsigned integer type
    fn is_unsigned_integer_type(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::UInt
        )
    }

    /// Promote two integer types to a common type (for bitwise operations)
    fn promote_integer_types(&self, left: &Type, right: &Type) -> Result<Type, TypeCheckError> {
        // Use the same promotion rules as arithmetic operations
        self.check_arithmetic_op(
            left,
            right,
            &Span {
                start: 0,
                end: 0,
                line: 0,
                column: 0,
            },
        )
    }

    /// Get the width of an integer type (in bits)
    fn integer_width(ty: &Type) -> Option<u32> {
        match ty {
            Type::I8 | Type::U8 => Some(8),
            Type::I16 | Type::U16 => Some(16),
            Type::I32 | Type::U32 | Type::Int | Type::UInt => Some(32),
            Type::I64 | Type::U64 => Some(64),
            _ => None,
        }
    }

    /// Check arithmetic operation and return promoted type
    fn check_arithmetic_op(
        &self,
        left: &Type,
        right: &Type,
        span: &Span,
    ) -> Result<Type, TypeCheckError> {
        // Float promotion rules (checked first):
        // - f32 + f32 → f32
        // - f64 + f64 → f64
        // - f32 + f64 → f64 (promote to wider)
        // - int/any_int + f32 → f32 (promote int to float)
        // - int/any_int + f64 → f64

        match (left, right) {
            (Type::F32, Type::F32) => Ok(Type::F32),
            (Type::F64, Type::F64) => Ok(Type::F64),
            (Type::F32, Type::F64) | (Type::F64, Type::F32) => Ok(Type::F64),
            (l, r) if matches!(l, Type::F32 | Type::F64) && self.is_numeric_type(r) => {
                Ok(l.clone())
            }
            (l, r) if matches!(r, Type::F32 | Type::F64) && self.is_numeric_type(l) => {
                Ok(r.clone())
            }
            // Integer promotion rules:
            // - Same type → same type
            // - Mixed-width: promote to wider type
            // - Signed/unsigned mixing: promote to signed wider type
            (l, r) if Self::integer_width(l).is_some() && Self::integer_width(r).is_some() => {
                let l_width = Self::integer_width(l).unwrap();
                let r_width = Self::integer_width(r).unwrap();
                let l_signed =
                    matches!(l, Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::Int);
                let r_signed =
                    matches!(r, Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::Int);

                // If mixing signed/unsigned, promote to signed wider type
                if l_signed != r_signed {
                    let max_width = l_width.max(r_width);
                    // Promote to signed type of max width
                    Ok(match max_width {
                        8 => Type::I16,  // Can't have signed 8-bit result
                        16 => Type::I32, // Can't have signed 16-bit result
                        32 => Type::I64, // Promote to 64-bit signed
                        64 => Type::I64,
                        _ => Type::Int,
                    })
                } else if l_width == r_width {
                    // Same width, same signedness → same type
                    Ok(l.clone())
                } else {
                    // Different width, same signedness → wider type
                    let wider = if l_width > r_width { l } else { r };
                    Ok(wider.clone())
                }
            }
            _ => Err(TypeCheckError::TypeMismatch {
                expected: "numeric types for arithmetic".to_string(),
                got: format!("{} and {}", type_to_string(left), type_to_string(right)),
                span: *span,
            }),
        }
    }

    /// Infer the value type produced by a match arm block (trailing expression or return value).
    fn infer_block_result_type(
        &mut self,
        block: &Block,
        _span: Span,
    ) -> Result<Type, TypeCheckError> {
        for stmt in block.statements.iter().rev() {
            match stmt {
                Stmt::Return(ret) => {
                    if let Some(ref value) = ret.value {
                        return self.check_expr(value);
                    }
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "return value in match arm".to_string(),
                        got: "no return value".to_string(),
                        span: ret.span,
                    });
                }
                Stmt::Expr(expr_stmt) => {
                    return self.check_expr(&expr_stmt.expr);
                }
                _ => {}
            }
        }
        // Arms that only run statements (e.g. empty None branch, if guards) produce unit.
        Ok(Type::Int)
    }

    /// Unify types from match arms; numeric coercion follows assignment rules.
    fn unify_match_arm_types(
        &mut self,
        prev: &Type,
        next: &Type,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        let prev = self.resolve_type_name(prev)?;
        let next = self.resolve_type_name(next)?;
        if types_equal(&prev, &next) {
            return Ok(prev);
        }
        if Self::can_coerce_numeric(&next, &prev) {
            return Ok(prev);
        }
        if Self::can_coerce_numeric(&prev, &next) {
            return Ok(next);
        }
        Err(TypeCheckError::TypeMismatch {
            expected: type_to_string(&prev),
            got: type_to_string(&next),
            span,
        })
    }

    /// Check if a numeric type can be coerced to another numeric type
    /// Rules:
    /// - f64 can be coerced to f32 (narrowing, but allowed for literals)
    /// - Any integer type can be coerced to f32 or f64
    /// - Narrower integer types can be coerced to wider integer types
    /// - Same-width signed/unsigned can be coerced (conservative: allow)
    fn can_coerce_numeric(from: &Type, to: &Type) -> bool {
        match (from, to) {
            // Float coercions
            (Type::F64, Type::F32) => true, // f64 → f32 (narrowing)
            (Type::F32, Type::F64) => true, // f32 → f64 (widening)
            (Type::F32, Type::F32) | (Type::F64, Type::F64) => true,

            // Integer to float
            (Type::Int, Type::F32) | (Type::Int, Type::F64) => true,
            (Type::I8, Type::F32) | (Type::I8, Type::F64) => true,
            (Type::I16, Type::F32) | (Type::I16, Type::F64) => true,
            (Type::I32, Type::F32) | (Type::I32, Type::F64) => true,
            (Type::I64, Type::F32) | (Type::I64, Type::F64) => true,
            (Type::U8, Type::F32) | (Type::U8, Type::F64) => true,
            (Type::U16, Type::F32) | (Type::U16, Type::F64) => true,
            (Type::U32, Type::F32) | (Type::U32, Type::F64) => true,
            (Type::U64, Type::F32) | (Type::U64, Type::F64) => true,
            (Type::UInt, Type::F32) | (Type::UInt, Type::F64) => true,

            // Integer to integer (narrower → wider)
            (Type::I8, Type::I16)
            | (Type::I8, Type::I32)
            | (Type::I8, Type::I64)
            | (Type::I8, Type::Int) => true,
            (Type::I16, Type::I32) | (Type::I16, Type::I64) | (Type::I16, Type::Int) => true,
            (Type::I32, Type::I64) | (Type::I32, Type::Int) => true,
            (Type::I64, Type::Int) => true,

            (Type::U8, Type::U16)
            | (Type::U8, Type::U32)
            | (Type::U8, Type::U64)
            | (Type::U8, Type::UInt) => true,
            (Type::U16, Type::U32) | (Type::U16, Type::U64) | (Type::U16, Type::UInt) => true,
            (Type::U32, Type::U64) | (Type::U32, Type::UInt) => true,
            (Type::U64, Type::UInt) => true,

            // int (default integer literal) can be coerced to any integer type
            (Type::Int, Type::I8)
            | (Type::Int, Type::I16)
            | (Type::Int, Type::I32)
            | (Type::Int, Type::I64) => true,
            (Type::Int, Type::U8)
            | (Type::Int, Type::U16)
            | (Type::Int, Type::U32)
            | (Type::Int, Type::U64)
            | (Type::Int, Type::UInt) => true,

            // I64 (signed 64-bit) can be coerced to unsigned types (for literals that fit)
            // This is needed when integer promotion results in I64 instead of Int
            (Type::I64, Type::U8)
            | (Type::I64, Type::U16)
            | (Type::I64, Type::U32)
            | (Type::I64, Type::U64)
            | (Type::I64, Type::UInt) => true,
            // I32 can also be coerced to unsigned types
            (Type::I32, Type::U8)
            | (Type::I32, Type::U16)
            | (Type::I32, Type::U32)
            | (Type::I32, Type::U64)
            | (Type::I32, Type::UInt) => true,
            // I16 can be coerced to unsigned types
            (Type::I16, Type::U8)
            | (Type::I16, Type::U16)
            | (Type::I16, Type::U32)
            | (Type::I16, Type::U64)
            | (Type::I16, Type::UInt) => true,
            // I8 can be coerced to unsigned types
            (Type::I8, Type::U8)
            | (Type::I8, Type::U16)
            | (Type::I8, Type::U32)
            | (Type::I8, Type::U64)
            | (Type::I8, Type::UInt) => true,

            // Same type
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => true,
            (Type::I8, Type::I8)
            | (Type::I16, Type::I16)
            | (Type::I32, Type::I32)
            | (Type::I64, Type::I64) => true,
            (Type::U8, Type::U8)
            | (Type::U16, Type::U16)
            | (Type::U32, Type::U32)
            | (Type::U64, Type::U64)
            | (Type::UInt, Type::UInt) => true,

            _ => false,
        }
    }
}

impl TypeChecker {
    /// Check if a type contains any references (directly or nested).
    /// Used to enforce the no-escape rule.
    fn is_reference_containing(&self, ty: &Type) -> bool {
        match ty {
            Type::Int
            | Type::Bool
            | Type::F32
            | Type::F64
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::UInt => false,
            Type::Ref { .. } => true,
            Type::RawPtr { inner } => self.is_reference_containing(inner),
            Type::Channel { elem_type } => self.is_reference_containing(elem_type),
            Type::Struct(name) => {
                if let Some(decl) = self.structs.get(name) {
                    decl.fields
                        .iter()
                        .any(|f| self.is_reference_containing(&f.ty))
                } else {
                    false
                }
            }
            Type::Enum(name) => {
                if let Some(decl) = self.enums.get(name) {
                    decl.variants.iter().any(|v| {
                        v.payload_types
                            .iter()
                            .any(|ty| self.is_reference_containing(ty))
                            || if let Some(ref named_fields) = v.named_fields {
                                named_fields
                                    .iter()
                                    .any(|(_, ty)| self.is_reference_containing(ty))
                            } else {
                                false
                            }
                    })
                } else {
                    false
                }
            }
            Type::Generic { name: _, params } => {
                params.iter().any(|p| self.is_reference_containing(p))
            }
            Type::Box { inner } => self.is_reference_containing(inner),
            Type::Vec { elem_type } => self.is_reference_containing(elem_type),
            Type::String => false,
            Type::Array { inner, .. } => self.is_reference_containing(inner),
            // Slices are like references - they contain references and must obey no-escape
            Type::Slice { .. } => true, // Slices are reference-like
            Type::Sender { elem_type } => self.is_reference_containing(elem_type),
            Type::Receiver { elem_type } => self.is_reference_containing(elem_type),
            Type::Tuple { elements } => elements.iter().any(|e| self.is_reference_containing(e)),
            Type::Fn {
                params,
                return_type,
            } => {
                params.iter().any(|p| self.is_reference_containing(p))
                    || self.is_reference_containing(return_type)
            }
        }
    }

    /// Check if a type is "Send" (safe to transfer across thread boundaries or send over channels).
    /// Structural Send predicate: a type is Send if and only if all its components are Send.
    /// Used for:
    /// - Values captured into `spawn`
    /// - Types used as channel elements (`channel<T>`)
    fn is_send(&self, ty: &Type) -> bool {
        match ty {
            // Primitives are Send
            Type::Int => true,
            Type::Bool => true,
            Type::F32 => true,
            Type::F64 => true,
            Type::I8 => true,
            Type::I16 => true,
            Type::I32 => true,
            Type::I64 => true,
            Type::U8 => true,
            Type::U16 => true,
            Type::U32 => true,
            Type::U64 => true,
            Type::UInt => true,
            // References are NOT Send (they cannot escape their lexical scope)
            Type::Ref { .. } => false,
            // Raw pointers are NOT Send (they escape to C code)
            Type::RawPtr { inner } => self.is_send(inner),
            // Channels are Send iff their element types are Send
            Type::Channel { elem_type } => self.is_send(elem_type),
            // Structs are Send iff all fields are Send
            Type::Struct(name) => {
                if let Some(decl) = self.structs.get(name) {
                    decl.fields.iter().all(|f| self.is_send(&f.ty))
                } else {
                    true
                }
            }
            Type::Enum(name) => {
                if let Some(decl) = self.enums.get(name) {
                    decl.variants.iter().all(|v| {
                        v.payload_types.iter().all(|ty| self.is_send(ty))
                            && v.named_fields.as_ref().is_none_or(|named_fields| {
                                named_fields.iter().all(|(_, ty)| self.is_send(ty))
                            })
                    })
                } else {
                    true
                }
            }
            Type::Generic { name: _, params } => params.iter().all(|p| self.is_send(p)),
            Type::Box { inner } => self.is_send(inner),
            Type::Vec { elem_type } => self.is_send(elem_type),
            Type::String => true,
            Type::Array { inner, .. } => self.is_send(inner),
            // Slices are NOT Send (like references, they cannot escape their lexical scope)
            Type::Slice { .. } => false,
            // Sender and Receiver are Send iff their element types are Send
            Type::Sender { elem_type } => self.is_send(elem_type),
            Type::Receiver { elem_type } => self.is_send(elem_type),
            // Tuples are Send iff all elements are Send
            Type::Tuple { elements } => elements.iter().all(|e| self.is_send(e)),
            // Function pointers are Send (no captured stack state for named functions)
            Type::Fn { .. } => true,
        }
    }

    /// Add pattern bindings to the variable scope
    fn add_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        enum_decl: &EnumDecl,
        expr_ty: &Type,
        _expr: &Expr,
    ) -> Result<(), TypeCheckError> {
        match pattern {
            Pattern::Variant {
                enum_name: _,
                variant,
                sub_patterns,
                named_fields,
                span,
            } => {
                // Find the variant and add bindings for its payloads
                if let Some(variant_decl) = enum_decl.variants.iter().find(|v| v.name == *variant) {
                    // Build substitution map for generic parameters
                    let substitutions = if let Type::Generic { name: _, params } = expr_ty {
                        // Map enum generic parameter names to concrete types
                        enum_decl
                            .generics
                            .iter()
                            .zip(params.iter())
                            .map(|(gen_name, concrete_ty)| (gen_name.clone(), concrete_ty.clone()))
                            .collect::<std::collections::HashMap<_, _>>()
                    } else {
                        std::collections::HashMap::new()
                    };

                    // Handle struct variants with named fields
                    if let Some(named_fields_patterns) = named_fields {
                        if let Some(variant_named_fields) = &variant_decl.named_fields {
                            for (field_name, field_pattern) in named_fields_patterns {
                                // Find the field type in the variant declaration
                                if let Some((_, field_ty)) = variant_named_fields
                                    .iter()
                                    .find(|(name, _)| name == field_name)
                                {
                                    // Substitute generic parameters in field type
                                    let concrete_field_ty =
                                        substitute_generic_types_impl(field_ty, &substitutions);

                                    match field_pattern {
                                        Pattern::Binding { name, .. } => {
                                            // Add binding to scope with the concrete field type
                                            self.variables.insert(
                                                name.clone(),
                                                Self::new_variable_info(concrete_field_ty, *span),
                                            );
                                        }
                                        Pattern::Variant { .. } => {
                                            // Nested variant - recurse (simplified for now)
                                            // For nested patterns, we'd need to handle them recursively
                                        }
                                        Pattern::Wildcard { .. } => {
                                            // Wildcard - no binding to add
                                        }
                                    }
                                } else {
                                    return Err(TypeCheckError::Message(format!(
                                        "Unknown field '{}' in variant '{}' of enum '{}'",
                                        field_name, variant, enum_decl.name
                                    )));
                                }
                            }
                        } else {
                            return Err(TypeCheckError::Message(format!(
                                "Variant '{}' of enum '{}' does not have named fields",
                                variant, enum_decl.name
                            )));
                        }
                    } else {
                        // Handle tuple variants with positional patterns
                        for (i, payload_ty) in variant_decl.payload_types.iter().enumerate() {
                            if let Some(sub_pattern) = sub_patterns.get(i) {
                                // Substitute generic parameters in payload type
                                // Always use substitute_generic_types_impl as it handles both
                                // direct generic parameters and nested generic types correctly
                                let concrete_payload_ty =
                                    substitute_generic_types_impl(payload_ty, &substitutions);

                                match sub_pattern {
                                    Pattern::Binding { name, .. } => {
                                        // Add binding to scope with the concrete payload type
                                        self.variables.insert(
                                            name.clone(),
                                            Self::new_variable_info(concrete_payload_ty, *span),
                                        );
                                    }
                                    Pattern::Variant { .. } => {
                                        // Nested variant - recurse (simplified for now)
                                        // For nested patterns, we'd need to handle them recursively
                                    }
                                    Pattern::Wildcard { .. } => {
                                        // Wildcard - no binding to add
                                    }
                                }
                            }
                        }
                    }
                }
                Ok(())
            }
            Pattern::Binding { name, span } => {
                // Binding pattern binds the entire matched value
                self.variables.insert(
                    name.clone(),
                    Self::new_variable_info(expr_ty.clone(), *span),
                );
                Ok(())
            }
            Pattern::Wildcard { .. } => {
                // Wildcard - no bindings to add
                Ok(())
            }
        }
    }
}

/// True when control flow can leave this block and continue at the enclosing merge point.
fn block_falls_through(block: &Block) -> bool {
    if block.statements.is_empty() {
        return true;
    }
    match block.statements.last().unwrap() {
        Stmt::Return(_) | Stmt::Break(_) | Stmt::Continue(_) => false,
        Stmt::If(if_stmt) => {
            let then_ft = block_falls_through(&if_stmt.then_block);
            match &if_stmt.else_block {
                Some(else_blk) => then_ft || block_falls_through(else_blk),
                None => true,
            }
        }
        _ => true,
    }
}

fn infer_generic_substitutions(
    expected: &Type,
    actual: &Type,
    fn_generics: &[String],
) -> std::collections::HashMap<String, Type> {
    let mut subs = std::collections::HashMap::new();
    match (expected, actual) {
        (
            Type::Generic {
                name: e_name,
                params: e_params,
            },
            Type::Generic {
                name: a_name,
                params: a_params,
            },
        ) if e_name == a_name && e_params.len() == a_params.len() => {
            for (e, a) in e_params.iter().zip(a_params.iter()) {
                subs.extend(infer_generic_substitutions(e, a, fn_generics));
            }
        }
        (Type::Struct(param_name), actual_ty) if fn_generics.contains(param_name) => {
            subs.insert(param_name.clone(), actual_ty.clone());
        }
        _ => {}
    }
    subs
}

/// Substitute generic type parameters with concrete types
fn substitute_generic_types_impl(
    ty: &Type,
    substitutions: &std::collections::HashMap<String, Type>,
) -> Type {
    match ty {
        Type::Generic { name, params } => {
            // If this is a generic parameter, substitute it
            if params.is_empty() {
                substitutions
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| ty.clone())
            } else {
                // Recursively substitute in nested generic types
                Type::Generic {
                    name: name.clone(),
                    params: params
                        .iter()
                        .map(|p| substitute_generic_types_impl(p, substitutions))
                        .collect(),
                }
            }
        }
        // The parser stores generic type parameters as Struct(name) or Enum(name)
        // when they appear in enum variant payloads. Check if this is actually
        // a generic parameter that needs substitution.
        Type::Struct(name) | Type::Enum(name) => {
            // If this name is in the substitutions map, it's a generic parameter
            substitutions
                .get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone())
        }
        Type::Box { inner } => Type::Box {
            inner: Box::new(substitute_generic_types_impl(inner, substitutions)),
        },
        Type::Vec { elem_type } => Type::Vec {
            elem_type: Box::new(substitute_generic_types_impl(elem_type, substitutions)),
        },
        Type::Ref { inner, mutable } => Type::Ref {
            inner: Box::new(substitute_generic_types_impl(inner, substitutions)),
            mutable: *mutable,
        },
        Type::Channel { elem_type } => Type::Channel {
            elem_type: Box::new(substitute_generic_types_impl(elem_type, substitutions)),
        },
        _ => ty.clone(),
    }
}

/// Collect names of variables from the enclosing scope referenced inside a block.
/// Locals declared inside the block (including nested scopes) are excluded.
pub(crate) fn collect_captured_vars(block: &Block) -> Vec<String> {
    use std::collections::HashSet;

    let mut refs = Vec::new();
    let mut locals = HashSet::new();
    collect_block_var_refs(block, &mut refs, &mut locals);
    refs.sort();
    refs.dedup();
    refs
}

fn add_pattern_binding_names(pattern: &Pattern, locals: &mut std::collections::HashSet<String>) {
    match pattern {
        Pattern::Binding { name, .. } => {
            locals.insert(name.clone());
        }
        Pattern::Variant {
            sub_patterns,
            named_fields,
            ..
        } => {
            for sub in sub_patterns {
                add_pattern_binding_names(sub, locals);
            }
            if let Some(fields) = named_fields {
                for (_, sub) in fields {
                    add_pattern_binding_names(sub, locals);
                }
            }
        }
        Pattern::Wildcard { .. } => {}
    }
}

fn collect_block_var_refs(
    block: &Block,
    refs: &mut Vec<String>,
    locals: &mut std::collections::HashSet<String>,
) {
    for stmt in &block.statements {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Some(ref init) = let_stmt.init {
                    collect_expr_var_refs(init, refs, locals);
                }
                locals.insert(let_stmt.name.clone());
                if let Some(ref patterns) = let_stmt.patterns {
                    for pattern in patterns {
                        add_pattern_binding_names(pattern, locals);
                    }
                }
            }
            Stmt::Return(return_stmt) => {
                if let Some(ref value) = return_stmt.value {
                    collect_expr_var_refs(value, refs, locals);
                }
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
            Stmt::Expr(expr_stmt) => collect_expr_var_refs(&expr_stmt.expr, refs, locals),
            Stmt::Defer(defer_stmt) => collect_expr_var_refs(&defer_stmt.expr, refs, locals),
            Stmt::Spawn(spawn_stmt) => {
                collect_block_var_refs(&spawn_stmt.body, refs, locals);
            }
            Stmt::If(if_stmt) => {
                collect_expr_var_refs(&if_stmt.cond, refs, locals);
                let mut then_locals = locals.clone();
                collect_block_var_refs(&if_stmt.then_block, refs, &mut then_locals);
                if let Some(ref else_blk) = if_stmt.else_block {
                    let mut else_locals = locals.clone();
                    collect_block_var_refs(else_blk, refs, &mut else_locals);
                }
            }
            Stmt::While(while_stmt) => {
                collect_expr_var_refs(&while_stmt.cond, refs, locals);
                let mut body_locals = locals.clone();
                collect_block_var_refs(&while_stmt.body, refs, &mut body_locals);
            }
            Stmt::Loop(loop_stmt) => {
                let mut body_locals = locals.clone();
                collect_block_var_refs(&loop_stmt.body, refs, &mut body_locals);
            }
            Stmt::UnsafeBlock(unsafe_stmt) => {
                collect_block_var_refs(&unsafe_stmt.body, refs, locals);
            }
            Stmt::For(for_stmt) => {
                collect_expr_var_refs(&for_stmt.iterable, refs, locals);
                let mut body_locals = locals.clone();
                body_locals.insert(for_stmt.var_name.clone());
                collect_block_var_refs(&for_stmt.body, refs, &mut body_locals);
            }
        }
    }
}

fn collect_expr_var_refs(
    expr: &Expr,
    refs: &mut Vec<String>,
    locals: &std::collections::HashSet<String>,
) {
    match expr {
        Expr::Lit(_) | Expr::BoolLiteral(_) | Expr::FloatLiteral(_) => {}
        Expr::Var(var_expr) => {
            if !locals.contains(&var_expr.name) {
                refs.push(var_expr.name.clone());
            }
        }
        Expr::Ref(ref_expr) => collect_expr_var_refs(&ref_expr.inner, refs, locals),
        Expr::StructLit(lit) => {
            for field in &lit.fields {
                collect_expr_var_refs(&field.value, refs, locals);
            }
        }
        Expr::TupleLit(tuple_lit) => {
            for elem in &tuple_lit.elements {
                collect_expr_var_refs(elem, refs, locals);
            }
        }
        Expr::FieldAccess(acc) => collect_expr_var_refs(&acc.base, refs, locals),
        Expr::BinOp(bin_op_expr) => {
            collect_expr_var_refs(&bin_op_expr.left, refs, locals);
            collect_expr_var_refs(&bin_op_expr.right, refs, locals);
        }
        Expr::UnOp(un_op_expr) => collect_expr_var_refs(&un_op_expr.operand, refs, locals),
        Expr::Send(send_expr) => {
            collect_expr_var_refs(&send_expr.channel, refs, locals);
            collect_expr_var_refs(&send_expr.value, refs, locals);
        }
        Expr::Recv(recv_expr) => collect_expr_var_refs(&recv_expr.channel, refs, locals),
        Expr::EnumLit(enum_lit) => {
            for arg in &enum_lit.args {
                collect_expr_var_refs(arg, refs, locals);
            }
        }
        Expr::Match(match_expr) => {
            collect_expr_var_refs(&match_expr.expr, refs, locals);
            for arm in &match_expr.arms {
                let mut arm_locals = locals.clone();
                add_pattern_binding_names(&arm.pattern, &mut arm_locals);
                collect_block_var_refs(&arm.body, refs, &mut arm_locals);
            }
        }
        Expr::Call(call_expr) => {
            for arg in &call_expr.args {
                collect_expr_var_refs(arg, refs, locals);
            }
        }
        Expr::MethodCall(method_call) => {
            collect_expr_var_refs(&method_call.receiver, refs, locals);
            for arg in &method_call.args {
                collect_expr_var_refs(arg, refs, locals);
            }
        }
        Expr::StringLit(_) => {}
        Expr::ArrayLiteral(arr_lit) => {
            for elem in &arr_lit.elements {
                collect_expr_var_refs(elem, refs, locals);
            }
        }
        Expr::Index(index_expr) => {
            collect_expr_var_refs(&index_expr.target, refs, locals);
            collect_expr_var_refs(&index_expr.index, refs, locals);
        }
        Expr::Cast(cast_expr) => collect_expr_var_refs(&cast_expr.expr, refs, locals),
        Expr::Assign(assign_expr) => {
            collect_expr_var_refs(&assign_expr.target, refs, locals);
            collect_expr_var_refs(&assign_expr.value, refs, locals);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;

    #[test]
    fn enum_variant_hover_spans_are_distinct() {
        let src = r#"enum HttpClass {
    Success;
    ClientError;
    ServerError;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = parser::Parser::new(tokens).parse().unwrap();
        let mut checker = TypeChecker::new();
        let (result, errors) = checker.check_program_collecting(&program);
        assert!(errors.is_empty());
        let e = &program.enums[0];
        let mut spans = Vec::new();
        for v in &e.variants {
            assert!(
                !spans.contains(&v.span),
                "duplicate variant span for {}",
                v.name
            );
            spans.push(v.span);
            let doc = result
                .lsp_info
                .hover_docs
                .get(&v.span)
                .expect("variant hover doc");
            assert_eq!(doc, &format!("HttpClass::{}", v.name));
        }
    }

    #[test]
    fn access_log_file_enum_variant_spans_not_overlapped_by_types() {
        use crate::compiler::Compiler;
        use std::path::Path;

        let path = Path::new("examples/access_log.ion");
        let src = std::fs::read_to_string(path).expect("access_log.ion");
        let tokens = crate::lexer::Lexer::new(&src).tokenize().unwrap();
        let ast = parser::Parser::new(tokens).parse().unwrap();
        let mut compiler = Compiler::new();
        let _ = compiler.load_imports(path, &ast.imports);
        let program = compiler.merge_modules(&ast, path);
        let mut checker = TypeChecker::new();
        checker.set_module_exports(compiler.get_module_exports().clone());
        let mut module_paths = std::collections::HashMap::new();
        for import in &ast.imports {
            module_paths.insert(
                import.alias.clone(),
                compiler.resolve_import_path(&import.path, path),
            );
        }
        checker.set_module_paths(module_paths);
        let (result, errors) = checker.check_program_collecting_with_source(&program, &ast);
        assert!(errors.is_empty(), "{errors:?}");
        let http_class = ast
            .enums
            .iter()
            .find(|e| e.name == "HttpClass")
            .expect("HttpClass");
        for v in &http_class.variants {
            for (span, ty) in &result.lsp_info.types {
                if spans_overlap(*span, v.span) {
                    panic!(
                        "type `{}` at line {} col {} (w {}) overlaps variant {} at line {} col {}",
                        type_to_string(ty),
                        span.line,
                        span.column,
                        span.end - span.start,
                        v.name,
                        v.span.line,
                        v.span.column
                    );
                }
            }
        }
    }

    #[test]
    fn access_log_enum_variant_hover_not_shadowed_by_types() {
        let src = r#"
enum HttpClass {
    Success;
    ClientError;
    ServerError;
}

fn classify(code: int) -> int {
    let mut kind: HttpClass = HttpClass::ClientError;
    if code >= 200 && code < 300 {
        kind = HttpClass::Success;
    } else if code >= 500 && code < 600 {
        kind = HttpClass::ServerError;
    }
    match kind {
        HttpClass::ClientError if code == 401 => {}
        HttpClass::ClientError => {}
        HttpClass::ServerError => {}
        HttpClass::Success => {}
    };
    return 0;
}
"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = parser::Parser::new(tokens).parse().unwrap();
        let mut checker = TypeChecker::new();
        let (result, errors) = checker.check_program_collecting(&program);
        assert!(errors.is_empty(), "{errors:?}");
        let http_class = &program.enums[0];
        for v in &http_class.variants {
            let doc = result.lsp_info.hover_docs.get(&v.span);
            assert_eq!(
                doc.map(String::as_str),
                Some(format!("HttpClass::{}", v.name).as_str()),
                "hover doc for {}",
                v.name
            );
            assert!(
                !result.lsp_info.types.contains_key(&v.span),
                "expression type shadowing variant {} hover",
                v.name
            );
            for (span, ty) in &result.lsp_info.types {
                if spans_overlap(*span, v.span) {
                    panic!(
                        "type `{}` at {:?} overlaps variant {} span {:?}",
                        type_to_string(ty),
                        span,
                        v.name,
                        v.span
                    );
                }
            }
        }
    }

    fn spans_overlap(a: Span, b: Span) -> bool {
        a.line == b.line
            && a.column < b.column + (b.end - b.start)
            && b.column < a.column + (a.end - a.start)
    }

    #[test]
    fn check_program_collecting_reports_multiple_function_errors() {
        let src = r#"
fn bad_a() -> int {
    let x = Box::new(1);
    let y = x;
    return Box::unwrap(x);
}

fn bad_b() -> int {
    let a = Box::new(2);
    let b = a;
    return Box::unwrap(a);
}
"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = parser::Parser::new(tokens).parse().unwrap();
        let mut checker = TypeChecker::new();
        let (_result, errors) = checker.check_program_collecting(&program);
        assert_eq!(errors.len(), 2);
        assert!(
            errors
                .iter()
                .all(|e| matches!(e, TypeCheckError::UseAfterMove { .. }))
        );
    }
}
