use crate::ast::*;
use std::collections::HashMap;
use std::path::PathBuf;

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
}

#[derive(Debug, Clone, Default)]
pub struct LspInfo {
    pub references: HashMap<Span, LspTarget>,
    pub types: HashMap<Span, Type>,
    /// Hover text keyed by definition span (functions, structs, enums).
    pub hover_docs: HashMap<Span, String>,
    /// Top-level and imported symbol names for completion.
    pub completion_symbols: Vec<String>,
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
    ReferenceEscape {
        description: String,
        span: Span,
    },
    Message(String),
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
        self.lsp_info.hover_docs.insert(span, doc);
    }

    fn record_completion_symbol(&mut self, name: &str) {
        if !self.lsp_info.completion_symbols.contains(&name.to_string()) {
            self.lsp_info.completion_symbols.push(name.to_string());
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
        self.lsp_info.references.insert(use_span, target);
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

    /// Check if a call expression is to a built-in function and type-check it.
    /// Returns Some(return_type) if it's a built-in, None otherwise.
    fn check_builtin_call(&mut self, call_expr: &CallExpr) -> Result<Option<Type>, TypeCheckError> {
        let callee = &call_expr.callee;

        // Box::new<T>(value: T) -> Box<T>
        if callee.starts_with("Box::new") {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let value_ty = self.check_expr(&call_expr.args[0])?;
            return Ok(Some(Type::Box {
                inner: Box::new(value_ty),
            }));
        }

        // Box::unwrap<T>(box: Box<T>) -> T
        if callee == "Box::unwrap" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let box_ty = self.check_expr(&call_expr.args[0])?;
            return match box_ty {
                Type::Box { inner } => Ok(Some(*inner)),
                _ => Err(TypeCheckError::TypeMismatch {
                    expected: "Box<T>".to_string(),
                    got: type_to_string(&box_ty),
                    span: call_expr.span,
                }),
            };
        }

        // Vec::new<T>() -> Vec<T>
        // Note: We can't infer T from empty args, so this requires type annotation
        // For now, we'll return Vec<Int> as a default and let codegen handle it
        if callee == "Vec::new" {
            if !call_expr.args.is_empty() {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "0 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            // Return Vec<Int> as default - actual type should come from context
            return Ok(Some(Type::Vec {
                elem_type: Box::new(Type::Int),
            }));
        }

        // Vec::with_capacity<T>(cap: int) -> Vec<T>
        if callee == "Vec::with_capacity" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let cap_ty = self.check_expr(&call_expr.args[0])?;
            if !types_equal(&cap_ty, &Type::Int) {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "int".to_string(),
                    got: type_to_string(&cap_ty),
                    span: call_expr.args[0].span(),
                });
            }
            // Return Vec<Int> as default
            return Ok(Some(Type::Vec {
                elem_type: Box::new(Type::Int),
            }));
        }

        // Vec::len<T>(vec: &Vec<T>) -> int
        if callee == "Vec::len" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: false,
            } = vec_ty
                && let Type::Vec { .. } = **inner_ty
            {
                return Ok(Some(Type::Int));
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::capacity<T>(vec: &Vec<T>) -> int
        if callee == "Vec::capacity" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: false,
            } = vec_ty
                && let Type::Vec { .. } = **inner_ty
            {
                return Ok(Some(Type::Int));
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::push<T>(vec: &mut Vec<T>, value: T)
        if callee == "Vec::push" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            let value_ty = self.check_expr(&call_expr.args[1])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = vec_ty
                && let Type::Vec { ref elem_type } = **inner_ty
            {
                if !types_equal(&value_ty, elem_type) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: type_to_string(elem_type),
                        got: type_to_string(&value_ty),
                        span: call_expr.args[1].span(),
                    });
                }
                return Ok(Some(Type::Int)); // void return
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::pop<T>(vec: &mut Vec<T>) -> Option<T>
        if callee == "Vec::pop" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = vec_ty
                && let Type::Vec { ref elem_type } = **inner_ty
            {
                // Check if Option<T> enum exists
                if self.enums.contains_key("Option") {
                    return Ok(Some(Type::Generic {
                        name: "Option".to_string(),
                        params: vec![(**elem_type).clone()],
                    }));
                } else {
                    // Return Option<Int> as fallback
                    return Ok(Some(Type::Generic {
                        name: "Option".to_string(),
                        params: vec![Type::Int],
                    }));
                }
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::get<T>(vec: &Vec<T>, index: int) -> Option<T>
        if callee == "Vec::get" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            let index_ty = self.check_expr(&call_expr.args[1])?;
            if !types_equal(&index_ty, &Type::Int) {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "int".to_string(),
                    got: type_to_string(&index_ty),
                    span: call_expr.args[1].span(),
                });
            }
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: false,
            } = vec_ty
                && let Type::Vec { ref elem_type } = **inner_ty
            {
                if self.enums.contains_key("Option") {
                    return Ok(Some(Type::Generic {
                        name: "Option".to_string(),
                        params: vec![(**elem_type).clone()],
                    }));
                } else {
                    return Ok(Some(Type::Generic {
                        name: "Option".to_string(),
                        params: vec![Type::Int],
                    }));
                }
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::set<T>(vec: &mut Vec<T>, index: int, value: T) -> int
        if callee == "Vec::set" {
            if call_expr.args.len() != 3 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "3 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            let index_ty = self.check_expr(&call_expr.args[1])?;
            let value_ty = self.check_expr(&call_expr.args[2])?;
            if !types_equal(&index_ty, &Type::Int) {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "int".to_string(),
                    got: type_to_string(&index_ty),
                    span: call_expr.args[1].span(),
                });
            }
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = vec_ty
                && let Type::Vec { ref elem_type } = **inner_ty
            {
                if !types_equal(&value_ty, elem_type) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: type_to_string(elem_type),
                        got: type_to_string(&value_ty),
                        span: call_expr.args[2].span(),
                    });
                }
                return Ok(Some(Type::Int)); // 0 on success, -1 on failure
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // String::new() -> String
        if callee == "String::new" {
            if !call_expr.args.is_empty() {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "0 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            return Ok(Some(Type::String));
        }

        // String::from(s: &str) -> String
        // Note: &str is not a real type in Ion yet, so we'll accept String for now
        if callee == "String::from" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let _arg_ty = self.check_expr(&call_expr.args[0])?;
            // Accept any type for now (String or string literal)
            return Ok(Some(Type::String));
        }

        // String::len(s: &String) -> int
        if callee == "String::len" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let str_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: false,
            } = str_ty
                && let Type::String = **inner_ty
            {
                return Ok(Some(Type::Int));
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&String".to_string(),
                got: type_to_string(&str_ty),
                span: call_expr.args[0].span(),
            });
        }

        // String::push_str(s: &mut String, other: &str)
        if callee == "String::push_str" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let str_ty = self.check_expr(&call_expr.args[0])?;
            let _other_ty = self.check_expr(&call_expr.args[1])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = str_ty
                && let Type::String = **inner_ty
            {
                return Ok(Some(Type::Int)); // void return
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut String".to_string(),
                got: type_to_string(&str_ty),
                span: call_expr.args[0].span(),
            });
        }

        // channel<T>() -> (Sender<T>, Receiver<T>)
        // This should only be called with tuple destructuring, handled in check_stmt
        if callee == "channel" {
            if !call_expr.args.is_empty() {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "0 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            // channel() needs type context from tuple destructuring
            // Return None so it can be handled in tuple destructuring context
            return Ok(None);
        }

        // Not a built-in function
        Ok(None)
    }

    pub fn check_program(&mut self, program: &Program) -> Result<TypeCheckResult, TypeCheckError> {
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
                    return Err(TypeCheckError::ReferenceEscape {
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
                        return Err(TypeCheckError::ReferenceEscape {
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

        for s in &program.structs {
            self.record_completion_symbol(&s.name);
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
        }
        for e in &program.enums {
            self.record_completion_symbol(&e.name);
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
        }
        for function in &program.functions {
            if function.pub_ {
                self.record_completion_symbol(&function.name);
            }
            let params: Vec<String> = function
                .params
                .iter()
                .map(|p| format!("{}: {}", p.name, type_to_string(&p.ty)))
                .collect();
            let ret = function
                .return_type
                .as_ref()
                .map(type_to_string)
                .unwrap_or_else(|| "void".to_string());
            let generics = if function.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", function.generics.join(", "))
            };
            self.record_hover_doc(
                function.span,
                format!(
                    "fn {}{}({}) -> {}",
                    function.name,
                    generics,
                    params.join(", "),
                    ret
                ),
            );
            self.check_function(function)?;
        }

        // Return the collected LSP information
        Ok(TypeCheckResult {
            lsp_info: self.lsp_info.clone(),
        })
    }

    /// Resolve a type name - if it's Type::Struct(name) but name is actually an enum, convert to Type::Enum(name)
    /// Also resolves type aliases transparently
    fn resolve_type_name(&self, ty: &Type) -> Result<Type, TypeCheckError> {
        match ty {
            Type::Struct(name) => {
                // Check if this is actually a type alias
                if let Some(alias) = self.type_aliases.get(name) {
                    // Resolve the alias (recursively in case of nested aliases)
                    let resolved = self.resolve_type_name(&alias.target)?;
                    return Ok(resolved);
                }
                // Check if this is actually an enum
                if self.enums.contains_key(name) {
                    Ok(Type::Enum(name.clone()))
                } else {
                    Ok(ty.clone())
                }
            }
            Type::Generic { name, params } => {
                // Check if this is a generic type alias
                if let Some(alias) = self.type_aliases.get(name)
                    && !alias.generics.is_empty()
                    && alias.generics.len() == params.len()
                {
                    // Substitute generic parameters in the target type
                    let substitutions: HashMap<String, &Type> = alias
                        .generics
                        .iter()
                        .zip(params.iter())
                        .map(|(gen_name, param_ty)| (gen_name.clone(), param_ty))
                        .collect();
                    let resolved = Self::substitute_type_params(&alias.target, &substitutions);
                    return self.resolve_type_name(&resolved);
                }
                Ok(ty.clone())
            }
            _ => Ok(ty.clone()),
        }
    }

    /// Extract the base type name for method lookup from a resolved type.
    /// Returns (type_name, is_reference, is_mutable_ref).
    /// For example:
    ///   Vec<int> -> ("Vec", false, false)
    ///   &Vec<int> -> ("Vec", true, false)
    ///   &mut Vec<int> -> ("Vec", true, true)
    fn extract_type_name_for_method(ty: &Type) -> Result<(String, bool, bool), TypeCheckError> {
        match ty {
            Type::Ref { inner, mutable } => {
                // Extract type name from reference
                let (name, _, _) = Self::extract_type_name_for_method(inner)?;
                Ok((name, true, *mutable))
            }
            Type::Vec { .. } => {
                // For generic types, we need to construct the qualified name with type params
                // But for method lookup, we just use "Vec"
                Ok(("Vec".to_string(), false, false))
            }
            Type::String => Ok(("String".to_string(), false, false)),
            Type::Box { .. } => Ok(("Box".to_string(), false, false)),
            Type::Struct(name) => Ok((name.clone(), false, false)),
            Type::Enum(name) => Ok((name.clone(), false, false)),
            Type::Generic { name, .. } => Ok((name.clone(), false, false)),
            _ => Err(TypeCheckError::Message(format!(
                "Cannot call methods on primitive type '{}'",
                type_to_string(ty)
            ))),
        }
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
                VariableInfo {
                    ty: resolved_param_ty,
                    state: OwnershipState::Valid,
                    definition_span: function.span, // Params defined at function declaration
                },
            );
        }

        // Check function body
        for stmt in &function.body.statements {
            self.check_stmt(stmt)?;
        }

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
                    if patterns.len() != 2 {
                        return Err(TypeCheckError::Message(format!(
                            "Tuple destructuring currently only supports exactly 2 patterns (for channel split), got {}",
                            patterns.len()
                        )));
                    }

                    // Check if this is a channel() call
                    if let Some(ref init) = let_stmt.init
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
                                            VariableInfo {
                                                ty: sender_ty,
                                                state: OwnershipState::Valid,
                                                definition_span: let_stmt.span,
                                            },
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
                                            VariableInfo {
                                                ty: receiver_ty,
                                                state: OwnershipState::Valid,
                                                definition_span: let_stmt.span,
                                            },
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

                    return Err(TypeCheckError::Message(
                        "Tuple destructuring is currently only supported for channel() calls"
                            .to_string(),
                    ));
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

                    // New variable starts as Valid (it owns the value from init)
                    self.variables.insert(
                        let_stmt.name.clone(),
                        VariableInfo {
                            ty: var_type.clone(),
                            state: OwnershipState::Valid,
                            definition_span: let_stmt.span,
                        },
                    );
                    if !let_stmt.name.is_empty() {
                        self.lsp_info
                            .types
                            .insert(let_stmt.name_span, var_type);
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
                        VariableInfo {
                            ty: resolved_type.clone(),
                            state: OwnershipState::Valid,
                            definition_span: let_stmt.span,
                        },
                    );
                    if !let_stmt.name.is_empty() {
                        self.lsp_info
                            .types
                            .insert(let_stmt.name_span, resolved_type);
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
                for inner in &if_stmt.then_block.statements {
                    self.check_stmt(inner)?;
                }
                let then_env = self.variables.clone();

                // Else-branch: same starting environment; if no else, treat as no-op.
                let else_env = if let Some(ref else_blk) = if_stmt.else_block {
                    self.variables = before.clone();
                    for inner in &else_blk.statements {
                        self.check_stmt(inner)?;
                    }
                    self.variables.clone()
                } else {
                    before.clone()
                };

                // Merge environments conservatively: a variable may only be considered
                // moved if it is moved in *both* branches. If it is moved in one
                // branch but not the other, we conservatively report an error.
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

                    let merged_state = match (then_state, else_state) {
                        (OwnershipState::Valid, OwnershipState::Valid) => OwnershipState::Valid,
                        (OwnershipState::Moved, OwnershipState::Moved) => OwnershipState::Moved,
                        // Moved in only one branch: future use would be ambiguous.
                        _ => {
                            return Err(TypeCheckError::UseAfterMove {
                                name: name.clone(),
                                span: if_stmt.span,
                            });
                        }
                    };

                    if let Some(info) = merged.get_mut(name) {
                        info.state = merged_state;
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
                    self.variables.insert(
                        name,
                        VariableInfo {
                            ty,
                            state: OwnershipState::Valid,
                            definition_span: spawn_stmt.span, // Captured vars defined at spawn
                        },
                    );
                }
                // Check the body of the spawn block
                for inner in &spawn_stmt.body.statements {
                    self.check_stmt(inner)?;
                }
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
                self.variables = before;
                // Check body
                self.loop_depth += 1;
                for inner in &while_stmt.body.statements {
                    self.check_stmt(inner)?;
                }
                self.loop_depth -= 1;
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
                for inner in &unsafe_stmt.body.statements {
                    self.check_stmt(inner)?;
                }

                // Exit unsafe context
                self.unsafe_context_depth -= 1;
            }
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
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
                        // Return function type (for now, just return int as placeholder)
                        // In a full implementation, we'd return a function type
                        Ok(Type::Int)
                    } else {
                        Err(TypeCheckError::UndefinedVariable {
                            name: format!("{}::{}", module_name, item_name),
                            span: var_expr.span,
                        })
                    }
                } else {
                    // Simple variable lookup
                    let var_info = self.variables.get(&var_expr.name).ok_or_else(|| {
                        TypeCheckError::UndefinedVariable {
                            name: var_expr.name.clone(),
                            span: var_expr.span,
                        }
                    })?;

                    // Check for use-after-move
                    if var_info.state == OwnershipState::Moved {
                        return Err(TypeCheckError::UseAfterMove {
                            name: var_expr.name.clone(),
                            span: var_expr.span,
                        });
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
                    self.lsp_info.types.insert(var_expr.span, var_ty.clone());

                    Ok(var_ty)
                }
            }
            Expr::Ref(ref_expr) => {
                // Check the inner expression
                let inner_type = self.check_expr(&ref_expr.inner)?;

                // Return the reference type
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
                let base_ty = self.check_expr(&acc.base)?;
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
                        Ok(field.ty.clone())
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
                        Ok(substitute_generic_types_impl(&field.ty, &substitutions))
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

                    // Type-check the arm body and ensure all arms return the same type
                    for stmt in &arm.body.statements {
                        self.check_stmt(stmt)?;
                    }
                    self.variables = prev_vars;

                    // For now, assume match returns int (will be improved later)
                    match_result_type = Some(Type::Int);
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

                Ok(match_result_type.unwrap_or(Type::Int))
            }
            Expr::Call(call_expr) => {
                // Check if this is a built-in function first
                if let Some(return_type) = self.check_builtin_call(call_expr)? {
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
                }

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

                Ok(return_type_opt.unwrap_or(Type::Int))
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
                let target_ty = self.check_expr(&index_expr.target)?;

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

    /// Check expression for moves and mark variables as Moved.
    /// This is called before using an expression in contexts that move ownership
    /// (assignment, return, function call arguments).
    fn check_expr_for_moves(&mut self, expr: &Expr) -> Result<(), TypeCheckError> {
        match expr {
            Expr::Lit(_) => Ok(()),          // Literals don't move anything
            Expr::BoolLiteral(_) => Ok(()),  // Boolean literals don't move anything
            Expr::FloatLiteral(_) => Ok(()), // Float literals don't move anything
            Expr::Var(var_expr) => {
                let var_info = self.variables.get_mut(&var_expr.name).ok_or_else(|| {
                    TypeCheckError::UndefinedVariable {
                        name: var_expr.name.clone(),
                        span: var_expr.span,
                    }
                })?;

                // Check for use-after-move
                if var_info.state == OwnershipState::Moved {
                    return Err(TypeCheckError::UseAfterMove {
                        name: var_expr.name.clone(),
                        span: var_expr.span,
                    });
                }

                // Primitives and references are copied, not moved (see ION_SPEC §5.2).
                if Self::is_copy_type(&var_info.ty) {
                    return Ok(());
                }

                // Mark as moved
                var_info.state = OwnershipState::Moved;
                Ok(())
            }
            Expr::Ref(ref_expr) => {
                // Reference expressions (&x, &mut x) borrow, they don't move
                // Just check the inner expression is valid
                self.check_expr(&ref_expr.inner)?;
                Ok(())
            }
            Expr::StructLit(lit) => {
                // Moving a struct literal moves each of its value expressions.
                for field in &lit.fields {
                    self.check_expr_for_moves(&field.value)?;
                }
                Ok(())
            }
            Expr::FieldAccess(acc) => {
                // Field access reads from the base but does not move the entire struct.
                self.check_expr(&acc.base)?;
                Ok(())
            }
            Expr::BinOp(bin_op_expr) => {
                // Binary operations use their operands (read), but don't move them
                // They only read the values
                self.check_expr(&bin_op_expr.left)?;
                self.check_expr(&bin_op_expr.right)?;
                Ok(())
            }
            Expr::UnOp(un_op_expr) => {
                // Unary operations use their operand (read), but don't move it
                self.check_expr(&un_op_expr.operand)?;
                Ok(())
            }
            Expr::Send(send_expr) => {
                // Sending moves the value operand; the channel itself is only read.
                self.check_expr_for_moves(&send_expr.value)
            }
            Expr::Recv(recv_expr) => {
                // Receiving from a channel does not move any existing variable;
                // it produces a fresh value.
                self.check_expr(&recv_expr.channel)?;
                Ok(())
            }
            Expr::EnumLit(enum_lit) => {
                // Check moves in enum literal arguments
                for arg in &enum_lit.args {
                    self.check_expr_for_moves(arg)?;
                }
                Ok(())
            }
            Expr::Match(match_expr) => {
                // Check moves in match expression
                self.check_expr_for_moves(&match_expr.expr)?;
                // Match arms don't move the matched value, they just pattern match
                Ok(())
            }
            Expr::Call(call_expr) => {
                for arg in &call_expr.args {
                    self.check_expr_for_moves(arg)?;
                }
                Ok(())
            }
            Expr::MethodCall(method_call) => {
                // Method calls need to check moves in receiver and arguments
                // The receiver might be moved or borrowed depending on method signature
                // For now, just check the receiver is valid (will be handled in desugaring)
                self.check_expr(&method_call.receiver)?;
                for arg in &method_call.args {
                    self.check_expr_for_moves(arg)?;
                }
                Ok(())
            }
            Expr::StringLit(_) => Ok(()), // String literals don't move anything
            Expr::ArrayLiteral(arr_lit) => {
                // Array literals move their elements
                for elem in &arr_lit.elements {
                    self.check_expr_for_moves(elem)?;
                }
                Ok(())
            }
            Expr::Index(index_expr) => {
                // Indexing reads from the target but doesn't move it
                self.check_expr(&index_expr.target)?;
                self.check_expr(&index_expr.index)?;
                Ok(())
            }
            Expr::Cast(cast_expr) => {
                // Casting moves the expression
                self.check_expr_for_moves(&cast_expr.expr)?;
                Ok(())
            }
            Expr::Assign(assign_expr) => {
                // Assignment moves the value, but not the target
                self.check_expr(&assign_expr.target)?; // Check target is valid
                self.check_expr_for_moves(&assign_expr.value)?; // Move the value
                Ok(())
            }
        }
    }

    /// Types copied rather than moved at the ownership level (ION_SPEC §5.2).
    fn is_copy_type(ty: &Type) -> bool {
        matches!(
            ty,
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
                | Type::UInt
                | Type::Ref { .. }
        )
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

fn types_equal(a: &Type, b: &Type) -> bool {
    // Note: Type alias resolution should happen before calling types_equal
    // The caller should use resolve_type_name first
    match (a, b) {
        (Type::Int, Type::Int) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::F32, Type::F32) => true,
        (Type::F64, Type::F64) => true,
        (Type::I8, Type::I8) => true,
        (Type::I16, Type::I16) => true,
        (Type::I32, Type::I32) => true,
        (Type::I64, Type::I64) => true,
        (Type::U8, Type::U8) => true,
        (Type::U16, Type::U16) => true,
        (Type::U32, Type::U32) => true,
        (Type::U64, Type::U64) => true,
        (Type::UInt, Type::UInt) => true,
        (
            Type::Ref {
                inner: a_inner,
                mutable: a_mut,
            },
            Type::Ref {
                inner: b_inner,
                mutable: b_mut,
            },
        ) => a_mut == b_mut && types_equal(a_inner, b_inner),
        (Type::RawPtr { inner: a_inner }, Type::RawPtr { inner: b_inner }) => {
            types_equal(a_inner, b_inner)
        }
        (Type::Channel { elem_type: a_elem }, Type::Channel { elem_type: b_elem }) => {
            types_equal(a_elem, b_elem)
        }
        (Type::Struct(a_name), Type::Struct(b_name)) => a_name == b_name,
        (Type::Enum(a_name), Type::Enum(b_name)) => a_name == b_name,
        // Allow base struct/enum types to match generic instantiations with same base name
        (Type::Struct(a_name), Type::Generic { name: b_name, .. }) => a_name == b_name,
        (Type::Generic { name: a_name, .. }, Type::Struct(b_name)) => a_name == b_name,
        (Type::Enum(a_name), Type::Generic { name: b_name, .. }) => a_name == b_name,
        (Type::Generic { name: a_name, .. }, Type::Enum(b_name)) => a_name == b_name,
        (Type::String, Type::String) => true,
        (Type::Box { inner: a_inner }, Type::Box { inner: b_inner }) => {
            types_equal(a_inner, b_inner)
        }
        (Type::Vec { elem_type: a_elem }, Type::Vec { elem_type: b_elem }) => {
            types_equal(a_elem, b_elem)
        }
        (
            Type::Array {
                inner: a_inner,
                size: a_size,
            },
            Type::Array {
                inner: b_inner,
                size: b_size,
            },
        ) => a_size == b_size && types_equal(a_inner, b_inner),
        (Type::Slice { inner: a_inner }, Type::Slice { inner: b_inner }) => {
            types_equal(a_inner, b_inner)
        }
        (Type::Sender { elem_type: a_elem }, Type::Sender { elem_type: b_elem }) => {
            types_equal(a_elem, b_elem)
        }
        (Type::Receiver { elem_type: a_elem }, Type::Receiver { elem_type: b_elem }) => {
            types_equal(a_elem, b_elem)
        }
        (Type::Tuple { elements: a_elems }, Type::Tuple { elements: b_elems }) => {
            a_elems.len() == b_elems.len()
                && a_elems
                    .iter()
                    .zip(b_elems.iter())
                    .all(|(a, b)| types_equal(a, b))
        }
        (
            Type::Generic {
                name: a_name,
                params: a_params,
            },
            Type::Generic {
                name: b_name,
                params: b_params,
            },
        ) => {
            a_name == b_name
                && a_params.len() == b_params.len()
                && a_params
                    .iter()
                    .zip(b_params.iter())
                    .all(|(a, b)| types_equal(a, b))
        }
        _ => false,
    }
}

pub fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Bool => "bool".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::U8 => "u8".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::UInt => "uint".to_string(),
        Type::Ref { inner, mutable } => {
            if *mutable {
                format!("&mut {}", type_to_string(inner))
            } else {
                format!("&{}", type_to_string(inner))
            }
        }
        Type::RawPtr { inner } => {
            format!("*{}", type_to_string(inner))
        }
        Type::Channel { elem_type } => {
            format!("channel<{}>", type_to_string(elem_type))
        }
        Type::Struct(name) => name.clone(),
        Type::Enum(name) => name.clone(),
        Type::Generic { name, params } => {
            let param_strs: Vec<String> = params.iter().map(type_to_string).collect();
            format!("{}<{}>", name, param_strs.join(", "))
        }
        Type::Box { inner } => format!("Box<{}>", type_to_string(inner)),
        Type::Vec { elem_type } => format!("Vec<{}>", type_to_string(elem_type)),
        Type::String => "String".to_string(),
        Type::Array { inner, size } => format!("[{}; {}]", type_to_string(inner), size),
        Type::Slice { inner } => format!("[]{}", type_to_string(inner)),
        Type::Sender { elem_type } => format!("Sender<{}>", type_to_string(elem_type)),
        Type::Receiver { elem_type } => format!("Receiver<{}>", type_to_string(elem_type)),
        Type::Tuple { elements } => {
            let elem_strs: Vec<String> = elements.iter().map(type_to_string).collect();
            format!("({})", elem_strs.join(", "))
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
                                                VariableInfo {
                                                    ty: concrete_field_ty,
                                                    state: OwnershipState::Valid,
                                                    definition_span: *span,
                                                },
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
                                            VariableInfo {
                                                ty: concrete_payload_ty,
                                                state: OwnershipState::Valid,
                                                definition_span: *span,
                                            },
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
                    VariableInfo {
                        ty: expr_ty.clone(),
                        state: OwnershipState::Valid,
                        definition_span: *span,
                    },
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
