mod builtins;
mod drop;
mod types;

use self::types::{
    fn_type_to_c_decl, fn_type_to_c_function_header, format_ret_val_decl, mangle_module_callee,
    mangle_type_name, resolve_type_alias, ret_val_decl, substitute_type_params, tuple_type_name,
    type_to_c_impl, type_to_c_return_type,
};

use crate::ast::{
    BinOp, EnumDecl, EnumVariant, ExternBlock, Program, Span, StructDecl, Type, TypeAliasDecl, UnOp,
};
use crate::ir::*;
use crate::types_util::{is_ref_to_vec, ref_to_vec_elem};
use std::collections::HashMap;

enum BoundsCheck {
    Fixed(usize),
    StringLen,
    SliceLen { by_ref: bool },
}

fn is_send_value_lvalue(expr: &IREexpr) -> bool {
    match expr {
        IREexpr::Var(_) => true,
        IREexpr::FieldAccess { base, .. } => is_send_value_lvalue(base),
        IREexpr::Index { target, .. } => is_send_value_lvalue(target),
        _ => false,
    }
}

fn escape_c_comment_text(s: &str) -> String {
    s.replace("*/", "* /")
}

#[derive(Clone)]
struct ScopeBinding {
    name: String,
    ty: Type,
    dropped: bool,
    read: bool,
}

#[derive(Clone)]
struct ScopeFrame {
    bindings: Vec<ScopeBinding>,
    defers: Vec<IREexpr>,
}

pub struct Codegen {
    output: String,
    indent_level: usize,
    enum_map: HashMap<String, EnumDecl>, // Map enum names to declarations for variant lookups
    generated_types: HashMap<String, bool>, // Track which monomorphized types have been generated
    struct_map: HashMap<String, StructDecl>, // Map struct names to declarations
    generic_instantiations: HashMap<String, (String, Vec<Type>)>, // Map base name to (monomorphized_name, params)
    match_counter: usize, // Counter for unique match variable names
    type_aliases: HashMap<String, TypeAliasDecl>, // Map type alias names to their declarations
    extern_functions: HashMap<String, Vec<Type>>, // Map extern function names to their parameter types
    current_return_type: Option<Type>, // Current function's return type (for array return handling)
    function_return_types: HashMap<String, Option<Type>>, // Map function names to their return types
    function_param_types: HashMap<String, Vec<Type>>,     // Map function names to parameter types
    in_unsafe_block: bool,                                // Track if we're in an unsafe block
    temp_var_counter: usize, // Counter for unique temporary variable names
    current_function_params: HashMap<String, Type>, // Track current function parameter types for field access
    spawn_counter: usize,
    spawn_forward_decls: String,
    spawn_definitions: String,
    fn_literal_forward_decls: String,
    fn_literal_definitions: String,
    generated_fn_literals: std::collections::HashSet<String>,
    scope_stack: Vec<ScopeFrame>,
    epilogue_label: String,
    loop_continue_label: Option<String>,
    /// When true (single-file merge), module calls use `{alias}_{func}` C names.
    mangle_merged_module_calls: bool,
    /// Set during multi-file codegen: prefix this module's functions (`io_print_int`).
    multi_file_module: Option<String>,
    /// Owned struct fields moved in the current statement; flushed after the statement completes.
    pending_field_nulls: Vec<String>,
    pending_field_null_set: std::collections::HashSet<String>,
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}

impl Codegen {
    pub fn new() -> Self {
        Codegen {
            output: String::new(),
            indent_level: 0,
            enum_map: HashMap::new(),
            generated_types: HashMap::new(),
            struct_map: HashMap::new(),
            generic_instantiations: HashMap::new(),
            match_counter: 0,
            type_aliases: HashMap::new(),
            extern_functions: HashMap::new(),
            current_return_type: None,
            function_return_types: HashMap::new(),
            function_param_types: HashMap::new(),
            in_unsafe_block: false,
            temp_var_counter: 0,
            current_function_params: HashMap::new(),
            spawn_counter: 0,
            spawn_forward_decls: String::new(),
            spawn_definitions: String::new(),
            fn_literal_forward_decls: String::new(),
            fn_literal_definitions: String::new(),
            generated_fn_literals: std::collections::HashSet::new(),
            scope_stack: Vec::new(),
            epilogue_label: "epilogue".to_string(),
            loop_continue_label: None,
            mangle_merged_module_calls: false,
            multi_file_module: None,
            pending_field_nulls: Vec::new(),
            pending_field_null_set: std::collections::HashSet::new(),
        }
    }

    fn resolve_c_function_name(&self, callee: &str) -> String {
        if callee == "main" {
            return "main".to_string();
        }
        if self.extern_functions.contains_key(callee) {
            return callee.to_string();
        }
        let mangle_modules = self.mangle_merged_module_calls || self.multi_file_module.is_some();
        if mangle_modules && let Some(mangled) = mangle_module_callee(callee) {
            return mangled;
        }
        if let Some(module) = &self.multi_file_module {
            if callee.contains("::") {
                return callee.split("::").last().unwrap_or(callee).to_string();
            }
            return format!("{}_{}", module, callee);
        }
        if callee.contains("::") {
            callee.split("::").last().unwrap_or(callee).to_string()
        } else {
            callee.to_string()
        }
    }

    fn module_c_symbol(&self, ion_name: &str) -> String {
        self.resolve_c_function_name(ion_name)
    }

    fn emit_generated_banner(&mut self, source_ion: &str) {
        let source_ion = escape_c_comment_text(source_ion);
        self.writeln(&format!(
            "/* Generated by ion-compiler from {}. Do not edit.",
            source_ion
        ));
        self.writeln(" * GNU C (GCC/Clang). Merged stdlib/import code may appear below. */");
        self.writeln("");
    }

    fn should_silence_unused_binding(name: &str, ty: &Type) -> bool {
        matches!(ty, Type::Ref { .. } | Type::RawPtr { .. }) || name.starts_with('_')
    }

    fn emit_silence_unused_binding(&mut self, name: &str) {
        self.write_indent();
        self.writeln(&format!("(void){name};"));
    }

    fn enum_variant_index(enum_decl: &EnumDecl, variant_name: &str) -> Option<usize> {
        enum_decl
            .variants
            .iter()
            .position(|v| v.name == variant_name)
    }

    fn emit_enum_variant_compound_literal(
        &mut self,
        c_type_name: &str,
        enum_base_name: &str,
        variant_name: &str,
        args: &[IREexpr],
        named_fields: Option<&[(String, IREexpr)]>,
    ) {
        let enum_decl = self
            .enum_map
            .get(enum_base_name)
            .cloned()
            .unwrap_or_else(|| panic!("unknown enum `{enum_base_name}` in enum literal codegen"));
        let variant_idx = Self::enum_variant_index(&enum_decl, variant_name).unwrap_or_else(|| {
            panic!("unknown variant `{variant_name}` on enum `{enum_base_name}`")
        });
        let variant = &enum_decl.variants[variant_idx];
        let has_payloads = !variant.payload_types.is_empty() || variant.named_fields.is_some();

        self.write(&format!(
            "({c_type_name}){{ .tag = {variant_idx}, .data = {{"
        ));
        if has_payloads {
            self.write(&format!(" .variant_{variant_idx} = {{"));
            if let Some(named_fields) = named_fields {
                for (i, (field_name, field_expr)) in named_fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!(" .{field_name} = "));
                    self.generate_expr(field_expr);
                }
            } else {
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!(" .arg{i} = "));
                    self.generate_expr(arg);
                }
            }
            self.write(" }");
        }
        self.write(" } }");
    }

    fn param_is_byte_ptr(param_ty: &Type) -> bool {
        match param_ty {
            Type::RawPtr { inner } => matches!(**inner, Type::U8),
            Type::Ref { inner, .. } => matches!(**inner, Type::U8),
            _ => false,
        }
    }

    pub fn generate(&mut self, program: &IRProgram, source_ion: &str) -> String {
        self.output.clear();
        self.indent_level = 0;
        self.mangle_merged_module_calls = true;
        // Build enum map for variant index lookups
        self.enum_map.clear();
        for e in &program.enums {
            self.enum_map.insert(e.name.clone(), e.clone());
        }
        // Build struct map for generic struct lookups
        self.struct_map.clear();
        for s in &program.structs {
            self.struct_map.insert(s.name.clone(), s.clone());
        }
        // Build type alias map for type resolution
        self.type_aliases.clear();
        for alias in &program.type_aliases {
            self.type_aliases.insert(alias.name.clone(), alias.clone());
        }
        // Build function return type map
        self.function_return_types.clear();
        self.function_param_types.clear();
        for function in &program.functions {
            self.function_return_types
                .insert(function.name.clone(), function.return_type.clone());
            self.function_param_types.insert(
                function.name.clone(),
                function.params.iter().map(|p| p.ty.clone()).collect(),
            );
        }
        self.generated_types.clear();
        self.spawn_counter = 0;
        self.spawn_forward_decls.clear();
        self.spawn_definitions.clear();
        self.fn_literal_forward_decls.clear();
        self.fn_literal_definitions.clear();
        self.generated_fn_literals.clear();

        self.emit_generated_banner(source_ion);

        // Generate includes if needed
        self.writeln("#include <stdio.h>");
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <string.h>");
        self.writeln("#include <stddef.h>");
        self.writeln("#include <stdint.h>"); // For integer types (int8_t, uint16_t, etc.)
        // Ion runtime (threads, channels, heap allocation, etc.)
        // Use ion_runtime.h and rely on compiler include paths
        self.writeln("#include \"ion_runtime.h\"");
        self.writeln("");

        // Collect all generic type instantiations used in the program
        // Store as (base_name, params) pairs - deduplicate using string keys
        let mut generic_instantiations_map: std::collections::HashMap<String, (String, Vec<Type>)> =
            std::collections::HashMap::new();
        collect_generic_instantiations(program, &mut generic_instantiations_map);

        // Also collect resolved generic types from type aliases
        // When we have Result<int> = Option<int>, we need to collect Option<int> too
        let mut resolved_instantiations = generic_instantiations_map.clone();
        for (base_name, params) in generic_instantiations_map.values() {
            // Resolve the type alias if it exists
            let ty = Type::Generic {
                name: base_name.clone(),
                params: params.clone(),
            };
            let resolved = resolve_type_alias(&ty, &self.type_aliases);
            // If resolved to a different generic type, collect it too
            if let Type::Generic {
                name: resolved_name,
                params: resolved_params,
            } = resolved
                && resolved_name != *base_name
            {
                let key = mangle_type_name(&resolved_name, &resolved_params);
                resolved_instantiations.insert(key, (resolved_name, resolved_params));
            }
        }

        self.generic_instantiations = resolved_instantiations;

        // Vec, slice, and tuple typedefs must precede struct fields that reference them.
        self.emit_vec_slice_tuple_typedefs(program);

        // Enums before structs so struct fields can use enum types by value.
        for e in &program.enums {
            if e.generics.is_empty() {
                self.generate_enum_type(e);
            }
        }

        for s in &program.structs {
            if s.generics.is_empty() {
                self.write(&format!("typedef struct {} {{\n", s.name));
                self.indent_level += 1;
                for field in &s.fields {
                    self.write_indent();
                    let field_decl = match &field.ty {
                        Type::Array { inner, size } => {
                            let base_type = self.type_to_c(inner);
                            format!("{} {}[{}]", base_type, field.name, size)
                        }
                        _ => {
                            format!("{} {}", self.type_to_c(&field.ty), field.name)
                        }
                    };
                    self.writeln(&format!("{};", field_decl));
                }
                self.indent_level -= 1;
                self.writeln(&format!("}} {};", s.name));
                self.writeln("");
            }
        }

        // Generate monomorphized struct/enum types for each generic instantiation
        // Use self.generic_instantiations which includes resolved types from aliases
        let mut instantiations_vec: Vec<_> = self.generic_instantiations.values().collect();
        // Sort for deterministic output
        instantiations_vec.sort_by_key(|(name, _)| name.clone());

        // Collect declarations first to avoid borrow checker issues
        let mut struct_instantiations: Vec<(StructDecl, Vec<Type>)> = Vec::new();
        let mut enum_instantiations: Vec<(EnumDecl, Vec<Type>)> = Vec::new();

        for (base_name, params) in instantiations_vec {
            let base_name_clone = base_name.clone();
            let params_clone = params.clone();

            if let Some(decl) = self.struct_map.get(&base_name_clone) {
                struct_instantiations.push((decl.clone(), params_clone));
            } else if let Some(decl) = self.enum_map.get(&base_name_clone) {
                enum_instantiations.push((decl.clone(), params_clone));
            }
        }

        // Now generate without borrowing self.struct_map/self.enum_map
        for (decl, params) in struct_instantiations {
            self.generate_monomorphized_struct(&decl, &params);
        }
        for (decl, params) in enum_instantiations {
            self.generate_monomorphized_enum(&decl, &params);
            // Mark as generated
            let key = mangle_type_name(&decl.name, &params);
            self.generated_types.insert(key, true);
        }

        // Handle built-in generic enums like Option<T> that aren't in enum_map
        // First, ensure Option template is in enum_map so match expressions can find variant indices
        // Only add synthetic template if Option is not already user-declared
        if !self.enum_map.contains_key("Option") {
            let option_template = EnumDecl {
                doc: None,
                pub_: false,
                name: "Option".to_string(),
                generics: vec!["T".to_string()],
                variants: vec![
                    EnumVariant {
                        doc: None,
                        name: "Some".to_string(),
                        payload_types: vec![Type::Generic {
                            name: "T".to_string(),
                            params: vec![],
                        }],
                        named_fields: None,
                        span: Span {
                            start: 0,
                            end: 0,
                            line: 0,
                            column: 0,
                        },
                    },
                    EnumVariant {
                        doc: None,
                        name: "None".to_string(),
                        payload_types: vec![],
                        named_fields: None,
                        span: Span {
                            start: 0,
                            end: 0,
                            line: 0,
                            column: 0,
                        },
                    },
                ],
                span: Span {
                    start: 0,
                    end: 0,
                    line: 0,
                    column: 0,
                },
            };
            self.enum_map.insert("Option".to_string(), option_template);
        }

        // Collect Option types to generate first to avoid borrow checker issues
        let mut option_types_to_generate: Vec<(String, Vec<Type>)> = Vec::new();
        for (key, (base_name, params)) in &self.generic_instantiations {
            if base_name == "Option" && !self.generated_types.contains_key(key) {
                option_types_to_generate.push((key.clone(), params.clone()));
            }
        }
        for (key, params) in option_types_to_generate {
            // Get Option template from enum_map (we already ensured it's there)
            let option_decl = self.enum_map.get("Option").unwrap().clone();
            self.generate_monomorphized_enum(&option_decl, &params);
            self.generated_types.insert(key, true);
        }

        // Collect and generate Vec type definitions
        let mut vec_types = std::collections::HashSet::new();
        collect_vec_types_impl(program, &mut vec_types);
        for vec_type_name in &vec_types {
            // Generate Option<T> for each Vec<T> since Vec::pop and Vec::get return Option<T>
            // Extract element type from Vec type name (e.g., "Vec_int" -> "int")
            if let Some(elem_type_str) = vec_type_name.strip_prefix("Vec_") {
                let option_type_name = format!("Option_{}", elem_type_str);
                if !self.generated_types.contains_key(&option_type_name) {
                    // Parse element type from string (e.g., "int" -> Type::Int)
                    let elem_type = match elem_type_str {
                        "int" => Type::Int,
                        "bool" => Type::Bool,
                        "f32" => Type::F32,
                        "f64" => Type::F64,
                        "i8" => Type::I8,
                        "i16" => Type::I16,
                        "i32" => Type::I32,
                        "i64" => Type::I64,
                        "u8" => Type::U8,
                        "u16" => Type::U16,
                        "u32" => Type::U32,
                        "u64" => Type::U64,
                        "uint" => Type::UInt,
                        _ => {
                            // Try to parse as a struct/enum type or skip complex types
                            // For now, skip complex types and only handle primitives
                            continue;
                        }
                    };

                    // Create a synthetic EnumDecl for Option<T>
                    let option_decl = EnumDecl {
                        doc: None,
                        pub_: false,
                        name: "Option".to_string(),
                        generics: vec!["T".to_string()],
                        variants: vec![
                            EnumVariant {
                                doc: None,
                                name: "Some".to_string(),
                                payload_types: vec![Type::Generic {
                                    name: "T".to_string(),
                                    params: vec![],
                                }],
                                named_fields: None,
                                span: Span {
                                    start: 0,
                                    end: 0,
                                    line: 0,
                                    column: 0,
                                },
                            },
                            EnumVariant {
                                doc: None,
                                name: "None".to_string(),
                                payload_types: vec![],
                                named_fields: None,
                                span: Span {
                                    start: 0,
                                    end: 0,
                                    line: 0,
                                    column: 0,
                                },
                            },
                        ],
                        span: Span {
                            start: 0,
                            end: 0,
                            line: 0,
                            column: 0,
                        },
                    };
                    let elem_type_clone = elem_type.clone();
                    self.generate_monomorphized_enum(
                        &option_decl,
                        std::slice::from_ref(&elem_type_clone),
                    );
                    self.generated_types.insert(option_type_name.clone(), true);

                    // Also add to generic_instantiations for consistency
                    self.generic_instantiations.insert(
                        option_type_name,
                        ("Option".to_string(), vec![elem_type_clone]),
                    );
                }
            }
        }

        // Generate extern function prototypes
        for extern_block in &program.extern_blocks {
            self.generate_extern_block(extern_block);
        }

        // Generate function prototypes (forward declarations) for ALL functions
        // This is required for single-file mode where functions may be called before definition
        let proto_insert_pos = self.output.len();
        for func in &program.functions {
            let param_list = self.format_ir_param_list_c(&func.params);
            if let Some(ret_ty) = func.return_type.as_ref() {
                let resolved = resolve_type_alias(ret_ty, &self.type_aliases);
                if matches!(resolved, Type::Fn { .. }) {
                    self.writeln(&format!(
                        "{};",
                        fn_type_to_c_function_header(&func.name, &param_list, &resolved)
                    ));
                    continue;
                }
            }
            let return_type = func
                .return_type
                .as_ref()
                .map(|ty| type_to_c_return_type(&resolve_type_alias(ty, &self.type_aliases)))
                .unwrap_or_else(|| "void".to_string());
            self.write(&format!("{} {}(", return_type, func.name));
            self.write(&param_list);
            self.writeln(");");
        }

        // Generate each function (populates spawn forward/definition buffers)
        for function in &program.functions {
            self.generate_function(function);
        }

        self.insert_spawn_fn_literal_forward_decls(proto_insert_pos);

        if !self.spawn_definitions.is_empty() || !self.fn_literal_definitions.is_empty() {
            self.writeln("");
            let mut trailing_defs = self.spawn_definitions.clone();
            trailing_defs.push_str(&self.fn_literal_definitions);
            self.write(&trailing_defs);
        }

        self.output.clone()
    }

    /// Generate C source file for a module (for multi-file mode)
    pub fn generate_module_source(
        &mut self,
        program: &IRProgram,
        symbol_prefix: &str,
        source_ion: &str,
        imports: &[String],
        header_stem: &str,
    ) -> String {
        self.output.clear();
        self.indent_level = 0;
        self.mangle_merged_module_calls = false;
        self.multi_file_module = Some(symbol_prefix.to_string());
        // Build enum map for variant index lookups
        self.enum_map.clear();
        for e in &program.enums {
            self.enum_map.insert(e.name.clone(), e.clone());
        }
        // Build struct map for generic struct lookups
        self.struct_map.clear();
        for s in &program.structs {
            self.struct_map.insert(s.name.clone(), s.clone());
        }
        self.generated_types.clear();
        self.spawn_counter = 0;
        self.spawn_forward_decls.clear();
        self.spawn_definitions.clear();
        self.fn_literal_forward_decls.clear();
        self.fn_literal_definitions.clear();
        self.generated_fn_literals.clear();

        self.extern_functions.clear();
        self.function_return_types.clear();
        self.function_param_types.clear();
        for extern_block in &program.extern_blocks {
            for ext_fn in &extern_block.functions {
                self.extern_functions.insert(
                    ext_fn.name.clone(),
                    ext_fn.params.iter().map(|p| p.ty.clone()).collect(),
                );
            }
        }
        for function in &program.functions {
            let c_name = format!("{}_{}", symbol_prefix, function.name);
            self.function_return_types
                .insert(c_name.clone(), function.return_type.clone());
            self.function_param_types.insert(
                c_name,
                function.params.iter().map(|p| p.ty.clone()).collect(),
            );
        }

        self.emit_generated_banner(source_ion);

        // Generate includes
        self.writeln("#include <stdio.h>");
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <string.h>");
        // Runtime header - use ion_runtime.h and rely on compiler include paths
        // The compiler will add -I. -I.. -Iruntime -I../runtime to find it
        self.writeln("#include \"ion_runtime.h\"");
        // Include the module's own header
        self.writeln(&format!("#include \"{}.h\"", header_stem));
        // Include headers for imported modules
        for import_name in imports {
            if import_name != header_stem {
                self.writeln(&format!("#include \"{}.h\"", import_name));
            }
        }
        self.writeln("");

        // Collect all generic type instantiations used in the program
        let mut generic_instantiations_map: std::collections::HashMap<String, (String, Vec<Type>)> =
            std::collections::HashMap::new();
        collect_generic_instantiations(program, &mut generic_instantiations_map);
        self.generic_instantiations = generic_instantiations_map.clone();

        self.emit_vec_slice_tuple_typedefs(program);

        // Emit struct type definitions first so functions can use them.
        for s in &program.structs {
            if s.generics.is_empty() {
                self.write(&format!("typedef struct {} {{\n", s.name));
                self.indent_level += 1;
                for field in &s.fields {
                    self.write_indent();
                    // Handle arrays specially: in struct fields, arrays must be declared as "type name[size];"
                    let field_decl = match &field.ty {
                        Type::Array { inner, size } => {
                            let base_type = self.type_to_c(inner);
                            format!("{} {}[{}]", base_type, field.name, size)
                        }
                        _ => {
                            format!("{} {}", self.type_to_c(&field.ty), field.name)
                        }
                    };
                    self.writeln(&format!("{};", field_decl));
                }
                self.indent_level -= 1;
                self.writeln(&format!("}} {};", s.name));
                self.writeln("");
            }
        }

        // Emit enum type definitions
        for e in &program.enums {
            if e.generics.is_empty() {
                self.generate_enum_type(e);
            }
        }

        // Generate monomorphized struct/enum types
        let mut instantiations_vec: Vec<_> = generic_instantiations_map.values().collect();
        instantiations_vec.sort_by_key(|(name, _)| name.clone());

        let mut struct_instantiations: Vec<(StructDecl, Vec<Type>)> = Vec::new();
        let mut enum_instantiations: Vec<(EnumDecl, Vec<Type>)> = Vec::new();

        for (base_name, params) in instantiations_vec {
            let base_name_clone = base_name.clone();
            let params_clone = params.clone();

            if let Some(decl) = self.struct_map.get(&base_name_clone) {
                struct_instantiations.push((decl.clone(), params_clone));
            } else if let Some(decl) = self.enum_map.get(&base_name_clone) {
                enum_instantiations.push((decl.clone(), params_clone));
            }
        }

        for (decl, params) in struct_instantiations {
            self.generate_monomorphized_struct(&decl, &params);
        }
        for (decl, params) in enum_instantiations {
            self.generate_monomorphized_enum(&decl, &params);
        }

        // Handle built-in generic enums like Option<T> that aren't in enum_map
        for (key, (base_name, params)) in &generic_instantiations_map {
            if base_name == "Option"
                && !self.enum_map.contains_key("Option")
                && !self.generated_types.contains_key(key)
            {
                // Create a synthetic EnumDecl for Option<T>
                let option_decl = EnumDecl {
                    doc: None,
                    pub_: false,
                    name: "Option".to_string(),
                    generics: vec!["T".to_string()],
                    variants: vec![
                        EnumVariant {
                            doc: None,
                            name: "Some".to_string(),
                            payload_types: vec![Type::Generic {
                                name: "T".to_string(),
                                params: vec![],
                            }],
                            named_fields: None,
                            span: Span {
                                start: 0,
                                end: 0,
                                line: 0,
                                column: 0,
                            },
                        },
                        EnumVariant {
                            doc: None,
                            name: "None".to_string(),
                            payload_types: vec![],
                            named_fields: None,
                            span: Span {
                                start: 0,
                                end: 0,
                                line: 0,
                                column: 0,
                            },
                        },
                    ],
                    span: Span {
                        start: 0,
                        end: 0,
                        line: 0,
                        column: 0,
                    },
                };
                self.generate_monomorphized_enum(&option_decl, params);
                self.generated_types.insert(key.clone(), true);
            }
        }

        // Collect and generate Vec type definitions
        let mut vec_types = std::collections::HashSet::new();
        collect_vec_types_impl(program, &mut vec_types);
        for vec_type_name in &vec_types {
            // Generate Option<T> for each Vec<T> since Vec::pop and Vec::get return Option<T>
            // Extract element type from Vec type name (e.g., "Vec_int" -> "int")
            if let Some(elem_type_str) = vec_type_name.strip_prefix("Vec_") {
                let option_type_name = format!("Option_{}", elem_type_str);
                if !self.generated_types.contains_key(&option_type_name) {
                    // Parse element type from string (e.g., "int" -> Type::Int)
                    let elem_type = match elem_type_str {
                        "int" => Type::Int,
                        "bool" => Type::Bool,
                        "f32" => Type::F32,
                        "f64" => Type::F64,
                        "i8" => Type::I8,
                        "i16" => Type::I16,
                        "i32" => Type::I32,
                        "i64" => Type::I64,
                        "u8" => Type::U8,
                        "u16" => Type::U16,
                        "u32" => Type::U32,
                        "u64" => Type::U64,
                        "uint" => Type::UInt,
                        _ => {
                            // Try to parse as a struct/enum type or skip complex types
                            // For now, skip complex types and only handle primitives
                            continue;
                        }
                    };

                    // Create a synthetic EnumDecl for Option<T>
                    let option_decl = EnumDecl {
                        doc: None,
                        pub_: false,
                        name: "Option".to_string(),
                        generics: vec!["T".to_string()],
                        variants: vec![
                            EnumVariant {
                                doc: None,
                                name: "Some".to_string(),
                                payload_types: vec![Type::Generic {
                                    name: "T".to_string(),
                                    params: vec![],
                                }],
                                named_fields: None,
                                span: Span {
                                    start: 0,
                                    end: 0,
                                    line: 0,
                                    column: 0,
                                },
                            },
                            EnumVariant {
                                doc: None,
                                name: "None".to_string(),
                                payload_types: vec![],
                                named_fields: None,
                                span: Span {
                                    start: 0,
                                    end: 0,
                                    line: 0,
                                    column: 0,
                                },
                            },
                        ],
                        span: Span {
                            start: 0,
                            end: 0,
                            line: 0,
                            column: 0,
                        },
                    };
                    let elem_type_clone = elem_type.clone();
                    self.generate_monomorphized_enum(
                        &option_decl,
                        std::slice::from_ref(&elem_type_clone),
                    );
                    self.generated_types.insert(option_type_name.clone(), true);

                    // Also add to generic_instantiations for consistency
                    self.generic_instantiations.insert(
                        option_type_name,
                        ("Option".to_string(), vec![elem_type_clone]),
                    );
                }
            }
        }

        // Generate extern function prototypes (declarations only, implementations come from headers)
        for extern_block in &program.extern_blocks {
            for ext_fn in &extern_block.functions {
                let return_type = ext_fn
                    .return_type
                    .as_ref()
                    .map(|t| self.type_to_c(t))
                    .unwrap_or_else(|| "void".to_string());
                self.write(&format!("{} {}(", return_type, ext_fn.name));

                // Generate parameters
                if ext_fn.params.is_empty() && !ext_fn.variadic {
                    self.write("void");
                } else {
                    for (i, param) in ext_fn.params.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        // Special handling for array types: C syntax is "int arr[3]" not "int[3] arr"
                        match &param.ty {
                            Type::Array { inner, size } => {
                                let base_type = self.type_to_c(inner);
                                self.write(&format!("{} {}[{}]", base_type, param.name, size));
                            }
                            Type::Fn { .. } => {
                                self.write(&fn_type_to_c_decl(&param.ty, &param.name));
                            }
                            _ => {
                                self.write(&format!(
                                    "{} {}",
                                    self.type_to_c(&param.ty),
                                    param.name
                                ));
                            }
                        }
                    }
                    if ext_fn.variadic {
                        if !ext_fn.params.is_empty() {
                            self.write(", ");
                        }
                        self.write("...");
                    }
                }

                self.writeln(");");
            }
        }
        self.writeln("");

        // Generate function implementations (both public and private)
        let impl_insert_pos = self.output.len();
        for function in &program.functions {
            self.generate_function(function);
        }

        self.insert_spawn_fn_literal_forward_decls(impl_insert_pos);

        if !self.spawn_definitions.is_empty() || !self.fn_literal_definitions.is_empty() {
            self.writeln("");
            let mut trailing_defs = self.spawn_definitions.clone();
            trailing_defs.push_str(&self.fn_literal_definitions);
            self.write(&trailing_defs);
        }

        self.multi_file_module = None;
        self.output.clone()
    }

    /// Generate C header file for a module (for multi-file mode)
    pub fn generate_module_header(
        &mut self,
        program: &Program,
        symbol_prefix: &str,
        header_stem: &str,
    ) -> String {
        self.output.clear();
        self.indent_level = 0;

        // Generate include guard
        let guard_name = format!("ION_{}_H", header_stem.to_uppercase().replace("-", "_"));
        self.writeln(&format!("#ifndef {}", guard_name));
        self.writeln(&format!("#define {}", guard_name));
        self.writeln("");

        // Include necessary headers
        self.writeln("#include <stdint.h>");
        self.writeln("#include \"ion_runtime.h\"");
        self.writeln("");

        // Generate public struct definitions
        for s in &program.structs {
            if s.pub_ && s.generics.is_empty() {
                self.write(&format!("typedef struct {} {{\n", s.name));
                self.indent_level += 1;
                for field in &s.fields {
                    self.write_indent();
                    // Handle arrays specially: in struct fields, arrays must be declared as "type name[size];"
                    let field_decl = match &field.ty {
                        Type::Array { inner, size } => {
                            let base_type = type_to_c_impl(inner);
                            format!("{} {}[{}]", base_type, field.name, size)
                        }
                        _ => {
                            format!("{} {}", type_to_c_impl(&field.ty), field.name)
                        }
                    };
                    self.writeln(&format!("{};", field_decl));
                }
                self.indent_level -= 1;
                self.writeln(&format!("}} {};", s.name));
                self.writeln("");
            }
        }

        // Generate public enum definitions
        for e in &program.enums {
            if e.pub_ && e.generics.is_empty() {
                self.generate_enum_type(e);
            }
        }

        // Generate public function prototypes
        for func in &program.functions {
            if func.pub_ {
                let return_type = func
                    .return_type
                    .as_ref()
                    .map(type_to_c_impl)
                    .unwrap_or_else(|| "void".to_string());
                let c_name = format!("{}_{}", symbol_prefix, func.name);
                self.write(&format!("{} {}(", return_type, c_name));
                if func.params.is_empty() {
                    self.write("void");
                } else {
                    for (i, param) in func.params.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        // Special handling for array types: C syntax is "int arr[3]" not "int[3] arr"
                        match &param.ty {
                            Type::Array { inner, size } => {
                                let base_type = type_to_c_impl(inner);
                                self.write(&format!("{} {}[{}]", base_type, param.name, size));
                            }
                            Type::Fn { .. } => {
                                self.write(&fn_type_to_c_decl(&param.ty, &param.name));
                            }
                            _ => {
                                self.write(&format!(
                                    "{} {}",
                                    type_to_c_impl(&param.ty),
                                    param.name
                                ));
                            }
                        }
                    }
                }
                self.writeln(");");
            }
        }

        // Close include guard
        self.writeln("");
        self.writeln(&format!("#endif // {}", guard_name));

        self.output.clone()
    }

    fn struct_decl_for_type(&self, ty: &Type) -> Option<(&StructDecl, HashMap<String, Type>)> {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        let (base_name, params) = match &resolved {
            Type::Struct(name) if self.struct_map.contains_key(name) => (name.as_str(), Vec::new()),
            Type::Generic { name, params } if self.struct_map.contains_key(name) => {
                (name.as_str(), params.clone())
            }
            _ => return None,
        };
        let decl = self.struct_map.get(base_name)?;
        let mut substitutions = HashMap::new();
        for (i, gen_name) in decl.generics.iter().enumerate() {
            if i < params.len() {
                substitutions.insert(gen_name.clone(), params[i].clone());
            }
        }
        Some((decl, substitutions))
    }

    fn enum_decl_for_type(&self, ty: &Type) -> Option<(&EnumDecl, HashMap<String, Type>)> {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        let (base_name, params) = match &resolved {
            Type::Enum(name) => (name.as_str(), Vec::new()),
            Type::Struct(name) if self.enum_map.contains_key(name) => (name.as_str(), Vec::new()),
            Type::Generic { name, params } if self.enum_map.contains_key(name) => {
                (name.as_str(), params.clone())
            }
            _ => return None,
        };
        let decl = self.enum_map.get(base_name)?;
        let mut substitutions = HashMap::new();
        for (i, gen_name) in decl.generics.iter().enumerate() {
            if i < params.len() {
                substitutions.insert(gen_name.clone(), params[i].clone());
            }
        }
        Some((decl, substitutions))
    }

    fn substitute_field_types(ty: &Type, substitutions: &HashMap<String, Type>) -> Type {
        let refs: HashMap<String, &Type> =
            substitutions.iter().map(|(k, v)| (k.clone(), v)).collect();
        substitute_generic_types(ty, &refs)
    }

    fn scope_begin(&mut self, defers: &[IREexpr]) {
        self.scope_stack.push(ScopeFrame {
            bindings: Vec::new(),
            defers: defers.to_vec(),
        });
    }

    fn scope_register_binding(&mut self, name: &str, ty: &Type) {
        if let Some(frame) = self.scope_stack.last_mut() {
            frame.bindings.push(ScopeBinding {
                name: name.to_string(),
                ty: ty.clone(),
                dropped: false,
                read: false,
            });
        }
    }

    fn scope_register_param_binding(&mut self, name: &str, ty: &Type) {
        self.scope_register_binding(name, ty);
        if Self::should_silence_unused_binding(name, ty) {
            self.emit_silence_unused_binding(name);
        }
    }

    fn scope_mark_binding_read(&mut self, name: &str) {
        for frame in self.scope_stack.iter_mut().rev() {
            if let Some(binding) = frame.bindings.iter_mut().find(|b| b.name == name) {
                binding.read = true;
                return;
            }
        }
    }

    fn scope_mark_moved(&mut self, name: &str) {
        for frame in self.scope_stack.iter_mut().rev() {
            if let Some(binding) = frame.bindings.iter_mut().find(|b| b.name == name) {
                binding.dropped = true;
                return;
            }
        }
    }

    fn lookup_binding_type(&self, name: &str) -> Option<Type> {
        for frame in self.scope_stack.iter().rev() {
            if let Some(binding) = frame.bindings.iter().find(|b| b.name == name) {
                return Some(binding.ty.clone());
            }
        }
        None
    }

    fn lookup_var_type(&self, name: &str) -> Option<Type> {
        self.lookup_binding_type(name)
            .or_else(|| self.current_function_params.get(name).cloned())
    }

    fn infer_irexpr_type(&self, expr: &IREexpr) -> Option<Type> {
        match expr {
            IREexpr::Var(name) => self.lookup_var_type(name),
            IREexpr::StringLit(_) => Some(Type::String),
            IREexpr::FieldAccess { base, field, .. } => {
                let base_ty = if let IREexpr::Var(name) = base.as_ref() {
                    self.lookup_var_type(name)?
                } else {
                    self.infer_irexpr_type(base)?
                };
                let struct_ty = match &base_ty {
                    Type::Ref { inner, .. } => inner.as_ref(),
                    other => other,
                };
                let (decl, substitutions) = self.struct_decl_for_type(struct_ty)?;
                let field_decl = decl.fields.iter().find(|f| f.name == *field)?;
                Some(Self::substitute_field_types(&field_decl.ty, &substitutions))
            }
            IREexpr::AddressOf { inner, mutable } => {
                self.infer_irexpr_type(inner).map(|inner_ty| Type::Ref {
                    inner: Box::new(inner_ty),
                    mutable: *mutable,
                })
            }
            IREexpr::Call { callee, .. } => match callee.as_str() {
                "String::new" | "String::from" => Some(Type::String),
                _ => None,
            },
            _ => None,
        }
    }

    /// C expression for `ion_vec_t*` from a `&Vec<T>`, `&mut Vec<T>`, or owned `Vec<T>` IR arg.
    pub(crate) fn vec_ion_ptr_expr(&self, arg: &IREexpr, vec_code: &str) -> String {
        let stripped = vec_code.strip_prefix('&').unwrap_or(vec_code);
        if matches!(arg, IREexpr::Var(_))
            && let Some(ty) = self.infer_irexpr_type(arg)
            && is_ref_to_vec(&ty)
        {
            return format!("(*{stripped})");
        }
        stripped.to_string()
    }

    pub(crate) fn vec_elem_type_from_arg(&self, arg: &IREexpr) -> Option<Type> {
        let inner_ty = match arg {
            IREexpr::AddressOf { inner, .. } => self.infer_irexpr_type(inner)?,
            _ => self.infer_irexpr_type(arg)?,
        };
        ref_to_vec_elem(&inner_ty).cloned()
    }

    pub(crate) fn resolve_vec_elem_c_type(
        &self,
        vec_arg: &IREexpr,
        return_type: Option<&Type>,
    ) -> String {
        if let Some(elem) = self.vec_elem_type_from_arg(vec_arg) {
            return self.type_to_c(&elem);
        }
        if let Some(Type::Generic { name, params }) = return_type
            && name == "Option"
            && params.len() == 1
        {
            return self.type_to_c(&params[0]);
        }
        self.generic_instantiations
            .iter()
            .find_map(|(_mono_name, (base, params))| {
                if base == "Vec" && params.len() == 1 {
                    Some(self.type_to_c(&params[0]))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| "int".to_string())
    }

    pub(crate) fn write_option_from_runtime_raw(
        &mut self,
        mono_name: &str,
        elem_c_type: &str,
        dest_var: &str,
        raw_expr: &IREexpr,
    ) {
        self.write(&format!("{mono_name} {dest_var};\n"));
        self.write_indent();
        self.write("{ void* _ion_raw = ");
        self.generate_expr(raw_expr);
        self.writeln(";");
        self.write_indent();
        self.writeln(&format!(
            "ion_option_from_raw(&{dest_var}, _ion_raw, sizeof({elem_c_type}), offsetof({mono_name}, data.variant_0.arg0)); }}"
        ));
        self.mark_moves_in_expr(raw_expr);
    }

    pub(crate) fn write_option_from_runtime_raw_stmt_expr(
        &mut self,
        mono_name: &str,
        elem_c_type: &str,
        raw_expr: &IREexpr,
    ) {
        self.write("({ ");
        self.write(&format!("{mono_name} _ion_opt; "));
        self.write("void* _ion_raw = ");
        self.generate_expr(raw_expr);
        self.write(&format!(
            "; ion_option_from_raw(&_ion_opt, _ion_raw, sizeof({elem_c_type}), offsetof({mono_name}, data.variant_0.arg0)); _ion_opt; }})"
        ));
        self.mark_moves_in_expr(raw_expr);
    }

    fn field_access_c_path(&self, expr: &IREexpr) -> Option<String> {
        match expr {
            IREexpr::Var(name) => Some(name.clone()),
            IREexpr::FieldAccess {
                base,
                field,
                is_pointer,
            } => {
                let base_path = self.field_access_c_path(base)?;
                Some(if *is_pointer {
                    format!("{base_path}->{field}")
                } else {
                    format!("{base_path}.{field}")
                })
            }
            _ => None,
        }
    }

    /// Clear a struct field after its owned value was moved out (e.g. `let v = s.items`).
    fn emit_struct_field_moved_out(&mut self, expr: &IREexpr) {
        let IREexpr::FieldAccess { .. } = expr else {
            return;
        };
        let Some(path) = self.field_access_c_path(expr) else {
            return;
        };
        let Some(ty) = self.infer_irexpr_type(expr) else {
            return;
        };
        if !self.needs_drop(&ty) {
            return;
        }
        if !self.pending_field_null_set.insert(path.clone()) {
            return;
        }
        self.pending_field_nulls.push(format!(
            "{} = {};",
            path,
            self.zero_value_for_scrutinee_payload(&ty)
        ));
    }

    fn flush_pending_field_nulls(&mut self) {
        let lines: Vec<String> = self.pending_field_nulls.drain(..).collect();
        self.pending_field_null_set.clear();
        for line in lines {
            self.write_indent();
            self.writeln(&line);
        }
    }

    fn is_string_compare_operand(&self, expr: &IREexpr) -> bool {
        match self.infer_irexpr_type(expr) {
            Some(Type::String) => true,
            Some(Type::Ref { inner, .. }) => matches!(*inner, Type::String),
            _ => false,
        }
    }

    fn emit_string_compare_operand(&mut self, expr: &IREexpr) {
        let needs_deref = match self.infer_irexpr_type(expr) {
            Some(Type::Ref { inner, .. }) => matches!(*inner, Type::String),
            _ => false,
        };
        if needs_deref {
            self.write("(*");
            self.generate_expr(expr);
            self.write(")");
        } else {
            self.generate_expr(expr);
        }
    }

    fn emit_binop_operand(&mut self, expr: &IREexpr) {
        let needs_deref = self
            .infer_irexpr_type(expr)
            .is_some_and(|ty| matches!(ty, Type::Ref { inner, .. } if matches!(*inner, Type::Int | Type::Bool | Type::F32 | Type::F64 | Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::UInt)));
        if needs_deref {
            self.write("(*");
            self.generate_expr(expr);
            self.write(")");
        } else {
            self.generate_expr(expr);
        }
    }

    fn generate_string_equality(
        &mut self,
        op: BinOp,
        left: &IREexpr,
        right: &IREexpr,
        wrap_parens: bool,
    ) {
        if wrap_parens {
            self.write("(");
        }
        if matches!(op, BinOp::Ne) {
            self.write("!");
        }
        self.write("ion_string_equals(");
        self.emit_string_compare_operand(left);
        self.write(", ");
        self.emit_string_compare_operand(right);
        self.write(")");
        if wrap_parens {
            self.write(")");
        }
    }

    fn slice_struct_name_for_elem(&self, elem_ty: &Type) -> String {
        format!(
            "ion_slice_{}",
            mangle_type_name(&self.type_to_c(elem_ty), &[])
        )
    }

    /// When init is `&arr` for `[T; N]` and expected is `&[]T`, return (array name, N, elem type).
    fn match_array_to_slice_coercion(
        &self,
        expected: &Type,
        init: &IREexpr,
    ) -> Option<(String, usize, Type)> {
        let expected = resolve_type_alias(expected, &self.type_aliases);
        let Type::Ref {
            inner: expected_inner,
            mutable: expected_mut,
        } = expected
        else {
            return None;
        };
        let Type::Slice { .. } = expected_inner.as_ref() else {
            return None;
        };
        let IREexpr::AddressOf {
            inner,
            mutable: init_mut,
        } = init
        else {
            return None;
        };
        if expected_mut != *init_mut {
            return None;
        }
        let IREexpr::Var(array_name) = inner.as_ref() else {
            return None;
        };
        let var_ty = self.lookup_var_type(array_name)?;
        let var_ty = resolve_type_alias(&var_ty, &self.type_aliases);
        let Type::Array {
            inner: array_elem,
            size,
        } = var_ty
        else {
            return None;
        };
        Some((array_name.clone(), size, (*array_elem).clone()))
    }

    fn emit_array_to_slice_ptr(&mut self, array_name: &str, size: usize, elem_ty: &Type) {
        let slice_type = self.slice_struct_name_for_elem(elem_ty);
        self.write("&(");
        self.write(&slice_type);
        self.write("){");
        self.write(array_name);
        self.write(", ");
        self.write(&size.to_string());
        self.write("}");
    }

    /// Mirror tc `check_expr_for_moves`: mark bindings consumed by this expression.
    fn mark_moves_in_expr(&mut self, expr: &IREexpr) {
        match expr {
            IREexpr::Var(name) => {
                if let Some(ty) = self.lookup_binding_type(name)
                    && self.needs_drop(&ty)
                {
                    self.scope_mark_moved(name);
                }
            }
            IREexpr::FieldAccess { .. } => {
                self.emit_struct_field_moved_out(expr);
            }
            IREexpr::StructLit { fields, .. } => {
                for field in fields {
                    self.mark_moves_in_expr(&field.value);
                }
            }
            IREexpr::TupleLit { elements, .. } => {
                for elem in elements {
                    self.mark_moves_in_expr(elem);
                }
            }
            IREexpr::EnumLit {
                args, named_fields, ..
            } => {
                for arg in args {
                    self.mark_moves_in_expr(arg);
                }
                if let Some(named_fields) = named_fields {
                    for (_, value) in named_fields {
                        self.mark_moves_in_expr(value);
                    }
                }
            }
            IREexpr::Send { value, .. } => self.mark_moves_in_expr(value),
            IREexpr::Call { args, .. } => {
                for arg in args {
                    self.mark_moves_in_expr(arg);
                }
            }
            IREexpr::ArrayLiteral {
                elements, repeat, ..
            } => {
                for elem in elements {
                    self.mark_moves_in_expr(elem);
                }
                if let Some((value, _)) = repeat {
                    self.mark_moves_in_expr(value);
                }
            }
            IREexpr::Cast { expr, .. } => self.mark_moves_in_expr(expr),
            IREexpr::Assign { value, .. } => self.mark_moves_in_expr(value),
            IREexpr::AssignIndex { value, .. } => self.mark_moves_in_expr(value),
            IREexpr::Match { expr, .. } => self.mark_moves_in_expr(expr),
            _ => {}
        }
    }

    fn scope_emit_exit(&mut self) {
        let Some(frame) = self.scope_stack.pop() else {
            return;
        };
        self.emit_frame_cleanup(&frame);
    }

    /// Emit cleanup for all active scopes (innermost first) without popping the stack.
    /// Used on `return` so sibling statements in the same function still codegen correctly.
    fn scope_emit_return_unwind(&mut self) {
        let frames: Vec<ScopeFrame> = self.scope_stack.iter().rev().cloned().collect();
        for frame in frames {
            self.emit_frame_cleanup(&frame);
        }
    }

    fn emit_frame_cleanup(&mut self, frame: &ScopeFrame) {
        for defer_expr in frame.defers.iter().rev() {
            self.write_indent();
            self.write("(void)(");
            self.generate_expr(defer_expr);
            self.writeln(");");
        }
        for binding in frame.bindings.iter().rev() {
            if !binding.dropped
                && !binding.read
                && !Self::should_silence_unused_binding(&binding.name, &binding.ty)
            {
                self.emit_silence_unused_binding(&binding.name);
            }
        }
        for binding in frame.bindings.iter().rev() {
            if !binding.dropped && self.needs_drop(&binding.ty) {
                self.emit_drop(&binding.name, &binding.ty);
            }
        }
    }

    /// Drop owned bindings in the current scope frame without popping it.
    /// Used when leaving a grouped `switch` arm early (guard match) before case-level cleanup.
    fn scope_emit_top_frame_drops(&mut self) {
        let Some(frame) = self.scope_stack.last().cloned() else {
            return;
        };
        self.emit_frame_cleanup(&frame);
        let to_drop: Vec<String> = frame
            .bindings
            .iter()
            .filter(|b| !b.dropped && self.needs_drop(&b.ty))
            .map(|b| b.name.clone())
            .collect();
        let Some(top) = self.scope_stack.last_mut() else {
            return;
        };
        for name in to_drop {
            if let Some(binding) = top.bindings.iter_mut().find(|b| b.name == name) {
                binding.dropped = true;
            }
        }
    }

    /// Assign `ret_val` (when needed), run scope unwind, and jump to the function epilogue.
    fn emit_function_return(&mut self, ret: &crate::ir::IRReturn) {
        self.write_indent();
        if let Some(ref value) = ret.value {
            let is_array_return = if let Some(ref ret_ty) = self.current_return_type {
                matches!(ret_ty, Type::Array { .. })
            } else {
                false
            };

            if is_array_return {
                if let Some(Type::Array { inner, size }) = self.current_return_type.as_ref() {
                    let base_type = self.type_to_c(inner);
                    self.writeln(&format!("static {} _ret_array[{}] = ", base_type, size));
                    self.write_indent();
                    self.write("    ");
                    self.generate_expr(value);
                    self.writeln(";");
                    self.write_indent();
                    self.writeln("ret_val = _ret_array;");
                }
            } else {
                let return_ty = self.current_return_type.clone();
                if let IREexpr::Match {
                    expr: match_expr,
                    enum_type,
                    arms,
                } = value
                {
                    if let Some(ref ty) = return_ty {
                        self.generate_match_block(
                            match_expr,
                            enum_type,
                            arms,
                            Some(("ret_val", ty)),
                        );
                    } else {
                        self.generate_match_block(match_expr, enum_type, arms, None);
                    }
                } else {
                    self.write("ret_val = ");

                    let mut needs_memcpy = false;
                    let mut array_field_name = String::new();
                    let mut array_var_name = String::new();

                    if let IREexpr::StructLit { type_name, fields } = value
                        && let Some(struct_decl) = self.struct_map.get(type_name)
                    {
                        for field in fields.iter() {
                            if let IREexpr::Var(ref var_name) = field.value
                                && let Some(field_decl) =
                                    struct_decl.fields.iter().find(|f| f.name == field.name)
                                && let Type::Array { .. } = field_decl.ty
                            {
                                needs_memcpy = true;
                                array_field_name = field.name.clone();
                                array_var_name = var_name.clone();
                                break;
                            }
                        }
                    }

                    self.generate_expr_with_type(value, return_ty.as_ref());
                    self.writeln(";");

                    if needs_memcpy {
                        self.write_indent();
                        self.writeln(&format!(
                            "memcpy(&ret_val.{}, &{}, sizeof(ret_val.{}));",
                            array_field_name, array_var_name, array_field_name
                        ));
                    }
                }
            }
        }
        if let Some(value) = &ret.value {
            self.mark_moves_in_expr(value);
        }
        self.flush_pending_field_nulls();
        self.scope_emit_return_unwind();
        self.write_indent();
        self.writeln(&format!("goto {};", self.epilogue_label));
    }

    fn format_ir_param_list_c(&self, params: &[IRParam]) -> String {
        if params.is_empty() {
            return "void".to_string();
        }
        params
            .iter()
            .map(|param| match &param.ty {
                Type::Array { inner, size } => {
                    format!("{} {}[{}]", self.type_to_c(inner), param.name, size)
                }
                Type::Fn { .. } => fn_type_to_c_decl(&param.ty, &param.name),
                _ => format!("{} {}", self.type_to_c(&param.ty), param.name),
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn generate_function(&mut self, function: &IRFunction) {
        // Store current return type for use in return statements
        self.current_return_type = function.return_type.clone();
        self.scope_stack.clear();
        self.epilogue_label = "epilogue".to_string();
        self.pending_field_nulls.clear();
        self.pending_field_null_set.clear();

        // Store parameter types for field access resolution
        self.current_function_params.clear();
        for param in &function.params {
            self.current_function_params
                .insert(param.name.clone(), param.ty.clone());
        }

        let c_name = self.module_c_symbol(&function.name);
        let param_list = self.format_ir_param_list_c(&function.params);

        if let Some(ret_ty) = function.return_type.as_ref() {
            let resolved = resolve_type_alias(ret_ty, &self.type_aliases);
            if matches!(resolved, Type::Fn { .. }) {
                self.write(&fn_type_to_c_function_header(
                    &c_name,
                    &param_list,
                    &resolved,
                ));
                self.writeln(" {");
            } else {
                let return_type = match ret_ty {
                    Type::Array { inner, .. } => format!("{}*", self.type_to_c(inner)),
                    _ => self.type_to_c(ret_ty),
                };
                self.write(&format!("{} {}(", return_type, c_name));
                self.write(&param_list);
                self.writeln(") {");
            }
        } else {
            self.write(&format!("void {}(", c_name));
            self.write(&param_list);
            self.writeln(") {");
        }

        self.indent_level += 1;

        // Synthetic return variable for functions with a return type.
        if let Some(ref ty) = function.return_type {
            self.write_indent();
            let resolved_ret = resolve_type_alias(ty, &self.type_aliases);
            self.writeln(&format_ret_val_decl(&ret_val_decl(&resolved_ret)));
        }

        self.scope_begin(&[]);
        for param in &function.params {
            self.scope_register_param_binding(&param.name, &param.ty);
        }

        // Generate function body (blocks)
        for block in &function.blocks {
            self.generate_block(block);
        }

        // Check if the last statement in the last block is a return
        // If not, and the function has a return type, add fallthrough goto
        let last_stmt_is_return = function
            .blocks
            .last()
            .and_then(|block| block.statements.last())
            .map(|stmt| matches!(stmt, IRStmt::Return(_)))
            .unwrap_or(false);

        // If control reaches the end without an explicit return, fall through to
        // epilogue (default-initialized ret_val for value-returning functions).
        if !last_stmt_is_return {
            self.scope_emit_return_unwind();
            self.write_indent();
            self.writeln("goto epilogue;");
        }

        // Epilogue: return (scope cleanup already ran on `return` or fallthrough above).
        self.writeln("epilogue:");
        self.indent_level += 1;

        if function.return_type.is_some() {
            self.write_indent();
            self.writeln("return ret_val;");
        } else {
            self.write_indent();
            self.writeln("return;");
        }

        self.indent_level -= 1;

        self.indent_level -= 1;
        self.writeln("}");
        self.writeln("");
    }

    fn generate_fn_literal(&mut self, lit: &crate::ir::IRFnLiteral) {
        if self.generated_fn_literals.contains(&lit.symbol) {
            return;
        }
        self.generated_fn_literals.insert(lit.symbol.clone());

        let return_type_c = lit
            .return_type
            .as_ref()
            .map(|t| match t {
                Type::Array { inner, .. } => format!("{}*", self.type_to_c(inner)),
                _ => self.type_to_c(t),
            })
            .unwrap_or_else(|| "void".to_string());

        self.fn_literal_forward_decls
            .push_str(&format!("static {} {}(", return_type_c, lit.symbol));
        if lit.params.is_empty() {
            self.fn_literal_forward_decls.push_str("void");
        } else {
            for (i, param) in lit.params.iter().enumerate() {
                if i > 0 {
                    self.fn_literal_forward_decls.push_str(", ");
                }
                match &param.ty {
                    Type::Array { inner, size } => {
                        let base_type = self.type_to_c(inner);
                        self.fn_literal_forward_decls
                            .push_str(&format!("{} {}[{}]", base_type, param.name, size));
                    }
                    Type::Fn { .. } => {
                        self.fn_literal_forward_decls
                            .push_str(&fn_type_to_c_decl(&param.ty, &param.name));
                    }
                    _ => {
                        self.fn_literal_forward_decls.push_str(&format!(
                            "{} {}",
                            self.type_to_c(&param.ty),
                            param.name
                        ));
                    }
                }
            }
        }
        self.fn_literal_forward_decls.push_str(");\n");

        let epilogue = format!("{}_epilogue", lit.symbol);
        let mut def = String::new();
        def.push_str(&format!("static {} {}(", return_type_c, lit.symbol));
        if lit.params.is_empty() {
            def.push_str("void");
        } else {
            for (i, param) in lit.params.iter().enumerate() {
                if i > 0 {
                    def.push_str(", ");
                }
                match &param.ty {
                    Type::Array { inner, size } => {
                        let base_type = self.type_to_c(inner);
                        def.push_str(&format!("{} {}[{}]", base_type, param.name, size));
                    }
                    Type::Fn { .. } => {
                        def.push_str(&fn_type_to_c_decl(&param.ty, &param.name));
                    }
                    _ => {
                        def.push_str(&format!("{} {}", self.type_to_c(&param.ty), param.name));
                    }
                }
            }
        }
        def.push_str(") {\n");

        if let Some(ty) = &lit.return_type {
            let resolved = resolve_type_alias(ty, &self.type_aliases);
            def.push_str(&format!(
                "    {}\n",
                format_ret_val_decl(&ret_val_decl(&resolved))
            ));
        }

        let saved_output = std::mem::take(&mut self.output);
        let saved_indent = self.indent_level;
        let saved_scope = std::mem::take(&mut self.scope_stack);
        let saved_epilogue = self.epilogue_label.clone();
        let saved_params = self.current_function_params.clone();
        let saved_return = self.current_return_type.clone();

        self.indent_level = 1;
        self.scope_stack.clear();
        self.epilogue_label = epilogue.clone();
        self.current_return_type = lit.return_type.clone();
        self.current_function_params.clear();
        for param in &lit.params {
            self.current_function_params
                .insert(param.name.clone(), param.ty.clone());
        }

        self.scope_begin(&[]);
        for param in &lit.params {
            self.scope_register_param_binding(&param.name, &param.ty);
        }
        self.generate_block(&lit.body);
        self.scope_emit_exit();

        let last_stmt_is_return = lit
            .body
            .statements
            .last()
            .map(|stmt| matches!(stmt, IRStmt::Return(_)))
            .unwrap_or(false);
        if lit.return_type.is_some() && !last_stmt_is_return {
            self.write_indent();
            self.writeln(&format!("goto {};", epilogue));
        }

        let body_code = std::mem::take(&mut self.output);
        def.push_str(&body_code);
        def.push_str(&format!("{}:\n", epilogue));
        if lit.return_type.is_some() {
            def.push_str("    return ret_val;\n");
        }
        def.push_str("}\n\n");

        self.output = saved_output;
        self.indent_level = saved_indent;
        self.scope_stack = saved_scope;
        self.epilogue_label = saved_epilogue;
        self.current_function_params = saved_params;
        self.current_return_type = saved_return;

        self.fn_literal_definitions.push_str(&def);
    }

    fn generate_spawn(&mut self, spawn: &IRSpawn) {
        let spawn_id = self.spawn_counter;
        self.spawn_counter += 1;

        let ctx_name = format!("ion_spawn_ctx_{}", spawn_id);
        let entry_name = format!("ion_spawn_entry_{}", spawn_id);
        let spawn_epilogue = format!("spawn_{}_epilogue", spawn_id);

        self.spawn_forward_decls
            .push_str(&format!("static void* {}(void* arg);\n", entry_name));

        if !spawn.captures.is_empty() {
            self.spawn_forward_decls.push_str("typedef struct {\n");
            for (name, ty) in &spawn.captures {
                self.spawn_forward_decls.push_str(&format!(
                    "    {} {};\n",
                    self.type_to_c(ty),
                    name
                ));
            }
            self.spawn_forward_decls
                .push_str(&format!("}} {};\n", ctx_name));
        }

        let mut def = String::new();
        def.push_str(&format!("static void* {}(void* arg) {{\n", entry_name));
        if !spawn.captures.is_empty() {
            def.push_str(&format!("    {}* ctx = ({}*)arg;\n", ctx_name, ctx_name));
            def.push_str("    if (!ctx) { ion_panic(\"spawn null context\"); }\n");
            for (name, ty) in &spawn.captures {
                def.push_str(&format!(
                    "    {} {} = ctx->{};\n",
                    self.type_to_c(ty),
                    name,
                    name
                ));
            }
            def.push_str("    free(ctx);\n");
        } else {
            def.push_str("    (void)arg;\n");
        }

        let saved_output = std::mem::take(&mut self.output);
        let saved_indent = self.indent_level;
        let saved_scope = std::mem::take(&mut self.scope_stack);
        let saved_epilogue = self.epilogue_label.clone();
        self.indent_level = 1;
        self.scope_stack.clear();
        self.epilogue_label = spawn_epilogue.clone();

        self.scope_begin(&[]);
        for (name, ty) in &spawn.captures {
            self.scope_register_param_binding(name, ty);
        }
        self.generate_block(&spawn.body);
        self.scope_emit_exit();
        self.write_indent();
        self.writeln(&format!("goto {};", spawn_epilogue));

        let body_code = std::mem::take(&mut self.output);
        self.output = saved_output;
        self.indent_level = saved_indent;
        self.scope_stack = saved_scope;
        self.epilogue_label = saved_epilogue;

        def.push_str(&body_code);
        def.push_str(&format!("{}:\n", spawn_epilogue));
        def.push_str("    return NULL;\n");
        def.push_str("}\n\n");
        self.spawn_definitions.push_str(&def);

        self.write_indent();
        self.writeln("{");
        self.indent_level += 1;
        self.write_indent();
        if spawn.captures.is_empty() {
            self.writeln(&format!(
                "if (ion_spawn({}, NULL) != 0) {{ ion_panic(\"spawn failed\"); }}",
                entry_name
            ));
        } else {
            self.writeln(&format!(
                "{}* ctx = ({}*)malloc(sizeof({}));",
                ctx_name, ctx_name, ctx_name
            ));
            self.write_indent();
            self.writeln("if (!ctx) { ion_panic(\"spawn allocation failed\"); }");
            for (name, ty) in &spawn.captures {
                self.write_indent();
                self.writeln(&format!("ctx->{} = {};", name, name));
                self.write_indent();
                self.writeln(&format!("{} = {};", name, self.zero_value_for_type(ty)));
                self.scope_mark_moved(name);
            }
            self.write_indent();
            self.writeln(&format!(
                "if (ion_spawn({}, ctx) != 0) {{ free(ctx); ion_panic(\"spawn failed\"); }}",
                entry_name
            ));
        }
        self.indent_level -= 1;
        self.write_indent();
        self.writeln("}");
    }

    fn generate_block(&mut self, block: &IRBlock) {
        let depth_at_entry = self.scope_stack.len();
        self.scope_begin(&block.defers);
        let mut i = 0;
        while i < block.statements.len() {
            // Check for consecutive channel tuple destructuring
            if let (IRStmt::Let(let1), Some(IRStmt::Let(let2))) =
                (&block.statements[i], block.statements.get(i + 1))
                && let Some(IREexpr::Call {
                    callee: callee1,
                    return_type: return_type1,
                    tuple_destructure_index: idx1,
                    ..
                }) = &let1.init
                && let Some(IREexpr::Call {
                    callee: callee2,
                    return_type: return_type2,
                    tuple_destructure_index: idx2,
                    ..
                }) = &let2.init
                && callee1 == "channel"
                && callee2 == "channel"
                && *idx1 == Some(0)
                && *idx2 == Some(1)
                && return_type1.is_some()
                && return_type2.is_some()
            {
                // Generate both declarations and single channel_new call
                if let Some(Type::Tuple { elements }) = return_type1
                    && elements.len() == 2
                    && let Type::Sender { elem_type } = &elements[0]
                {
                    let elem_c_type = self.type_to_c(elem_type);
                    let elem_size = format!("sizeof({})", elem_c_type);

                    // Generate both variable declarations
                    self.write_indent();
                    self.write(&format!("{} {}", self.type_to_c(&let1.ty), let1.name));
                    self.writeln(";");
                    self.write_indent();
                    self.write(&format!("{} {}", self.type_to_c(&let2.ty), let2.name));
                    self.writeln(";");

                    // Generate channel_new call
                    self.write_indent();
                    self.write("ion_channel_new(");
                    self.write(&elem_size);
                    self.write(", 1, &");
                    self.write(&let1.name);
                    self.write(", &");
                    self.write(&let2.name);
                    self.writeln(");");

                    self.scope_register_binding(&let1.name, &let1.ty);
                    self.scope_register_binding(&let2.name, &let2.ty);

                    // Skip both statements
                    i += 2;
                    continue;
                }
            }

            // Normal statement generation
            self.generate_stmt(&block.statements[i]);
            i += 1;
        }
        let ends_with_return = matches!(block.statements.last(), Some(IRStmt::Return(_)));
        if ends_with_return {
            while self.scope_stack.len() > depth_at_entry {
                self.scope_stack.pop();
            }
        } else {
            self.scope_emit_exit();
        }
    }

    fn generate_stmt(&mut self, stmt: &IRStmt) {
        match stmt {
            IRStmt::Let(let_stmt) => {
                if let Some(ref init) = let_stmt.init
                    && let Some((array_name, size, elem_ty)) =
                        self.match_array_to_slice_coercion(&let_stmt.ty, init)
                {
                    let slice_type = self.slice_struct_name_for_elem(&elem_ty);
                    let temp = format!("__ion_arr_slice_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;
                    self.write_indent();
                    self.writeln(&format!(
                        "{} {} = {{ {}, {} }};",
                        slice_type, temp, array_name, size
                    ));
                    self.write_indent();
                    self.write(&format!(
                        "{} {} = &{};",
                        self.type_to_c(&let_stmt.ty),
                        let_stmt.name,
                        temp
                    ));
                    self.writeln(";");
                    self.scope_register_binding(&let_stmt.name, &let_stmt.ty);
                    self.mark_moves_in_expr(init);
                    self.flush_pending_field_nulls();
                    return;
                }
                self.write_indent();
                // Special handling: if initialized from function call that returns array,
                // declare as pointer (C doesn't allow returning arrays)
                // Otherwise, if type is array, declare as array
                // Check if this is a function call returning an array (regardless of let_stmt.ty)
                let call_returns_array_type = if let Some(IREexpr::Call {
                    callee,
                    return_type,
                    ..
                }) = let_stmt.init.as_ref()
                {
                    // First check if return_type is set in the Call expression
                    let from_call = return_type.as_ref().and_then(|rt| {
                        if let Type::Array { inner, .. } = rt {
                            Some(self.type_to_c(inner))
                        } else {
                            None
                        }
                    });
                    // If not set, look up the function's return type
                    if from_call.is_none() {
                        self.function_return_types
                            .get(callee)
                            .and_then(|ret_ty_opt| {
                                ret_ty_opt.as_ref().and_then(|rt| {
                                    if let Type::Array { inner, .. } = rt {
                                        Some(self.type_to_c(inner))
                                    } else {
                                        None
                                    }
                                })
                            })
                    } else {
                        from_call
                    }
                } else {
                    None
                };

                if let Some(base_type) = call_returns_array_type {
                    // Function call returning array - declare as pointer
                    self.write(&format!("{}* {}", base_type, let_stmt.name));
                } else {
                    // Special handling for array types: C syntax is "int arr[3]" not "int[3] arr"
                    match &let_stmt.ty {
                        Type::Array { inner, size } => {
                            let base_type = self.type_to_c(inner);
                            self.write(&format!("{} {}[{}]", base_type, let_stmt.name, size));
                        }
                        Type::Fn { .. } => {
                            self.write(&fn_type_to_c_decl(&let_stmt.ty, &let_stmt.name));
                        }
                        _ => {
                            self.write(&format!(
                                "{} {}",
                                self.type_to_c(&let_stmt.ty),
                                let_stmt.name
                            ));
                        }
                    }
                }

                if let Some(ref init) = let_stmt.init {
                    // Special handling for channel() tuple destructuring
                    if let IREexpr::Call {
                        callee,
                        args: _,
                        return_type,
                        tuple_destructure_index,
                    } = init
                        && callee == "channel"
                        && tuple_destructure_index.is_some()
                    {
                        // For tuple destructuring, we need to handle this specially
                        // Index 0 = sender, Index 1 = receiver
                        if *tuple_destructure_index == Some(0) {
                            // First variable (sender) - generate the full channel_new call
                            // Extract element type from return_type
                            if let Some(Type::Tuple { elements }) = return_type
                                && elements.len() == 2
                                && let Type::Sender { elem_type } = &elements[0]
                            {
                                let elem_c_type = self.type_to_c(elem_type);
                                let elem_size = format!("sizeof({})", elem_c_type);

                                // Generate temporary receiver variable and channel_new call
                                let temp_rx_name = format!("_channel_rx_{}", let_stmt.name);
                                self.writeln("");
                                self.write_indent();
                                self.write(&format!(
                                    "{} {};",
                                    self.type_to_c(&elements[1]),
                                    temp_rx_name
                                ));
                                self.writeln("");
                                self.write_indent();
                                self.write(" = {0};");
                                self.writeln("");
                                self.write_indent();
                                self.write("ion_channel_new(");
                                self.write(&elem_size);
                                self.write(", 1, &");
                                self.write(&let_stmt.name);
                                self.write(", &");
                                self.write(&temp_rx_name);
                                self.write(");");
                                self.writeln("");
                                // Store temp receiver name for next statement
                                // We'll use a simple naming convention: _channel_rx_<sender_name>
                                return; // Skip normal initialization
                            }
                        } else if *tuple_destructure_index == Some(1) {
                            // Second variable (receiver) - copy from temporary
                            // The temp variable name is _channel_rx_<previous_variable>
                            // We need to find the previous let statement...
                            // Actually, simpler: use a fixed naming pattern based on this variable name
                            // But we don't know the sender name...

                            // Let's use a different approach: generate the assignment from temp
                            // The temp name should be _channel_rx_<something>
                            // For now, let's assume it's _channel_rx_temp
                            self.write(" = _channel_rx_temp;");
                            self.writeln("");
                            return; // Skip normal initialization
                        }
                    }

                    if let IREexpr::Match {
                        expr: match_expr,
                        enum_type,
                        arms,
                    } = init
                    {
                        self.writeln(";");
                        self.generate_match_block(
                            match_expr,
                            enum_type,
                            arms,
                            Some((&let_stmt.name, &let_stmt.ty)),
                        );
                        self.scope_register_binding(&let_stmt.name, &let_stmt.ty);
                        self.mark_moves_in_expr(match_expr);
                        self.flush_pending_field_nulls();
                        return;
                    }

                    self.write(" = ");
                    // Special handling: if assigning String type from string literal, convert it
                    if matches!(let_stmt.ty, Type::String) {
                        if let IREexpr::StringLit(value) = init {
                            // Convert string literal to heap-allocated string
                            // Escape special characters for C string literal
                            let escaped = value
                                .replace('\\', "\\\\")
                                .replace('"', "\\\"")
                                .replace('\n', "\\n")
                                .replace('\r', "\\r")
                                .replace('\t', "\\t")
                                .replace('\0', "\\0");
                            self.write("ion_string_from_literal(");
                            self.write(&format!("\"{}\", ", escaped));
                            self.write(&format!("{}", value.len()));
                            self.write(")");
                        } else {
                            self.generate_expr_with_type(init, Some(&let_stmt.ty));
                        }
                    } else if let Type::Generic { name, params } = &let_stmt.ty {
                        // Special handling for Option<T> from Vec::get/Vec::pop
                        // These return void* that need to be cast to Option<T>*
                        if name == "Option" && params.len() == 1 {
                            if let IREexpr::Call {
                                callee,
                                return_type,
                                ..
                            } = init
                            {
                                let needs_cast = callee == "Vec::pop"
                                    || callee == "Vec::get"
                                    || callee == "METHOD::pop"
                                    || callee == "METHOD::get"
                                    || callee.starts_with("ion_vec_pop")
                                    || callee.starts_with("ion_vec_get")
                                    || (return_type.is_some()
                                        && matches!(return_type.as_ref().unwrap(), Type::Generic { name: n, params } if n == "Option" && params.len() == 1 && !matches!(&params[0], Type::Ref { .. })));

                                if needs_cast {
                                    let mono_name = mangle_type_name("Option", params);
                                    let elem_c_type = self.type_to_c(&params[0]);
                                    self.write_option_from_runtime_raw_stmt_expr(
                                        &mono_name,
                                        &elem_c_type,
                                        init,
                                    );
                                } else {
                                    self.generate_expr_with_type(init, Some(&let_stmt.ty));
                                }
                            } else {
                                self.generate_expr_with_type(init, Some(&let_stmt.ty));
                            }
                        } else {
                            // Pass type context so enum/struct literals can use monomorphized names
                            self.generate_expr_with_type(init, Some(&let_stmt.ty));
                        }
                    } else if matches!(let_stmt.ty, Type::Int) {
                        // Special handling: if declared type is Int but init is a variable,
                        // it might actually be a Vec pointer (type inference limitation).
                        // Check if the init expression looks like it should be cast.
                        if let IREexpr::Var(_) = init {
                            // For variables, generate as-is - the type might be wrong but
                            // we'll let the C compiler handle it or fix in a later pass
                            self.generate_expr_with_type(init, Some(&let_stmt.ty));
                        } else {
                            self.generate_expr_with_type(init, Some(&let_stmt.ty));
                        }
                    } else {
                        // Pass type context so enum/struct literals can use monomorphized names
                        self.generate_expr_with_type(init, Some(&let_stmt.ty));
                    }
                } else if self.needs_drop(&let_stmt.ty) {
                    self.write(" = 0");
                }

                self.writeln(";");
                self.scope_register_binding(&let_stmt.name, &let_stmt.ty);
                if Self::should_silence_unused_binding(&let_stmt.name, &let_stmt.ty) {
                    self.emit_silence_unused_binding(&let_stmt.name);
                }
                if let Some(init) = &let_stmt.init {
                    self.mark_moves_in_expr(init);
                }
                self.flush_pending_field_nulls();
            }
            IRStmt::Return(ret) => {
                self.emit_function_return(ret);
            }
            IRStmt::Break => {
                self.write_indent();
                self.writeln("break;");
            }
            IRStmt::Continue => {
                self.write_indent();
                if let Some(ref label) = self.loop_continue_label {
                    self.writeln(&format!("goto {};", label));
                } else {
                    self.writeln("continue;");
                }
            }
            IRStmt::Expr(expr) => {
                // Special handling: match expressions used as statements should be blocks, not statement expressions
                match expr {
                    IREexpr::Match {
                        expr: match_expr,
                        enum_type,
                        arms,
                    } => {
                        self.generate_match_block(match_expr, enum_type, arms, None);
                        self.mark_moves_in_expr(match_expr);
                        self.flush_pending_field_nulls();
                    }
                    IREexpr::Send {
                        channel,
                        value,
                        value_type,
                    } => {
                        // Send is used as a statement - generate it as a block to handle temp variables
                        // This avoids statement expression syntax issues with comma operator
                        self.write_indent();
                        self.write("{ ");
                        let needs_temp = !is_send_value_lvalue(value);
                        if needs_temp {
                            self.write(&format!("{} _send_val = ", self.type_to_c(value_type)));
                            self.generate_expr(value);
                            self.write("; ");
                        }
                        self.write("ion_channel_send(");
                        if let IREexpr::AddressOf { inner, mutable: _ } = channel.as_ref() {
                            self.write("&");
                            self.generate_expr(inner);
                        } else {
                            self.write("&");
                            self.generate_expr(channel);
                        }
                        self.write(", ");
                        if needs_temp {
                            self.write("&_send_val");
                        } else {
                            self.write("&");
                            self.generate_expr(value);
                        }
                        self.write("); }");
                        self.writeln("");
                        self.mark_moves_in_expr(value.as_ref());
                        self.flush_pending_field_nulls();
                    }
                    _ => {
                        self.write_indent();
                        self.generate_expr(expr);
                        self.writeln(";");
                        self.mark_moves_in_expr(expr);
                        self.flush_pending_field_nulls();
                    }
                }
            }
            IRStmt::Defer(expr) => {
                self.mark_moves_in_expr(expr);
            }
            IRStmt::Spawn(spawn) => {
                self.generate_spawn(spawn);
            }
            IRStmt::If(ir_if) => {
                // Generate: if (cond) { ... } else { ... }
                self.write_indent();
                self.write("if (");
                self.generate_expr_conditional(&ir_if.cond);
                self.writeln(") {");
                self.indent_level += 1;
                self.generate_block(&ir_if.then_block);
                self.indent_level -= 1;
                self.write_indent();
                if let Some(ref else_block) = ir_if.else_block {
                    self.writeln("} else {");
                    self.indent_level += 1;
                    self.generate_block(else_block);
                    self.indent_level -= 1;
                    self.write_indent();
                    self.writeln("}");
                } else {
                    self.writeln("}");
                }
            }
            IRStmt::While(ir_while) => {
                self.write_indent();
                self.write("while (");
                self.generate_expr_conditional(&ir_while.cond);
                self.writeln(") {");
                self.indent_level += 1;
                let prev_continue_label = self.loop_continue_label.take();
                self.loop_continue_label = ir_while.continue_label.clone();
                self.generate_block(&ir_while.body);
                if let Some(ref step) = ir_while.step {
                    if let Some(ref label) = ir_while.continue_label {
                        self.write_indent();
                        self.writeln(&format!("goto {};", label));
                        self.write_indent();
                        self.writeln(&format!("{}:", label));
                    }
                    self.generate_block(step);
                }
                self.loop_continue_label = prev_continue_label;
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
            }
            IRStmt::UnsafeBlock(unsafe_block) => {
                // Unsafe blocks lower to regular C blocks (no special syntax)
                // Track unsafe context to disable bounds checking
                let prev_unsafe = self.in_unsafe_block;
                self.in_unsafe_block = true;

                self.write_indent();
                self.writeln("{");
                self.indent_level += 1;
                self.generate_block(&unsafe_block.body);
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");

                self.in_unsafe_block = prev_unsafe;
            }
        }
    }

    fn generate_expr(&mut self, expr: &IREexpr) {
        self.generate_expr_with_type(expr, None);
    }

    fn generate_expr_conditional(&mut self, expr: &IREexpr) {
        // Generate expression for conditional context (if/while) without extra parentheses
        match expr {
            IREexpr::BinOp { op, left, right } => {
                if matches!(op, BinOp::Eq | BinOp::Ne)
                    && self.is_string_compare_operand(left)
                    && self.is_string_compare_operand(right)
                {
                    self.generate_string_equality(*op, left, right, false);
                } else {
                    // For conditionals, don't add extra parentheses around simple comparisons
                    self.generate_expr(left);
                    self.write(&format!(" {} ", self.op_to_c(*op)));
                    self.generate_expr(right);
                }
            }
            IREexpr::UnOp { op, operand } => match op {
                UnOp::Not => {
                    self.write("!");
                    self.generate_expr(operand);
                }
                UnOp::Neg => {
                    if let IREexpr::Lit(v) = operand.as_ref() {
                        if *v == 2147483647i64 + 1 {
                            self.write(&crate::cgen::types::c_int_limit(&Type::Int, false));
                        } else {
                            self.write("-");
                            self.generate_expr(operand);
                        }
                    } else {
                        self.write("-");
                        self.generate_expr(operand);
                    }
                }
            },
            _ => {
                // For other expressions, generate normally
                self.generate_expr(expr);
            }
        }
    }

    fn generate_expr_with_type(&mut self, expr: &IREexpr, type_context: Option<&Type>) {
        match expr {
            IREexpr::Lit(value) => {
                self.write(&value.to_string());
            }
            IREexpr::IntLimit { ty, max } => {
                self.write(&crate::cgen::types::c_int_limit(ty, *max));
            }
            IREexpr::BoolLiteral(value) => {
                // Boolean literals: true -> 1, false -> 0
                self.write(if *value { "1" } else { "0" });
            }
            IREexpr::FloatLiteral(value) => {
                // Float literals: output as-is (C will handle the format)
                self.write(&value.to_string());
            }
            IREexpr::Var(name) => {
                self.scope_mark_binding_read(name);
                self.write(name);
            }
            IREexpr::AddressOf { inner, mutable: _ } => {
                // Generate address-of: &x
                self.write("&");
                self.generate_expr(inner);
            }
            IREexpr::BinOp { op, left, right } => {
                if matches!(op, BinOp::Eq | BinOp::Ne)
                    && self.is_string_compare_operand(left)
                    && self.is_string_compare_operand(right)
                {
                    self.generate_string_equality(*op, left, right, true);
                } else {
                    self.write("(");
                    self.emit_binop_operand(left);
                    self.write(&format!(" {} ", self.op_to_c(*op)));
                    self.emit_binop_operand(right);
                    self.write(")");
                }
            }
            IREexpr::UnOp { op, operand } => match op {
                UnOp::Not => {
                    self.write("(!");
                    self.generate_expr(operand);
                    self.write(")");
                }
                UnOp::Neg => {
                    if let IREexpr::Lit(v) = operand.as_ref()
                        && *v == 2147483647i64 + 1
                    {
                        self.write(&crate::cgen::types::c_int_limit(&Type::Int, false));
                        return;
                    }
                    self.write("(-");
                    self.generate_expr(operand);
                    self.write(")");
                }
            },
            IREexpr::Send {
                channel,
                value,
                value_type,
            } => {
                // ion_channel_send(sender, &value)
                // channel is &Sender<T>, which should be passed as the sender value
                // value needs to be in a variable (can't take address of literal)
                // When used as a statement, we use a statement expression to handle temp variables
                self.write("({ ");
                let needs_temp = !is_send_value_lvalue(value);
                if needs_temp {
                    self.write(&format!("{} _send_val = ", self.type_to_c(value_type)));
                    self.generate_expr(value);
                    self.write("; ");
                }
                self.write("ion_channel_send(");
                // channel is &Sender<T>, so we need to pass &tx where tx is the Sender value
                // If channel is AddressOf { inner: Var("tx") }, then tx is Sender, and &tx is &Sender
                // ion_channel_send expects const ion_sender_t*, so we pass &tx
                if let IREexpr::AddressOf { inner, mutable: _ } = channel.as_ref() {
                    // If it's &tx where tx is Sender, pass &tx (address of the struct)
                    self.write("&");
                    self.generate_expr(inner);
                } else {
                    // If it's already a reference expression, pass it directly
                    self.write("&");
                    self.generate_expr(channel);
                }
                self.write(", ");
                if needs_temp {
                    self.write("&_send_val");
                } else {
                    self.write("&");
                    self.generate_expr(value);
                }
                // Statement expression: last thing must be an expression (no semicolon)
                // Wrap the comma expression in parentheses to ensure proper parsing
                self.write("), (0)) })");
            }
            IREexpr::Recv { channel, elem_type } => {
                // recv(rx) lowers to:
                // ({ T tmp; ion_channel_recv(&rx, &tmp); tmp; })
                // This uses a GCC statement-expression, which is supported by gcc.
                // channel is &mut Receiver<T>, so we need to pass &rx where rx is the Receiver value
                self.write("({ ");
                self.write(&format!("{} tmp;", self.type_to_c(elem_type)));
                self.write(" ion_channel_recv(");
                // channel is &mut Receiver<T>, so we need to pass &rx
                // If channel is AddressOf { inner: Var("rx"), mutable: true }, then rx is Receiver, and &mut rx is &mut Receiver
                // ion_channel_recv expects ion_receiver_t*, so we pass &rx
                if let IREexpr::AddressOf { inner, mutable: _ } = channel.as_ref() {
                    // If it's &mut rx where rx is Receiver, pass &rx (address of the struct)
                    self.write("&");
                    self.generate_expr(inner);
                } else {
                    // If it's already a reference expression, pass it directly
                    self.write("&");
                    self.generate_expr(channel);
                }
                self.write(", &tmp); tmp; })");
            }
            IREexpr::StructLit { type_name, fields } => {
                // C99 compound literal: (Type){ .field1 = v1, .field2 = v2 }
                // Use type context to get monomorphized name if it's a generic type
                let struct_type_name = if let Some(context_ty) = type_context {
                    self.type_to_c(context_ty)
                } else {
                    type_name.clone()
                };

                // Check if any field is an array that needs special handling
                let mut needs_memcpy = false;
                let mut array_field_name = String::new();
                let mut array_var_name = String::new();

                // Try to get struct definition from type_context or type_name
                let struct_name_to_check = if let Some(context_ty) = type_context {
                    if let Type::Struct(struct_name) = context_ty {
                        Some(struct_name)
                    } else {
                        None
                    }
                } else {
                    Some(type_name)
                };

                if let Some(struct_name) = struct_name_to_check
                    && let Some(struct_decl) = self.struct_map.get(struct_name)
                {
                    for field in fields.iter() {
                        if let IREexpr::Var(ref var_name) = field.value {
                            // Check if this field is an array type
                            if let Some(field_decl) =
                                struct_decl.fields.iter().find(|f| f.name == field.name)
                                && let Type::Array { .. } = field_decl.ty
                            {
                                needs_memcpy = true;
                                array_field_name = field.name.clone();
                                array_var_name = var_name.clone();
                                break;
                            }
                        }
                    }
                }

                if needs_memcpy {
                    // When assigning an array variable to an array field, we can't do it directly in C
                    // Generate struct with zero initialization - memcpy will be handled separately
                    // We'll generate a comment marker that the caller can detect
                    self.write(&format!(
                        "({}){{{{0}}}} /* ARRAY_FIELD:{}:{} */",
                        struct_type_name, array_field_name, array_var_name
                    ));
                } else {
                    self.write(&format!("({}){{", struct_type_name));
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write(&format!(".{} = ", field.name));
                        self.generate_expr(&field.value);
                    }
                    self.write("}");
                }
            }
            IREexpr::FieldAccess {
                base,
                field,
                is_pointer,
            } => {
                // Special handling for String literal field access
                if let IREexpr::Var(ref var_name) = **base {
                    // Check if this variable is a function parameter of type String
                    let should_use_pointer =
                        if let Some(param_ty) = self.current_function_params.get(var_name) {
                            matches!(param_ty, Type::String)
                        } else if let Some(ty) = self.lookup_var_type(var_name) {
                            matches!(
                                ty,
                                Type::Ref {
                                    inner,
                                    ..
                                } if matches!(
                                    *inner,
                                    Type::Struct(_)
                                        | Type::Generic { .. }
                                        | Type::Tuple { .. }
                                        | Type::String
                                )
                            )
                        } else {
                            *is_pointer
                        };

                    self.generate_expr(base);
                    if should_use_pointer {
                        self.write("->");
                    } else {
                        self.write(".");
                    }
                    self.write(field);
                } else if let IREexpr::StringLit(ref s) = **base {
                    match field.as_str() {
                        "data" => {
                            // "string".data -> uint8_t pointer for byte APIs
                            self.write("(uint8_t*)");
                            self.generate_expr(base);
                        }
                        "len" => {
                            // "string".len -> length of the string
                            self.write(&s.len().to_string());
                        }
                        _ => {
                            // Should not happen if type checker is correct
                            self.generate_expr(base);
                            self.write(".");
                            self.write(field);
                        }
                    }
                } else {
                    let use_arrow = if let Some(ty) = self.infer_irexpr_type(base) {
                        matches!(
                            ty,
                            Type::Ref {
                                inner,
                                ..
                            } if matches!(
                                *inner,
                                Type::Struct(_) | Type::Generic { .. } | Type::Tuple { .. }
                            )
                        )
                    } else {
                        *is_pointer
                    };
                    self.generate_expr(base);
                    if use_arrow || *is_pointer {
                        self.write("->");
                    } else {
                        self.write(".");
                    }
                    self.write(field);
                }
            }
            IREexpr::EnumLit {
                enum_name,
                variant,
                args,
                named_fields,
            } => {
                let monomorphized_enum_name = if let Some(context_ty) = type_context {
                    let is_generic = self
                        .enum_map
                        .get(enum_name)
                        .map(|e| !e.generics.is_empty())
                        .unwrap_or(false);
                    if is_generic {
                        self.type_to_c(context_ty)
                    } else {
                        enum_name.clone()
                    }
                } else {
                    enum_name.clone()
                };
                self.emit_enum_variant_compound_literal(
                    &monomorphized_enum_name,
                    enum_name,
                    variant,
                    args,
                    named_fields.as_deref(),
                );
            }
            IREexpr::Match {
                expr,
                enum_type,
                arms,
            } => {
                // Generate match expression using switch statement
                // Get monomorphized enum name if it's generic
                let enum_decl = self.enum_map.get(enum_type).cloned();

                // Try to extract type from the expression if it's a Call to Vec::pop or Vec::get
                let (monomorphized_enum_name, type_params) =
                    self.resolve_option_match_instantiation(expr, enum_type, enum_decl.as_ref());

                let match_var_name = format!("match_val_{}", self.match_counter);
                self.match_counter += 1;

                self.write("({ ");
                // Check if the expression is a call to Vec::pop or Vec::get that returns void*
                let needs_cast = if let IREexpr::Call { callee, .. } = expr.as_ref() {
                    callee == "Vec::pop" || callee == "Vec::get"
                } else {
                    false
                };

                if needs_cast {
                    let elem_c_type = if type_params.is_empty() {
                        "int".to_string()
                    } else {
                        self.type_to_c(&type_params[0])
                    };
                    self.write_option_from_runtime_raw(
                        &monomorphized_enum_name,
                        &elem_c_type,
                        &match_var_name,
                        expr,
                    );
                } else {
                    // Evaluate expression once into a temporary with correct enum type
                    self.write(&format!(
                        "{} {} = ",
                        monomorphized_enum_name, match_var_name
                    ));
                    self.generate_expr(expr);
                    self.writeln(";");
                }
                self.write_indent();
                self.write(&format!("switch ({}.tag) {{", match_var_name));
                self.writeln("");
                self.emit_grouped_match_arms(
                    arms,
                    &monomorphized_enum_name,
                    enum_decl.as_ref(),
                    &match_var_name,
                    &type_params,
                    None,
                );
                self.write_indent();
                self.writeln("}");
                self.write(" 0 })");
            }
            IREexpr::Call {
                callee,
                args,
                return_type,
                tuple_destructure_index: _,
            } => {
                // Resolve METHOD:: prefix for method calls
                let resolved_callee = if callee.starts_with("METHOD::") {
                    self.resolve_method_call(callee, args)
                } else {
                    callee.clone()
                };

                // Handle special built-in functions
                let builtin_return_type = match resolved_callee.as_str() {
                    "Vec::new" | "Vec::with_capacity" => type_context.or(return_type.as_ref()),
                    _ => return_type.as_ref(),
                };
                if let Some(code) =
                    self.generate_builtin_call(resolved_callee.as_str(), args, builtin_return_type)
                {
                    self.write(&code);
                    for arg in args {
                        self.mark_moves_in_expr(arg);
                    }
                } else {
                    // Regular function call.
                    let func_name = self.resolve_c_function_name(&resolved_callee);

                    self.write(&func_name);
                    self.write("(");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        // If this is an extern function expecting int by value, convert &int (immutable) arguments to int
                        // For &int -> int: just use the inner expression (the variable itself)
                        // For &mut int -> int*: keep the address-of (don't dereference)
                        if let Some(param_types) = self.function_param_types.get(&func_name)
                            && i < param_types.len()
                            && let Some((array_name, size, elem_ty)) =
                                self.match_array_to_slice_coercion(&param_types[i], arg)
                        {
                            self.emit_array_to_slice_ptr(&array_name, size, &elem_ty);
                            continue;
                        }
                        if let Some(param_types) = self.extern_functions.get(&func_name)
                            && i < param_types.len()
                            && let Type::Ref {
                                inner: boxed_int,
                                mutable: param_mutable,
                            } = &param_types[i]
                            && matches!(**boxed_int, Type::Int)
                            && let IREexpr::AddressOf {
                                inner,
                                mutable: arg_mutable,
                            } = arg
                        {
                            // Only dereference if parameter is &int (immutable) and argument is also &int (immutable)
                            // For &mut int parameters, we need to pass int*, so keep the address-of
                            if !param_mutable && !arg_mutable {
                                // &int -> int: just use the inner expression (the variable itself)
                                // This converts &client_fd to client_fd
                                self.generate_expr(inner);
                                continue;
                            }
                            // For &mut int, fall through to generate &var normally
                        }
                        let call_param_types = self
                            .function_param_types
                            .get(&func_name)
                            .or_else(|| self.extern_functions.get(&func_name));
                        if matches!(arg, IREexpr::StringLit(_))
                            && let Some(param_types) = call_param_types
                            && i < param_types.len()
                            && Self::param_is_byte_ptr(&param_types[i])
                        {
                            self.write("(uint8_t*)");
                        }
                        self.generate_expr(arg);
                    }
                    self.write(")");
                    for arg in args {
                        self.mark_moves_in_expr(arg);
                    }
                }
            }
            IREexpr::StringLit(value) => {
                // String literals are read-only C string literals
                // When used as String type, they should be converted at assignment site
                // For other contexts, emit as C string literal
                // Escape special characters for C (order matters - escape backslash first)
                let escaped = value
                    .replace('\\', "\\\\") // Escape backslashes first
                    .replace('"', "\\\"") // Escape quotes
                    .replace('\n', "\\n") // Escape newlines
                    .replace('\r', "\\r") // Escape carriage returns
                    .replace('\t', "\\t"); // Escape tabs
                self.write(&format!("\"{}\"", escaped));
            }
            IREexpr::TupleLit {
                elements,
                elem_types,
            } => {
                let name = tuple_type_name(elem_types);
                self.write(&format!("({}){{", name));
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!(".f{} = ", i));
                    self.generate_expr(elem);
                }
                self.write("}");
            }
            IREexpr::ArrayLiteral { elements, repeat } => {
                if let Some((value_expr, count)) = repeat {
                    // Array repeat: [value; count]
                    // For zero initialization, use {0} syntax
                    // For other values, we'll generate a compound literal with explicit initialization
                    if let IREexpr::Lit(0) = **value_expr {
                        // Zero initialization: [0; count] -> {0}
                        self.write("{0}");
                    } else {
                        // Non-zero: generate explicit initialization
                        // Note: C doesn't support [value; count] directly, so we generate
                        // a compound literal. For large arrays, this might be inefficient,
                        // but it's correct for now.
                        self.write("{");
                        let count_val = *count;
                        for i in 0..count_val {
                            if i > 0 {
                                self.write(", ");
                            }
                            self.generate_expr(value_expr);
                        }
                        self.write("}");
                    }
                } else {
                    // Regular array literal: [1, 2, 3] -> {1, 2, 3}
                    self.write("{");
                    for (i, elem) in elements.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.generate_expr(elem);
                    }
                    self.write("}");
                }
            }
            IREexpr::Index {
                target,
                index,
                target_type,
            } => {
                let bounds_check = self.bounds_check_for_target_type(target_type.as_ref());

                if self.in_unsafe_block {
                    self.emit_index_access(target, index, bounds_check.as_ref());
                } else {
                    let temp_var = format!("__ion_idx_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;

                    match bounds_check {
                        Some(BoundsCheck::Fixed(len)) => {
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; (");
                            self.write(&temp_var);
                            self.write(" >= 0 && ");
                            self.write(&temp_var);
                            self.write(&format!(" < {}) ? ", len));
                            self.generate_expr(target);
                            self.write("[");
                            self.write(&temp_var);
                            self.write("] : (ion_panic(\"Array index out of bounds\"), ");
                            self.generate_expr(target);
                            self.write("[0]); })");
                        }
                        Some(BoundsCheck::StringLen) => {
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; (");
                            self.write(&temp_var);
                            self.write(" >= 0 && ");
                            self.write(&temp_var);
                            self.write(" < (int)(");
                            self.generate_expr(target);
                            self.write("->len)) ? ");
                            self.generate_expr(target);
                            self.write("->data[");
                            self.write(&temp_var);
                            self.write(
                                "] : (ion_panic(\"String index out of bounds\"), (uint8_t)0); })",
                            );
                        }
                        Some(BoundsCheck::SliceLen { by_ref }) => {
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; (");
                            self.write(&temp_var);
                            self.write(" >= 0 && ");
                            self.write(&temp_var);
                            self.write(" < ");
                            self.emit_slice_len(target, by_ref);
                            self.write(") ? ");
                            self.emit_slice_data_index(target, &temp_var, by_ref);
                            self.write(" : (ion_panic(\"Slice index out of bounds\"), ");
                            self.emit_slice_data_index(target, "0", by_ref);
                            self.write("); })");
                        }
                        None => {
                            self.emit_index_access(target, index, None);
                        }
                    }
                }
            }
            IREexpr::Cast { expr, target_type } => {
                // Cast: (target_type)expr
                self.write("(");
                self.write(&self.type_to_c(target_type));
                self.write(")");
                self.generate_expr(expr);
            }
            IREexpr::Assign { target, value } => {
                // Generate assignment: target = value
                self.write(target);
                self.write(" = ");
                self.generate_expr(value);
            }
            IREexpr::AssignIndex {
                target,
                index,
                value,
            } => {
                // Generate array element assignment: arr[i] = value
                self.generate_expr(target);
                self.write("[");
                self.generate_expr(index);
                self.write("] = ");
                self.generate_expr(value);
            }
            IREexpr::FnLiteral(lit) => {
                self.generate_fn_literal(lit);
                self.write(&lit.symbol);
            }
        }
    }

    fn type_to_c(&self, ty: &Type) -> String {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        type_to_c_impl(&resolved)
    }

    fn bounds_check_for_target_type(&self, ty: Option<&Type>) -> Option<BoundsCheck> {
        let resolved = ty.map(|t| resolve_type_alias(t, &self.type_aliases));
        match resolved.as_ref() {
            Some(Type::Array { size, .. }) => Some(BoundsCheck::Fixed(*size)),
            Some(Type::String) => Some(BoundsCheck::StringLen),
            Some(Type::Slice { .. }) => Some(BoundsCheck::SliceLen { by_ref: false }),
            Some(Type::Ref { inner, .. }) if matches!(**inner, Type::Slice { .. }) => {
                Some(BoundsCheck::SliceLen { by_ref: true })
            }
            _ => None,
        }
    }

    fn emit_slice_len(&mut self, target: &IREexpr, by_ref: bool) {
        self.generate_expr(target);
        if by_ref {
            self.write("->len");
        } else {
            self.write(".len");
        }
    }

    fn emit_slice_data_index(&mut self, target: &IREexpr, index: &str, by_ref: bool) {
        self.generate_expr(target);
        if by_ref {
            self.write("->data[");
        } else {
            self.write(".data[");
        }
        self.write(index);
        self.write("]");
    }

    fn emit_index_access(
        &mut self,
        target: &IREexpr,
        index: &IREexpr,
        bounds_check: Option<&BoundsCheck>,
    ) {
        match bounds_check {
            Some(BoundsCheck::SliceLen { by_ref }) => {
                self.generate_expr(target);
                if *by_ref {
                    self.write("->data[");
                } else {
                    self.write(".data[");
                }
                self.generate_expr(index);
                self.write("]");
            }
            Some(BoundsCheck::StringLen) => {
                self.generate_expr(target);
                self.write("->data[");
                self.generate_expr(index);
                self.write("]");
            }
            _ => {
                self.generate_expr(target);
                self.write("[");
                self.generate_expr(index);
                self.write("]");
            }
        }
    }

    /// Zero-initialize a moved capture in the parent scope after ownership transfers to the thread.
    fn zero_value_for_type(&self, ty: &Type) -> String {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        match resolved {
            Type::Struct(_) | Type::Enum(_) | Type::Sender { .. } | Type::Receiver { .. } => {
                format!("({}){{0}}", type_to_c_impl(&resolved))
            }
            _ => "0".to_string(),
        }
    }

    /// C initializer that clears a moved-out enum variant payload field so a later scrutinee drop
    /// cannot double-free the same owned value.
    fn zero_value_for_scrutinee_payload(&self, ty: &Type) -> String {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        match resolved {
            Type::String | Type::Box { .. } | Type::Vec { .. } => "NULL".to_string(),
            Type::Struct(_) | Type::Enum(_) | Type::Sender { .. } | Type::Receiver { .. } => {
                format!("({}){{0}}", self.type_to_c(&resolved))
            }
            _ => "0".to_string(),
        }
    }

    fn emit_match_scrutinee_payload_moved_out(
        &mut self,
        match_var_name: &str,
        variant_idx: usize,
        field_name: &str,
        ty: &Type,
    ) {
        if !self.needs_drop(ty) {
            return;
        }
        self.write_indent();
        self.writeln(&format!(
            "{}.data.variant_{}.{} = {};",
            match_var_name,
            variant_idx,
            field_name,
            self.zero_value_for_scrutinee_payload(ty)
        ));
    }

    fn emit_match_scrutinee_whole_enum_moved_out(
        &mut self,
        match_var_name: &str,
        enum_decl: &EnumDecl,
        type_params: &[Type],
    ) {
        let substitutions: std::collections::HashMap<String, &Type> = enum_decl
            .generics
            .iter()
            .zip(type_params.iter())
            .map(|(name, ty)| (name.clone(), ty))
            .collect();

        self.write_indent();
        self.writeln(&format!("switch ({}.tag) {{", match_var_name));
        self.indent_level += 1;
        for (variant_idx, variant) in enum_decl.variants.iter().enumerate() {
            let has_payloads = !variant.payload_types.is_empty() || variant.named_fields.is_some();
            if !has_payloads {
                continue;
            }
            self.write_indent();
            self.writeln(&format!("case {variant_idx}:"));
            self.indent_level += 1;
            if let Some(named_fields) = &variant.named_fields {
                for (field_name, field_ty) in named_fields {
                    let concrete_ty = if !substitutions.is_empty() {
                        substitute_type_params(field_ty, &substitutions)
                    } else {
                        field_ty.clone()
                    };
                    self.emit_match_scrutinee_payload_moved_out(
                        match_var_name,
                        variant_idx,
                        field_name,
                        &concrete_ty,
                    );
                }
            } else {
                for (arg_idx, payload_ty) in variant.payload_types.iter().enumerate() {
                    let concrete_ty = if !substitutions.is_empty() {
                        substitute_type_params(payload_ty, &substitutions)
                    } else {
                        payload_ty.clone()
                    };
                    self.emit_match_scrutinee_payload_moved_out(
                        match_var_name,
                        variant_idx,
                        &format!("arg{arg_idx}"),
                        &concrete_ty,
                    );
                }
            }
            self.write_indent();
            self.writeln("break;");
            self.indent_level -= 1;
        }
        self.indent_level -= 1;
        self.write_indent();
        self.writeln("}");
    }

    fn op_to_c(&self, op: BinOp) -> String {
        match op {
            BinOp::Add => "+".to_string(),
            BinOp::Sub => "-".to_string(),
            BinOp::Mul => "*".to_string(),
            BinOp::Div => "/".to_string(),
            BinOp::Rem => "%".to_string(),
            BinOp::Lt => "<".to_string(),
            BinOp::Gt => ">".to_string(),
            BinOp::Le => "<=".to_string(),
            BinOp::Ge => ">=".to_string(),
            BinOp::Eq => "==".to_string(),
            BinOp::Ne => "!=".to_string(),
            BinOp::And => "&&".to_string(),
            BinOp::BitAnd => "&".to_string(),
            BinOp::BitOr => "|".to_string(),
            BinOp::BitXor => "^".to_string(),
            BinOp::ShiftLeft => "<<".to_string(),
            BinOp::ShiftRight => ">>".to_string(),
            BinOp::Or => "||".to_string(),
        }
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn writeln(&mut self, s: &str) {
        self.write(s);
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.write("    ");
        }
    }

    fn generate_enum_type(&mut self, enum_decl: &EnumDecl) {
        // Generate enum type with tag and union: typedef struct EnumName { int tag; union { ... } data; } EnumName;
        let enum_name = &enum_decl.name;
        self.write(&format!("typedef struct {} {{\n", enum_name));
        self.indent_level += 1;
        self.write_indent();
        self.writeln("int tag;");
        self.write_indent();
        self.write("union {\n");
        self.indent_level += 1;

        // Generate union members for each variant with payloads
        for (variant_idx, variant) in enum_decl.variants.iter().enumerate() {
            // Check if variant has payloads (either tuple or struct variant)
            let has_payloads = !variant.payload_types.is_empty() || variant.named_fields.is_some();
            if has_payloads {
                self.write_indent();
                self.write("struct {\n");
                self.indent_level += 1;

                // Handle struct variants with named fields
                if let Some(ref named_fields) = variant.named_fields {
                    for (field_name, field_ty) in named_fields {
                        self.write_indent();
                        self.write(&format!("{} {};", self.type_to_c(field_ty), field_name));
                        self.writeln("");
                    }
                } else {
                    // Handle tuple variants with positional arguments
                    for (i, payload_ty) in variant.payload_types.iter().enumerate() {
                        self.write_indent();
                        self.write(&format!("{} arg{};", self.type_to_c(payload_ty), i));
                        self.writeln("");
                    }
                }

                self.indent_level -= 1;
                self.write_indent();
                self.write(&format!("}} variant_{};\n", variant_idx));
            }
        }

        self.indent_level -= 1;
        self.write_indent();
        self.writeln("} data;");
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", enum_name));
        self.writeln("");
    }

    fn generate_extern_block(&mut self, extern_block: &ExternBlock) {
        // Only support "C" linkage for now
        if extern_block.linkage != "C" {
            // Skip non-C linkage blocks
            return;
        }

        // Generate extern function prototypes
        for extern_fn in &extern_block.functions {
            // Store parameter types for later use in call generation
            let param_types: Vec<Type> = extern_fn.params.iter().map(|p| p.ty.clone()).collect();
            self.extern_functions
                .insert(extern_fn.name.clone(), param_types);

            let return_type = extern_fn
                .return_type
                .as_ref()
                .map(|t| self.type_to_c(t))
                .unwrap_or_else(|| "void".to_string());

            self.write(&format!("extern {} {}(", return_type, extern_fn.name));

            // Generate parameters
            if extern_fn.params.is_empty() && !extern_fn.variadic {
                self.write("void");
            } else {
                for (i, param) in extern_fn.params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    // Special handling: for &int (immutable) parameters in extern functions, generate as int
                    // (POSIX functions expect int by value, not int*)
                    // &mut int should remain int* (for output parameters like addrlen in accept)
                    let param_c_type = match &param.ty {
                        Type::Ref { inner, mutable } => {
                            if let Type::Int = **inner {
                                if *mutable {
                                    // &mut int -> int* (for output parameters)
                                    "int*".to_string()
                                } else {
                                    // &int -> int (for POSIX compatibility, input parameters)
                                    "int".to_string()
                                }
                            } else {
                                self.type_to_c(&param.ty)
                            }
                        }
                        Type::Array { inner, size } => {
                            let base_type = self.type_to_c(inner);
                            format!("{} {}[{}]", base_type, param.name, size)
                        }
                        _ => self.type_to_c(&param.ty),
                    };
                    // Special handling for array types: C syntax is "int arr[3]" not "int[3] arr"
                    match &param.ty {
                        Type::Array { inner, size } => {
                            let base_type = self.type_to_c(inner);
                            self.write(&format!("{} {}[{}]", base_type, param.name, size));
                        }
                        Type::Fn { .. } => {
                            self.write(&fn_type_to_c_decl(&param.ty, &param.name));
                        }
                        _ => {
                            self.write(&format!("{} {}", param_c_type, param.name));
                        }
                    }
                }
                if extern_fn.variadic {
                    if !extern_fn.params.is_empty() {
                        self.write(", ");
                    }
                    self.write("...");
                }
            }

            self.writeln(");");
        }
        self.writeln("");
    }

    /// Resolve METHOD:: prefix to actual qualified method name
    /// This infers the type from common patterns (Vec, String, etc.)
    fn resolve_method_call(&self, callee: &str, args: &[IREexpr]) -> String {
        if !callee.starts_with("METHOD::") {
            return callee.to_string();
        }

        let method_name = callee.strip_prefix("METHOD::").unwrap_or(callee);

        // Try to infer receiver type from first argument
        if let Some(receiver) = args.first() {
            // Check if receiver is a variable or field access that we can infer from
            // For now, we'll check known method patterns
            let vec_methods = [
                "push",
                "pop",
                "len",
                "capacity",
                "get",
                "set",
                "with_capacity",
            ];
            let string_methods = ["push_str", "push_byte", "len"];

            if vec_methods.contains(&method_name) {
                // Check if we have Vec types in generic_instantiations
                if self
                    .generic_instantiations
                    .iter()
                    .any(|(_, (base, _))| base == "Vec")
                {
                    return format!("Vec::{}", method_name);
                }
            }

            if string_methods.contains(&method_name) {
                // String methods
                return format!("String::{}", method_name);
            }

            // Try to infer from receiver expression patterns
            match receiver {
                IREexpr::Var(_var_name) => {
                    // We can't look up variable types here, but we can try common patterns
                    // For now, fall back to checking if it's a known Vec/String method
                    if vec_methods.contains(&method_name) {
                        return format!("Vec::{}", method_name);
                    }
                    if string_methods.contains(&method_name) {
                        return format!("String::{}", method_name);
                    }
                }
                IREexpr::Call {
                    callee: inner_callee,
                    ..
                } => {
                    // If receiver is a call like Vec::new(), infer Vec
                    if inner_callee == "Vec::new" && vec_methods.contains(&method_name) {
                        return format!("Vec::{}", method_name);
                    }
                    if inner_callee == "String::new" && string_methods.contains(&method_name) {
                        return format!("String::{}", method_name);
                    }
                }
                _ => {}
            }
        }

        // Fallback: try to infer from method name alone for built-in types
        let vec_methods = ["push", "pop", "len", "capacity", "get", "set"];
        let string_methods = ["push_str", "push_byte", "len"];

        if vec_methods.contains(&method_name) {
            format!("Vec::{}", method_name)
        } else if string_methods.contains(&method_name) {
            format!("String::{}", method_name)
        } else {
            // Unknown method, return as-is (will likely error later)
            callee.to_string()
        }
    }

    /// Find the monomorphized enum name and type parameters for a given base enum name
    fn find_enum_instantiation(
        &self,
        enum_type: &str,
        enum_decl: Option<&EnumDecl>,
    ) -> (String, Vec<Type>) {
        if let Some((mono_name, params)) = self
            .generic_instantiations
            .iter()
            .find(|(_, (base, _))| base == enum_type)
            .map(|(mono, (_, params))| (mono.clone(), params.clone()))
        {
            (mono_name, params)
        } else if let Some(decl) = enum_decl {
            // If not found in instantiations but enum is generic, compute it
            if !decl.generics.is_empty() {
                // This shouldn't happen in well-typed code, but fallback to base name
                (enum_type.to_string(), Vec::new())
            } else {
                (enum_type.to_string(), Vec::new())
            }
        } else {
            (enum_type.to_string(), Vec::new())
        }
    }

    /// Resolve monomorphized `Option<T>` when matching on `Vec::get` / `Vec::pop`.
    fn resolve_option_match_instantiation(
        &self,
        scrutinee: &IREexpr,
        enum_type: &str,
        enum_decl: Option<&EnumDecl>,
    ) -> (String, Vec<Type>) {
        if let IREexpr::Call {
            callee,
            args,
            return_type,
            ..
        } = scrutinee
        {
            let is_vec_pop_or_get = callee == "Vec::pop"
                || callee == "Vec::get"
                || callee == "METHOD::pop"
                || callee == "METHOD::get";

            if is_vec_pop_or_get || callee == "Vec::get_ref" {
                if let Some(Type::Generic { name, params }) = return_type
                    && name == "Option"
                    && params.len() == 1
                {
                    let mono_name = mangle_type_name("Option", params);
                    return (mono_name, params.clone());
                }
                if let Some(first_arg) = args.first()
                    && let Some(elem_type) = self.vec_elem_type_from_arg(first_arg)
                {
                    if callee == "Vec::get_ref" {
                        let ref_elem = Type::Ref {
                            inner: Box::new(elem_type.clone()),
                            mutable: false,
                        };
                        let mono_name = mangle_type_name("Option", std::slice::from_ref(&ref_elem));
                        return (mono_name, vec![ref_elem]);
                    }
                    let mono_name = mangle_type_name("Option", std::slice::from_ref(&elem_type));
                    return (mono_name, vec![elem_type]);
                }
            } else if let Some(Type::Generic { name, params }) = return_type
                && name == "Option"
                && params.len() == 1
            {
                let mono_name = mangle_type_name("Option", params);
                return (mono_name, params.clone());
            }
        }
        self.find_enum_instantiation(enum_type, enum_decl)
    }

    fn generate_match_block(
        &mut self,
        expr: &IREexpr,
        enum_type: &str,
        arms: &[IRMatchArm],
        match_result: Option<(&str, &Type)>,
    ) {
        // Generate match as a block (for statement context)
        // Get monomorphized enum name if it's generic
        let enum_decl = self.enum_map.get(enum_type).cloned();

        // Try to extract type from the expression if it's a Call to Vec::pop or Vec::get
        let (monomorphized_enum_name, type_params) =
            self.resolve_option_match_instantiation(expr, enum_type, enum_decl.as_ref());

        // Ensure Option<int> is in generic_instantiations
        // Note: The actual type generation happens at file scope before functions
        if !type_params.is_empty() && enum_type == "Option" {
            let key = monomorphized_enum_name.clone();
            if !self.generated_types.contains_key(&key) {
                // Add to generic_instantiations if not already there
                if !self.generic_instantiations.contains_key(&key) {
                    self.generic_instantiations
                        .insert(key.clone(), ("Option".to_string(), type_params.clone()));
                }
            }
        }

        let match_var_name = format!("match_val_{}", self.match_counter);
        self.match_counter += 1;

        self.write_indent();
        // Check if the expression is a call to Vec::pop or Vec::get (or METHOD::pop/get for method calls) that returns void*
        // Also check if it's a Call expression that returns Option<T> - these runtime functions return void* that need casting
        let needs_cast = if let IREexpr::Call {
            callee,
            return_type,
            ..
        } = expr
        {
            let is_vec_heap_option = callee == "Vec::pop"
                || callee == "Vec::get"
                || callee == "METHOD::pop"
                || callee == "METHOD::get"
                || callee.starts_with("ion_vec_pop")
                || callee.starts_with("ion_vec_get");

            // Stack-local Option<&T> from Vec::get_ref is assigned directly.
            let returns_heap_option = if let Some(Type::Generic { name, params }) = return_type {
                name == "Option" && params.len() == 1 && !matches!(params[0], Type::Ref { .. })
            } else {
                false
            };

            is_vec_heap_option || returns_heap_option
        } else {
            false
        };

        if needs_cast {
            let elem_c_type = if type_params.is_empty() {
                "int".to_string()
            } else {
                self.type_to_c(&type_params[0])
            };
            self.write_option_from_runtime_raw(
                &monomorphized_enum_name,
                &elem_c_type,
                &match_var_name,
                expr,
            );
        } else {
            self.write(&format!(
                "{} {} = ",
                monomorphized_enum_name, match_var_name
            ));
            self.generate_expr(expr);
            self.writeln(";");
            self.mark_moves_in_expr(expr);
        }
        self.write_indent();
        self.write(&format!("switch ({}.tag) {{", match_var_name));
        self.writeln("");
        self.emit_grouped_match_arms(
            arms,
            &monomorphized_enum_name,
            enum_decl.as_ref(),
            &match_var_name,
            &type_params,
            match_result,
        );
        self.write_indent();
        self.writeln("}");
    }

    fn emit_grouped_match_arms(
        &mut self,
        arms: &[IRMatchArm],
        enum_type: &str,
        enum_decl: Option<&EnumDecl>,
        match_var_name: &str,
        type_params: &[Type],
        match_result: Option<(&str, &Type)>,
    ) {
        self.indent_level += 1;
        let mut grouped: std::collections::BTreeMap<usize, Vec<&IRMatchArm>> =
            std::collections::BTreeMap::new();
        for arm in arms {
            let variant_idx = match &arm.pattern {
                IRPattern::Variant { variant, .. } => enum_decl
                    .and_then(|e| e.variants.iter().position(|v| v.name == *variant))
                    .unwrap_or(0),
                _ => usize::MAX,
            };
            grouped.entry(variant_idx).or_default().push(arm);
        }
        for (variant_idx, group_arms) in grouped {
            if variant_idx == usize::MAX {
                for arm in group_arms {
                    self.generate_match_arm(
                        arm,
                        enum_type,
                        enum_decl,
                        match_var_name,
                        type_params,
                        false,
                        match_result,
                    );
                }
            } else {
                let variant_name = enum_decl
                    .and_then(|e| e.variants.get(variant_idx))
                    .map(|v| v.name.as_str())
                    .unwrap_or("variant");
                self.write_indent();
                self.writeln(&format!("case {}: {{ // {}", variant_idx, variant_name));
                self.indent_level += 1;
                self.scope_begin(&[]);
                if let Some(first_arm) = group_arms.first() {
                    self.generate_match_arm_payload_bindings(
                        first_arm,
                        enum_decl,
                        match_var_name,
                        type_params,
                        variant_idx,
                    );
                }
                for arm in group_arms {
                    if let Some(ref guard) = arm.guard {
                        self.write_indent();
                        self.write("if (");
                        self.generate_expr(guard);
                        self.writeln(") {");
                        self.indent_level += 1;
                        if let Some((result_var, result_type)) = match_result {
                            self.emit_match_arm_result_stmts(&arm.body, result_var, result_type);
                        } else {
                            self.generate_block(&arm.body);
                        }
                        self.scope_emit_top_frame_drops();
                        self.write_indent();
                        self.writeln("break;");
                        self.indent_level -= 1;
                        self.write_indent();
                        self.writeln("}");
                    } else {
                        self.generate_match_arm_body(arm, match_result, false);
                    }
                }
                self.scope_emit_exit();
                self.write_indent();
                self.writeln("break;");
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
            }
        }
        self.indent_level -= 1;
    }

    fn generate_match_arm_payload_bindings(
        &mut self,
        arm: &IRMatchArm,
        enum_decl: Option<&EnumDecl>,
        match_var_name: &str,
        type_params: &[Type],
        variant_idx: usize,
    ) {
        let IRPattern::Variant {
            variant,
            sub_patterns,
            named_fields,
            ..
        } = &arm.pattern
        else {
            return;
        };

        if let Some(variant_decl) =
            enum_decl.and_then(|e| e.variants.iter().find(|v| v.name == *variant))
        {
            // Build substitution map for generic parameters
            let substitutions: std::collections::HashMap<String, &Type> = enum_decl
                .map(|e| {
                    e.generics
                        .iter()
                        .zip(type_params.iter())
                        .map(|(name, ty)| (name.clone(), ty))
                        .collect()
                })
                .unwrap_or_default();

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
                            let concrete_field_ty = if !substitutions.is_empty() {
                                substitute_type_params(field_ty, &substitutions)
                            } else {
                                field_ty.clone()
                            };

                            match field_pattern {
                                IRPattern::Binding { name } => {
                                    // Extract field into binding variable
                                    // For struct variants, fields are stored in variant_N.field_name
                                    self.write_indent();
                                    self.write(&format!(
                                        "{} {} = {}.data.variant_{}.{};",
                                        self.type_to_c(&concrete_field_ty),
                                        name,
                                        match_var_name,
                                        variant_idx,
                                        field_name
                                    ));
                                    self.writeln("");
                                    self.emit_match_scrutinee_payload_moved_out(
                                        match_var_name,
                                        variant_idx,
                                        field_name,
                                        &concrete_field_ty,
                                    );
                                    self.scope_register_binding(name, &concrete_field_ty);
                                    if Self::should_silence_unused_binding(name, &concrete_field_ty)
                                    {
                                        self.emit_silence_unused_binding(name);
                                    }
                                }
                                IRPattern::Wildcard => {
                                    // Wildcard - don't extract, field is ignored
                                }
                                IRPattern::Variant { .. } => {
                                    // Nested variant pattern - extract to temp
                                    let temp_name = format!("_field_{}", field_name);
                                    self.write_indent();
                                    self.write(&format!(
                                        "{} {} = {}.data.variant_{}.{};",
                                        self.type_to_c(&concrete_field_ty),
                                        temp_name,
                                        match_var_name,
                                        variant_idx,
                                        field_name
                                    ));
                                    self.writeln("");
                                    self.emit_match_scrutinee_payload_moved_out(
                                        match_var_name,
                                        variant_idx,
                                        field_name,
                                        &concrete_field_ty,
                                    );
                                }
                            }
                        }
                    }
                }
            } else {
                // Handle tuple variants with positional patterns
                for (i, payload_ty) in variant_decl.payload_types.iter().enumerate() {
                    // Substitute generic parameters in payload type
                    let concrete_payload_ty = if !substitutions.is_empty() {
                        substitute_type_params(payload_ty, &substitutions)
                    } else {
                        // If no substitutions available, use the payload type as-is
                        // (This shouldn't happen in well-typed code, but handle gracefully)
                        payload_ty.clone()
                    };

                    // Get the pattern for this payload position (or use wildcard)
                    if let Some(sub_pattern) = sub_patterns.get(i) {
                        match sub_pattern {
                            IRPattern::Binding { name } => {
                                // Extract payload into binding variable
                                let payload_field = format!("arg{i}");
                                self.write_indent();
                                if let Type::Ref { inner, .. } = &concrete_payload_ty {
                                    // Vec::get_ref yields Option<&T>. Copy primitives in-place; borrow
                                    // structs with owned fields as T* so nested Vec fields are not dropped.
                                    if self.type_needs_drop(inner) {
                                        self.write(&format!(
                                            "{} {} = {}.data.variant_{}.{};",
                                            self.type_to_c(&concrete_payload_ty),
                                            name,
                                            match_var_name,
                                            variant_idx,
                                            payload_field
                                        ));
                                        self.writeln("");
                                        self.scope_register_binding(name, &concrete_payload_ty);
                                    } else {
                                        self.write(&format!(
                                            "{} {} = *{}.data.variant_{}.{};",
                                            self.type_to_c(inner),
                                            name,
                                            match_var_name,
                                            variant_idx,
                                            payload_field
                                        ));
                                        self.writeln("");
                                        self.scope_register_binding(name, inner);
                                    }
                                } else {
                                    self.write(&format!(
                                        "{} {} = {}.data.variant_{}.{};",
                                        self.type_to_c(&concrete_payload_ty),
                                        name,
                                        match_var_name,
                                        variant_idx,
                                        payload_field
                                    ));
                                    self.writeln("");
                                    self.emit_match_scrutinee_payload_moved_out(
                                        match_var_name,
                                        variant_idx,
                                        &payload_field,
                                        &concrete_payload_ty,
                                    );
                                    self.scope_register_binding(name, &concrete_payload_ty);
                                }
                                if Self::should_silence_unused_binding(
                                    name,
                                    if let Type::Ref { inner, .. } = &concrete_payload_ty {
                                        inner
                                    } else {
                                        &concrete_payload_ty
                                    },
                                ) {
                                    self.emit_silence_unused_binding(name);
                                }
                            }
                            IRPattern::Wildcard => {
                                // Wildcard - don't extract, payload is ignored
                            }
                            IRPattern::Variant { .. } => {
                                // Nested variant pattern - for now, extract to temp and match recursively
                                // This is a simplified version - full implementation would recurse
                                let payload_field = format!("arg{i}");
                                let temp_name = format!("_payload_{}", i);
                                self.write_indent();
                                self.write(&format!(
                                    "{} {} = {}.data.variant_{}.{};",
                                    self.type_to_c(&concrete_payload_ty),
                                    temp_name,
                                    match_var_name,
                                    variant_idx,
                                    payload_field
                                ));
                                self.writeln("");
                                self.emit_match_scrutinee_payload_moved_out(
                                    match_var_name,
                                    variant_idx,
                                    &payload_field,
                                    &concrete_payload_ty,
                                );
                            }
                        }
                    } else {
                        // No pattern specified - treat as wildcard
                    }
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn generate_match_arm(
        &mut self,
        arm: &IRMatchArm,
        enum_type: &str,
        _enum_decl: Option<&EnumDecl>,
        match_var_name: &str,
        type_params: &[Type],
        _grouped: bool,
        match_result: Option<(&str, &Type)>,
    ) {
        match &arm.pattern {
            IRPattern::Wildcard => {
                self.write_indent();
                self.writeln("default:");
                self.indent_level += 1;
                self.generate_match_arm_body(arm, match_result, true);
                self.indent_level -= 1;
            }
            IRPattern::Binding { name } => {
                self.write_indent();
                self.writeln(&format!("default: // binding {}", name));
                self.indent_level += 1;
                self.write_indent();
                self.writeln(&format!("{} {} = {};", enum_type, name, match_var_name));
                if let Some(enum_decl) = _enum_decl {
                    self.emit_match_scrutinee_whole_enum_moved_out(
                        match_var_name,
                        enum_decl,
                        type_params,
                    );
                }
                self.generate_match_arm_body(arm, match_result, true);
                self.indent_level -= 1;
            }
            IRPattern::Variant { .. } => {
                // Variant arms are handled by grouped generation in generate_match_block.
            }
        }
    }

    fn generate_match_arm_body(
        &mut self,
        arm: &IRMatchArm,
        match_result: Option<(&str, &Type)>,
        trailing_break: bool,
    ) {
        if let Some(ref guard) = arm.guard {
            self.write_indent();
            self.write("if (");
            self.generate_expr(guard);
            self.writeln(") {");
            self.indent_level += 1;
            if let Some((result_var, result_type)) = match_result {
                self.emit_match_arm_result_stmts(&arm.body, result_var, result_type);
            } else {
                self.generate_block(&arm.body);
            }
            self.indent_level -= 1;
            self.write_indent();
            self.writeln("}");
            if trailing_break {
                self.write_indent();
                self.writeln("break;");
            }
        } else if let Some((result_var, result_type)) = match_result {
            self.emit_match_arm_result_stmts(&arm.body, result_var, result_type);
            if trailing_break {
                self.write_indent();
                self.writeln("break;");
            }
        } else {
            self.generate_block(&arm.body);
            if trailing_break {
                self.write_indent();
                self.writeln("break;");
            }
        }
    }

    fn emit_match_arm_result_stmts(
        &mut self,
        body: &IRBlock,
        result_var: &str,
        result_type: &Type,
    ) {
        self.emit_match_arm_result_from_stmts(&body.statements, 0, result_var, result_type);
    }

    /// Lower rvalue-match arm statements into assignments / control flow inside a `switch`.
    /// Function exits (`return`) use `emit_function_return` like every other return site.
    fn emit_match_arm_result_from_stmts(
        &mut self,
        stmts: &[IRStmt],
        idx: usize,
        result_var: &str,
        result_type: &Type,
    ) {
        if idx >= stmts.len() {
            return;
        }

        let is_last = idx + 1 == stmts.len();
        match &stmts[idx] {
            IRStmt::Return(ret) => {
                self.emit_function_return(ret);
            }
            IRStmt::Break => {
                self.write_indent();
                self.writeln("break;");
            }
            IRStmt::Continue => {
                self.write_indent();
                self.writeln("continue;");
            }
            IRStmt::Expr(expr) => {
                if is_last {
                    self.write_indent();
                    self.write(&format!("{} = ", result_var));
                    self.generate_expr_with_type(expr, Some(result_type));
                    self.writeln(";");
                } else {
                    self.generate_stmt(&stmts[idx]);
                    self.emit_match_arm_result_from_stmts(stmts, idx + 1, result_var, result_type);
                }
            }
            IRStmt::If(ir_if) => {
                if is_last {
                    self.write_indent();
                    self.write("if (");
                    self.generate_expr(&ir_if.cond);
                    self.writeln(") {");
                    self.indent_level += 1;
                    self.emit_match_arm_result_from_stmts(
                        &ir_if.then_block.statements,
                        0,
                        result_var,
                        result_type,
                    );
                    self.indent_level -= 1;
                    if let Some(else_blk) = &ir_if.else_block {
                        self.write_indent();
                        self.writeln("} else {");
                        self.indent_level += 1;
                        self.emit_match_arm_result_from_stmts(
                            &else_blk.statements,
                            0,
                            result_var,
                            result_type,
                        );
                        self.indent_level -= 1;
                    }
                    self.write_indent();
                    self.writeln("}");
                } else {
                    self.generate_stmt(&stmts[idx]);
                    self.emit_match_arm_result_from_stmts(stmts, idx + 1, result_var, result_type);
                }
            }
            IRStmt::UnsafeBlock(unsafe_blk) => {
                self.write_indent();
                self.writeln("{");
                self.indent_level += 1;
                if is_last {
                    self.emit_match_arm_result_from_stmts(
                        &unsafe_blk.body.statements,
                        0,
                        result_var,
                        result_type,
                    );
                } else {
                    self.generate_block(&unsafe_blk.body);
                    self.emit_match_arm_result_from_stmts(stmts, idx + 1, result_var, result_type);
                }
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
            }
            _ => {
                self.generate_stmt(&stmts[idx]);
                self.emit_match_arm_result_from_stmts(stmts, idx + 1, result_var, result_type);
            }
        }
    }
}

fn collect_slice_types_impl(
    program: &IRProgram,
    slice_types: &mut std::collections::HashSet<String>,
) {
    // Collect slice types from function parameters, return types, and statements
    for function in &program.functions {
        // Check return type
        if let Some(ref ret_ty) = function.return_type {
            collect_slice_types_from_type(ret_ty, slice_types);
        }
        // Check parameters
        for param in &function.params {
            collect_slice_types_from_type(&param.ty, slice_types);
        }
        // Check function body (variables, etc.)
        for block in &function.blocks {
            for stmt in &block.statements {
                collect_slice_types_from_stmt(stmt, slice_types);
            }
        }
    }
    // Check struct fields
    for s in &program.structs {
        for field in &s.fields {
            collect_slice_types_from_type(&field.ty, slice_types);
        }
    }
}

fn collect_slice_types_from_type(ty: &Type, slice_types: &mut std::collections::HashSet<String>) {
    match ty {
        Type::Void
        | Type::Int
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
        | Type::String => {}
        Type::Slice { inner } => {
            let slice_type_name = format!(
                "ion_slice_{}",
                mangle_type_name(&type_to_c_impl(inner), &[])
            );
            slice_types.insert(slice_type_name);
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::Array { inner, .. } => {
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::Ref { inner, .. } => {
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::RawPtr { inner } => {
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::Box { inner } => {
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::Vec { elem_type } => {
            collect_slice_types_from_type(elem_type, slice_types);
        }
        Type::Channel { elem_type } => {
            collect_slice_types_from_type(elem_type, slice_types);
        }
        Type::Sender { elem_type } => {
            collect_slice_types_from_type(elem_type, slice_types);
        }
        Type::Receiver { elem_type } => {
            collect_slice_types_from_type(elem_type, slice_types);
        }
        Type::Tuple { elements } => {
            for elem in elements {
                collect_slice_types_from_type(elem, slice_types);
            }
        }
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                collect_slice_types_from_type(param, slice_types);
            }
            collect_slice_types_from_type(return_type, slice_types);
        }
        Type::Struct(_) | Type::Enum(_) | Type::Generic { .. } => {
            // User-defined types - would need to check fields/variants
        }
    }
}

fn collect_slice_types_from_stmt(
    stmt: &IRStmt,
    slice_types: &mut std::collections::HashSet<String>,
) {
    match stmt {
        IRStmt::Let(let_stmt) => {
            collect_slice_types_from_type(&let_stmt.ty, slice_types);
            if let Some(ref init) = let_stmt.init {
                collect_slice_types_from_expr(init, slice_types);
            }
        }
        IRStmt::Return(ret) => {
            if let Some(ref value) = ret.value {
                collect_slice_types_from_expr(value, slice_types);
            }
        }
        IRStmt::Break | IRStmt::Continue => {}
        IRStmt::Expr(expr) => {
            collect_slice_types_from_expr(expr, slice_types);
        }
        IRStmt::Defer(expr) => {
            collect_slice_types_from_expr(expr, slice_types);
        }
        IRStmt::Spawn(spawn) => {
            for stmt in &spawn.body.statements {
                collect_slice_types_from_stmt(stmt, slice_types);
            }
        }
        IRStmt::If(ir_if) => {
            collect_slice_types_from_expr(&ir_if.cond, slice_types);
            for stmt in &ir_if.then_block.statements {
                collect_slice_types_from_stmt(stmt, slice_types);
            }
            if let Some(ref else_block) = ir_if.else_block {
                for stmt in &else_block.statements {
                    collect_slice_types_from_stmt(stmt, slice_types);
                }
            }
        }
        IRStmt::While(ir_while) => {
            collect_slice_types_from_expr(&ir_while.cond, slice_types);
            for stmt in &ir_while.body.statements {
                collect_slice_types_from_stmt(stmt, slice_types);
            }
            if let Some(ref step) = ir_while.step {
                for stmt in &step.statements {
                    collect_slice_types_from_stmt(stmt, slice_types);
                }
            }
        }
        IRStmt::UnsafeBlock(unsafe_block) => {
            for stmt in &unsafe_block.body.statements {
                collect_slice_types_from_stmt(stmt, slice_types);
            }
        }
    }
}

fn collect_slice_types_from_expr(
    expr: &IREexpr,
    slice_types: &mut std::collections::HashSet<String>,
) {
    match expr {
        IREexpr::Lit(_)
        | IREexpr::IntLimit { .. }
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::Var(_)
        | IREexpr::StringLit(_) => {}
        IREexpr::AddressOf { inner, .. } => {
            collect_slice_types_from_expr(inner, slice_types);
        }
        IREexpr::BinOp { left, right, .. } => {
            collect_slice_types_from_expr(left, slice_types);
            collect_slice_types_from_expr(right, slice_types);
        }
        IREexpr::UnOp { operand, .. } => {
            collect_slice_types_from_expr(operand, slice_types);
        }
        IREexpr::Send { channel, value, .. } => {
            collect_slice_types_from_expr(channel, slice_types);
            collect_slice_types_from_expr(value, slice_types);
        }
        IREexpr::Recv { channel, .. } => {
            collect_slice_types_from_expr(channel, slice_types);
        }
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                collect_slice_types_from_expr(&field.value, slice_types);
            }
        }
        IREexpr::FieldAccess { base, .. } => {
            collect_slice_types_from_expr(base, slice_types);
        }
        IREexpr::EnumLit { args, .. } => {
            for arg in args {
                collect_slice_types_from_expr(arg, slice_types);
            }
        }
        IREexpr::Match { expr, .. } => {
            collect_slice_types_from_expr(expr, slice_types);
        }
        IREexpr::Call {
            args, return_type, ..
        } => {
            if let Some(ret_ty) = return_type {
                collect_slice_types_from_type(ret_ty, slice_types);
            }
            for arg in args {
                collect_slice_types_from_expr(arg, slice_types);
            }
        }
        IREexpr::ArrayLiteral { elements, repeat } => {
            for elem in elements {
                collect_slice_types_from_expr(elem, slice_types);
            }
            if let Some((value_expr, _)) = repeat {
                collect_slice_types_from_expr(value_expr, slice_types);
            }
        }
        IREexpr::Index {
            target,
            index,
            target_type: _,
        } => {
            collect_slice_types_from_expr(target, slice_types);
            collect_slice_types_from_expr(index, slice_types);
        }
        IREexpr::Cast { expr, .. } => {
            collect_slice_types_from_expr(expr, slice_types);
        }
        IREexpr::Assign { target: _, value } => {
            collect_slice_types_from_expr(value, slice_types);
        }
        IREexpr::AssignIndex {
            target,
            index,
            value,
        } => {
            collect_slice_types_from_expr(target, slice_types);
            collect_slice_types_from_expr(index, slice_types);
            collect_slice_types_from_expr(value, slice_types);
        }
        IREexpr::TupleLit { .. } => {}
        IREexpr::FnLiteral(lit) => {
            for param in &lit.params {
                collect_slice_types_from_type(&param.ty, slice_types);
            }
            if let Some(ret) = &lit.return_type {
                collect_slice_types_from_type(ret, slice_types);
            }
            for stmt in &lit.body.statements {
                collect_slice_types_from_stmt(stmt, slice_types);
            }
        }
    }
}

fn collect_tuple_types_impl(
    program: &IRProgram,
    tuple_types: &mut std::collections::HashMap<String, Vec<Type>>,
) {
    for function in &program.functions {
        if let Some(ref ret_ty) = function.return_type {
            collect_tuple_types_from_type(ret_ty, tuple_types);
        }
        for param in &function.params {
            collect_tuple_types_from_type(&param.ty, tuple_types);
        }
        for block in &function.blocks {
            for stmt in &block.statements {
                collect_tuple_types_from_stmt(stmt, tuple_types);
            }
        }
    }
    for s in &program.structs {
        for field in &s.fields {
            collect_tuple_types_from_type(&field.ty, tuple_types);
        }
    }
}

fn collect_tuple_types_from_type(
    ty: &Type,
    tuple_types: &mut std::collections::HashMap<String, Vec<Type>>,
) {
    if let Type::Tuple { elements } = ty {
        tuple_types.insert(tuple_type_name(elements), elements.clone());
        for elem in elements {
            collect_tuple_types_from_type(elem, tuple_types);
        }
        return;
    }
    match ty {
        Type::Ref { inner, .. } => collect_tuple_types_from_type(inner, tuple_types),
        Type::RawPtr { inner } => collect_tuple_types_from_type(inner, tuple_types),
        Type::Box { inner } => collect_tuple_types_from_type(inner, tuple_types),
        Type::Vec { elem_type } => collect_tuple_types_from_type(elem_type, tuple_types),
        Type::Channel { elem_type } => collect_tuple_types_from_type(elem_type, tuple_types),
        Type::Sender { elem_type } => collect_tuple_types_from_type(elem_type, tuple_types),
        Type::Receiver { elem_type } => collect_tuple_types_from_type(elem_type, tuple_types),
        Type::Array { inner, .. } => collect_tuple_types_from_type(inner, tuple_types),
        Type::Slice { inner } => collect_tuple_types_from_type(inner, tuple_types),
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                collect_tuple_types_from_type(param, tuple_types);
            }
            collect_tuple_types_from_type(return_type, tuple_types);
        }
        _ => {}
    }
}

fn collect_tuple_types_from_stmt(
    stmt: &IRStmt,
    tuple_types: &mut std::collections::HashMap<String, Vec<Type>>,
) {
    match stmt {
        IRStmt::Let(let_stmt) => {
            collect_tuple_types_from_type(&let_stmt.ty, tuple_types);
            if let Some(ref init) = let_stmt.init {
                collect_tuple_types_from_expr(init, tuple_types);
            }
        }
        IRStmt::Return(ret) => {
            if let Some(ref value) = ret.value {
                collect_tuple_types_from_expr(value, tuple_types);
            }
        }
        IRStmt::Break | IRStmt::Continue => {}
        IRStmt::Expr(expr) => collect_tuple_types_from_expr(expr, tuple_types),
        IRStmt::Defer(expr) => collect_tuple_types_from_expr(expr, tuple_types),
        IRStmt::Spawn(spawn) => {
            for stmt in &spawn.body.statements {
                collect_tuple_types_from_stmt(stmt, tuple_types);
            }
        }
        IRStmt::If(ir_if) => {
            collect_tuple_types_from_expr(&ir_if.cond, tuple_types);
            for stmt in &ir_if.then_block.statements {
                collect_tuple_types_from_stmt(stmt, tuple_types);
            }
            if let Some(ref else_block) = ir_if.else_block {
                for stmt in &else_block.statements {
                    collect_tuple_types_from_stmt(stmt, tuple_types);
                }
            }
        }
        IRStmt::While(ir_while) => {
            collect_tuple_types_from_expr(&ir_while.cond, tuple_types);
            for stmt in &ir_while.body.statements {
                collect_tuple_types_from_stmt(stmt, tuple_types);
            }
            if let Some(ref step) = ir_while.step {
                for stmt in &step.statements {
                    collect_tuple_types_from_stmt(stmt, tuple_types);
                }
            }
        }
        IRStmt::UnsafeBlock(unsafe_block) => {
            for stmt in &unsafe_block.body.statements {
                collect_tuple_types_from_stmt(stmt, tuple_types);
            }
        }
    }
}

fn collect_tuple_types_from_expr(
    expr: &IREexpr,
    tuple_types: &mut std::collections::HashMap<String, Vec<Type>>,
) {
    match expr {
        IREexpr::AddressOf { inner, .. } => collect_tuple_types_from_expr(inner, tuple_types),
        IREexpr::BinOp { left, right, .. } => {
            collect_tuple_types_from_expr(left, tuple_types);
            collect_tuple_types_from_expr(right, tuple_types);
        }
        IREexpr::UnOp { operand, .. } => collect_tuple_types_from_expr(operand, tuple_types),
        IREexpr::Send { channel, value, .. } => {
            collect_tuple_types_from_expr(channel, tuple_types);
            collect_tuple_types_from_expr(value, tuple_types);
        }
        IREexpr::Recv { channel, .. } => collect_tuple_types_from_expr(channel, tuple_types),
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                collect_tuple_types_from_expr(&field.value, tuple_types);
            }
        }
        IREexpr::FieldAccess { base, .. } => collect_tuple_types_from_expr(base, tuple_types),
        IREexpr::EnumLit {
            args, named_fields, ..
        } => {
            for arg in args {
                collect_tuple_types_from_expr(arg, tuple_types);
            }
            if let Some(named_fields) = named_fields {
                for (_, value) in named_fields {
                    collect_tuple_types_from_expr(value, tuple_types);
                }
            }
        }
        IREexpr::Match { expr, arms, .. } => {
            collect_tuple_types_from_expr(expr, tuple_types);
            for arm in arms {
                if let Some(ref guard) = arm.guard {
                    collect_tuple_types_from_expr(guard, tuple_types);
                }
                for stmt in &arm.body.statements {
                    collect_tuple_types_from_stmt(stmt, tuple_types);
                }
            }
        }
        IREexpr::Call {
            return_type, args, ..
        } => {
            if let Some(ret_ty) = return_type {
                collect_tuple_types_from_type(ret_ty, tuple_types);
            }
            for arg in args {
                collect_tuple_types_from_expr(arg, tuple_types);
            }
        }
        IREexpr::TupleLit {
            elements,
            elem_types,
        } => {
            tuple_types.insert(tuple_type_name(elem_types), elem_types.clone());
            for elem in elements {
                collect_tuple_types_from_expr(elem, tuple_types);
            }
        }
        IREexpr::ArrayLiteral { elements, repeat } => {
            for elem in elements {
                collect_tuple_types_from_expr(elem, tuple_types);
            }
            if let Some((value_expr, _)) = repeat {
                collect_tuple_types_from_expr(value_expr, tuple_types);
            }
        }
        IREexpr::Index { target, index, .. } => {
            collect_tuple_types_from_expr(target, tuple_types);
            collect_tuple_types_from_expr(index, tuple_types);
        }
        IREexpr::Cast {
            expr, target_type, ..
        } => {
            collect_tuple_types_from_type(target_type, tuple_types);
            collect_tuple_types_from_expr(expr, tuple_types);
        }
        IREexpr::Assign { value, .. } => collect_tuple_types_from_expr(value, tuple_types),
        IREexpr::AssignIndex {
            target,
            index,
            value,
        } => {
            collect_tuple_types_from_expr(target, tuple_types);
            collect_tuple_types_from_expr(index, tuple_types);
            collect_tuple_types_from_expr(value, tuple_types);
        }
        IREexpr::Lit(_)
        | IREexpr::IntLimit { .. }
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::Var(_)
        | IREexpr::StringLit(_) => {}
        IREexpr::FnLiteral(lit) => {
            for param in &lit.params {
                collect_tuple_types_from_type(&param.ty, tuple_types);
            }
            if let Some(ret) = &lit.return_type {
                collect_tuple_types_from_type(ret, tuple_types);
            }
            for stmt in &lit.body.statements {
                collect_tuple_types_from_stmt(stmt, tuple_types);
            }
        }
    }
}

fn collect_vec_types_impl(program: &IRProgram, vec_types: &mut std::collections::HashSet<String>) {
    // Collect Vec types from function parameters, return types, and struct fields
    for function in &program.functions {
        // Check return type
        if let Some(ref ret_ty) = function.return_type {
            collect_vec_types_from_type(ret_ty, vec_types);
        }
        // Check parameters
        for param in &function.params {
            collect_vec_types_from_type(&param.ty, vec_types);
        }
        // Check function body (variables, etc.)
        for block in &function.blocks {
            for stmt in &block.statements {
                collect_vec_types_from_stmt(stmt, vec_types);
            }
        }
    }
    // Check struct fields
    for s in &program.structs {
        for field in &s.fields {
            collect_vec_types_from_type(&field.ty, vec_types);
        }
    }
}

fn collect_vec_types_from_type(ty: &Type, vec_types: &mut std::collections::HashSet<String>) {
    match ty {
        Type::Vec { elem_type } => {
            let vec_name = mangle_type_name("Vec", std::slice::from_ref(elem_type));
            vec_types.insert(vec_name);
            // Recursively collect nested Vec types
            collect_vec_types_from_type(elem_type, vec_types);
        }
        Type::Generic { name, params } if name == "Vec" && params.len() == 1 => {
            let vec_name = mangle_type_name("Vec", params);
            vec_types.insert(vec_name);
            // Recursively collect nested Vec types
            for param in params {
                collect_vec_types_from_type(param, vec_types);
            }
        }
        Type::Ref { inner, .. } => {
            collect_vec_types_from_type(inner, vec_types);
        }
        Type::Box { inner } => {
            collect_vec_types_from_type(inner, vec_types);
        }
        Type::Channel { elem_type } => {
            collect_vec_types_from_type(elem_type, vec_types);
        }
        Type::Tuple { elements } => {
            for elem in elements {
                collect_vec_types_from_type(elem, vec_types);
            }
        }
        _ => {}
    }
}

fn collect_vec_types_from_stmt(stmt: &IRStmt, vec_types: &mut std::collections::HashSet<String>) {
    match stmt {
        IRStmt::Let(let_stmt) => {
            collect_vec_types_from_type(&let_stmt.ty, vec_types);
            if let Some(ref init) = let_stmt.init {
                collect_vec_types_from_expr(init, vec_types);
            }
        }
        IRStmt::Return(ret) => {
            if let Some(ref value) = ret.value {
                collect_vec_types_from_expr(value, vec_types);
            }
        }
        IRStmt::Break | IRStmt::Continue => {}
        IRStmt::Expr(expr) => {
            collect_vec_types_from_expr(expr, vec_types);
        }
        IRStmt::If(ir_if) => {
            collect_vec_types_from_expr(&ir_if.cond, vec_types);
            for stmt in &ir_if.then_block.statements {
                collect_vec_types_from_stmt(stmt, vec_types);
            }
            if let Some(ref else_block) = ir_if.else_block {
                for stmt in &else_block.statements {
                    collect_vec_types_from_stmt(stmt, vec_types);
                }
            }
        }
        IRStmt::While(ir_while) => {
            collect_vec_types_from_expr(&ir_while.cond, vec_types);
            for stmt in &ir_while.body.statements {
                collect_vec_types_from_stmt(stmt, vec_types);
            }
            if let Some(ref step) = ir_while.step {
                for stmt in &step.statements {
                    collect_vec_types_from_stmt(stmt, vec_types);
                }
            }
        }
        IRStmt::Spawn(spawn) => {
            for stmt in &spawn.body.statements {
                collect_vec_types_from_stmt(stmt, vec_types);
            }
        }
        IRStmt::Defer(_) => {}
        IRStmt::UnsafeBlock(unsafe_block) => {
            for stmt in &unsafe_block.body.statements {
                collect_vec_types_from_stmt(stmt, vec_types);
            }
        }
    }
}

fn collect_vec_types_from_expr(expr: &IREexpr, vec_types: &mut std::collections::HashSet<String>) {
    match expr {
        IREexpr::Var(_)
        | IREexpr::Lit(_)
        | IREexpr::IntLimit { .. }
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::StringLit(_) => {}
        IREexpr::BinOp { left, right, .. } => {
            collect_vec_types_from_expr(left, vec_types);
            collect_vec_types_from_expr(right, vec_types);
        }
        IREexpr::UnOp { operand, .. } => {
            collect_vec_types_from_expr(operand, vec_types);
        }
        IREexpr::AddressOf { inner, .. } => {
            collect_vec_types_from_expr(inner, vec_types);
        }
        IREexpr::Send { channel, value, .. } => {
            collect_vec_types_from_expr(channel, vec_types);
            collect_vec_types_from_expr(value, vec_types);
        }
        IREexpr::Recv {
            channel, elem_type, ..
        } => {
            collect_vec_types_from_expr(channel, vec_types);
            // Extract Vec types from the element type
            collect_vec_types_from_type(elem_type, vec_types);
        }
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                collect_vec_types_from_expr(&field.value, vec_types);
            }
        }
        IREexpr::FieldAccess { base, .. } => {
            collect_vec_types_from_expr(base, vec_types);
        }
        IREexpr::EnumLit { args, .. } => {
            for arg in args {
                collect_vec_types_from_expr(arg, vec_types);
            }
        }
        IREexpr::Match { expr, .. } => {
            collect_vec_types_from_expr(expr, vec_types);
        }
        IREexpr::Call {
            args, return_type, ..
        } => {
            // Extract Vec types from the return type - this uses vec_types directly
            if let Some(ret_ty) = return_type {
                collect_vec_types_from_type(ret_ty, vec_types);
            }
            for arg in args {
                collect_vec_types_from_expr(arg, vec_types);
            }
        }
        IREexpr::TupleLit { elements, .. } => {
            for elem in elements {
                collect_vec_types_from_expr(elem, vec_types);
            }
        }
        IREexpr::ArrayLiteral { elements, repeat } => {
            for elem in elements {
                collect_vec_types_from_expr(elem, vec_types);
            }
            if let Some((value_expr, _)) = repeat {
                collect_vec_types_from_expr(value_expr, vec_types);
            }
        }
        IREexpr::Index {
            target,
            index,
            target_type: _,
        } => {
            collect_vec_types_from_expr(target, vec_types);
            collect_vec_types_from_expr(index, vec_types);
        }
        IREexpr::Cast { expr, .. } => {
            collect_vec_types_from_expr(expr, vec_types);
        }
        IREexpr::Assign { target: _, value } => {
            collect_vec_types_from_expr(value, vec_types);
        }
        IREexpr::AssignIndex {
            target,
            index,
            value,
        } => {
            collect_vec_types_from_expr(target, vec_types);
            collect_vec_types_from_expr(index, vec_types);
            collect_vec_types_from_expr(value, vec_types);
        }
        IREexpr::FnLiteral(lit) => {
            for param in &lit.params {
                collect_vec_types_from_type(&param.ty, vec_types);
            }
            if let Some(ret) = &lit.return_type {
                collect_vec_types_from_type(ret, vec_types);
            }
            for stmt in &lit.body.statements {
                collect_vec_types_from_stmt(stmt, vec_types);
            }
        }
    }
}

impl Codegen {
    fn insert_spawn_fn_literal_forward_decls(&mut self, insert_at: usize) {
        if self.spawn_forward_decls.is_empty() && self.fn_literal_forward_decls.is_empty() {
            return;
        }
        let mut forward_decls = self.spawn_forward_decls.clone();
        forward_decls.push_str(&self.fn_literal_forward_decls);
        forward_decls.push('\n');
        self.output.insert_str(insert_at, &forward_decls);
    }

    fn emit_vec_slice_tuple_typedefs(&mut self, program: &IRProgram) {
        let mut vec_types = std::collections::HashSet::new();
        collect_vec_types_impl(program, &mut vec_types);
        for vec_type_name in &vec_types {
            self.generate_vec_struct(vec_type_name);
        }

        let mut slice_types = std::collections::HashSet::new();
        collect_slice_types_impl(program, &mut slice_types);
        for slice_type_name in &slice_types {
            self.generate_slice_struct(slice_type_name);
        }

        let mut tuple_types: std::collections::HashMap<String, Vec<Type>> =
            std::collections::HashMap::new();
        collect_tuple_types_impl(program, &mut tuple_types);
        for (tuple_name, elements) in &tuple_types {
            self.generate_tuple_struct(tuple_name, elements);
        }
    }

    fn generate_vec_struct(&mut self, vec_type_name: &str) {
        // Generate Vec struct definition matching ion_vec_t layout
        // Format: typedef struct Vec_T { void* data; size_t len; size_t capacity; size_t elem_size; } Vec_T;
        self.write(&format!("typedef struct {} {{\n", vec_type_name));
        self.indent_level += 1;
        self.write_indent();
        self.writeln("void* data;");
        self.write_indent();
        self.writeln("size_t len;");
        self.write_indent();
        self.writeln("size_t capacity;");
        self.write_indent();
        self.writeln("size_t elem_size;");
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", vec_type_name));
        self.writeln("");
    }

    fn generate_slice_struct(&mut self, slice_type_name: &str) {
        // Generate slice struct definition: typedef struct ion_slice_T { T* data; int len; } ion_slice_T;
        // Extract element type from slice type name (ion_slice_int -> int)
        let elem_type_name = slice_type_name.strip_prefix("ion_slice_").unwrap_or("int");

        // Map element type name back to C type
        let elem_c_type = match elem_type_name {
            "int" => "int",
            "u8" | "uint8_t" => "uint8_t",
            _ => elem_type_name, // Assume it's already a valid C type
        };

        self.write(&format!("typedef struct {} {{\n", slice_type_name));
        self.indent_level += 1;
        self.write_indent();
        self.writeln(&format!("{}* data;", elem_c_type));
        self.write_indent();
        self.writeln("int len;");
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", slice_type_name));
        self.writeln("");
    }

    fn generate_tuple_struct(&mut self, tuple_name: &str, elements: &[Type]) {
        if self.generated_types.contains_key(tuple_name) {
            return;
        }
        self.write(&format!("typedef struct {} {{\n", tuple_name));
        self.indent_level += 1;
        for (i, elem) in elements.iter().enumerate() {
            self.write_indent();
            self.writeln(&format!("{} f{};", self.type_to_c(elem), i));
        }
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", tuple_name));
        self.writeln("");
        self.generated_types.insert(tuple_name.to_string(), true);
    }

    fn generate_monomorphized_struct(&mut self, decl: &StructDecl, params: &[Type]) {
        // Generate monomorphized struct: Point<int> -> Point_int
        let monomorphized_name = mangle_type_name(&decl.name, params);

        // Create substitution map: generic param name -> concrete type
        let mut substitutions: HashMap<String, &Type> = HashMap::new();
        for (i, param_name) in decl.generics.iter().enumerate() {
            if i < params.len() {
                substitutions.insert(param_name.clone(), &params[i]);
            }
        }

        self.write(&format!("typedef struct {} {{\n", monomorphized_name));
        self.indent_level += 1;
        for field in &decl.fields {
            self.write_indent();
            let field_ty = substitute_generic_types(&field.ty, &substitutions);
            // Handle arrays specially: in struct fields, arrays must be declared as "type name[size];"
            // not "type[size] name;" which is invalid C syntax
            let field_decl = match &field_ty {
                Type::Array { inner, size } => {
                    let base_type = self.type_to_c(inner);
                    format!("{} {}[{}]", base_type, field.name, size)
                }
                _ => {
                    format!("{} {}", self.type_to_c(&field_ty), field.name)
                }
            };
            self.writeln(&format!("{};", field_decl));
        }
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", monomorphized_name));
        self.writeln("");
    }

    fn generate_monomorphized_enum(&mut self, decl: &EnumDecl, params: &[Type]) {
        // Generate monomorphized enum: Option<int> -> Option_int
        let monomorphized_name = mangle_type_name(&decl.name, params);

        // Create substitution map: generic param name -> concrete type
        let mut substitutions: HashMap<String, &Type> = HashMap::new();
        for (i, param_name) in decl.generics.iter().enumerate() {
            if i < params.len() {
                substitutions.insert(param_name.clone(), &params[i]);
            }
        }

        self.write(&format!("typedef struct {} {{\n", monomorphized_name));
        self.indent_level += 1;
        self.write_indent();
        self.writeln("int tag;");
        self.write_indent();
        self.write("union {\n");
        self.indent_level += 1;

        // Generate union members for each variant with payloads
        for (variant_idx, variant) in decl.variants.iter().enumerate() {
            if !variant.payload_types.is_empty() {
                self.write_indent();
                self.write("struct {\n");
                self.indent_level += 1;
                for (i, payload_ty) in variant.payload_types.iter().enumerate() {
                    self.write_indent();
                    let substituted_ty = substitute_generic_types(payload_ty, &substitutions);
                    self.write(&format!("{} arg{};", self.type_to_c(&substituted_ty), i));
                    self.writeln("");
                }
                self.indent_level -= 1;
                self.write_indent();
                self.write(&format!("}} variant_{};\n", variant_idx));
            }
        }

        self.indent_level -= 1;
        self.write_indent();
        self.writeln("} data;");
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", monomorphized_name));
        self.writeln("");
    }
}

fn substitute_generic_types(ty: &Type, substitutions: &HashMap<String, &Type>) -> Type {
    match ty {
        Type::Struct(name) => {
            // Check if this is a generic parameter name
            if let Some(substituted) = substitutions.get(name) {
                (*substituted).clone()
            } else {
                ty.clone()
            }
        }
        Type::Enum(name) => {
            // Check if this is a generic parameter name
            if let Some(substituted) = substitutions.get(name) {
                (*substituted).clone()
            } else {
                ty.clone()
            }
        }
        Type::Ref { inner, mutable } => Type::Ref {
            inner: Box::new(substitute_generic_types(inner, substitutions)),
            mutable: *mutable,
        },
        Type::Box { inner } => Type::Box {
            inner: Box::new(substitute_generic_types(inner, substitutions)),
        },
        Type::Vec { elem_type } => Type::Vec {
            elem_type: Box::new(substitute_generic_types(elem_type, substitutions)),
        },
        Type::Channel { elem_type } => Type::Channel {
            elem_type: Box::new(substitute_generic_types(elem_type, substitutions)),
        },
        Type::Generic { name, params } => {
            // First check if the generic name itself is a generic parameter (e.g., T in Option<T>)
            if let Some(substituted) = substitutions.get(name) {
                // If the generic name is a parameter, substitute it entirely
                // But we still need to recursively substitute any nested generics
                substitute_generic_types(substituted, substitutions)
            } else {
                // Otherwise, recursively substitute parameters
                let substituted_params: Vec<Type> = params
                    .iter()
                    .map(|p| substitute_generic_types(p, substitutions))
                    .collect();
                Type::Generic {
                    name: name.clone(),
                    params: substituted_params,
                }
            }
        }
        _ => ty.clone(),
    }
}

fn collect_generic_instantiations(
    program: &IRProgram,
    instantiations: &mut std::collections::HashMap<String, (String, Vec<Type>)>,
) {
    // Collect from function parameters, return types, and struct fields
    for function in &program.functions {
        if let Some(ref ret_ty) = function.return_type {
            collect_generic_from_type(ret_ty, instantiations);
        }
        for param in &function.params {
            collect_generic_from_type(&param.ty, instantiations);
        }
        for block in &function.blocks {
            for stmt in &block.statements {
                collect_generic_from_stmt(stmt, instantiations);
            }
        }
    }
    for s in &program.structs {
        for field in &s.fields {
            collect_generic_from_type(&field.ty, instantiations);
        }
    }
}

fn type_params_are_concrete(params: &[Type]) -> bool {
    params.iter().all(|param| match param {
        Type::Struct(_) | Type::Enum(_) => true,
        Type::Generic { params: nested, .. } => type_params_are_concrete(nested),
        Type::Ref { inner, .. }
        | Type::Box { inner }
        | Type::Vec { elem_type: inner }
        | Type::Channel { elem_type: inner }
        | Type::Array { inner, .. }
        | Type::Slice { inner }
        | Type::Sender { elem_type: inner }
        | Type::Receiver { elem_type: inner }
        | Type::RawPtr { inner } => type_params_are_concrete(std::slice::from_ref(inner)),
        Type::Tuple { elements } => type_params_are_concrete(elements),
        Type::Fn {
            params,
            return_type,
        } => {
            type_params_are_concrete(params)
                && type_params_are_concrete(std::slice::from_ref(return_type))
        }
        _ => true,
    })
}

fn collect_generic_from_type(
    ty: &Type,
    instantiations: &mut std::collections::HashMap<String, (String, Vec<Type>)>,
) {
    match ty {
        Type::Generic { name, params } => {
            if type_params_are_concrete(params) {
                if name == "Vec" && params.len() == 1 {
                    let key = mangle_type_name(name, params);
                    instantiations.insert(key, (name.clone(), params.clone()));
                }
                if name != "Box" && name != "Vec" {
                    let key = mangle_type_name(name, params);
                    instantiations.insert(key, (name.clone(), params.clone()));
                }
            }
            for param in params {
                collect_generic_from_type(param, instantiations);
            }
        }
        Type::Ref { inner, .. } => collect_generic_from_type(inner, instantiations),
        Type::Box { inner } => collect_generic_from_type(inner, instantiations),
        Type::Vec { elem_type } => {
            // Also collect Vec types when encountered as Vec { elem_type }
            let key = mangle_type_name("Vec", std::slice::from_ref(elem_type));
            instantiations.insert(key, ("Vec".to_string(), vec![(**elem_type).clone()]));

            // Also collect Option<T> for Vec<T> since Vec::pop and Vec::get return Option<T>
            let elem_type_clone = (**elem_type).clone();
            let option_key = mangle_type_name("Option", std::slice::from_ref(&elem_type_clone));
            instantiations
                .entry(option_key)
                .or_insert_with(|| ("Option".to_string(), vec![elem_type_clone]));

            collect_generic_from_type(elem_type, instantiations);
        }
        Type::Channel { elem_type } => collect_generic_from_type(elem_type, instantiations),
        Type::Array { inner, .. } => collect_generic_from_type(inner, instantiations),
        Type::Slice { inner } => collect_generic_from_type(inner, instantiations),
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                collect_generic_from_type(param, instantiations);
            }
            collect_generic_from_type(return_type, instantiations);
        }
        _ => {}
    }
}

fn collect_generic_from_stmt(
    stmt: &IRStmt,
    instantiations: &mut std::collections::HashMap<String, (String, Vec<Type>)>,
) {
    match stmt {
        IRStmt::Let(let_stmt) => {
            collect_generic_from_type(&let_stmt.ty, instantiations);
            if let Some(ref init) = let_stmt.init {
                collect_generic_from_expr(init, instantiations);
            }
        }
        IRStmt::Return(ret) => {
            if let Some(ref value) = ret.value {
                collect_generic_from_expr(value, instantiations);
            }
        }
        IRStmt::Break | IRStmt::Continue => {}
        IRStmt::Expr(expr) => collect_generic_from_expr(expr, instantiations),
        IRStmt::If(ir_if) => {
            collect_generic_from_expr(&ir_if.cond, instantiations);
            for stmt in &ir_if.then_block.statements {
                collect_generic_from_stmt(stmt, instantiations);
            }
            if let Some(ref else_block) = ir_if.else_block {
                for stmt in &else_block.statements {
                    collect_generic_from_stmt(stmt, instantiations);
                }
            }
        }
        IRStmt::While(ir_while) => {
            collect_generic_from_expr(&ir_while.cond, instantiations);
            for stmt in &ir_while.body.statements {
                collect_generic_from_stmt(stmt, instantiations);
            }
            if let Some(ref step) = ir_while.step {
                for stmt in &step.statements {
                    collect_generic_from_stmt(stmt, instantiations);
                }
            }
        }
        IRStmt::Spawn(spawn) => {
            for stmt in &spawn.body.statements {
                collect_generic_from_stmt(stmt, instantiations);
            }
        }
        IRStmt::Defer(_) => {}
        IRStmt::UnsafeBlock(unsafe_block) => {
            for stmt in &unsafe_block.body.statements {
                collect_generic_from_stmt(stmt, instantiations);
            }
        }
    }
}

fn collect_generic_from_expr(
    expr: &IREexpr,
    instantiations: &mut std::collections::HashMap<String, (String, Vec<Type>)>,
) {
    match expr {
        IREexpr::Var(_)
        | IREexpr::Lit(_)
        | IREexpr::IntLimit { .. }
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::StringLit(_) => {}
        IREexpr::BinOp { left, right, .. } => {
            collect_generic_from_expr(left, instantiations);
            collect_generic_from_expr(right, instantiations);
        }
        IREexpr::UnOp { operand, .. } => {
            collect_generic_from_expr(operand, instantiations);
        }
        IREexpr::AddressOf { inner, .. } => {
            collect_generic_from_expr(inner, instantiations);
        }
        IREexpr::Send { channel, value, .. } => {
            collect_generic_from_expr(channel, instantiations);
            collect_generic_from_expr(value, instantiations);
        }
        IREexpr::Recv {
            channel, elem_type, ..
        } => {
            collect_generic_from_expr(channel, instantiations);
            collect_generic_from_type(elem_type, instantiations);
        }
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                collect_generic_from_expr(&field.value, instantiations);
            }
        }
        IREexpr::FieldAccess { base, .. } => {
            collect_generic_from_expr(base, instantiations);
        }
        IREexpr::EnumLit { args, .. } => {
            for arg in args {
                collect_generic_from_expr(arg, instantiations);
            }
        }
        IREexpr::Match { expr, arms, .. } => {
            collect_generic_from_expr(expr, instantiations);
            // Also collect Option types from Vec::pop/Vec::get calls in match expressions
            if let IREexpr::Call {
                callee,
                return_type,
                ..
            } = expr.as_ref()
            {
                let is_vec_option_call = callee == "Vec::pop"
                    || callee == "Vec::get"
                    || callee == "Vec::get_ref"
                    || callee == "METHOD::pop"
                    || callee == "METHOD::get";

                if is_vec_option_call {
                    // First try to get Option type from return_type if available
                    if let Some(Type::Generic { name, params }) = return_type
                        && name == "Option"
                        && params.len() == 1
                    {
                        let option_key = mangle_type_name("Option", params);
                        instantiations
                            .entry(option_key)
                            .or_insert_with(|| ("Option".to_string(), params.clone()));
                    } else if callee == "Vec::get_ref" {
                        if let Some(elem_type) =
                            instantiations
                                .iter()
                                .find_map(|(_mono_name, (base, params))| {
                                    if base == "Vec" && params.len() == 1 {
                                        Some(params[0].clone())
                                    } else {
                                        None
                                    }
                                })
                        {
                            let ref_elem = Type::Ref {
                                inner: Box::new(elem_type.clone()),
                                mutable: false,
                            };
                            let option_key =
                                mangle_type_name("Option", std::slice::from_ref(&ref_elem));
                            instantiations
                                .entry(option_key)
                                .or_insert_with(|| ("Option".to_string(), vec![ref_elem]));
                        }
                    } else {
                        // Otherwise, extract element type from Vec<T> in instantiations
                        if let Some(elem_type) =
                            instantiations
                                .iter()
                                .find_map(|(_mono_name, (base, params))| {
                                    if base == "Vec" && params.len() == 1 {
                                        Some(params[0].clone())
                                    } else {
                                        None
                                    }
                                })
                        {
                            // Add Option<T> to instantiations
                            let option_key =
                                mangle_type_name("Option", std::slice::from_ref(&elem_type));
                            instantiations
                                .entry(option_key)
                                .or_insert_with(|| ("Option".to_string(), vec![elem_type]));
                        }
                    }
                }
            }
            for arm in arms {
                for stmt in &arm.body.statements {
                    collect_generic_from_stmt(stmt, instantiations);
                }
            }
        }
        IREexpr::Call {
            args, return_type, ..
        } => {
            if let Some(ret_ty) = return_type {
                collect_generic_from_type(ret_ty, instantiations);
            }
            for arg in args {
                collect_generic_from_expr(arg, instantiations);
            }
        }
        IREexpr::TupleLit { elements, .. } => {
            for elem in elements {
                collect_generic_from_expr(elem, instantiations);
            }
        }
        IREexpr::ArrayLiteral { elements, repeat } => {
            for elem in elements {
                collect_generic_from_expr(elem, instantiations);
            }
            if let Some((value_expr, _)) = repeat {
                collect_generic_from_expr(value_expr, instantiations);
            }
        }
        IREexpr::Index {
            target,
            index,
            target_type: _,
        } => {
            collect_generic_from_expr(target, instantiations);
            collect_generic_from_expr(index, instantiations);
        }
        IREexpr::Cast { expr, .. } => {
            collect_generic_from_expr(expr, instantiations);
        }
        IREexpr::Assign { target: _, value } => {
            collect_generic_from_expr(value, instantiations);
        }
        IREexpr::AssignIndex {
            target,
            index,
            value,
        } => {
            collect_generic_from_expr(target, instantiations);
            collect_generic_from_expr(index, instantiations);
            collect_generic_from_expr(value, instantiations);
        }
        IREexpr::FnLiteral(lit) => {
            for param in &lit.params {
                collect_generic_from_type(&param.ty, instantiations);
            }
            if let Some(ret) = &lit.return_type {
                collect_generic_from_type(ret, instantiations);
            }
            for stmt in &lit.body.statements {
                collect_generic_from_stmt(stmt, instantiations);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fn_return_type_ret_val_decl() {
        let src = r#"fn make_inc() -> fn(int) -> int {
    return fn(x: int) -> int { return x + 1; };
}
fn main() -> int { return 0; }"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let make_inc = ir.functions.iter().find(|f| f.name == "make_inc").unwrap();
        assert!(
            matches!(make_inc.return_type, Some(Type::Fn { .. })),
            "expected fn return type, got {:?}",
            make_inc.return_type
        );
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("int (*ret_val)(int) = 0"),
            "expected fn pointer ret_val decl in:\n{c}"
        );
    }

    #[test]
    fn tuple_return_ret_val_decl() {
        let src = r#"struct Item { done: bool; }
fn pack(items: Vec<Item>) -> (Vec<Item>, int) {
    let mut open: int = 0;
    for item in items {
        if !item.done { open = open + 1; }
    }
    return (Vec::new(), open);
}
fn main() -> int { return 0; }"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("tuple_Vec_Item_int ret_val = (tuple_Vec_Item_int){0}"),
            "expected tuple ret_val init in:\n{c}"
        );
    }

    #[test]
    fn fn_literal_tuple_return_ret_val_decl() {
        let src = r#"fn main() -> int {
    let get = fn() -> (int, int) { return (1, 2); };
    let t: (int, int) = get();
    return t.0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("tuple_int_int ret_val = (tuple_int_int){0}"),
            "expected fn literal tuple ret_val init in:\n{c}"
        );
    }

    #[test]
    fn unused_channel_binding_silenced_on_return() {
        let src = r#"fn main() -> int {
    let ch: channel<int>;
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("(void)ch;"),
            "expected unused channel silence in:\n{c}"
        );
    }

    #[test]
    fn unused_binding_silenced_on_return() {
        let src = r#"fn main() -> int {
    let x: int = 1;
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("(void)x;"),
            "expected unused binding silence in:\n{c}"
        );
    }

    #[test]
    fn enum_literal_uses_compound_init_not_constructor() {
        let src = r#"enum Option {
    Some(int);
    None;
}

fn main() -> int {
    let x: Option = Option::Some(42);
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("(Option){ .tag = 0, .data = { .variant_0 = { .arg0 = 42 } } }"),
            "expected compound enum literal in:\n{c}"
        );
        assert!(
            !c.contains("_new("),
            "expected no enum constructor helpers in:\n{c}"
        );
        assert!(
            !c.contains("ION_MAYBE_UNUSED"),
            "expected no ION_MAYBE_UNUSED in:\n{c}"
        );
    }

    #[test]
    fn unused_fn_param_silenced_without_maybe_unused_attribute() {
        let src = r#"fn ignore(x: int) -> int {
    return 0;
}

fn main() -> int {
    return ignore(1);
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            !c.contains("ION_MAYBE_UNUSED"),
            "expected no ION_MAYBE_UNUSED in:\n{c}"
        );
        assert!(
            c.contains("(void)x;"),
            "expected unused param silence in:\n{c}"
        );
    }

    #[test]
    fn outer_return_drops_after_inner_if_return() {
        let src = r#"fn main() -> int {
    let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
    let mut rx_mut: Receiver<int> = rx;
    let result: int = recv(&mut rx_mut);
    if result == 42 {
        return 0;
    }
    return 1;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let outer = c
            .split("ret_val = 1;")
            .nth(1)
            .and_then(|tail| tail.split("goto epilogue;").next())
            .unwrap_or("");
        assert!(
            outer.contains("ion_channel_handle_drop(tx.channel)"),
            "expected outer return to drop sender in:\n{c}"
        );
        assert!(
            outer.contains("ion_channel_handle_drop(rx_mut.channel)"),
            "expected outer return to drop receiver in:\n{c}"
        );
    }

    #[test]
    fn outer_return_drops_after_inner_match_arm_return() {
        let src = r#"enum Opt {
    Some(int);
    None;
}

fn main() -> int {
    let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
    let mut rx_mut: Receiver<int> = rx;
    let x: Opt = Opt::Some(1);
    match x {
        Opt::Some(v) => {
            if v == 99 {
                return 0;
            }
        },
        Opt::None => {
            let _pad: int = 0;
        },
    };
    return 1;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let outer = c
            .split("ret_val = 1;")
            .nth(1)
            .and_then(|tail| tail.split("goto epilogue;").next())
            .unwrap_or("");
        assert!(
            outer.contains("ion_channel_handle_drop(tx.channel)"),
            "expected outer return after match arm to drop sender in:\n{c}"
        );
        assert!(
            outer.contains("ion_channel_handle_drop(rx_mut.channel)"),
            "expected outer return after match arm to drop receiver in:\n{c}"
        );
    }

    #[test]
    fn statement_match_payload_move_neutralizes_scrutinee() {
        let src = r#"enum ReadResult {
    Ok(String);
    Err(int);
}

fn read() -> ReadResult {
    return ReadResult::Ok("hello ion\n");
}

fn main() -> int {
    match read() {
        ReadResult::Ok(raw) => {
            let content: String = raw;
            if content != "hello ion\n" {
                return 1;
            }
            return 80;
        },
        ReadResult::Err(_) => {
            return 2;
        },
    };
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let ok_arm = c
            .split("case 0:")
            .nth(1)
            .and_then(|tail| tail.split("case 1:").next())
            .unwrap_or("");
        assert!(
            ok_arm.contains("match_val_0.data.variant_0.arg0 = NULL"),
            "expected moved-out scrutinee payload to be nulled in:\n{c}"
        );
        assert!(
            ok_arm.contains("if (content) { ion_string_free(content); }"),
            "expected binding drop to free content in:\n{c}"
        );
        assert!(
            !ok_arm.contains("ion_string_free(match_val_0"),
            "expected no enum drop on scrutinee string payload in:\n{c}"
        );
    }

    #[test]
    fn whole_enum_binding_neutralizes_scrutinee_payloads() {
        let src = r#"enum ReadResult {
    Ok(String);
    Err(int);
}

fn read() -> ReadResult {
    return ReadResult::Ok("hello ion\n");
}

fn main() -> int {
    match read() {
        r => {
            return 80;
        },
    };
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let binding_arm = c
            .split("default: // binding r")
            .nth(1)
            .and_then(|tail| tail.split("goto epilogue;").next())
            .unwrap_or("");
        assert!(
            binding_arm.contains("ReadResult r = match_val_0;"),
            "expected whole-enum binding copy in:\n{c}"
        );
        assert!(
            binding_arm.contains("switch (match_val_0.tag)"),
            "expected scrutinee tag switch for whole-enum move-out in:\n{c}"
        );
        assert!(
            binding_arm.contains("case 0:")
                && binding_arm.contains("match_val_0.data.variant_0.arg0 = NULL"),
            "expected Ok string payload nulled in scrutinee switch in:\n{c}"
        );
        assert!(
            !binding_arm.contains("ion_string_free(match_val_0"),
            "expected no direct scrutinee string drop in:\n{c}"
        );
    }

    #[test]
    fn rvalue_match_divergent_return_unwinds_owned() {
        let src = r#"enum E {
    A(int);
    B(int);
}

fn main() -> int {
    let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
    let mut rx_mut: Receiver<int> = rx;
    let s: String = "hi";
    let e: E = E::A(0);
    let n: int = match e {
        E::A(_) => {
            return 1;
        },
        E::B(v) => {
            v;
        },
    };
    return n;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let divergent_arm = c
            .split("case 0:")
            .nth(1)
            .and_then(|tail| tail.split("break;").next())
            .unwrap_or("");
        assert!(
            divergent_arm.contains("ret_val = 1;"),
            "expected function return lowering in divergent rvalue arm in:\n{c}"
        );
        assert!(
            !divergent_arm.contains("return 1;"),
            "expected goto epilogue, not bare C return, in:\n{c}"
        );
        assert!(
            divergent_arm.contains("ion_channel_handle_drop(tx.channel)"),
            "expected channel unwind in divergent rvalue arm in:\n{c}"
        );
        assert!(
            divergent_arm.contains("ion_string_free(s)"),
            "expected String drop in divergent rvalue arm in:\n{c}"
        );
    }

    #[test]
    fn underscore_binding_silenced_once() {
        let src = r#"extern "C" {
    fn write(fd: int, buf: *u8, count: int) -> int;
}
fn log_line() {
    unsafe {
        let _result: int = write(1, "x", 1);
    }
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert_eq!(
            c.matches("(void)_result;").count(),
            1,
            "expected single silence for _result in:\n{c}"
        );
    }

    #[test]
    fn vec_push_struct_var_uses_address_of_lvalue() {
        let src = r#"struct Item { done: bool; }
fn main() -> int {
    let mut items: Vec<Item> = Vec::new();
    let item: Item = Item { done: false };
    Vec::push(&mut items, item);
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("ion_vec_push((ion_vec_t*)(items), &item, sizeof(Item))"),
            "expected address of struct variable in:\n{c}"
        );
    }

    #[test]
    fn vec_string_mangles_ion_type_name() {
        let src = r#"enum Option<T> { Some(T); None; }
fn main() -> int {
    let mut names: Vec<String> = Vec::new();
    Vec::push(&mut names, String::from("a"));
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("typedef struct Vec_String"),
            "expected Vec_String typedef in:\n{c}"
        );
        assert!(
            !c.contains("Vec_ion_string"),
            "unexpected C-type mangling in:\n{c}"
        );
    }

    #[test]
    fn vec_get_match_uses_per_vector_option_type() {
        let src = r#"enum Option<T> { Some(T); None; }
struct Customer { id: int; name: String; }
fn main() -> int {
    let mut customers: Vec<Customer> = Vec::new();
    let mut names: Vec<String> = Vec::new();
    Vec::push(&mut customers, Customer { id: 1, name: String::from("a") });
    Vec::push(&mut names, String::from("b"));
    match Vec::get(&customers, 0) {
        Option::Some(c) => { if c.id != 1 { return 1; } }
        Option::None => { return 2; }
    };
    match Vec::get(&names, 0) {
        Option::Some(s) => { if String::len(&s) != 1 { return 3; } }
        Option::None => { return 4; }
    };
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("Option_Customer match_val_0") && c.contains("Option_String match_val_1"),
            "expected per-vector Option types in:\n{c}"
        );
    }

    #[test]
    fn string_push_str_owned_reads_source_buffer() {
        let src = r#"fn main() -> int {
    let mut s: String = String::from("n=");
    let part: String = String::from("42");
    String::push_str(&mut s, part);
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("_ion_push_other") && c.contains("_ion_push_other->data"),
            "expected owned String append via source buffer in:\n{c}"
        );
    }

    #[test]
    fn struct_field_move_neutralizes_source_field() {
        let src = r#"struct Item { done: bool; }
struct Board { items: Vec<Item>; }
fn take(board: Board) -> Vec<Item> {
    return board.items;
}
fn main() -> int {
    let board: Board = Board { items: 0 };
    let taken: Vec<Item> = take(board);
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("board.items = NULL"),
            "expected moved-out vec field neutralization in:\n{c}"
        );
    }

    #[test]
    fn call_struct_field_move_defers_field_null() {
        let src = r#"struct Item { done: bool; }
struct Batch { items: Vec<Item>; }
fn take(items: Vec<Item>) -> int { return 0; }
fn main() -> int {
    let batch: Batch = Batch { items: 0 };
    let _x: int = take(batch.items);
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("take(batch.items);")
                && c.contains("batch.items = NULL;")
                && c.find("take(batch.items);").unwrap() < c.find("batch.items = NULL;").unwrap(),
            "expected deferred field null after call in:\n{c}"
        );
    }

    #[test]
    fn tuple_vec_int_uses_mangled_tuple_type() {
        let src = r#"struct Item { done: bool; }
fn pair(items: Vec<Item>, n: int) -> (Vec<Item>, int) {
    return (items, n);
}
fn main() -> int {
    let mut items: Vec<Item> = Vec::new();
    let t: (Vec<Item>, int) = pair(items, 1);
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("tuple_Vec_Item_int"),
            "expected Vec tuple mangling in:\n{c}"
        );
    }

    #[test]
    fn get_ref_match_arm_binds_borrow_without_dropping_nested_vec() {
        let src = r#"enum Option<T> {
    Some(T);
    None;
}

struct Line {
    price_cents: int;
    qty: int;
}

struct Order {
    customer_id: int;
    lines: Vec<Line>;
}

fn revenue(orders: &Vec<Order>, customer_id: int) -> int {
    let len: int = Vec::len(orders);
    let mut sum: int = 0;
    let mut i: int = 0;
    while i < len {
        match Vec::get_ref(orders, i) {
            Option::Some(order) => {
                if order.customer_id == customer_id {
                    let line_len: int = Vec::len(&order.lines);
                    let mut j: int = 0;
                    while j < line_len {
                        match Vec::get_ref(&order.lines, j) {
                            Option::Some(line) => {
                                sum = sum + line.price_cents * line.qty;
                            }
                            Option::None => {}
                        };
                        j = j + 1;
                    }
                } else {
                }
            }
            Option::None => {}
        };
        i = i + 1;
    }
    return sum;
}

fn main() -> int {
    let mut orders: Vec<Order> = Vec::new();
    let mut lines: Vec<Line> = Vec::new();
    let line: Line = Line { price_cents: 100, qty: 2 };
    Vec::push(&mut lines, line);
    let order: Order = Order { customer_id: 1, lines: lines };
    Vec::push(&mut orders, order);
    let first: int = revenue(&orders, 1);
    let second: int = revenue(&orders, 1);
    if first != 200 || second != 200 {
        return 1;
    }
    return 0;
}"#;
        let tokens = crate::lexer::Lexer::new(src).tokenize().unwrap();
        let program = crate::parser::Parser::new(tokens).parse().unwrap();
        let ir = crate::ir::IRBuilder::build(&program);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("Order* order = match_val_"),
            "expected get_ref arm to bind Order* borrow in:\n{c}"
        );
        assert!(
            !c.contains("Order order = *match_val_"),
            "expected no by-value copy from get_ref borrow in:\n{c}"
        );
        let revenue_fn = c
            .split("int revenue(")
            .nth(1)
            .and_then(|tail| tail.split("int main(").next())
            .unwrap_or("");
        assert!(
            !revenue_fn.contains("ion_vec_free((ion_vec_t*)(order.lines))"),
            "expected no nested Vec drop inside get_ref scan loop in:\n{c}"
        );
    }

    #[test]
    fn multi_file_resolves_module_local_symbols() {
        let mut cg = Codegen::new();
        cg.multi_file_module = Some("fmt".to_string());
        assert_eq!(cg.module_c_symbol("print_int"), "fmt_print_int");
        assert_eq!(cg.module_c_symbol("io::print_int"), "io_print_int");
        cg.extern_functions.insert(
            "write".to_string(),
            vec![
                Type::Int,
                Type::RawPtr {
                    inner: Box::new(Type::U8),
                },
                Type::Int,
            ],
        );
        assert_eq!(cg.module_c_symbol("write"), "write");
    }
}
