use crate::ast::{
    BinOp, EnumDecl, EnumVariant, ExternBlock, Program, Span, StructDecl, Type, TypeAliasDecl, UnOp,
};
use crate::ir::*;
use std::collections::HashMap;

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
    in_unsafe_block: bool,                                // Track if we're in an unsafe block
    temp_var_counter: usize, // Counter for unique temporary variable names
    current_function_params: HashMap<String, Type>, // Track current function parameter types for field access
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}

/// Mangle a type name for C identifier safety
fn mangle_type_name(base: &str, params: &[Type]) -> String {
    if params.is_empty() {
        base.to_string()
    } else {
        let param_strs: Vec<String> = params.iter().map(type_to_c_impl).collect();
        // Sanitize type names for C identifiers (replace spaces and special chars with _)
        let sanitized_params: Vec<String> = param_strs
            .iter()
            .map(|s| {
                s.chars()
                    .map(|c| {
                        if c.is_alphanumeric() || c == '_' {
                            c
                        } else {
                            '_'
                        }
                    })
                    .collect()
            })
            .collect();
        format!("{}_{}", base, sanitized_params.join("_"))
    }
}

/// Substitute generic type parameters in a type using the substitution map
fn substitute_type_params(
    ty: &Type,
    substitutions: &std::collections::HashMap<String, &Type>,
) -> Type {
    match ty {
        Type::Struct(name) | Type::Enum(name) => {
            // Check if this is a generic parameter that needs substitution
            substitutions
                .get(name)
                .map(|&sub_ty| sub_ty.clone())
                .unwrap_or_else(|| ty.clone())
        }
        Type::Generic { name, params } => {
            // First check if the generic type itself is a parameter (e.g., T where T is a generic)
            if let Some(&sub_ty) = substitutions.get(name) {
                sub_ty.clone()
            } else {
                // Otherwise, recursively substitute parameters
                let substituted_params: Vec<Type> = params
                    .iter()
                    .map(|p| substitute_type_params(p, substitutions))
                    .collect();
                Type::Generic {
                    name: name.clone(),
                    params: substituted_params,
                }
            }
        }
        Type::Ref { inner, mutable } => Type::Ref {
            inner: Box::new(substitute_type_params(inner, substitutions)),
            mutable: *mutable,
        },
        Type::RawPtr { inner } => Type::RawPtr {
            inner: Box::new(substitute_type_params(inner, substitutions)),
        },
        Type::Box { inner } => Type::Box {
            inner: Box::new(substitute_type_params(inner, substitutions)),
        },
        Type::Vec { elem_type } => Type::Vec {
            elem_type: Box::new(substitute_type_params(elem_type, substitutions)),
        },
        Type::Channel { elem_type } => Type::Channel {
            elem_type: Box::new(substitute_type_params(elem_type, substitutions)),
        },
        Type::Array { inner, size } => Type::Array {
            inner: Box::new(substitute_type_params(inner, substitutions)),
            size: *size,
        },
        Type::Slice { inner } => Type::Slice {
            inner: Box::new(substitute_type_params(inner, substitutions)),
        },
        Type::Sender { elem_type } => Type::Sender {
            elem_type: Box::new(substitute_type_params(elem_type, substitutions)),
        },
        Type::Receiver { elem_type } => Type::Receiver {
            elem_type: Box::new(substitute_type_params(elem_type, substitutions)),
        },
        Type::Tuple { elements } => Type::Tuple {
            elements: elements
                .iter()
                .map(|e| substitute_type_params(e, substitutions))
                .collect(),
        },
        // Primitive types don't need substitution
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
        | Type::String => ty.clone(),
    }
}

/// Resolve type aliases recursively
fn resolve_type_alias(ty: &Type, type_aliases: &HashMap<String, TypeAliasDecl>) -> Type {
    match ty {
        Type::Struct(name) | Type::Enum(name) => {
            if let Some(alias) = type_aliases.get(name) {
                // If the alias has generic parameters, we need to substitute them
                if !alias.generics.is_empty() {
                    // For now, handle simple case - if the type is Generic with matching param count
                    if let Type::Generic { params, .. } = ty
                        && params.len() == alias.generics.len()
                    {
                        let substitutions: HashMap<String, &Type> = alias
                            .generics
                            .iter()
                            .zip(params.iter())
                            .map(|(gen_name, param_ty)| (gen_name.clone(), param_ty))
                            .collect();
                        let resolved = substitute_type_params(&alias.target, &substitutions);
                        return resolve_type_alias(&resolved, type_aliases);
                    }
                    // Fall through to return original type if substitution doesn't match
                    return resolve_type_alias(&alias.target, type_aliases);
                }
                return resolve_type_alias(&alias.target, type_aliases);
            }
            ty.clone()
        }
        Type::Generic { name, params } => {
            if let Some(alias) = type_aliases.get(name) {
                if !alias.generics.is_empty() && params.len() == alias.generics.len() {
                    let substitutions: HashMap<String, &Type> = alias
                        .generics
                        .iter()
                        .zip(params.iter())
                        .map(|(gen_name, param_ty)| (gen_name.clone(), param_ty))
                        .collect();
                    let resolved = substitute_type_params(&alias.target, &substitutions);
                    return resolve_type_alias(&resolved, type_aliases);
                }
                return resolve_type_alias(&alias.target, type_aliases);
            }
            ty.clone()
        }
        Type::Ref { inner, mutable } => Type::Ref {
            inner: Box::new(resolve_type_alias(inner, type_aliases)),
            mutable: *mutable,
        },
        Type::RawPtr { inner } => Type::RawPtr {
            inner: Box::new(resolve_type_alias(inner, type_aliases)),
        },
        Type::Box { inner } => Type::Box {
            inner: Box::new(resolve_type_alias(inner, type_aliases)),
        },
        Type::Vec { elem_type } => Type::Vec {
            elem_type: Box::new(resolve_type_alias(elem_type, type_aliases)),
        },
        Type::Channel { elem_type } => Type::Channel {
            elem_type: Box::new(resolve_type_alias(elem_type, type_aliases)),
        },
        Type::Array { inner, size } => Type::Array {
            inner: Box::new(resolve_type_alias(inner, type_aliases)),
            size: *size,
        },
        Type::Slice { inner } => Type::Slice {
            inner: Box::new(resolve_type_alias(inner, type_aliases)),
        },
        _ => ty.clone(),
    }
}

fn type_to_c_impl(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Bool => "int".to_string(), // C doesn't have native bool, use int with 0/1
        Type::F32 => "float".to_string(),
        Type::F64 => "double".to_string(),
        Type::I8 => "int8_t".to_string(),
        Type::I16 => "int16_t".to_string(),
        Type::I32 => "int32_t".to_string(),
        Type::I64 => "int64_t".to_string(),
        Type::U8 => "uint8_t".to_string(),
        Type::U16 => "uint16_t".to_string(),
        Type::U32 => "uint32_t".to_string(),
        Type::U64 => "uint64_t".to_string(),
        Type::UInt => "unsigned int".to_string(),
        Type::Ref { inner, mutable: _ } => {
            // References map to C pointers: &T -> T*
            format!("{}*", type_to_c_impl(inner))
        }
        Type::RawPtr { inner } => {
            // Raw pointers map to C pointers: *T -> T*
            format!("{}*", type_to_c_impl(inner))
        }
        Type::Channel { elem_type: _ } => {
            // Channels are opaque pointers to the runtime channel type.
            "ion_channel_t*".to_string()
        }
        Type::Struct(name) => {
            // Structs map to their C typedef name.
            name.clone()
        }
        Type::Enum(name) => name.clone(),
        Type::Generic { name, params } => {
            // Generic types: resolve and mangle the name
            // For now, we handle special cases (Box, Vec) and assume others need resolution
            match name.as_str() {
                "Box" if params.len() == 1 => {
                    format!("{}*", type_to_c_impl(&params[0]))
                }
                "Vec" if params.len() == 1 => {
                    format!(
                        "Vec_{}*",
                        mangle_type_name(&type_to_c_impl(&params[0]), &[])
                    )
                }
                _ => {
                    // Generic user-defined types - mangle the name with parameters
                    mangle_type_name(name, params)
                }
            }
        }
        Type::Box { inner } => {
            // Box<T> is a pointer to T on the heap
            format!("{}*", type_to_c_impl(inner))
        }
        Type::Vec { elem_type } => {
            // Vec<T> will be a pointer to a vector structure
            format!("Vec_{}*", mangle_type_name(&type_to_c_impl(elem_type), &[]))
        }
        Type::String => "ion_string_t*".to_string(),
        Type::Array { inner, size } => {
            // Fixed-size arrays: [T; N] -> T name[N]
            format!("{}[{}]", type_to_c_impl(inner), size)
        }
        Type::Slice { inner } => {
            // Slices: []T -> ion_slice_T (fat pointer struct)
            format!(
                "ion_slice_{}",
                mangle_type_name(&type_to_c_impl(inner), &[])
            )
        }
        Type::Sender { elem_type: _ } => {
            // Sender<T> is a struct value (ion_sender_t), not a pointer
            "ion_sender_t".to_string()
        }
        Type::Receiver { elem_type: _ } => {
            // Receiver<T> is a struct value (ion_receiver_t), not a pointer
            "ion_receiver_t".to_string()
        }
        Type::Tuple { elements: _ } => {
            // Tuples are not directly representable in C, they'll be handled specially
            // This should not appear in normal codegen (tuples are desugared)
            "void".to_string()
        }
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
            in_unsafe_block: false,
            temp_var_counter: 0,
            current_function_params: HashMap::new(),
        }
    }

    pub fn generate(&mut self, program: &IRProgram) -> String {
        self.output.clear();
        self.indent_level = 0;
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
        for function in &program.functions {
            self.function_return_types
                .insert(function.name.clone(), function.return_type.clone());
        }
        self.generated_types.clear();

        // Generate includes if needed
        self.writeln("#include <stdio.h>");
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <string.h>");
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

        // Emit struct type definitions first so functions can use them.
        // Skip generic structs - they'll be generated as monomorphized versions
        for s in &program.structs {
            if s.generics.is_empty() {
                // Non-generic struct - generate directly
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
        // Skip generic enums - they'll be generated as monomorphized versions
        for e in &program.enums {
            if e.generics.is_empty() {
                // Non-generic enum - generate directly
                self.generate_enum_type(e);
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
                        span: Span {
                            start: 0,
                            end: 0,
                            line: 0,
                            column: 0,
                        },
                    },
                    EnumVariant {
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
            self.generate_vec_struct(vec_type_name);

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
                                span: Span {
                                    start: 0,
                                    end: 0,
                                    line: 0,
                                    column: 0,
                                },
                            },
                            EnumVariant {
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

        // Collect and generate slice type definitions
        let mut slice_types = std::collections::HashSet::new();
        collect_slice_types_impl(program, &mut slice_types);
        for slice_type_name in &slice_types {
            self.generate_slice_struct(slice_type_name);
        }

        // Generate extern function prototypes
        for extern_block in &program.extern_blocks {
            self.generate_extern_block(extern_block);
        }

        // Generate function prototypes (forward declarations) for ALL functions
        // This is required for single-file mode where functions may be called before definition
        for func in &program.functions {
            let return_type = func
                .return_type
                .as_ref()
                .map(|t| self.type_to_c(t))
                .unwrap_or_else(|| "void".to_string());
            self.write(&format!("{} {}(", return_type, func.name));
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
                            let base_type = self.type_to_c(inner);
                            self.write(&format!("{} {}[{}]", base_type, param.name, size));
                        }
                        _ => {
                            self.write(&format!("{} {}", self.type_to_c(&param.ty), param.name));
                        }
                    }
                }
            }
            self.writeln(");");
        }

        // Generate each function
        for function in &program.functions {
            self.generate_function(function);
        }

        self.output.clone()
    }

    /// Generate C source file for a module (for multi-file mode)
    pub fn generate_module_source(
        &mut self,
        program: &IRProgram,
        module_name: &str,
        imports: &[String],
    ) -> String {
        self.output.clear();
        self.indent_level = 0;
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

        // Generate includes
        self.writeln("#include <stdio.h>");
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <string.h>");
        // Runtime header - use ion_runtime.h and rely on compiler include paths
        // The compiler will add -I. -I.. -Iruntime -I../runtime to find it
        self.writeln("#include \"ion_runtime.h\"");
        // Include the module's own header
        self.writeln(&format!("#include \"{}.h\"", module_name));
        // Include headers for imported modules
        for import_name in imports {
            if import_name != module_name {
                self.writeln(&format!("#include \"{}.h\"", import_name));
            }
        }
        self.writeln("");

        // Collect all generic type instantiations used in the program
        let mut generic_instantiations_map: std::collections::HashMap<String, (String, Vec<Type>)> =
            std::collections::HashMap::new();
        collect_generic_instantiations(program, &mut generic_instantiations_map);
        self.generic_instantiations = generic_instantiations_map.clone();

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
                            span: Span {
                                start: 0,
                                end: 0,
                                line: 0,
                                column: 0,
                            },
                        },
                        EnumVariant {
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
            self.generate_vec_struct(vec_type_name);

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
                                span: Span {
                                    start: 0,
                                    end: 0,
                                    line: 0,
                                    column: 0,
                                },
                            },
                            EnumVariant {
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

        // Collect and generate slice type definitions
        let mut slice_types = std::collections::HashSet::new();
        collect_slice_types_impl(program, &mut slice_types);
        for slice_type_name in &slice_types {
            self.generate_slice_struct(slice_type_name);
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
        for function in &program.functions {
            self.generate_function(function);
        }

        self.output.clone()
    }

    /// Generate C header file for a module (for multi-file mode)
    pub fn generate_module_header(&mut self, program: &Program, module_name: &str) -> String {
        self.output.clear();
        self.indent_level = 0;

        // Generate include guard
        let guard_name = format!("ION_{}_H", module_name.to_uppercase().replace("-", "_"));
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
                self.write(&format!("{} {}(", return_type, func.name));
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

    fn generate_function(&mut self, function: &IRFunction) {
        // Store current return type for use in return statements
        self.current_return_type = function.return_type.clone();

        // Store parameter types for field access resolution
        self.current_function_params.clear();
        for param in &function.params {
            self.current_function_params
                .insert(param.name.clone(), param.ty.clone());
        }

        // Generate return type
        // Special handling: C doesn't allow returning arrays directly, so convert to pointer
        let return_type = function
            .return_type
            .as_ref()
            .map(|t| {
                match t {
                    Type::Array { inner, .. } => {
                        // Convert array return type to pointer: [T; N] -> T*
                        format!("{}*", self.type_to_c(inner))
                    }
                    _ => self.type_to_c(t),
                }
            })
            .unwrap_or_else(|| "void".to_string());

        self.write(&format!("{} {}(", return_type, function.name));

        // Generate parameters
        if function.params.is_empty() {
            self.write("void");
        } else {
            for (i, param) in function.params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                // Special handling for array types: C syntax is "int arr[3]" not "int[3] arr"
                match &param.ty {
                    Type::Array { inner, size } => {
                        let base_type = self.type_to_c(inner);
                        self.write(&format!("{} {}[{}]", base_type, param.name, size));
                    }
                    _ => {
                        self.write(&format!("{} {}", self.type_to_c(&param.ty), param.name));
                    }
                }
            }
        }

        self.writeln(") {");
        self.indent_level += 1;

        // Synthetic return variable for functions with a return type.
        if let Some(ref ty) = function.return_type {
            self.write_indent();
            let (ret_var_type, init_value) = match ty {
                // Array types: use pointer type for return variable, initialize to NULL
                Type::Array { inner, .. } => {
                    (format!("{}*", self.type_to_c(inner)), "0".to_string())
                }
                // Pointer types (Box, Vec, String, channels, refs) use 0/NULL
                Type::Box { .. }
                | Type::Vec { .. }
                | Type::String
                | Type::Channel { .. }
                | Type::Ref { .. } => (self.type_to_c(ty), "0".to_string()),
                // Struct and enum types use {0} for zero-initialization
                Type::Struct(_) | Type::Enum(_) => (self.type_to_c(ty), "{0}".to_string()),
                // Generic types: check if they resolve to struct/enum or pointer
                Type::Generic { name, .. } => {
                    match name.as_str() {
                        "Box" | "Vec" => (self.type_to_c(ty), "0".to_string()), // Pointers
                        _ => (self.type_to_c(ty), "{0}".to_string()), // Assume struct/enum for user-defined generics
                    }
                }
                // Primitive types use 0
                _ => (self.type_to_c(ty), "0".to_string()),
            };
            self.writeln(&format!("{} ret_val = {};", ret_var_type, init_value));
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

        // If the function has a return type and control reaches the end
        // without an explicit return, fall through to epilogue with the
        // default-initialized ret_val (0 / NULL).
        if function.return_type.is_some() && !last_stmt_is_return {
            self.write_indent();
            self.writeln("goto epilogue;");
        }

        // Epilogue: run defers in reverse order, then return.
        self.writeln("epilogue:");
        self.indent_level += 1;

        // Emit defers in LIFO order.
        for defer_expr in function.defers.iter().rev() {
            self.write_indent();
            self.generate_expr(defer_expr);
            self.writeln(";");
        }

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

    fn generate_block(&mut self, block: &IRBlock) {
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

                    // Skip both statements
                    i += 2;
                    continue;
                }
            }

            // Normal statement generation
            self.generate_stmt(&block.statements[i]);
            i += 1;
        }
    }

    fn generate_stmt(&mut self, stmt: &IRStmt) {
        match stmt {
            IRStmt::Let(let_stmt) => {
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
                                        && matches!(return_type.as_ref().unwrap(), Type::Generic { name: n, .. } if n == "Option"));

                                if needs_cast {
                                    // Cast void* to Option_T* and dereference
                                    let mono_name = mangle_type_name("Option", params);
                                    self.write(&format!("*(({}*)(", mono_name));
                                    self.generate_expr(init);
                                    self.write("))");
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
                }

                self.writeln(";");
            }
            IRStmt::Return(ret) => {
                self.write_indent();
                if let Some(ref value) = ret.value {
                    // Special handling for array returns: C doesn't allow returning arrays directly
                    // If return type is an array (converted to pointer), we need to use static storage
                    let is_array_return = if let Some(ref ret_ty) = self.current_return_type {
                        matches!(ret_ty, Type::Array { .. })
                    } else {
                        false
                    };

                    if is_array_return {
                        // For array returns, create a static array and return its address
                        if let Some(Type::Array { inner, size }) = self.current_return_type.as_ref()
                        {
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
                        // Assign to synthetic return variable and jump to epilogue.
                        self.write("ret_val = ");

                        // Check if this is a struct literal with array field that needs memcpy
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

                        self.generate_expr(value);
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
                self.write_indent();
                self.writeln("goto epilogue;");
            }
            IRStmt::Expr(expr) => {
                // Special handling: match expressions used as statements should be blocks, not statement expressions
                match expr {
                    IREexpr::Match {
                        expr: match_expr,
                        enum_type,
                        arms,
                    } => {
                        self.generate_match_block(match_expr, enum_type, arms);
                    }
                    IREexpr::Send { channel, value } => {
                        // Send is used as a statement - generate it as a block to handle temp variables
                        // This avoids statement expression syntax issues with comma operator
                        self.write_indent();
                        self.write("{ ");
                        // Create temporary variable for value if it's a literal
                        let needs_temp =
                            matches!(value.as_ref(), IREexpr::Lit(_) | IREexpr::BoolLiteral(_));
                        if needs_temp {
                            let value_type = Type::Int; // TODO: infer from context
                            self.write(&format!("{} _send_val = ", self.type_to_c(&value_type)));
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
                    }
                    _ => {
                        self.write_indent();
                        self.generate_expr(expr);
                        self.writeln(";");
                    }
                }
            }
            IRStmt::Defer(_) => {
                // Defers are collected at the function level and emitted
                // in the epilogue; no direct code is generated here.
            }
            IRStmt::Spawn(spawn) => {
                // For now, spawn is lowered to a synchronous nested block.
                self.write_indent();
                self.writeln("{");
                self.indent_level += 1;
                self.generate_block(&spawn.body);
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
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
                // Generate: while (cond) { ... }
                self.write_indent();
                self.write("while (");
                self.generate_expr_conditional(&ir_while.cond);
                self.writeln(") {");
                self.indent_level += 1;
                self.generate_block(&ir_while.body);
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
                // For conditionals, don't add extra parentheses around simple comparisons
                self.generate_expr(left);
                self.write(&format!(" {} ", self.op_to_c(*op)));
                self.generate_expr(right);
            }
            IREexpr::UnOp { op, operand } => match op {
                UnOp::Not => {
                    self.write("!");
                    self.generate_expr(operand);
                }
                UnOp::Neg => {
                    self.write("-");
                    self.generate_expr(operand);
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
            IREexpr::BoolLiteral(value) => {
                // Boolean literals: true -> 1, false -> 0
                self.write(if *value { "1" } else { "0" });
            }
            IREexpr::FloatLiteral(value) => {
                // Float literals: output as-is (C will handle the format)
                self.write(&value.to_string());
            }
            IREexpr::Var(name) => {
                self.write(name);
            }
            IREexpr::AddressOf { inner, mutable: _ } => {
                // Generate address-of: &x
                self.write("&");
                self.generate_expr(inner);
            }
            IREexpr::BinOp { op, left, right } => {
                self.write("(");
                self.generate_expr(left);
                self.write(&format!(" {} ", self.op_to_c(*op)));
                self.generate_expr(right);
                self.write(")");
            }
            IREexpr::UnOp { op, operand } => match op {
                UnOp::Not => {
                    self.write("(!");
                    self.generate_expr(operand);
                    self.write(")");
                }
                UnOp::Neg => {
                    self.write("(-");
                    self.generate_expr(operand);
                    self.write(")");
                }
            },
            IREexpr::Send { channel, value } => {
                // ion_channel_send(sender, &value)
                // channel is &Sender<T>, which should be passed as the sender value
                // value needs to be in a variable (can't take address of literal)
                // When used as a statement, we use a statement expression to handle temp variables
                self.write("({ ");
                // Create temporary variable for value if it's a literal
                let needs_temp =
                    matches!(value.as_ref(), IREexpr::Lit(_) | IREexpr::BoolLiteral(_));
                if needs_temp {
                    // Infer value type from context or default to int
                    let value_type = Type::Int; // TODO: infer from context
                    self.write(&format!("{} _send_val = ", self.type_to_c(&value_type)));
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
                            // "string".data -> just the string literal
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
                    self.generate_expr(base);
                    if *is_pointer {
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
                // Check if this is actually a qualified function call that was mis-parsed as EnumLit
                // If the enum_name is not in enum_map, it's likely a module function call
                if !self.enum_map.contains_key(enum_name) {
                    // This is actually a function call, not an enum literal
                    // Use just the variant name as the function name (module prefix already stripped)
                    self.write(variant);
                    self.write("(");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.generate_expr(arg);
                    }
                    self.write(")");
                    return;
                }

                // Generate enum constructor call: EnumName_VariantName_new(arg1, arg2, ...)
                // Use type context to get monomorphized name if it's a generic type
                let monomorphized_enum_name = if let Some(context_ty) = type_context {
                    // Check if the enum is generic - if so, use the type context (which should be monomorphized)
                    let is_generic = self
                        .enum_map
                        .get(enum_name)
                        .map(|e| !e.generics.is_empty())
                        .unwrap_or(false);
                    if is_generic {
                        // Use the type context which should already be the monomorphized name
                        self.type_to_c(context_ty)
                    } else {
                        enum_name.clone()
                    }
                } else {
                    enum_name.clone()
                };
                let constructor_name = format!("{}_{}_new", monomorphized_enum_name, variant);
                self.write(&constructor_name);
                self.write("(");

                // Handle struct variants with named fields
                if let Some(named_fields) = named_fields {
                    for (i, (_field_name, field_expr)) in named_fields.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.generate_expr(field_expr);
                    }
                } else {
                    // Handle tuple variants with positional arguments
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.generate_expr(arg);
                    }
                }
                self.write(")");
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
                let (monomorphized_enum_name, type_params) = if let IREexpr::Call {
                    callee,
                    return_type,
                    ..
                } = expr.as_ref()
                {
                    // If matching on Vec::pop or Vec::get result, extract element type from Vec argument
                    if callee == "Vec::pop" || callee == "Vec::get" {
                        // Extract element type from Vec<T> in generic_instantiations
                        // Look for Vec_* types and extract the element type
                        if let Some(elem_type) = self.generic_instantiations.iter().find_map(
                            |(_mono_name, (base, params))| {
                                if base == "Vec" && params.len() == 1 {
                                    Some(params[0].clone())
                                } else {
                                    None
                                }
                            },
                        ) {
                            // We have the element type, construct Option<T>
                            let mono_name =
                                mangle_type_name("Option", std::slice::from_ref(&elem_type));
                            (mono_name, vec![elem_type])
                        } else {
                            // Fall back to searching in generic_instantiations
                            self.find_enum_instantiation(enum_type, enum_decl.as_ref())
                        }
                    } else if let Some(Type::Generic { name, params }) = return_type
                        && name == "Option"
                        && params.len() == 1
                    {
                        // We have Option<T> from return_type, compute the monomorphized name
                        let mono_name = mangle_type_name("Option", params);
                        (mono_name, params.clone())
                    } else {
                        // Fall back to searching in generic_instantiations
                        self.find_enum_instantiation(enum_type, enum_decl.as_ref())
                    }
                } else {
                    // Fall back to searching in generic_instantiations
                    self.find_enum_instantiation(enum_type, enum_decl.as_ref())
                };

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
                    // Cast void* to Option_T* and dereference
                    self.write(&format!(
                        "{} {} = *(({}*)(",
                        monomorphized_enum_name, match_var_name, monomorphized_enum_name
                    ));
                    self.generate_expr(expr);
                    self.writeln("));");
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
                self.indent_level += 1;
                for arm in arms {
                    self.generate_match_arm(
                        arm,
                        &monomorphized_enum_name,
                        enum_decl.as_ref(),
                        &match_var_name,
                        &type_params,
                    );
                }
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
                // Statement expressions need a value - use 0 as dummy value
                // (actual return statements in arms will have already returned)
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
                if let Some(code) =
                    self.generate_builtin_call(resolved_callee.as_str(), args, return_type.as_ref())
                {
                    self.write(&code);
                } else {
                    // Regular function call - strip module prefix if present (mod::func -> func)
                    let func_name = if resolved_callee.contains("::") {
                        resolved_callee
                            .split("::")
                            .last()
                            .unwrap_or(&resolved_callee)
                    } else {
                        &resolved_callee
                    };

                    self.write(func_name);
                    self.write("(");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        // If this is an extern function expecting int by value, convert &int (immutable) arguments to int
                        // For &int -> int: just use the inner expression (the variable itself)
                        // For &mut int -> int*: keep the address-of (don't dereference)
                        if let Some(param_types) = self.extern_functions.get(func_name)
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
                        self.generate_expr(arg);
                    }
                    self.write(")");
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
                if self.in_unsafe_block {
                    // Unchecked indexing for unsafe blocks - direct C array access
                    self.generate_expr(target);
                    self.write("[");
                    self.generate_expr(index);
                    self.write("]");
                } else {
                    // Bounds-checked indexing using statement expression
                    // Generate: ({ int __idx = index; (__idx >= 0 && __idx < len) ? arr[__idx] : (ion_panic("..."), arr[0]); })

                    // Extract array length if we have type information
                    let array_len = if let Some(Type::Array { size, .. }) = target_type {
                        Some(*size)
                    } else {
                        None
                    };

                    if let Some(len) = array_len {
                        // We know the array length at compile time - generate bounds check
                        let temp_var = format!("__ion_idx_{}", self.temp_var_counter);
                        self.temp_var_counter += 1;

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
                    } else {
                        // No type information - fall back to unchecked (for now)
                        // TODO: Track array types through IR for complete bounds checking
                        self.generate_expr(target);
                        self.write("[");
                        self.generate_expr(index);
                        self.write("]");
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
        }
    }

    fn type_to_c(&self, ty: &Type) -> String {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        type_to_c_impl(&resolved)
    }

    /// Generate code for built-in function calls.
    /// Returns Some(code) if it's a built-in, None otherwise.
    fn generate_builtin_call(
        &mut self,
        callee: &str,
        args: &[IREexpr],
        return_type: Option<&Type>,
    ) -> Option<String> {
        // Box::new<T>(value: T) -> Box<T>
        if callee.starts_with("Box::new") && args.len() == 1 {
            // Get the type from the return type
            let inner_type = return_type
                .and_then(|t| {
                    if let Type::Box { inner } = t {
                        Some(inner.as_ref())
                    } else {
                        None
                    }
                })
                .unwrap_or(&Type::Int);

            let inner_c_type = self.type_to_c(inner_type);
            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&format!(
                "{}* ptr = ({}*)ion_box_alloc(sizeof({}));",
                inner_c_type, inner_c_type, inner_c_type
            ));
            code.push_str(" if (ptr) { *ptr = ");
            // Generate the argument expression
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            code.push_str(&arg_code);
            code.push_str("; } ptr; })");
            return Some(code);
        }

        // Box::unwrap<T>(box: Box<T>) -> T
        if callee == "Box::unwrap" && args.len() == 1 {
            let mut code = String::new();
            code.push_str("(*");
            // Generate the argument expression
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            code.push_str(&arg_code);
            code.push(')');
            return Some(code);
        }

        // Vec::new<T>() -> Vec<T>
        if callee == "Vec::new" && args.is_empty() {
            // Extract element type from return type
            let elem_type = return_type
                .and_then(|t| {
                    if let Type::Vec { elem_type } = t {
                        Some(elem_type.as_ref())
                    } else {
                        None
                    }
                })
                .unwrap_or(&Type::Int);

            let elem_c_type = self.type_to_c(elem_type);
            // Vec::new returns ion_vec_t*, but we need Vec_T*
            // Cast ion_vec_t* to Vec_T*
            // mangle_type_name expects a base name and params, but elem_c_type is already a C type name
            // So we just use it directly
            let vec_type_name =
                format!("Vec_{}", elem_c_type.replace(' ', "_").replace('*', "ptr"));
            let code = format!(
                "(({}*)(ion_vec_new(sizeof({}))))",
                vec_type_name, elem_c_type
            );
            return Some(code);
        }

        // Vec::with_capacity<T>(cap: int) -> Vec<T>
        if callee == "Vec::with_capacity" && args.len() == 1 {
            let elem_type = return_type
                .and_then(|t| {
                    if let Type::Vec { elem_type } = t {
                        Some(elem_type.as_ref())
                    } else {
                        None
                    }
                })
                .unwrap_or(&Type::Int);

            let elem_c_type = self.type_to_c(elem_type);
            let vec_type_name =
                format!("Vec_{}", elem_c_type.replace(' ', "_").replace('*', "ptr"));
            let mut code = String::new();
            code.push_str(&format!(
                "(({}*)(ion_vec_with_capacity(sizeof(",
                vec_type_name
            ));
            code.push_str(&elem_c_type);
            code.push_str("), ");
            // Generate capacity argument
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            code.push_str(&arg_code);
            code.push_str(")))");
            return Some(code);
        }

        // Vec::len<T>(vec: &Vec<T>) -> int
        if callee == "Vec::len" && args.len() == 1 {
            let mut code = String::new();
            // Generate vec argument - it's a reference, so we need to dereference it
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            // Remove leading & if present - &v where v: Vec<T> gives Vec_T**, we need Vec_T*
            let deref_arg = arg_code.strip_prefix('&').unwrap_or(&arg_code);
            code.push_str("((");
            code.push_str(deref_arg);
            code.push_str(") ? (int)((ion_vec_t*)(");
            code.push_str(deref_arg);
            code.push_str("))->len : 0)");
            return Some(code);
        }

        // Vec::capacity<T>(vec: &Vec<T>) -> int
        if callee == "Vec::capacity" && args.len() == 1 {
            let mut code = String::new();
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            // Remove leading & if present
            let deref_arg = arg_code.strip_prefix('&').unwrap_or(&arg_code);
            code.push_str("((");
            code.push_str(deref_arg);
            code.push_str(") ? (int)((ion_vec_t*)(");
            code.push_str(deref_arg);
            code.push_str("))->capacity : 0)");
            return Some(code);
        }

        // Vec::push<T>(vec: &mut Vec<T>, value: T)
        if callee == "Vec::push" && args.len() == 2 {
            let mut code = String::new();
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);
            // Remove leading & if present
            let deref_vec = vec_code.strip_prefix('&').unwrap_or(&vec_code);

            let mut value_code = String::new();
            let old_output = std::mem::replace(&mut self.output, value_code);
            self.generate_expr(&args[1]);
            value_code = std::mem::replace(&mut self.output, old_output);

            // Extract element type from Vec<T> in generic_instantiations
            // Look for Vec_* types and extract the element type
            let elem_c_type = self
                .generic_instantiations
                .iter()
                .find_map(|(_mono_name, (base, params))| {
                    if base == "Vec" && params.len() == 1 {
                        Some(self.type_to_c(&params[0]))
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| "int".to_string());

            // Use compound literal to take address of value
            code.push_str("ion_vec_push((ion_vec_t*)(");
            code.push_str(deref_vec);
            code.push_str("), &(");
            code.push_str(&format!("({}){{", elem_c_type));
            code.push_str(&value_code);
            code.push_str("}), sizeof(");
            code.push_str(&elem_c_type);
            code.push_str("))");
            return Some(code);
        }

        // Vec::pop<T>(vec: &mut Vec<T>) -> Option<T>
        if callee == "Vec::pop" && args.len() == 1 {
            let mut code = String::new();
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);
            // Remove leading & if present
            let deref_vec = vec_code.strip_prefix('&').unwrap_or(&vec_code);

            // Extract element type from return type (Option<T>)
            let elem_type = return_type
                .and_then(|t| {
                    if let Type::Generic { name, params } = t {
                        if name == "Option" && params.len() == 1 {
                            Some(&params[0])
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .unwrap_or(&Type::Int);

            let elem_c_type = self.type_to_c(elem_type);
            code.push_str("ion_vec_pop((ion_vec_t*)(");
            code.push_str(deref_vec);
            code.push_str("), sizeof(");
            code.push_str(&elem_c_type);
            code.push_str("))");
            return Some(code);
        }

        // Vec::get<T>(vec: &Vec<T>, index: int) -> Option<T>
        if callee == "Vec::get" && args.len() == 2 {
            let mut code = String::new();
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);

            let mut index_code = String::new();
            let old_output = std::mem::replace(&mut self.output, index_code);
            self.generate_expr(&args[1]);
            index_code = std::mem::replace(&mut self.output, old_output);

            let elem_type = return_type
                .and_then(|t| {
                    if let Type::Generic { name, params } = t {
                        if name == "Option" && params.len() == 1 {
                            Some(&params[0])
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .unwrap_or(&Type::Int);

            let elem_c_type = self.type_to_c(elem_type);
            // Remove leading & if present
            let deref_vec = vec_code.strip_prefix('&').unwrap_or(&vec_code);
            code.push_str("ion_vec_get((ion_vec_t*)(");
            code.push_str(deref_vec);
            code.push_str("), ");
            code.push_str(&index_code);
            code.push_str(", sizeof(");
            code.push_str(&elem_c_type);
            code.push_str("))");
            return Some(code);
        }

        // Vec::set<T>(vec: &mut Vec<T>, index: int, value: T) -> int
        if callee == "Vec::set" && args.len() == 3 {
            let mut code = String::new();
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);
            // Remove leading & if present
            let deref_vec = vec_code.strip_prefix('&').unwrap_or(&vec_code);

            let mut index_code = String::new();
            let old_output = std::mem::replace(&mut self.output, index_code);
            self.generate_expr(&args[1]);
            index_code = std::mem::replace(&mut self.output, old_output);

            let mut value_code = String::new();
            let old_output = std::mem::replace(&mut self.output, value_code);
            self.generate_expr(&args[2]);
            value_code = std::mem::replace(&mut self.output, old_output);

            // Extract element type from Vec<T> in generic_instantiations
            // Look for Vec_* types and extract the element type
            let elem_c_type = self
                .generic_instantiations
                .iter()
                .find_map(|(_mono_name, (base, params))| {
                    if base == "Vec" && params.len() == 1 {
                        Some(self.type_to_c(&params[0]))
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| "int".to_string());

            // Use compound literal to take address of value
            code.push_str("ion_vec_set((ion_vec_t*)(");
            code.push_str(deref_vec);
            code.push_str("), ");
            code.push_str(&index_code);
            code.push_str(", &(");
            code.push_str(&format!("({}){{", elem_c_type));
            code.push_str(&value_code);
            code.push_str("}), sizeof(");
            code.push_str(&elem_c_type);
            code.push_str("))");
            return Some(code);
        }

        // String::new() -> String
        if callee == "String::new" && args.is_empty() {
            return Some("ion_string_new()".to_string());
        }

        // String::from(s: &str) -> String
        if callee == "String::from" && args.len() == 1 {
            let mut code = String::new();
            // Generate string literal or string argument
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);

            // If it's a string literal, use ion_string_from_literal
            // Otherwise, assume it's already a String and clone it
            if arg_code.starts_with('"') {
                // Extract string length
                let len = arg_code.len() - 2; // Remove quotes
                code.push_str("ion_string_from_literal(");
                code.push_str(&arg_code);
                code.push_str(", ");
                code.push_str(&len.to_string());
                code.push(')');
            } else {
                // Assume it's a String, clone it
                code.push_str("ion_string_clone(");
                code.push_str(&arg_code);
                code.push(')');
            }
            return Some(code);
        }

        // String::len(s: &String) -> int
        if callee == "String::len" && args.len() == 1 {
            let mut code = String::new();
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            // arg_code is like "&s" where s is ion_string_t*, so &s is ion_string_t**
            // We need to dereference it: *(&s) = s
            // Remove the leading & if present
            let deref_arg = arg_code.strip_prefix('&').unwrap_or(&arg_code);
            code.push_str("((");
            code.push_str(deref_arg);
            code.push_str(") ? (int)((");
            code.push_str(deref_arg);
            code.push_str(")->len) : 0)");
            return Some(code);
        }

        // String::push_str(s: &mut String, other: &str)
        if callee == "String::push_str" && args.len() == 2 {
            let mut code = String::new();
            let mut str_code = String::new();
            let old_output = std::mem::replace(&mut self.output, str_code);
            self.generate_expr(&args[0]);
            str_code = std::mem::replace(&mut self.output, old_output);
            // Remove leading & if present
            let deref_str = str_code.strip_prefix('&').unwrap_or(&str_code);

            let mut other_code = String::new();
            let old_output = std::mem::replace(&mut self.output, other_code);
            self.generate_expr(&args[1]);
            other_code = std::mem::replace(&mut self.output, old_output);

            code.push_str("ion_string_push_str(");
            code.push_str(deref_str);
            code.push_str(", ");
            if other_code.starts_with('"') {
                let len = other_code.len() - 2;
                code.push_str(&other_code);
                code.push_str(", ");
                code.push_str(&len.to_string());
            } else {
                // For String type, remove & if present
                let deref_other = other_code.strip_prefix('&').unwrap_or(&other_code);
                code.push_str(deref_other);
                code.push_str(", (");
                code.push_str(deref_other);
                code.push_str(") ? (");
                code.push_str(deref_other);
                code.push_str(")->len : 0");
            }
            code.push(')');
            return Some(code);
        }

        None
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

        // Generate constructor functions for each variant
        for (variant_idx, variant) in enum_decl.variants.iter().enumerate() {
            let constructor_name = format!("{}_{}_new", enum_name, variant.name);
            self.write(&format!("static {} {}", enum_name, constructor_name));
            self.write("(");

            // Handle struct variants with named fields
            if let Some(ref named_fields) = variant.named_fields {
                for (i, (field_name, field_ty)) in named_fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!("{} {}", self.type_to_c(field_ty), field_name));
                }
            } else {
                // Handle tuple variants with positional arguments
                for (i, payload_ty) in variant.payload_types.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!("{} arg{}", self.type_to_c(payload_ty), i));
                }
            }

            self.writeln(") {");
            self.indent_level += 1;
            self.write_indent();
            self.write(&format!(
                "{} result = {{ .tag = {}, .data = {{",
                enum_name, variant_idx
            ));

            let has_payloads = !variant.payload_types.is_empty() || variant.named_fields.is_some();
            if has_payloads {
                self.write(&format!(" .variant_{} = {{", variant_idx));

                // Handle struct variants with named fields
                if let Some(ref named_fields) = variant.named_fields {
                    for (i, (field_name, _)) in named_fields.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write(&format!(" .{} = {}", field_name, field_name));
                    }
                } else {
                    // Handle tuple variants with positional arguments
                    for (i, _) in variant.payload_types.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write(&format!(" .arg{} = arg{}", i, i));
                    }
                }

                self.write(" }");
            }
            self.writeln(" } };");
            self.write_indent();
            self.writeln("return result;");
            self.indent_level -= 1;
            self.writeln("}");
            self.writeln("");
        }
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
            let string_methods = ["push_str", "len"];

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
        let string_methods = ["push_str", "len"];

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

    fn generate_match_block(&mut self, expr: &IREexpr, enum_type: &str, arms: &[IRMatchArm]) {
        // Generate match as a block (for statement context)
        // Get monomorphized enum name if it's generic
        let enum_decl = self.enum_map.get(enum_type).cloned();

        // Try to extract type from the expression if it's a Call to Vec::pop or Vec::get
        let (monomorphized_enum_name, type_params) = if let IREexpr::Call {
            callee,
            return_type,
            ..
        } = expr
        {
            // Check if this is a Vec::pop/Vec::get or METHOD::pop/METHOD::get call
            let is_vec_pop_or_get = callee == "Vec::pop"
                || callee == "Vec::get"
                || callee == "METHOD::pop"
                || callee == "METHOD::get";

            // If matching on Vec::pop or Vec::get result, extract element type from Vec argument
            if is_vec_pop_or_get {
                // Extract element type from Vec<T> in generic_instantiations
                // Look for Vec_* types and extract the element type
                if let Some(elem_type) =
                    self.generic_instantiations
                        .iter()
                        .find_map(|(_mono_name, (base, params))| {
                            if base == "Vec" && params.len() == 1 {
                                Some(params[0].clone())
                            } else {
                                None
                            }
                        })
                {
                    // We have the element type, construct Option<T>
                    let mono_name = mangle_type_name("Option", std::slice::from_ref(&elem_type));
                    (mono_name, vec![elem_type])
                } else {
                    // Fall back: if we have Vec types, use the first one's element type
                    // Otherwise, try to extract from return_type or fall back
                    if let Some(Type::Generic { name, params }) = return_type
                        && name == "Option"
                        && params.len() == 1
                    {
                        let mono_name = mangle_type_name("Option", params);
                        (mono_name, params.clone())
                    } else {
                        // Fall back to searching in generic_instantiations
                        self.find_enum_instantiation(enum_type, enum_decl.as_ref())
                    }
                }
            } else if let Some(Type::Generic { name, params }) = return_type
                && name == "Option"
                && params.len() == 1
            {
                // We have Option<T> from return_type, compute the monomorphized name
                let mono_name = mangle_type_name("Option", params);
                (mono_name, params.clone())
            } else {
                // Fall back to searching in generic_instantiations
                self.find_enum_instantiation(enum_type, enum_decl.as_ref())
            }
        } else {
            // Fall back to searching in generic_instantiations
            let (name, params) = self.find_enum_instantiation(enum_type, enum_decl.as_ref());

            // If we found params, use them. Otherwise check for generic Option as last resort
            if !params.is_empty() {
                // Already have type parameters, use them
                (name, params)
            } else if enum_type == "Option" {
                // Check if Option enum is generic (has generics in its declaration)
                let is_generic = enum_decl
                    .as_ref()
                    .map(|decl| !decl.generics.is_empty())
                    .unwrap_or(false);

                if is_generic {
                    // It's a generic Option but we don't have the params yet
                    // Check if we have it in generic_instantiations
                    if let Some((mono_name, params)) = self
                        .generic_instantiations
                        .iter()
                        .find(|(_, (base, _))| base == "Option")
                        .map(|(mono, (_, params))| (mono.clone(), params.clone()))
                    {
                        (mono_name, params)
                    } else {
                        // Last resort: assume int for generic Option
                        let mono_name = mangle_type_name("Option", &[Type::Int]);
                        (mono_name, vec![Type::Int])
                    }
                } else {
                    // Non-generic Option, use the base name as-is
                    (name, params)
                }
            } else {
                // For other enum types, use the result from find_enum_instantiation
                (name, params)
            }
        };

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
            let is_vec_pop_or_get = callee == "Vec::pop"
                || callee == "Vec::get"
                || callee == "METHOD::pop"
                || callee == "METHOD::get"
                || callee.starts_with("ion_vec_pop")
                || callee.starts_with("ion_vec_get");

            // Also check if return type is Option<T> - runtime functions return void* that need casting
            let returns_option = if let Some(Type::Generic { name, .. }) = return_type {
                name == "Option"
            } else {
                false
            };

            is_vec_pop_or_get || returns_option
        } else {
            false
        };

        if needs_cast {
            // Cast void* to Option_T* and dereference
            self.write(&format!(
                "{} {} = *(({}*)(",
                monomorphized_enum_name, match_var_name, monomorphized_enum_name
            ));
            self.generate_expr(expr);
            self.writeln("));");
        } else {
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
        self.indent_level += 1;
        for arm in arms {
            self.generate_match_arm(
                arm,
                &monomorphized_enum_name,
                enum_decl.as_ref(),
                &match_var_name,
                &type_params,
            );
        }
        self.indent_level -= 1;
        self.write_indent();
        self.writeln("}");
    }

    fn generate_match_arm(
        &mut self,
        arm: &IRMatchArm,
        enum_type: &str,
        enum_decl: Option<&EnumDecl>,
        match_var_name: &str,
        type_params: &[Type],
    ) {
        match &arm.pattern {
            IRPattern::Variant {
                enum_name: _,
                variant,
                sub_patterns,
                named_fields,
            } => {
                // Look up variant index from enum declaration
                let variant_idx = enum_decl
                    .and_then(|e| e.variants.iter().position(|v| v.name == *variant))
                    .unwrap_or(0); // Fallback to 0 if not found (should not happen in well-typed code)

                self.write_indent();
                self.writeln(&format!("case {}: // {}", variant_idx, variant));
                self.indent_level += 1;

                // Extract payloads into pattern bindings
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
                                        self.write_indent();
                                        self.write(&format!(
                                            "{} {} = {}.data.variant_{}.arg{};",
                                            self.type_to_c(&concrete_payload_ty),
                                            name,
                                            match_var_name,
                                            variant_idx,
                                            i
                                        ));
                                        self.writeln("");
                                    }
                                    IRPattern::Wildcard => {
                                        // Wildcard - don't extract, payload is ignored
                                    }
                                    IRPattern::Variant { .. } => {
                                        // Nested variant pattern - for now, extract to temp and match recursively
                                        // This is a simplified version - full implementation would recurse
                                        let temp_name = format!("_payload_{}", i);
                                        self.write_indent();
                                        self.write(&format!(
                                            "{} {} = {}.data.variant_{}.arg{};",
                                            self.type_to_c(&concrete_payload_ty),
                                            temp_name,
                                            match_var_name,
                                            variant_idx,
                                            i
                                        ));
                                        self.writeln("");
                                    }
                                }
                            } else {
                                // No pattern specified - treat as wildcard
                            }
                        }
                    }
                }

                self.generate_block(&arm.body);
                self.write_indent();
                self.writeln("break;");
                self.indent_level -= 1;
            }
            IRPattern::Wildcard => {
                self.write_indent();
                self.writeln("default:");
                self.indent_level += 1;
                self.generate_block(&arm.body);
                self.write_indent();
                self.writeln("break;");
                self.indent_level -= 1;
            }
            IRPattern::Binding { name } => {
                // Binding pattern matches any variant - just assign match_val to the binding
                self.write_indent();
                self.writeln(&format!("default: // binding {}", name));
                self.indent_level += 1;
                self.write_indent();
                self.writeln(&format!("{} {} = match_val;", enum_type, name));
                self.generate_block(&arm.body);
                self.write_indent();
                self.writeln("break;");
                self.indent_level -= 1;
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
        IREexpr::Send { channel, value } => {
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
            let vec_name = format!("Vec_{}", mangle_type_name(&type_to_c_impl(elem_type), &[]));
            vec_types.insert(vec_name);
            // Recursively collect nested Vec types
            collect_vec_types_from_type(elem_type, vec_types);
        }
        Type::Generic { name, params } if name == "Vec" && params.len() == 1 => {
            let vec_name = format!("Vec_{}", mangle_type_name(&type_to_c_impl(&params[0]), &[]));
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
        }
        IRStmt::Spawn(_) | IRStmt::Defer(_) => {}
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
        IREexpr::Send { channel, value } => {
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
    }
}

impl Codegen {
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

        // Generate constructor functions for each variant
        for (variant_idx, variant) in decl.variants.iter().enumerate() {
            let constructor_name = format!("{}_{}_new", monomorphized_name, variant.name);
            self.write(&format!(
                "static {} {}",
                monomorphized_name, constructor_name
            ));
            self.write("(");
            for (i, payload_ty) in variant.payload_types.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                let substituted_ty = substitute_generic_types(payload_ty, &substitutions);
                self.write(&format!("{} arg{}", self.type_to_c(&substituted_ty), i));
            }
            self.writeln(") {");
            self.indent_level += 1;
            self.write_indent();
            self.write(&format!(
                "{} result = {{ .tag = {}, .data = {{",
                monomorphized_name, variant_idx
            ));
            if !variant.payload_types.is_empty() {
                self.write(&format!(" .variant_{} = {{", variant_idx));
                for (i, _) in variant.payload_types.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!(" .arg{} = arg{}", i, i));
                }
                self.write(" }");
            }
            self.writeln(" } };");
            self.write_indent();
            self.writeln("return result;");
            self.indent_level -= 1;
            self.writeln("}");
            self.writeln("");
        }
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

fn collect_generic_from_type(
    ty: &Type,
    instantiations: &mut std::collections::HashMap<String, (String, Vec<Type>)>,
) {
    // Resolve type aliases first to ensure we collect the actual generic types
    // Note: This is a simplified version that doesn't have access to type_aliases
    // For now, we collect both the original type and will handle resolution during codegen
    match ty {
        Type::Generic { name, params } => {
            // Collect Vec types for element type extraction in match expressions
            if name == "Vec" && params.len() == 1 {
                // Use mangled name as key for deduplication, store base name and params
                let key = mangle_type_name(name, params);
                instantiations.insert(key, (name.clone(), params.clone()));
            }
            // Check if this is a user-defined generic type (not Box)
            if name != "Box" && name != "Vec" {
                // Use mangled name as key for deduplication, store base name and params
                let key = mangle_type_name(name, params);
                instantiations.insert(key, (name.clone(), params.clone()));
            }
            // Recursively collect nested generics
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
        }
        IRStmt::Spawn(_) | IRStmt::Defer(_) => {}
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
        IREexpr::Send { channel, value } => {
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
        IREexpr::Match { expr, .. } => {
            collect_generic_from_expr(expr, instantiations);
            // Also collect Option types from Vec::pop/Vec::get calls in match expressions
            if let IREexpr::Call {
                callee,
                return_type,
                ..
            } = expr.as_ref()
            {
                let is_vec_pop_or_get = callee == "Vec::pop"
                    || callee == "Vec::get"
                    || callee == "METHOD::pop"
                    || callee == "METHOD::get";

                if is_vec_pop_or_get {
                    // First try to get Option type from return_type if available
                    if let Some(Type::Generic { name, params }) = return_type
                        && name == "Option"
                        && params.len() == 1
                    {
                        let option_key = mangle_type_name("Option", params);
                        instantiations
                            .entry(option_key)
                            .or_insert_with(|| ("Option".to_string(), params.clone()));
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
    }
}
