use crate::ast::{Type, TypeAliasDecl};
use std::collections::HashMap;

/// Mangle a type name for C identifier safety
pub(crate) fn mangle_type_name(base: &str, params: &[Type]) -> String {
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

pub(crate) fn tuple_type_name(elements: &[Type]) -> String {
    let parts: Vec<String> = elements.iter().map(type_to_c_impl).collect();
    format!("tuple_{}", parts.join("_"))
}

/// Map a module-qualified Ion callee (`io::print_int`) to a unique C symbol (`io_print_int`).
/// Type-associated builtins (`Box::new`, `Vec::len`, etc.) keep their existing mangling path.
pub(crate) fn mangle_module_callee(callee: &str) -> Option<String> {
    if callee.starts_with("METHOD::") {
        return None;
    }
    let parts: Vec<&str> = callee.split("::").collect();
    if parts.len() != 2 {
        return None;
    }
    let (module, func) = (parts[0], parts[1]);
    if matches!(module, "Box" | "Vec" | "String" | "Option") {
        return None;
    }
    Some(format!("{}_{}", module, func))
}

/// Substitute generic type parameters in a type using the substitution map
pub(crate) fn substitute_type_params(
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
        Type::Fn {
            params,
            return_type,
        } => Type::Fn {
            params: params
                .iter()
                .map(|p| substitute_type_params(p, substitutions))
                .collect(),
            return_type: Box::new(substitute_type_params(return_type, substitutions)),
        },
        // Primitive types don't need substitution
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
        | Type::String => ty.clone(),
    }
}

/// Resolve type aliases recursively
pub(crate) fn resolve_type_alias(ty: &Type, type_aliases: &HashMap<String, TypeAliasDecl>) -> Type {
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
        Type::Fn {
            params,
            return_type,
        } => Type::Fn {
            params: params
                .iter()
                .map(|p| resolve_type_alias(p, type_aliases))
                .collect(),
            return_type: Box::new(resolve_type_alias(return_type, type_aliases)),
        },
        _ => ty.clone(),
    }
}

pub(crate) fn fn_type_to_c_ptr(ty: &Type) -> String {
    let Type::Fn {
        params,
        return_type,
    } = ty
    else {
        panic!("fn_type_to_c_ptr called on non-fn type");
    };
    let ret = type_to_c_impl(return_type);
    let param_strs: Vec<String> = params.iter().map(type_to_c_impl).collect();
    format!("{} (*)({})", ret, param_strs.join(", "))
}

pub(crate) fn fn_type_to_c_decl(ty: &Type, name: &str) -> String {
    let Type::Fn {
        params,
        return_type,
    } = ty
    else {
        panic!("fn_type_to_c_decl called on non-fn type");
    };
    let ret = type_to_c_impl(return_type);
    let param_strs: Vec<String> = params.iter().map(type_to_c_impl).collect();
    format!("{} (*{})({})", ret, name, param_strs.join(", "))
}

/// C prototype/definition header for a function whose return type is `fn(...) -> ...`.
pub(crate) fn fn_type_to_c_function_header(name: &str, param_list: &str, ret_ty: &Type) -> String {
    let ptr = fn_type_to_c_ptr(ret_ty);
    ptr.replacen("(*)", &format!("(*{name}({param_list}))"), 1)
}

pub(crate) fn type_to_c_impl(ty: &Type) -> String {
    match ty {
        Type::Void => "void".to_string(),
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
        Type::Tuple { elements } => tuple_type_name(elements),
        Type::Fn { .. } => fn_type_to_c_ptr(ty),
    }
}

// In src/cgen/mod.rs, near your existing type helper functions
pub(crate) fn type_to_c_return_type(ty: &Type) -> String {
    match ty {
        Type::Array { inner, size: _ } => {
            // CRITICAL FIX: Array return types in Ion must become pointers in C.
            format!("{}*", type_to_c_impl(inner))
        }
        _ => type_to_c_impl(ty),
    }
}
