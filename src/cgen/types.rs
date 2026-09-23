use crate::ast::{Type, TypeAliasDecl};
use std::collections::HashMap;

/// Mangle a type name for C identifier safety
pub(crate) fn mangle_type_name(base: &str, params: &[Type]) -> String {
    if params.is_empty() {
        base.to_string()
    } else {
        let param_strs: Vec<String> = params.iter().map(mangle_type_component).collect();
        format!("{}_{}", base, param_strs.join("_"))
    }
}

/// C typedef name for `[T; N]`, e.g. `[int; 2]` -> `arr_int_2`, `[[int; 2]; 3]` -> `arr_arr_int_2_3`.
pub(crate) fn array_type_name(inner: &Type, size: usize) -> String {
    format!("arr_{}_{}", mangle_type_component(inner), size)
}

fn mangle_type_component(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Bool => "bool".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::String => "String".to_string(),
        Type::Str => "str".to_string(),
        Type::File => "File".to_string(),
        Type::JoinHandle => "JoinHandle".to_string(),
        Type::Struct(name) | Type::Enum(name) => name.clone(),
        Type::Vec { elem_type } => format!("Vec_{}", mangle_type_component(elem_type)),
        Type::Box { inner } => format!("Box_{}", mangle_type_component(inner)),
        Type::Array { inner, size, .. } => array_type_name(inner, *size),
        Type::Generic { name, params } if name == "Vec" && params.len() == 1 => {
            format!("Vec_{}", mangle_type_component(&params[0]))
        }
        Type::Generic { name, params } if name == "Box" && params.len() == 1 => {
            format!("Box_{}", mangle_type_component(&params[0]))
        }
        Type::Ref { inner, .. } => format!("ref_{}", mangle_type_component(inner)),
        Type::Generic { name, params } if params.is_empty() => name.clone(),
        Type::Generic { name, params } => mangle_type_name(name, params),
        _ => type_to_c_impl(ty)
            .chars()
            .map(|ch| match ch {
                ' ' | '*' | '(' | ')' | '[' | ']' => '_',
                other => other,
            })
            .collect(),
    }
}

pub(crate) fn tuple_type_name(elements: &[Type]) -> String {
    let parts: Vec<String> = elements.iter().map(mangle_type_component).collect();
    format!("tuple_{}", parts.join("_"))
}

/// C declarator and initializer for a function's synthetic `ret_val`.
pub(crate) enum RetValDecl {
    /// Fn-pointer return: declarator already names `ret_val`.
    FnPtr { decl: String },
    /// Value return: separate type and initializer.
    Value { ty: String, init: String },
}

pub(crate) fn ret_val_decl(resolved: &Type) -> RetValDecl {
    match resolved {
        Type::Array { inner, .. } => RetValDecl::Value {
            ty: format!("{}*", type_to_c_impl(inner)),
            init: "0".to_string(),
        },
        Type::Fn { .. } => RetValDecl::FnPtr {
            decl: format!("{} = 0", fn_type_to_c_decl(resolved, "ret_val")),
        },
        Type::Box { .. }
        | Type::Vec { .. }
        | Type::String
        | Type::Channel { .. }
        | Type::Ref { .. } => RetValDecl::Value {
            ty: type_to_c_impl(resolved),
            init: "0".to_string(),
        },
        Type::Struct(_) | Type::Enum(_) => RetValDecl::Value {
            ty: type_to_c_impl(resolved),
            init: "{0}".to_string(),
        },
        Type::Tuple { elements } => {
            let name = tuple_type_name(elements);
            RetValDecl::Value {
                ty: name.clone(),
                init: format!("({name}){{0}}"),
            }
        }
        Type::Generic { name, .. } => match name.as_str() {
            "Box" | "Vec" => RetValDecl::Value {
                ty: type_to_c_impl(resolved),
                init: "0".to_string(),
            },
            _ => RetValDecl::Value {
                ty: type_to_c_impl(resolved),
                init: "{0}".to_string(),
            },
        },
        _ => RetValDecl::Value {
            ty: type_to_c_impl(resolved),
            init: "0".to_string(),
        },
    }
}

pub(crate) fn format_ret_val_decl(decl: &RetValDecl) -> String {
    match decl {
        RetValDecl::FnPtr { decl } => format!("{decl};"),
        RetValDecl::Value { ty, init } => format!("{ty} ret_val = {init};"),
    }
}

/// Map a module-qualified Ion callee (`io::print_int`) to a unique C symbol (`io_print_int`).
/// Type-associated builtins (`Box::new`, `Vec::len`, etc.) keep their existing mangling path.
pub(crate) fn mangle_module_callee(callee: &str) -> Option<String> {
    if callee.starts_with("METHOD::") {
        panic!("compiler bug: unresolved method callee {callee}");
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

pub(crate) fn substitute_type_params(
    ty: &Type,
    substitutions: &std::collections::HashMap<String, &Type>,
) -> Type {
    crate::types_util::substitute_type_params(ty, substitutions)
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
                            .map(|(tp, param_ty)| (tp.name.clone(), param_ty))
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
                        .map(|(tp, param_ty)| (tp.name.clone(), param_ty))
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
        Type::Array {
            inner,
            size,
            len_name,
        } => Type::Array {
            inner: Box::new(resolve_type_alias(inner, type_aliases)),
            size: *size,
            len_name: len_name.clone(),
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
    let ptr = fn_type_to_c_ptr(ty);
    let Some(pos) = ptr.rfind("(*)") else {
        return ptr;
    };
    let mut out = String::with_capacity(ptr.len() + name.len());
    out.push_str(&ptr[..pos]);
    out.push_str("(*");
    out.push_str(name);
    out.push(')');
    out.push_str(&ptr[pos + 3..]);
    out
}

/// C prototype/definition header for a function whose return type is `fn(...) -> ...`.
pub(crate) fn fn_type_to_c_function_header(name: &str, param_list: &str, ret_ty: &Type) -> String {
    let ptr = fn_type_to_c_ptr(ret_ty);
    ptr.replacen("(*)", &format!("(*{name}({param_list}))"), 1)
}

pub(crate) fn type_to_c_impl(ty: &Type) -> String {
    match ty {
        Type::Void => "void".to_string(),
        Type::Int
        | Type::I8
        | Type::I16
        | Type::I32
        | Type::I64
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::UInt => crate::integer_limits::integer_row(ty)
            .expect("integer row")
            .c_type
            .to_string(),
        Type::Bool => "int".to_string(), // C doesn't have native bool, use int with 0/1
        Type::F32 => "float".to_string(),
        Type::F64 => "double".to_string(),
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
                    format!("{}*", mangle_type_name("Vec", params))
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
            format!(
                "{}*",
                mangle_type_name("Vec", std::slice::from_ref(elem_type))
            )
        }
        Type::String => "ion_string_t*".to_string(),
        Type::Str => "char".to_string(),
        Type::Array { inner, size, .. } => {
            // Named typedef so the array can appear as a C type specifier
            // (`Box<[T; N]>` -> `arr_T_N*`, nested arrays, `sizeof`).
            array_type_name(inner, *size)
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
        Type::JoinHandle => "ion_thread_t".to_string(),
        Type::File => "ion_file_t".to_string(),
    }
}

// In src/cgen/mod.rs, near your existing type helper functions
pub(crate) fn type_to_c_return_type(ty: &Type) -> String {
    match ty {
        Type::Array { inner, size: _, .. } => {
            // CRITICAL FIX: Array return types in Ion must become pointers in C.
            format!("{}*", type_to_c_impl(inner))
        }
        _ => type_to_c_impl(ty),
    }
}

/// Signed/unsigned C types and bitwidth for defined integer lowering.
pub(crate) fn int_c_repr(ty: &Type) -> Option<(&'static str, &'static str, Option<u32>, bool)> {
    crate::integer_limits::integer_row(ty)
        .map(|row| (row.c_type, row.c_unsigned, row.width, row.signed))
}

/// C-safe literal for `Type::MIN` / `Type::MAX` (avoids `-2147483648`-style overflow in C).
pub(crate) fn c_int_limit(ty: &Type, max: bool) -> String {
    match crate::integer_limits::integer_row(ty) {
        Some(row) => {
            if max {
                row.c_max.to_string()
            } else {
                row.c_min.to_string()
            }
        }
        None => "0".to_string(),
    }
}
