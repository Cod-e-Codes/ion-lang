use crate::ast::Type;
use std::collections::HashMap;

/// Element type `T` from `Vec<T>` whether stored as `Type::Vec` or generic `Vec`.
pub fn vec_elem_type(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Vec { elem_type } => Some(elem_type.as_ref()),
        Type::Generic { name, params } if name == "Vec" && params.len() == 1 => Some(&params[0]),
        _ => None,
    }
}

/// `T` from `&Vec<T>`, `&mut Vec<T>`, or owned `Vec<T>`.
pub fn ref_to_vec_elem(ty: &Type) -> Option<&Type> {
    let inner = match ty {
        Type::Ref { inner, .. } => inner.as_ref(),
        other => other,
    };
    vec_elem_type(inner)
}

pub fn is_ref_to_vec(ty: &Type) -> bool {
    matches!(ty, Type::Ref { inner, .. } if vec_elem_type(inner).is_some())
}

/// Walk `expected` vs `actual` and bind generic parameter names in `fn_generics`.
/// Recurses through `&` / `&mut`, `Vec`, `Box`, and nested `Generic` so
/// `fn insert<T>(arena: &mut Arena<T>, ...)` infers `T` from `&mut Arena<int>`.
pub fn infer_generic_substitutions(
    expected: &Type,
    actual: &Type,
    fn_generics: &[String],
) -> HashMap<String, Type> {
    let mut subs = HashMap::new();
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
        (Type::Struct(param_name), actual_ty) | (Type::Enum(param_name), actual_ty)
            if fn_generics.contains(param_name) =>
        {
            subs.insert(param_name.clone(), actual_ty.clone());
        }
        (Type::Generic { name, params }, actual_ty)
            if params.is_empty() && fn_generics.contains(name) =>
        {
            subs.insert(name.clone(), actual_ty.clone());
        }
        (Type::Ref { inner: e, .. }, Type::Ref { inner: a, .. }) => {
            subs.extend(infer_generic_substitutions(e, a, fn_generics));
        }
        (Type::Vec { elem_type: e }, Type::Vec { elem_type: a }) => {
            subs.extend(infer_generic_substitutions(e, a, fn_generics));
        }
        (Type::Box { inner: e }, Type::Box { inner: a }) => {
            subs.extend(infer_generic_substitutions(e, a, fn_generics));
        }
        _ => {}
    }
    subs
}
