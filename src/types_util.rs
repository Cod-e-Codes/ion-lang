use crate::ast::Type;
use std::collections::{HashMap, HashSet};

/// Element type `T` from `Vec<T>` whether stored as `Type::Vec` or generic `Vec`.
pub fn vec_elem_type(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Vec { elem_type } => Some(elem_type.as_ref()),
        Type::Generic { name, params } if name == "Vec" && params.len() == 1 => Some(&params[0]),
        _ => None,
    }
}

pub fn is_value_scrutinee(ty: &Type) -> bool {
    crate::integer_limits::integer_row(ty).is_some()
        || matches!(ty, Type::Bool | Type::String | Type::Struct(_))
}

/// `T` from `[]T`, `[T; N]`, or a reference to either.
pub fn slice_elem_type(ty: &Type) -> Option<Type> {
    let inner = match ty {
        Type::Ref { inner, .. } => inner.as_ref(),
        other => other,
    };
    match inner {
        Type::Slice { inner } | Type::Array { inner, .. } => Some((**inner).clone()),
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
        (
            Type::Array {
                inner: e, size: es, ..
            },
            Type::Array {
                inner: a,
                size: asz,
                ..
            },
        ) if es == asz => {
            subs.extend(infer_generic_substitutions(e, a, fn_generics));
        }
        (Type::Slice { inner: e }, Type::Slice { inner: a }) => {
            subs.extend(infer_generic_substitutions(e, a, fn_generics));
        }
        (Type::Tuple { elements: e }, Type::Tuple { elements: a }) if e.len() == a.len() => {
            for (ee, aa) in e.iter().zip(a.iter()) {
                subs.extend(infer_generic_substitutions(ee, aa, fn_generics));
            }
        }
        (Type::Sender { elem_type: e }, Type::Sender { elem_type: a })
        | (Type::Receiver { elem_type: e }, Type::Receiver { elem_type: a })
        | (Type::Channel { elem_type: e }, Type::Channel { elem_type: a }) => {
            subs.extend(infer_generic_substitutions(e, a, fn_generics));
        }
        (
            Type::Fn {
                params: e,
                return_type: er,
            },
            Type::Fn {
                params: a,
                return_type: ar,
            },
        ) if e.len() == a.len() => {
            for (ee, aa) in e.iter().zip(a.iter()) {
                subs.extend(infer_generic_substitutions(ee, aa, fn_generics));
            }
            subs.extend(infer_generic_substitutions(er, ar, fn_generics));
        }
        _ => {}
    }
    subs
}

pub fn substitute_type_params(ty: &Type, substitutions: &HashMap<String, &Type>) -> Type {
    let owned: HashMap<String, Type> = substitutions
        .iter()
        .map(|(name, ty)| (name.clone(), (*ty).clone()))
        .collect();
    substitute_type(ty, &owned)
}

/// Replace type-parameter names and rebuild every composite around the result.
/// A name already being expanded is left in place so a replacement that mentions
/// itself cannot recurse forever.
pub fn substitute_type(ty: &Type, substitutions: &HashMap<String, Type>) -> Type {
    fn go(
        ty: &Type,
        substitutions: &HashMap<String, Type>,
        expanding: &mut HashSet<String>,
    ) -> Type {
        let param_name = match ty {
            Type::Struct(name) | Type::Enum(name) => Some(name.as_str()),
            Type::Generic { name, .. } => Some(name.as_str()),
            _ => None,
        };
        if let Some(name) = param_name
            && substitutions.contains_key(name)
            && !expanding.contains(name)
        {
            let replacement = substitutions[name].clone();
            expanding.insert(name.to_string());
            let result = go(&replacement, substitutions, expanding);
            expanding.remove(name);
            return result;
        }
        match ty {
            Type::Ref { inner, mutable } => Type::Ref {
                inner: Box::new(go(inner, substitutions, expanding)),
                mutable: *mutable,
            },
            Type::RawPtr { inner } => Type::RawPtr {
                inner: Box::new(go(inner, substitutions, expanding)),
            },
            Type::Box { inner } => Type::Box {
                inner: Box::new(go(inner, substitutions, expanding)),
            },
            Type::Vec { elem_type } => Type::Vec {
                elem_type: Box::new(go(elem_type, substitutions, expanding)),
            },
            Type::Channel { elem_type } => Type::Channel {
                elem_type: Box::new(go(elem_type, substitutions, expanding)),
            },
            Type::Array {
                inner,
                size,
                len_name,
            } => Type::Array {
                inner: Box::new(go(inner, substitutions, expanding)),
                size: *size,
                len_name: len_name.clone(),
            },
            Type::Slice { inner } => Type::Slice {
                inner: Box::new(go(inner, substitutions, expanding)),
            },
            Type::Sender { elem_type } => Type::Sender {
                elem_type: Box::new(go(elem_type, substitutions, expanding)),
            },
            Type::Receiver { elem_type } => Type::Receiver {
                elem_type: Box::new(go(elem_type, substitutions, expanding)),
            },
            Type::Tuple { elements } => Type::Tuple {
                elements: elements
                    .iter()
                    .map(|elem| go(elem, substitutions, expanding))
                    .collect(),
            },
            Type::Fn {
                params,
                return_type,
            } => Type::Fn {
                params: params
                    .iter()
                    .map(|param| go(param, substitutions, expanding))
                    .collect(),
                return_type: Box::new(go(return_type, substitutions, expanding)),
            },
            Type::Generic { name, params } => Type::Generic {
                name: name.clone(),
                params: params
                    .iter()
                    .map(|param| go(param, substitutions, expanding))
                    .collect(),
            },
            Type::Struct(_)
            | Type::Enum(_)
            | Type::Void
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
            | Type::String
            | Type::Str
            | Type::JoinHandle
            | Type::File => ty.clone(),
        }
    }
    go(ty, substitutions, &mut HashSet::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn substitute_walks_raw_ptr_and_nested_params() {
        let mut subs = HashMap::new();
        subs.insert("U".to_string(), Type::Int);
        subs.insert(
            "T".to_string(),
            Type::Vec {
                elem_type: Box::new(Type::Struct("U".to_string())),
            },
        );
        let ty = Type::RawPtr {
            inner: Box::new(Type::Struct("T".to_string())),
        };
        let got = substitute_type(&ty, &subs);
        assert!(matches!(
            got,
            Type::RawPtr { inner }
                if matches!(
                    inner.as_ref(),
                    Type::Vec { elem_type } if matches!(elem_type.as_ref(), Type::Int)
                )
        ));
    }

    #[test]
    fn substitute_stops_when_a_replacement_mentions_itself() {
        let mut subs = HashMap::new();
        subs.insert("T".to_string(), Type::Struct("T".to_string()));
        let got = substitute_type(&Type::Struct("T".to_string()), &subs);
        assert!(matches!(got, Type::Struct(name) if name == "T"));
    }
}
