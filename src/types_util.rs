use crate::ast::Type;

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
