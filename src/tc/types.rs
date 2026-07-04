use super::*;

impl TypeChecker {
    /// Resolve a type name - if it's Type::Struct(name) but name is actually an enum, convert to Type::Enum(name)
    /// Also resolves type aliases transparently
    pub(crate) fn resolve_type_name(&self, ty: &Type) -> Result<Type, TypeCheckError> {
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
                        .map(|(tp, param_ty)| (tp.name.clone(), param_ty))
                        .collect();
                    let concrete_substitutions: HashMap<String, Type> = alias
                        .generics
                        .iter()
                        .zip(params.iter())
                        .map(|(tp, param_ty)| (tp.name.clone(), (*param_ty).clone()))
                        .collect();
                    self.check_instantiation_bounds(
                        &alias.generics,
                        &concrete_substitutions,
                        &format!("type alias '{}'", name),
                        Span::default(),
                    )?;
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
    pub(crate) fn extract_type_name_for_method(
        ty: &Type,
    ) -> Result<(String, bool, bool), TypeCheckError> {
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
}

use crate::ast::*;

pub(crate) fn fn_type_from_signature(params: &[Param], return_type: &Option<Type>) -> Type {
    Type::Fn {
        params: params.iter().map(|p| p.ty.clone()).collect(),
        return_type: Box::new(return_type.clone().unwrap_or(Type::Void)),
    }
}

pub(crate) fn types_equal(a: &Type, b: &Type) -> bool {
    // Note: Type alias resolution should happen before calling types_equal
    // The caller should use resolve_type_name first
    match (a, b) {
        (Type::Void, Type::Void) => true,
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
        (Type::Str, Type::Str) => true,
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
        (
            Type::Fn {
                params: a_params,
                return_type: a_ret,
            },
            Type::Fn {
                params: b_params,
                return_type: b_ret,
            },
        ) => {
            a_params.len() == b_params.len()
                && a_params
                    .iter()
                    .zip(b_params.iter())
                    .all(|(a, b)| types_equal(a, b))
                && types_equal(a_ret, b_ret)
        }
        _ => false,
    }
}

pub fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Void => "void".to_string(),
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
        Type::Str => "str".to_string(),
        Type::Array { inner, size } => format!("[{}; {}]", type_to_string(inner), size),
        Type::Slice { inner } => format!("[]{}", type_to_string(inner)),
        Type::Sender { elem_type } => format!("Sender<{}>", type_to_string(elem_type)),
        Type::Receiver { elem_type } => format!("Receiver<{}>", type_to_string(elem_type)),
        Type::Tuple { elements } => {
            let elem_strs: Vec<String> = elements.iter().map(type_to_string).collect();
            format!("({})", elem_strs.join(", "))
        }
        Type::Fn {
            params,
            return_type,
        } => {
            let param_strs: Vec<String> = params.iter().map(type_to_string).collect();
            format!(
                "fn({}) -> {}",
                param_strs.join(", "),
                type_to_string(return_type)
            )
        }
    }
}
