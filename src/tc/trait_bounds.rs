use super::*;
use std::collections::HashSet;

pub(crate) const KNOWN_TRAIT_BOUNDS: &[&str] = &["Copy", "Eq", "Send"];

impl TypeChecker {
    pub(crate) fn validate_declared_bounds(
        &self,
        params: &[TypeParam],
        span: Span,
    ) -> Result<(), TypeCheckError> {
        for param in params {
            for bound in &param.bounds {
                if !KNOWN_TRAIT_BOUNDS.contains(&bound.as_str())
                    && !self.capabilities.contains_key(bound)
                {
                    return Err(TypeCheckError::UnknownTraitBound {
                        bound: bound.clone(),
                        span,
                    });
                }
            }
        }
        Ok(())
    }

    pub(crate) fn check_instantiation_bounds(
        &self,
        type_params: &[TypeParam],
        substitutions: &HashMap<String, Type>,
        context: &str,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        for param in type_params {
            if let Some(concrete) = substitutions.get(&param.name) {
                let resolved = self.resolve_type_name(concrete)?;
                for bound in &param.bounds {
                    if !self.satisfies_bound(&resolved, bound) {
                        return Err(TypeCheckError::TraitBoundNotSatisfied {
                            type_name: type_to_string(&resolved),
                            bound: bound.clone(),
                            context: context.to_string(),
                            span,
                        });
                    }
                }
            }
        }
        Ok(())
    }

    pub(crate) fn satisfies_bound(&self, ty: &Type, bound: &str) -> bool {
        let param_name = match ty {
            Type::Struct(name) | Type::Enum(name) => Some(name.as_str()),
            Type::Generic { name, params } if params.is_empty() => Some(name.as_str()),
            _ => None,
        };
        if let Some(name) = param_name
            && let Some(param) = self.lookup_type_param(name)
            && param.bounds.iter().any(|b| b == bound)
        {
            return true;
        }
        match bound {
            "Copy" => self.is_copy_type(ty),
            "Send" => self.is_send(ty),
            "Eq" => self.is_eq_type(ty),
            _ => self.satisfies_capability(ty, bound),
        }
    }

    fn satisfies_capability(&self, ty: &Type, capability: &str) -> bool {
        if capability == "Hash"
            && crate::integer_limits::builtin_hash_type_name(ty)
                .is_some_and(|name| crate::integer_limits::builtin_hash_symbol(name).is_some())
        {
            return true;
        }
        if !self.capabilities.contains_key(capability) {
            return false;
        }
        let owned = match ty {
            Type::Ref { inner, .. } => inner.as_ref(),
            other => other,
        };
        let (base, params) = match owned {
            Type::Struct(name) | Type::Enum(name) => (name.as_str(), Vec::new()),
            Type::Generic { name, params } => (name.as_str(), params.clone()),
            _ => return false,
        };
        self.impls.iter().any(|imp| {
            if imp.capability != capability || imp.type_name != base {
                return false;
            }
            match &imp.target {
                Type::Struct(name) | Type::Enum(name) => name == base && params.is_empty(),
                Type::Generic {
                    name,
                    params: impl_params,
                } => name == base && impl_params.len() == params.len(),
                _ => false,
            }
        })
    }

    /// Types that support `==` and `!=` with correct semantics.
    pub(crate) fn is_eq_type(&self, ty: &Type) -> bool {
        self.is_eq_type_rec(ty, &mut HashSet::new())
    }

    fn is_eq_type_rec(&self, ty: &Type, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Type::Void => false,
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
            | Type::UInt => true,
            Type::Ref { inner, .. } => self.is_eq_type_rec(inner, visiting),
            Type::RawPtr { .. } => false,
            Type::Channel { .. } | Type::Sender { .. } | Type::Receiver { .. } => false,
            Type::Struct(name) => {
                if !visiting.insert(name.clone()) {
                    // Coinductive: cycle edge does not introduce non-Eq by itself.
                    return true;
                }
                let result = if let Some(decl) = self.structs.get(name) {
                    decl.fields
                        .iter()
                        .all(|f| self.is_eq_type_rec(&f.ty, visiting))
                } else {
                    true
                };
                visiting.remove(name);
                result
            }
            Type::Enum(name) => {
                if !visiting.insert(name.clone()) {
                    return true;
                }
                let result = if let Some(decl) = self.enums.get(name) {
                    decl.variants.iter().all(|v| {
                        v.payload_types
                            .iter()
                            .all(|ty| self.is_eq_type_rec(ty, visiting))
                            && v.named_fields.as_ref().is_none_or(|named_fields| {
                                named_fields
                                    .iter()
                                    .all(|(_, ty)| self.is_eq_type_rec(ty, visiting))
                            })
                    })
                } else {
                    true
                };
                visiting.remove(name);
                result
            }
            Type::Generic { params, .. } => params.iter().all(|p| self.is_eq_type_rec(p, visiting)),
            // Box/Vec do not support == (pointer identity is not value Eq).
            Type::Box { .. } | Type::Vec { .. } => false,
            Type::String | Type::Str => true,
            Type::Array { inner, .. } => self.is_eq_type_rec(inner, visiting),
            Type::Slice { .. } => false,
            Type::Tuple { elements } => elements.iter().all(|e| self.is_eq_type_rec(e, visiting)),
            Type::Fn { .. } => true,
            Type::JoinHandle { .. } | Type::File | Type::Allocator | Type::Endpoint { .. } => false,
        }
    }
}
