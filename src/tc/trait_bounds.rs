use super::*;

pub(crate) const KNOWN_TRAIT_BOUNDS: &[&str] = &["Copy", "Eq", "Send"];

impl TypeChecker {
    pub(crate) fn validate_declared_bounds(
        &self,
        params: &[TypeParam],
        span: Span,
    ) -> Result<(), TypeCheckError> {
        for param in params {
            for bound in &param.bounds {
                if !KNOWN_TRAIT_BOUNDS.contains(&bound.as_str()) {
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
                    if !KNOWN_TRAIT_BOUNDS.contains(&bound.as_str()) {
                        continue;
                    }
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
        match bound {
            "Copy" => Self::is_copy_type(ty),
            "Send" => self.is_send(ty),
            "Eq" => self.is_eq_type(ty),
            _ => false,
        }
    }

    /// Types that support `==` and `!=` with correct semantics.
    pub(crate) fn is_eq_type(&self, ty: &Type) -> bool {
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
            Type::Ref { inner, .. } => self.is_eq_type(inner),
            Type::RawPtr { .. } => false,
            Type::Channel { .. } | Type::Sender { .. } | Type::Receiver { .. } => false,
            Type::Struct(name) => {
                if let Some(decl) = self.structs.get(name) {
                    decl.fields.iter().all(|f| self.is_eq_type(&f.ty))
                } else {
                    true
                }
            }
            Type::Enum(name) => {
                if let Some(decl) = self.enums.get(name) {
                    decl.variants.iter().all(|v| {
                        v.payload_types.iter().all(|ty| self.is_eq_type(ty))
                            && v.named_fields.as_ref().is_none_or(|named_fields| {
                                named_fields.iter().all(|(_, ty)| self.is_eq_type(ty))
                            })
                    })
                } else {
                    true
                }
            }
            Type::Generic { params, .. } => params.iter().all(|p| self.is_eq_type(p)),
            Type::Box { .. } | Type::Vec { .. } => false,
            Type::String | Type::Str => true,
            Type::Array { inner, .. } => self.is_eq_type(inner),
            Type::Slice { .. } => false,
            Type::Tuple { elements } => elements.iter().all(|e| self.is_eq_type(e)),
            Type::Fn { .. } => true,
        }
    }
}
