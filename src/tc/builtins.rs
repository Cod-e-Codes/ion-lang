use super::*;

impl TypeChecker {
    /// Check if a call expression is to a built-in function and type-check it.
    /// Returns Some(return_type) if it's a built-in, None otherwise.
    pub(crate) fn check_builtin_call(
        &mut self,
        call_expr: &CallExpr,
    ) -> Result<Option<Type>, TypeCheckError> {
        let callee = &call_expr.callee;

        // Box::new<T>(value: T) -> Box<T>
        if callee.starts_with("Box::new") {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let value_ty = self.check_expr(&call_expr.args[0])?;
            return Ok(Some(Type::Box {
                inner: Box::new(value_ty),
            }));
        }

        // Box::unwrap<T>(box: Box<T>) -> T
        if callee == "Box::unwrap" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let box_ty = self.check_expr(&call_expr.args[0])?;
            return match box_ty {
                Type::Box { inner } => Ok(Some(*inner)),
                _ => Err(TypeCheckError::TypeMismatch {
                    expected: "Box<T>".to_string(),
                    got: type_to_string(&box_ty),
                    span: call_expr.span,
                }),
            };
        }

        // Vec::new<T>() -> Vec<T>
        // Note: We can't infer T from empty args, so this requires type annotation
        // For now, we'll return Vec<Int> as a default and let codegen handle it
        if callee == "Vec::new" {
            if !call_expr.args.is_empty() {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "0 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            // Return Vec<Int> as default - actual type should come from context
            return Ok(Some(Type::Vec {
                elem_type: Box::new(Type::Int),
            }));
        }

        // Vec::with_capacity<T>(cap: int) -> Vec<T>
        if callee == "Vec::with_capacity" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let cap_ty = self.check_expr(&call_expr.args[0])?;
            if !self.is_integer_type(&cap_ty) {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "integer type".to_string(),
                    got: type_to_string(&cap_ty),
                    span: call_expr.args[0].span(),
                });
            }
            // Return Vec<Int> as default
            return Ok(Some(Type::Vec {
                elem_type: Box::new(Type::Int),
            }));
        }

        // Vec::len<T>(vec: &Vec<T>) -> int
        if callee == "Vec::len" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: false,
            } = vec_ty
                && let Type::Vec { .. } = **inner_ty
            {
                return Ok(Some(Type::Int));
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::capacity<T>(vec: &Vec<T>) -> int
        if callee == "Vec::capacity" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: false,
            } = vec_ty
                && let Type::Vec { .. } = **inner_ty
            {
                return Ok(Some(Type::Int));
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::push<T>(vec: &mut Vec<T>, value: T)
        if callee == "Vec::push" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            let value_ty = self.check_expr(&call_expr.args[1])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = vec_ty
                && let Type::Vec { ref elem_type } = **inner_ty
            {
                let resolved_value_ty = self.resolve_type_name(&value_ty)?;
                let resolved_elem_ty = self.resolve_type_name(elem_type)?;
                let numeric_coerced =
                    Self::can_coerce_numeric(&resolved_value_ty, &resolved_elem_ty);
                if !numeric_coerced && !types_equal(&resolved_value_ty, &resolved_elem_ty) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: type_to_string(elem_type),
                        got: type_to_string(&value_ty),
                        span: call_expr.args[1].span(),
                    });
                }
                return Ok(Some(Type::Void)); // void return
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::pop<T>(vec: &mut Vec<T>) -> Option<T>
        if callee == "Vec::pop" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = vec_ty
                && let Type::Vec { ref elem_type } = **inner_ty
            {
                // Check if Option<T> enum exists
                if self.enums.contains_key("Option") {
                    return Ok(Some(Type::Generic {
                        name: "Option".to_string(),
                        params: vec![(**elem_type).clone()],
                    }));
                } else {
                    // Return Option<Int> as fallback
                    return Ok(Some(Type::Generic {
                        name: "Option".to_string(),
                        params: vec![Type::Int],
                    }));
                }
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::get<T>(vec: &Vec<T>, index: int) -> Option<T>
        if callee == "Vec::get" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let index_ty = self.check_expr(&call_expr.args[1])?;
            if !self.is_integer_type(&index_ty) {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "integer type".to_string(),
                    got: type_to_string(&index_ty),
                    span: call_expr.args[1].span(),
                });
            }

            let elem_type = self.vec_elem_type_from_receiver(&call_expr.args[0]).or_else(|| {
                let vec_ty = self.check_expr(&call_expr.args[0]).ok()?;
                if let Type::Ref {
                    inner,
                    mutable: false,
                } = vec_ty
                    && let Type::Vec { elem_type } = *inner
                {
                    return Some(*elem_type);
                }
                None
            });

            if let Some(elem_type) = elem_type {
                let resolved_elem = self.resolve_type_name(&elem_type)?;
                return Ok(Some(Type::Generic {
                    name: "Option".to_string(),
                    params: vec![resolved_elem],
                }));
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::set<T>(vec: &mut Vec<T>, index: int, value: T) -> int
        if callee == "Vec::set" {
            if call_expr.args.len() != 3 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "3 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            let index_ty = self.check_expr(&call_expr.args[1])?;
            let value_ty = self.check_expr(&call_expr.args[2])?;
            if !self.is_integer_type(&index_ty) {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "integer type".to_string(),
                    got: type_to_string(&index_ty),
                    span: call_expr.args[1].span(),
                });
            }
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = vec_ty
                && let Type::Vec { ref elem_type } = **inner_ty
            {
                let resolved_value_ty = self.resolve_type_name(&value_ty)?;
                let resolved_elem_ty = self.resolve_type_name(elem_type)?;
                let numeric_coerced =
                    Self::can_coerce_numeric(&resolved_value_ty, &resolved_elem_ty);
                if !numeric_coerced && !types_equal(&resolved_value_ty, &resolved_elem_ty) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: type_to_string(elem_type),
                        got: type_to_string(&value_ty),
                        span: call_expr.args[2].span(),
                    });
                }
                return Ok(Some(Type::Int)); // 0 on success, -1 on failure
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // String::new() -> String
        if callee == "String::new" {
            if !call_expr.args.is_empty() {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "0 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            return Ok(Some(Type::String));
        }

        // String::from(s: &str) -> String
        // Note: &str is not a real type in Ion yet, so we'll accept String for now
        if callee == "String::from" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let _arg_ty = self.check_expr(&call_expr.args[0])?;
            // Accept any type for now (String or string literal)
            return Ok(Some(Type::String));
        }

        // String::len(s: &String) -> int
        if callee == "String::len" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let str_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: false,
            } = str_ty
                && let Type::String = **inner_ty
            {
                return Ok(Some(Type::Int));
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&String".to_string(),
                got: type_to_string(&str_ty),
                span: call_expr.args[0].span(),
            });
        }

        // String::push_str(s: &mut String, other: &str)
        if callee == "String::push_str" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let str_ty = self.check_expr(&call_expr.args[0])?;
            let _other_ty = self.check_expr(&call_expr.args[1])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = str_ty
                && let Type::String = **inner_ty
            {
                return Ok(Some(Type::Void)); // void return
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut String".to_string(),
                got: type_to_string(&str_ty),
                span: call_expr.args[0].span(),
            });
        }

        // String::push_byte(s: &mut String, b: u8)
        if callee == "String::push_byte" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let str_ty = self.check_expr(&call_expr.args[0])?;
            let byte_ty = self.check_expr(&call_expr.args[1])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: true,
            } = str_ty
                && let Type::String = **inner_ty
            {
                if !matches!(byte_ty, Type::U8) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "u8".to_string(),
                        got: type_to_string(&byte_ty),
                        span: call_expr.args[1].span(),
                    });
                }
                return Ok(Some(Type::Void)); // void return
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&mut String".to_string(),
                got: type_to_string(&str_ty),
                span: call_expr.args[0].span(),
            });
        }

        // channel<T>() -> (Sender<T>, Receiver<T>)
        // This should only be called with tuple destructuring, handled in check_stmt
        if callee == "channel" {
            if !call_expr.args.is_empty() {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "0 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            // channel() needs type context from tuple destructuring
            // Return None so it can be handled in tuple destructuring context
            return Ok(None);
        }

        // Not a built-in function
        Ok(None)
    }

    /// Element type for `&vec` when `vec` is a local binding (for-loop temps keep full `Vec<T>`).
    fn vec_elem_type_from_receiver(&self, receiver: &Expr) -> Option<Type> {
        if let Expr::Ref(r) = receiver
            && let Expr::Var(v) = r.inner.as_ref()
            && let Some(info) = self.variables.get(&v.name)
            && let Type::Vec { elem_type } = &info.ty
        {
            return Some((**elem_type).clone());
        }
        None
    }
}
