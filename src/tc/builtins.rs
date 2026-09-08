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
            let expected_inner = match &self.expr_expected {
                Some(Type::Box { inner }) => Some(inner.as_ref().clone()),
                _ => match &self.current_return_type {
                    Some(Type::Box { inner }) => Some(inner.as_ref().clone()),
                    _ => None,
                },
            };
            let value_ty = if let Some(inner) = &expected_inner {
                self.check_expr_with_expected(&call_expr.args[0], inner)?
            } else {
                self.check_expr(&call_expr.args[0])?
            };
            let box_ty = Type::Box {
                inner: Box::new(value_ty),
            };
            self.check_no_off_stack_reference(&box_ty, call_expr.span)?;
            return Ok(Some(box_ty));
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
            // `&mut Vec<T>` reborrows as `&Vec<T>` (method calls on `&mut Arena` fields).
            if crate::types_util::is_ref_to_vec(&vec_ty) {
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
            if crate::types_util::is_ref_to_vec(&vec_ty) {
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
            let expected_elem = match &vec_ty {
                Type::Ref {
                    inner,
                    mutable: true,
                } => match inner.as_ref() {
                    Type::Vec { elem_type } => Some(elem_type.as_ref().clone()),
                    _ => None,
                },
                _ => None,
            };
            let value_ty = if let Some(elem) = &expected_elem {
                self.check_expr_with_expected(&call_expr.args[1], elem)?
            } else {
                self.check_expr(&call_expr.args[1])?
            };
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
            self.check_integer_operand(&call_expr.args[1])?;

            let elem_type = self.vec_elem_type_from_vec_arg(&call_expr.args[0]);

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

        // Vec::get_ref<T>(vec: &Vec<T>, index: int) -> Option<&T>
        if callee == "Vec::get_ref" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            self.check_integer_operand(&call_expr.args[1])?;

            let elem_type = self.vec_elem_type_from_vec_arg(&call_expr.args[0]);

            if let Some(elem_type) = elem_type {
                let resolved_elem = self.resolve_type_name(&elem_type)?;
                return Ok(Some(Type::Generic {
                    name: "Option".to_string(),
                    params: vec![Type::Ref {
                        inner: Box::new(resolved_elem),
                        mutable: false,
                    }],
                }));
            }
            let vec_ty = self.check_expr(&call_expr.args[0])?;
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Vec<T>".to_string(),
                got: type_to_string(&vec_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Slice::len<T>(s: &[]T) -> int
        if callee == "Slice::len" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            if self
                .slice_elem_type_from_slice_arg(&call_expr.args[0])
                .is_some()
            {
                return Ok(Some(Type::Int));
            }
            let slice_ty = self.check_expr(&call_expr.args[0])?;
            return Err(TypeCheckError::TypeMismatch {
                expected: "&[]T".to_string(),
                got: type_to_string(&slice_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Slice::get_ref<T>(s: &[]T, index: int) -> Option<&T>
        if callee == "Slice::get_ref" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            self.check_integer_operand(&call_expr.args[1])?;

            let elem_type = self.slice_elem_type_from_slice_arg(&call_expr.args[0]);

            if let Some(elem_type) = elem_type {
                let resolved_elem = self.resolve_type_name(&elem_type)?;
                return Ok(Some(Type::Generic {
                    name: "Option".to_string(),
                    params: vec![Type::Ref {
                        inner: Box::new(resolved_elem),
                        mutable: false,
                    }],
                }));
            }
            let slice_ty = self.check_expr(&call_expr.args[0])?;
            return Err(TypeCheckError::TypeMismatch {
                expected: "&[]T".to_string(),
                got: type_to_string(&slice_ty),
                span: call_expr.args[0].span(),
            });
        }

        // Vec::set<T>(vec: &mut Vec<T>, index: int, value: T) -> SetResult
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
            let expected_elem = match &vec_ty {
                Type::Ref {
                    inner,
                    mutable: true,
                } => match inner.as_ref() {
                    Type::Vec { elem_type } => Some(elem_type.as_ref().clone()),
                    _ => None,
                },
                _ => None,
            };
            let value_ty = if let Some(elem) = &expected_elem {
                self.check_expr_with_expected(&call_expr.args[2], elem)?
            } else {
                self.check_expr(&call_expr.args[2])?
            };
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
                self.require_named_enum(
                    "SetResult",
                    &[("Ok", false), ("OutOfBounds", false)],
                    call_expr.span,
                )?;
                return Ok(Some(Type::Enum("SetResult".to_string())));
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
        if callee == "String::from" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let arg_ty = self.check_expr(&call_expr.args[0])?;
            let resolved_arg_ty = self.resolve_type_name(&arg_ty)?;
            if !Self::can_coerce_to_str_ref(&resolved_arg_ty)
                && !matches!(call_expr.args[0], Expr::StringLit(_))
            {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "&str".to_string(),
                    got: type_to_string(&resolved_arg_ty),
                    span: call_expr.args[0].span(),
                });
            }
            return Ok(Some(Type::String));
        }

        // String::from_utf8(bytes: Vec<u8>) -> Option<String>
        if callee == "String::from_utf8" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let bytes_ty = self.check_expr(&call_expr.args[0])?;
            let resolved = self.resolve_type_name(&bytes_ty)?;
            match resolved {
                Type::Vec { elem_type } if matches!(*elem_type, Type::U8) => {
                    return Ok(Some(Type::Generic {
                        name: "Option".to_string(),
                        params: vec![Type::String],
                    }));
                }
                other => {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "Vec<u8>".to_string(),
                        got: type_to_string(&other),
                        span: call_expr.args[0].span(),
                    });
                }
            }
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

        // String::get(s: &String, index: int) -> Option<u8>
        if callee == "String::get" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            self.check_integer_operand(&call_expr.args[1])?;
            let str_ty = self.check_expr(&call_expr.args[0])?;
            if let Type::Ref {
                inner: ref inner_ty,
                mutable: false,
            } = str_ty
                && let Type::String = **inner_ty
            {
                return Ok(Some(Type::Generic {
                    name: "Option".to_string(),
                    params: vec![Type::U8],
                }));
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

        // channel<T>() / channel<T>(cap) -> (Sender<T>, Receiver<T>)
        // Tuple destructuring is handled in check_stmt.
        if callee == "channel" {
            self.check_channel_capacity_args(call_expr)?;
            return Ok(None);
        }

        // clone_sender(&Sender<T>) -> Sender<T>
        if callee == "clone_sender" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let sender_ref = self.check_expr(&call_expr.args[0])?;
            return match sender_ref {
                Type::Ref {
                    inner,
                    mutable: false,
                } => match *inner {
                    Type::Sender { elem_type } => Ok(Some(Type::Sender { elem_type })),
                    other => Err(TypeCheckError::TypeMismatch {
                        expected: "&Sender<T>".to_string(),
                        got: type_to_string(&other),
                        span: call_expr.args[0].span(),
                    }),
                },
                other => Err(TypeCheckError::TypeMismatch {
                    expected: "&Sender<T>".to_string(),
                    got: type_to_string(&other),
                    span: call_expr.args[0].span(),
                }),
            };
        }

        if callee == "try_send" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let sender_ref = self.check_expr(&call_expr.args[0])?;
            let elem_type = match sender_ref {
                Type::Ref { inner, .. } => match *inner {
                    Type::Sender { elem_type } => *elem_type,
                    other => {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "&Sender<T>".to_string(),
                            got: type_to_string(&other),
                            span: call_expr.args[0].span(),
                        });
                    }
                },
                other => {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "&Sender<T>".to_string(),
                        got: type_to_string(&other),
                        span: call_expr.args[0].span(),
                    });
                }
            };
            let _value_ty = self.check_expr_with_expected(&call_expr.args[1], &elem_type)?;
            self.require_named_enum(
                "TrySendResult",
                &[("Sent", false), ("Full", true), ("Closed", true)],
                call_expr.span,
            )?;
            return Ok(Some(Type::Generic {
                name: "TrySendResult".to_string(),
                params: vec![elem_type],
            }));
        }

        if callee == "try_recv" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let recv_ref = self.check_expr(&call_expr.args[0])?;
            let elem_type = match recv_ref {
                Type::Ref {
                    inner,
                    mutable: true,
                } => match *inner {
                    Type::Receiver { elem_type } => *elem_type,
                    other => {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: "&mut Receiver<T>".to_string(),
                            got: type_to_string(&other),
                            span: call_expr.args[0].span(),
                        });
                    }
                },
                other => {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: "&mut Receiver<T>".to_string(),
                        got: type_to_string(&other),
                        span: call_expr.args[0].span(),
                    });
                }
            };
            self.require_named_enum(
                "TryRecvResult",
                &[("Msg", true), ("Empty", false), ("Closed", false)],
                call_expr.span,
            )?;
            return Ok(Some(Type::Generic {
                name: "TryRecvResult".to_string(),
                params: vec![elem_type],
            }));
        }

        if callee == "join" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let handle_ty = self.check_expr(&call_expr.args[0])?;
            if !matches!(handle_ty, Type::JoinHandle) {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "JoinHandle".to_string(),
                    got: type_to_string(&handle_ty),
                    span: call_expr.args[0].span(),
                });
            }
            return Ok(Some(Type::Void));
        }

        if callee == "Arena::get_ref" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let arena_ty = self.check_expr(&call_expr.args[0])?;
            let handle_ty = self.check_expr(&call_expr.args[1])?;
            let handle_ok = match &handle_ty {
                Type::Struct(name) | Type::Generic { name, .. } => name == "Handle",
                _ => false,
            };
            if !handle_ok {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "Handle".to_string(),
                    got: type_to_string(&handle_ty),
                    span: call_expr.args[1].span(),
                });
            }
            let elem_type = match &arena_ty {
                Type::Ref { inner, .. } => match inner.as_ref() {
                    Type::Generic { name, params } if name == "Arena" && params.len() == 1 => {
                        Some(params[0].clone())
                    }
                    _ => None,
                },
                Type::Generic { name, params } if name == "Arena" && params.len() == 1 => {
                    Some(params[0].clone())
                }
                _ => None,
            };
            if let Some(elem_type) = elem_type {
                let resolved_elem = self.resolve_type_name(&elem_type)?;
                self.require_named_enum(
                    "Option",
                    &[("Some", true), ("None", false)],
                    call_expr.span,
                )?;
                return Ok(Some(Type::Generic {
                    name: "Option".to_string(),
                    params: vec![Type::Ref {
                        inner: Box::new(resolved_elem),
                        mutable: false,
                    }],
                }));
            }
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Arena<T>".to_string(),
                got: type_to_string(&arena_ty),
                span: call_expr.args[0].span(),
            });
        }

        if callee == "File::open" || callee == "File::create" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let path_ty = self.check_expr(&call_expr.args[0])?;
            let path_ok = match &path_ty {
                Type::Ref { inner, .. } => matches!(inner.as_ref(), Type::String | Type::Str),
                Type::String | Type::Str => true,
                _ => false,
            };
            if !path_ok {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "&String".to_string(),
                    got: type_to_string(&path_ty),
                    span: call_expr.args[0].span(),
                });
            }
            self.require_named_enum("Option", &[("Some", true), ("None", false)], call_expr.span)?;
            return Ok(Some(Type::Generic {
                name: "Option".to_string(),
                params: vec![Type::File],
            }));
        }

        if callee == "File::read" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let file_ty = self.check_expr(&call_expr.args[0])?;
            let buf_ty = self.check_expr(&call_expr.args[1])?;
            let file_ok = matches!(
                &file_ty,
                Type::Ref {
                    inner,
                    mutable: true
                } if matches!(inner.as_ref(), Type::File)
            );
            if !file_ok {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "&mut File".to_string(),
                    got: type_to_string(&file_ty),
                    span: call_expr.args[0].span(),
                });
            }
            let buf_ok = match &buf_ty {
                Type::Ref {
                    inner,
                    mutable: true,
                } => matches!(
                    inner.as_ref(),
                    Type::Vec { elem_type } if matches!(elem_type.as_ref(), Type::U8)
                ),
                _ => false,
            };
            if !buf_ok {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "&mut Vec<u8>".to_string(),
                    got: type_to_string(&buf_ty),
                    span: call_expr.args[1].span(),
                });
            }
            return Ok(Some(Type::Int));
        }

        if callee == "File::write" {
            if call_expr.args.len() != 2 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "2 arguments".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let file_ty = self.check_expr(&call_expr.args[0])?;
            let buf_ty = self.check_expr(&call_expr.args[1])?;
            let file_ok = matches!(
                &file_ty,
                Type::Ref {
                    inner,
                    mutable: true
                } if matches!(inner.as_ref(), Type::File)
            );
            if !file_ok {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "&mut File".to_string(),
                    got: type_to_string(&file_ty),
                    span: call_expr.args[0].span(),
                });
            }
            let buf_ok = match &buf_ty {
                Type::Ref { inner, .. } => matches!(
                    inner.as_ref(),
                    Type::Vec { elem_type } if matches!(elem_type.as_ref(), Type::U8)
                ),
                Type::Vec { elem_type } => matches!(elem_type.as_ref(), Type::U8),
                _ => false,
            };
            if !buf_ok {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "&Vec<u8>".to_string(),
                    got: type_to_string(&buf_ty),
                    span: call_expr.args[1].span(),
                });
            }
            return Ok(Some(Type::Int));
        }

        if callee == "File::close" {
            if call_expr.args.len() != 1 {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "1 argument".to_string(),
                    got: format!("{} arguments", call_expr.args.len()),
                    span: call_expr.span,
                });
            }
            let file_ty = self.check_expr(&call_expr.args[0])?;
            let file_ok = matches!(
                &file_ty,
                Type::Ref {
                    inner,
                    mutable: true
                } if matches!(inner.as_ref(), Type::File)
            ) || matches!(file_ty, Type::File);
            if !file_ok {
                return Err(TypeCheckError::TypeMismatch {
                    expected: "&mut File".to_string(),
                    got: type_to_string(&file_ty),
                    span: call_expr.args[0].span(),
                });
            }
            return Ok(Some(Type::Void));
        }

        // Not a built-in function
        Ok(None)
    }

    /// Root owner binding for `Vec::get_ref` / `Slice::get_ref` receivers (`&owner` or `&owner.field`).
    pub(crate) fn vec_owner_from_get_ref_receiver(
        &self,
        receiver: &Expr,
    ) -> Option<(String, Span)> {
        if let Expr::Ref(r) = receiver {
            return self.borrow_owner_from_expr(&r.inner);
        }
        None
    }

    pub(crate) fn is_get_ref_call(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Call(c) if (c.callee == "Vec::get_ref"
                || c.callee == "Slice::get_ref"
                || c.callee == "Arena::get_ref")
                && c.args.len() == 2
        )
    }

    /// Register a shared borrow on the root owner while `Option<&T>` from get_ref is live.
    pub(crate) fn register_get_ref_borrow_from_receiver(
        &mut self,
        receiver: &Expr,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        if let Some((owner, owner_span)) = self.vec_owner_from_get_ref_receiver(receiver) {
            self.register_borrow(&owner, false, owner_span)?;
        } else {
            let _ = self.check_expr(receiver)?;
            return Err(TypeCheckError::TypeMismatch {
                expected: "&Vec<T> or &[]T with a local owner".to_string(),
                got: "expression with a local owner".to_string(),
                span,
            });
        }
        Ok(())
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

    fn vec_elem_type_from_vec_arg(&mut self, arg: &Expr) -> Option<Type> {
        let checked = self.check_expr(arg).ok();
        if let Some(elem) = self.vec_elem_type_from_receiver(arg) {
            return Some(elem);
        }
        if let Some(Type::Ref { inner, .. }) = checked
            && let Type::Vec { elem_type } = *inner
        {
            return Some(*elem_type);
        }
        None
    }

    fn slice_elem_type_from_slice_arg(&mut self, arg: &Expr) -> Option<Type> {
        let slice_ty = self.check_expr(arg).ok()?;
        match slice_ty {
            Type::Ref {
                inner,
                mutable: false,
            } => match *inner {
                Type::Slice { inner: elem } => Some(*elem),
                // Array-to-slice coercion at call sites: `&[T; N]` accepted as `&[]T`.
                Type::Array { inner: elem, .. } => Some(*elem),
                _ => None,
            },
            _ => None,
        }
    }

    fn check_integer_operand(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        let raw_ty = self.check_expr(expr)?;
        let ty = Self::comparison_operand_type(&raw_ty);
        if !self.is_integer_type(&ty) {
            return Err(TypeCheckError::TypeMismatch {
                expected: "integer type".to_string(),
                got: type_to_string(&raw_ty),
                span: expr.span(),
            });
        }
        Ok(ty)
    }
}
