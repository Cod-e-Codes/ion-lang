use super::*;
use crate::cgen::types::mangle_type_name;

impl Codegen {
    /// Generate code for built-in function calls.
    /// Returns Some(code) if it's a built-in, None otherwise.
    pub(crate) fn generate_builtin_call(
        &mut self,
        callee: &str,
        args: &[IREexpr],
        return_type: Option<&Type>,
    ) -> Option<String> {
        // Box::new<T>(value: T) -> Box<T>
        if callee.starts_with("Box::new") && args.len() == 1 {
            // Get the type from the return type
            let inner_type = return_type
                .and_then(|t| {
                    if let Type::Box { inner } = t {
                        Some(inner.as_ref())
                    } else {
                        None
                    }
                })
                .unwrap_or(&Type::Int);

            let inner_c_type = self.type_to_c(inner_type);
            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&format!(
                "{}* ptr = ({}*)ion_box_alloc(sizeof({}));",
                inner_c_type, inner_c_type, inner_c_type
            ));
            code.push_str(" if (ptr) { *ptr = ");
            // Generate the argument expression
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            code.push_str(&arg_code);
            code.push_str("; } ptr; })");
            return Some(code);
        }

        // Box::unwrap<T>(box: Box<T>) -> T
        if callee == "Box::unwrap" && args.len() == 1 {
            let mut code = String::new();
            code.push_str("(*");
            // Generate the argument expression
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            code.push_str(&arg_code);
            code.push(')');
            return Some(code);
        }

        // Vec::new<T>() -> Vec<T>
        if callee == "Vec::new" && args.is_empty() {
            // Extract element type from return type
            let elem_type = return_type
                .and_then(|t| {
                    if let Type::Vec { elem_type } = t {
                        Some(elem_type.as_ref())
                    } else {
                        None
                    }
                })
                .unwrap_or(&Type::Int);

            let elem_c_type = self.type_to_c(elem_type);
            let vec_type_name = mangle_type_name("Vec", std::slice::from_ref(elem_type));
            let code = format!(
                "(({}*)(ion_vec_new(sizeof({}))))",
                vec_type_name, elem_c_type
            );
            return Some(code);
        }

        // Vec::with_capacity<T>(cap: int) -> Vec<T>
        if callee == "Vec::with_capacity" && args.len() == 1 {
            let elem_type = return_type
                .and_then(|t| {
                    if let Type::Vec { elem_type } = t {
                        Some(elem_type.as_ref())
                    } else {
                        None
                    }
                })
                .unwrap_or(&Type::Int);

            let elem_c_type = self.type_to_c(elem_type);
            let vec_type_name = mangle_type_name("Vec", std::slice::from_ref(elem_type));
            let mut code = String::new();
            code.push_str(&format!(
                "(({}*)(ion_vec_with_capacity(sizeof(",
                vec_type_name
            ));
            code.push_str(&elem_c_type);
            code.push_str("), ");
            // Generate capacity argument
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            code.push_str(&arg_code);
            code.push_str(")))");
            return Some(code);
        }

        // Vec::len<T>(vec: &Vec<T>) -> int
        if callee == "Vec::len" && args.len() == 1 {
            let mut code = String::new();
            // Generate vec argument - it's a reference, so we need to dereference it
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            let deref_arg = self.vec_ion_ptr_expr(&args[0], &arg_code);
            code.push_str("((");
            code.push_str(&deref_arg);
            code.push_str(") ? (int)((ion_vec_t*)(");
            code.push_str(&deref_arg);
            code.push_str("))->len : 0)");
            return Some(code);
        }

        // Vec::capacity<T>(vec: &Vec<T>) -> int
        if callee == "Vec::capacity" && args.len() == 1 {
            let mut code = String::new();
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            let deref_arg = self.vec_ion_ptr_expr(&args[0], &arg_code);
            code.push_str("((");
            code.push_str(&deref_arg);
            code.push_str(") ? (int)((ion_vec_t*)(");
            code.push_str(&deref_arg);
            code.push_str("))->capacity : 0)");
            return Some(code);
        }

        // Vec::push<T>(vec: &mut Vec<T>, value: T)
        if callee == "Vec::push" && args.len() == 2 {
            let mut code = String::new();
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);
            let deref_vec = self.vec_ion_ptr_expr(&args[0], &vec_code);

            let mut value_code = String::new();
            let old_output = std::mem::replace(&mut self.output, value_code);
            self.generate_expr(&args[1]);
            value_code = std::mem::replace(&mut self.output, old_output);

            let elem_c_type = self.resolve_vec_elem_c_type(&args[0], return_type);

            let value_is_lvalue = matches!(
                args[1],
                IREexpr::StructLit { .. } | IREexpr::Var(_) | IREexpr::FieldAccess { .. }
            );
            if value_is_lvalue {
                code.push_str("ion_vec_push((ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("), &");
                code.push_str(&value_code);
                code.push_str(", sizeof(");
                code.push_str(&elem_c_type);
                code.push_str("))");
            } else if matches!(args[1], IREexpr::Call { .. }) {
                code.push_str("({ ");
                code.push_str(&elem_c_type);
                code.push_str(" _ion_push_val = ");
                code.push_str(&value_code);
                code.push_str("; ion_vec_push((ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("), &_ion_push_val, sizeof(");
                code.push_str(&elem_c_type);
                code.push_str(")); })");
            } else {
                code.push_str("ion_vec_push((ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("), &(");
                code.push_str(&format!("({}){{", elem_c_type));
                code.push_str(&value_code);
                code.push_str("}), sizeof(");
                code.push_str(&elem_c_type);
                code.push_str("))");
            }
            return Some(code);
        }

        // Vec::pop<T>(vec: &mut Vec<T>) -> Option<T>
        if callee == "Vec::pop" && args.len() == 1 {
            let mut code = String::new();
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);
            let deref_vec = self.vec_ion_ptr_expr(&args[0], &vec_code);

            let elem_c_type = self.resolve_vec_elem_c_type(&args[0], return_type);
            code.push_str("ion_vec_pop((ion_vec_t*)(");
            code.push_str(&deref_vec);
            code.push_str("), sizeof(");
            code.push_str(&elem_c_type);
            code.push_str("))");
            return Some(code);
        }

        // Vec::get<T>(vec: &Vec<T>, index: int) -> Option<T>
        if callee == "Vec::get" && args.len() == 2 {
            let mut code = String::new();
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);

            let mut index_code = String::new();
            let old_output = std::mem::replace(&mut self.output, index_code);
            self.generate_expr(&args[1]);
            index_code = std::mem::replace(&mut self.output, old_output);

            let elem_c_type = self.resolve_vec_elem_c_type(&args[0], return_type);
            let deref_vec = self.vec_ion_ptr_expr(&args[0], &vec_code);
            code.push_str("ion_vec_get((ion_vec_t*)(");
            code.push_str(&deref_vec);
            code.push_str("), ");
            code.push_str(&index_code);
            code.push_str(", sizeof(");
            code.push_str(&elem_c_type);
            code.push_str("))");
            return Some(code);
        }

        // Vec::get_ref<T>(vec: &Vec<T>, index: int) -> Option<&T> (stack-local, no move-out)
        if callee == "Vec::get_ref" && args.len() == 2 {
            let effective_return_type = return_type.cloned().or_else(|| {
                self.vec_elem_type_from_arg(&args[0])
                    .map(|elem| Type::Generic {
                        name: "Option".to_string(),
                        params: vec![Type::Ref {
                            inner: Box::new(elem),
                            mutable: false,
                        }],
                    })
            });
            let option_name = effective_return_type
                .as_ref()
                .map(|t| {
                    mangle_type_name(
                        "Option",
                        match t {
                            Type::Generic { params, .. } => params.as_slice(),
                            _ => &[],
                        },
                    )
                })
                .unwrap_or_else(|| "Option_int_".to_string());
            let ref_c_type = effective_return_type
                .as_ref()
                .and_then(|t| {
                    if let Type::Generic { params, .. } = t
                        && params.len() == 1
                        && let Type::Ref { inner, .. } = &params[0]
                    {
                        Some(self.type_to_c(&Type::Ref {
                            inner: inner.clone(),
                            mutable: false,
                        }))
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| "int*".to_string());

            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);

            let mut index_code = String::new();
            let old_output = std::mem::replace(&mut self.output, index_code);
            self.generate_expr(&args[1]);
            index_code = std::mem::replace(&mut self.output, old_output);

            let elem_c_type = self.resolve_vec_elem_c_type(
                &args[0],
                effective_return_type.as_ref().and_then(|t| {
                    if let Type::Generic { params, .. } = t
                        && params.len() == 1
                        && let Type::Ref { inner, .. } = &params[0]
                    {
                        Some(inner.as_ref())
                    } else {
                        None
                    }
                }),
            );
            let deref_vec = self.vec_ion_ptr_expr(&args[0], &vec_code);

            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&option_name);
            code.push_str(" _ion_get_ref; ion_vec_t* _ion_v = (ion_vec_t*)(");
            code.push_str(&deref_vec);
            code.push_str("); int _ion_i = ");
            code.push_str(&index_code);
            code.push_str("; if (_ion_v && _ion_i >= 0 && (size_t)_ion_i < _ion_v->len) { _ion_get_ref.tag = 0; _ion_get_ref.data.variant_0.arg0 = (");
            code.push_str(&ref_c_type);
            code.push_str(")((char*)_ion_v->data + _ion_i * sizeof(");
            code.push_str(&elem_c_type);
            code.push_str(")); } else { _ion_get_ref.tag = 1; } _ion_get_ref; })");
            return Some(code);
        }

        // Vec::set<T>(vec: &mut Vec<T>, index: int, value: T) -> int
        if callee == "Vec::set" && args.len() == 3 {
            let mut code = String::new();
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);
            let deref_vec = self.vec_ion_ptr_expr(&args[0], &vec_code);

            let mut index_code = String::new();
            let old_output = std::mem::replace(&mut self.output, index_code);
            self.generate_expr(&args[1]);
            index_code = std::mem::replace(&mut self.output, old_output);

            let mut value_code = String::new();
            let old_output = std::mem::replace(&mut self.output, value_code);
            self.generate_expr(&args[2]);
            value_code = std::mem::replace(&mut self.output, old_output);

            let elem_c_type = self.resolve_vec_elem_c_type(&args[0], return_type);

            let value_is_lvalue = matches!(
                args[2],
                IREexpr::StructLit { .. } | IREexpr::Var(_) | IREexpr::FieldAccess { .. }
            );
            if value_is_lvalue {
                code.push_str("ion_vec_set((ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("), ");
                code.push_str(&index_code);
                code.push_str(", &");
                code.push_str(&value_code);
                code.push_str(", sizeof(");
                code.push_str(&elem_c_type);
                code.push_str("))");
            } else if matches!(args[2], IREexpr::Call { .. }) {
                code.push_str("({ ");
                code.push_str(&elem_c_type);
                code.push_str(" _ion_set_val = ");
                code.push_str(&value_code);
                code.push_str("; ion_vec_set((ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("), ");
                code.push_str(&index_code);
                code.push_str(", &_ion_set_val, sizeof(");
                code.push_str(&elem_c_type);
                code.push_str(")); })");
            } else {
                code.push_str("ion_vec_set((ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("), ");
                code.push_str(&index_code);
                code.push_str(", &(");
                code.push_str(&format!("({}){{", elem_c_type));
                code.push_str(&value_code);
                code.push_str("}), sizeof(");
                code.push_str(&elem_c_type);
                code.push_str("))");
            }
            return Some(code);
        }

        // String::new() -> String
        if callee == "String::new" && args.is_empty() {
            return Some("ion_string_new()".to_string());
        }

        // String::from(s: &str) -> String
        if callee == "String::from" && args.len() == 1 {
            let mut code = String::new();
            // Generate string literal or string argument
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);

            // If it's a string literal, use ion_string_from_literal
            // Otherwise, assume it's already a String and clone it
            if arg_code.starts_with('"') {
                // Extract string length
                let len = arg_code.len() - 2; // Remove quotes
                code.push_str("ion_string_from_literal(");
                code.push_str(&arg_code);
                code.push_str(", ");
                code.push_str(&len.to_string());
                code.push(')');
            } else {
                // Assume it's a String, clone it
                code.push_str("ion_string_clone(");
                code.push_str(&arg_code);
                code.push(')');
            }
            return Some(code);
        }

        // String::len(s: &String) -> int
        if callee == "String::len" && args.len() == 1 {
            let mut code = String::new();
            // Only bare Ref{String} vars (e.g. get_ref bindings) need (*r)->len.
            // AddressOf (&local) must not use needs_deref: (&s) is always true under -Waddress.
            let needs_deref = matches!(&args[0], IREexpr::Var(_))
                && self.infer_irexpr_type(&args[0]).is_some_and(
                    |ty| matches!(ty, Type::Ref { inner, .. } if matches!(*inner, Type::String)),
                );
            if needs_deref {
                code.push_str("((");
                let mut arg_code = String::new();
                let old_output = std::mem::replace(&mut self.output, arg_code);
                self.generate_expr(&args[0]);
                arg_code = std::mem::replace(&mut self.output, old_output);
                code.push_str(&arg_code);
                code.push_str(") ? (int)((*");
                code.push_str(&arg_code);
                code.push_str(")->len) : 0)");
            } else {
                let mut arg_code = String::new();
                let old_output = std::mem::replace(&mut self.output, arg_code);
                self.generate_expr(&args[0]);
                arg_code = std::mem::replace(&mut self.output, old_output);
                let deref_arg = arg_code.strip_prefix('&').unwrap_or(&arg_code);
                code.push_str("((");
                code.push_str(deref_arg);
                code.push_str(") ? (int)((");
                code.push_str(deref_arg);
                code.push_str(")->len) : 0)");
            }
            return Some(code);
        }

        // String::push_str(s: &mut String, other: &str)
        if callee == "String::push_str" && args.len() == 2 {
            let mut code = String::new();
            let mut str_code = String::new();
            let old_output = std::mem::replace(&mut self.output, str_code);
            self.generate_expr(&args[0]);
            str_code = std::mem::replace(&mut self.output, old_output);
            // Remove leading & if present
            let deref_str = str_code.strip_prefix('&').unwrap_or(&str_code);

            let mut other_code = String::new();
            let old_output = std::mem::replace(&mut self.output, other_code);
            self.generate_expr(&args[1]);
            other_code = std::mem::replace(&mut self.output, old_output);

            if other_code.starts_with('"') {
                code.push_str("ion_string_push_str(");
                code.push_str(deref_str);
                code.push_str(", ");
                let len = other_code.len() - 2;
                code.push_str(&other_code);
                code.push_str(", ");
                code.push_str(&len.to_string());
                code.push(')');
            } else {
                // Owned String or &String: append using heap buffer (.data/.len).
                let deref_other = other_code.strip_prefix('&').unwrap_or(&other_code);
                code.push_str("({ ion_string_t* _ion_push_other = ");
                code.push_str(deref_other);
                code.push_str("; ion_string_push_str(");
                code.push_str(deref_str);
                code.push_str(", ((_ion_push_other != NULL && _ion_push_other->data != NULL) ? (const char*)_ion_push_other->data : \"\"), (_ion_push_other != NULL ? _ion_push_other->len : (size_t)0)); })");
            }
            return Some(code);
        }

        // String::push_byte(s: &mut String, b: u8)
        if callee == "String::push_byte" && args.len() == 2 {
            let mut code = String::new();
            let mut str_code = String::new();
            let old_output = std::mem::replace(&mut self.output, str_code);
            self.generate_expr(&args[0]);
            str_code = std::mem::replace(&mut self.output, old_output);
            let deref_str = str_code.strip_prefix('&').unwrap_or(&str_code);

            let mut byte_code = String::new();
            let old_output = std::mem::replace(&mut self.output, byte_code);
            self.generate_expr(&args[1]);
            byte_code = std::mem::replace(&mut self.output, old_output);

            code.push_str("ion_string_push_byte(");
            code.push_str(deref_str);
            code.push_str(", (unsigned char)(");
            code.push_str(&byte_code);
            code.push_str("))");
            return Some(code);
        }

        None
    }
}
