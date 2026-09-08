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
        // clone_sender(&Sender<T>) -> Sender<T>
        if callee == "clone_sender" && args.len() == 1 {
            let mut src = String::new();
            let old_output = std::mem::replace(&mut self.output, src);
            if let IREexpr::AddressOf { inner, .. } = &args[0] {
                self.write("&");
                self.generate_expr(inner);
            } else {
                self.write("&");
                self.generate_expr(&args[0]);
            }
            src = std::mem::replace(&mut self.output, old_output);
            let mut code = String::new();
            code.push_str("({ ion_sender_t _cloned; ");
            code.push_str(&format!(
                "if (ion_channel_clone_sender({src}, &_cloned) != 0) ion_panic(\"clone_sender failed\"); "
            ));
            code.push_str("_cloned; })");
            return Some(code);
        }
        if callee == "try_send" && args.len() == 2 {
            let sender_addr = self.sender_addr_code(&args[0]);
            let value_type = return_type
                .and_then(|t| match t {
                    Type::Generic { name, params }
                        if name == "TrySendResult" && params.len() == 1 =>
                    {
                        Some(params[0].clone())
                    }
                    _ => None,
                })
                .or_else(|| self.infer_irexpr_type(&args[1]))
                .unwrap_or(Type::Int);
            let val_tmp = format!("_try_send_val_{}", self.temp_var_counter);
            self.temp_var_counter += 1;
            let st_tmp = format!("_try_send_st_{}", self.temp_var_counter);
            self.temp_var_counter += 1;
            let c_ty = self.type_to_c(&value_type);
            let result_ty = Type::Generic {
                name: "TrySendResult".to_string(),
                params: vec![value_type.clone()],
            };
            let result_c = self.type_to_c(&result_ty);
            let sent = self.c_enum_literal(&result_c, "TrySendResult", "Sent", None);
            let full = self.c_enum_literal(&result_c, "TrySendResult", "Full", Some(&val_tmp));
            let closed = self.c_enum_literal(&result_c, "TrySendResult", "Closed", Some(&val_tmp));
            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&format!("{c_ty} {val_tmp} = "));
            let mut val_code = String::new();
            let old_output = std::mem::replace(&mut self.output, val_code);
            self.generate_expr_with_type(&args[1], Some(&value_type));
            val_code = std::mem::replace(&mut self.output, old_output);
            code.push_str(&val_code);
            code.push_str("; ");
            code.push_str(&format!(
                "int {st_tmp} = ion_channel_try_send({sender_addr}, &{val_tmp}); "
            ));
            code.push_str(&format!("{result_c} _try_send_res; "));
            code.push_str(&format!(
                "if ({st_tmp} == 0) {{ _try_send_res = {sent}; }} else if ({st_tmp} == -2) {{ _try_send_res = {full}; }} else {{ _try_send_res = {closed}; }} "
            ));
            code.push_str("_try_send_res; })");
            return Some(code);
        }
        if callee == "try_recv" && args.len() == 1 {
            let recv_addr = self.sender_addr_code(&args[0]);
            let elem_type = return_type
                .and_then(|t| {
                    if let Type::Generic { name, params } = t {
                        if name == "TryRecvResult" && params.len() == 1 {
                            Some(params[0].clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .unwrap_or(Type::Int);
            let tmp = format!("_try_recv_tmp_{}", self.temp_var_counter);
            self.temp_var_counter += 1;
            let st_tmp = format!("_try_recv_st_{}", self.temp_var_counter);
            self.temp_var_counter += 1;
            let c_ty = self.type_to_c(&elem_type);
            let result_ty = Type::Generic {
                name: "TryRecvResult".to_string(),
                params: vec![elem_type.clone()],
            };
            let result_c = self.type_to_c(&result_ty);
            let msg = self.c_enum_literal(&result_c, "TryRecvResult", "Msg", Some(&tmp));
            let empty = self.c_enum_literal(&result_c, "TryRecvResult", "Empty", None);
            let closed = self.c_enum_literal(&result_c, "TryRecvResult", "Closed", None);
            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&format!("{c_ty} {tmp} = {{0}}; "));
            code.push_str(&format!(
                "int {st_tmp} = ion_channel_try_recv({recv_addr}, &{tmp}); "
            ));
            code.push_str(&format!("{result_c} _try_recv_res; "));
            code.push_str(&format!(
                "if ({st_tmp} == 0) {{ _try_recv_res = {msg}; }} else if ({st_tmp} == -2) {{ _try_recv_res = {empty}; }} else {{ _try_recv_res = {closed}; }} "
            ));
            code.push_str("_try_recv_res; })");
            return Some(code);
        }
        if callee == "join" && args.len() == 1 {
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            let code = format!(
                "({{ ion_thread_t _ion_jh = {arg_code}; if (ion_join(&_ion_jh) != 0) ion_panic(\"join failed\"); }})"
            );
            return Some(code);
        }
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
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr_with_type(&args[0], Some(inner_type));
            arg_code = std::mem::replace(&mut self.output, old_output);
            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&format!(
                "{}* ptr = ({}*)ion_box_alloc(sizeof({}));",
                inner_c_type, inner_c_type, inner_c_type
            ));
            // C arrays are not assignable, even behind a typedef name.
            if matches!(inner_type, Type::Array { .. }) {
                code.push_str(" if (!ptr) ion_panic(\"Box::new allocation failed\"); ");
                code.push_str(&memcpy_from_value("ptr", &inner_c_type, &arg_code));
                code.push_str("; ptr; })");
            } else {
                code.push_str(" if (!ptr) ion_panic(\"Box::new allocation failed\"); *ptr = ");
                code.push_str(&arg_code);
                code.push_str("; ptr; })");
            }
            return Some(code);
        }

        // Box::unwrap<T>(box: Box<T>) -> T
        // Copy T out, then free the box allocation without dropping T.
        if callee == "Box::unwrap" && args.len() == 1 {
            let inferred_inner = self.infer_irexpr_type(&args[0]).and_then(|ty| match ty {
                Type::Box { inner } => Some(*inner),
                _ => None,
            });
            let inner_type = return_type.cloned().or(inferred_inner).unwrap_or(Type::Int);
            let inner_c_type = self.type_to_c(&inner_type);
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            let code = if matches!(inner_type, Type::Array { .. }) {
                format!(
                    "({{ {ty}* _box = {arg}; static {ty} _val; memcpy(&_val, _box, sizeof(_val)); ion_box_free(_box); _val; }})",
                    ty = inner_c_type,
                    arg = arg_code
                )
            } else {
                format!(
                    "({{ {ty}* _box = {arg}; {ty} _val = *_box; ion_box_free(_box); _val; }})",
                    ty = inner_c_type,
                    arg = arg_code
                )
            };
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
                "({{ {vec}* _v = ({vec}*)ion_vec_new(sizeof({elem})); if (!_v) ion_panic(\"Vec::new allocation failed\"); _v; }})",
                vec = vec_type_name,
                elem = elem_c_type
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
                "({{ {vec}* _v = ({vec}*)ion_vec_with_capacity(sizeof({elem}), ",
                vec = vec_type_name,
                elem = elem_c_type
            ));
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            code.push_str(&arg_code);
            code.push_str(
                "); if (!_v) ion_panic(\"Vec::with_capacity allocation failed\"); _v; })",
            );
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

            let elem_ty = self.vec_elem_type_from_arg(&args[0]);
            let mut value_code = String::new();
            let old_output = std::mem::replace(&mut self.output, value_code);
            self.generate_expr_with_type(&args[1], elem_ty.as_ref());
            value_code = std::mem::replace(&mut self.output, old_output);

            let elem_c_type = self.resolve_vec_elem_c_type(&args[0], return_type);

            let value_is_lvalue = matches!(
                args[1],
                IREexpr::StructLit { .. }
                    | IREexpr::EnumLit { .. }
                    | IREexpr::Var(_)
                    | IREexpr::FieldAccess { .. }
            );
            let elem_is_array = matches!(elem_ty, Some(Type::Array { .. }));
            let mut push_call = String::new();
            if value_is_lvalue {
                push_call.push_str("ion_vec_push((ion_vec_t*)(");
                push_call.push_str(&deref_vec);
                push_call.push_str("), &");
                push_call.push_str(&value_code);
                push_call.push_str(", sizeof(");
                push_call.push_str(&elem_c_type);
                push_call.push_str("))");
                code = wrap_status_panic(&push_call, "Vec::push failed");
            } else if elem_is_array {
                push_call.push_str("ion_vec_push((ion_vec_t*)(");
                push_call.push_str(&deref_vec);
                push_call.push_str("), ");
                push_call.push_str(&compound_literal_addr(&elem_c_type, &value_code));
                push_call.push_str(", sizeof(");
                push_call.push_str(&elem_c_type);
                push_call.push_str("))");
                code = wrap_status_panic(&push_call, "Vec::push failed");
            } else if matches!(args[1], IREexpr::Call { .. }) {
                code.push_str("({ ");
                code.push_str(&elem_c_type);
                code.push_str(" _ion_push_val = ");
                code.push_str(&value_code);
                code.push_str("; if (ion_vec_push((ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("), &_ion_push_val, sizeof(");
                code.push_str(&elem_c_type);
                code.push_str(")) != 0) ion_panic(\"Vec::push failed\"); })");
            } else {
                push_call.push_str("ion_vec_push((ion_vec_t*)(");
                push_call.push_str(&deref_vec);
                push_call.push_str("), &(");
                push_call.push_str(&format!("({}){{", elem_c_type));
                push_call.push_str(&value_code);
                push_call.push_str("}), sizeof(");
                push_call.push_str(&elem_c_type);
                push_call.push_str("))");
                code = wrap_status_panic(&push_call, "Vec::push failed");
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
            let elem_ty = self.vec_elem_type_from_arg(&args[0]);
            let hollow = elem_ty.as_ref().is_some_and(|t| self.type_needs_drop(t));
            if hollow {
                let n = self.temp_var_counter;
                self.temp_var_counter += 1;
                let gv = format!("_ion_gv{n}");
                let gi = format!("_ion_gi{n}");
                let gr = format!("_ion_gr{n}");
                code.push_str("({ ion_vec_t* ");
                code.push_str(&gv);
                code.push_str(" = (ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("); int ");
                code.push_str(&gi);
                code.push_str(" = ");
                code.push_str(&index_code);
                code.push_str("; void* ");
                code.push_str(&gr);
                code.push_str(" = ion_vec_get(");
                code.push_str(&gv);
                code.push_str(", ");
                code.push_str(&gi);
                code.push_str(", sizeof(");
                code.push_str(&elem_c_type);
                code.push_str(")); if (");
                code.push_str(&gr);
                code.push_str(" && *(int*)");
                code.push_str(&gr);
                code.push_str(" == 0 && ");
                code.push_str(&gv);
                code.push_str(" && ");
                code.push_str(&gv);
                code.push_str("->data) { memset((char*)");
                code.push_str(&gv);
                code.push_str("->data + (size_t)");
                code.push_str(&gi);
                code.push_str(" * sizeof(");
                code.push_str(&elem_c_type);
                code.push_str("), 0, sizeof(");
                code.push_str(&elem_c_type);
                code.push_str(")); } ");
                code.push_str(&gr);
                code.push_str("; })");
            } else {
                code.push_str("ion_vec_get((ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("), ");
                code.push_str(&index_code);
                code.push_str(", sizeof(");
                code.push_str(&elem_c_type);
                code.push_str("))");
            }
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
            let elem_ty = self.vec_elem_type_from_arg(&args[0]);
            self.generate_expr_with_type(&args[2], elem_ty.as_ref());
            value_code = std::mem::replace(&mut self.output, old_output);

            let elem_c_type = self.resolve_vec_elem_c_type(&args[0], return_type);
            let drop_old = elem_ty.as_ref().is_some_and(|t| self.type_needs_drop(t));

            let value_is_lvalue = matches!(
                args[2],
                IREexpr::StructLit { .. }
                    | IREexpr::EnumLit { .. }
                    | IREexpr::Var(_)
                    | IREexpr::FieldAccess { .. }
            );
            if drop_old {
                let n = self.temp_var_counter;
                self.temp_var_counter += 1;
                let sv = format!("_ion_sv{n}");
                let si = format!("_ion_si{n}");
                let slot = format!("(({elem_c_type}*)(({sv})->data))[{si}]");
                let drop_old_stmt = elem_ty
                    .as_ref()
                    .map(|t| self.capture_drop_at_path(&slot, t))
                    .unwrap_or_default();
                code.push_str("({ ion_vec_t* ");
                code.push_str(&sv);
                code.push_str(" = (ion_vec_t*)(");
                code.push_str(&deref_vec);
                code.push_str("); int ");
                code.push_str(&si);
                code.push_str(" = ");
                code.push_str(&index_code);
                code.push_str("; ");
                let value_ptr = if value_is_lvalue {
                    format!("&{value_code}")
                } else if matches!(args[2], IREexpr::Call { .. }) {
                    code.push_str(&elem_c_type);
                    code.push_str(" _ion_set_val = ");
                    code.push_str(&value_code);
                    code.push_str("; ");
                    "&_ion_set_val".to_string()
                } else {
                    format!("&(({elem_c_type}){{{value_code}}})")
                };
                code.push_str("if (");
                code.push_str(&sv);
                code.push_str(" && ");
                code.push_str(&si);
                code.push_str(" >= 0 && (size_t)");
                code.push_str(&si);
                code.push_str(" < ");
                code.push_str(&sv);
                code.push_str("->len) { ");
                code.push_str(&drop_old_stmt);
                code.push_str(" } ion_vec_set(");
                code.push_str(&sv);
                code.push_str(", ");
                code.push_str(&si);
                code.push_str(", ");
                code.push_str(&value_ptr);
                code.push_str(", sizeof(");
                code.push_str(&elem_c_type);
                code.push_str(")); })");
            } else if value_is_lvalue {
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
            return Some(self.wrap_set_result(code));
        }

        // String::new() -> String
        if callee == "String::new" && args.is_empty() {
            return Some(
                "({ ion_string_t* _s = ion_string_new(); if (!_s) ion_panic(\"String::new allocation failed\"); _s; })"
                    .to_string(),
            );
        }

        // String::from(s: &str) -> String
        if callee == "String::from" && args.len() == 1 {
            let code;
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);

            let arg_ty = self.infer_irexpr_type(&args[0]).or_else(|| {
                if let IREexpr::Var(name) = &args[0] {
                    self.current_function_params
                        .get(name)
                        .cloned()
                        .or_else(|| self.lookup_binding_type(name))
                } else {
                    None
                }
            });

            // If it's a string literal, use ion_string_from_literal
            // &str (char*) uses strlen; &String clones the owned buffer.
            if arg_code.starts_with('"') {
                let len = arg_code.len() - 2;
                code = wrap_string_ptr(
                    &format!("ion_string_from_literal({arg_code}, {len})"),
                    "String::from allocation failed",
                );
            } else if matches!(
                arg_ty.as_ref(),
                Some(Type::Ref {
                    inner,
                    mutable: false,
                }) if matches!(**inner, Type::Str)
            ) {
                code = wrap_string_ptr(
                    &format!(
                        "({{ const char* _ion_s = {arg_code}; ion_string_from_literal(_ion_s, strlen(_ion_s)); }})"
                    ),
                    "String::from allocation failed",
                );
            } else {
                code = wrap_string_ptr(
                    &format!("ion_string_clone({arg_code})"),
                    "String::from allocation failed",
                );
            }
            return Some(code);
        }

        // String::from_utf8(bytes: Vec<u8>) -> Option<String>
        if callee == "String::from_utf8" && args.len() == 1 {
            let option_name = mangle_type_name("Option", std::slice::from_ref(&Type::String));
            let mut vec_code = String::new();
            let old_output = std::mem::replace(&mut self.output, vec_code);
            self.generate_expr(&args[0]);
            vec_code = std::mem::replace(&mut self.output, old_output);
            let deref_vec = self.vec_ion_ptr_expr(&args[0], &vec_code);
            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&option_name);
            code.push_str(" _ion_utf8; ion_vec_t* _ion_bytes = (ion_vec_t*)(");
            code.push_str(&deref_vec);
            code.push_str("); const uint8_t* _ion_d = (_ion_bytes && _ion_bytes->data) ? (const uint8_t*)_ion_bytes->data : (const uint8_t*)\"\"; size_t _ion_n = _ion_bytes ? _ion_bytes->len : 0; if (!ion_utf8_valid(_ion_d, _ion_n)) { _ion_utf8.tag = 1; } else { ion_string_t* _ion_s = ion_string_from_literal((const char*)_ion_d, _ion_n); if (!_ion_s) ion_panic(\"String::from_utf8 allocation failed\"); _ion_utf8.tag = 0; _ion_utf8.data.variant_0.arg0 = _ion_s; } if (_ion_bytes) ion_vec_free(_ion_bytes); _ion_utf8; })");
            return Some(code);
        }

        // String::len(s: &String) -> int
        if callee == "String::len" && args.len() == 1 {
            let mut code = String::new();
            let mut arg_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arg_code);
            self.generate_expr(&args[0]);
            arg_code = std::mem::replace(&mut self.output, old_output);
            let deref_arg = self.string_ion_ptr_expr(&args[0], &arg_code);
            code.push_str("((");
            code.push_str(&deref_arg);
            code.push_str(") ? (int)((");
            code.push_str(&deref_arg);
            code.push_str(")->len) : 0)");
            return Some(code);
        }

        // String::push_str(s: &mut String, other: &str)
        if callee == "String::push_str" && args.len() == 2 {
            let mut str_code = String::new();
            let old_output = std::mem::replace(&mut self.output, str_code);
            self.generate_expr(&args[0]);
            str_code = std::mem::replace(&mut self.output, old_output);
            let deref_str = self.string_ion_ptr_expr(&args[0], &str_code);

            let mut other_code = String::new();
            let old_output = std::mem::replace(&mut self.output, other_code);
            self.generate_expr(&args[1]);
            other_code = std::mem::replace(&mut self.output, old_output);

            let other_is_str_slice = match &args[1] {
                IREexpr::StringLit(_) => true,
                IREexpr::Var(name) => self.lookup_var_type(name).is_some_and(|ty| match &ty {
                    Type::Str => true,
                    Type::Ref { inner, .. } => matches!(inner.as_ref(), Type::Str),
                    _ => false,
                }),
                _ => false,
            };

            let code = if matches!(&args[1], IREexpr::StringLit(_)) {
                let len = other_code.len() - 2;
                wrap_status_panic(
                    &format!("ion_string_push_str({deref_str}, {other_code}, {len})"),
                    "String::push_str failed",
                )
            } else if other_is_str_slice {
                wrap_status_panic(
                    &format!(
                        "ion_string_push_str({deref_str}, {other_code}, strlen({other_code}))"
                    ),
                    "String::push_str failed",
                )
            } else {
                let deref_other = other_code.strip_prefix('&').unwrap_or(&other_code);
                format!(
                    "({{ ion_string_t* _ion_push_other = {deref_other}; if (ion_string_push_str({deref_str}, ((_ion_push_other != NULL && _ion_push_other->data != NULL) ? (const char*)_ion_push_other->data : \"\"), (_ion_push_other != NULL ? _ion_push_other->len : (size_t)0)) != 0) ion_panic(\"String::push_str failed\"); }})"
                )
            };
            return Some(code);
        }

        // String::push_byte(s: &mut String, b: u8)
        if callee == "String::push_byte" && args.len() == 2 {
            let mut str_code = String::new();
            let old_output = std::mem::replace(&mut self.output, str_code);
            self.generate_expr(&args[0]);
            str_code = std::mem::replace(&mut self.output, old_output);
            let deref_str = self.string_ion_ptr_expr(&args[0], &str_code);

            let mut byte_code = String::new();
            let old_output = std::mem::replace(&mut self.output, byte_code);
            self.generate_expr(&args[1]);
            byte_code = std::mem::replace(&mut self.output, old_output);

            return Some(wrap_status_panic(
                &format!("ion_string_push_byte({deref_str}, (unsigned char)({byte_code}))"),
                "String::push_byte failed",
            ));
        }

        // String::get(s: &String, index: int) -> Option<u8>
        if callee == "String::get" && args.len() == 2 {
            let option_name = mangle_type_name("Option", std::slice::from_ref(&Type::U8));

            let mut str_code = String::new();
            let old_output = std::mem::replace(&mut self.output, str_code);
            self.generate_expr(&args[0]);
            str_code = std::mem::replace(&mut self.output, old_output);
            let deref_str = self.string_ion_ptr_expr(&args[0], &str_code);

            let mut index_code = String::new();
            let old_output = std::mem::replace(&mut self.output, index_code);
            self.generate_expr(&args[1]);
            index_code = std::mem::replace(&mut self.output, old_output);

            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&option_name);
            code.push_str(" _ion_get; ion_string_t* _ion_s = ");
            code.push_str(&deref_str);
            code.push_str("; int _ion_i = ");
            code.push_str(&index_code);
            code.push_str("; if (_ion_s && _ion_s->data && _ion_i >= 0 && (size_t)_ion_i < _ion_s->len) { _ion_get.tag = 0; _ion_get.data.variant_0.arg0 = (uint8_t)_ion_s->data[_ion_i]; } else { _ion_get.tag = 1; } _ion_get; })");
            return Some(code);
        }

        // Slice::len<T>(s: &[]T) -> int
        if callee == "Slice::len" && args.len() == 1 {
            if let Some((_, size, _)) = self.slice_arg_as_array(&args[0]) {
                return Some(size.to_string());
            }
            let mut slice_code = String::new();
            let old_output = std::mem::replace(&mut self.output, slice_code);
            self.generate_expr(&args[0]);
            slice_code = std::mem::replace(&mut self.output, old_output);
            let (slice_expr, by_ref) = self.slice_ion_access_from_code(&args[0], &slice_code);
            let len_access = if by_ref {
                format!("({slice_expr})->len")
            } else {
                format!("({slice_expr}).len")
            };
            return Some(format!("(int)({len_access})"));
        }

        // Slice::get_ref<T>(s: &[]T, index: int) -> Option<&T> (stack-local, no move-out)
        if callee == "Slice::get_ref" && args.len() == 2 {
            let effective_return_type = return_type.cloned().or_else(|| {
                self.slice_elem_type_from_arg(&args[0])
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
            let elem_c_type = effective_return_type
                .as_ref()
                .and_then(|t| {
                    if let Type::Generic { params, .. } = t
                        && params.len() == 1
                        && let Type::Ref { inner, .. } = &params[0]
                    {
                        Some(self.type_to_c(inner))
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    self.slice_elem_type_from_arg(&args[0])
                        .map(|elem| self.type_to_c(&elem))
                })
                .unwrap_or_else(|| "int".to_string());

            let mut index_code = String::new();
            let old_output = std::mem::replace(&mut self.output, index_code);
            self.generate_expr(&args[1]);
            index_code = std::mem::replace(&mut self.output, old_output);

            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&option_name);
            code.push_str(" _ion_get_ref; int _ion_i = ");
            code.push_str(&index_code);
            code.push_str("; ");

            if let Some((array_name, size, _)) = self.slice_arg_as_array(&args[0]) {
                code.push_str(&elem_c_type);
                code.push_str("* _ion_data = ");
                code.push_str(&array_name);
                code.push_str("; int _ion_len = ");
                code.push_str(&size.to_string());
                code.push_str("; if (_ion_i >= 0 && _ion_i < _ion_len) { _ion_get_ref.tag = 0; _ion_get_ref.data.variant_0.arg0 = (");
                code.push_str(&ref_c_type);
                code.push_str(
                    ")(_ion_data + _ion_i); } else { _ion_get_ref.tag = 1; } _ion_get_ref; })",
                );
            } else {
                let mut slice_code = String::new();
                let old_output = std::mem::replace(&mut self.output, slice_code);
                self.generate_expr(&args[0]);
                slice_code = std::mem::replace(&mut self.output, old_output);
                let (slice_expr, by_ref) = self.slice_ion_access_from_code(&args[0], &slice_code);
                let len_access = if by_ref {
                    format!("({slice_expr})->len")
                } else {
                    format!("({slice_expr}).len")
                };
                let data_access = if by_ref {
                    format!("({slice_expr})->data")
                } else {
                    format!("({slice_expr}).data")
                };
                code.push_str("if (_ion_i >= 0 && _ion_i < ");
                code.push_str(&len_access);
                code.push_str(") { _ion_get_ref.tag = 0; _ion_get_ref.data.variant_0.arg0 = (");
                code.push_str(&ref_c_type);
                code.push_str(")(");
                code.push_str(&data_access);
                code.push_str(" + _ion_i); } else { _ion_get_ref.tag = 1; } _ion_get_ref; })");
            }
            return Some(code);
        }

        if callee == "Arena::get_ref" && args.len() == 2 {
            let elem_type = return_type
                .and_then(|t| {
                    if let Type::Generic { name, params } = t
                        && name == "Option"
                        && params.len() == 1
                        && let Type::Ref { inner, .. } = &params[0]
                    {
                        return Some((**inner).clone());
                    }
                    None
                })
                .unwrap_or(Type::Int);
            let option_name = return_type
                .map(|t| {
                    mangle_type_name(
                        "Option",
                        match t {
                            Type::Generic { params, .. } => params.as_slice(),
                            _ => &[],
                        },
                    )
                })
                .unwrap_or_else(|| "Option".to_string());
            let ref_c_type = self.type_to_c(&Type::Ref {
                inner: Box::new(elem_type.clone()),
                mutable: false,
            });
            let slot_c = mangle_type_name("Slot", std::slice::from_ref(&elem_type));
            let mut arena_code = String::new();
            let old_output = std::mem::replace(&mut self.output, arena_code);
            self.generate_expr(&args[0]);
            arena_code = std::mem::replace(&mut self.output, old_output);
            let mut handle_code = String::new();
            let old_output = std::mem::replace(&mut self.output, handle_code);
            self.generate_expr(&args[1]);
            handle_code = std::mem::replace(&mut self.output, old_output);
            let mut code = String::new();
            code.push_str("({ ");
            code.push_str(&option_name);
            code.push_str(" _ion_get_ref; ");
            code.push_str(&format!(
                "Handle _ion_h = {handle_code}; ion_vec_t* _ion_v = (ion_vec_t*)(({arena_code})->slots); "
            ));
            code.push_str(
                "if (_ion_v && _ion_h.index >= 0 && (size_t)_ion_h.index < _ion_v->len) { ",
            );
            code.push_str(&format!(
                "{slot_c}* _ion_slot = &(({slot_c}*)_ion_v->data)[_ion_h.index]; if (_ion_slot->tag == 0 && _ion_slot->data.variant_0.generation == _ion_h.generation) {{ _ion_get_ref.tag = 0; _ion_get_ref.data.variant_0.arg0 = ({ref_c_type})(&_ion_slot->data.variant_0.value); }} else {{ _ion_get_ref.tag = 1; }} "
            ));
            code.push_str("} else { _ion_get_ref.tag = 1; } _ion_get_ref; })");
            return Some(code);
        }

        if (callee == "File::open" || callee == "File::create") && args.len() == 1 {
            let mode = if callee == "File::open" { "rb" } else { "w+b" };
            let mut path_code = String::new();
            let old_output = std::mem::replace(&mut self.output, path_code);
            self.generate_expr(&args[0]);
            path_code = std::mem::replace(&mut self.output, old_output);
            let path_ptr = self.string_ion_ptr_expr(&args[0], &path_code);
            let option_name = mangle_type_name("Option", std::slice::from_ref(&Type::File));
            let some = self.c_enum_literal(&option_name, "Option", "Some", Some("_ion_f"));
            let none = self.c_enum_literal(&option_name, "Option", "None", None);
            let code = format!(
                "({{ ion_string_t* _ion_p = {path_ptr}; const char* _ion_path = (_ion_p && _ion_p->data) ? (const char*)_ion_p->data : \"\"; ion_file_t _ion_f = ion_file_open(_ion_path, \"{mode}\"); {option_name} _ion_fo; if (_ion_f.fp) {{ _ion_fo = {some}; }} else {{ _ion_fo = {none}; }} _ion_fo; }})"
            );
            return Some(code);
        }

        if callee == "File::read" && args.len() == 2 {
            let mut file_code = String::new();
            let old_output = std::mem::replace(&mut self.output, file_code);
            self.generate_expr(&args[0]);
            file_code = std::mem::replace(&mut self.output, old_output);
            let mut buf_code = String::new();
            let old_output = std::mem::replace(&mut self.output, buf_code);
            self.generate_expr(&args[1]);
            buf_code = std::mem::replace(&mut self.output, old_output);
            let buf_ptr = self.vec_ion_ptr_expr(&args[1], &buf_code);
            let code = format!(
                "({{ ion_file_t* _ion_f = {file_code}; ion_vec_t* _ion_b = (ion_vec_t*)({buf_ptr}); size_t _ion_got = 0; int _ion_n = -1; if (_ion_f && _ion_b) {{ if (ion_file_read(_ion_f, _ion_b->data, _ion_b->capacity, &_ion_got) == 0) {{ _ion_b->len = _ion_got; _ion_n = (int)_ion_got; }} }} _ion_n; }})"
            );
            return Some(code);
        }

        if callee == "File::write" && args.len() == 2 {
            let mut file_code = String::new();
            let old_output = std::mem::replace(&mut self.output, file_code);
            self.generate_expr(&args[0]);
            file_code = std::mem::replace(&mut self.output, old_output);
            let mut buf_code = String::new();
            let old_output = std::mem::replace(&mut self.output, buf_code);
            self.generate_expr(&args[1]);
            buf_code = std::mem::replace(&mut self.output, old_output);
            let buf_ptr = self.vec_ion_ptr_expr(&args[1], &buf_code);
            let code = format!(
                "({{ ion_file_t* _ion_f = {file_code}; ion_vec_t* _ion_b = (ion_vec_t*)({buf_ptr}); size_t _ion_got = 0; int _ion_n = -1; if (_ion_f && _ion_b) {{ if (ion_file_write(_ion_f, _ion_b->data, _ion_b->len, &_ion_got) == 0) {{ _ion_n = (int)_ion_got; }} }} _ion_n; }})"
            );
            return Some(code);
        }

        if callee == "File::close" && args.len() == 1 {
            let mut file_code = String::new();
            let old_output = std::mem::replace(&mut self.output, file_code);
            self.generate_expr(&args[0]);
            file_code = std::mem::replace(&mut self.output, old_output);
            let code = format!("({{ ion_file_close({file_code}); }})");
            return Some(code);
        }

        None
    }

    fn wrap_set_result(&self, set_expr: String) -> String {
        let result_c = self.type_to_c(&Type::Enum("SetResult".to_string()));
        let ok = self.c_enum_literal(&result_c, "SetResult", "Ok", None);
        let oob = self.c_enum_literal(&result_c, "SetResult", "OutOfBounds", None);
        format!(
            "({{ {result_c} _ion_set_res; int _ion_set_st = {set_expr}; if (_ion_set_st == 0) {{ _ion_set_res = {ok}; }} else {{ _ion_set_res = {oob}; }} _ion_set_res; }})"
        )
    }
}

fn wrap_status_panic(call: &str, msg: &str) -> String {
    format!("({{ if (({call}) != 0) ion_panic(\"{msg}\"); }})")
}

fn wrap_string_ptr(expr: &str, msg: &str) -> String {
    format!("({{ ion_string_t* _p = ({expr}); if (!_p) ion_panic(\"{msg}\"); _p; }})")
}

/// Address of a value for memcpy / `ion_vec_push`. Brace lists become typed compound literals.
fn compound_literal_addr(c_ty: &str, value_code: &str) -> String {
    let trimmed = value_code.trim_start();
    if trimmed.starts_with('{') {
        format!("&(({c_ty}){trimmed})")
    } else {
        format!("&({value_code})")
    }
}

fn memcpy_from_value(dest_ptr: &str, c_ty: &str, value_code: &str) -> String {
    format!(
        "memcpy({dest_ptr}, {}, sizeof({c_ty}))",
        compound_literal_addr(c_ty, value_code)
    )
}
