use super::*;
use std::collections::HashSet;

impl Codegen {
    pub(crate) fn needs_drop(&self, ty: &Type) -> bool {
        self.type_needs_drop(ty)
    }

    pub(crate) fn type_needs_drop(&self, ty: &Type) -> bool {
        self.type_needs_drop_rec(ty, &mut HashSet::new())
    }

    fn type_needs_drop_rec(&self, ty: &Type, visiting: &mut HashSet<String>) -> bool {
        if let Some((decl, substitutions)) = self.struct_decl_for_type(ty) {
            let name = match ty {
                Type::Struct(n) => n.clone(),
                Type::Generic { name, .. } => name.clone(),
                _ => format!("{:?}", ty),
            };
            if !visiting.insert(name.clone()) {
                // Coinductive for drop: cycle edge alone does not need drop;
                // Box/Vec on the cycle already return true via the leaf match.
                return false;
            }
            let result = decl.fields.iter().any(|field| {
                let field_ty = Self::substitute_field_types(&field.ty, &substitutions);
                self.type_needs_drop_rec(&field_ty, visiting)
            });
            visiting.remove(&name);
            return result;
        }
        if let Some((decl, substitutions)) = self.enum_decl_for_type(ty) {
            let name = match ty {
                Type::Enum(n) => n.clone(),
                Type::Struct(n) => n.clone(),
                Type::Generic { name, .. } => name.clone(),
                _ => format!("{:?}", ty),
            };
            if !visiting.insert(name.clone()) {
                return false;
            }
            let result = decl.variants.iter().any(|variant| {
                if let Some(named_fields) = &variant.named_fields {
                    named_fields.iter().any(|(_, field_ty)| {
                        let ft = Self::substitute_field_types(field_ty, &substitutions);
                        self.type_needs_drop_rec(&ft, visiting)
                    })
                } else {
                    variant.payload_types.iter().any(|payload_ty| {
                        let ft = Self::substitute_field_types(payload_ty, &substitutions);
                        self.type_needs_drop_rec(&ft, visiting)
                    })
                }
            });
            visiting.remove(&name);
            return result;
        }
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        matches!(
            resolved,
            Type::Box { .. }
                | Type::Vec { .. }
                | Type::String
                | Type::Sender { .. }
                | Type::Receiver { .. }
        )
    }

    pub(crate) fn emit_drop(&mut self, name: &str, ty: &Type) {
        self.emit_drop_at_path(name, ty);
    }

    /// Capture drop statements for embedding in a GNU statement expression.
    pub(crate) fn capture_drop_at_path(&mut self, path: &str, ty: &Type) -> String {
        let mut captured = String::new();
        let old_output = std::mem::replace(&mut self.output, captured);
        let old_indent = self.indent_level;
        self.indent_level = 0;
        self.emit_drop_at_path(path, ty);
        captured = std::mem::replace(&mut self.output, old_output);
        self.indent_level = old_indent;
        captured
    }

    fn fresh_temp(&mut self, prefix: &str) -> String {
        let n = self.temp_var_counter;
        self.temp_var_counter += 1;
        format!("{prefix}{n}")
    }

    fn drop_function_name(&self, ty: &Type) -> String {
        let c = self.type_to_c(ty);
        let sanitized: String = c
            .chars()
            .map(|ch| match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => ch,
                _ => '_',
            })
            .collect();
        format!("_ion_drop_{sanitized}")
    }

    fn is_drop_adt(&self, ty: &Type) -> bool {
        self.struct_decl_for_type(ty).is_some() || self.enum_decl_for_type(ty).is_some()
    }

    /// True when inlined drop glue for this ADT would recurse in the compiler
    /// (for example `Node` containing `Option<Box<Node>>`).
    ///
    /// Only the types that appear on their own drop cycle get a helper.
    /// `Option<Forest>` wrapping a cyclic `Vec<Forest>` inlines and calls
    /// `_ion_drop_Forest`; emitting `_ion_drop_Option_Forest` would be unused.
    fn adt_needs_named_drop(&self, ty: &Type) -> bool {
        if !self.is_drop_adt(ty) || !self.type_needs_drop(ty) {
            return false;
        }
        let start = self.drop_function_name(ty);
        self.adt_drop_reaches_start(ty, &start, &mut HashSet::new(), true)
    }

    fn adt_drop_reaches_start(
        &self,
        ty: &Type,
        start: &str,
        visiting: &mut HashSet<String>,
        is_root: bool,
    ) -> bool {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        if let Type::Box { inner } = &resolved {
            return self.adt_drop_reaches_start(inner, start, visiting, is_root);
        }
        if let Type::Vec { elem_type } = &resolved {
            return self.adt_drop_reaches_start(elem_type, start, visiting, is_root);
        }
        if self.is_drop_adt(&resolved) {
            let key = self.drop_function_name(&resolved);
            if !is_root && key == start {
                return true;
            }
            if !visiting.insert(key.clone()) {
                return false;
            }
            let cyclic = if let Some((decl, substitutions)) = self.struct_decl_for_type(&resolved) {
                decl.fields.iter().any(|field| {
                    let field_ty = Self::substitute_field_types(&field.ty, &substitutions);
                    self.adt_drop_reaches_start(&field_ty, start, visiting, false)
                })
            } else if let Some((decl, substitutions)) = self.enum_decl_for_type(&resolved) {
                decl.variants.iter().any(|variant| {
                    if let Some(named_fields) = &variant.named_fields {
                        named_fields.iter().any(|(_, field_ty)| {
                            let ft = Self::substitute_field_types(field_ty, &substitutions);
                            self.adt_drop_reaches_start(&ft, start, visiting, false)
                        })
                    } else {
                        variant.payload_types.iter().any(|payload_ty| {
                            let ft = Self::substitute_field_types(payload_ty, &substitutions);
                            self.adt_drop_reaches_start(&ft, start, visiting, false)
                        })
                    }
                })
            } else {
                false
            };
            visiting.remove(&key);
            return cyclic;
        }
        false
    }

    fn collect_named_drop_types(&self) -> Vec<Type> {
        let mut seen = HashSet::new();
        let mut types = Vec::new();
        let mut consider = |ty: Type| {
            if self.adt_needs_named_drop(&ty) {
                let name = self.drop_function_name(&ty);
                if seen.insert(name) {
                    types.push(ty);
                }
            }
        };
        for name in self.struct_map.keys() {
            consider(Type::Struct(name.clone()));
        }
        for name in self.enum_map.keys() {
            consider(Type::Enum(name.clone()));
        }
        for (base, params) in self.generic_instantiations.values() {
            consider(Type::Generic {
                name: base.clone(),
                params: params.clone(),
            });
        }
        types.sort_by_key(|a| self.drop_function_name(a));
        types
    }

    pub(crate) fn emit_named_drop_functions(&mut self) {
        let types = self.collect_named_drop_types();
        if types.is_empty() {
            return;
        }
        for ty in &types {
            let name = self.drop_function_name(ty);
            let c_ty = self.type_to_c(ty);
            self.writeln(&format!("static void {name}({c_ty} *p);"));
        }
        self.writeln("");
        for ty in &types {
            let name = self.drop_function_name(ty);
            let c_ty = self.type_to_c(ty);
            self.writeln(&format!("static void {name}({c_ty} *p) {{"));
            self.indent_level += 1;
            self.emit_drop_adt_inline("(*p)", ty);
            self.indent_level -= 1;
            self.writeln("}");
            self.writeln("");
        }
    }

    pub(crate) fn emit_drop_at_path(&mut self, path: &str, ty: &Type) {
        if self.is_drop_adt(ty) && self.adt_needs_named_drop(ty) {
            let fn_name = self.drop_function_name(ty);
            self.write_indent();
            self.writeln(&format!("{fn_name}(&({path}));"));
            return;
        }
        self.emit_drop_adt_inline(path, ty);
    }

    fn emit_drop_adt_inline(&mut self, path: &str, ty: &Type) {
        if let Some((decl, substitutions)) = self.struct_decl_for_type(ty) {
            let fields: Vec<(String, Type)> = decl
                .fields
                .iter()
                .map(|field| {
                    (
                        field.name.clone(),
                        Self::substitute_field_types(&field.ty, &substitutions),
                    )
                })
                .collect();
            for (field_name, field_ty) in fields {
                if self.type_needs_drop(&field_ty) {
                    let field_path = format!("{path}.{field_name}");
                    self.emit_drop_at_path(&field_path, &field_ty);
                }
            }
            return;
        }

        if let Some((decl, substitutions)) = self.enum_decl_for_type(ty) {
            let variants: Vec<(usize, Vec<(String, Type)>)> = decl
                .variants
                .iter()
                .enumerate()
                .filter_map(|(variant_idx, variant)| {
                    let has_payloads =
                        !variant.payload_types.is_empty() || variant.named_fields.is_some();
                    if !has_payloads {
                        return None;
                    }
                    let fields = if let Some(named_fields) = &variant.named_fields {
                        named_fields
                            .iter()
                            .map(|(field_name, field_ty)| {
                                (
                                    field_name.clone(),
                                    Self::substitute_field_types(field_ty, &substitutions),
                                )
                            })
                            .collect()
                    } else {
                        variant
                            .payload_types
                            .iter()
                            .enumerate()
                            .map(|(arg_idx, payload_ty)| {
                                (
                                    format!("arg{arg_idx}"),
                                    Self::substitute_field_types(payload_ty, &substitutions),
                                )
                            })
                            .collect()
                    };
                    Some((variant_idx, fields))
                })
                .collect();

            self.write_indent();
            self.writeln(&format!("switch ({path}.tag) {{"));
            for (variant_idx, fields) in variants {
                self.write_indent();
                self.writeln(&format!("case {variant_idx}:"));
                self.indent_level += 1;
                for (field_name, field_ty) in fields {
                    if self.type_needs_drop(&field_ty) {
                        let field_path = format!("{path}.data.variant_{variant_idx}.{field_name}");
                        self.emit_drop_at_path(&field_path, &field_ty);
                    }
                }
                self.write_indent();
                self.writeln("break;");
                self.indent_level -= 1;
            }
            self.write_indent();
            self.writeln("}");
            return;
        }

        let resolved = resolve_type_alias(ty, &self.type_aliases);
        match resolved {
            Type::Box { inner } => {
                let inner = *inner;
                if self.type_needs_drop(&inner) {
                    self.write_indent();
                    self.writeln(&format!("if ({path}) {{"));
                    self.indent_level += 1;
                    self.emit_drop_at_path(&format!("(*({path}))"), &inner);
                    self.write_indent();
                    self.writeln(&format!("ion_box_free({path});"));
                    self.indent_level -= 1;
                    self.write_indent();
                    self.writeln("}");
                } else {
                    self.write_indent();
                    self.writeln(&format!("if ({path}) {{ ion_box_free({path}); }}"));
                }
            }
            Type::Vec { elem_type } => {
                let elem_type = *elem_type;
                if self.type_needs_drop(&elem_type) {
                    let idx = self.fresh_temp("_ion_di");
                    let elem_c = self.type_to_c(&elem_type);
                    let slot = format!("(({elem_c}*)(({path})->data))[{idx}]");
                    self.write_indent();
                    self.writeln(&format!("if ({path}) {{"));
                    self.indent_level += 1;
                    self.write_indent();
                    self.writeln(&format!(
                        "for (size_t {idx} = 0; {idx} < ({path})->len; {idx}++) {{"
                    ));
                    self.indent_level += 1;
                    self.emit_drop_at_path(&slot, &elem_type);
                    self.indent_level -= 1;
                    self.write_indent();
                    self.writeln("}");
                    self.write_indent();
                    self.writeln(&format!("ion_vec_free((ion_vec_t*)({path}));"));
                    self.indent_level -= 1;
                    self.write_indent();
                    self.writeln("}");
                } else {
                    self.write_indent();
                    self.writeln(&format!(
                        "if ({path}) {{ ion_vec_free((ion_vec_t*)({path})); }}"
                    ));
                }
            }
            Type::String => {
                self.write_indent();
                self.writeln(&format!("if ({path}) {{ ion_string_free({path}); }}"));
            }
            Type::Sender { .. } | Type::Receiver { .. } => {
                self.write_indent();
                self.writeln(&format!(
                    "if ({path}.channel) {{ ion_channel_handle_drop({path}.channel); }}"
                ));
            }
            _ => {}
        }
    }
}
