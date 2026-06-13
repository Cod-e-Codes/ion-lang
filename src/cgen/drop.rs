use super::*;

impl Codegen {
    pub(crate) fn needs_drop(&self, ty: &Type) -> bool {
        self.type_needs_drop(ty)
    }

    pub(crate) fn type_needs_drop(&self, ty: &Type) -> bool {
        if let Some((decl, substitutions)) = self.struct_decl_for_type(ty) {
            return decl.fields.iter().any(|field| {
                let field_ty = Self::substitute_field_types(&field.ty, &substitutions);
                self.type_needs_drop(&field_ty)
            });
        }
        if let Some((decl, substitutions)) = self.enum_decl_for_type(ty) {
            return decl.variants.iter().any(|variant| {
                if let Some(named_fields) = &variant.named_fields {
                    named_fields.iter().any(|(_, field_ty)| {
                        let ft = Self::substitute_field_types(field_ty, &substitutions);
                        self.type_needs_drop(&ft)
                    })
                } else {
                    variant.payload_types.iter().any(|payload_ty| {
                        let ft = Self::substitute_field_types(payload_ty, &substitutions);
                        self.type_needs_drop(&ft)
                    })
                }
            });
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

    pub(crate) fn emit_drop_at_path(&mut self, path: &str, ty: &Type) {
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
            Type::Box { .. } => {
                self.write_indent();
                self.writeln(&format!("if ({path}) {{ ion_box_free({path}); }}"));
            }
            Type::Vec { .. } => {
                self.write_indent();
                self.writeln(&format!(
                    "if ({path}) {{ ion_vec_free((ion_vec_t*)({path})); }}"
                ));
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
