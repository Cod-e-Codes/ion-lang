use super::*;

impl TypeChecker {
    pub(crate) fn push_borrow_scope(&mut self) {
        self.borrow_scopes.push(Vec::new());
    }

    pub(crate) fn pop_borrow_scope(&mut self) {
        if let Some(scope) = self.borrow_scopes.pop() {
            for (owner, mutable) in scope {
                self.release_borrow(&owner, mutable);
            }
        }
    }

    pub(crate) fn check_borrow_allowed(
        &self,
        owner: &str,
        mutable: bool,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        let info = self
            .variables
            .get(owner)
            .ok_or_else(|| TypeCheckError::UndefinedVariable {
                name: owner.to_string(),
                span,
            })?;

        if mutable {
            if info.shared_borrow_count > 0 || info.mut_borrow_count > 0 {
                return Err(TypeCheckError::BorrowConflict {
                    name: owner.to_string(),
                    description: "as mutable while it is already borrowed".to_string(),
                    span,
                });
            }
        } else if info.mut_borrow_count > 0 {
            return Err(TypeCheckError::BorrowConflict {
                name: owner.to_string(),
                description: "as shared while it is mutably borrowed".to_string(),
                span,
            });
        }

        Ok(())
    }

    pub(crate) fn register_borrow(
        &mut self,
        owner: &str,
        mutable: bool,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        self.check_borrow_allowed(owner, mutable, span)?;

        let info = self
            .variables
            .get_mut(owner)
            .expect("owner exists after check");

        if mutable {
            info.mut_borrow_count += 1;
        } else {
            info.shared_borrow_count += 1;
        }

        if let Some(scope) = self.borrow_scopes.last_mut() {
            scope.push((owner.to_string(), mutable));
        }

        Ok(())
    }

    fn release_borrow(&mut self, owner: &str, mutable: bool) {
        if let Some(info) = self.variables.get_mut(owner) {
            if mutable {
                info.mut_borrow_count = info.mut_borrow_count.saturating_sub(1);
            } else {
                info.shared_borrow_count = info.shared_borrow_count.saturating_sub(1);
            }
        }
    }

    pub(crate) fn check_owner_not_borrowed(
        &self,
        owner: &str,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        if let Some(info) = self.variables.get(owner)
            && (info.shared_borrow_count > 0 || info.mut_borrow_count > 0)
        {
            return Err(TypeCheckError::BorrowConflict {
                name: owner.to_string(),
                description: "while it is borrowed".to_string(),
                span,
            });
        }
        Ok(())
    }

    /// Check expression for moves and mark variables as Moved.
    /// This is called before using an expression in contexts that move ownership
    /// (assignment, return, function call arguments).
    pub(crate) fn check_expr_for_moves(&mut self, expr: &Expr) -> Result<(), TypeCheckError> {
        match expr {
            Expr::Lit(_) => Ok(()),          // Literals don't move anything
            Expr::BoolLiteral(_) => Ok(()),  // Boolean literals don't move anything
            Expr::FloatLiteral(_) => Ok(()), // Float literals don't move anything
            Expr::Var(var_expr) => {
                if self.functions.contains_key(&var_expr.name)
                    || self.extern_functions.contains_key(&var_expr.name)
                {
                    return Ok(());
                }
                if var_expr.name.contains("::") {
                    let parts: Vec<&str> = var_expr.name.split("::").collect();
                    if parts.len() == 2
                        && let Some(module_exports) = self.module_imports.get(parts[0])
                        && module_exports.all_functions.contains_key(parts[1])
                    {
                        return Ok(());
                    }
                }

                let var_info = self.variables.get(&var_expr.name).ok_or_else(|| {
                    TypeCheckError::UndefinedVariable {
                        name: var_expr.name.clone(),
                        span: var_expr.span,
                    }
                })?;

                // Check for use-after-move
                if var_info.state == OwnershipState::Moved {
                    return Err(TypeCheckError::UseAfterMove {
                        name: var_expr.name.clone(),
                        span: var_expr.span,
                    });
                }

                // Primitives and references are copied, not moved (see ION_SPEC §5.2).
                if Self::is_copy_type(&var_info.ty) {
                    return Ok(());
                }

                self.check_owner_not_borrowed(&var_expr.name, var_expr.span)?;

                // Mark as moved
                self.variables
                    .get_mut(&var_expr.name)
                    .expect("owner exists after checks")
                    .state = OwnershipState::Moved;
                Ok(())
            }
            Expr::Ref(ref_expr) => {
                // Reference expressions (&x, &mut x) borrow, they don't move
                // Just check the inner expression is valid
                self.check_expr(&ref_expr.inner)?;
                Ok(())
            }
            Expr::StructLit(lit) => {
                // Moving a struct literal moves each of its value expressions.
                for field in &lit.fields {
                    self.check_expr_for_moves(&field.value)?;
                }
                Ok(())
            }
            Expr::TupleLit(tuple_lit) => {
                for elem in &tuple_lit.elements {
                    self.check_expr_for_moves(elem)?;
                }
                Ok(())
            }
            Expr::FieldAccess(acc) => {
                // Field access reads from the base but does not move the entire struct.
                self.check_expr(&acc.base)?;
                Ok(())
            }
            Expr::BinOp(bin_op_expr) => {
                // Binary operations use their operands (read), but don't move them
                // They only read the values
                self.check_expr(&bin_op_expr.left)?;
                self.check_expr(&bin_op_expr.right)?;
                Ok(())
            }
            Expr::UnOp(un_op_expr) => {
                // Unary operations use their operand (read), but don't move it
                self.check_expr(&un_op_expr.operand)?;
                Ok(())
            }
            Expr::Send(send_expr) => {
                // Sending moves the value operand; the channel itself is only read.
                self.check_expr_for_moves(&send_expr.value)
            }
            Expr::Recv(recv_expr) => {
                // Receiving from a channel does not move any existing variable;
                // it produces a fresh value.
                self.check_expr(&recv_expr.channel)?;
                Ok(())
            }
            Expr::EnumLit(enum_lit) => {
                // Check moves in enum literal arguments
                for arg in &enum_lit.args {
                    self.check_expr_for_moves(arg)?;
                }
                Ok(())
            }
            Expr::Match(match_expr) => {
                // Check moves in match expression
                self.check_expr_for_moves(&match_expr.expr)?;
                // Match arms don't move the matched value, they just pattern match
                Ok(())
            }
            Expr::Call(call_expr) => {
                for arg in &call_expr.args {
                    self.check_expr_for_moves(arg)?;
                }
                Ok(())
            }
            Expr::MethodCall(method_call) => {
                // Method calls need to check moves in receiver and arguments
                // The receiver might be moved or borrowed depending on method signature
                // For now, just check the receiver is valid (will be handled in desugaring)
                self.check_expr(&method_call.receiver)?;
                for arg in &method_call.args {
                    self.check_expr_for_moves(arg)?;
                }
                Ok(())
            }
            Expr::StringLit(_) => Ok(()), // String literals don't move anything
            Expr::ArrayLiteral(arr_lit) => {
                // Array literals move their elements
                for elem in &arr_lit.elements {
                    self.check_expr_for_moves(elem)?;
                }
                Ok(())
            }
            Expr::Index(index_expr) => {
                // Indexing reads from the target but doesn't move it
                self.check_expr(&index_expr.target)?;
                self.check_expr(&index_expr.index)?;
                Ok(())
            }
            Expr::Cast(cast_expr) => {
                // Casting moves the expression
                self.check_expr_for_moves(&cast_expr.expr)?;
                Ok(())
            }
            Expr::Assign(assign_expr) => {
                // Assignment moves the value, but not the target
                self.check_expr(&assign_expr.target)?; // Check target is valid
                self.check_expr_for_moves(&assign_expr.value)?; // Move the value
                Ok(())
            }
        }
    }

    /// Types copied rather than moved at the ownership level (ION_SPEC §5.2).
    pub(crate) fn is_copy_type(ty: &Type) -> bool {
        matches!(
            ty,
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
                | Type::UInt
                | Type::Ref { .. }
                | Type::Fn { .. }
        )
    }
}
