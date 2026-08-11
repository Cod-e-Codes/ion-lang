use super::*;

/// Join ownership states from reachable control-flow edges (ION_SPEC §5.2).
/// Empty `states` is a caller error; prefer skipping the join when no edges reach.
pub(crate) fn join_ownership_states(
    states: &[OwnershipState],
    name: &str,
    span: Span,
) -> Result<OwnershipState, TypeCheckError> {
    if states.is_empty() {
        return Ok(OwnershipState::Valid);
    }
    if states.iter().all(|s| *s == OwnershipState::Valid) {
        Ok(OwnershipState::Valid)
    } else if states.iter().all(|s| *s == OwnershipState::Moved) {
        Ok(OwnershipState::Moved)
    } else {
        Err(TypeCheckError::UseAfterMove {
            name: name.to_string(),
            span,
        })
    }
}

impl TypeChecker {
    pub(crate) fn push_loop_ownership_frame(&mut self) {
        let entry_states = self
            .variables
            .iter()
            .map(|(name, info)| (name.clone(), info.state))
            .collect();
        self.loop_frames.push(LoopOwnershipFrame {
            entry_states,
            continue_snaps: Vec::new(),
            break_snaps: Vec::new(),
        });
    }

    pub(crate) fn pop_loop_ownership_frame(&mut self) -> Option<LoopOwnershipFrame> {
        self.loop_frames.pop()
    }

    fn snapshot_loop_entry_ownership(&self) -> HashMap<String, OwnershipState> {
        let Some(frame) = self.loop_frames.last() else {
            return HashMap::new();
        };
        frame
            .entry_states
            .keys()
            .map(|name| {
                let state = self
                    .variables
                    .get(name)
                    .map(|info| info.state)
                    .unwrap_or(OwnershipState::Moved);
                (name.clone(), state)
            })
            .collect()
    }

    pub(crate) fn record_loop_break_snapshot(&mut self) {
        let snap = self.snapshot_loop_entry_ownership();
        if let Some(frame) = self.loop_frames.last_mut() {
            frame.break_snaps.push(snap);
        }
    }

    pub(crate) fn record_loop_continue_snapshot(&mut self) {
        let snap = self.snapshot_loop_entry_ownership();
        if let Some(frame) = self.loop_frames.last_mut() {
            frame.continue_snaps.push(snap);
        }
    }

    /// Validate reentry edges, then join exit edges into `before` for the env after the loop.
    pub(crate) fn finish_loop_ownership(
        &self,
        before: &HashMap<String, VariableInfo>,
        body: &Block,
        body_env: &HashMap<String, VariableInfo>,
        frame: &LoopOwnershipFrame,
        include_condition_false_exit: bool,
        span: Span,
    ) -> Result<HashMap<String, VariableInfo>, TypeCheckError> {
        // Reentry: body fall-through + continue snapshots.
        let mut reentry_snaps: Vec<HashMap<String, OwnershipState>> = frame.continue_snaps.clone();
        if block_falls_through(body) {
            let mut fall_through = HashMap::new();
            for name in frame.entry_states.keys() {
                let state = body_env
                    .get(name)
                    .map(|info| info.state)
                    .unwrap_or(OwnershipState::Moved);
                fall_through.insert(name.clone(), state);
            }
            reentry_snaps.push(fall_through);
        }

        for (name, entry_state) in &frame.entry_states {
            if *entry_state != OwnershipState::Valid {
                continue;
            }
            for snap in &reentry_snaps {
                let state = snap.get(name).copied().unwrap_or(*entry_state);
                if state != OwnershipState::Valid {
                    return Err(TypeCheckError::UseAfterMove {
                        name: name.clone(),
                        span,
                    });
                }
            }
        }

        // Exit join: while/for use loop-head (condition-false) + breaks; loop uses breaks only.
        let mut merged = before.clone();
        for (name, prev_info) in before.iter() {
            let mut exit_states: Vec<OwnershipState> = Vec::new();
            if include_condition_false_exit {
                exit_states.push(
                    frame
                        .entry_states
                        .get(name)
                        .copied()
                        .unwrap_or(prev_info.state),
                );
            }
            for snap in &frame.break_snaps {
                exit_states.push(snap.get(name).copied().unwrap_or(prev_info.state));
            }

            let merged_state = if exit_states.is_empty() {
                prev_info.state
            } else {
                join_ownership_states(&exit_states, name, span)?
            };

            if let Some(info) = merged.get_mut(name) {
                info.state = merged_state;
                info.shared_borrow_count = prev_info.shared_borrow_count;
                info.mut_borrow_count = prev_info.mut_borrow_count;
            }
        }

        Ok(merged)
    }

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

    /// Peel field/index chains to the root owner variable binding, if any.
    pub(crate) fn borrow_owner_from_expr(&self, expr: &Expr) -> Option<(String, Span)> {
        let mut current = expr;
        loop {
            match current {
                Expr::Var(var_expr) => {
                    if self.variables.contains_key(&var_expr.name) {
                        return Some((var_expr.name.clone(), var_expr.span));
                    }
                    return None;
                }
                Expr::FieldAccess(acc) => current = &acc.base,
                Expr::Index(index_expr) => current = &index_expr.target,
                _ => return None,
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
            Expr::TypeConst(_) => Ok(()),
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

                self.check_owner_not_borrowed(&var_expr.name, var_expr.span)?;

                // Primitives and references are copied, not moved (see ION_SPEC §5.2).
                if Self::is_copy_type(&var_info.ty) {
                    return Ok(());
                }

                // Mark as moved
                self.variables
                    .get_mut(&var_expr.name)
                    .expect("owner exists after checks")
                    .state = OwnershipState::Moved;
                Ok(())
            }
            Expr::Ref(ref_expr) => {
                // Creating a reference borrows the owner; it is not a direct owner use.
                if let Some((owner, span)) = self.borrow_owner_from_expr(&ref_expr.inner) {
                    self.check_borrow_allowed(&owner, ref_expr.mutable, span)?;
                }
                if !matches!(ref_expr.inner.as_ref(), Expr::Var(_)) {
                    self.check_expr_borrow_operand(&ref_expr.inner)?;
                }
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
                // Match scrutinee is consumed per-arm, not moved at entry (see ION_SPEC §5.2).
                match match_expr.expr.as_ref() {
                    Expr::Call(call_expr) => {
                        for arg in &call_expr.args {
                            self.check_expr_for_moves(arg)?;
                        }
                    }
                    Expr::FieldAccess(acc) => {
                        self.check_expr(&acc.base)?;
                    }
                    Expr::Var(_) | Expr::Ref(_) | Expr::Index(_) => {}
                    _ => self.check_expr_for_moves(&match_expr.expr)?,
                }
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
            Expr::FnLiteral(_) => Ok(()),
        }
    }

    /// Types copied rather than moved at the ownership level (ION_SPEC §5.2).
    pub(crate) fn is_copy_type(ty: &Type) -> bool {
        matches!(
            ty,
            Type::Void
                | Type::Int
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
