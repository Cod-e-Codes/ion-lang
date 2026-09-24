use super::*;
use crate::types_util::substitute_type;
use std::collections::{HashMap, HashSet};

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

/// Pops one match-sibling frame when the match check ends, including on error.
pub(crate) struct MatchSiblingGuard {
    loans: *mut Vec<Vec<usize>>,
}

impl Drop for MatchSiblingGuard {
    fn drop(&mut self) {
        if self.loans.is_null() {
            return;
        }
        // SAFETY: `loans` points at `TypeChecker::match_sibling_loans` for the
        // checker that pushed this frame. Drop runs as that match check ends
        // and only pops the frame `enter_match_siblings` pushed.
        unsafe {
            (*self.loans).pop();
        }
        self.loans = std::ptr::null_mut();
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
            } else if exit_states.iter().all(|s| *s == OwnershipState::Valid) {
                OwnershipState::Valid
            } else {
                // One exit moved the binding and another did not. It is moved
                // after the loop, so a later use is UseAfterMove. The exit that
                // still owns it drops the value on that path.
                OwnershipState::Moved
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
        self.borrow_shadows.push(Vec::new());
    }

    pub(crate) fn pop_borrow_scope(&mut self) {
        if let Some(scope) = self.borrow_scopes.pop() {
            for idx in scope {
                self.release_live(idx);
            }
        }
        if let Some(shadows) = self.borrow_shadows.pop() {
            for (name, prev) in shadows.into_iter().rev() {
                let live: Vec<usize> = prev
                    .into_iter()
                    .filter(|idx| {
                        self.live_borrows
                            .get(*idx)
                            .is_some_and(|borrow| !borrow.released)
                    })
                    .collect();
                if live.is_empty() {
                    self.borrow_names.remove(&name);
                } else {
                    self.borrow_names.insert(name, live);
                }
            }
        }
    }

    fn release_live(&mut self, idx: usize) {
        let Some(borrow) = self.live_borrows.get(idx) else {
            return;
        };
        if borrow.released {
            return;
        }
        let owner = borrow.owner.clone();
        let mutable = borrow.mutable;
        let whole = borrow.fields.is_none();
        self.live_borrows[idx].released = true;
        if whole {
            self.release_borrow(&owner, mutable);
        }
    }

    pub(crate) fn check_borrow_allowed(
        &self,
        owner: &str,
        fields: Option<&[String]>,
        mutable: bool,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        self.check_borrow_allowed_except(owner, fields, mutable, span, &[])
    }

    fn check_borrow_allowed_except(
        &self,
        owner: &str,
        fields: Option<&[String]>,
        mutable: bool,
        span: Span,
        ignore: &[usize],
    ) -> Result<(), TypeCheckError> {
        if !self.variables.contains_key(owner) {
            return Err(TypeCheckError::UndefinedVariable {
                name: owner.to_string(),
                span,
            });
        }
        for (idx, borrow) in self.live_borrows.iter().enumerate() {
            if ignore.contains(&idx)
                || self.loan_ignored_as_match_sibling(idx)
                || borrow.released
                || borrow.owner != owner
                || self.loan_ignored_in_branch(idx)
            {
                continue;
            }
            if borrow_paths_conflict(borrow.mutable, borrow.fields.as_deref(), mutable, fields) {
                return Err(TypeCheckError::BorrowConflict {
                    name: owner.to_string(),
                    description: if mutable {
                        "as mutable while it is already borrowed".to_string()
                    } else {
                        "as shared while it is mutably borrowed".to_string()
                    },
                    span,
                });
            }
        }
        Ok(())
    }

    pub(crate) fn register_borrow(
        &mut self,
        owner: &str,
        fields: Option<Vec<String>>,
        mutable: bool,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        self.register_borrow_except(owner, fields, mutable, span, &[])
    }

    fn register_borrow_except(
        &mut self,
        owner: &str,
        fields: Option<Vec<String>>,
        mutable: bool,
        span: Span,
        ignore: &[usize],
    ) -> Result<(), TypeCheckError> {
        self.check_borrow_allowed_except(owner, fields.as_deref(), mutable, span, ignore)?;
        if fields.is_none() {
            let info = self
                .variables
                .get_mut(owner)
                .expect("owner exists after check");
            if mutable {
                info.mut_borrow_count += 1;
            } else {
                info.shared_borrow_count += 1;
            }
        }
        let idx = self.live_borrows.len();
        self.live_borrows.push(LiveBorrow {
            owner: owner.to_string(),
            fields,
            mutable,
            released: false,
            depth: self.borrow_scopes.len(),
            carriers: Vec::new(),
        });
        if let Some(scope) = self.borrow_scopes.last_mut() {
            scope.push(idx);
        }
        Ok(())
    }

    fn ensure_loan_shadow(&mut self, name: &str) {
        let already = self
            .borrow_shadows
            .last()
            .is_some_and(|frame| frame.iter().any(|(existing, _)| existing == name));
        if already {
            return;
        }
        let prev = self.borrow_names.get(name).cloned().unwrap_or_default();
        if let Some(frame) = self.borrow_shadows.last_mut() {
            frame.push((name.to_string(), prev));
        }
    }

    fn loans_of(&self, name: &str) -> Vec<usize> {
        self.borrow_names
            .get(name)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .filter(|idx| {
                self.live_borrows
                    .get(*idx)
                    .is_some_and(|borrow| !borrow.released)
            })
            .collect()
    }

    fn add_loan(&mut self, name: &str, idx: usize) {
        if name.is_empty() {
            return;
        }
        if self
            .live_borrows
            .get(idx)
            .is_some_and(|borrow| borrow.released)
        {
            return;
        }
        if let Some(borrow) = self.live_borrows.get_mut(idx)
            && !borrow.carriers.iter().any(|(existing, _)| existing == name)
        {
            let span = self
                .variables
                .get(name)
                .map(|info| info.definition_span)
                .unwrap_or(Span {
                    start: 0,
                    end: 0,
                    line: 0,
                    column: 0,
                });
            borrow.carriers.push((name.to_string(), span));
        }
        let entry = self.borrow_names.entry(name.to_string()).or_default();
        if !entry.contains(&idx) {
            entry.push(idx);
        }
    }

    /// A `let` binding. Restored when the borrow scope that introduced it pops.
    pub(crate) fn add_loan_in_scope(&mut self, name: &str, idx: usize) {
        self.ensure_loan_shadow(name);
        self.add_loan(name, idx);
    }

    pub(crate) fn var_is_ref(&self, name: &str) -> bool {
        self.variables
            .get(name)
            .is_some_and(|info| matches!(info.ty, Type::Ref { .. }))
    }

    /// `&mut a.field` or `&mut a[i]` reads through a reference that already carries a loan.
    fn is_projected_reborrow(&self, owner: &str, fields: Option<&[String]>, inner: &Expr) -> bool {
        let through_field = fields.is_some_and(|path| !path.is_empty())
            && self.var_is_ref(owner)
            && !self.loans_of(owner).is_empty();
        through_field || self.indexes_through_ref(inner)
    }

    /// `a[i]` where `a` is a reference that already carries a loan.
    /// An index borrows the whole place behind that reference.
    fn indexes_through_ref(&self, expr: &Expr) -> bool {
        let mut saw_index = false;
        let mut current = expr;
        loop {
            match current {
                Expr::FieldAccess(acc) => current = &acc.base,
                Expr::Index(index) => {
                    saw_index = true;
                    current = &index.target;
                }
                Expr::Var(var) => {
                    return saw_index
                        && self.var_is_ref(&var.name)
                        && !self.loans_of(&var.name).is_empty();
                }
                _ => return false,
            }
        }
    }

    fn combine_loan_fields(
        existing: &Option<Vec<String>>,
        extra: Option<&[String]>,
    ) -> Option<Vec<String>> {
        let extra = extra.unwrap_or(&[]);
        if extra.is_empty() {
            return existing.clone();
        }
        match existing {
            None => Some(extra.to_vec()),
            Some(prefix) => {
                let mut path = prefix.clone();
                path.extend(extra.iter().cloned());
                Some(path)
            }
        }
    }

    fn push_unique_loans(out: &mut Vec<usize>, more: Vec<usize>) {
        for idx in more {
            if !out.contains(&idx) {
                out.push(idx);
            }
        }
    }

    /// Loans carried by the value of `expr`: a reference variable, a reborrow
    /// through one, a fresh `&` / `&mut` stored in a struct, enum, tuple, or
    /// match result, or a field of such a value.
    pub(crate) fn loans_in_expr(&self, expr: &Expr) -> Vec<usize> {
        self.loans_in_expr_kind(expr, false)
    }

    fn loans_in_expr_kind(&self, expr: &Expr, stored_only: bool) -> Vec<usize> {
        match expr {
            Expr::Var(var) if !stored_only => self.loans_of(&var.name),
            Expr::Ref(ref_expr) if stored_only => self
                .stored_ref_loans
                .get(&ref_expr.id)
                .cloned()
                .unwrap_or_default(),
            Expr::Ref(ref_expr) => {
                if let Some(ids) = self.stored_ref_loans.get(&ref_expr.id) {
                    ids.clone()
                } else if let Some((owner, fields, _)) = self.place_from_expr(&ref_expr.inner)
                    && self.is_projected_reborrow(&owner, fields.as_deref(), &ref_expr.inner)
                {
                    self.loans_of(&owner)
                } else {
                    Vec::new()
                }
            }
            Expr::TupleLit(tuple) => {
                let mut out = Vec::new();
                for element in &tuple.elements {
                    Self::push_unique_loans(
                        &mut out,
                        self.loans_in_expr_kind(element, stored_only),
                    );
                }
                out
            }
            Expr::FieldAccess(acc) => self.loans_in_expr_kind(&acc.base, stored_only),
            Expr::StructLit(lit) => {
                let mut out = Vec::new();
                for field in &lit.fields {
                    Self::push_unique_loans(
                        &mut out,
                        self.loans_in_expr_kind(&field.value, stored_only),
                    );
                }
                out
            }
            Expr::EnumLit(lit) => {
                let mut out = Vec::new();
                for arg in &lit.args {
                    Self::push_unique_loans(&mut out, self.loans_in_expr_kind(arg, stored_only));
                }
                if let Some(fields) = &lit.named_fields {
                    for (_, value) in fields {
                        Self::push_unique_loans(
                            &mut out,
                            self.loans_in_expr_kind(value, stored_only),
                        );
                    }
                }
                out
            }
            Expr::Match(match_expr) if !stored_only => {
                if let Some(recorded) = self.match_result_loans.get(&match_expr.id) {
                    let mut out = recorded.clone();
                    for arm in &match_expr.arms {
                        Self::push_unique_loans(&mut out, self.stored_loans_in_block(&arm.body));
                    }
                    out
                } else {
                    let mut out = Vec::new();
                    for arm in &match_expr.arms {
                        Self::push_unique_loans(&mut out, self.loans_in_block(&arm.body));
                    }
                    out
                }
            }
            Expr::Match(match_expr) => {
                let mut out = Vec::new();
                for arm in &match_expr.arms {
                    Self::push_unique_loans(&mut out, self.stored_loans_in_block(&arm.body));
                }
                out
            }
            _ => Vec::new(),
        }
    }

    pub(crate) fn loans_in_block(&self, block: &crate::ast::Block) -> Vec<usize> {
        self.loans_in_stmts_kind(&block.statements, false)
    }

    fn loans_in_stmts_kind(&self, stmts: &[crate::ast::Stmt], stored_only: bool) -> Vec<usize> {
        let Some(last) = stmts.last() else {
            return Vec::new();
        };
        match last {
            crate::ast::Stmt::Expr(expr) => self.loans_in_expr_kind(&expr.expr, stored_only),
            crate::ast::Stmt::If(if_stmt) => {
                let mut out = self.loans_in_stmts_kind(&if_stmt.then_block.statements, stored_only);
                if let Some(else_block) = &if_stmt.else_block {
                    Self::push_unique_loans(
                        &mut out,
                        self.loans_in_stmts_kind(&else_block.statements, stored_only),
                    );
                }
                out
            }
            _ => Vec::new(),
        }
    }

    fn stored_loans_in_block(&self, block: &crate::ast::Block) -> Vec<usize> {
        self.loans_in_stmts_kind(&block.statements, true)
    }

    /// Register lasting loans for `&` / `&mut` created inside a stored value.
    /// `ignore` lists loans from other arms of the same match or `if`: only one
    /// of those runs. Call arguments are not walked, so they stay ephemeral.
    /// Returns loan indices created by this walk.
    fn ensure_stored_ref_loans(
        &mut self,
        expr: &Expr,
        ignore: &[usize],
    ) -> Result<Vec<usize>, TypeCheckError> {
        match expr {
            Expr::Ref(ref_expr) => self.register_stored_ref(ref_expr, ignore),
            Expr::StructLit(lit) => {
                let mut created = Vec::new();
                for field in &lit.fields {
                    created.extend(self.ensure_stored_ref_loans(&field.value, ignore)?);
                }
                Ok(created)
            }
            Expr::EnumLit(lit) => {
                let mut created = Vec::new();
                for arg in &lit.args {
                    created.extend(self.ensure_stored_ref_loans(arg, ignore)?);
                }
                if let Some(fields) = &lit.named_fields {
                    for (_, value) in fields {
                        created.extend(self.ensure_stored_ref_loans(value, ignore)?);
                    }
                }
                Ok(created)
            }
            Expr::TupleLit(tuple) => {
                let mut created = Vec::new();
                for element in &tuple.elements {
                    created.extend(self.ensure_stored_ref_loans(element, ignore)?);
                }
                Ok(created)
            }
            Expr::FieldAccess(acc) => self.ensure_stored_ref_loans(&acc.base, ignore),
            Expr::Match(match_expr) => {
                self.ensure_stored_alternatives(match_expr.arms.iter().map(|arm| &arm.body), ignore)
            }
            _ => Ok(Vec::new()),
        }
    }

    fn ensure_stored_in_block(
        &mut self,
        block: &crate::ast::Block,
        ignore: &[usize],
    ) -> Result<Vec<usize>, TypeCheckError> {
        self.ensure_stored_in_stmts(&block.statements, ignore)
    }

    fn ensure_stored_in_stmts(
        &mut self,
        stmts: &[crate::ast::Stmt],
        ignore: &[usize],
    ) -> Result<Vec<usize>, TypeCheckError> {
        let Some(last) = stmts.last() else {
            return Ok(Vec::new());
        };
        match last {
            crate::ast::Stmt::Expr(expr) => self.ensure_stored_ref_loans(&expr.expr, ignore),
            crate::ast::Stmt::If(if_stmt) => {
                let mut branches = vec![&if_stmt.then_block];
                if let Some(else_block) = &if_stmt.else_block {
                    branches.push(else_block);
                }
                self.ensure_stored_alternatives(branches.into_iter(), ignore)
            }
            _ => Ok(Vec::new()),
        }
    }

    fn ensure_stored_alternatives<'a>(
        &mut self,
        branches: impl Iterator<Item = &'a crate::ast::Block>,
        ignore: &[usize],
    ) -> Result<Vec<usize>, TypeCheckError> {
        let mut arm_ignore = ignore.to_vec();
        let mut created = Vec::new();
        for block in branches {
            let arm_new = self.ensure_stored_in_block(block, &arm_ignore)?;
            for idx in &arm_new {
                if !arm_ignore.contains(idx) {
                    arm_ignore.push(*idx);
                }
            }
            created.extend(arm_new);
        }
        Ok(created)
    }

    /// Record the loan of a `&` / `&mut` that is stored in a value, not bound
    /// by itself. A second walk of the same expression does not register again.
    fn register_stored_ref(
        &mut self,
        ref_expr: &RefExpr,
        ignore: &[usize],
    ) -> Result<Vec<usize>, TypeCheckError> {
        if self.stored_ref_loans.contains_key(&ref_expr.id) {
            return Ok(Vec::new());
        }
        let Some((owner, fields, span)) = self.place_from_expr(&ref_expr.inner) else {
            self.stored_ref_loans.insert(ref_expr.id, Vec::new());
            return Ok(Vec::new());
        };
        let before = self.live_borrows.len();
        let mut carried = Vec::new();
        if self.is_projected_reborrow(&owner, fields.as_deref(), &ref_expr.inner) {
            let parents = self.loans_of(&owner);
            let parent = &self.live_borrows[parents[0]];
            let under_owner = parent.owner.clone();
            let under_fields = Self::combine_loan_fields(&parent.fields, fields.as_deref());
            let mut except = parents.clone();
            for idx in ignore {
                if !except.contains(idx) {
                    except.push(*idx);
                }
            }
            self.register_borrow_except(
                &under_owner,
                under_fields,
                ref_expr.mutable,
                span,
                &except,
            )?;
            carried.extend(parents);
        } else {
            self.register_borrow_except(&owner, fields, ref_expr.mutable, span, ignore)?;
        }
        let mut created = Vec::new();
        if self.live_borrows.len() > before {
            let idx = self.live_borrows.len() - 1;
            carried.push(idx);
            created.push(idx);
        }
        self.stored_ref_loans.insert(ref_expr.id, carried);
        Ok(created)
    }

    pub(crate) fn carry_expr_loans(
        &mut self,
        dest: &str,
        expr: &Expr,
        in_scope: bool,
    ) -> Result<(), TypeCheckError> {
        self.ensure_stored_ref_loans(expr, &[])?;
        for idx in self.loans_in_expr(expr) {
            if in_scope {
                self.add_loan_in_scope(dest, idx);
            } else {
                self.add_loan(dest, idx);
            }
        }
        Ok(())
    }

    /// An enum, struct, or array still holds the pointer after its last read.
    /// A bare `&T` binding ends at its last use.
    fn carrier_outlives_uses(&self, name: &str) -> bool {
        self.variables.get(name).is_some_and(|info| {
            matches!(
                info.ty,
                Type::Enum(_) | Type::Generic { .. } | Type::Struct(_) | Type::Array { .. }
            )
        })
    }

    pub(crate) fn record_match_loans(&mut self, id: ExprId, loans: &[usize]) {
        let entry = self.match_result_loans.entry(id).or_default();
        for idx in loans {
            if !entry.contains(idx) {
                entry.push(*idx);
            }
        }
    }

    pub(crate) fn enter_match_siblings(&mut self) -> MatchSiblingGuard {
        self.match_sibling_loans.push(Vec::new());
        MatchSiblingGuard {
            loans: &mut self.match_sibling_loans,
        }
    }

    fn loan_ignored_as_match_sibling(&self, idx: usize) -> bool {
        self.match_sibling_loans
            .iter()
            .any(|frame| frame.contains(&idx))
    }

    fn loan_ignored_in_branch(&self, idx: usize) -> bool {
        self.branch_ignored_loans
            .iter()
            .any(|frame| frame.contains(&idx))
    }

    /// Loans whose carriers are not used in `branch` and are not used again
    /// after the current statement. The other arm of an `if` may borrow the
    /// same place.
    pub(crate) fn loans_unused_in_branch(&self, branch: &Block) -> Vec<usize> {
        let mut names = Vec::new();
        for stmt in &branch.statements {
            collect_borrow_use_names(stmt, &mut names);
        }
        let used: HashSet<String> = names.into_iter().collect();
        let mut ignore = Vec::new();
        for (idx, borrow) in self.live_borrows.iter().enumerate() {
            if borrow.released {
                continue;
            }
            let carriers: Vec<&String> = self
                .borrow_names
                .iter()
                .filter(|(_, loans)| loans.contains(&idx))
                .map(|(name, _)| name)
                .collect();
            if carriers.is_empty() || carriers.iter().any(|name| used.contains(*name)) {
                continue;
            }
            let used_later = carriers.iter().any(|name| {
                self.borrow_last_use
                    .get(*name)
                    .is_some_and(|at| self.current_stmt_index.is_some_and(|current| *at > current))
            });
            if !used_later {
                ignore.push(idx);
            }
        }
        ignore
    }

    /// Keep loans the arm yields. A loan created on a local in the arm would
    /// otherwise end with that local, while the match result still holds the
    /// pointer. Loans that already belonged to an outer binding stay put.
    /// Loans this arm created are ignored by later arms, because only one arm runs.
    pub(crate) fn keep_arm_result_loans(&mut self, match_id: ExprId, escaping: &[usize]) {
        let current = self.borrow_scopes.last().cloned().unwrap_or_default();
        let created: Vec<usize> = escaping
            .iter()
            .copied()
            .filter(|idx| current.contains(idx))
            .collect();
        self.promote_current_scope_loans(&created);
        self.record_match_loans(match_id, escaping);
        if let Some(frame) = self.match_sibling_loans.last_mut() {
            for idx in created {
                if !frame.contains(&idx) {
                    frame.push(idx);
                }
            }
        }
    }

    /// Move loans this scope registered onto the parent scope so a `match`
    /// result can keep them after the arm ends.
    pub(crate) fn promote_current_scope_loans(&mut self, loans: &[usize]) {
        let Some(current) = self.borrow_scopes.last().cloned() else {
            return;
        };
        if self.borrow_scopes.len() < 2 {
            return;
        }
        let parent_depth = self.borrow_scopes.len() - 1;
        for idx in loans {
            if !current.contains(idx) {
                continue;
            }
            if let Some(scope) = self.borrow_scopes.last_mut() {
                scope.retain(|id| id != idx);
            }
            if let Some(borrow) = self.live_borrows.get_mut(*idx) {
                borrow.depth = parent_depth;
            }
            let parent = self.borrow_scopes.len() - 2;
            if let Some(scope) = self.borrow_scopes.get_mut(parent)
                && !scope.contains(idx)
            {
                scope.push(*idx);
            }
        }
    }

    /// Register the loan for `&` / `&mut`, including a field reborrow of an
    /// existing reference. The new binding carries that same loan.
    pub(crate) fn finish_ref_loan(
        &mut self,
        dest: &str,
        mutable: bool,
        inner: &Expr,
        in_scope: bool,
    ) -> Result<(), TypeCheckError> {
        let Some((owner, fields, span)) = self.place_from_expr(inner) else {
            return Ok(());
        };
        if self.is_projected_reborrow(&owner, fields.as_deref(), inner) {
            let parents = self.loans_of(&owner);
            let parent = &self.live_borrows[parents[0]];
            let under_owner = parent.owner.clone();
            let under_fields = Self::combine_loan_fields(&parent.fields, fields.as_deref());
            self.register_borrow_except(&under_owner, under_fields, mutable, span, &parents)?;
            for idx in parents {
                if in_scope {
                    self.add_loan_in_scope(dest, idx);
                } else {
                    self.add_loan(dest, idx);
                }
            }
            self.note_borrow_binding_scoped(dest, in_scope);
            return Ok(());
        }
        self.register_borrow(&owner, fields, mutable, span)?;
        self.note_borrow_binding_scoped(dest, in_scope);
        Ok(())
    }

    pub(crate) fn note_borrow_binding(&mut self, name: &str) {
        self.note_borrow_binding_scoped(name, true);
    }

    fn note_borrow_binding_scoped(&mut self, name: &str, in_scope: bool) {
        if let Some(idx) = self.live_borrows.len().checked_sub(1) {
            if in_scope {
                self.add_loan_in_scope(name, idx);
            } else {
                self.add_loan(name, idx);
            }
        }
    }

    /// Check a borrow that may be a projection through an existing reference.
    pub(crate) fn check_ref_place(
        &self,
        mutable: bool,
        inner: &Expr,
    ) -> Result<(), TypeCheckError> {
        let Some((owner, fields, span)) = self.place_from_expr(inner) else {
            return Ok(());
        };
        if self.is_projected_reborrow(&owner, fields.as_deref(), inner) {
            let parents = self.loans_of(&owner);
            let parent = &self.live_borrows[parents[0]];
            let under_owner = parent.owner.clone();
            let under_fields = Self::combine_loan_fields(&parent.fields, fields.as_deref());
            return self.check_borrow_allowed_except(
                &under_owner,
                under_fields.as_deref(),
                mutable,
                span,
                &parents,
            );
        }
        self.check_borrow_allowed(&owner, fields.as_deref(), mutable, span)
    }

    pub(crate) fn release_borrows_ending_at(
        &mut self,
        stmt_index: usize,
        last_uses: &HashMap<String, usize>,
        depth: usize,
    ) {
        let mut by_loan: HashMap<usize, Vec<String>> = HashMap::new();
        for (name, idxs) in &self.borrow_names {
            for idx in idxs {
                let Some(borrow) = self.live_borrows.get(*idx) else {
                    continue;
                };
                if borrow.released || borrow.depth != depth {
                    continue;
                }
                by_loan.entry(*idx).or_default().push(name.clone());
            }
        }
        let mut ending = Vec::new();
        for (idx, names) in by_loan {
            if names
                .iter()
                .any(|name| !last_uses.contains_key(name) || self.carrier_outlives_uses(name))
            {
                continue;
            }
            let max_use = names
                .iter()
                .filter_map(|name| last_uses.get(name))
                .copied()
                .max();
            if max_use == Some(stmt_index) {
                ending.push((idx, names));
            }
        }
        for (idx, names) in ending {
            for name in names {
                if let Some(list) = self.borrow_names.get_mut(&name) {
                    list.retain(|loan| *loan != idx);
                }
                if self
                    .borrow_names
                    .get(&name)
                    .is_some_and(|list| list.is_empty())
                {
                    self.borrow_names.remove(&name);
                }
            }
            self.release_live(idx);
        }
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

    pub(crate) fn place_from_expr(
        &self,
        expr: &Expr,
    ) -> Option<(String, Option<Vec<String>>, Span)> {
        let mut fields = Vec::new();
        let mut current = expr;
        loop {
            match current {
                Expr::FieldAccess(acc) => {
                    fields.push(acc.field.clone());
                    current = &acc.base;
                }
                Expr::Index(index_expr) => {
                    if let Expr::Lit(lit) = index_expr.index.as_ref() {
                        fields.push(lit.value.to_string());
                        current = &index_expr.target;
                    } else {
                        let (owner, span) = self.borrow_owner_from_expr(expr)?;
                        return Some((owner, None, span));
                    }
                }
                Expr::Var(var) => {
                    if !self.variables.contains_key(&var.name) {
                        return None;
                    }
                    fields.reverse();
                    let path = if fields.is_empty() {
                        None
                    } else {
                        Some(fields)
                    };
                    return Some((var.name.clone(), path, var.span));
                }
                _ => return None,
            }
        }
    }

    pub(crate) fn check_owner_not_borrowed(
        &self,
        owner: &str,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        self.check_borrow_allowed(owner, None, true, span)
    }

    /// A non-`Copy` move into a closure. A live loan is `BorrowConflict`.
    /// So is a carrier that is still this binding, including one that was
    /// never read after its `let` and whose loan already ended.
    pub(crate) fn check_closure_capture_move(
        &self,
        owner: &str,
        span: Span,
    ) -> Result<(), TypeCheckError> {
        self.check_owner_not_borrowed(owner, span)?;
        for borrow in &self.live_borrows {
            if borrow.owner != owner {
                continue;
            }
            if !borrow_paths_conflict(borrow.mutable, borrow.fields.as_deref(), true, None) {
                continue;
            }
            let still_held = borrow.carriers.iter().any(|(name, defined)| {
                self.variables
                    .get(name)
                    .is_some_and(|info| info.definition_span == *defined)
            });
            if still_held {
                return Err(TypeCheckError::BorrowConflict {
                    name: owner.to_string(),
                    description: "as mutable while it is already borrowed".to_string(),
                    span,
                });
            }
        }
        Ok(())
    }

    /// `let` registers the carrier before the binding exists. Fill its span
    /// once the binding is in scope so a later `let` of the same name is not
    /// the same carrier.
    pub(crate) fn stamp_carrier_span(&mut self, name: &str) {
        let Some(span) = self.variables.get(name).map(|info| info.definition_span) else {
            return;
        };
        let blank = Span {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
        };
        for borrow in &mut self.live_borrows {
            for (carrier, defined) in &mut borrow.carriers {
                if carrier == name && *defined == blank {
                    *defined = span;
                }
            }
        }
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

                let (state, ty) = {
                    let var_info = self.variables.get(&var_expr.name).ok_or_else(|| {
                        TypeCheckError::UndefinedVariable {
                            name: var_expr.name.clone(),
                            span: var_expr.span,
                        }
                    })?;
                    (var_info.state, var_info.ty.clone())
                };

                if state == OwnershipState::Moved {
                    return Err(TypeCheckError::UseAfterMove {
                        name: var_expr.name.clone(),
                        span: var_expr.span,
                    });
                }

                self.check_owner_not_borrowed(&var_expr.name, var_expr.span)?;

                // Primitives, references, and `T: Copy` parameters are copied.
                if self.satisfies_bound(&ty, "Copy") {
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
                self.check_ref_place(ref_expr.mutable, &ref_expr.inner)?;
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
                if let Some((owner, fields, span)) =
                    self.place_from_expr(&Expr::FieldAccess(acc.clone()))
                {
                    self.check_borrow_allowed(&owner, fields.as_deref(), false, span)?;
                }
                let base_ty = self.check_expr_with_context(&acc.base, true)?;
                if let Some(field_ty) = self.struct_field_type(&base_ty, &acc.field)
                    && !self.is_copy_type(&field_ty)
                    && (self.type_has_user_drop(&base_ty) || self.chain_has_user_drop(&acc.base))
                {
                    return Err(TypeCheckError::Message(
                        "cannot partially move a value that implements Drop".to_string(),
                    ));
                }
                Ok(())
            }
            Expr::BinOp(bin_op_expr) => {
                self.check_operand_for_nested_moves(&bin_op_expr.left)?;
                self.check_operand_for_nested_moves(&bin_op_expr.right)?;
                Ok(())
            }
            Expr::UnOp(un_op_expr) => {
                self.check_operand_for_nested_moves(&un_op_expr.operand)?;
                Ok(())
            }
            Expr::Send(send_expr) => {
                let channel_ty = self.type_info.expr_types.get(&send_expr.channel.id());
                if matches!(channel_ty, Some(Type::Endpoint { .. })) {
                    self.check_expr_for_moves(&send_expr.channel)?;
                }
                self.check_expr_for_moves(&send_expr.value)
            }
            Expr::Recv(recv_expr) => {
                let channel_ty = self.type_info.expr_types.get(&recv_expr.channel.id());
                if matches!(channel_ty, Some(Type::Endpoint { .. })) {
                    self.check_expr_for_moves(&recv_expr.channel)?;
                } else {
                    self.check_expr(&recv_expr.channel)?;
                }
                Ok(())
            }
            Expr::Spawn(_) => Ok(()),
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
            Expr::Try(try_expr) => self.check_expr_for_moves(&try_expr.operand),
            Expr::Call(call_expr) => {
                if let Some(var_ty) = self
                    .variables
                    .get(&call_expr.callee)
                    .map(|info| (info.ty.clone(), info.state))
                    && let (Type::Struct(name), state) = &var_ty
                    && self
                        .type_info
                        .closures
                        .get(name)
                        .is_some_and(|sig| sig.consumes)
                {
                    if *state == OwnershipState::Moved {
                        return Err(TypeCheckError::UseAfterMove {
                            name: call_expr.callee.clone(),
                            span: call_expr.span,
                        });
                    }
                    self.variables
                        .get_mut(&call_expr.callee)
                        .expect("closure binding")
                        .state = OwnershipState::Moved;
                }
                for arg in &call_expr.args {
                    self.check_expr_for_moves(arg)?;
                }
                Ok(())
            }
            Expr::FnLiteral(lit) => {
                let Some(ty) = self.type_info.expr_types.get(&lit.id).cloned() else {
                    return Ok(());
                };
                let Type::Struct(name) = ty else {
                    return Ok(());
                };
                let Some(sig) = self.type_info.closures.get(&name).cloned() else {
                    return Ok(());
                };
                for (name, cap_ty) in &sig.captures {
                    if self.is_copy_type(cap_ty) {
                        continue;
                    }
                    self.check_closure_capture_move(name, lit.span)?;
                    if let Some(info) = self.variables.get_mut(name) {
                        info.state = OwnershipState::Moved;
                    }
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
                self.check_operand_for_nested_moves(&index_expr.target)?;
                self.check_operand_for_nested_moves(&index_expr.index)?;
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

    /// A place read (`x`, `s.f`, `a[i]`) is not moved. A call nested in that
    /// operand still moves its arguments (`sum + Box::unwrap(extra)`).
    fn check_operand_for_nested_moves(&mut self, expr: &Expr) -> Result<(), TypeCheckError> {
        match expr {
            Expr::Var(_)
            | Expr::Lit(_)
            | Expr::BoolLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::StringLit(_)
            | Expr::TypeConst(_)
            | Expr::Ref(_) => Ok(()),
            Expr::FieldAccess(acc) => self.check_operand_for_nested_moves(&acc.base),
            Expr::Index(index_expr) => {
                self.check_operand_for_nested_moves(&index_expr.target)?;
                self.check_operand_for_nested_moves(&index_expr.index)
            }
            Expr::BinOp(op) => {
                self.check_operand_for_nested_moves(&op.left)?;
                self.check_operand_for_nested_moves(&op.right)
            }
            Expr::UnOp(op) => self.check_operand_for_nested_moves(&op.operand),
            other => self.check_expr_for_moves(other),
        }
    }

    /// Types copied rather than moved at the ownership level (ION_SPEC §5.2).
    pub(crate) fn is_copy_type(&self, ty: &Type) -> bool {
        let resolved = match ty {
            Type::Struct(name) | Type::Enum(name) => {
                if let Some(alias) = self.type_aliases.get(name) {
                    return self.is_copy_type(&alias.target);
                }
                ty
            }
            _ => ty,
        };
        type_is_copy(resolved, &self.structs, &self.enums, &self.drop_impls)
    }
}

/// Structural `Copy`: primitives, references, function pointers, and aggregates
/// whose fields are `Copy` and that have no `impl Drop`.
pub(crate) fn type_is_copy(
    ty: &Type,
    structs: &HashMap<String, StructDecl>,
    enums: &HashMap<String, EnumDecl>,
    drop_impls: &HashMap<String, String>,
) -> bool {
    type_is_copy_rec(ty, structs, enums, drop_impls, &mut HashSet::new())
}

fn type_is_copy_rec(
    ty: &Type,
    structs: &HashMap<String, StructDecl>,
    enums: &HashMap<String, EnumDecl>,
    drop_impls: &HashMap<String, String>,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
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
        | Type::Allocator => true,
        Type::RawPtr { .. }
        | Type::Channel { .. }
        | Type::Sender { .. }
        | Type::Receiver { .. }
        | Type::Box { .. }
        | Type::Vec { .. }
        | Type::String
        | Type::Str
        | Type::Slice { .. }
        | Type::JoinHandle { .. }
        | Type::File
        | Type::Endpoint { .. } => false,
        Type::Array { inner, .. } => type_is_copy_rec(inner, structs, enums, drop_impls, visiting),
        Type::Tuple { elements } => elements
            .iter()
            .all(|elem| type_is_copy_rec(elem, structs, enums, drop_impls, visiting)),
        Type::Struct(name) => copy_named(name, &[], structs, enums, drop_impls, visiting),
        Type::Enum(name) => copy_named(name, &[], structs, enums, drop_impls, visiting),
        Type::Generic { name, params } => {
            copy_named(name, params, structs, enums, drop_impls, visiting)
        }
    }
}

fn copy_named(
    name: &str,
    args: &[Type],
    structs: &HashMap<String, StructDecl>,
    enums: &HashMap<String, EnumDecl>,
    drop_impls: &HashMap<String, String>,
    visiting: &mut HashSet<String>,
) -> bool {
    if drop_impls.contains_key(name) {
        return false;
    }
    if !visiting.insert(name.to_string()) {
        return true;
    }
    let result = if let Some(decl) = structs.get(name) {
        let subst = generic_subst(&decl.generics, args);
        decl.fields.iter().all(|field| {
            let ty = substitute_type(&field.ty, &subst);
            type_is_copy_rec(&ty, structs, enums, drop_impls, visiting)
        })
    } else if let Some(decl) = enums.get(name) {
        let subst = generic_subst(&decl.generics, args);
        decl.variants.iter().all(|variant| {
            variant.payload_types.iter().all(|ty| {
                let ty = substitute_type(ty, &subst);
                type_is_copy_rec(&ty, structs, enums, drop_impls, visiting)
            }) && variant.named_fields.as_ref().is_none_or(|fields| {
                fields.iter().all(|(_, ty)| {
                    let ty = substitute_type(ty, &subst);
                    type_is_copy_rec(&ty, structs, enums, drop_impls, visiting)
                })
            })
        })
    } else {
        false
    };
    visiting.remove(name);
    result
}

fn generic_subst(params: &[TypeParam], args: &[Type]) -> HashMap<String, Type> {
    params
        .iter()
        .filter(|param| param.const_ty.is_none())
        .zip(args.iter())
        .map(|(param, arg)| (param.name.clone(), arg.clone()))
        .collect()
}

pub(crate) fn lasting_borrow_uses(stmts: &[Stmt]) -> HashMap<String, usize> {
    let mut last = HashMap::new();
    for (index, stmt) in stmts.iter().enumerate() {
        let mut names = Vec::new();
        collect_borrow_use_names(stmt, &mut names);
        for name in names {
            last.insert(name, index);
        }
    }
    last
}

fn collect_borrow_use_names(stmt: &Stmt, names: &mut Vec<String>) {
    match stmt {
        Stmt::Let(let_stmt) => {
            names.push(let_stmt.name.clone());
            if let Some(init) = &let_stmt.init {
                collect_expr_names(init, names);
            }
        }
        Stmt::Return(ret) => {
            if let Some(value) = &ret.value {
                collect_expr_names(value, names);
            }
        }
        Stmt::Expr(expr) => collect_expr_names(&expr.expr, names),
        Stmt::If(if_stmt) => {
            collect_expr_names(&if_stmt.cond, names);
            for inner in &if_stmt.then_block.statements {
                collect_borrow_use_names(inner, names);
            }
            if let Some(else_block) = &if_stmt.else_block {
                for inner in &else_block.statements {
                    collect_borrow_use_names(inner, names);
                }
            }
        }
        Stmt::While(while_stmt) => {
            collect_expr_names(&while_stmt.cond, names);
            for inner in &while_stmt.body.statements {
                collect_borrow_use_names(inner, names);
            }
        }
        Stmt::Loop(loop_stmt) => {
            for inner in &loop_stmt.body.statements {
                collect_borrow_use_names(inner, names);
            }
        }
        Stmt::UnsafeBlock(block) => {
            for inner in &block.body.statements {
                collect_borrow_use_names(inner, names);
            }
        }
        Stmt::Scope(block) => {
            for inner in &block.body.statements {
                collect_borrow_use_names(inner, names);
            }
        }
        Stmt::For(for_stmt) => {
            collect_expr_names(&for_stmt.iterable, names);
            for inner in &for_stmt.body.statements {
                collect_borrow_use_names(inner, names);
            }
        }
        Stmt::Defer(defer_stmt) => collect_expr_names(&defer_stmt.expr, names),
        Stmt::Spawn(spawn) => {
            for inner in &spawn.body.statements {
                collect_borrow_use_names(inner, names);
            }
        }
        Stmt::Select(select) => {
            for arm in &select.recv_arms {
                collect_expr_names(&arm.recv.channel, names);
                collect_block_names(&arm.body, names);
            }
            if let Some(body) = &select.default_body {
                collect_block_names(body, names);
            }
            if let Some(ms) = &select.timeout_ms {
                collect_expr_names(ms, names);
            }
            if let Some(body) = &select.timeout_body {
                collect_block_names(body, names);
            }
        }
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

fn collect_block_names(block: &Block, names: &mut Vec<String>) {
    for stmt in &block.statements {
        collect_borrow_use_names(stmt, names);
    }
}

fn collect_expr_names(expr: &Expr, names: &mut Vec<String>) {
    match expr {
        Expr::Var(var) => names.push(var.name.clone()),
        Expr::BinOp(op) => {
            collect_expr_names(&op.left, names);
            collect_expr_names(&op.right, names);
        }
        Expr::UnOp(op) => collect_expr_names(&op.operand, names),
        Expr::Ref(inner) => collect_expr_names(&inner.inner, names),
        Expr::Call(call) => {
            for arg in &call.args {
                collect_expr_names(arg, names);
            }
        }
        Expr::MethodCall(call) => {
            collect_expr_names(&call.receiver, names);
            for arg in &call.args {
                collect_expr_names(arg, names);
            }
        }
        Expr::FieldAccess(acc) => collect_expr_names(&acc.base, names),
        Expr::Index(index) => {
            collect_expr_names(&index.target, names);
            collect_expr_names(&index.index, names);
        }
        Expr::Assign(assign) => {
            collect_expr_names(&assign.target, names);
            collect_expr_names(&assign.value, names);
        }
        Expr::StructLit(lit) => {
            for field in &lit.fields {
                collect_expr_names(&field.value, names);
            }
        }
        Expr::EnumLit(lit) => {
            for arg in &lit.args {
                collect_expr_names(arg, names);
            }
            if let Some(fields) = &lit.named_fields {
                for (_, value) in fields {
                    collect_expr_names(value, names);
                }
            }
        }
        Expr::Match(match_expr) => {
            collect_expr_names(&match_expr.expr, names);
            for arm in &match_expr.arms {
                if let Some(guard) = &arm.guard {
                    collect_expr_names(guard, names);
                }
                collect_block_names(&arm.body, names);
            }
        }
        Expr::Try(try_expr) => collect_expr_names(&try_expr.operand, names),
        Expr::Send(send) => {
            collect_expr_names(&send.channel, names);
            collect_expr_names(&send.value, names);
        }
        Expr::Recv(recv) => collect_expr_names(&recv.channel, names),
        Expr::Spawn(spawn) => collect_block_names(&spawn.body, names),
        Expr::ArrayLiteral(arr) => {
            for elem in &arr.elements {
                collect_expr_names(elem, names);
            }
            if let Some((value, _)) = &arr.repeat {
                collect_expr_names(value, names);
            }
        }
        Expr::TupleLit(tuple) => {
            for elem in &tuple.elements {
                collect_expr_names(elem, names);
            }
        }
        Expr::Cast(cast) => collect_expr_names(&cast.expr, names),
        // Names in a fn literal are not uses of an outer loan. A non-Copy
        // capture is checked at the move, including a carrier still in scope.
        Expr::FnLiteral(_)
        | Expr::Lit(_)
        | Expr::BoolLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::StringLit(_)
        | Expr::TypeConst(_) => {}
    }
}

fn borrow_paths_conflict(
    existing_mut: bool,
    existing: Option<&[String]>,
    new_mut: bool,
    new_fields: Option<&[String]>,
) -> bool {
    if !existing_mut && !new_mut {
        return false;
    }
    match (existing, new_fields) {
        (None, _) | (_, None) => true,
        (Some(left), Some(right)) => {
            if left.is_empty() || right.is_empty() {
                return true;
            }
            left.iter()
                .zip(right.iter())
                .take_while(|(a, b)| a == b)
                .count()
                > 0
        }
    }
}
