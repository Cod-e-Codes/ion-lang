//! Expression temporaries.
//!
//! An owned rvalue that is not moved into a binding, return, store, or owning
//! parameter is evaluated once and dropped at the end of that full expression.
//! `&&` and `||` each plan one side, so the right-hand side runs only when C
//! evaluates it.

use super::*;

pub(super) struct TempPlan {
    decls: Vec<String>,
    drops: Vec<String>,
    /// `(c name, type)` for temps that scope exit must drop if a `return` runs first.
    owned: Vec<(String, Type)>,
    bind_start: usize,
}

enum TempBody {
    Expr,
    Conditional,
    Typed(Type),
}

impl Codegen {
    pub(super) fn bound_operand_name(&self, expr: &IREexpr) -> Option<&str> {
        let ptr = expr_ptr(expr);
        if self.temp_init_root == Some(ptr) {
            return None;
        }
        self.bound_operands
            .iter()
            .rev()
            .find(|(p, _)| *p == ptr)
            .map(|(_, name)| name.as_str())
    }

    /// Value of a full expression. Nested owned temporaries are dropped after the
    /// value is produced. `moved` means the caller keeps the result.
    pub(super) fn write_temp_expr(&mut self, expr: &IREexpr, moved: bool) {
        self.write_temp_expr_mode(expr, moved, TempBody::Expr);
    }

    pub(super) fn write_temp_expr_typed(&mut self, expr: &IREexpr, moved: bool, ty: &Type) {
        self.write_temp_expr_mode(expr, moved, TempBody::Typed(ty.clone()));
    }

    pub(super) fn write_temp_conditional(&mut self, expr: &IREexpr) {
        self.write_temp_expr_mode(expr, false, TempBody::Conditional);
    }

    /// Discarded statement. The result is dropped when it still owns a value.
    pub(super) fn write_temp_stmt(&mut self, expr: &IREexpr) {
        let plan = self.plan_temps(expr, false);
        if plan.decls.is_empty() && plan.drops.is_empty() {
            self.write_indent();
            self.generate_expr(expr);
            self.writeln(";");
            self.finish_temp_plan(&plan);
            return;
        }
        self.register_temp_owned(&plan);
        self.write_indent();
        self.writeln("{");
        self.indent_level += 1;
        for decl in &plan.decls {
            self.write_indent();
            self.writeln(decl);
        }
        self.write_indent();
        self.write("(void)(");
        self.generate_expr(expr);
        self.writeln(");");
        self.emit_temp_drops(&plan);
        self.indent_level -= 1;
        self.write_indent();
        self.writeln("}");
        self.finish_temp_plan(&plan);
    }

    fn write_temp_expr_mode(&mut self, expr: &IREexpr, moved: bool, body: TempBody) {
        let plan = self.plan_temps(expr, moved);
        if plan.decls.is_empty() && plan.drops.is_empty() {
            self.emit_temp_body(expr, &body);
            self.finish_temp_plan(&plan);
            return;
        }
        let result_ty = match &body {
            TempBody::Typed(ty) => Some(ty.clone()),
            _ => self.stored_expr_type(expr),
        };
        let Some(result_ty) = result_ty.filter(|ty| !matches!(ty, Type::Void)) else {
            self.register_temp_owned(&plan);
            self.write("({ ");
            for decl in &plan.decls {
                self.write(decl);
                self.write(" ");
            }
            self.emit_temp_body(expr, &body);
            self.write("; ");
            self.write_temp_drops_inline(&plan);
            self.write("0; })");
            self.finish_temp_plan(&plan);
            return;
        };
        if self.type_is_array(&result_ty) {
            // An array cannot be yielded from a statement expression. Drop the
            // plan's bindings so element emission plans its own temps.
            self.bound_operands.truncate(plan.bind_start);
            self.emit_temp_body(expr, &body);
            return;
        }
        self.register_temp_owned(&plan);
        let result_c = self.type_to_c(&result_ty);
        let n = self.temp_var_counter;
        self.temp_var_counter += 1;
        let result_name = format!("_ion_fv{n}");
        let captured = String::new();
        let old = std::mem::replace(&mut self.output, captured);
        self.emit_temp_body(expr, &body);
        let captured = std::mem::replace(&mut self.output, old);
        self.write("({ ");
        for decl in &plan.decls {
            self.write(decl);
            self.write(" ");
        }
        self.write(&format!("{result_c} {result_name} = ({captured}); "));
        self.write_temp_drops_inline(&plan);
        self.write(&format!("{result_name}; }})"));
        self.finish_temp_plan(&plan);
    }

    fn emit_temp_body(&mut self, expr: &IREexpr, body: &TempBody) {
        match body {
            TempBody::Expr => self.generate_expr(expr),
            TempBody::Conditional => self.generate_expr_conditional(expr),
            TempBody::Typed(ty) => self.generate_expr_with_type(expr, Some(ty)),
        }
    }

    fn emit_temp_drops(&mut self, plan: &TempPlan) {
        for drop_stmt in plan.drops.iter().rev() {
            if drop_stmt.is_empty() {
                continue;
            }
            self.write_indent();
            self.write(drop_stmt);
            if !drop_stmt.ends_with('\n') {
                self.writeln("");
            }
        }
    }

    fn write_temp_drops_inline(&mut self, plan: &TempPlan) {
        for drop_stmt in plan.drops.iter().rev() {
            let text = drop_stmt.trim();
            if text.is_empty() {
                continue;
            }
            self.write(text);
            if !text.ends_with(';') && !text.ends_with('}') {
                self.write(";");
            }
            self.write(" ");
        }
    }

    fn register_temp_owned(&mut self, plan: &TempPlan) {
        for (name, ty) in &plan.owned {
            self.scope_register_c(name, name.clone(), ty);
            if let Some(frame) = self.scope_stack.last_mut()
                && let Some(binding) = frame.bindings.iter_mut().rev().find(|b| b.c_name == *name)
            {
                binding.read = true;
            }
        }
    }

    fn finish_temp_plan(&mut self, plan: &TempPlan) {
        for (name, _) in &plan.owned {
            self.scope_mark_moved(name);
        }
        self.bound_operands.truncate(plan.bind_start);
    }

    /// Declare temps for an array initializer, which cannot sit inside a statement expression.
    /// [`Self::end_temp_hoist`] drops them after the array is stored.
    pub(super) fn begin_temp_hoist(&mut self, expr: &IREexpr, moved: bool) {
        let plan = self.plan_temps(expr, moved);
        if plan.decls.is_empty() && plan.drops.is_empty() {
            self.bound_operands.truncate(plan.bind_start);
            return;
        }
        self.register_temp_owned(&plan);
        for decl in &plan.decls {
            self.write_indent();
            self.writeln(decl);
        }
        self.temp_hoist.push(plan);
    }

    /// Same as [`Self::begin_temp_hoist`], written into the current expression.
    pub(super) fn begin_temp_hoist_inline(&mut self, expr: &IREexpr, moved: bool) {
        let plan = self.plan_temps(expr, moved);
        if plan.decls.is_empty() && plan.drops.is_empty() {
            self.bound_operands.truncate(plan.bind_start);
            return;
        }
        self.register_temp_owned(&plan);
        for decl in &plan.decls {
            self.write(decl);
            self.write(" ");
        }
        self.temp_hoist.push(plan);
    }

    pub(super) fn end_temp_hoist_inline(&mut self) {
        let Some(plan) = self.temp_hoist.pop() else {
            return;
        };
        self.write_temp_drops_inline(&plan);
        self.finish_temp_plan(&plan);
    }

    pub(super) fn end_temp_hoist(&mut self) {
        let Some(plan) = self.temp_hoist.pop() else {
            return;
        };
        self.emit_temp_drops(&plan);
        self.finish_temp_plan(&plan);
    }

    pub(super) fn end_temp_hoist_to(&mut self, mark: usize) {
        while self.temp_hoist.len() > mark {
            self.end_temp_hoist();
        }
    }

    fn plan_temps(&mut self, expr: &IREexpr, moved: bool) -> TempPlan {
        let mut plan = TempPlan {
            decls: Vec::new(),
            drops: Vec::new(),
            owned: Vec::new(),
            bind_start: self.bound_operands.len(),
        };
        self.plan_rec(expr, moved, false, false, &mut plan);
        plan
    }

    fn plan_rec(
        &mut self,
        expr: &IREexpr,
        moved: bool,
        string_value: bool,
        force: bool,
        plan: &mut TempPlan,
    ) {
        if self.already_bound(expr) {
            return;
        }
        self.plan_rec_inner(expr, moved, string_value, force, plan);
    }

    fn plan_rec_inner(
        &mut self,
        expr: &IREexpr,
        moved: bool,
        string_value: bool,
        force: bool,
        plan: &mut TempPlan,
    ) {
        match expr {
            IREexpr::BinOp {
                op: BinOp::And | BinOp::Or,
                ..
            } => {}
            IREexpr::BinOp {
                op, left, right, ..
            } if matches!(op, BinOp::Eq | BinOp::Ne)
                && self.is_string_compare_operand(left)
                && self.is_string_compare_operand(right) =>
            {
                self.plan_rec(left, false, true, false, plan);
                self.plan_rec(right, false, true, false, plan);
            }
            IREexpr::BinOp { left, right, .. } => {
                self.plan_rec(left, false, false, false, plan);
                self.plan_rec(right, false, false, false, plan);
            }
            IREexpr::UnOp { operand, .. } => {
                self.plan_rec(operand, false, false, false, plan);
            }
            IREexpr::Cast { expr, .. } => {
                self.plan_rec(expr, moved, string_value, force, plan);
            }
            IREexpr::AddressOf { inner, .. } => {
                self.plan_rec(inner, false, false, force, plan);
            }
            IREexpr::FieldAccess {
                base, field, ty, ..
            } => {
                self.plan_rec(base, false, false, false, plan);
                if moved
                    && self.type_needs_drop(ty)
                    && let Some(name) = self.bound_operand_name(base).map(str::to_string)
                {
                    let sep = self.field_c_separator(base);
                    let path = format!("{name}{sep}{field}");
                    plan.drops
                        .push(format!("{}; ", self.moved_clear_line(&path, ty)));
                }
            }
            IREexpr::Index {
                target,
                index,
                target_type,
            } => {
                self.plan_rec(target, false, false, true, plan);
                let null_moved = moved
                    && target_type.as_ref().is_some_and(|target_ty| {
                        Self::index_element_type(target_ty)
                            .is_some_and(|elem| self.type_needs_drop(&elem))
                    });
                self.plan_rec(index, true, false, null_moved, plan);
                if null_moved
                    && let Some(target_ty) = target_type.as_ref()
                    && let Some(elem) = Self::index_element_type(target_ty)
                    && self.already_bound(target)
                {
                    let path = self.index_element_lvalue(target, index, target_ty);
                    plan.drops
                        .push(format!("{}; ", self.moved_clear_line(&path, &elem)));
                }
            }
            IREexpr::Call { callee, args, .. } => {
                for (i, arg) in args.iter().enumerate() {
                    let arg_moved = self.call_arg_moved(callee, i, arg);
                    let arg_force = self.call_arg_force_once(callee, i);
                    self.plan_rec(arg, arg_moved, false, arg_force, plan);
                }
            }
            IREexpr::StructLit { fields, .. } => {
                for field in fields {
                    self.plan_rec(&field.value, true, false, false, plan);
                }
            }
            IREexpr::EnumLit {
                args, named_fields, ..
            } => {
                for arg in args {
                    self.plan_rec(arg, true, false, false, plan);
                }
                if let Some(fields) = named_fields {
                    for (_, value) in fields {
                        self.plan_rec(value, true, false, false, plan);
                    }
                }
            }
            IREexpr::TupleLit { elements, .. } => {
                for elem in elements {
                    self.plan_rec(elem, true, false, false, plan);
                }
            }
            IREexpr::ArrayLiteral { elements, repeat } => {
                // `[expr; N]` stays source-unrolled, so each copy plans itself.
                if repeat.is_none() {
                    for elem in elements {
                        self.plan_rec(elem, true, false, false, plan);
                    }
                }
            }
            IREexpr::Recv { channel, .. } => {
                self.plan_rec(channel, false, false, false, plan);
            }
            IREexpr::Match { expr, .. } => {
                self.plan_rec(expr, true, false, false, plan);
            }
            IREexpr::Send { channel, value, .. } => {
                self.plan_rec(channel, false, false, false, plan);
                self.plan_rec(value, true, false, false, plan);
            }
            IREexpr::Assign { value, .. } => {
                self.plan_rec(value, true, false, false, plan);
            }
            IREexpr::AssignField { value, .. } | IREexpr::AssignIndex { value, .. } => {
                self.plan_rec(value, true, false, false, plan);
            }
            _ => {}
        }
        if self.should_bind(expr, moved, string_value, force) {
            self.materialize(expr, moved, string_value, plan);
        }
    }

    fn should_bind(&self, expr: &IREexpr, moved: bool, string_value: bool, force: bool) -> bool {
        if self.is_place_expr(expr) || self.already_bound(expr) {
            return false;
        }
        // The owner temp is dropped. Binding the projection as well would free it twice.
        // A moved field or element is copied out by the parent, then the slot is cleared.
        if matches!(expr, IREexpr::FieldAccess { .. } | IREexpr::Index { .. }) {
            return false;
        }
        if matches!(
            expr,
            IREexpr::Assign { .. } | IREexpr::AssignIndex { .. } | IREexpr::AssignField { .. }
        ) {
            return false;
        }
        if matches!(expr, IREexpr::StringLit(_)) {
            return string_value && !moved;
        }
        if is_pure_literal(expr) {
            return false;
        }
        let Some(ty) = self.expr_owned_type(expr) else {
            return false;
        };
        if matches!(ty, Type::Void | Type::Ref { .. }) {
            return false;
        }
        // An indexed call must be a real array lvalue. Yielding the array from a
        // statement expression ends that object before the subscript reads it.
        if self.type_is_array(&ty) {
            return force && self.expr_returns_array(expr);
        }
        if !moved && self.type_needs_drop(&ty) {
            return true;
        }
        force
    }

    fn expr_owned_type(&self, expr: &IREexpr) -> Option<Type> {
        if let Some(ty) = self.stored_expr_type(expr) {
            return Some(ty);
        }
        if let IREexpr::StructLit { type_name, .. } = expr {
            let decl = self.struct_map.get(type_name)?;
            if decl.generics.is_empty() {
                return Some(Type::Struct(type_name.clone()));
            }
        }
        None
    }

    fn materialize(
        &mut self,
        expr: &IREexpr,
        moved: bool,
        string_value: bool,
        plan: &mut TempPlan,
    ) {
        if let Some(ty) = self.array_expr_type(expr) {
            self.materialize_array_call(expr, &ty, moved, plan);
            return;
        }
        let Some(ty) = self.expr_owned_type(expr) else {
            return;
        };
        if self.type_is_array(&ty) || matches!(ty, Type::Void) {
            return;
        }
        let c_ty = self.type_to_c(&ty);
        let n = self.temp_var_counter;
        self.temp_var_counter += 1;
        let name = format!("_ion_op{n}");
        let init = self.capture_temp_init(expr, string_value);
        plan.decls.push(format!("{c_ty} {name} = {init};"));
        self.bound_operands.push((expr_ptr(expr), name.clone()));
        let drop_owned = !moved && self.type_needs_drop(&ty);
        if drop_owned {
            let drop_stmt = self.capture_drop_at_path(&name, &ty);
            plan.drops.push(drop_stmt);
            plan.owned.push((name, ty));
        }
    }

    /// Copy an array-returning call into a named array. C cannot initialize an
    /// array with `=`, and the wrapper's `_data` must be read while the wrapper
    /// is still alive.
    fn materialize_array_call(
        &mut self,
        expr: &IREexpr,
        ty: &Type,
        moved: bool,
        plan: &mut TempPlan,
    ) {
        let c_ty = self.type_to_c(ty);
        let wrapper = array_return_wrapper_name(ty);
        let n = self.temp_var_counter;
        self.temp_var_counter += 1;
        let name = format!("_ion_op{n}");
        let wname = format!("_ion_w{n}");
        let saved = self.keep_array_wrapper;
        self.keep_array_wrapper = true;
        let init = self.capture_temp_init(expr, false);
        self.keep_array_wrapper = saved;
        plan.decls.push(format!(
            "{c_ty} {name}; {wrapper} {wname} = {init}; memcpy({name}, {wname}._data, sizeof({name}));"
        ));
        self.bound_operands.push((expr_ptr(expr), name.clone()));
        let drop_owned = !moved && self.type_needs_drop(ty);
        if drop_owned {
            let drop_stmt = self.capture_drop_at_path(&name, ty);
            plan.drops.push(drop_stmt);
            plan.owned.push((name, ty.clone()));
        }
    }

    fn capture_temp_init(&mut self, expr: &IREexpr, string_value: bool) -> String {
        if string_value && let IREexpr::StringLit(value) = expr {
            let captured = String::new();
            let old = std::mem::replace(&mut self.output, captured);
            self.write_ion_string_from_literal(value);
            return std::mem::replace(&mut self.output, old);
        }
        let prev = self.temp_init_root.replace(expr_ptr(expr));
        let code = self.capture_expr_code(expr);
        self.temp_init_root = prev;
        code
    }

    fn already_bound(&self, expr: &IREexpr) -> bool {
        let ptr = expr_ptr(expr);
        self.bound_operands.iter().any(|(p, _)| *p == ptr)
    }

    pub(super) fn eq_temp_is_sole_owner(&self, expr: &IREexpr) -> bool {
        !self.is_place_expr(expr) && !self.already_bound(expr)
    }

    fn is_place_expr(&self, expr: &IREexpr) -> bool {
        match expr {
            IREexpr::Var(_) => true,
            IREexpr::FieldAccess { base, .. } => self.is_place_expr(base),
            IREexpr::Index { target, .. } => self.is_place_expr(target),
            _ => false,
        }
    }

    pub(super) fn call_arg_moved(&self, callee: &str, index: usize, arg: &IREexpr) -> bool {
        if self.builtin_arg_borrowed(callee, index) || matches!(arg, IREexpr::AddressOf { .. }) {
            return false;
        }
        let c_name = self.resolve_c_function_name(callee);
        if let Some(params) = self.lookup_call_param_types(callee, &c_name)
            && let Some(ty) = params.get(index)
        {
            if matches!(ty, Type::Ref { .. }) {
                return false;
            }
            return !self.value_is_copy(ty);
        }
        match self.stored_expr_type(arg) {
            Some(Type::Ref { .. }) => false,
            Some(ty) => !self.value_is_copy(&ty),
            // Aggregate literals have no stored type. A builtin that does not
            // borrow this argument takes it by value (`Box::new(Guard { ... })`).
            None => self.builtin_arg_by_value(callee, index),
        }
    }

    fn builtin_arg_borrowed(&self, callee: &str, index: usize) -> bool {
        matches!(
            (callee, index),
            ("String::len", 0)
                | ("Vec::len", 0)
                | ("Vec::capacity", 0)
                | ("String::push_str", 0)
                | ("String::push_str", 1)
                | ("String::push_byte", 0)
                | ("String::get", 0)
                | ("String::from", 0)
                | ("Vec::push", 0)
                | ("Vec::pop", 0)
                | ("Vec::get", 0)
                | ("Vec::get_ref", 0)
                | ("Vec::set", 0)
                | ("Vec::set", 1)
                | ("Slice::len", 0)
                | ("Slice::get_ref", 0)
                | ("File::open", 0)
                | ("File::create", 0)
                | ("File::read", 0)
                | ("File::read", 1)
                | ("File::write", 0)
                | ("File::write", 1)
                | ("File::close", 0)
                | ("Arena::get_ref", 0)
        )
    }

    /// Builtins that take this argument by value when the argument has no stored type.
    fn builtin_arg_by_value(&self, callee: &str, index: usize) -> bool {
        if self.builtin_arg_borrowed(callee, index) {
            return false;
        }
        callee.starts_with("Box::new")
            || matches!(
                callee,
                "Box::unwrap" | "String::from_utf8" | "Vec::push" | "Vec::set" | "join"
            )
    }

    /// These builtins paste the operand text into both a null check and a field load.
    fn call_arg_force_once(&self, callee: &str, index: usize) -> bool {
        matches!(
            (callee, index),
            ("String::len", 0) | ("Vec::len", 0) | ("Vec::capacity", 0) | ("String::push_str", 1)
        )
    }

    fn field_c_separator(&self, base: &IREexpr) -> &'static str {
        if matches!(base, IREexpr::FieldAccess { .. }) {
            return ".";
        }
        match self.stored_expr_type(base) {
            Some(Type::Ref { inner, .. })
                if matches!(
                    inner.as_ref(),
                    Type::Struct(_) | Type::Generic { .. } | Type::Tuple { .. }
                ) =>
            {
                "->"
            }
            _ => ".",
        }
    }
}

fn expr_ptr(expr: &IREexpr) -> usize {
    expr as *const IREexpr as usize
}

fn is_pure_literal(expr: &IREexpr) -> bool {
    matches!(
        expr,
        IREexpr::Lit(_)
            | IREexpr::BoolLiteral(_)
            | IREexpr::FloatLiteral(_)
            | IREexpr::IntLimit { .. }
    )
}
