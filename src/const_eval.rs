//! Interpreter for the type-checked const subset: const items, const fn, const
//! parameters, and `const_assert`.

use crate::ast::*;
use crate::tc::TypeInfo;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
enum Value {
    Int(i64),
    Bool(bool),
}

pub fn prepare(program: &mut Program) -> Result<(), String> {
    validate_const_fns(program)?;
    let values = eval_items(program)?;
    resolve_array_lengths(program, &values)?;
    fold_const_uses(program, &values)?;
    eval_const_asserts(program, &values)?;
    Ok(())
}

/// After type checking, clone each `fn f<const N: int>` call into a concrete
/// function and record that callee on `type_info`.
pub fn instantiate(program: &mut Program, type_info: &mut TypeInfo) -> Result<(), String> {
    let templates: Vec<FnDecl> = program
        .functions
        .iter()
        .filter(|f| f.generics.iter().any(|g| g.const_ty.is_some()))
        .cloned()
        .collect();
    if templates.is_empty() {
        return Ok(());
    }
    let mut additions: Vec<FnDecl> = Vec::new();
    let mut records = Vec::new();
    for func in &program.functions {
        collect_const_calls(
            &func.body,
            &templates,
            type_info,
            &mut additions,
            &mut |id, name| records.push((id, name)),
        )?;
    }
    for (id, name) in records {
        type_info.call_callees.insert(id, name);
    }
    for extra in additions {
        if !program.functions.iter().any(|f| f.name == extra.name) {
            program.functions.push(extra);
        }
    }
    Ok(())
}

fn validate_const_fns(program: &Program) -> Result<(), String> {
    for func in &program.functions {
        if func.const_fn {
            validate_block(&func.body, program)?;
        }
    }
    Ok(())
}

fn validate_block(block: &Block, program: &Program) -> Result<(), String> {
    for stmt in &block.statements {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Some(init) = &let_stmt.init {
                    validate_expr(init, program)?;
                }
            }
            Stmt::Return(ret) => {
                if let Some(value) = &ret.value {
                    validate_expr(value, program)?;
                }
            }
            Stmt::Expr(expr) => validate_expr(&expr.expr, program)?,
            Stmt::If(if_stmt) => {
                validate_expr(&if_stmt.cond, program)?;
                validate_block(&if_stmt.then_block, program)?;
                if let Some(else_block) = &if_stmt.else_block {
                    validate_block(else_block, program)?;
                }
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
            _ => {
                return Err("const fn body cannot use loops, spawn, defer, or unsafe".to_string());
            }
        }
    }
    Ok(())
}

fn validate_expr(expr: &Expr, program: &Program) -> Result<(), String> {
    match expr {
        Expr::Lit(_) | Expr::BoolLiteral(_) | Expr::Var(_) => Ok(()),
        Expr::BinOp(op) => {
            validate_expr(&op.left, program)?;
            validate_expr(&op.right, program)
        }
        Expr::UnOp(op) => validate_expr(&op.operand, program),
        Expr::StructLit(lit) => {
            for field in &lit.fields {
                validate_expr(&field.value, program)?;
            }
            Ok(())
        }
        Expr::TupleLit(lit) => {
            for elem in &lit.elements {
                validate_expr(elem, program)?;
            }
            Ok(())
        }
        Expr::FieldAccess(access) => validate_expr(&access.base, program),
        Expr::Cast(cast) => validate_expr(&cast.expr, program),
        Expr::Match(match_expr) => {
            validate_expr(&match_expr.expr, program)?;
            for arm in &match_expr.arms {
                validate_block(&arm.body, program)?;
            }
            Ok(())
        }
        Expr::Call(call) => {
            if call.callee == "const_assert" {
                if call.args.len() != 1 {
                    return Err("const_assert takes one argument".to_string());
                }
                return validate_expr(&call.args[0], program);
            }
            let Some(func) = program.functions.iter().find(|f| f.name == call.callee) else {
                return Err(format!("const context cannot call '{}'", call.callee));
            };
            if !func.const_fn {
                return Err(format!("'{}' is not a const fn", call.callee));
            }
            for arg in &call.args {
                validate_expr(arg, program)?;
            }
            Ok(())
        }
        _ => Err("expression is not allowed in a const context".to_string()),
    }
}

fn eval_items(program: &Program) -> Result<HashMap<String, Value>, String> {
    let mut values = HashMap::new();
    let mut pending: Vec<&ConstDecl> = program.consts.iter().collect();
    let mut guard = 0;
    while !pending.is_empty() {
        guard += 1;
        if guard > pending.len() + program.consts.len() + 1 {
            return Err(format!(
                "const '{}' depends on a cycle or an unknown const",
                pending[0].name
            ));
        }
        let decl = pending.remove(0);
        match eval_expr(&decl.init, program, &values) {
            Ok(value) => {
                check_const_type(&decl.ty, &value, &decl.name)?;
                values.insert(decl.name.clone(), value);
            }
            Err(err) if err.contains("unknown const") => pending.push(decl),
            Err(err) => return Err(err),
        }
    }
    Ok(values)
}

fn check_const_type(ty: &Type, value: &Value, name: &str) -> Result<(), String> {
    match value {
        Value::Bool(_) if matches!(ty, Type::Bool) => Ok(()),
        Value::Int(n) if crate::integer_limits::integer_value_fits(ty, *n) => Ok(()),
        _ => Err(format!("const '{name}' does not match its type")),
    }
}

fn eval_expr(
    expr: &Expr,
    program: &Program,
    env: &HashMap<String, Value>,
) -> Result<Value, String> {
    match expr {
        Expr::Lit(lit) => Ok(Value::Int(lit.value)),
        Expr::BoolLiteral(lit) => Ok(Value::Bool(lit.value)),
        Expr::Var(var) => env
            .get(&var.name)
            .cloned()
            .ok_or_else(|| format!("unknown const '{}'", var.name)),
        Expr::UnOp(op) => {
            let value = eval_expr(&op.operand, program, env)?;
            match (op.op, value) {
                (UnOp::Neg, Value::Int(n)) => Ok(Value::Int(n.wrapping_neg())),
                (UnOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                _ => Err("const unary operator does not apply to this value".to_string()),
            }
        }
        Expr::BinOp(op) => eval_binop(op.op, &op.left, &op.right, program, env),
        Expr::Call(call) => {
            if call.callee == "const_assert" {
                let value = eval_expr(&call.args[0], program, env)?;
                let Value::Bool(true) = value else {
                    return Err("const_assert failed".to_string());
                };
                return Ok(Value::Bool(true));
            }
            let Some(func) = program.functions.iter().find(|f| f.name == call.callee) else {
                return Err(format!("unknown const fn '{}'", call.callee));
            };
            if call.args.len() != func.params.len() {
                return Err(format!("const fn '{}' argument count", call.callee));
            }
            let mut local = env.clone();
            for (param, arg) in func.params.iter().zip(call.args.iter()) {
                local.insert(param.name.clone(), eval_expr(arg, program, env)?);
            }
            eval_block_value(&func.body, program, &mut local)
        }
        Expr::Cast(cast) => eval_cast(&cast.expr, &cast.target_type, program, env),
        Expr::Match(match_expr) => eval_match(match_expr, program, env),
        _ => Err("expression is not allowed in a const context".to_string()),
    }
}

fn eval_cast(
    expr: &Expr,
    target: &Type,
    program: &Program,
    env: &HashMap<String, Value>,
) -> Result<Value, String> {
    let value = eval_expr(expr, program, env)?;
    match value {
        Value::Int(n) if crate::integer_limits::integer_row(target).is_some() => {
            let low = crate::integer_limits::integer_low_bits(n, target)
                .ok_or_else(|| "const cast target is not an integer".to_string())?;
            Ok(Value::Int(low))
        }
        Value::Bool(b) if matches!(target, Type::Bool) => Ok(Value::Bool(b)),
        Value::Int(n) if matches!(target, Type::Bool) => Ok(Value::Bool(n != 0)),
        Value::Bool(b) if crate::integer_limits::integer_row(target).is_some() => {
            let low = crate::integer_limits::integer_low_bits(i64::from(b), target)
                .ok_or_else(|| "const cast target is not an integer".to_string())?;
            Ok(Value::Int(low))
        }
        _ => Err("const cast is only defined for integers and bool".to_string()),
    }
}

fn eval_match(
    match_expr: &MatchExpr,
    program: &Program,
    env: &HashMap<String, Value>,
) -> Result<Value, String> {
    let scrutinee = eval_expr(&match_expr.expr, program, env)?;
    for arm in &match_expr.arms {
        if let Some(guard) = &arm.guard {
            if !pattern_matches(&arm.pattern, &scrutinee)? {
                continue;
            }
            let Value::Bool(true) = eval_expr(guard, program, env)? else {
                continue;
            };
        } else if !pattern_matches(&arm.pattern, &scrutinee)? {
            continue;
        }
        let mut local = env.clone();
        bind_pattern(&arm.pattern, &scrutinee, &mut local)?;
        return eval_value_block(&arm.body, program, &mut local);
    }
    Err("const match was not exhaustive".to_string())
}

fn eval_value_block(
    block: &Block,
    program: &Program,
    env: &mut HashMap<String, Value>,
) -> Result<Value, String> {
    if let [Stmt::Expr(expr)] = block.statements.as_slice() {
        return eval_expr(&expr.expr, program, env);
    }
    eval_block_value(block, program, env)
}

fn pattern_matches(pattern: &Pattern, value: &Value) -> Result<bool, String> {
    match pattern {
        Pattern::Wildcard { .. } | Pattern::Binding { .. } => Ok(true),
        Pattern::Lit { lit, .. } => Ok(match (lit, value) {
            (PatLit::Int(n), Value::Int(v)) => n == v,
            (PatLit::Bool(b), Value::Bool(v)) => b == v,
            _ => false,
        }),
        Pattern::Range { lo, hi, .. } => match value {
            Value::Int(n) => Ok(*n >= *lo && *n <= *hi),
            _ => Ok(false),
        },
        Pattern::Or { alts, .. } => {
            for alt in alts {
                if pattern_matches(alt, value)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        _ => Err("const match pattern is not a literal, range, or wildcard".to_string()),
    }
}

fn bind_pattern(
    pattern: &Pattern,
    value: &Value,
    env: &mut HashMap<String, Value>,
) -> Result<(), String> {
    match pattern {
        Pattern::Binding { name, .. } => {
            env.insert(name.clone(), value.clone());
            Ok(())
        }
        Pattern::Or { alts, .. } => {
            for alt in alts {
                bind_pattern(alt, value, env)?;
            }
            Ok(())
        }
        Pattern::Wildcard { .. } | Pattern::Lit { .. } | Pattern::Range { .. } => Ok(()),
        _ => Ok(()),
    }
}

fn eval_binop(
    op: BinOp,
    left: &Expr,
    right: &Expr,
    program: &Program,
    env: &HashMap<String, Value>,
) -> Result<Value, String> {
    let left = eval_expr(left, program, env)?;
    let right = eval_expr(right, program, env)?;
    match (op, left, right) {
        (BinOp::Add, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_add(b))),
        (BinOp::Sub, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_sub(b))),
        (BinOp::Mul, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_mul(b))),
        (BinOp::Div, Value::Int(_), Value::Int(0)) => Err("const division by zero".to_string()),
        (BinOp::Div, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_div(b))),
        (BinOp::Rem, Value::Int(_), Value::Int(0)) => Err("const division by zero".to_string()),
        (BinOp::Rem, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_rem(b))),
        (BinOp::Eq, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
        (BinOp::Ne, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a != b)),
        (BinOp::Lt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
        (BinOp::Gt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
        (BinOp::Le, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
        (BinOp::Ge, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
        (BinOp::Eq, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
        (BinOp::Ne, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a != b)),
        (BinOp::And, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
        (BinOp::Or, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
        (BinOp::BitAnd, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
        (BinOp::BitOr, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
        (BinOp::BitXor, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
        (BinOp::ShiftLeft, Value::Int(a), Value::Int(b)) => {
            Ok(Value::Int(a.wrapping_shl(b as u32)))
        }
        (BinOp::ShiftRight, Value::Int(a), Value::Int(b)) => {
            Ok(Value::Int(a.wrapping_shr(b as u32)))
        }
        _ => Err("const operator does not apply to these values".to_string()),
    }
}

fn eval_block_value(
    block: &Block,
    program: &Program,
    env: &mut HashMap<String, Value>,
) -> Result<Value, String> {
    for stmt in &block.statements {
        match stmt {
            Stmt::Let(let_stmt) => {
                let Some(init) = &let_stmt.init else {
                    return Err("const let requires an initializer".to_string());
                };
                let value = eval_expr(init, program, env)?;
                env.insert(let_stmt.name.clone(), value);
            }
            Stmt::Return(ret) => {
                let Some(value) = &ret.value else {
                    return Err("const fn return requires a value".to_string());
                };
                return eval_expr(value, program, env);
            }
            Stmt::If(if_stmt) => {
                let Value::Bool(cond) = eval_expr(&if_stmt.cond, program, env)? else {
                    return Err("const if condition must be bool".to_string());
                };
                if cond {
                    if block_returns(&if_stmt.then_block) {
                        return eval_block_value(&if_stmt.then_block, program, env);
                    }
                    eval_block_value(&if_stmt.then_block, program, env)?;
                } else if let Some(else_block) = &if_stmt.else_block {
                    if block_returns(else_block) {
                        return eval_block_value(else_block, program, env);
                    }
                    eval_block_value(else_block, program, env)?;
                }
            }
            Stmt::Expr(expr) => {
                eval_expr(&expr.expr, program, env)?;
            }
            _ => return Err("statement is not allowed in a const context".to_string()),
        }
    }
    Err("const fn did not return a value".to_string())
}

fn block_returns(block: &Block) -> bool {
    block.statements.iter().any(|stmt| {
        matches!(stmt, Stmt::Return(_))
            || matches!(stmt, Stmt::If(if_stmt) if block_returns(&if_stmt.then_block))
    })
}

fn resolve_array_lengths(
    program: &mut Program,
    values: &HashMap<String, Value>,
) -> Result<(), String> {
    for func in &mut program.functions {
        let const_params: HashSet<String> = func
            .generics
            .iter()
            .filter(|g| g.const_ty.is_some())
            .map(|g| g.name.clone())
            .collect();
        resolve_type_list(
            func.params.iter_mut().map(|p| &mut p.ty),
            values,
            &const_params,
        )?;
        if let Some(ret) = &mut func.return_type {
            resolve_type(ret, values, &const_params)?;
        }
        resolve_block(&mut func.body, values, &const_params)?;
    }
    for decl in &mut program.structs {
        for field in &mut decl.fields {
            resolve_type(&mut field.ty, values, &HashSet::new())?;
        }
    }
    Ok(())
}

fn resolve_type_list<'a>(
    types: impl Iterator<Item = &'a mut Type>,
    values: &HashMap<String, Value>,
    const_params: &HashSet<String>,
) -> Result<(), String> {
    for ty in types {
        resolve_type(ty, values, const_params)?;
    }
    Ok(())
}

fn resolve_type(
    ty: &mut Type,
    values: &HashMap<String, Value>,
    const_params: &HashSet<String>,
) -> Result<(), String> {
    match ty {
        Type::Array {
            inner,
            size,
            len_name,
        } => {
            if let Some(name) = len_name.clone() {
                if const_params.contains(&name) {
                    resolve_type(inner, values, const_params)?;
                    return Ok(());
                }
                let Some(Value::Int(n)) = values.get(&name) else {
                    return Err(format!("array length '{name}' is not a const int"));
                };
                if *n < 0 {
                    return Err(format!("array length '{name}' is negative"));
                }
                *size = *n as usize;
                *len_name = None;
            }
            resolve_type(inner, values, const_params)
        }
        Type::Ref { inner, .. }
        | Type::RawPtr { inner }
        | Type::Box { inner }
        | Type::Slice { inner } => resolve_type(inner, values, const_params),
        Type::Vec { elem_type }
        | Type::Channel { elem_type }
        | Type::Sender { elem_type }
        | Type::Receiver { elem_type } => resolve_type(elem_type, values, const_params),
        Type::Tuple { elements }
        | Type::Generic {
            params: elements, ..
        } => {
            for elem in elements {
                resolve_type(elem, values, const_params)?;
            }
            Ok(())
        }
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                resolve_type(param, values, const_params)?;
            }
            resolve_type(return_type, values, const_params)
        }
        _ => Ok(()),
    }
}

fn resolve_block(
    block: &mut Block,
    values: &HashMap<String, Value>,
    const_params: &HashSet<String>,
) -> Result<(), String> {
    for stmt in &mut block.statements {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Some(ty) = &mut let_stmt.type_ann {
                    resolve_type(ty, values, const_params)?;
                }
            }
            Stmt::If(if_stmt) => {
                resolve_block(&mut if_stmt.then_block, values, const_params)?;
                if let Some(else_block) = &mut if_stmt.else_block {
                    resolve_block(else_block, values, const_params)?;
                }
            }
            Stmt::While(while_stmt) => resolve_block(&mut while_stmt.body, values, const_params)?,
            Stmt::Loop(loop_stmt) => resolve_block(&mut loop_stmt.body, values, const_params)?,
            Stmt::For(for_stmt) => resolve_block(&mut for_stmt.body, values, const_params)?,
            Stmt::UnsafeBlock(unsafe_stmt) => {
                resolve_block(&mut unsafe_stmt.body, values, const_params)?
            }
            Stmt::Scope(scope_stmt) => resolve_block(&mut scope_stmt.body, values, const_params)?,
            Stmt::Spawn(spawn) => resolve_block(&mut spawn.body, values, const_params)?,
            _ => {}
        }
    }
    Ok(())
}

fn fold_const_uses(program: &mut Program, values: &HashMap<String, Value>) -> Result<(), String> {
    for func in &mut program.functions {
        if func.const_fn {
            continue;
        }
        let const_params: HashSet<String> = func
            .generics
            .iter()
            .filter(|g| g.const_ty.is_some())
            .map(|g| g.name.clone())
            .collect();
        fold_block(&mut func.body, values, &const_params);
    }
    Ok(())
}

fn fold_block(block: &mut Block, values: &HashMap<String, Value>, const_params: &HashSet<String>) {
    for stmt in &mut block.statements {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Some(init) = &mut let_stmt.init {
                    fold_expr(init, values, const_params);
                }
            }
            Stmt::Return(ret) => {
                if let Some(value) = &mut ret.value {
                    fold_expr(value, values, const_params);
                }
            }
            Stmt::Expr(expr) => fold_expr(&mut expr.expr, values, const_params),
            Stmt::If(if_stmt) => {
                fold_expr(&mut if_stmt.cond, values, const_params);
                fold_block(&mut if_stmt.then_block, values, const_params);
                if let Some(else_block) = &mut if_stmt.else_block {
                    fold_block(else_block, values, const_params);
                }
            }
            Stmt::While(while_stmt) => {
                fold_expr(&mut while_stmt.cond, values, const_params);
                fold_block(&mut while_stmt.body, values, const_params);
            }
            Stmt::For(for_stmt) => {
                fold_expr(&mut for_stmt.iterable, values, const_params);
                fold_block(&mut for_stmt.body, values, const_params);
            }
            Stmt::Loop(loop_stmt) => fold_block(&mut loop_stmt.body, values, const_params),
            Stmt::UnsafeBlock(unsafe_stmt) => {
                fold_block(&mut unsafe_stmt.body, values, const_params)
            }
            Stmt::Scope(scope_stmt) => fold_block(&mut scope_stmt.body, values, const_params),
            Stmt::Defer(defer_stmt) => fold_expr(&mut defer_stmt.expr, values, const_params),
            _ => {}
        }
    }
}

fn fold_expr(expr: &mut Expr, values: &HashMap<String, Value>, const_params: &HashSet<String>) {
    if let Expr::Var(var) = expr {
        if const_params.contains(&var.name) {
            return;
        }
        if let Some(value) = values.get(&var.name) {
            *expr = value_to_expr(value, var.id, var.span);
            return;
        }
    }
    match expr {
        Expr::BinOp(op) => {
            fold_expr(&mut op.left, values, const_params);
            fold_expr(&mut op.right, values, const_params);
        }
        Expr::UnOp(op) => fold_expr(&mut op.operand, values, const_params),
        Expr::Call(call) => {
            for arg in &mut call.args {
                fold_expr(arg, values, const_params);
            }
        }
        Expr::Ref(inner) => fold_expr(&mut inner.inner, values, const_params),
        Expr::Index(index) => {
            fold_expr(&mut index.target, values, const_params);
            fold_expr(&mut index.index, values, const_params);
        }
        Expr::FieldAccess(access) => fold_expr(&mut access.base, values, const_params),
        Expr::Assign(assign) => {
            fold_expr(&mut assign.target, values, const_params);
            fold_expr(&mut assign.value, values, const_params);
        }
        _ => {}
    }
}

fn value_to_expr(value: &Value, id: ExprId, span: Span) -> Expr {
    match value {
        Value::Int(n) => Expr::Lit(LitExpr {
            id,
            value: *n,
            span,
        }),
        Value::Bool(b) => Expr::BoolLiteral(BoolLiteralExpr {
            id,
            value: *b,
            span,
        }),
    }
}

fn eval_const_asserts(
    program: &mut Program,
    values: &HashMap<String, Value>,
) -> Result<(), String> {
    let snapshot = program.clone();
    for func in &mut program.functions {
        eval_asserts_in_block(&mut func.body, &snapshot, values)?;
    }
    Ok(())
}

fn eval_asserts_in_block(
    block: &mut Block,
    program: &Program,
    values: &HashMap<String, Value>,
) -> Result<(), String> {
    let mut kept = Vec::new();
    for stmt in block.statements.drain(..) {
        match stmt {
            Stmt::Expr(expr_stmt) if is_const_assert(&expr_stmt.expr) => {
                eval_expr(&expr_stmt.expr, program, values)?;
            }
            Stmt::If(mut if_stmt) => {
                eval_asserts_in_block(&mut if_stmt.then_block, program, values)?;
                if let Some(else_block) = &mut if_stmt.else_block {
                    eval_asserts_in_block(else_block, program, values)?;
                }
                kept.push(Stmt::If(if_stmt));
            }
            other => kept.push(other),
        }
    }
    block.statements = kept;
    Ok(())
}

fn is_const_assert(expr: &Expr) -> bool {
    matches!(expr, Expr::Call(call) if call.callee == "const_assert")
}

fn collect_const_calls(
    block: &Block,
    templates: &[FnDecl],
    type_info: &TypeInfo,
    additions: &mut Vec<FnDecl>,
    record: &mut impl FnMut(ExprId, String),
) -> Result<(), String> {
    for stmt in &block.statements {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Some(init) = &let_stmt.init {
                    collect_const_calls_expr(init, templates, type_info, additions, record)?;
                }
            }
            Stmt::Return(ret) => {
                if let Some(value) = &ret.value {
                    collect_const_calls_expr(value, templates, type_info, additions, record)?;
                }
            }
            Stmt::Expr(expr) => {
                collect_const_calls_expr(&expr.expr, templates, type_info, additions, record)?
            }
            Stmt::If(if_stmt) => {
                collect_const_calls_expr(&if_stmt.cond, templates, type_info, additions, record)?;
                collect_const_calls(&if_stmt.then_block, templates, type_info, additions, record)?;
                if let Some(else_block) = &if_stmt.else_block {
                    collect_const_calls(else_block, templates, type_info, additions, record)?;
                }
            }
            Stmt::While(while_stmt) => {
                collect_const_calls_expr(
                    &while_stmt.cond,
                    templates,
                    type_info,
                    additions,
                    record,
                )?;
                collect_const_calls(&while_stmt.body, templates, type_info, additions, record)?;
            }
            Stmt::For(for_stmt) => {
                collect_const_calls_expr(
                    &for_stmt.iterable,
                    templates,
                    type_info,
                    additions,
                    record,
                )?;
                collect_const_calls(&for_stmt.body, templates, type_info, additions, record)?;
            }
            Stmt::Loop(loop_stmt) => {
                collect_const_calls(&loop_stmt.body, templates, type_info, additions, record)?
            }
            Stmt::UnsafeBlock(unsafe_stmt) => {
                collect_const_calls(&unsafe_stmt.body, templates, type_info, additions, record)?
            }
            Stmt::Scope(scope_stmt) => {
                collect_const_calls(&scope_stmt.body, templates, type_info, additions, record)?
            }
            _ => {}
        }
    }
    Ok(())
}

fn collect_const_calls_expr(
    expr: &Expr,
    templates: &[FnDecl],
    type_info: &TypeInfo,
    additions: &mut Vec<FnDecl>,
    record: &mut impl FnMut(ExprId, String),
) -> Result<(), String> {
    if let Expr::Call(call) = expr {
        if let Some(template) = templates.iter().find(|f| f.name == call.callee) {
            let mut bindings = HashMap::new();
            let const_names: HashSet<String> = template
                .generics
                .iter()
                .filter(|g| g.const_ty.is_some())
                .map(|g| g.name.clone())
                .collect();
            for (arg, param) in call.args.iter().zip(template.params.iter()) {
                let Some(arg_ty) = type_info.expr_types.get(&arg.id()) else {
                    return Err(format!(
                        "const parameter call '{}' is missing an argument type",
                        call.callee
                    ));
                };
                bind_const_lens(&param.ty, arg_ty, &const_names, &mut bindings)?;
            }
            for name in &const_names {
                if !bindings.contains_key(name) {
                    return Err(format!("const parameter '{name}' was not inferred"));
                }
            }
            let mut concrete = template.clone();
            let suffix: Vec<String> = const_names
                .iter()
                .filter_map(|name| bindings.get(name).map(|n| n.to_string()))
                .collect();
            concrete.name = format!("{}_{}", template.name, suffix.join("_"));
            concrete.generics.retain(|g| g.const_ty.is_none());
            concrete.const_fn = false;
            substitute_function(&mut concrete, &bindings);
            record(call.id, concrete.name.clone());
            if !additions.iter().any(|f| f.name == concrete.name) {
                additions.push(concrete);
            }
        }
        for arg in &call.args {
            collect_const_calls_expr(arg, templates, type_info, additions, record)?;
        }
    }
    Ok(())
}

fn bind_const_lens(
    param: &Type,
    arg: &Type,
    names: &HashSet<String>,
    out: &mut HashMap<String, i64>,
) -> Result<(), String> {
    match (param, arg) {
        (
            Type::Ref {
                inner: param_inner, ..
            },
            Type::Ref {
                inner: arg_inner, ..
            },
        ) => bind_const_lens(param_inner, arg_inner, names, out),
        (
            Type::Array {
                len_name: Some(name),
                inner: param_inner,
                ..
            },
            Type::Array {
                size,
                len_name: None,
                inner: arg_inner,
                ..
            },
        ) if names.contains(name) => {
            let value = *size as i64;
            if let Some(prev) = out.get(name) {
                if *prev != value {
                    return Err(format!("const parameter '{name}' has conflicting values"));
                }
            } else {
                out.insert(name.clone(), value);
            }
            bind_const_lens(param_inner, arg_inner, names, out)
        }
        _ => Ok(()),
    }
}

fn substitute_function(func: &mut FnDecl, bindings: &HashMap<String, i64>) {
    for param in &mut func.params {
        substitute_type_len(&mut param.ty, bindings);
    }
    if let Some(ret) = &mut func.return_type {
        substitute_type_len(ret, bindings);
    }
    substitute_block(&mut func.body, bindings);
}

fn substitute_type_len(ty: &mut Type, bindings: &HashMap<String, i64>) {
    match ty {
        Type::Array {
            inner,
            size,
            len_name,
        } => {
            if let Some(name) = len_name.clone()
                && let Some(value) = bindings.get(&name)
            {
                *size = *value as usize;
                *len_name = None;
            }
            substitute_type_len(inner, bindings);
        }
        Type::Ref { inner, .. }
        | Type::RawPtr { inner }
        | Type::Box { inner }
        | Type::Slice { inner } => substitute_type_len(inner, bindings),
        Type::Vec { elem_type }
        | Type::Channel { elem_type }
        | Type::Sender { elem_type }
        | Type::Receiver { elem_type } => substitute_type_len(elem_type, bindings),
        Type::Tuple { elements } => {
            for elem in elements {
                substitute_type_len(elem, bindings);
            }
        }
        Type::Generic { params, .. } => {
            for param in params {
                substitute_type_len(param, bindings);
            }
        }
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                substitute_type_len(param, bindings);
            }
            substitute_type_len(return_type, bindings);
        }
        _ => {}
    }
}

fn substitute_block(block: &mut Block, bindings: &HashMap<String, i64>) {
    for stmt in &mut block.statements {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Some(ty) = &mut let_stmt.type_ann {
                    substitute_type_len(ty, bindings);
                }
                if let Some(init) = &mut let_stmt.init {
                    substitute_expr(init, bindings);
                }
            }
            Stmt::Return(ret) => {
                if let Some(value) = &mut ret.value {
                    substitute_expr(value, bindings);
                }
            }
            Stmt::Expr(expr) => substitute_expr(&mut expr.expr, bindings),
            Stmt::If(if_stmt) => {
                substitute_expr(&mut if_stmt.cond, bindings);
                substitute_block(&mut if_stmt.then_block, bindings);
                if let Some(else_block) = &mut if_stmt.else_block {
                    substitute_block(else_block, bindings);
                }
            }
            _ => {}
        }
    }
}

fn substitute_expr(expr: &mut Expr, bindings: &HashMap<String, i64>) {
    let replaced = if let Expr::Var(var) = &*expr {
        bindings
            .get(&var.name)
            .map(|value| (var.id, *value, var.span))
    } else {
        None
    };
    if let Some((id, value, span)) = replaced {
        *expr = Expr::Lit(LitExpr { id, value, span });
        return;
    }
    match expr {
        Expr::BinOp(op) => {
            substitute_expr(&mut op.left, bindings);
            substitute_expr(&mut op.right, bindings);
        }
        Expr::UnOp(op) => substitute_expr(&mut op.operand, bindings),
        Expr::Call(call) => {
            for arg in &mut call.args {
                substitute_expr(arg, bindings);
            }
        }
        Expr::Ref(inner) => substitute_expr(&mut inner.inner, bindings),
        _ => {}
    }
}
