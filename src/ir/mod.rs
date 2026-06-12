use crate::ast::*;
use crate::tc::collect_captured_vars;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct IRProgram {
    pub structs: Vec<StructDecl>,
    pub enums: Vec<EnumDecl>,
    pub functions: Vec<IRFunction>,
    pub extern_blocks: Vec<ExternBlock>,
    pub type_aliases: Vec<TypeAliasDecl>,
}

#[derive(Debug, Clone)]
pub struct IRFunction {
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<IRParam>,
    pub return_type: Option<Type>,
    pub blocks: Vec<IRBlock>,
    /// Collected defers for this function, in source order.
    /// Codegen will emit them in reverse (LIFO) order at the epilogue.
    pub defers: Vec<IREexpr>,
}

#[derive(Debug, Clone)]
pub struct IRParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct IRBlock {
    pub name: String,
    pub statements: Vec<IRStmt>,
}

#[derive(Debug, Clone)]
pub enum IRStmt {
    Let(IRLetStmt),
    Return(IRReturn),
    Expr(IREexpr),
    Defer(IREexpr),
    Spawn(IRSpawn),
    If(IRIf),
    While(IRWhile),
    UnsafeBlock(IRUnsafeBlock),
}

#[derive(Debug, Clone)]
pub struct IRWhile {
    pub cond: IREexpr,
    pub body: IRBlock,
}

#[derive(Debug, Clone)]
pub struct IRLetStmt {
    pub name: String,
    pub ty: Type,
    pub init: Option<IREexpr>,
}

#[derive(Debug, Clone)]
pub struct IRReturn {
    pub value: Option<IREexpr>,
}

#[derive(Debug, Clone)]
pub struct IRSpawn {
    pub captures: Vec<(String, Type)>,
    pub body: IRBlock,
    /// Defers collected from the spawn body (run on thread exit).
    pub defers: Vec<IREexpr>,
}

#[derive(Debug, Clone)]
pub struct IRIf {
    pub cond: IREexpr,
    pub then_block: IRBlock,
    pub else_block: Option<IRBlock>,
}

#[derive(Debug, Clone)]
pub struct IRUnsafeBlock {
    pub body: IRBlock,
}

#[derive(Debug, Clone)]
pub enum IREexpr {
    Lit(i64),
    BoolLiteral(bool),
    FloatLiteral(f64),
    Var(String),
    AddressOf {
        inner: Box<IREexpr>,
        mutable: bool,
    },
    BinOp {
        op: BinOp,
        left: Box<IREexpr>,
        right: Box<IREexpr>,
    },
    UnOp {
        op: UnOp,
        operand: Box<IREexpr>,
    },
    Send {
        channel: Box<IREexpr>,
        value: Box<IREexpr>,
        value_type: Type,
    },
    Recv {
        channel: Box<IREexpr>,
        elem_type: Type,
    },
    StructLit {
        type_name: String,
        fields: Vec<IRStructLitField>,
    },
    FieldAccess {
        base: Box<IREexpr>,
        field: String,
        is_pointer: bool,
    },
    EnumLit {
        enum_name: String,
        variant: String,
        args: Vec<IREexpr>,                           // For tuple variants
        named_fields: Option<Vec<(String, IREexpr)>>, // For struct variants: { field: expr }
    },
    Match {
        expr: Box<IREexpr>,
        enum_type: String,
        arms: Vec<IRMatchArm>,
    },
    Call {
        callee: String,
        args: Vec<IREexpr>,
        return_type: Option<Type>,
        tuple_destructure_index: Option<usize>, // For tuple destructuring: Some(0) = first element, Some(1) = second, etc.
    },
    StringLit(String),
    ArrayLiteral {
        elements: Vec<IREexpr>,
        repeat: Option<(Box<IREexpr>, usize)>, // For [value; count] syntax: (value, count)
    },
    Index {
        target: Box<IREexpr>,
        index: Box<IREexpr>,
        target_type: Option<Type>, // Type of target for bounds checking
    },
    Assign {
        target: String,
        value: Box<IREexpr>,
    },
    AssignIndex {
        target: Box<IREexpr>,
        index: Box<IREexpr>,
        value: Box<IREexpr>,
    },
    Cast {
        expr: Box<IREexpr>,
        target_type: Type,
    },
}

#[derive(Debug, Clone)]
pub struct IRMatchArm {
    pub pattern: IRPattern,
    pub guard: Option<IREexpr>,
    pub body: IRBlock,
}

struct LoweringContext {
    var_types: HashMap<String, Type>,
}

impl LoweringContext {
    fn from_params(params: &[IRParam]) -> Self {
        let mut var_types = HashMap::new();
        for p in params {
            var_types.insert(p.name.clone(), p.ty.clone());
        }
        Self { var_types }
    }

    fn resolve_expr_type(&self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Var(v) => self.var_types.get(&v.name).cloned(),
            Expr::Ref(r) => self.resolve_expr_type(&r.inner),
            _ => Some(infer_type_from_expr(expr)),
        }
    }

    fn record_binding(&mut self, name: &str, ty: &Type) {
        self.var_types.insert(name.to_string(), ty.clone());
    }
}

fn pattern_to_ir(pattern: &Pattern) -> IRPattern {
    match pattern {
        Pattern::Variant {
            enum_name,
            variant,
            sub_patterns,
            named_fields,
            ..
        } => IRPattern::Variant {
            enum_name: enum_name.clone(),
            variant: variant.clone(),
            sub_patterns: sub_patterns.iter().map(pattern_to_ir).collect(),
            named_fields: named_fields.as_ref().map(|fields| {
                fields
                    .iter()
                    .map(|(name, p)| (name.clone(), pattern_to_ir(p)))
                    .collect()
            }),
        },
        Pattern::Wildcard { .. } => IRPattern::Wildcard,
        Pattern::Binding { name, .. } => IRPattern::Binding { name: name.clone() },
    }
}

#[derive(Debug, Clone)]
pub enum IRPattern {
    Variant {
        enum_name: String,
        variant: String,
        sub_patterns: Vec<IRPattern>, // For tuple variants
        named_fields: Option<Vec<(String, IRPattern)>>, // For struct variants: { field: pattern }
    },
    Wildcard,
    Binding {
        name: String,
    },
}

#[derive(Debug, Clone)]
pub struct IRStructLitField {
    pub name: String,
    pub value: IREexpr,
}

pub struct IRBuilder;

impl IRBuilder {
    pub fn build(ast: &Program) -> IRProgram {
        let builder = IRBuilder;
        let functions = ast
            .functions
            .iter()
            .map(|f| builder.build_function(f))
            .collect();
        let mut program = IRProgram {
            structs: ast.structs.clone(),
            enums: ast.enums.clone(),
            functions,
            extern_blocks: ast.extern_blocks.clone(),
            type_aliases: ast.type_aliases.clone(),
        };
        monomorphize_generic_functions(&mut program);
        program
    }

    fn build_function(&self, function: &FnDecl) -> IRFunction {
        let params: Vec<IRParam> = function
            .params
            .iter()
            .map(|p| IRParam {
                name: p.name.clone(),
                ty: p.ty.clone(),
            })
            .collect();

        // For minimal subset, we use a single basic block
        let mut statements = Vec::new();
        let mut defers = Vec::new();
        let mut ctx = LoweringContext::from_params(&params);

        for stmt in &function.body.statements {
            Self::lower_stmt(stmt, &mut statements, &mut defers, &mut ctx);
        }

        // Create a single basic block named "entry"
        let blocks = vec![IRBlock {
            name: "entry".to_string(),
            statements,
        }];

        IRFunction {
            name: function.name.clone(),
            generics: function.generics.clone(),
            params,
            return_type: function.return_type.clone(),
            blocks,
            defers,
        }
    }

    fn lower_stmt(
        stmt: &Stmt,
        out: &mut Vec<IRStmt>,
        defers: &mut Vec<IREexpr>,
        ctx: &mut LoweringContext,
    ) {
        match stmt {
            Stmt::Let(let_stmt) => {
                // Handle tuple destructuring for channel()
                if let Some(ref patterns) = let_stmt.patterns
                    && patterns.len() == 2
                    && let Some(ref init) = let_stmt.init
                    && let Expr::Call(call_expr) = init
                    && call_expr.callee == "channel"
                {
                    // Extract element type from type annotation
                    if let Some(ref type_ann) = let_stmt.type_ann
                        && let Type::Tuple { elements } = type_ann
                        && elements.len() == 2
                        && let Type::Sender { .. } = &elements[0]
                        && let Type::Receiver { .. } = &elements[1]
                    {
                        // Extract variable names from patterns
                        if let Pattern::Binding { name: tx_name, .. } = &patterns[0]
                            && let Pattern::Binding { name: rx_name, .. } = &patterns[1]
                        {
                            // Generate two let statements for sender and receiver
                            // The init will be handled specially in codegen
                            out.push(IRStmt::Let(IRLetStmt {
                                name: tx_name.clone(),
                                ty: elements[0].clone(),
                                init: Some(IREexpr::Call {
                                    callee: "channel".to_string(),
                                    args: call_expr
                                        .args
                                        .iter()
                                        .map(|a| build_expr_with_ctx(a, ctx))
                                        .collect(),
                                    return_type: Some(Type::Tuple {
                                        elements: elements.clone(),
                                    }),
                                    tuple_destructure_index: Some(0), // 0 = first element (sender)
                                }),
                            }));
                            out.push(IRStmt::Let(IRLetStmt {
                                name: rx_name.clone(),
                                ty: elements[1].clone(),
                                init: Some(IREexpr::Call {
                                    callee: "channel".to_string(),
                                    args: call_expr
                                        .args
                                        .iter()
                                        .map(|a| build_expr_with_ctx(a, ctx))
                                        .collect(),
                                    return_type: Some(Type::Tuple {
                                        elements: elements.clone(),
                                    }),
                                    tuple_destructure_index: Some(1), // 1 = second element (receiver)
                                }),
                            }));
                            ctx.record_binding(tx_name, &elements[0]);
                            ctx.record_binding(rx_name, &elements[1]);
                            return;
                        }
                    }
                }

                // If there is an explicit type annotation, use it.
                // Otherwise, try to infer the type from the initializer expression.
                // For variables (Expr::Var), we can't infer from the variable name itself,
                // but if it's a simple assignment like `let x = y;`, the type checker has
                // already verified the types match, so we should preserve the type.
                // However, since we don't have access to the type checker's context here,
                // we'll use a heuristic: if the init is a variable and we can't infer,
                // we'll need to rely on type annotations or let the type checker handle it.
                let ty = if let Some(ref type_ann) = let_stmt.type_ann {
                    type_ann.clone()
                } else if let Some(ref init_expr) = let_stmt.init {
                    // For variables, we can't infer the type, but the type checker has
                    // already verified it. For now, we'll use a fallback that works for
                    // common cases. The real fix would be to pass type information from
                    // the type checker, but for now we'll handle Receiver/Sender specially.
                    match init_expr {
                        Expr::Var(_var_expr) => {
                            // For variables, we can't infer from the name alone.
                            // But if this is a Receiver or Sender, we should preserve that.
                            // Since we don't have type info, we'll default to Int and let
                            // explicit type annotations handle it, OR we could check if
                            // the variable name suggests it's a receiver/sender.
                            // For now, use Int as fallback - type annotations should be used.
                            Type::Int
                        }
                        _ => infer_type_from_expr(init_expr),
                    }
                } else {
                    Type::Int
                };

                ctx.record_binding(&let_stmt.name, &ty);
                out.push(IRStmt::Let(IRLetStmt {
                    name: let_stmt.name.clone(),
                    ty: ty.clone(),
                    init: let_stmt.init.as_ref().map(|e| build_expr_with_ctx(e, ctx)),
                }));
            }
            Stmt::Return(return_stmt) => {
                out.push(IRStmt::Return(IRReturn {
                    value: return_stmt
                        .value
                        .as_ref()
                        .map(|e| build_expr_with_ctx(e, ctx)),
                }));
            }
            Stmt::Expr(expr_stmt) => {
                out.push(IRStmt::Expr(build_expr_with_ctx(&expr_stmt.expr, ctx)));
            }
            Stmt::Defer(defer_stmt) => {
                // Collect defers per function; they will be emitted at the epilogue.
                defers.push(build_expr_with_ctx(&defer_stmt.expr, ctx));
            }
            Stmt::Spawn(spawn_stmt) => {
                let captured_names = collect_captured_vars(&spawn_stmt.body);
                let mut captures = Vec::new();
                for name in captured_names {
                    if let Some(ty) = ctx.var_types.get(&name) {
                        captures.push((name, ty.clone()));
                    }
                }

                let parent_vars = ctx.var_types.clone();
                ctx.var_types.clear();
                for (name, ty) in &captures {
                    ctx.record_binding(name, ty);
                }

                let mut inner_stmts = Vec::new();
                let mut spawn_defers = Vec::new();
                for inner in &spawn_stmt.body.statements {
                    Self::lower_stmt(inner, &mut inner_stmts, &mut spawn_defers, ctx);
                }
                ctx.var_types = parent_vars;

                let inner_block = IRBlock {
                    name: "spawn_body".to_string(),
                    statements: inner_stmts,
                };
                out.push(IRStmt::Spawn(IRSpawn {
                    captures,
                    body: inner_block,
                    defers: spawn_defers,
                }));
            }
            Stmt::If(if_stmt) => {
                // Lower `if` to an IRIf with nested blocks.
                let cond = build_expr_with_ctx(&if_stmt.cond, ctx);

                let mut then_stmts = Vec::new();
                for inner in &if_stmt.then_block.statements {
                    Self::lower_stmt(inner, &mut then_stmts, defers, ctx);
                }
                let then_block = IRBlock {
                    name: "then_block".to_string(),
                    statements: then_stmts,
                };

                let else_block = if let Some(ref else_blk) = if_stmt.else_block {
                    let mut else_stmts = Vec::new();
                    for inner in &else_blk.statements {
                        Self::lower_stmt(inner, &mut else_stmts, defers, ctx);
                    }
                    Some(IRBlock {
                        name: "else_block".to_string(),
                        statements: else_stmts,
                    })
                } else {
                    None
                };

                out.push(IRStmt::If(IRIf {
                    cond,
                    then_block,
                    else_block,
                }));
            }
            Stmt::While(while_stmt) => {
                let cond = build_expr_with_ctx(&while_stmt.cond, ctx);
                let mut body_stmts = Vec::new();
                for inner in &while_stmt.body.statements {
                    Self::lower_stmt(inner, &mut body_stmts, defers, ctx);
                }
                let body = IRBlock {
                    name: "while_body".to_string(),
                    statements: body_stmts,
                };
                out.push(IRStmt::While(IRWhile { cond, body }));
            }
            Stmt::For(for_stmt) => {
                let container_var = format!("__for_container_{}", for_stmt.span.start);
                let index_var = format!("__for_i_{}", for_stmt.span.start);

                let container_ty = ctx
                    .resolve_expr_type(&for_stmt.iterable)
                    .unwrap_or_else(|| infer_type_from_expr(&for_stmt.iterable));

                let elem_type = match &container_ty {
                    Type::Vec { elem_type } => (**elem_type).clone(),
                    Type::String => Type::U8,
                    Type::Array { inner, .. } => (**inner).clone(),
                    _ => Type::Int,
                };

                let use_container_copy = !matches!(container_ty, Type::Array { .. });
                if use_container_copy {
                    ctx.record_binding(&container_var, &container_ty);
                    out.push(IRStmt::Let(IRLetStmt {
                        name: container_var.clone(),
                        ty: container_ty.clone(),
                        init: Some(build_expr_with_ctx(&for_stmt.iterable, ctx)),
                    }));
                }
                ctx.record_binding(&index_var, &Type::Int);
                out.push(IRStmt::Let(IRLetStmt {
                    name: index_var.clone(),
                    ty: Type::Int,
                    init: Some(IREexpr::Lit(0)),
                }));

                let iterable_expr = build_expr_with_ctx(&for_stmt.iterable, ctx);
                let container_ref = if use_container_copy {
                    IREexpr::AddressOf {
                        inner: Box::new(IREexpr::Var(container_var.clone())),
                        mutable: false,
                    }
                } else {
                    IREexpr::AddressOf {
                        inner: Box::new(iterable_expr.clone()),
                        mutable: false,
                    }
                };
                let index_ref = IREexpr::Var(index_var.clone());
                let index_target = if use_container_copy {
                    IREexpr::Var(container_var.clone())
                } else {
                    iterable_expr.clone()
                };

                let cond = match &container_ty {
                    Type::Array { size, .. } => IREexpr::BinOp {
                        op: BinOp::Lt,
                        left: Box::new(index_ref.clone()),
                        right: Box::new(IREexpr::Lit(*size as i64)),
                    },
                    Type::Vec { .. } => IREexpr::BinOp {
                        op: BinOp::Lt,
                        left: Box::new(index_ref.clone()),
                        right: Box::new(IREexpr::Call {
                            callee: "Vec::len".to_string(),
                            args: vec![container_ref.clone()],
                            return_type: Some(Type::Int),
                            tuple_destructure_index: None,
                        }),
                    },
                    Type::String => IREexpr::BinOp {
                        op: BinOp::Lt,
                        left: Box::new(index_ref.clone()),
                        right: Box::new(IREexpr::Call {
                            callee: "String::len".to_string(),
                            args: vec![container_ref.clone()],
                            return_type: Some(Type::Int),
                            tuple_destructure_index: None,
                        }),
                    },
                    _ => IREexpr::BoolLiteral(false),
                };

                let mut while_body_stmts = Vec::new();

                match &container_ty {
                    Type::Vec { .. } => {
                        let opt_var = format!("__for_opt_{}", for_stmt.span.start);
                        let get_call = IREexpr::Call {
                            callee: "Vec::get".to_string(),
                            args: vec![container_ref, index_ref.clone()],
                            return_type: Some(Type::Generic {
                                name: "Option".to_string(),
                                params: vec![elem_type.clone()],
                            }),
                            tuple_destructure_index: None,
                        };
                        while_body_stmts.push(IRStmt::Let(IRLetStmt {
                            name: opt_var.clone(),
                            ty: Type::Generic {
                                name: "Option".to_string(),
                                params: vec![elem_type.clone()],
                            },
                            init: Some(get_call),
                        }));
                        let mut match_body_stmts = Vec::new();
                        ctx.record_binding(&for_stmt.var_name, &elem_type);
                        for inner in &for_stmt.body.statements {
                            Self::lower_stmt(inner, &mut match_body_stmts, defers, ctx);
                        }
                        match_body_stmts.push(IRStmt::Expr(IREexpr::Assign {
                            target: index_var.clone(),
                            value: Box::new(IREexpr::BinOp {
                                op: BinOp::Add,
                                left: Box::new(IREexpr::Var(index_var.clone())),
                                right: Box::new(IREexpr::Lit(1)),
                            }),
                        }));
                        while_body_stmts.push(IRStmt::Expr(IREexpr::Match {
                            expr: Box::new(IREexpr::Var(opt_var)),
                            enum_type: "Option".to_string(),
                            arms: vec![
                                IRMatchArm {
                                    pattern: IRPattern::Variant {
                                        enum_name: "Option".to_string(),
                                        variant: "Some".to_string(),
                                        sub_patterns: vec![IRPattern::Binding {
                                            name: for_stmt.var_name.clone(),
                                        }],
                                        named_fields: None,
                                    },
                                    guard: None,
                                    body: IRBlock {
                                        name: "for_match_body".to_string(),
                                        statements: match_body_stmts,
                                    },
                                },
                                IRMatchArm {
                                    pattern: IRPattern::Variant {
                                        enum_name: "Option".to_string(),
                                        variant: "None".to_string(),
                                        sub_patterns: Vec::new(),
                                        named_fields: None,
                                    },
                                    guard: None,
                                    body: IRBlock {
                                        name: "for_none_body".to_string(),
                                        statements: Vec::new(),
                                    },
                                },
                            ],
                        }));
                    }
                    Type::Array { .. } | Type::String => {
                        let target_type = Some(container_ty.clone());
                        while_body_stmts.push(IRStmt::Let(IRLetStmt {
                            name: for_stmt.var_name.clone(),
                            ty: elem_type.clone(),
                            init: Some(IREexpr::Index {
                                target: Box::new(index_target),
                                index: Box::new(index_ref.clone()),
                                target_type,
                            }),
                        }));
                        ctx.record_binding(&for_stmt.var_name, &elem_type);
                        for inner in &for_stmt.body.statements {
                            Self::lower_stmt(inner, &mut while_body_stmts, defers, ctx);
                        }
                        while_body_stmts.push(IRStmt::Expr(IREexpr::Assign {
                            target: index_var.clone(),
                            value: Box::new(IREexpr::BinOp {
                                op: BinOp::Add,
                                left: Box::new(IREexpr::Var(index_var)),
                                right: Box::new(IREexpr::Lit(1)),
                            }),
                        }));
                    }
                    _ => {}
                }

                out.push(IRStmt::While(IRWhile {
                    cond,
                    body: IRBlock {
                        name: "for_while_body".to_string(),
                        statements: while_body_stmts,
                    },
                }));
            }
            Stmt::UnsafeBlock(unsafe_stmt) => {
                let mut body_stmts = Vec::new();
                for inner in &unsafe_stmt.body.statements {
                    Self::lower_stmt(inner, &mut body_stmts, defers, ctx);
                }
                let body = IRBlock {
                    name: "unsafe_body".to_string(),
                    statements: body_stmts,
                };
                out.push(IRStmt::UnsafeBlock(IRUnsafeBlock { body }));
            }
        }
    }
}

fn infer_send_value_type(expr: &Expr) -> Type {
    match expr {
        Expr::Lit(_) => Type::Int,
        Expr::BoolLiteral(_) => Type::Bool,
        Expr::FloatLiteral(_) => Type::F64,
        _ => Type::Int,
    }
}

fn build_expr_with_ctx(expr: &Expr, ctx: &LoweringContext) -> IREexpr {
    match expr {
        Expr::Lit(lit_expr) => IREexpr::Lit(lit_expr.value),
        Expr::BoolLiteral(bool_expr) => IREexpr::BoolLiteral(bool_expr.value),
        Expr::FloatLiteral(float_expr) => IREexpr::FloatLiteral(float_expr.value),
        Expr::Var(var_expr) => IREexpr::Var(var_expr.name.clone()),
        Expr::Ref(ref_expr) => {
            // Reference expression: &x or &mut x
            IREexpr::AddressOf {
                inner: Box::new(build_expr_with_ctx(&ref_expr.inner, ctx)),
                mutable: ref_expr.mutable,
            }
        }
        Expr::BinOp(bin_op_expr) => IREexpr::BinOp {
            op: bin_op_expr.op,
            left: Box::new(build_expr_with_ctx(&bin_op_expr.left, ctx)),
            right: Box::new(build_expr_with_ctx(&bin_op_expr.right, ctx)),
        },
        Expr::UnOp(un_op_expr) => IREexpr::UnOp {
            op: un_op_expr.op,
            operand: Box::new(build_expr_with_ctx(&un_op_expr.operand, ctx)),
        },
        Expr::Send(send_expr) => {
            let value_type = ctx
                .resolve_expr_type(&send_expr.value)
                .unwrap_or_else(|| infer_send_value_type(&send_expr.value));
            IREexpr::Send {
                channel: Box::new(build_expr_with_ctx(&send_expr.channel, ctx)),
                value: Box::new(build_expr_with_ctx(&send_expr.value, ctx)),
                value_type,
            }
        }
        Expr::Recv(recv_expr) => IREexpr::Recv {
            channel: Box::new(build_expr_with_ctx(&recv_expr.channel, ctx)),
            elem_type: Type::Int,
        },
        Expr::StructLit(lit) => IREexpr::StructLit {
            type_name: lit.type_name.clone(),
            fields: lit
                .fields
                .iter()
                .map(|f| IRStructLitField {
                    name: f.name.clone(),
                    value: build_expr_with_ctx(&f.value, ctx),
                })
                .collect(),
        },
        Expr::FieldAccess(acc) => IREexpr::FieldAccess {
            base: Box::new(build_expr_with_ctx(&acc.base, ctx)),
            field: acc.field.clone(),
            is_pointer: false,
        },
        Expr::EnumLit(enum_lit) => IREexpr::EnumLit {
            enum_name: enum_lit.enum_name.clone(),
            variant: enum_lit.variant.clone(),
            args: enum_lit
                .args
                .iter()
                .map(|a| build_expr_with_ctx(a, ctx))
                .collect(),
            named_fields: enum_lit.named_fields.as_ref().map(|fields| {
                fields
                    .iter()
                    .map(|(field_name, field_expr)| {
                        (field_name.clone(), build_expr_with_ctx(field_expr, ctx))
                    })
                    .collect()
            }),
        },
        Expr::Match(match_expr) => {
            let arms = match_expr
                .arms
                .iter()
                .map(|arm| {
                    let mut body_stmts = Vec::new();
                    let mut arm_defers = Vec::new();
                    let mut arm_ctx = LoweringContext {
                        var_types: ctx.var_types.clone(),
                    };
                    for stmt in &arm.body.statements {
                        IRBuilder::lower_stmt(stmt, &mut body_stmts, &mut arm_defers, &mut arm_ctx);
                    }
                    IRMatchArm {
                        pattern: pattern_to_ir(&arm.pattern),
                        guard: arm.guard.as_ref().map(|g| build_expr_with_ctx(g, ctx)),
                        body: IRBlock {
                            name: "match_arm".to_string(),
                            statements: body_stmts,
                        },
                    }
                })
                .collect();
            let enum_name = match_expr
                .arms
                .iter()
                .find_map(|arm| match &arm.pattern {
                    Pattern::Variant { enum_name, .. } => Some(enum_name.clone()),
                    _ => None,
                })
                .unwrap_or_else(|| "Unknown".to_string());

            IREexpr::Match {
                expr: Box::new(build_expr_with_ctx(&match_expr.expr, ctx)),
                enum_type: enum_name,
                arms,
            }
        }
        Expr::Call(call_expr) => {
            let return_type = infer_type_from_call(&call_expr.callee, &call_expr.args);
            IREexpr::Call {
                callee: call_expr.callee.clone(),
                args: call_expr
                    .args
                    .iter()
                    .map(|a| build_expr_with_ctx(a, ctx))
                    .collect(),
                return_type,
                tuple_destructure_index: None,
            }
        }
        Expr::MethodCall(method_call) => {
            let receiver_expr = build_expr_with_ctx(&method_call.receiver, ctx);
            let method_args: Vec<IREexpr> = method_call
                .args
                .iter()
                .map(|a| build_expr_with_ctx(a, ctx))
                .collect();
            let mut all_args = vec![receiver_expr];
            all_args.extend(method_args);
            IREexpr::Call {
                callee: format!("METHOD::{}", method_call.method),
                args: all_args,
                return_type: None,
                tuple_destructure_index: None,
            }
        }
        Expr::StringLit(string_lit) => IREexpr::StringLit(string_lit.value.clone()),
        Expr::ArrayLiteral(arr_lit) => IREexpr::ArrayLiteral {
            elements: arr_lit
                .elements
                .iter()
                .map(|e| build_expr_with_ctx(e, ctx))
                .collect(),
            repeat: arr_lit
                .repeat
                .as_ref()
                .map(|(expr, count)| (Box::new(build_expr_with_ctx(expr, ctx)), *count)),
        },
        Expr::Index(index_expr) => {
            let target_type = ctx.resolve_expr_type(&index_expr.target);
            IREexpr::Index {
                target: Box::new(build_expr_with_ctx(&index_expr.target, ctx)),
                index: Box::new(build_expr_with_ctx(&index_expr.index, ctx)),
                target_type,
            }
        }
        Expr::Cast(cast_expr) => IREexpr::Cast {
            expr: Box::new(build_expr_with_ctx(&cast_expr.expr, ctx)),
            target_type: cast_expr.target_type.clone(),
        },
        Expr::Assign(assign_expr) => match &*assign_expr.target {
            Expr::Var(var_expr) => IREexpr::Assign {
                target: var_expr.name.clone(),
                value: Box::new(build_expr_with_ctx(&assign_expr.value, ctx)),
            },
            Expr::Index(index_expr) => IREexpr::AssignIndex {
                target: Box::new(build_expr_with_ctx(&index_expr.target, ctx)),
                index: Box::new(build_expr_with_ctx(&index_expr.index, ctx)),
                value: Box::new(build_expr_with_ctx(&assign_expr.value, ctx)),
            },
            _ => panic!("Invalid assignment target in IR lowering"),
        },
    }
}

/// Infer the type of an expression for type inference in let statements.
fn infer_type_from_expr(expr: &Expr) -> Type {
    match expr {
        Expr::Lit(_) => Type::Int,
        Expr::BoolLiteral(_) => Type::Bool,
        Expr::FloatLiteral(_) => Type::F64, // Float literals default to f64
        Expr::Var(_) => Type::Int, // Can't infer from variable without type info - will be fixed by type checker
        Expr::Call(call_expr) => {
            infer_type_from_call(&call_expr.callee, &call_expr.args).unwrap_or(Type::Int)
        }
        Expr::StringLit(_) => Type::String,
        Expr::Ref(ref_expr) => Type::Ref {
            inner: Box::new(infer_type_from_expr(&ref_expr.inner)),
            mutable: ref_expr.mutable,
        },
        _ => Type::Int, // Default fallback
    }
}

/// Infer the return type of a function call.
fn infer_type_from_call(callee: &str, args: &[Expr]) -> Option<Type> {
    // Handle built-in functions
    if callee == "Box::new" && !args.is_empty() {
        // Box::new<T>(value: T) -> Box<T>
        let arg_type = infer_type_from_expr(&args[0]);
        return Some(Type::Box {
            inner: Box::new(arg_type),
        });
    }
    if callee == "Vec::new" {
        // Vec::new<T>() -> Vec<T>
        // We can't infer T from no arguments, so return None
        return None;
    }
    if callee == "Vec::with_capacity" {
        // Vec::with_capacity<T>(cap: int) -> Vec<T>
        // We can't infer T from capacity, so return None
        return None;
    }
    if callee == "String::new" {
        return Some(Type::String);
    }
    if callee == "String::from" && !args.is_empty() {
        // String::from(str: &str) -> String
        return Some(Type::String);
    }
    // For other calls, we can't infer without type information
    None
}

/// Monomorphize generic functions at each call site and drop unresolved templates.
fn monomorphize_generic_functions(program: &mut IRProgram) {
    let generic_defs: HashMap<String, IRFunction> = program
        .functions
        .iter()
        .filter(|f| !f.generics.is_empty())
        .map(|f| (f.name.clone(), f.clone()))
        .collect();

    if generic_defs.is_empty() {
        return;
    }

    let mut instantiations: HashMap<String, (IRFunction, HashMap<String, Type>)> = HashMap::new();

    for func in &mut program.functions {
        let mut var_types: HashMap<String, Type> = HashMap::new();
        for param in &func.params {
            var_types.insert(param.name.clone(), param.ty.clone());
        }
        for block in &mut func.blocks {
            rewrite_generic_calls_in_block(
                block,
                &generic_defs,
                &mut var_types,
                &mut instantiations,
            );
        }
        for defer_expr in &mut func.defers {
            rewrite_generic_calls_in_expr(
                defer_expr,
                &generic_defs,
                &var_types,
                &mut instantiations,
            );
        }
    }

    let monomorphized: Vec<IRFunction> = instantiations
        .into_values()
        .map(|(template, subs)| instantiate_generic_function(&template, &subs))
        .collect();

    let mut functions: Vec<IRFunction> = program
        .functions
        .iter()
        .filter(|f| f.generics.is_empty())
        .cloned()
        .collect();
    functions.extend(monomorphized);
    program.functions = functions;
}

fn rewrite_generic_calls_in_block(
    block: &mut IRBlock,
    generic_defs: &HashMap<String, IRFunction>,
    var_types: &mut HashMap<String, Type>,
    instantiations: &mut HashMap<String, (IRFunction, HashMap<String, Type>)>,
) {
    for stmt in &mut block.statements {
        match stmt {
            IRStmt::Let(let_stmt) => {
                if let Some(ref mut init) = let_stmt.init {
                    rewrite_generic_calls_in_expr(init, generic_defs, var_types, instantiations);
                }
                var_types.insert(let_stmt.name.clone(), let_stmt.ty.clone());
            }
            IRStmt::Return(ret) => {
                if let Some(ref mut value) = ret.value {
                    rewrite_generic_calls_in_expr(value, generic_defs, var_types, instantiations);
                }
            }
            IRStmt::Expr(expr) => {
                rewrite_generic_calls_in_expr(expr, generic_defs, var_types, instantiations);
            }
            IRStmt::If(ir_if) => {
                rewrite_generic_calls_in_expr(
                    &mut ir_if.cond,
                    generic_defs,
                    var_types,
                    instantiations,
                );
                rewrite_generic_calls_in_block(
                    &mut ir_if.then_block,
                    generic_defs,
                    var_types,
                    instantiations,
                );
                if let Some(ref mut else_block) = ir_if.else_block {
                    rewrite_generic_calls_in_block(
                        else_block,
                        generic_defs,
                        var_types,
                        instantiations,
                    );
                }
            }
            IRStmt::While(ir_while) => {
                rewrite_generic_calls_in_expr(
                    &mut ir_while.cond,
                    generic_defs,
                    var_types,
                    instantiations,
                );
                rewrite_generic_calls_in_block(
                    &mut ir_while.body,
                    generic_defs,
                    var_types,
                    instantiations,
                );
            }
            IRStmt::UnsafeBlock(unsafe_block) => {
                rewrite_generic_calls_in_block(
                    &mut unsafe_block.body,
                    generic_defs,
                    var_types,
                    instantiations,
                );
            }
            IRStmt::Defer(expr) => {
                rewrite_generic_calls_in_expr(expr, generic_defs, var_types, instantiations);
            }
            IRStmt::Spawn(spawn) => {
                let mut spawn_vars = var_types.clone();
                for (name, ty) in &spawn.captures {
                    spawn_vars.insert(name.clone(), ty.clone());
                }
                rewrite_generic_calls_in_block(
                    &mut spawn.body,
                    generic_defs,
                    &mut spawn_vars,
                    instantiations,
                );
                for defer_expr in &mut spawn.defers {
                    rewrite_generic_calls_in_expr(
                        defer_expr,
                        generic_defs,
                        &spawn_vars,
                        instantiations,
                    );
                }
            }
        }
    }
}

fn rewrite_generic_calls_in_expr(
    expr: &mut IREexpr,
    generic_defs: &HashMap<String, IRFunction>,
    var_types: &HashMap<String, Type>,
    instantiations: &mut HashMap<String, (IRFunction, HashMap<String, Type>)>,
) {
    match expr {
        IREexpr::Call {
            callee,
            args,
            return_type,
            ..
        } => {
            for arg in args.iter_mut() {
                rewrite_generic_calls_in_expr(arg, generic_defs, var_types, instantiations);
            }
            if let Some(template) = generic_defs.get(callee.as_str())
                && !args.is_empty()
                && !template.params.is_empty()
                && let (Some(arg_ty), Some(param_ty)) = (
                    infer_ir_expr_type(&args[0], var_types),
                    template.params.first().map(|p| p.ty.clone()),
                )
            {
                let subs = infer_generic_substitutions(&param_ty, &arg_ty, &template.generics);
                if subs.len() == template.generics.len() {
                    let mangled = mangle_function_name(callee, &template.generics, &subs);
                    instantiations
                        .entry(mangled.clone())
                        .or_insert_with(|| (template.clone(), subs.clone()));
                    *callee = mangled;
                    if let Some(ret) = return_type {
                        *ret = substitute_type(ret, &subs);
                    }
                }
            }
        }
        IREexpr::AddressOf { inner, .. } => {
            rewrite_generic_calls_in_expr(inner, generic_defs, var_types, instantiations);
        }
        IREexpr::BinOp { left, right, .. } => {
            rewrite_generic_calls_in_expr(left, generic_defs, var_types, instantiations);
            rewrite_generic_calls_in_expr(right, generic_defs, var_types, instantiations);
        }
        IREexpr::UnOp { operand, .. } => {
            rewrite_generic_calls_in_expr(operand, generic_defs, var_types, instantiations);
        }
        IREexpr::Send { channel, value, .. } => {
            rewrite_generic_calls_in_expr(channel, generic_defs, var_types, instantiations);
            rewrite_generic_calls_in_expr(value, generic_defs, var_types, instantiations);
        }
        IREexpr::Recv { channel, .. } => {
            rewrite_generic_calls_in_expr(channel, generic_defs, var_types, instantiations);
        }
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                rewrite_generic_calls_in_expr(
                    &mut field.value,
                    generic_defs,
                    var_types,
                    instantiations,
                );
            }
        }
        IREexpr::FieldAccess { base, .. } => {
            rewrite_generic_calls_in_expr(base, generic_defs, var_types, instantiations);
        }
        IREexpr::EnumLit {
            args, named_fields, ..
        } => {
            for arg in args {
                rewrite_generic_calls_in_expr(arg, generic_defs, var_types, instantiations);
            }
            if let Some(fields) = named_fields {
                for (_, value) in fields {
                    rewrite_generic_calls_in_expr(value, generic_defs, var_types, instantiations);
                }
            }
        }
        IREexpr::Match {
            expr: inner, arms, ..
        } => {
            rewrite_generic_calls_in_expr(inner, generic_defs, var_types, instantiations);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    rewrite_generic_calls_in_expr(guard, generic_defs, var_types, instantiations);
                }
                let mut arm_vars = var_types.clone();
                rewrite_generic_calls_in_block(
                    &mut arm.body,
                    generic_defs,
                    &mut arm_vars,
                    instantiations,
                );
            }
        }
        IREexpr::ArrayLiteral { elements, repeat } => {
            for element in elements {
                rewrite_generic_calls_in_expr(element, generic_defs, var_types, instantiations);
            }
            if let Some((value, _)) = repeat {
                rewrite_generic_calls_in_expr(value, generic_defs, var_types, instantiations);
            }
        }
        IREexpr::Index { target, index, .. } => {
            rewrite_generic_calls_in_expr(target, generic_defs, var_types, instantiations);
            rewrite_generic_calls_in_expr(index, generic_defs, var_types, instantiations);
        }
        IREexpr::Assign { value, .. } => {
            rewrite_generic_calls_in_expr(value, generic_defs, var_types, instantiations);
        }
        IREexpr::AssignIndex {
            target,
            index,
            value,
        } => {
            rewrite_generic_calls_in_expr(target, generic_defs, var_types, instantiations);
            rewrite_generic_calls_in_expr(index, generic_defs, var_types, instantiations);
            rewrite_generic_calls_in_expr(value, generic_defs, var_types, instantiations);
        }
        IREexpr::Cast { expr: inner, .. } => {
            rewrite_generic_calls_in_expr(inner, generic_defs, var_types, instantiations);
        }
        IREexpr::Lit(_)
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::Var(_)
        | IREexpr::StringLit(_) => {}
    }
}

fn infer_ir_expr_type(expr: &IREexpr, var_types: &HashMap<String, Type>) -> Option<Type> {
    match expr {
        IREexpr::Var(name) => var_types.get(name).cloned(),
        IREexpr::Lit(_) => Some(Type::Int),
        IREexpr::BoolLiteral(_) => Some(Type::Bool),
        IREexpr::FloatLiteral(_) => Some(Type::F64),
        IREexpr::StringLit(_) => Some(Type::String),
        IREexpr::Call {
            return_type: Some(ty),
            ..
        } => Some(ty.clone()),
        _ => None,
    }
}

fn infer_generic_substitutions(
    expected: &Type,
    actual: &Type,
    fn_generics: &[String],
) -> HashMap<String, Type> {
    let mut subs = HashMap::new();
    match (expected, actual) {
        (
            Type::Generic {
                name: e_name,
                params: e_params,
            },
            Type::Generic {
                name: a_name,
                params: a_params,
            },
        ) if e_name == a_name && e_params.len() == a_params.len() => {
            for (e, a) in e_params.iter().zip(a_params.iter()) {
                subs.extend(infer_generic_substitutions(e, a, fn_generics));
            }
        }
        (Type::Struct(param_name), actual_ty) if fn_generics.contains(param_name) => {
            subs.insert(param_name.clone(), actual_ty.clone());
        }
        _ => {}
    }
    subs
}

fn mangle_function_name(
    base: &str,
    generic_params: &[String],
    substitutions: &HashMap<String, Type>,
) -> String {
    let concrete: Vec<Type> = generic_params
        .iter()
        .filter_map(|name| substitutions.get(name).cloned())
        .collect();
    if concrete.len() != generic_params.len() {
        return base.to_string();
    }
    mangle_type_name(base, &concrete)
}

fn mangle_type_name(base: &str, params: &[Type]) -> String {
    if params.is_empty() {
        base.to_string()
    } else {
        let param_strs: Vec<String> = params.iter().map(type_name_for_mangle).collect();
        format!("{}_{}", base, param_strs.join("_"))
    }
}

fn type_name_for_mangle(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Bool => "bool".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::U8 => "u8".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::UInt => "uint".to_string(),
        Type::String => "String".to_string(),
        Type::Struct(name) | Type::Enum(name) => name.clone(),
        Type::Generic { name, params } => mangle_type_name(name, params),
        Type::Box { inner } => mangle_type_name("Box", std::slice::from_ref(inner)),
        Type::Vec { elem_type } => mangle_type_name("Vec", std::slice::from_ref(elem_type)),
        Type::Ref { inner, .. } => format!("ref_{}", type_name_for_mangle(inner)),
        Type::RawPtr { inner } => format!("ptr_{}", type_name_for_mangle(inner)),
        Type::Array { inner, size } => format!("{}_{}", type_name_for_mangle(inner), size),
        Type::Slice { inner } => format!("slice_{}", type_name_for_mangle(inner)),
        Type::Channel { elem_type } => mangle_type_name("Channel", std::slice::from_ref(elem_type)),
        Type::Sender { elem_type } => mangle_type_name("Sender", std::slice::from_ref(elem_type)),
        Type::Receiver { elem_type } => {
            mangle_type_name("Receiver", std::slice::from_ref(elem_type))
        }
        Type::Tuple { elements } => {
            let parts: Vec<String> = elements.iter().map(type_name_for_mangle).collect();
            format!("tuple_{}", parts.join("_"))
        }
    }
}

fn instantiate_generic_function(
    template: &IRFunction,
    substitutions: &HashMap<String, Type>,
) -> IRFunction {
    let name = mangle_function_name(&template.name, &template.generics, substitutions);
    let params = template
        .params
        .iter()
        .map(|p| IRParam {
            name: p.name.clone(),
            ty: substitute_type(&p.ty, substitutions),
        })
        .collect();
    let return_type = template
        .return_type
        .as_ref()
        .map(|ty| substitute_type(ty, substitutions));
    let blocks = template
        .blocks
        .iter()
        .map(|block| substitute_types_in_block(block, substitutions))
        .collect();
    let defers = template
        .defers
        .iter()
        .map(|expr| substitute_types_in_expr(expr, substitutions))
        .collect();
    IRFunction {
        name,
        generics: Vec::new(),
        params,
        return_type,
        blocks,
        defers,
    }
}

fn substitute_types_in_block(block: &IRBlock, substitutions: &HashMap<String, Type>) -> IRBlock {
    IRBlock {
        name: block.name.clone(),
        statements: block
            .statements
            .iter()
            .map(|stmt| substitute_types_in_stmt(stmt, substitutions))
            .collect(),
    }
}

fn substitute_types_in_stmt(stmt: &IRStmt, substitutions: &HashMap<String, Type>) -> IRStmt {
    match stmt {
        IRStmt::Let(let_stmt) => IRStmt::Let(IRLetStmt {
            name: let_stmt.name.clone(),
            ty: substitute_type(&let_stmt.ty, substitutions),
            init: let_stmt
                .init
                .as_ref()
                .map(|expr| substitute_types_in_expr(expr, substitutions)),
        }),
        IRStmt::Return(ret) => IRStmt::Return(IRReturn {
            value: ret
                .value
                .as_ref()
                .map(|expr| substitute_types_in_expr(expr, substitutions)),
        }),
        IRStmt::Expr(expr) => IRStmt::Expr(substitute_types_in_expr(expr, substitutions)),
        IRStmt::Defer(expr) => IRStmt::Defer(substitute_types_in_expr(expr, substitutions)),
        IRStmt::Spawn(spawn) => IRStmt::Spawn(IRSpawn {
            captures: spawn
                .captures
                .iter()
                .map(|(name, ty)| (name.clone(), substitute_type(ty, substitutions)))
                .collect(),
            body: substitute_types_in_block(&spawn.body, substitutions),
            defers: spawn
                .defers
                .iter()
                .map(|expr| substitute_types_in_expr(expr, substitutions))
                .collect(),
        }),
        IRStmt::If(ir_if) => IRStmt::If(IRIf {
            cond: substitute_types_in_expr(&ir_if.cond, substitutions),
            then_block: substitute_types_in_block(&ir_if.then_block, substitutions),
            else_block: ir_if
                .else_block
                .as_ref()
                .map(|block| substitute_types_in_block(block, substitutions)),
        }),
        IRStmt::While(ir_while) => IRStmt::While(IRWhile {
            cond: substitute_types_in_expr(&ir_while.cond, substitutions),
            body: substitute_types_in_block(&ir_while.body, substitutions),
        }),
        IRStmt::UnsafeBlock(unsafe_block) => IRStmt::UnsafeBlock(IRUnsafeBlock {
            body: substitute_types_in_block(&unsafe_block.body, substitutions),
        }),
    }
}

fn substitute_types_in_expr(expr: &IREexpr, substitutions: &HashMap<String, Type>) -> IREexpr {
    match expr {
        IREexpr::Call {
            callee,
            args,
            return_type,
            tuple_destructure_index,
        } => IREexpr::Call {
            callee: callee.clone(),
            args: args
                .iter()
                .map(|arg| substitute_types_in_expr(arg, substitutions))
                .collect(),
            return_type: return_type
                .as_ref()
                .map(|ty| substitute_type(ty, substitutions)),
            tuple_destructure_index: *tuple_destructure_index,
        },
        IREexpr::Send {
            channel,
            value,
            value_type,
        } => IREexpr::Send {
            channel: Box::new(substitute_types_in_expr(channel, substitutions)),
            value: Box::new(substitute_types_in_expr(value, substitutions)),
            value_type: substitute_type(value_type, substitutions),
        },
        IREexpr::Recv { channel, elem_type } => IREexpr::Recv {
            channel: Box::new(substitute_types_in_expr(channel, substitutions)),
            elem_type: substitute_type(elem_type, substitutions),
        },
        IREexpr::Index {
            target,
            index,
            target_type,
        } => IREexpr::Index {
            target: Box::new(substitute_types_in_expr(target, substitutions)),
            index: Box::new(substitute_types_in_expr(index, substitutions)),
            target_type: target_type
                .as_ref()
                .map(|ty| substitute_type(ty, substitutions)),
        },
        IREexpr::Cast {
            expr: inner,
            target_type,
        } => IREexpr::Cast {
            expr: Box::new(substitute_types_in_expr(inner, substitutions)),
            target_type: substitute_type(target_type, substitutions),
        },
        IREexpr::AddressOf { inner, mutable } => IREexpr::AddressOf {
            inner: Box::new(substitute_types_in_expr(inner, substitutions)),
            mutable: *mutable,
        },
        IREexpr::BinOp { op, left, right } => IREexpr::BinOp {
            op: *op,
            left: Box::new(substitute_types_in_expr(left, substitutions)),
            right: Box::new(substitute_types_in_expr(right, substitutions)),
        },
        IREexpr::UnOp { op, operand } => IREexpr::UnOp {
            op: *op,
            operand: Box::new(substitute_types_in_expr(operand, substitutions)),
        },
        IREexpr::StructLit { type_name, fields } => IREexpr::StructLit {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|field| IRStructLitField {
                    name: field.name.clone(),
                    value: substitute_types_in_expr(&field.value, substitutions),
                })
                .collect(),
        },
        IREexpr::FieldAccess {
            base,
            field,
            is_pointer,
        } => IREexpr::FieldAccess {
            base: Box::new(substitute_types_in_expr(base, substitutions)),
            field: field.clone(),
            is_pointer: *is_pointer,
        },
        IREexpr::EnumLit {
            enum_name,
            variant,
            args,
            named_fields,
        } => IREexpr::EnumLit {
            enum_name: enum_name.clone(),
            variant: variant.clone(),
            args: args
                .iter()
                .map(|arg| substitute_types_in_expr(arg, substitutions))
                .collect(),
            named_fields: named_fields.as_ref().map(|fields| {
                fields
                    .iter()
                    .map(|(name, value)| {
                        (name.clone(), substitute_types_in_expr(value, substitutions))
                    })
                    .collect()
            }),
        },
        IREexpr::Match {
            expr: inner,
            enum_type,
            arms,
        } => IREexpr::Match {
            expr: Box::new(substitute_types_in_expr(inner, substitutions)),
            enum_type: enum_type.clone(),
            arms: arms
                .iter()
                .map(|arm| IRMatchArm {
                    pattern: arm.pattern.clone(),
                    guard: arm
                        .guard
                        .as_ref()
                        .map(|g| substitute_types_in_expr(g, substitutions)),
                    body: substitute_types_in_block(&arm.body, substitutions),
                })
                .collect(),
        },
        IREexpr::ArrayLiteral { elements, repeat } => IREexpr::ArrayLiteral {
            elements: elements
                .iter()
                .map(|element| substitute_types_in_expr(element, substitutions))
                .collect(),
            repeat: repeat.as_ref().map(|(value, count)| {
                (
                    Box::new(substitute_types_in_expr(value, substitutions)),
                    *count,
                )
            }),
        },
        IREexpr::Assign { target, value } => IREexpr::Assign {
            target: target.clone(),
            value: Box::new(substitute_types_in_expr(value, substitutions)),
        },
        IREexpr::AssignIndex {
            target,
            index,
            value,
        } => IREexpr::AssignIndex {
            target: Box::new(substitute_types_in_expr(target, substitutions)),
            index: Box::new(substitute_types_in_expr(index, substitutions)),
            value: Box::new(substitute_types_in_expr(value, substitutions)),
        },
        IREexpr::Lit(v) => IREexpr::Lit(*v),
        IREexpr::BoolLiteral(v) => IREexpr::BoolLiteral(*v),
        IREexpr::FloatLiteral(v) => IREexpr::FloatLiteral(*v),
        IREexpr::Var(v) => IREexpr::Var(v.clone()),
        IREexpr::StringLit(v) => IREexpr::StringLit(v.clone()),
    }
}

fn substitute_type(ty: &Type, substitutions: &HashMap<String, Type>) -> Type {
    match ty {
        Type::Generic { name, params } => {
            if params.is_empty() {
                substitutions
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| ty.clone())
            } else {
                Type::Generic {
                    name: name.clone(),
                    params: params
                        .iter()
                        .map(|p| substitute_type(p, substitutions))
                        .collect(),
                }
            }
        }
        Type::Struct(name) | Type::Enum(name) => substitutions
            .get(name)
            .cloned()
            .unwrap_or_else(|| ty.clone()),
        Type::Ref { inner, mutable } => Type::Ref {
            inner: Box::new(substitute_type(inner, substitutions)),
            mutable: *mutable,
        },
        Type::RawPtr { inner } => Type::RawPtr {
            inner: Box::new(substitute_type(inner, substitutions)),
        },
        Type::Box { inner } => Type::Box {
            inner: Box::new(substitute_type(inner, substitutions)),
        },
        Type::Vec { elem_type } => Type::Vec {
            elem_type: Box::new(substitute_type(elem_type, substitutions)),
        },
        Type::Channel { elem_type } => Type::Channel {
            elem_type: Box::new(substitute_type(elem_type, substitutions)),
        },
        Type::Array { inner, size } => Type::Array {
            inner: Box::new(substitute_type(inner, substitutions)),
            size: *size,
        },
        Type::Slice { inner } => Type::Slice {
            inner: Box::new(substitute_type(inner, substitutions)),
        },
        Type::Sender { elem_type } => Type::Sender {
            elem_type: Box::new(substitute_type(elem_type, substitutions)),
        },
        Type::Receiver { elem_type } => Type::Receiver {
            elem_type: Box::new(substitute_type(elem_type, substitutions)),
        },
        Type::Tuple { elements } => Type::Tuple {
            elements: elements
                .iter()
                .map(|e| substitute_type(e, substitutions))
                .collect(),
        },
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
        | Type::String => ty.clone(),
    }
}
