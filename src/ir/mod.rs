use crate::ast::*;

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
    pub body: IRBlock,
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
    pub body: IRBlock,
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
        IRProgram {
            structs: ast.structs.clone(),
            enums: ast.enums.clone(),
            functions,
            extern_blocks: ast.extern_blocks.clone(),
            type_aliases: ast.type_aliases.clone(),
        }
    }

    fn build_function(&self, function: &FnDecl) -> IRFunction {
        let params = function
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

        for stmt in &function.body.statements {
            Self::lower_stmt(stmt, &mut statements, &mut defers);
        }

        // Create a single basic block named "entry"
        let blocks = vec![IRBlock {
            name: "entry".to_string(),
            statements,
        }];

        IRFunction {
            name: function.name.clone(),
            params,
            return_type: function.return_type.clone(),
            blocks,
            defers,
        }
    }

    fn lower_stmt(stmt: &Stmt, out: &mut Vec<IRStmt>, defers: &mut Vec<IREexpr>) {
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
                                    args: call_expr.args.iter().map(build_expr).collect(),
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
                                    args: call_expr.args.iter().map(build_expr).collect(),
                                    return_type: Some(Type::Tuple {
                                        elements: elements.clone(),
                                    }),
                                    tuple_destructure_index: Some(1), // 1 = second element (receiver)
                                }),
                            }));
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

                out.push(IRStmt::Let(IRLetStmt {
                    name: let_stmt.name.clone(),
                    ty,
                    init: let_stmt.init.as_ref().map(build_expr),
                }));
            }
            Stmt::Return(return_stmt) => {
                out.push(IRStmt::Return(IRReturn {
                    value: return_stmt.value.as_ref().map(build_expr),
                }));
            }
            Stmt::Expr(expr_stmt) => {
                out.push(IRStmt::Expr(build_expr(&expr_stmt.expr)));
            }
            Stmt::Defer(defer_stmt) => {
                // Collect defers per function; they will be emitted at the epilogue.
                defers.push(build_expr(&defer_stmt.expr));
            }
            Stmt::Spawn(spawn_stmt) => {
                // Lower spawn to a nested block in IR for now (synchronous execution).
                let mut inner_stmts = Vec::new();
                for inner in &spawn_stmt.body.statements {
                    Self::lower_stmt(inner, &mut inner_stmts, defers);
                }
                let inner_block = IRBlock {
                    name: "spawn_body".to_string(),
                    statements: inner_stmts,
                };
                out.push(IRStmt::Spawn(IRSpawn { body: inner_block }));
            }
            Stmt::If(if_stmt) => {
                // Lower `if` to an IRIf with nested blocks.
                let cond = build_expr(&if_stmt.cond);

                let mut then_stmts = Vec::new();
                for inner in &if_stmt.then_block.statements {
                    Self::lower_stmt(inner, &mut then_stmts, defers);
                }
                let then_block = IRBlock {
                    name: "then_block".to_string(),
                    statements: then_stmts,
                };

                let else_block = if let Some(ref else_blk) = if_stmt.else_block {
                    let mut else_stmts = Vec::new();
                    for inner in &else_blk.statements {
                        Self::lower_stmt(inner, &mut else_stmts, defers);
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
                let cond = build_expr(&while_stmt.cond);
                let mut body_stmts = Vec::new();
                for inner in &while_stmt.body.statements {
                    Self::lower_stmt(inner, &mut body_stmts, defers);
                }
                let body = IRBlock {
                    name: "while_body".to_string(),
                    statements: body_stmts,
                };
                out.push(IRStmt::While(IRWhile { cond, body }));
            }
            Stmt::For(for_stmt) => {
                // Desugar: for x in container { body }
                // to:
                //   let mut __i = 0;
                //   while __i < container.len() {
                //       let __opt = container.get(__i);
                //       match __opt {
                //           Option::Some(x) => {
                //               body;
                //               __i = __i + 1;
                //           }
                //           Option::None => {}
                //       }
                //   }

                // Create unique variable names to avoid conflicts
                let container_var = format!("__for_container_{}", for_stmt.span.start);
                let index_var = format!("__for_i_{}", for_stmt.span.start);
                let opt_var = format!("__for_opt_{}", for_stmt.span.start);

                // 1. Initialize container variable: let container_var = container_expr;
                // Infer the container type from the expression
                // Note: For variables, infer_type_from_expr returns Type::Int as fallback,
                // but the type checker has verified it's actually Vec<T>, String, or Array<T>.
                // Since we're using Vec::len and Vec::get below, we know it must be a Vec.
                // For now, we'll use Vec<int> as a default - the actual element type doesn't
                // matter much for the container variable since we're just passing it to Vec methods.
                let container_ty = match infer_type_from_expr(&for_stmt.iterable) {
                    Type::Int => {
                        // Variable type inference failed - default to Vec<int> since we use Vec methods
                        Type::Vec {
                            elem_type: Box::new(Type::Int),
                        }
                    }
                    ty => ty,
                };
                out.push(IRStmt::Let(IRLetStmt {
                    name: container_var.clone(),
                    ty: container_ty,
                    init: Some(build_expr(&for_stmt.iterable)),
                }));

                // 2. Initialize index variable: let mut __i = 0;
                out.push(IRStmt::Let(IRLetStmt {
                    name: index_var.clone(),
                    ty: Type::Int,
                    init: Some(IREexpr::Lit(0)),
                }));

                // 3. Create condition: __i < container.len()
                let container_var_expr = IREexpr::Var(container_var.clone());
                let len_call = IREexpr::Call {
                    callee: "Vec::len".to_string(), // Assume Vec for now - type checker has verified this
                    args: vec![IREexpr::AddressOf {
                        inner: Box::new(container_var_expr.clone()),
                        mutable: false,
                    }],
                    return_type: Some(Type::Int),
                    tuple_destructure_index: None,
                };
                let cond = IREexpr::BinOp {
                    op: BinOp::Lt,
                    left: Box::new(IREexpr::Var(index_var.clone())),
                    right: Box::new(len_call),
                };

                // 4. Create match body with loop body + increment
                let mut match_body_stmts = Vec::new();

                // Add loop body statements
                for stmt in &for_stmt.body.statements {
                    Self::lower_stmt(stmt, &mut match_body_stmts, defers);
                }

                // Add increment: __i = __i + 1
                let incr_value = IREexpr::BinOp {
                    op: BinOp::Add,
                    left: Box::new(IREexpr::Var(index_var.clone())),
                    right: Box::new(IREexpr::Lit(1)),
                };
                match_body_stmts.push(IRStmt::Expr(IREexpr::Assign {
                    target: index_var.clone(),
                    value: Box::new(incr_value),
                }));

                let match_body = IRBlock {
                    name: "for_match_body".to_string(),
                    statements: match_body_stmts,
                };

                // 5. Create Option::None arm (empty)
                let none_body = IRBlock {
                    name: "for_none_body".to_string(),
                    statements: Vec::new(),
                };

                // 6. Create get call: container.get(__i)
                let get_call = IREexpr::Call {
                    callee: "Vec::get".to_string(),
                    args: vec![
                        IREexpr::AddressOf {
                            inner: Box::new(container_var_expr),
                            mutable: false,
                        },
                        IREexpr::Var(index_var.clone()),
                    ],
                    return_type: Some(Type::Generic {
                        name: "Option".to_string(),
                        params: vec![Type::Int], // Element type - type checker has verified this
                    }),
                    tuple_destructure_index: None,
                };

                // 7. Create match expression
                let match_expr = IREexpr::Match {
                    expr: Box::new(IREexpr::Var(opt_var.clone())),
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
                            body: match_body,
                        },
                        IRMatchArm {
                            pattern: IRPattern::Variant {
                                enum_name: "Option".to_string(),
                                variant: "None".to_string(),
                                sub_patterns: Vec::new(),
                                named_fields: None,
                            },
                            body: none_body,
                        },
                    ],
                };

                // 8. Create while loop body with opt variable and match
                let while_body_stmts = vec![
                    // Store Option in a variable
                    IRStmt::Let(IRLetStmt {
                        name: opt_var,
                        ty: Type::Generic {
                            name: "Option".to_string(),
                            params: vec![Type::Int],
                        },
                        init: Some(get_call),
                    }),
                    // Add match statement
                    IRStmt::Expr(match_expr),
                ];

                let while_body = IRBlock {
                    name: "for_while_body".to_string(),
                    statements: while_body_stmts,
                };

                // 9. Create and add while loop
                out.push(IRStmt::While(IRWhile {
                    cond,
                    body: while_body,
                }));
            }
            Stmt::UnsafeBlock(unsafe_stmt) => {
                let mut body_stmts = Vec::new();
                for inner in &unsafe_stmt.body.statements {
                    Self::lower_stmt(inner, &mut body_stmts, defers);
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

fn build_expr(expr: &Expr) -> IREexpr {
    match expr {
        Expr::Lit(lit_expr) => IREexpr::Lit(lit_expr.value),
        Expr::BoolLiteral(bool_expr) => IREexpr::BoolLiteral(bool_expr.value),
        Expr::FloatLiteral(float_expr) => IREexpr::FloatLiteral(float_expr.value),
        Expr::Var(var_expr) => IREexpr::Var(var_expr.name.clone()),
        Expr::Ref(ref_expr) => {
            // Reference expression: &x or &mut x
            IREexpr::AddressOf {
                inner: Box::new(build_expr(&ref_expr.inner)),
                mutable: ref_expr.mutable,
            }
        }
        Expr::BinOp(bin_op_expr) => IREexpr::BinOp {
            op: bin_op_expr.op,
            left: Box::new(build_expr(&bin_op_expr.left)),
            right: Box::new(build_expr(&bin_op_expr.right)),
        },
        Expr::UnOp(un_op_expr) => IREexpr::UnOp {
            op: un_op_expr.op,
            operand: Box::new(build_expr(&un_op_expr.operand)),
        },
        Expr::Send(send_expr) => IREexpr::Send {
            channel: Box::new(build_expr(&send_expr.channel)),
            value: Box::new(build_expr(&send_expr.value)),
        },
        Expr::Recv(recv_expr) => {
            // For now, we do not track the exact element type in IR and rely on
            // the type checker to enforce correctness. We use `int` as the
            // concrete type in codegen, which matches our tests.
            IREexpr::Recv {
                channel: Box::new(build_expr(&recv_expr.channel)),
                elem_type: Type::Int,
            }
        }
        Expr::StructLit(lit) => IREexpr::StructLit {
            type_name: lit.type_name.clone(),
            fields: lit
                .fields
                .iter()
                .map(|f| IRStructLitField {
                    name: f.name.clone(),
                    value: build_expr(&f.value),
                })
                .collect(),
        },
        Expr::FieldAccess(acc) => IREexpr::FieldAccess {
            base: Box::new(build_expr(&acc.base)),
            field: acc.field.clone(),
            is_pointer: false, // Default to false as we don't have type info here
        },
        Expr::EnumLit(enum_lit) => IREexpr::EnumLit {
            enum_name: enum_lit.enum_name.clone(),
            variant: enum_lit.variant.clone(),
            args: enum_lit.args.iter().map(build_expr).collect(),
            named_fields: enum_lit.named_fields.as_ref().map(|fields| {
                fields
                    .iter()
                    .map(|(field_name, field_expr)| (field_name.clone(), build_expr(field_expr)))
                    .collect()
            }),
        },
        Expr::Match(match_expr) => {
            let arms = match_expr
                .arms
                .iter()
                .map(|arm| {
                    let pattern = match &arm.pattern {
                        Pattern::Variant {
                            enum_name,
                            variant,
                            sub_patterns,
                            named_fields,
                            span: _,
                        } => {
                            IRPattern::Variant {
                                enum_name: enum_name.clone(),
                                variant: variant.clone(),
                                sub_patterns: sub_patterns
                                    .iter()
                                    .map(|p| match p {
                                        Pattern::Variant {
                                            enum_name,
                                            variant,
                                            sub_patterns: _,
                                            named_fields: _,
                                            span: _,
                                        } => {
                                            IRPattern::Variant {
                                                enum_name: enum_name.clone(),
                                                variant: variant.clone(),
                                                sub_patterns: Vec::new(), // TODO: Handle nested patterns
                                                named_fields: None,
                                            }
                                        }
                                        Pattern::Wildcard { span: _ } => IRPattern::Wildcard,
                                        Pattern::Binding { name, span: _ } => {
                                            IRPattern::Binding { name: name.clone() }
                                        }
                                    })
                                    .collect(),
                                named_fields: named_fields.as_ref().map(|fields| {
                                    fields
                                        .iter()
                                        .map(|(field_name, field_pattern)| {
                                            let ir_field_pattern = match field_pattern {
                                                Pattern::Variant {
                                                    enum_name,
                                                    variant,
                                                    sub_patterns: _,
                                                    named_fields: _,
                                                    span: _,
                                                } => IRPattern::Variant {
                                                    enum_name: enum_name.clone(),
                                                    variant: variant.clone(),
                                                    sub_patterns: Vec::new(),
                                                    named_fields: None,
                                                },
                                                Pattern::Wildcard { span: _ } => {
                                                    IRPattern::Wildcard
                                                }
                                                Pattern::Binding { name, span: _ } => {
                                                    IRPattern::Binding { name: name.clone() }
                                                }
                                            };
                                            (field_name.clone(), ir_field_pattern)
                                        })
                                        .collect()
                                }),
                            }
                        }
                        Pattern::Wildcard { span: _ } => IRPattern::Wildcard,
                        Pattern::Binding { name, span: _ } => {
                            IRPattern::Binding { name: name.clone() }
                        }
                    };
                    let mut body_stmts = Vec::new();
                    let mut defers = Vec::new();
                    for stmt in &arm.body.statements {
                        IRBuilder::lower_stmt(stmt, &mut body_stmts, &mut defers);
                    }
                    IRMatchArm {
                        pattern,
                        body: IRBlock {
                            name: "match_arm".to_string(),
                            statements: body_stmts,
                        },
                    }
                })
                .collect();
            {
                // Extract enum name from the first variant pattern
                let enum_name = match_expr
                    .arms
                    .iter()
                    .find_map(|arm| match &arm.pattern {
                        Pattern::Variant { enum_name, .. } => Some(enum_name.clone()),
                        _ => None,
                    })
                    .unwrap_or_else(|| "Unknown".to_string());

                IREexpr::Match {
                    expr: Box::new(build_expr(&match_expr.expr)),
                    enum_type: enum_name,
                    arms,
                }
            }
        }
        Expr::Call(call_expr) => {
            let return_type = infer_type_from_call(&call_expr.callee, &call_expr.args);
            IREexpr::Call {
                callee: call_expr.callee.clone(),
                args: call_expr.args.iter().map(build_expr).collect(),
                return_type,
                tuple_destructure_index: None,
            }
        }
        Expr::MethodCall(method_call) => {
            // Desugar method calls: receiver becomes first arg, use placeholder callee
            let receiver_expr = build_expr(&method_call.receiver);
            let method_args: Vec<IREexpr> = method_call.args.iter().map(build_expr).collect();
            let mut all_args = vec![receiver_expr];
            all_args.extend(method_args);
            let placeholder_callee = format!("METHOD::{}", method_call.method);

            IREexpr::Call {
                callee: placeholder_callee,
                args: all_args,
                return_type: None,
                tuple_destructure_index: None,
            }
        }
        Expr::StringLit(string_lit) => IREexpr::StringLit(string_lit.value.clone()),
        Expr::ArrayLiteral(arr_lit) => IREexpr::ArrayLiteral {
            elements: arr_lit.elements.iter().map(build_expr).collect(),
            repeat: arr_lit
                .repeat
                .as_ref()
                .map(|(expr, count)| (Box::new(build_expr(expr)), *count)),
        },
        Expr::Index(index_expr) => IREexpr::Index {
            target: Box::new(build_expr(&index_expr.target)),
            index: Box::new(build_expr(&index_expr.index)),
            target_type: None, // TODO: Get from type checker
        },
        Expr::Cast(cast_expr) => IREexpr::Cast {
            expr: Box::new(build_expr(&cast_expr.expr)),
            target_type: cast_expr.target_type.clone(),
        },
        Expr::Assign(assign_expr) => {
            // Handle assignment: target = value
            match &*assign_expr.target {
                Expr::Var(var_expr) => {
                    // Variable assignment: x = value
                    IREexpr::Assign {
                        target: var_expr.name.clone(),
                        value: Box::new(build_expr(&assign_expr.value)),
                    }
                }
                Expr::Index(index_expr) => {
                    // Array element assignment: arr[i] = value
                    // We need a way to represent this in IR
                    // For now, we'll use a special form or extend Assign
                    // Let's create an AssignIndex variant or handle it specially
                    // Actually, let's check if we can use the existing Assign with a special target format
                    // Or we need to add AssignIndex to IREexpr
                    // For simplicity, let's add AssignIndex
                    IREexpr::AssignIndex {
                        target: Box::new(build_expr(&index_expr.target)),
                        index: Box::new(build_expr(&index_expr.index)),
                        value: Box::new(build_expr(&assign_expr.value)),
                    }
                }
                _ => {
                    // This shouldn't happen due to parser validation, but handle it
                    panic!("Invalid assignment target in IR lowering");
                }
            }
        }
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
        Expr::Ref(ref_expr) => {
            // &T -> T, but we can't infer T from the inner expression easily
            // For now, just infer from inner
            infer_type_from_expr(&ref_expr.inner)
        }
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
