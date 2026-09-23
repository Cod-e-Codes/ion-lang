use crate::ast::*;
use crate::tc::{TypeInfo, collect_captured_vars};

/// Sentinel lowered into nested-match catch-all bindings. Codegen rewrites it to
/// the parent match's C scrutinee temp so `other` is the whole outer value.
pub(crate) const MATCH_PARENT_SCRUTINEE: &str = "__ion_match_parent_scrutinee";
use crate::types_util::{infer_generic_substitutions, ref_to_vec_elem, slice_elem_type};
use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct IRProgram {
    pub structs: Vec<StructDecl>,
    pub enums: Vec<EnumDecl>,
    pub functions: Vec<IRFunction>,
    pub extern_blocks: Vec<ExternBlock>,
    pub type_aliases: Vec<TypeAliasDecl>,
    pub drop_impls: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct IRFunction {
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<IRParam>,
    pub return_type: Option<Type>,
    pub blocks: Vec<IRBlock>,
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
    /// Defers registered in this block, in source order (emitted LIFO at scope exit).
    pub defers: Vec<IREexpr>,
}

#[derive(Debug, Clone)]
pub enum IRStmt {
    Let(IRLetStmt),
    Return(IRReturn),
    Break,
    Continue,
    Expr(IREexpr),
    Defer(IREexpr),
    Spawn(IRSpawn),
    Select(IRSelect),
    If(IRIf),
    While(IRWhile),
    UnsafeBlock(IRUnsafeBlock),
    Scope(IRScope),
}

#[derive(Debug, Clone)]
pub struct IRWhile {
    pub cond: IREexpr,
    pub body: IRBlock,
    /// For `for` loops lowered to `while`: statements run at end of each iteration.
    pub step: Option<IRBlock>,
    /// When set, `continue` in the loop body jumps here (the step block).
    pub continue_label: Option<String>,
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
    /// Value the thread returns. `Void` when the block does not return one.
    pub result: Type,
}

#[derive(Debug, Clone)]
pub struct IRSelectRecvArm {
    pub binding: Option<String>,
    pub channel: IREexpr,
    pub elem_type: Type,
    pub body: IRBlock,
}

#[derive(Debug, Clone)]
pub struct IRSelect {
    pub recv_arms: Vec<IRSelectRecvArm>,
    pub default_body: Option<IRBlock>,
    pub timeout_ms: Option<IREexpr>,
    pub timeout_body: Option<IRBlock>,
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
pub struct IRScope {
    pub body: IRBlock,
}

#[derive(Debug, Clone)]
pub enum IREexpr {
    Lit(i64),
    BoolLiteral(bool),
    FloatLiteral(f64),
    IntLimit {
        ty: Type,
        max: bool,
    },
    Var(String),
    AddressOf {
        inner: Box<IREexpr>,
        mutable: bool,
        /// Checked type, `Type::Ref` of `inner`.
        ty: Type,
    },
    BinOp {
        op: BinOp,
        left: Box<IREexpr>,
        right: Box<IREexpr>,
        result_type: Type,
    },
    UnOp {
        op: UnOp,
        operand: Box<IREexpr>,
        result_type: Type,
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
    Spawn {
        captures: Vec<(String, Type)>,
        body: IRBlock,
        result: Type,
    },
    StructLit {
        type_name: String,
        fields: Vec<IRStructLitField>,
    },
    FieldAccess {
        base: Box<IREexpr>,
        field: String,
        is_pointer: bool,
        /// Checked type of this field access.
        ty: Type,
    },
    EnumLit {
        enum_name: String,
        variant: String,
        args: Vec<IREexpr>,                           // For tuple variants
        named_fields: Option<Vec<(String, IREexpr)>>, // For struct variants: { field: expr }
        /// Instantiated enum type from TypeInfo (`Result<int, int>`, not the bare name).
        ty: Type,
    },
    Match {
        expr: Box<IREexpr>,
        enum_type: String,
        scrutinee_type: Option<Type>,
        /// Type of the match expression itself. `Void` when the match is a statement.
        result_type: Type,
        arms: Vec<IRMatchArm>,
    },
    Call {
        callee: String,
        args: Vec<IREexpr>,
        return_type: Option<Type>,
        tuple_destructure_index: Option<usize>, // For tuple destructuring: Some(0) = first element, Some(1) = second, etc.
    },
    StringLit(String),
    TupleLit {
        elements: Vec<IREexpr>,
        elem_types: Vec<Type>,
    },
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
        target_type: Option<Type>,
    },
    AssignField {
        target: Box<IREexpr>,
        value: Box<IREexpr>,
        /// Checked type of the field being assigned.
        field_ty: Type,
    },
    Cast {
        expr: Box<IREexpr>,
        target_type: Type,
    },
    FnLiteral(IRFnLiteral),
}

#[derive(Debug, Clone)]
pub struct IRFnLiteral {
    pub symbol: String,
    pub params: Vec<IRParam>,
    pub return_type: Option<Type>,
    pub body: IRBlock,
    /// Owned captures moved into the closure value. Empty for a function pointer.
    pub captures: Vec<(String, Type)>,
    /// Struct type name of a move closure.
    pub env_struct: Option<String>,
}

#[derive(Debug, Clone)]
pub struct IRMatchArm {
    pub pattern: IRPattern,
    pub guard: Option<IREexpr>,
    pub body: IRBlock,
}

#[derive(Clone)]
struct LoweringContext {
    var_types: HashMap<String, Type>,
    struct_decls: HashMap<String, StructDecl>,
    enum_decls: HashMap<String, EnumDecl>,
    enum_param_counts: HashMap<String, usize>,
    tuple_temp_counter: usize,
    fn_literal_counter: Rc<Cell<usize>>,
    function_returns: HashMap<String, Option<Type>>,
    types: TypeInfo,
}

impl LoweringContext {
    fn from_params(
        params: &[IRParam],
        struct_decls: HashMap<String, StructDecl>,
        enum_decls: HashMap<String, EnumDecl>,
        enum_param_counts: HashMap<String, usize>,
        fn_literal_counter: Rc<Cell<usize>>,
        function_returns: HashMap<String, Option<Type>>,
        types: TypeInfo,
    ) -> Self {
        let mut var_types = HashMap::new();
        for p in params {
            var_types.insert(p.name.clone(), p.ty.clone());
        }
        Self {
            var_types,
            struct_decls,
            enum_decls,
            enum_param_counts,
            tuple_temp_counter: 0,
            fn_literal_counter,
            function_returns,
            types,
        }
    }

    fn expr_type(&self, expr: &Expr) -> Type {
        self.types.resolve(&self.types.expr_type(expr))
    }

    fn resolve_expr_type(&self, expr: &Expr) -> Option<Type> {
        Some(self.expr_type(expr))
    }

    fn record_binding(&mut self, name: &str, ty: &Type) {
        self.var_types.insert(name.to_string(), ty.clone());
    }
}

fn record_match_arm_bindings(pattern: &Pattern, scrutinee_ty: &Type, ctx: &mut LoweringContext) {
    let original = scrutinee_ty.clone();
    let through_mut = matches!(scrutinee_ty, Type::Ref { mutable: true, .. });
    let through_ref = matches!(scrutinee_ty, Type::Ref { .. });
    let scrutinee_ty = match scrutinee_ty {
        Type::Ref { inner, .. } => inner.as_ref(),
        other => other,
    };
    let struct_decls = ctx.struct_decls.clone();
    let enum_decls = ctx.enum_decls.clone();
    let drop_impls = ctx.types.drop_impls.clone();
    let bind_ty = |field_ty: &Type| -> Type {
        if through_ref
            && !crate::tc::type_is_copy(field_ty, &struct_decls, &enum_decls, &drop_impls)
        {
            Type::Ref {
                inner: Box::new(field_ty.clone()),
                mutable: through_mut,
            }
        } else {
            field_ty.clone()
        }
    };
    let bound_value = if matches!(original, Type::Ref { .. }) {
        original
    } else {
        bind_ty(scrutinee_ty)
    };
    match pattern {
        Pattern::Binding { name, .. } => {
            ctx.record_binding(name, &bound_value);
        }
        Pattern::Wildcard { .. } => {}
        Pattern::Variant {
            variant,
            sub_patterns,
            named_fields,
            ..
        } => {
            record_variant_payload_bindings(
                variant,
                sub_patterns,
                named_fields.as_ref(),
                scrutinee_ty,
                ctx,
            );
            if let Type::Generic { params, .. } = scrutinee_ty
                && let Some(payload_ty) = params.first()
            {
                if let Some(sub) = sub_patterns.first() {
                    let bound = bind_ty(payload_ty);
                    record_match_arm_bindings(sub, &bound, ctx);
                }
                if let Some(named) = named_fields {
                    for (_, sub) in named {
                        let bound = bind_ty(payload_ty);
                        record_match_arm_bindings(sub, &bound, ctx);
                    }
                }
            }
        }
        Pattern::At { name, pattern, .. } => {
            ctx.record_binding(name, &bound_value);
            record_match_arm_bindings(pattern, &bound_value, ctx);
        }
        Pattern::Or { alts, .. } => {
            if let Some(alt) = alts.first() {
                let bound = bind_ty(scrutinee_ty);
                record_match_arm_bindings(alt, &bound, ctx);
            }
        }
        Pattern::Struct { name, fields, .. } => {
            let peeled = match scrutinee_ty {
                Type::Ref { inner, .. } => inner.as_ref(),
                other => other,
            };
            let decl = ctx.struct_decls.get(name).cloned();
            let subst = decl
                .as_ref()
                .map(|decl| struct_subst(decl, peeled))
                .unwrap_or_default();
            for (field_name, field_pattern) in fields {
                let field_ty = decl.as_ref().and_then(|decl| {
                    decl.fields
                        .iter()
                        .find(|field| field.name == *field_name)
                        .map(|field| ctx.types.resolve(&substitute_type(&field.ty, &subst)))
                });
                let Some(field_ty) = field_ty else {
                    panic!("compiler bug: struct '{name}' has no field '{field_name}'");
                };
                let bound = bind_ty(&field_ty);
                record_match_arm_bindings(field_pattern, &bound, ctx);
            }
        }
        Pattern::Lit { .. } | Pattern::Range { .. } | Pattern::Rest { .. } => {}
    }
}

fn match_scrutinee_type(expr: &Expr, ctx: &LoweringContext) -> Option<Type> {
    if let Expr::Call(call) = expr {
        return builtin_option_vec_return(&call.callee, &call.args, ctx).or_else(|| {
            lookup_merged(&ctx.function_returns, &call.callee).and_then(|ret| ret.clone())
        });
    }
    ctx.resolve_expr_type(expr)
}

fn expand_or_patterns(pattern: &Pattern) -> Vec<Pattern> {
    match pattern {
        Pattern::Or { alts, .. } => alts.iter().flat_map(expand_or_patterns).collect(),
        other => vec![other.clone()],
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
        Pattern::Binding { name, .. } => IRPattern::Binding {
            name: name.clone(),
            ty: None,
        },
        Pattern::Lit { lit, .. } => IRPattern::Lit { lit: lit.clone() },
        Pattern::Range { lo, hi, .. } => IRPattern::Range { lo: *lo, hi: *hi },
        Pattern::Or { alts, .. } => IRPattern::Or {
            alts: alts.iter().map(pattern_to_ir).collect(),
        },
        Pattern::Struct {
            name, fields, rest, ..
        } => IRPattern::Struct {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|(field, pattern)| (field.clone(), pattern_to_ir(pattern)))
                .collect(),
            rest: *rest,
        },
        Pattern::At { name, pattern, .. } => IRPattern::At {
            name: name.clone(),
            pattern: Box::new(pattern_to_ir(pattern)),
        },
        Pattern::Rest { .. } => IRPattern::Rest,
    }
}

fn enum_name_from_type(ty: &Type) -> Option<String> {
    match ty {
        // Parser stores user type names as Struct until tc; enums share the same name.
        Type::Enum(name) | Type::Struct(name) => Some(name.clone()),
        Type::Generic { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn infer_match_enum_name(expr: &Expr, ctx: &LoweringContext) -> Option<String> {
    if let Expr::EnumLit(lit) = expr {
        return Some(lit.enum_name.clone());
    }
    if let Some(ty) = ctx.resolve_expr_type(expr)
        && let Some(name) = enum_name_from_type(&ty)
    {
        return Some(name);
    }
    if let Expr::Call(call) = expr
        && let Some(ret) = ctx
            .function_returns
            .get(&call.callee)
            .and_then(|r| r.clone())
        && let Some(name) = enum_name_from_type(&ret)
    {
        return Some(name);
    }
    None
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
        /// Type of this name in the arm, from the same ref-pattern rule as the
        /// type checker. `None` for bindings synthesized later; codegen then
        /// uses the enum field type. Substitution replaces type parameters in
        /// `Some`, so a parameter bound as `&V` stays a reference after `V`
        /// becomes a copy type.
        ty: Option<Type>,
    },
    Lit {
        lit: crate::ast::PatLit,
    },
    Range {
        lo: i64,
        hi: i64,
    },
    Or {
        alts: Vec<IRPattern>,
    },
    Struct {
        name: String,
        fields: Vec<(String, IRPattern)>,
        rest: bool,
    },
    At {
        name: String,
        pattern: Box<IRPattern>,
    },
    Rest,
}

#[derive(Clone)]
enum NestedSlot {
    Pos(usize),
    Named(String),
}

fn zip_generic_params(generics: &[TypeParam], scrutinee: &Type) -> HashMap<String, Type> {
    let Type::Generic { params, .. } = scrutinee else {
        return HashMap::new();
    };
    generics
        .iter()
        .zip(params.iter())
        .map(|(param, ty)| (param.name.clone(), ty.clone()))
        .collect()
}

fn struct_subst(decl: &StructDecl, scrutinee: &Type) -> HashMap<String, Type> {
    zip_generic_params(&decl.generics, scrutinee)
}

fn generic_subst_from_scrutinee(decl: &EnumDecl, scrutinee: &Type) -> HashMap<String, Type> {
    let peeled = match scrutinee {
        Type::Ref { inner, .. } => inner.as_ref(),
        other => other,
    };
    zip_generic_params(&decl.generics, peeled)
}

fn peel_type_ref(ty: &Type) -> Type {
    match ty {
        Type::Ref { inner, .. } => peel_type_ref(inner),
        other => other.clone(),
    }
}

/// Record the type each binding has in this arm. A match through `&T` / `&mut T`
/// binds a non-copy field as a reference, including a type parameter. Later
/// substitution replaces the parameter and leaves the reference in place.
fn assign_pattern_binding_types(
    pattern: &mut IRPattern,
    scrutinee_ty: &Type,
    ctx: &LoweringContext,
) {
    let through_mut = matches!(scrutinee_ty, Type::Ref { mutable: true, .. });
    let through_ref = matches!(scrutinee_ty, Type::Ref { .. });
    let peeled = peel_type_ref(scrutinee_ty);
    let bind_field = |field_ty: &Type| -> Type {
        if through_ref
            && !crate::tc::type_is_copy(
                field_ty,
                &ctx.struct_decls,
                &ctx.enum_decls,
                &ctx.types.drop_impls,
            )
        {
            Type::Ref {
                inner: Box::new(field_ty.clone()),
                mutable: through_mut,
            }
        } else {
            field_ty.clone()
        }
    };
    match pattern {
        IRPattern::Binding { ty, .. } => {
            *ty = Some(if matches!(scrutinee_ty, Type::Ref { .. }) {
                scrutinee_ty.clone()
            } else {
                bind_field(&peeled)
            });
        }
        IRPattern::At { pattern, .. } => {
            assign_pattern_binding_types(pattern, scrutinee_ty, ctx);
        }
        IRPattern::Or { alts } => {
            for alt in alts {
                assign_pattern_binding_types(alt, scrutinee_ty, ctx);
            }
        }
        IRPattern::Struct { name, fields, .. } => {
            let decl = ctx.struct_decls.get(name).cloned();
            let subst = decl
                .as_ref()
                .map(|decl| struct_subst(decl, &peeled))
                .unwrap_or_default();
            for (field_name, field_pattern) in fields {
                let field_ty = decl.as_ref().and_then(|decl| {
                    decl.fields
                        .iter()
                        .find(|field| field.name == *field_name)
                        .map(|field| ctx.types.resolve(&substitute_type(&field.ty, &subst)))
                });
                let Some(field_ty) = field_ty else {
                    panic!("compiler bug: struct '{name}' has no field '{field_name}'");
                };
                assign_pattern_binding_types(field_pattern, &bind_field(&field_ty), ctx);
            }
        }
        IRPattern::Variant {
            enum_name,
            variant,
            sub_patterns,
            named_fields,
        } => {
            let Some(decl) = ctx.enum_decls.get(enum_name).cloned() else {
                return;
            };
            let subst = generic_subst_from_scrutinee(&decl, &peeled);
            let Some(vdecl) = decl.variants.iter().find(|v| v.name == *variant).cloned() else {
                return;
            };
            if let Some(named) = named_fields {
                let Some(fields) = vdecl.named_fields.clone() else {
                    return;
                };
                for (fname, sub) in named {
                    if let Some((_, ty)) = fields.iter().find(|(n, _)| n == fname) {
                        let concrete = ctx.types.resolve(&substitute_type(ty, &subst));
                        assign_pattern_binding_types(sub, &bind_field(&concrete), ctx);
                    }
                }
            } else {
                for (i, sub) in sub_patterns.iter_mut().enumerate() {
                    if let Some(ty) = vdecl.payload_types.get(i) {
                        let concrete = ctx.types.resolve(&substitute_type(ty, &subst));
                        assign_pattern_binding_types(sub, &bind_field(&concrete), ctx);
                    }
                }
            }
        }
        IRPattern::Wildcard | IRPattern::Lit { .. } | IRPattern::Range { .. } | IRPattern::Rest => {
        }
    }
}

fn ir_pattern_has_nested_variant(pattern: &IRPattern) -> bool {
    match pattern {
        IRPattern::Variant {
            sub_patterns,
            named_fields,
            ..
        } => {
            sub_patterns
                .iter()
                .any(|p| matches!(p, IRPattern::Variant { .. }) || ir_pattern_has_nested_variant(p))
                || named_fields.as_ref().is_some_and(|fields| {
                    fields.iter().any(|(_, p)| {
                        matches!(p, IRPattern::Variant { .. }) || ir_pattern_has_nested_variant(p)
                    })
                })
        }
        _ => false,
    }
}

fn slot_pattern<'a>(pattern: &'a IRPattern, slot: &NestedSlot) -> Option<&'a IRPattern> {
    match (pattern, slot) {
        (
            IRPattern::Variant {
                sub_patterns,
                named_fields,
                ..
            },
            NestedSlot::Pos(i),
        ) => {
            if named_fields.is_some() {
                None
            } else {
                sub_patterns.get(*i)
            }
        }
        (
            IRPattern::Variant {
                named_fields: Some(fields),
                ..
            },
            NestedSlot::Named(name),
        ) => fields.iter().find(|(n, _)| n == name).map(|(_, p)| p),
        _ => None,
    }
}

fn pattern_ctor_key(pattern: &IRPattern) -> String {
    match pattern {
        IRPattern::Variant { variant, .. } => format!("v:{variant}"),
        IRPattern::Wildcard => "_".to_string(),
        IRPattern::Binding { name, .. } => format!("b:{name}"),
        IRPattern::At { pattern, .. } => pattern_ctor_key(pattern),
        IRPattern::Or { .. } => "or".to_string(),
        IRPattern::Struct { name, .. } => format!("s:{name}"),
        IRPattern::Lit { .. } => "lit".to_string(),
        IRPattern::Range { .. } => "range".to_string(),
        IRPattern::Rest => "..".to_string(),
    }
}

fn payload_type_for_slot(
    ctx: &LoweringContext,
    enum_type: &str,
    variant: &str,
    slot: &NestedSlot,
    scrutinee_type: Option<&Type>,
) -> Type {
    let Some(decl) = ctx.enum_decls.get(enum_type) else {
        return Type::Enum("Unknown".to_string());
    };
    let subst = scrutinee_type
        .map(|ty| generic_subst_from_scrutinee(decl, ty))
        .unwrap_or_default();
    let Some(vdecl) = decl.variants.iter().find(|v| v.name == variant) else {
        return Type::Enum("Unknown".to_string());
    };
    let raw = match slot {
        NestedSlot::Pos(i) => vdecl.payload_types.get(*i).cloned(),
        NestedSlot::Named(name) => vdecl.named_fields.as_ref().and_then(|fields| {
            fields
                .iter()
                .find(|(n, _)| n == name)
                .map(|(_, t)| t.clone())
        }),
    };
    let ty = raw.unwrap_or_else(|| Type::Enum("Unknown".to_string()));
    peel_type_ref(&ctx.types.resolve(&substitute_type(&ty, &subst)))
}

fn nested_slots_for_group(group: &[IRMatchArm]) -> Vec<NestedSlot> {
    let mut slots = Vec::new();
    let Some(IRPattern::Variant {
        sub_patterns,
        named_fields,
        ..
    }) = group.first().map(|a| &a.pattern)
    else {
        return slots;
    };
    if let Some(fields) = named_fields {
        for (name, _) in fields {
            if group.iter().any(|arm| {
                slot_pattern(&arm.pattern, &NestedSlot::Named(name.clone()))
                    .is_some_and(|p| matches!(p, IRPattern::Variant { .. }))
            }) {
                slots.push(NestedSlot::Named(name.clone()));
            }
        }
    } else {
        let max_len = group
            .iter()
            .map(|arm| match &arm.pattern {
                IRPattern::Variant { sub_patterns, .. } => sub_patterns.len(),
                _ => 0,
            })
            .max()
            .unwrap_or(sub_patterns.len());
        for i in 0..max_len {
            if group.iter().any(|arm| {
                slot_pattern(&arm.pattern, &NestedSlot::Pos(i))
                    .is_some_and(|p| matches!(p, IRPattern::Variant { .. }))
            }) {
                slots.push(NestedSlot::Pos(i));
            }
        }
    }
    slots
}

fn replace_nested_slots_with_bindings(
    pattern: &IRPattern,
    slots: &[(NestedSlot, String)],
) -> IRPattern {
    let IRPattern::Variant {
        enum_name,
        variant,
        sub_patterns,
        named_fields,
    } = pattern
    else {
        return pattern.clone();
    };
    let mut sub_patterns = sub_patterns.clone();
    let mut named_fields = named_fields.clone();
    for (slot, temp) in slots {
        match slot {
            NestedSlot::Pos(i) => {
                if let Some(p) = sub_patterns.get_mut(*i) {
                    *p = IRPattern::Binding {
                        name: temp.clone(),
                        ty: None,
                    };
                }
            }
            NestedSlot::Named(name) => {
                if let Some(fields) = named_fields.as_mut()
                    && let Some((_, p)) = fields.iter_mut().find(|(n, _)| n == name)
                {
                    *p = IRPattern::Binding {
                        name: temp.clone(),
                        ty: None,
                    };
                }
            }
        }
    }
    IRPattern::Variant {
        enum_name: enum_name.clone(),
        variant: variant.clone(),
        sub_patterns,
        named_fields,
    }
}

fn catch_all_body_binding_outer(ca: &IRMatchArm, scrutinee_ty: Option<&Type>) -> IRBlock {
    match &ca.pattern {
        IRPattern::Binding { name, .. } => {
            let ty = scrutinee_ty
                .cloned()
                .unwrap_or_else(|| Type::Enum("Unknown".to_string()));
            let mut statements = vec![IRStmt::Let(IRLetStmt {
                name: name.clone(),
                ty,
                init: Some(IREexpr::Var(MATCH_PARENT_SCRUTINEE.to_string())),
            })];
            statements.extend(ca.body.statements.iter().cloned());
            IRBlock {
                name: ca.body.name.clone(),
                statements,
                defers: ca.body.defers.clone(),
            }
        }
        _ => ca.body.clone(),
    }
}

fn compile_nested_slots(
    group: &[IRMatchArm],
    catch_alls: &[IRMatchArm],
    temps: &[(NestedSlot, String, Type, String)],
    slot_idx: usize,
    ctx: &LoweringContext,
    next_temp: &mut usize,
    ctx_scrutinee_ty: Option<&Type>,
) -> IRBlock {
    if slot_idx >= temps.len() {
        if let Some(arm) = group.first() {
            return arm.body.clone();
        }
        return IRBlock {
            name: "match_arm".to_string(),
            statements: Vec::new(),
            defers: Vec::new(),
        };
    }

    let (slot, temp, payload_ty, inner_enum) = &temps[slot_idx];
    let mut order: Vec<String> = Vec::new();
    let mut groups: HashMap<String, Vec<IRMatchArm>> = HashMap::new();
    for arm in group {
        let pat = slot_pattern(&arm.pattern, slot)
            .cloned()
            .unwrap_or(IRPattern::Wildcard);
        let mut key = pattern_ctor_key(&pat);
        if arm.guard.is_some() {
            key.push_str(&format!("#g{}", order.len()));
        }
        if !groups.contains_key(&key) {
            order.push(key.clone());
        }
        groups.entry(key).or_default().push(arm.clone());
    }

    let mut inner_arms: Vec<IRMatchArm> = Vec::new();
    for key in order {
        let sub = groups.remove(&key).unwrap();
        let inner_pat = slot_pattern(&sub[0].pattern, slot)
            .cloned()
            .unwrap_or(IRPattern::Wildcard);
        let guard = if sub.len() == 1 {
            sub[0].guard.clone()
        } else {
            None
        };
        let body = compile_nested_slots(
            &sub,
            &[],
            temps,
            slot_idx + 1,
            ctx,
            next_temp,
            ctx_scrutinee_ty,
        );
        inner_arms.push(IRMatchArm {
            pattern: inner_pat,
            guard,
            body,
        });
    }
    if slot_idx == 0 {
        for ca in catch_alls {
            inner_arms.push(IRMatchArm {
                pattern: IRPattern::Wildcard,
                guard: ca.guard.clone(),
                body: catch_all_body_binding_outer(ca, ctx_scrutinee_ty),
            });
        }
    }

    let inner_arms =
        specialize_nested_match_arms(inner_arms, inner_enum, Some(payload_ty), ctx, next_temp);
    IRBlock {
        name: "match_arm".to_string(),
        statements: vec![IRStmt::Expr(IREexpr::Match {
            expr: Box::new(IREexpr::Var(temp.clone())),
            enum_type: inner_enum.clone(),
            scrutinee_type: Some(payload_ty.clone()),
            result_type: Type::Void,
            arms: inner_arms,
        })],
        defers: Vec::new(),
    }
}

fn specialize_constructor_group(
    group: Vec<IRMatchArm>,
    catch_alls: &[IRMatchArm],
    enum_type: &str,
    scrutinee_type: Option<&Type>,
    ctx: &LoweringContext,
    next_temp: &mut usize,
) -> IRMatchArm {
    let slots = nested_slots_for_group(&group);
    let variant = match &group[0].pattern {
        IRPattern::Variant { variant, .. } => variant.clone(),
        _ => String::new(),
    };
    let mut temps: Vec<(NestedSlot, String, Type, String)> = Vec::new();
    let mut slot_bindings: Vec<(NestedSlot, String)> = Vec::new();
    for slot in slots {
        let temp = format!("__ion_nested_{}", *next_temp);
        *next_temp += 1;
        let payload_ty = payload_type_for_slot(ctx, enum_type, &variant, &slot, scrutinee_type);
        let inner_enum = group
            .iter()
            .find_map(|arm| match slot_pattern(&arm.pattern, &slot) {
                Some(IRPattern::Variant { enum_name, .. }) => Some(enum_name.clone()),
                _ => None,
            })
            .or_else(|| enum_name_from_type(&payload_ty))
            .unwrap_or_else(|| "Unknown".to_string());
        slot_bindings.push((slot.clone(), temp.clone()));
        temps.push((slot, temp, payload_ty, inner_enum));
    }

    let outer_pattern = replace_nested_slots_with_bindings(&group[0].pattern, &slot_bindings);
    let body = compile_nested_slots(
        &group,
        catch_alls,
        &temps,
        0,
        ctx,
        next_temp,
        scrutinee_type,
    );
    IRMatchArm {
        pattern: outer_pattern,
        guard: None,
        body,
    }
}

fn specialize_nested_match_arms(
    arms: Vec<IRMatchArm>,
    enum_type: &str,
    scrutinee_type: Option<&Type>,
    ctx: &LoweringContext,
    next_temp: &mut usize,
) -> Vec<IRMatchArm> {
    if !arms
        .iter()
        .any(|a| ir_pattern_has_nested_variant(&a.pattern))
    {
        return arms;
    }

    let mut ctor_order: Vec<String> = Vec::new();
    let mut ctor_groups: HashMap<String, Vec<IRMatchArm>> = HashMap::new();
    let mut catch_alls: Vec<IRMatchArm> = Vec::new();
    for arm in arms {
        match &arm.pattern {
            IRPattern::Variant { variant, .. } => {
                if !ctor_groups.contains_key(variant) {
                    ctor_order.push(variant.clone());
                }
                ctor_groups.entry(variant.clone()).or_default().push(arm);
            }
            IRPattern::Wildcard
            | IRPattern::Binding { .. }
            | IRPattern::Lit { .. }
            | IRPattern::Range { .. }
            | IRPattern::Or { .. }
            | IRPattern::Struct { .. }
            | IRPattern::At { .. }
            | IRPattern::Rest => catch_alls.push(arm),
        }
    }

    let mut out = Vec::new();
    for variant in ctor_order {
        let group = ctor_groups.remove(&variant).unwrap();
        if group
            .iter()
            .any(|a| ir_pattern_has_nested_variant(&a.pattern))
        {
            out.push(specialize_constructor_group(
                group,
                &catch_alls,
                enum_type,
                scrutinee_type,
                ctx,
                next_temp,
            ));
        } else {
            out.extend(group);
        }
    }
    out.extend(catch_alls);
    out
}

fn record_variant_payload_bindings(
    variant: &str,
    sub_patterns: &[Pattern],
    named_fields: Option<&Vec<(String, Pattern)>>,
    scrutinee_ty: &Type,
    ctx: &mut LoweringContext,
) {
    let peeled = match scrutinee_ty {
        Type::Ref { inner, .. } => inner.as_ref(),
        other => other,
    };
    let Some(name) = enum_name_from_type(peeled) else {
        return;
    };
    let Some(decl) = ctx.enum_decls.get(&name).cloned() else {
        return;
    };
    let subst = generic_subst_from_scrutinee(&decl, peeled);
    let Some(vdecl) = decl.variants.iter().find(|v| v.name == variant).cloned() else {
        return;
    };
    if let Some(named) = named_fields {
        if let Some(fields) = &vdecl.named_fields {
            for (fname, sub) in named {
                if let Some((_, ty)) = fields.iter().find(|(n, _)| n == fname) {
                    let concrete = ctx.types.resolve(&substitute_type(ty, &subst));
                    record_match_arm_bindings(sub, &concrete, ctx);
                }
            }
        }
    } else {
        for (i, sub) in sub_patterns.iter().enumerate() {
            if let Some(ty) = vdecl.payload_types.get(i) {
                let concrete = ctx.types.resolve(&substitute_type(ty, &subst));
                record_match_arm_bindings(sub, &concrete, ctx);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct IRStructLitField {
    pub name: String,
    pub value: IREexpr,
}

pub struct IRBuilder;

impl IRBuilder {
    pub fn build(ast: &Program, types: &TypeInfo) -> IRProgram {
        let builder = IRBuilder;
        let mut function_returns = types.function_returns.clone();
        for f in &ast.functions {
            function_returns
                .entry(f.name.clone())
                .or_insert_with(|| f.return_type.clone());
        }
        let struct_decls: HashMap<String, StructDecl> = ast
            .structs
            .iter()
            .map(|s| (s.name.clone(), s.clone()))
            .collect();
        let enum_decls: HashMap<String, EnumDecl> = ast
            .enums
            .iter()
            .map(|e| (e.name.clone(), e.clone()))
            .collect();
        let enum_param_counts: HashMap<String, usize> = ast
            .enums
            .iter()
            .map(|e| (e.name.clone(), e.generics.len()))
            .collect();
        let functions = ast
            .functions
            .iter()
            .map(|f| {
                builder.build_function(
                    f,
                    &function_returns,
                    &struct_decls,
                    &enum_decls,
                    &enum_param_counts,
                    types,
                )
            })
            .collect();
        let mut program = IRProgram {
            structs: ast.structs.clone(),
            enums: ast.enums.clone(),
            functions,
            extern_blocks: ast.extern_blocks.clone(),
            type_aliases: ast.type_aliases.clone(),
            drop_impls: types.drop_impls.clone(),
        };
        monomorphize_generic_functions(&mut program);
        program
    }

    fn build_function(
        &self,
        function: &FnDecl,
        function_returns: &HashMap<String, Option<Type>>,
        struct_decls: &HashMap<String, StructDecl>,
        enum_decls: &HashMap<String, EnumDecl>,
        enum_param_counts: &HashMap<String, usize>,
        types: &TypeInfo,
    ) -> IRFunction {
        let params: Vec<IRParam> = function
            .params
            .iter()
            .map(|p| IRParam {
                name: p.name.clone(),
                ty: types.resolve(&p.ty),
            })
            .collect();

        // For minimal subset, we use a single basic block
        let fn_literal_counter = Rc::new(Cell::new(0));
        let mut ctx = LoweringContext::from_params(
            &params,
            struct_decls.clone(),
            enum_decls.clone(),
            enum_param_counts.clone(),
            fn_literal_counter,
            function_returns.clone(),
            types.clone(),
        );
        let entry = Self::lower_ast_block("entry", &function.body, &mut ctx);
        let blocks = vec![entry];

        IRFunction {
            name: function.name.clone(),
            generics: TypeParam::names(&function.generics),
            params,
            return_type: function.return_type.as_ref().map(|t| types.resolve(t)),
            blocks,
        }
    }

    fn lower_ast_block(name: &str, body: &Block, ctx: &mut LoweringContext) -> IRBlock {
        let mut statements = Vec::new();
        let mut defers = Vec::new();
        for stmt in &body.statements {
            Self::lower_stmt(stmt, &mut statements, &mut defers, ctx);
        }
        IRBlock {
            name: name.to_string(),
            statements,
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

                // General tuple destructuring: let (a, b) = expr;
                if let Some(ref patterns) = let_stmt.patterns
                    && let Some(ref init) = let_stmt.init
                {
                    let tuple_ty = if let Some(ref type_ann) = let_stmt.type_ann {
                        ctx.types.resolve(type_ann)
                    } else {
                        ctx.expr_type(init)
                    };
                    if let Type::Tuple { elements } = tuple_ty
                        && (patterns.len() == elements.len()
                            || patterns.iter().any(|p| matches!(p, Pattern::Rest { .. })))
                    {
                        let temp = format!("__ion_tuple_{}", ctx.tuple_temp_counter);
                        ctx.tuple_temp_counter += 1;
                        out.push(IRStmt::Let(IRLetStmt {
                            name: temp.clone(),
                            ty: Type::Tuple {
                                elements: elements.clone(),
                            },
                            init: Some(build_expr_with_ctx(init, ctx)),
                        }));
                        for (i, pattern) in patterns.iter().enumerate() {
                            if matches!(pattern, Pattern::Rest { .. }) {
                                continue;
                            }
                            let rest_at = patterns
                                .iter()
                                .position(|p| matches!(p, Pattern::Rest { .. }));
                            let elem_index = if let Some(rest_at) = rest_at {
                                let tail = patterns.len() - rest_at - 1;
                                if i < rest_at {
                                    i
                                } else {
                                    elements.len() - tail + (i - rest_at - 1)
                                }
                            } else {
                                i
                            };
                            if let Pattern::Binding { name, .. } = pattern {
                                out.push(IRStmt::Let(IRLetStmt {
                                    name: name.clone(),
                                    ty: elements[elem_index].clone(),
                                    init: Some(IREexpr::FieldAccess {
                                        base: Box::new(IREexpr::Var(temp.clone())),
                                        field: format!("f{elem_index}"),
                                        is_pointer: false,
                                        ty: elements[elem_index].clone(),
                                    }),
                                }));
                                ctx.record_binding(name, &elements[elem_index]);
                            }
                        }
                        return;
                    }
                }

                let ty = if let Some(ref type_ann) = let_stmt.type_ann {
                    ctx.types.resolve(type_ann)
                } else if let Some(ref init_expr) = let_stmt.init {
                    ctx.expr_type(init_expr)
                } else {
                    Type::Void
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
            Stmt::Break(_) => {
                out.push(IRStmt::Break);
            }
            Stmt::Continue(_) => {
                out.push(IRStmt::Continue);
            }
            Stmt::Expr(expr_stmt) => {
                out.push(IRStmt::Expr(build_expr_with_ctx(&expr_stmt.expr, ctx)));
            }
            Stmt::Defer(defer_stmt) => {
                let expr = build_expr_with_ctx(&defer_stmt.expr, ctx);
                defers.push(expr.clone());
                out.push(IRStmt::Defer(expr));
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

                let body = Self::lower_ast_block("spawn_body", &spawn_stmt.body, ctx);
                ctx.var_types = parent_vars;

                out.push(IRStmt::Spawn(IRSpawn {
                    captures,
                    body,
                    result: Type::Void,
                }));
            }
            Stmt::Select(select_stmt) => {
                let mut recv_arms = Vec::new();
                for arm in &select_stmt.recv_arms {
                    recv_arms.push(IRSelectRecvArm {
                        binding: arm.binding.clone(),
                        channel: build_expr_with_ctx(&arm.recv.channel, ctx),
                        elem_type: resolve_recv_elem_type(&arm.recv.channel, ctx),
                        body: Self::lower_ast_block("select_arm", &arm.body, ctx),
                    });
                }
                let default_body = select_stmt
                    .default_body
                    .as_ref()
                    .map(|b| Self::lower_ast_block("select_default", b, ctx));
                let timeout_ms = select_stmt
                    .timeout_ms
                    .as_ref()
                    .map(|e| build_expr_with_ctx(e, ctx));
                let timeout_body = select_stmt
                    .timeout_body
                    .as_ref()
                    .map(|b| Self::lower_ast_block("select_timeout", b, ctx));
                out.push(IRStmt::Select(IRSelect {
                    recv_arms,
                    default_body,
                    timeout_ms,
                    timeout_body,
                }));
            }
            Stmt::If(if_stmt) => {
                let cond = build_expr_with_ctx(&if_stmt.cond, ctx);
                let then_block = Self::lower_ast_block("then_block", &if_stmt.then_block, ctx);
                let else_block = if_stmt
                    .else_block
                    .as_ref()
                    .map(|b| Self::lower_ast_block("else_block", b, ctx));

                out.push(IRStmt::If(IRIf {
                    cond,
                    then_block,
                    else_block,
                }));
            }
            Stmt::While(while_stmt) => {
                let cond = build_expr_with_ctx(&while_stmt.cond, ctx);
                let body = Self::lower_ast_block("while_body", &while_stmt.body, ctx);
                out.push(IRStmt::While(IRWhile {
                    cond,
                    body,
                    step: None,
                    continue_label: None,
                }));
            }
            Stmt::Loop(loop_stmt) => {
                let body = Self::lower_ast_block("loop_body", &loop_stmt.body, ctx);
                out.push(IRStmt::While(IRWhile {
                    cond: IREexpr::BoolLiteral(true),
                    body,
                    step: None,
                    continue_label: None,
                }));
            }
            Stmt::For(for_stmt) => {
                let iter_ty = ctx.expr_type(&for_stmt.iterable);
                if let Some((symbol, elem)) = iter_call_for_type(&ctx.types, &iter_ty) {
                    lower_iter_for(for_stmt, ctx, iter_ty, symbol, elem, out);
                } else {
                    let container_var = format!("__for_container_{}", for_stmt.span.start);
                    let index_var = format!("__for_i_{}", for_stmt.span.start);

                    let container_ty = ctx.expr_type(&for_stmt.iterable);

                    let elem_type = match &container_ty {
                        Type::Vec { elem_type } => (**elem_type).clone(),
                        Type::String => Type::U8,
                        Type::Array { inner, .. } => (**inner).clone(),
                        _ => panic!(
                            "compiler bug: for-iterable type is not Vec/String/Array: {:?}",
                            container_ty
                        ),
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
                    let container_ref_ty = Type::Ref {
                        inner: Box::new(container_ty.clone()),
                        mutable: false,
                    };
                    let container_ref = if use_container_copy {
                        IREexpr::AddressOf {
                            inner: Box::new(IREexpr::Var(container_var.clone())),
                            mutable: false,
                            ty: container_ref_ty,
                        }
                    } else {
                        IREexpr::AddressOf {
                            inner: Box::new(iterable_expr.clone()),
                            mutable: false,
                            ty: Type::Ref {
                                inner: Box::new(container_ty.clone()),
                                mutable: false,
                            },
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
                            result_type: Type::Bool,
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
                            result_type: Type::Bool,
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
                            result_type: Type::Bool,
                        },
                        _ => IREexpr::BoolLiteral(false),
                    };

                    let mut while_body_stmts = Vec::new();
                    let mut while_defers = Vec::new();

                    let step_label = format!("__for_step_{}", for_stmt.span.start);
                    let step_stmt = IRStmt::Expr(IREexpr::Assign {
                        target: index_var.clone(),
                        value: Box::new(IREexpr::BinOp {
                            op: BinOp::Add,
                            left: Box::new(IREexpr::Var(index_var.clone())),
                            right: Box::new(IREexpr::Lit(1)),
                            result_type: Type::Int,
                        }),
                    });
                    let step_block = IRBlock {
                        name: "for_step".to_string(),
                        statements: vec![step_stmt],
                        defers: Vec::new(),
                    };

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
                            let mut match_defers = Vec::new();
                            ctx.record_binding(&for_stmt.var_name, &elem_type);
                            for inner in &for_stmt.body.statements {
                                Self::lower_stmt(
                                    inner,
                                    &mut match_body_stmts,
                                    &mut match_defers,
                                    ctx,
                                );
                            }
                            while_body_stmts.push(IRStmt::Expr(IREexpr::Match {
                                expr: Box::new(IREexpr::Var(opt_var)),
                                enum_type: "Option".to_string(),
                                scrutinee_type: None,
                                result_type: Type::Void,
                                arms: vec![
                                    IRMatchArm {
                                        pattern: IRPattern::Variant {
                                            enum_name: "Option".to_string(),
                                            variant: "Some".to_string(),
                                            sub_patterns: vec![IRPattern::Binding {
                                                name: for_stmt.var_name.clone(),
                                                ty: None,
                                            }],
                                            named_fields: None,
                                        },
                                        guard: None,
                                        body: IRBlock {
                                            name: "for_match_body".to_string(),
                                            statements: match_body_stmts,
                                            defers: match_defers,
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
                                            defers: Vec::new(),
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
                                Self::lower_stmt(
                                    inner,
                                    &mut while_body_stmts,
                                    &mut while_defers,
                                    ctx,
                                );
                            }
                        }
                        _ => {}
                    }

                    out.push(IRStmt::While(IRWhile {
                        cond,
                        body: IRBlock {
                            name: "for_while_body".to_string(),
                            statements: while_body_stmts,
                            defers: while_defers,
                        },
                        step: Some(step_block),
                        continue_label: Some(step_label),
                    }));
                }
            }
            Stmt::UnsafeBlock(unsafe_stmt) => {
                let body = Self::lower_ast_block("unsafe_body", &unsafe_stmt.body, ctx);
                out.push(IRStmt::UnsafeBlock(IRUnsafeBlock { body }));
            }
            Stmt::Scope(scope_stmt) => {
                let body = Self::lower_ast_block("scope_body", &scope_stmt.body, ctx);
                out.push(IRStmt::Scope(IRScope { body }));
            }
        }
    }
}

fn resolve_recv_elem_type(channel: &Expr, ctx: &LoweringContext) -> Type {
    let receiver_type = match channel {
        Expr::Ref(r) => ctx.expr_type(&r.inner),
        _ => ctx.expr_type(channel),
    };
    match receiver_type {
        Type::Receiver { elem_type } => (*elem_type).clone(),
        Type::Endpoint {
            protocol,
            step,
            dual,
        } => {
            let proto = ctx.types.protocols.get(&protocol).expect("protocol exists");
            let payload = match proto.steps.get(step) {
                Some(ProtocolStep::Send(ty) | ProtocolStep::Recv(ty)) => ty.clone(),
                None => Type::Void,
            };
            Type::Tuple {
                elements: vec![
                    payload,
                    Type::Endpoint {
                        protocol,
                        step: step + 1,
                        dual,
                    },
                ],
            }
        }
        other => panic!("compiler bug: recv channel is not Receiver: {:?}", other),
    }
}

fn iter_call_for_type(types: &TypeInfo, ty: &Type) -> Option<(String, Type)> {
    if matches!(ty, Type::Vec { .. } | Type::String | Type::Array { .. }) {
        return None;
    }
    let name = match ty {
        Type::Struct(name) | Type::Enum(name) => name.clone(),
        Type::Generic { name, .. } => name.clone(),
        _ => return None,
    };
    let info = types.iter_impls.get(&name)?;
    let concrete = match ty {
        Type::Generic { params, .. } => params.clone(),
        _ => Vec::new(),
    };
    if info.generics.len() != concrete.len() {
        return None;
    }
    let substitutions = info
        .generics
        .iter()
        .cloned()
        .zip(concrete)
        .collect::<HashMap<_, _>>();
    Some((
        info.symbol.clone(),
        crate::types_util::substitute_type(&info.element, &substitutions),
    ))
}

fn lower_iter_for(
    for_stmt: &ForStmt,
    ctx: &mut LoweringContext,
    iter_ty: Type,
    symbol: String,
    elem: Type,
    out: &mut Vec<IRStmt>,
) {
    let iter_var = format!("__for_iter_{}", for_stmt.span.start);
    let opt_var = format!("__for_opt_{}", for_stmt.span.start);
    ctx.record_binding(&iter_var, &iter_ty);
    out.push(IRStmt::Let(IRLetStmt {
        name: iter_var.clone(),
        ty: iter_ty.clone(),
        init: Some(build_expr_with_ctx(&for_stmt.iterable, ctx)),
    }));
    let opt_ty = Type::Generic {
        name: "Option".to_string(),
        params: vec![elem.clone()],
    };
    let next = IREexpr::Call {
        callee: symbol,
        args: vec![IREexpr::AddressOf {
            inner: Box::new(IREexpr::Var(iter_var)),
            mutable: true,
            ty: Type::Ref {
                inner: Box::new(iter_ty),
                mutable: true,
            },
        }],
        return_type: Some(opt_ty.clone()),
        tuple_destructure_index: None,
    };
    let mut body = Vec::new();
    body.push(IRStmt::Let(IRLetStmt {
        name: opt_var.clone(),
        ty: opt_ty,
        init: Some(next),
    }));
    let mut some_stmts = Vec::new();
    let mut some_defers = Vec::new();
    ctx.record_binding(&for_stmt.var_name, &elem);
    for inner in &for_stmt.body.statements {
        IRBuilder::lower_stmt(inner, &mut some_stmts, &mut some_defers, ctx);
    }
    body.push(IRStmt::Expr(IREexpr::Match {
        expr: Box::new(IREexpr::Var(opt_var)),
        enum_type: "Option".to_string(),
        scrutinee_type: None,
        result_type: Type::Void,
        arms: vec![
            IRMatchArm {
                pattern: IRPattern::Variant {
                    enum_name: "Option".to_string(),
                    variant: "Some".to_string(),
                    sub_patterns: vec![IRPattern::Binding {
                        name: for_stmt.var_name.clone(),
                        ty: None,
                    }],
                    named_fields: None,
                },
                guard: None,
                body: IRBlock {
                    name: "for_iter_some".to_string(),
                    statements: some_stmts,
                    defers: some_defers,
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
                    name: "for_iter_none".to_string(),
                    statements: vec![IRStmt::Break],
                    defers: Vec::new(),
                },
            },
        ],
    }));
    out.push(IRStmt::While(IRWhile {
        cond: IREexpr::BoolLiteral(true),
        body: IRBlock {
            name: "for_iter".to_string(),
            statements: body,
            defers: Vec::new(),
        },
        step: None,
        continue_label: None,
    }));
}

fn lower_enum_try(try_expr: &TryExpr, ctx: &LoweringContext, operand_ty: &Type) -> IREexpr {
    let id = try_expr.id.0;
    let (enum_name, concrete) = match operand_ty {
        Type::Enum(name) => (name.clone(), Vec::new()),
        Type::Generic { name, params } => (name.clone(), params.clone()),
        other => panic!("compiler bug: ? lowering expected an enum, got {:?}", other),
    };
    let Some(decl) = ctx.enum_decls.get(&enum_name).cloned() else {
        panic!("compiler bug: ? lowering missing enum '{enum_name}'");
    };
    let Some((success, payload)) = crate::tc::enum_try_success(&decl, &concrete) else {
        panic!("compiler bug: ? lowering enum '{enum_name}' has no success variant");
    };
    let ok_name = format!("__ion_try_ok_{id}");
    let mut arms = Vec::new();
    for variant in &decl.variants {
        if variant.name == success {
            arms.push(IRMatchArm {
                pattern: IRPattern::Variant {
                    enum_name: enum_name.clone(),
                    variant: variant.name.clone(),
                    sub_patterns: vec![IRPattern::Binding {
                        name: ok_name.clone(),
                        ty: None,
                    }],
                    named_fields: None,
                },
                guard: None,
                body: IRBlock {
                    name: "try_ok".to_string(),
                    statements: vec![IRStmt::Expr(IREexpr::Var(ok_name.clone()))],
                    defers: Vec::new(),
                },
            });
            continue;
        }
        let mut sub_patterns = Vec::new();
        let mut args = Vec::new();
        for (index, _) in variant.payload_types.iter().enumerate() {
            let binding = format!("__ion_try_e_{id}_{}_{index}", variant.name);
            sub_patterns.push(IRPattern::Binding {
                name: binding.clone(),
                ty: None,
            });
            args.push(IREexpr::Var(binding));
        }
        arms.push(IRMatchArm {
            pattern: IRPattern::Variant {
                enum_name: enum_name.clone(),
                variant: variant.name.clone(),
                sub_patterns,
                named_fields: None,
            },
            guard: None,
            body: IRBlock {
                name: "try_err".to_string(),
                statements: vec![IRStmt::Return(IRReturn {
                    value: Some(IREexpr::EnumLit {
                        enum_name: enum_name.clone(),
                        variant: variant.name.clone(),
                        args,
                        named_fields: None,
                        ty: operand_ty.clone(),
                    }),
                })],
                defers: Vec::new(),
            },
        });
    }
    IREexpr::Match {
        expr: Box::new(build_expr_with_ctx(&try_expr.operand, ctx)),
        enum_type: enum_name,
        scrutinee_type: Some(operand_ty.clone()),
        result_type: payload,
        arms,
    }
}

fn lower_try_expr(try_expr: &TryExpr, ctx: &LoweringContext) -> IREexpr {
    let operand_ty = ctx.expr_type(&try_expr.operand);
    let id = try_expr.id.0;
    let ok_name = format!("__ion_try_ok_{id}");
    let err_name = format!("__ion_try_err_{id}");
    let (enum_name, success_variant, error_variant, error_has_payload) = match &operand_ty {
        Type::Generic { name, .. } if name == "Option" => (
            "Option".to_string(),
            "Some".to_string(),
            "None".to_string(),
            false,
        ),
        Type::Generic { name, .. } if name == "Result" => (
            "Result".to_string(),
            "Ok".to_string(),
            "Err".to_string(),
            true,
        ),
        other => {
            return lower_enum_try(try_expr, ctx, other);
        }
    };

    let success_arm = IRMatchArm {
        pattern: IRPattern::Variant {
            enum_name: enum_name.clone(),
            variant: success_variant,
            sub_patterns: vec![IRPattern::Binding {
                name: ok_name.clone(),
                ty: None,
            }],
            named_fields: None,
        },
        guard: None,
        body: IRBlock {
            name: "try_ok".to_string(),
            statements: vec![IRStmt::Expr(IREexpr::Var(ok_name))],
            defers: Vec::new(),
        },
    };

    let error_return = if error_has_payload {
        IREexpr::EnumLit {
            enum_name: enum_name.clone(),
            variant: error_variant.clone(),
            args: vec![IREexpr::Var(err_name.clone())],
            named_fields: None,
            ty: operand_ty.clone(),
        }
    } else {
        IREexpr::EnumLit {
            enum_name: enum_name.clone(),
            variant: error_variant.clone(),
            args: Vec::new(),
            named_fields: None,
            ty: operand_ty.clone(),
        }
    };
    let error_arm = IRMatchArm {
        pattern: IRPattern::Variant {
            enum_name: enum_name.clone(),
            variant: error_variant,
            sub_patterns: if error_has_payload {
                vec![IRPattern::Binding {
                    name: err_name,
                    ty: None,
                }]
            } else {
                Vec::new()
            },
            named_fields: None,
        },
        guard: None,
        body: IRBlock {
            name: "try_err".to_string(),
            statements: vec![IRStmt::Return(IRReturn {
                value: Some(error_return),
            })],
            defers: Vec::new(),
        },
    };

    let result_type = match &operand_ty {
        Type::Generic { params, .. } if !params.is_empty() => params[0].clone(),
        other => other.clone(),
    };
    IREexpr::Match {
        expr: Box::new(build_expr_with_ctx(&try_expr.operand, ctx)),
        enum_type: enum_name,
        scrutinee_type: Some(operand_ty),
        result_type,
        arms: vec![success_arm, error_arm],
    }
}

/// Field access of a non-Copy field through `&` or `&mut` is a reborrow.
/// Assignment stores the owned field, so the place type is that inner type.
fn owned_field_place_type(ty: Type) -> Type {
    match ty {
        Type::Ref {
            inner,
            mutable: true,
        } => *inner,
        other => other,
    }
}

fn build_expr_with_ctx(expr: &Expr, ctx: &LoweringContext) -> IREexpr {
    match expr {
        Expr::Lit(lit_expr) => IREexpr::Lit(lit_expr.value),
        Expr::BoolLiteral(bool_expr) => IREexpr::BoolLiteral(bool_expr.value),
        Expr::FloatLiteral(float_expr) => IREexpr::FloatLiteral(float_expr.value),
        Expr::TypeConst(type_const) => {
            let ty = crate::integer_limits::resolve_integer_limit(
                &type_const.type_name,
                &type_const.member,
            )
            .unwrap_or(Type::Int);
            IREexpr::IntLimit {
                ty,
                max: type_const.member == "MAX",
            }
        }
        Expr::Var(var_expr) => IREexpr::Var(var_expr.name.clone()),
        Expr::Ref(ref_expr) => {
            // Reference expression: &x or &mut x
            IREexpr::AddressOf {
                inner: Box::new(build_expr_with_ctx(&ref_expr.inner, ctx)),
                mutable: ref_expr.mutable,
                ty: ctx.expr_type(expr),
            }
        }
        Expr::BinOp(bin_op_expr) => IREexpr::BinOp {
            op: bin_op_expr.op,
            left: Box::new(build_expr_with_ctx(&bin_op_expr.left, ctx)),
            right: Box::new(build_expr_with_ctx(&bin_op_expr.right, ctx)),
            result_type: ctx.expr_type(expr),
        },
        Expr::UnOp(un_op_expr) => IREexpr::UnOp {
            op: un_op_expr.op,
            operand: Box::new(build_expr_with_ctx(&un_op_expr.operand, ctx)),
            result_type: ctx.expr_type(expr),
        },
        Expr::Send(send_expr) => {
            let value_type = ctx.expr_type(&send_expr.value);
            IREexpr::Send {
                channel: Box::new(build_expr_with_ctx(&send_expr.channel, ctx)),
                value: Box::new(build_expr_with_ctx(&send_expr.value, ctx)),
                value_type,
            }
        }
        Expr::Recv(recv_expr) => IREexpr::Recv {
            channel: Box::new(build_expr_with_ctx(&recv_expr.channel, ctx)),
            elem_type: resolve_recv_elem_type(&recv_expr.channel, ctx),
        },
        Expr::Spawn(spawn_expr) => {
            let captured_names = collect_captured_vars(&spawn_expr.body);
            let mut captures = Vec::new();
            for name in captured_names {
                if let Some(ty) = ctx.var_types.get(&name) {
                    captures.push((name, ty.clone()));
                }
            }
            let mut spawn_ctx = ctx.clone();
            spawn_ctx.var_types.clear();
            for (name, ty) in &captures {
                spawn_ctx.record_binding(name, ty);
            }
            let body = IRBuilder::lower_ast_block("spawn_body", &spawn_expr.body, &mut spawn_ctx);
            let result = match ctx.expr_type(expr) {
                Type::JoinHandle { result } => *result,
                _ => Type::Void,
            };
            IREexpr::Spawn {
                captures,
                body,
                result,
            }
        }
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
        Expr::FieldAccess(acc) => {
            let base_ty = ctx.resolve_expr_type(&acc.base);
            let is_pointer = matches!(
                base_ty,
                Some(Type::Ref {
                    inner,
                    ..
                }) if matches!(
                    *inner,
                    Type::Struct(_) | Type::Generic { .. } | Type::Tuple { .. }
                )
            );
            let field = if let Some(Type::Tuple { elements }) = ctx.resolve_expr_type(&acc.base) {
                if let Ok(idx) = acc.field.parse::<usize>() {
                    if idx < elements.len() {
                        format!("f{}", idx)
                    } else {
                        acc.field.clone()
                    }
                } else {
                    acc.field.clone()
                }
            } else {
                acc.field.clone()
            };
            IREexpr::FieldAccess {
                base: Box::new(build_expr_with_ctx(&acc.base, ctx)),
                field,
                is_pointer,
                ty: ctx.expr_type(expr),
            }
        }
        Expr::EnumLit(enum_lit) => {
            if let Some(callee) = ctx.types.call_callees.get(&enum_lit.id).cloned() {
                return IREexpr::Call {
                    callee,
                    args: enum_lit
                        .args
                        .iter()
                        .map(|a| build_expr_with_ctx(a, ctx))
                        .collect(),
                    return_type: Some(ctx.expr_type(expr)),
                    tuple_destructure_index: None,
                };
            }
            IREexpr::EnumLit {
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
                ty: ctx.expr_type(expr),
            }
        }
        Expr::Match(match_expr) => {
            let scrutinee_ty = match_scrutinee_type(&match_expr.expr, ctx);
            let arms = match_expr
                .arms
                .iter()
                .flat_map(|arm| {
                    expand_or_patterns(&arm.pattern).into_iter().map(|pattern| {
                        let mut body_stmts = Vec::new();
                        let mut arm_defers = Vec::new();
                        let mut arm_ctx = LoweringContext {
                            var_types: ctx.var_types.clone(),
                            struct_decls: ctx.struct_decls.clone(),
                            enum_decls: ctx.enum_decls.clone(),
                            enum_param_counts: ctx.enum_param_counts.clone(),
                            tuple_temp_counter: ctx.tuple_temp_counter,
                            fn_literal_counter: ctx.fn_literal_counter.clone(),
                            function_returns: ctx.function_returns.clone(),
                            types: ctx.types.clone(),
                        };
                        if let Some(ref ty) = scrutinee_ty {
                            record_match_arm_bindings(&pattern, ty, &mut arm_ctx);
                        }
                        for stmt in &arm.body.statements {
                            IRBuilder::lower_stmt(
                                stmt,
                                &mut body_stmts,
                                &mut arm_defers,
                                &mut arm_ctx,
                            );
                        }
                        IRMatchArm {
                            pattern: pattern_to_ir(&pattern),
                            guard: arm.guard.as_ref().map(|g| build_expr_with_ctx(g, ctx)),
                            body: IRBlock {
                                name: "match_arm".to_string(),
                                statements: body_stmts,
                                defers: arm_defers,
                            },
                        }
                    })
                })
                .collect();
            let enum_name = match_expr
                .arms
                .iter()
                .find_map(|arm| match &arm.pattern {
                    Pattern::Variant { enum_name, .. } => Some(enum_name.clone()),
                    _ => None,
                })
                .or_else(|| infer_match_enum_name(&match_expr.expr, ctx))
                .unwrap_or_else(|| "Unknown".to_string());

            let scrutinee_type = ctx.resolve_expr_type(&match_expr.expr);
            let mut next_temp = 0;
            let mut arms = specialize_nested_match_arms(
                arms,
                &enum_name,
                scrutinee_type.as_ref(),
                ctx,
                &mut next_temp,
            );
            if let Some(ty) = scrutinee_type.as_ref() {
                for arm in &mut arms {
                    assign_pattern_binding_types(&mut arm.pattern, ty, ctx);
                }
            }
            IREexpr::Match {
                expr: Box::new(build_expr_with_ctx(&match_expr.expr, ctx)),
                enum_type: enum_name,
                scrutinee_type,
                result_type: ctx.expr_type(expr),
                arms,
            }
        }
        Expr::Try(try_expr) => lower_try_expr(try_expr, ctx),
        Expr::Call(call_expr) => {
            let return_type = Some(ctx.expr_type(expr));
            let callee = ctx
                .types
                .call_callees
                .get(&call_expr.id)
                .cloned()
                .unwrap_or_else(|| call_expr.callee.clone());
            IREexpr::Call {
                callee,
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
            let resolved = ctx
                .types
                .resolved_methods
                .get(&method_call.id)
                .cloned()
                .unwrap_or_else(|| {
                    panic!(
                        "compiler bug: missing resolved method '{}' at line {}",
                        method_call.method, method_call.span.line
                    )
                });
            let receiver_ty = ctx.expr_type(&method_call.receiver);
            let receiver_expr = if resolved.take_address {
                IREexpr::AddressOf {
                    ty: Type::Ref {
                        inner: Box::new(receiver_ty),
                        mutable: resolved.address_mutable,
                    },
                    inner: Box::new(build_expr_with_ctx(&method_call.receiver, ctx)),
                    mutable: resolved.address_mutable,
                }
            } else {
                build_expr_with_ctx(&method_call.receiver, ctx)
            };
            let callee = resolved.callee;
            let method_args: Vec<IREexpr> = method_call
                .args
                .iter()
                .map(|a| build_expr_with_ctx(a, ctx))
                .collect();
            let mut all_args = vec![receiver_expr];
            all_args.extend(method_args);
            IREexpr::Call {
                callee,
                args: all_args,
                return_type: Some(ctx.expr_type(expr)),
                tuple_destructure_index: None,
            }
        }
        Expr::StringLit(string_lit) => IREexpr::StringLit(string_lit.value.clone()),
        Expr::TupleLit(tuple_lit) => {
            let elem_types: Vec<Type> = tuple_lit
                .elements
                .iter()
                .map(|e| ctx.expr_type(e))
                .collect();
            IREexpr::TupleLit {
                elements: tuple_lit
                    .elements
                    .iter()
                    .map(|e| build_expr_with_ctx(e, ctx))
                    .collect(),
                elem_types,
            }
        }
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
                target_type: ctx.resolve_expr_type(&index_expr.target),
            },
            Expr::FieldAccess(_) => IREexpr::AssignField {
                field_ty: owned_field_place_type(ctx.expr_type(&assign_expr.target)),
                target: Box::new(build_expr_with_ctx(&assign_expr.target, ctx)),
                value: Box::new(build_expr_with_ctx(&assign_expr.value, ctx)),
            },
            _ => panic!("Invalid assignment target in IR lowering"),
        },
        Expr::FnLiteral(lit) => {
            let lit_ty = ctx.expr_type(expr);
            let closure = match &lit_ty {
                Type::Struct(name) => ctx.types.closures.get(name).cloned(),
                _ => None,
            };
            let user_params: Vec<IRParam> = lit
                .params
                .iter()
                .map(|p| IRParam {
                    name: p.name.clone(),
                    ty: p.ty.clone(),
                })
                .collect();
            if let Some(sig) = closure {
                let struct_name = match &lit_ty {
                    Type::Struct(name) => name.clone(),
                    _ => String::new(),
                };
                let env_ty = Type::Ref {
                    inner: Box::new(Type::Struct(struct_name.clone())),
                    mutable: true,
                };
                let mut lit_params = vec![IRParam {
                    name: "env".to_string(),
                    ty: env_ty,
                }];
                lit_params.extend(user_params);
                let mut lit_ctx = LoweringContext::from_params(
                    &lit_params,
                    ctx.struct_decls.clone(),
                    ctx.enum_decls.clone(),
                    ctx.enum_param_counts.clone(),
                    ctx.fn_literal_counter.clone(),
                    ctx.function_returns.clone(),
                    ctx.types.clone(),
                );
                for (name, cap_ty) in &sig.captures {
                    lit_ctx.record_binding(name, cap_ty);
                }
                let mut body = IRBuilder::lower_ast_block("fn_lit_body", &lit.body, &mut lit_ctx);
                let capture_names: HashMap<String, Type> = sig.captures.iter().cloned().collect();
                rewrite_closure_body(&mut body, &capture_names);
                IREexpr::FnLiteral(IRFnLiteral {
                    symbol: sig.symbol,
                    params: lit_params,
                    return_type: lit.return_type.clone(),
                    body,
                    captures: sig.captures,
                    env_struct: Some(struct_name),
                })
            } else {
                let id = ctx.fn_literal_counter.get();
                ctx.fn_literal_counter.set(id + 1);
                let symbol = format!("ion_fn_lit_{}", id);
                let mut lit_ctx = LoweringContext::from_params(
                    &user_params,
                    ctx.struct_decls.clone(),
                    ctx.enum_decls.clone(),
                    ctx.enum_param_counts.clone(),
                    ctx.fn_literal_counter.clone(),
                    ctx.function_returns.clone(),
                    ctx.types.clone(),
                );
                let body = IRBuilder::lower_ast_block("fn_lit_body", &lit.body, &mut lit_ctx);
                IREexpr::FnLiteral(IRFnLiteral {
                    symbol,
                    params: user_params,
                    return_type: lit.return_type.clone(),
                    body,
                    captures: Vec::new(),
                    env_struct: None,
                })
            }
        }
    }
}

fn rewrite_closure_body(block: &mut IRBlock, captures: &HashMap<String, Type>) {
    for stmt in &mut block.statements {
        rewrite_closure_stmt(stmt, captures);
    }
    for defer in &mut block.defers {
        rewrite_closure_expr(defer, captures);
    }
}

fn rewrite_closure_stmt(stmt: &mut IRStmt, captures: &HashMap<String, Type>) {
    match stmt {
        IRStmt::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.init {
                rewrite_closure_expr(init, captures);
            }
        }
        IRStmt::Return(ret) => {
            if let Some(value) = &mut ret.value {
                rewrite_closure_expr(value, captures);
            }
        }
        IRStmt::Expr(expr) | IRStmt::Defer(expr) => rewrite_closure_expr(expr, captures),
        IRStmt::If(ir_if) => {
            rewrite_closure_expr(&mut ir_if.cond, captures);
            rewrite_closure_body(&mut ir_if.then_block, captures);
            if let Some(else_block) = &mut ir_if.else_block {
                rewrite_closure_body(else_block, captures);
            }
        }
        IRStmt::While(ir_while) => {
            rewrite_closure_expr(&mut ir_while.cond, captures);
            rewrite_closure_body(&mut ir_while.body, captures);
            if let Some(step) = &mut ir_while.step {
                rewrite_closure_body(step, captures);
            }
        }
        IRStmt::UnsafeBlock(block) => rewrite_closure_body(&mut block.body, captures),
        IRStmt::Scope(block) => rewrite_closure_body(&mut block.body, captures),
        IRStmt::Spawn(spawn) => rewrite_closure_body(&mut spawn.body, captures),
        IRStmt::Select(sel) => {
            for arm in &mut sel.recv_arms {
                rewrite_closure_expr(&mut arm.channel, captures);
                rewrite_closure_body(&mut arm.body, captures);
            }
            if let Some(body) = &mut sel.default_body {
                rewrite_closure_body(body, captures);
            }
            if let Some(ms) = &mut sel.timeout_ms {
                rewrite_closure_expr(ms, captures);
            }
            if let Some(body) = &mut sel.timeout_body {
                rewrite_closure_body(body, captures);
            }
        }
        IRStmt::Break | IRStmt::Continue => {}
    }
}

fn rewrite_closure_expr(expr: &mut IREexpr, captures: &HashMap<String, Type>) {
    if let IREexpr::Var(name) = expr
        && let Some(ty) = captures.get(name)
    {
        *expr = IREexpr::FieldAccess {
            base: Box::new(IREexpr::Var("env".to_string())),
            field: name.clone(),
            is_pointer: true,
            ty: ty.clone(),
        };
        return;
    }
    match expr {
        IREexpr::AddressOf { inner, .. }
        | IREexpr::UnOp { operand: inner, .. }
        | IREexpr::Recv { channel: inner, .. }
        | IREexpr::Cast { expr: inner, .. } => rewrite_closure_expr(inner, captures),
        IREexpr::BinOp { left, right, .. } => {
            rewrite_closure_expr(left, captures);
            rewrite_closure_expr(right, captures);
        }
        IREexpr::Send { channel, value, .. } => {
            rewrite_closure_expr(channel, captures);
            rewrite_closure_expr(value, captures);
        }
        IREexpr::FieldAccess { base, .. } => rewrite_closure_expr(base, captures),
        IREexpr::Index { target, index, .. } => {
            rewrite_closure_expr(target, captures);
            rewrite_closure_expr(index, captures);
        }
        IREexpr::Assign { value, .. } => rewrite_closure_expr(value, captures),
        IREexpr::AssignIndex {
            target,
            index,
            value,
            ..
        } => {
            rewrite_closure_expr(target, captures);
            rewrite_closure_expr(index, captures);
            rewrite_closure_expr(value, captures);
        }
        IREexpr::AssignField { target, value, .. } => {
            rewrite_closure_expr(target, captures);
            rewrite_closure_expr(value, captures);
        }
        IREexpr::Call { args, .. } => {
            for arg in args {
                rewrite_closure_expr(arg, captures);
            }
        }
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                rewrite_closure_expr(&mut field.value, captures);
            }
        }
        IREexpr::EnumLit {
            args, named_fields, ..
        } => {
            for arg in args {
                rewrite_closure_expr(arg, captures);
            }
            if let Some(fields) = named_fields {
                for (_, value) in fields {
                    rewrite_closure_expr(value, captures);
                }
            }
        }
        IREexpr::TupleLit { elements, .. } => {
            for elem in elements {
                rewrite_closure_expr(elem, captures);
            }
        }
        IREexpr::ArrayLiteral { elements, repeat } => {
            for elem in elements {
                rewrite_closure_expr(elem, captures);
            }
            if let Some((value, _)) = repeat {
                rewrite_closure_expr(value, captures);
            }
        }
        IREexpr::Match { expr, arms, .. } => {
            rewrite_closure_expr(expr, captures);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    rewrite_closure_expr(guard, captures);
                }
                rewrite_closure_body(&mut arm.body, captures);
            }
        }
        IREexpr::Spawn { body, .. } => rewrite_closure_body(body, captures),
        IREexpr::FnLiteral(lit) => rewrite_closure_body(&mut lit.body, captures),
        IREexpr::Lit(_)
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::IntLimit { .. }
        | IREexpr::Var(_)
        | IREexpr::StringLit(_) => {}
    }
}

fn vec_elem_type_from_arg_expr(arg: &Expr, ctx: &LoweringContext) -> Option<Type> {
    let ty = match arg {
        Expr::Ref(r) => ctx.resolve_expr_type(&r.inner)?,
        _ => ctx.resolve_expr_type(arg)?,
    };
    ref_to_vec_elem(&ty).cloned()
}

fn slice_elem_type_from_arg_expr(arg: &Expr, ctx: &LoweringContext) -> Option<Type> {
    let ty = match arg {
        Expr::Ref(r) => ctx.resolve_expr_type(&r.inner)?,
        _ => ctx.resolve_expr_type(arg)?,
    };
    slice_elem_type(&ty)
}

fn builtin_option_vec_return(callee: &str, args: &[Expr], ctx: &LoweringContext) -> Option<Type> {
    if callee == "Vec::get_ref" {
        let elem = vec_elem_type_from_arg_expr(args.first()?, ctx)?;
        return Some(Type::Generic {
            name: "Option".to_string(),
            params: vec![Type::Ref {
                inner: Box::new(elem),
                mutable: false,
            }],
        });
    }
    if callee == "Arena::get_ref" {
        let ty = match args.first()? {
            Expr::Ref(r) => ctx.resolve_expr_type(&r.inner)?,
            _ => ctx.resolve_expr_type(args.first()?)?,
        };
        let inner = match ty {
            Type::Ref { inner, .. } => *inner,
            other => other,
        };
        let elem = match inner {
            Type::Generic { name, params } if name == "Arena" && params.len() == 1 => {
                params[0].clone()
            }
            _ => return None,
        };
        return Some(Type::Generic {
            name: "Option".to_string(),
            params: vec![Type::Ref {
                inner: Box::new(elem),
                mutable: false,
            }],
        });
    }
    if callee == "Slice::get_ref" {
        let elem = slice_elem_type_from_arg_expr(args.first()?, ctx)?;
        return Some(Type::Generic {
            name: "Option".to_string(),
            params: vec![Type::Ref {
                inner: Box::new(elem),
                mutable: false,
            }],
        });
    }
    if callee == "String::get" {
        return Some(Type::Generic {
            name: "Option".to_string(),
            params: vec![Type::U8],
        });
    }
    if callee != "Vec::get" && callee != "Vec::pop" {
        return None;
    }
    let elem = vec_elem_type_from_arg_expr(args.first()?, ctx)?;
    Some(Type::Generic {
        name: "Option".to_string(),
        params: vec![elem],
    })
}

/// Monomorphize generic functions at each call site and drop unresolved templates.
fn fn_type_from_ir_function(func: &IRFunction) -> Type {
    Type::Fn {
        params: func.params.iter().map(|p| p.ty.clone()).collect(),
        return_type: Box::new(func.return_type.clone().unwrap_or(Type::Void)),
    }
}

fn build_function_fn_types(functions: &[IRFunction]) -> HashMap<String, Type> {
    functions
        .iter()
        .map(|f| (f.name.clone(), fn_type_from_ir_function(f)))
        .collect()
}

/// Merged stdlib functions are stored as `handle_new` while call sites keep `handle::new`.
fn lookup_merged<'a, V>(map: &'a HashMap<String, V>, name: &str) -> Option<&'a V> {
    map.get(name).or_else(|| {
        if name.contains("::") {
            map.get(&name.replace("::", "_"))
        } else {
            None
        }
    })
}

fn apply_generic_instantiation(
    callee: &mut String,
    return_type: &mut Option<Type>,
    template: &IRFunction,
    subs: &HashMap<String, Type>,
    instantiations: &mut HashMap<String, (IRFunction, HashMap<String, Type>)>,
) {
    let mangled = mangle_function_name(&template.name, &template.generics, subs);
    instantiations
        .entry(mangled.clone())
        .or_insert_with(|| (template.clone(), subs.clone()));
    *callee = mangled;
    if let Some(ret) = return_type.as_mut() {
        *ret = substitute_type(ret, subs);
    } else if let Some(template_ret) = &template.return_type {
        *return_type = Some(substitute_type(template_ret, subs));
    }
}

struct GenericRewrite<'a> {
    generic_defs: &'a HashMap<String, IRFunction>,
    function_fn_types: &'a HashMap<String, Type>,
    instantiations: &'a mut HashMap<String, (IRFunction, HashMap<String, Type>)>,
    structs: &'a [StructDecl],
    enums: &'a [EnumDecl],
    drop_impls: &'a HashMap<String, String>,
}

fn bind_ir_pattern_vars(
    pattern: &IRPattern,
    ty: &Type,
    structs: &[StructDecl],
    enums: &[EnumDecl],
    drop_impls: &HashMap<String, String>,
    vars: &mut HashMap<String, Type>,
) {
    let original = ty.clone();
    let through_mut = matches!(ty, Type::Ref { mutable: true, .. });
    let through_ref = matches!(ty, Type::Ref { .. });
    let ty = peel_type_ref(ty);
    let struct_map: HashMap<String, StructDecl> = structs
        .iter()
        .map(|decl| (decl.name.clone(), decl.clone()))
        .collect();
    let enum_map: HashMap<String, EnumDecl> = enums
        .iter()
        .map(|decl| (decl.name.clone(), decl.clone()))
        .collect();
    let bind_ty = |field_ty: &Type| -> Type {
        if through_ref && !crate::tc::type_is_copy(field_ty, &struct_map, &enum_map, drop_impls) {
            Type::Ref {
                inner: Box::new(field_ty.clone()),
                mutable: through_mut,
            }
        } else {
            field_ty.clone()
        }
    };
    // A pattern that binds a reference value keeps the pointer. Field
    // projections passed in already peeled still copy `Copy` fields.
    let bound_value = if matches!(original, Type::Ref { .. }) {
        original
    } else {
        bind_ty(&ty)
    };
    match pattern {
        IRPattern::Binding { name, .. } => {
            vars.insert(name.clone(), bound_value);
        }
        IRPattern::At { name, pattern } => {
            vars.insert(name.clone(), bound_value.clone());
            bind_ir_pattern_vars(pattern, &bound_value, structs, enums, drop_impls, vars);
        }
        IRPattern::Struct { name, fields, .. } => {
            let decl = structs.iter().find(|decl| decl.name == *name);
            let subst = decl.map(|decl| struct_subst(decl, &ty)).unwrap_or_default();
            for (field_name, field_pattern) in fields {
                let Some(field) = decl
                    .and_then(|decl| decl.fields.iter().find(|field| field.name == *field_name))
                else {
                    panic!("compiler bug: struct '{name}' has no field '{field_name}'");
                };
                let field_ty = substitute_type(&field.ty, &subst);
                bind_ir_pattern_vars(
                    field_pattern,
                    &bind_ty(&field_ty),
                    structs,
                    enums,
                    drop_impls,
                    vars,
                );
            }
        }
        IRPattern::Variant {
            enum_name,
            variant,
            sub_patterns,
            named_fields,
        } => {
            let decl = enums
                .iter()
                .find(|decl| decl.name == *enum_name)
                .unwrap_or_else(|| {
                    panic!("compiler bug: enum pattern '{enum_name}' has no enum declaration")
                });
            let subst = generic_subst_from_scrutinee(decl, &ty);
            let variant_decl = decl
                .variants
                .iter()
                .find(|item| item.name == *variant)
                .unwrap_or_else(|| {
                    panic!("compiler bug: enum '{enum_name}' has no variant '{variant}'")
                });
            if let Some(named) = named_fields {
                let fields = variant_decl.named_fields.as_ref().unwrap_or_else(|| {
                    panic!("compiler bug: enum '{enum_name}::{variant}' has no named fields")
                });
                for (field_name, field_pattern) in named {
                    if field_name == ".." {
                        continue;
                    }
                    let (_, field_ty) = fields.iter().find(|(fname, _)| fname == field_name).unwrap_or_else(|| {
                        panic!("compiler bug: enum '{enum_name}::{variant}' has no field '{field_name}'")
                    });
                    let concrete = substitute_type(field_ty, &subst);
                    bind_ir_pattern_vars(
                        field_pattern,
                        &bind_ty(&concrete),
                        structs,
                        enums,
                        drop_impls,
                        vars,
                    );
                }
            } else {
                for (index, sub_pattern) in sub_patterns.iter().enumerate() {
                    if matches!(sub_pattern, IRPattern::Rest) {
                        continue;
                    }
                    let field_ty = variant_decl.payload_types.get(index).unwrap_or_else(|| {
                        panic!("compiler bug: enum '{enum_name}::{variant}' has no payload {index}")
                    });
                    let concrete = substitute_type(field_ty, &subst);
                    bind_ir_pattern_vars(
                        sub_pattern,
                        &bind_ty(&concrete),
                        structs,
                        enums,
                        drop_impls,
                        vars,
                    );
                }
            }
        }
        IRPattern::Or { alts } => {
            if let Some(alt) = alts.first() {
                bind_ir_pattern_vars(alt, &bind_ty(&ty), structs, enums, drop_impls, vars);
            }
        }
        IRPattern::Lit { .. } | IRPattern::Range { .. } | IRPattern::Wildcard | IRPattern::Rest => {
        }
    }
}

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

    let function_fn_types = build_function_fn_types(&program.functions);
    let mut instantiations: HashMap<String, (IRFunction, HashMap<String, Type>)> = HashMap::new();
    let mut ctx = GenericRewrite {
        generic_defs: &generic_defs,
        function_fn_types: &function_fn_types,
        instantiations: &mut instantiations,
        structs: &program.structs,
        enums: &program.enums,
        drop_impls: &program.drop_impls,
    };

    for func in &mut program.functions {
        if !func.generics.is_empty() {
            continue;
        }
        let mut var_types: HashMap<String, Type> = HashMap::new();
        for param in &func.params {
            var_types.insert(param.name.clone(), param.ty.clone());
        }
        for block in &mut func.blocks {
            rewrite_generic_calls_in_block(block, &mut ctx, &mut var_types);
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
    resolve_capability_callees(
        &mut functions,
        &program.structs,
        &program.enums,
        &program.drop_impls,
    );
    append_drop_instantiations(&generic_defs, &mut functions);

    let mut pending = functions;
    for _ in 0..8 {
        let generic_defs: HashMap<String, IRFunction> = program
            .functions
            .iter()
            .filter(|f| !f.generics.is_empty())
            .map(|f| (f.name.clone(), f.clone()))
            .collect();
        let function_fn_types = build_function_fn_types(&program.functions);
        let mut instantiations: HashMap<String, (IRFunction, HashMap<String, Type>)> =
            HashMap::new();
        let mut ctx = GenericRewrite {
            generic_defs: &generic_defs,
            function_fn_types: &function_fn_types,
            instantiations: &mut instantiations,
            structs: &program.structs,
            enums: &program.enums,
            drop_impls: &program.drop_impls,
        };
        for func in &mut pending {
            let mut var_types: HashMap<String, Type> = HashMap::new();
            for param in &func.params {
                var_types.insert(param.name.clone(), param.ty.clone());
            }
            for block in &mut func.blocks {
                rewrite_generic_calls_in_block(block, &mut ctx, &mut var_types);
            }
        }
        if instantiations.is_empty() {
            program.functions = pending;
            return;
        }
        let more: Vec<IRFunction> = instantiations
            .into_values()
            .map(|(template, subs)| instantiate_generic_function(&template, &subs))
            .filter(|func| !pending.iter().any(|existing| existing.name == func.name))
            .collect();
        resolve_capability_callees(
            &mut pending,
            &program.structs,
            &program.enums,
            &program.drop_impls,
        );
        let mut more = more;
        resolve_capability_callees(
            &mut more,
            &program.structs,
            &program.enums,
            &program.drop_impls,
        );
        pending.extend(more);
    }
    panic!("compiler bug: generic instantiation did not finish within 8 rounds");
}

fn append_drop_instantiations(
    generic_defs: &HashMap<String, IRFunction>,
    functions: &mut Vec<IRFunction>,
) {
    let templates: Vec<IRFunction> = generic_defs
        .values()
        .filter(|func| func.name.ends_with("_Drop_drop") && !func.generics.is_empty())
        .cloned()
        .collect();
    if templates.is_empty() {
        return;
    }
    let mut types = Vec::new();
    for func in functions.iter() {
        collect_drop_types_fn(func, &mut types);
    }
    let mut extra: Vec<IRFunction> = Vec::new();
    for ty in &types {
        let Type::Generic { name, params } = ty else {
            continue;
        };
        let stem = format!("{name}_Drop_drop");
        let Some(template) = templates.iter().find(|func| func.name == stem) else {
            continue;
        };
        if template.generics.len() != params.len() {
            continue;
        }
        let subs: HashMap<String, Type> = template
            .generics
            .iter()
            .cloned()
            .zip(params.iter().cloned())
            .collect();
        let inst = instantiate_generic_function(template, &subs);
        if functions.iter().any(|func| func.name == inst.name)
            || extra.iter().any(|func| func.name == inst.name)
        {
            continue;
        }
        extra.push(inst);
    }
    functions.extend(extra);
}

fn collect_drop_types_fn(func: &IRFunction, out: &mut Vec<Type>) {
    if let Some(ret) = &func.return_type {
        out.push(ret.clone());
    }
    for param in &func.params {
        out.push(param.ty.clone());
    }
    for block in &func.blocks {
        collect_drop_types_block(block, out);
    }
}

fn collect_drop_types_block(block: &IRBlock, out: &mut Vec<Type>) {
    for stmt in &block.statements {
        match stmt {
            IRStmt::Let(let_stmt) => out.push(let_stmt.ty.clone()),
            IRStmt::If(ir_if) => {
                collect_drop_types_block(&ir_if.then_block, out);
                if let Some(else_block) = &ir_if.else_block {
                    collect_drop_types_block(else_block, out);
                }
            }
            IRStmt::While(ir_while) => {
                collect_drop_types_block(&ir_while.body, out);
                if let Some(step) = &ir_while.step {
                    collect_drop_types_block(step, out);
                }
            }
            IRStmt::UnsafeBlock(block) => collect_drop_types_block(&block.body, out),
            IRStmt::Scope(block) => collect_drop_types_block(&block.body, out),
            IRStmt::Spawn(spawn) => collect_drop_types_block(&spawn.body, out),
            IRStmt::Select(sel) => {
                for arm in &sel.recv_arms {
                    collect_drop_types_block(&arm.body, out);
                }
                if let Some(body) = &sel.default_body {
                    collect_drop_types_block(body, out);
                }
                if let Some(body) = &sel.timeout_body {
                    collect_drop_types_block(body, out);
                }
            }
            IRStmt::Return(_)
            | IRStmt::Break
            | IRStmt::Continue
            | IRStmt::Expr(_)
            | IRStmt::Defer(_) => {}
        }
    }
}

struct TypeLayouts<'a> {
    structs: &'a [StructDecl],
    enums: &'a [EnumDecl],
    drop_impls: &'a HashMap<String, String>,
}

fn resolve_capability_callees(
    functions: &mut [IRFunction],
    structs: &[StructDecl],
    enums: &[EnumDecl],
    drop_impls: &HashMap<String, String>,
) {
    let layouts = TypeLayouts {
        structs,
        enums,
        drop_impls,
    };
    for func in functions {
        let mut vars: HashMap<String, Type> = HashMap::new();
        for param in &func.params {
            vars.insert(param.name.clone(), param.ty.clone());
        }
        for block in &mut func.blocks {
            resolve_capability_block(&layouts, block, &mut vars);
        }
    }
}

fn resolve_capability_block(
    layouts: &TypeLayouts<'_>,
    block: &mut IRBlock,
    vars: &mut HashMap<String, Type>,
) {
    for stmt in &mut block.statements {
        match stmt {
            IRStmt::Let(let_stmt) => {
                if let Some(init) = &mut let_stmt.init {
                    resolve_capability_expr(layouts, init, vars);
                }
                vars.insert(let_stmt.name.clone(), let_stmt.ty.clone());
            }
            IRStmt::Return(ret) => {
                if let Some(value) = &mut ret.value {
                    resolve_capability_expr(layouts, value, vars);
                }
            }
            IRStmt::Expr(expr) | IRStmt::Defer(expr) => {
                resolve_capability_expr(layouts, expr, vars)
            }
            IRStmt::If(ir_if) => {
                resolve_capability_expr(layouts, &mut ir_if.cond, vars);
                resolve_capability_block(layouts, &mut ir_if.then_block, vars);
                if let Some(else_block) = &mut ir_if.else_block {
                    resolve_capability_block(layouts, else_block, vars);
                }
            }
            IRStmt::While(ir_while) => {
                resolve_capability_expr(layouts, &mut ir_while.cond, vars);
                resolve_capability_block(layouts, &mut ir_while.body, vars);
                if let Some(step) = &mut ir_while.step {
                    resolve_capability_block(layouts, step, vars);
                }
            }
            IRStmt::UnsafeBlock(block) => resolve_capability_block(layouts, &mut block.body, vars),
            IRStmt::Scope(block) => resolve_capability_block(layouts, &mut block.body, vars),
            IRStmt::Spawn(spawn) => {
                let mut spawn_vars = vars.clone();
                for (name, ty) in &spawn.captures {
                    spawn_vars.insert(name.clone(), ty.clone());
                }
                resolve_capability_block(layouts, &mut spawn.body, &mut spawn_vars);
            }
            IRStmt::Select(sel) => {
                for arm in &mut sel.recv_arms {
                    resolve_capability_expr(layouts, &mut arm.channel, vars);
                    resolve_capability_block(layouts, &mut arm.body, vars);
                }
                if let Some(body) = &mut sel.default_body {
                    resolve_capability_block(layouts, body, vars);
                }
                if let Some(ms) = &mut sel.timeout_ms {
                    resolve_capability_expr(layouts, ms, vars);
                }
                if let Some(body) = &mut sel.timeout_body {
                    resolve_capability_block(layouts, body, vars);
                }
            }
            IRStmt::Break | IRStmt::Continue => {}
        }
    }
}

fn resolve_capability_expr(
    layouts: &TypeLayouts<'_>,
    expr: &mut IREexpr,
    vars: &HashMap<String, Type>,
) {
    match expr {
        IREexpr::Call { callee, args, .. } => {
            for arg in args.iter_mut() {
                resolve_capability_expr(layouts, arg, vars);
            }
            if let Some(rest) = callee.strip_prefix("CAP::")
                && let Some((cap, method)) = rest.split_once("::")
            {
                let base = args
                    .first()
                    .and_then(|arg| capability_receiver_base(arg, vars));
                let Some(base) = base else {
                    panic!(
                        "compiler bug: capability call '{callee}' has no receiver type after monomorphization"
                    );
                };
                *callee = if cap == "Hash"
                    && method == "hash"
                    && let Some(symbol) = crate::integer_limits::builtin_hash_symbol(&base)
                {
                    symbol.to_string()
                } else {
                    format!("{base}_{cap}_{method}")
                };
            }
        }
        IREexpr::AddressOf { inner, .. } => resolve_capability_expr(layouts, inner, vars),
        IREexpr::BinOp { left, right, .. } => {
            resolve_capability_expr(layouts, left, vars);
            resolve_capability_expr(layouts, right, vars);
        }
        IREexpr::UnOp { operand, .. } => resolve_capability_expr(layouts, operand, vars),
        IREexpr::Send { channel, value, .. } => {
            resolve_capability_expr(layouts, channel, vars);
            resolve_capability_expr(layouts, value, vars);
        }
        IREexpr::Recv { channel, .. } => resolve_capability_expr(layouts, channel, vars),
        IREexpr::Spawn { body, captures, .. } => {
            let mut spawn_vars = vars.clone();
            for (name, ty) in captures {
                spawn_vars.insert(name.clone(), ty.clone());
            }
            resolve_capability_block(layouts, body, &mut spawn_vars);
        }
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                resolve_capability_expr(layouts, &mut field.value, vars);
            }
        }
        IREexpr::FieldAccess { base, .. } => resolve_capability_expr(layouts, base, vars),
        IREexpr::EnumLit {
            args, named_fields, ..
        } => {
            for arg in args {
                resolve_capability_expr(layouts, arg, vars);
            }
            if let Some(fields) = named_fields {
                for (_, value) in fields {
                    resolve_capability_expr(layouts, value, vars);
                }
            }
        }
        IREexpr::Match {
            expr,
            arms,
            scrutinee_type,
            ..
        } => {
            resolve_capability_expr(layouts, expr, vars);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    resolve_capability_expr(layouts, guard, vars);
                }
                let mut arm_vars = vars.clone();
                if let Some(ty) = scrutinee_type {
                    bind_ir_pattern_vars(
                        &arm.pattern,
                        ty,
                        layouts.structs,
                        layouts.enums,
                        layouts.drop_impls,
                        &mut arm_vars,
                    );
                }
                resolve_capability_block(layouts, &mut arm.body, &mut arm_vars);
            }
        }
        IREexpr::TupleLit { elements, .. } | IREexpr::ArrayLiteral { elements, .. } => {
            for element in elements {
                resolve_capability_expr(layouts, element, vars);
            }
        }
        IREexpr::Index { target, index, .. } => {
            resolve_capability_expr(layouts, target, vars);
            resolve_capability_expr(layouts, index, vars);
        }
        IREexpr::Assign { value, .. } => resolve_capability_expr(layouts, value, vars),
        IREexpr::AssignIndex {
            target,
            index,
            value,
            ..
        } => {
            resolve_capability_expr(layouts, target, vars);
            resolve_capability_expr(layouts, index, vars);
            resolve_capability_expr(layouts, value, vars);
        }
        IREexpr::AssignField { target, value, .. } => {
            resolve_capability_expr(layouts, target, vars);
            resolve_capability_expr(layouts, value, vars);
        }
        IREexpr::Cast { expr, .. } => resolve_capability_expr(layouts, expr, vars),
        IREexpr::FnLiteral(lit) => {
            resolve_capability_block(layouts, &mut lit.body, &mut vars.clone())
        }
        IREexpr::Lit(_)
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::IntLimit { .. }
        | IREexpr::Var(_)
        | IREexpr::StringLit(_) => {}
    }
}

fn capability_receiver_base(expr: &IREexpr, vars: &HashMap<String, Type>) -> Option<String> {
    let ty = match expr {
        IREexpr::AddressOf { ty, .. } => ty.clone(),
        IREexpr::Var(name) => vars.get(name)?.clone(),
        IREexpr::Call {
            return_type: Some(ty),
            ..
        } => ty.clone(),
        _ => return None,
    };
    let mut owned = ty;
    while let Type::Ref { inner, .. } = owned {
        owned = *inner;
    }
    if let Some(name) = crate::integer_limits::builtin_hash_type_name(&owned) {
        return Some(name.to_string());
    }
    match owned {
        Type::Struct(name) | Type::Enum(name) => Some(name),
        Type::Generic { name, .. } => Some(name),
        _ => None,
    }
}

fn rewrite_generic_calls_in_block(
    block: &mut IRBlock,
    ctx: &mut GenericRewrite<'_>,
    var_types: &mut HashMap<String, Type>,
) {
    for defer_expr in &mut block.defers {
        rewrite_generic_calls_in_expr(defer_expr, ctx, var_types);
    }
    for stmt in &mut block.statements {
        match stmt {
            IRStmt::Let(let_stmt) => {
                if let Some(ref mut init) = let_stmt.init {
                    rewrite_generic_calls_in_expr(init, ctx, var_types);
                    rewrite_zero_arg_generic_call(init, &let_stmt.ty, ctx);
                }
                var_types.insert(let_stmt.name.clone(), let_stmt.ty.clone());
            }
            IRStmt::Return(ret) => {
                if let Some(ref mut value) = ret.value {
                    rewrite_generic_calls_in_expr(value, ctx, var_types);
                }
            }
            IRStmt::Break | IRStmt::Continue => {}
            IRStmt::Expr(expr) => {
                rewrite_generic_calls_in_expr(expr, ctx, var_types);
            }
            IRStmt::If(ir_if) => {
                rewrite_generic_calls_in_expr(&mut ir_if.cond, ctx, var_types);
                rewrite_generic_calls_in_block(&mut ir_if.then_block, ctx, var_types);
                if let Some(ref mut else_block) = ir_if.else_block {
                    rewrite_generic_calls_in_block(else_block, ctx, var_types);
                }
            }
            IRStmt::While(ir_while) => {
                rewrite_generic_calls_in_expr(&mut ir_while.cond, ctx, var_types);
                rewrite_generic_calls_in_block(&mut ir_while.body, ctx, var_types);
                if let Some(ref mut step) = ir_while.step {
                    rewrite_generic_calls_in_block(step, ctx, var_types);
                }
            }
            IRStmt::UnsafeBlock(unsafe_block) => {
                rewrite_generic_calls_in_block(&mut unsafe_block.body, ctx, var_types);
            }
            IRStmt::Scope(scope) => {
                rewrite_generic_calls_in_block(&mut scope.body, ctx, var_types);
            }
            IRStmt::Defer(expr) => {
                rewrite_generic_calls_in_expr(expr, ctx, var_types);
            }
            IRStmt::Spawn(spawn) => {
                let mut spawn_vars = var_types.clone();
                for (name, ty) in &spawn.captures {
                    spawn_vars.insert(name.clone(), ty.clone());
                }
                rewrite_generic_calls_in_block(&mut spawn.body, ctx, &mut spawn_vars);
            }
            IRStmt::Select(sel) => {
                for arm in &mut sel.recv_arms {
                    rewrite_generic_calls_in_expr(&mut arm.channel, ctx, var_types);
                    rewrite_generic_calls_in_block(&mut arm.body, ctx, var_types);
                }
                if let Some(body) = &mut sel.default_body {
                    rewrite_generic_calls_in_block(body, ctx, var_types);
                }
                if let Some(ms) = &mut sel.timeout_ms {
                    rewrite_generic_calls_in_expr(ms, ctx, var_types);
                }
                if let Some(body) = &mut sel.timeout_body {
                    rewrite_generic_calls_in_block(body, ctx, var_types);
                }
            }
        }
    }
}

fn rewrite_generic_calls_in_expr(
    expr: &mut IREexpr,
    ctx: &mut GenericRewrite<'_>,
    var_types: &HashMap<String, Type>,
) {
    match expr {
        IREexpr::Call {
            callee,
            args,
            return_type,
            ..
        } => {
            for arg in args.iter_mut() {
                rewrite_generic_calls_in_expr(arg, ctx, var_types);
            }
            if let Some(template) = lookup_merged(ctx.generic_defs, callee).cloned()
                && !args.is_empty()
                && !template.params.is_empty()
            {
                let mut subs = HashMap::new();
                for (arg, param) in args.iter().zip(template.params.iter()) {
                    if let Some(arg_ty) = stored_ir_expr_type(arg, ctx, var_types) {
                        subs.extend(infer_generic_substitutions(
                            &param.ty,
                            &arg_ty,
                            &template.generics,
                        ));
                    }
                }
                if subs.len() == template.generics.len() {
                    apply_generic_instantiation(
                        callee,
                        return_type,
                        &template,
                        &subs,
                        ctx.instantiations,
                    );
                }
            }
        }
        IREexpr::AddressOf { inner, .. } => {
            rewrite_generic_calls_in_expr(inner, ctx, var_types);
        }
        IREexpr::BinOp { left, right, .. } => {
            rewrite_generic_calls_in_expr(left, ctx, var_types);
            rewrite_generic_calls_in_expr(right, ctx, var_types);
        }
        IREexpr::UnOp { operand, .. } => {
            rewrite_generic_calls_in_expr(operand, ctx, var_types);
        }
        IREexpr::Send { channel, value, .. } => {
            rewrite_generic_calls_in_expr(channel, ctx, var_types);
            rewrite_generic_calls_in_expr(value, ctx, var_types);
        }
        IREexpr::Recv { channel, .. } => {
            rewrite_generic_calls_in_expr(channel, ctx, var_types);
        }
        IREexpr::Spawn { captures, body, .. } => {
            let mut spawn_vars = var_types.clone();
            for (name, ty) in captures.iter() {
                spawn_vars.insert(name.clone(), ty.clone());
            }
            rewrite_generic_calls_in_block(body, ctx, &mut spawn_vars);
        }
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                rewrite_generic_calls_in_expr(&mut field.value, ctx, var_types);
            }
        }
        IREexpr::FieldAccess { base, .. } => {
            rewrite_generic_calls_in_expr(base, ctx, var_types);
        }
        IREexpr::EnumLit {
            args, named_fields, ..
        } => {
            for arg in args {
                rewrite_generic_calls_in_expr(arg, ctx, var_types);
            }
            if let Some(fields) = named_fields {
                for (_, value) in fields {
                    rewrite_generic_calls_in_expr(value, ctx, var_types);
                }
            }
        }
        IREexpr::Match {
            expr: inner,
            scrutinee_type,
            arms,
            ..
        } => {
            rewrite_generic_calls_in_expr(inner, ctx, var_types);
            if let IREexpr::Call {
                return_type: Some(ret),
                ..
            } = inner.as_ref()
            {
                *scrutinee_type = Some(ret.clone());
            }
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    rewrite_generic_calls_in_expr(guard, ctx, var_types);
                }
                let mut arm_vars = var_types.clone();
                if let Some(ty) = scrutinee_type.as_ref() {
                    bind_ir_pattern_vars(
                        &arm.pattern,
                        ty,
                        ctx.structs,
                        ctx.enums,
                        ctx.drop_impls,
                        &mut arm_vars,
                    );
                }
                rewrite_generic_calls_in_block(&mut arm.body, ctx, &mut arm_vars);
            }
        }
        IREexpr::TupleLit { elements, .. } => {
            for element in elements {
                rewrite_generic_calls_in_expr(element, ctx, var_types);
            }
        }
        IREexpr::ArrayLiteral { elements, repeat } => {
            for element in elements {
                rewrite_generic_calls_in_expr(element, ctx, var_types);
            }
            if let Some((value, _)) = repeat {
                rewrite_generic_calls_in_expr(value, ctx, var_types);
            }
        }
        IREexpr::Index { target, index, .. } => {
            rewrite_generic_calls_in_expr(target, ctx, var_types);
            rewrite_generic_calls_in_expr(index, ctx, var_types);
        }
        IREexpr::Assign { value, .. } => {
            rewrite_generic_calls_in_expr(value, ctx, var_types);
        }
        IREexpr::AssignIndex {
            target,
            index,
            value,
            ..
        } => {
            rewrite_generic_calls_in_expr(target, ctx, var_types);
            rewrite_generic_calls_in_expr(index, ctx, var_types);
            rewrite_generic_calls_in_expr(value, ctx, var_types);
        }
        IREexpr::AssignField { target, value, .. } => {
            rewrite_generic_calls_in_expr(target, ctx, var_types);
            rewrite_generic_calls_in_expr(value, ctx, var_types);
        }
        IREexpr::Cast { expr: inner, .. } => {
            rewrite_generic_calls_in_expr(inner, ctx, var_types);
        }
        IREexpr::FnLiteral(lit) => {
            let mut lit_vars = var_types.clone();
            rewrite_generic_calls_in_block(&mut lit.body, ctx, &mut lit_vars);
        }
        IREexpr::Lit(_)
        | IREexpr::IntLimit { .. }
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::Var(_)
        | IREexpr::StringLit(_) => {}
    }
}

pub(crate) fn ir_expr_stored_type(expr: &IREexpr) -> Option<Type> {
    match expr {
        IREexpr::Var(_) => None,
        IREexpr::Lit(_) => Some(Type::Int),
        IREexpr::BoolLiteral(_) => Some(Type::Bool),
        IREexpr::FloatLiteral(_) => Some(Type::F64),
        IREexpr::IntLimit { ty, .. } => Some(ty.clone()),
        IREexpr::StringLit(_) => Some(Type::String),
        IREexpr::FieldAccess { ty, .. } => Some(ty.clone()),
        IREexpr::AddressOf { ty, .. } => Some(ty.clone()),
        IREexpr::BinOp { result_type, .. } | IREexpr::UnOp { result_type, .. } => {
            Some(result_type.clone())
        }
        IREexpr::Call { return_type, .. } => return_type.clone(),
        IREexpr::TupleLit { elem_types, .. } => Some(Type::Tuple {
            elements: elem_types.clone(),
        }),
        IREexpr::Spawn { result, .. } => Some(Type::JoinHandle {
            result: Box::new(result.clone()),
        }),
        IREexpr::Cast { target_type, .. } => Some(target_type.clone()),
        IREexpr::EnumLit { ty, .. } => Some(ty.clone()),
        IREexpr::Match { result_type, .. } => Some(result_type.clone()),
        IREexpr::Index { target_type, .. } => target_type.clone(),
        IREexpr::FnLiteral(lit) => Some(Type::Fn {
            params: lit.params.iter().map(|p| p.ty.clone()).collect(),
            return_type: Box::new(lit.return_type.clone().unwrap_or(Type::Void)),
        }),
        IREexpr::Send { .. }
        | IREexpr::Recv { .. }
        | IREexpr::StructLit { .. }
        | IREexpr::ArrayLiteral { .. }
        | IREexpr::Assign { .. }
        | IREexpr::AssignIndex { .. }
        | IREexpr::AssignField { .. } => None,
    }
}

fn stored_ir_expr_type(
    expr: &IREexpr,
    ctx: &GenericRewrite<'_>,
    var_types: &HashMap<String, Type>,
) -> Option<Type> {
    if let IREexpr::Var(name) = expr {
        return var_types
            .get(name)
            .cloned()
            .or_else(|| ctx.function_fn_types.get(name).cloned());
    }
    // Index stores the container type. Using it as the value type would infer the wrong parameter.
    if matches!(expr, IREexpr::Index { .. }) {
        return None;
    }
    match ir_expr_stored_type(expr) {
        Some(Type::Tuple { elements }) if elements.is_empty() => None,
        other => other,
    }
}

fn rewrite_zero_arg_generic_call(
    expr: &mut IREexpr,
    expected_ty: &Type,
    ctx: &mut GenericRewrite<'_>,
) {
    let IREexpr::Call {
        callee,
        args,
        return_type,
        ..
    } = expr
    else {
        return;
    };
    if !args.is_empty() {
        return;
    }
    let Some(template) = lookup_merged(ctx.generic_defs, callee).cloned() else {
        return;
    };
    let Some(template_ret) = template.return_type.clone() else {
        return;
    };
    let subs = infer_generic_substitutions(&template_ret, expected_ty, &template.generics);
    if subs.len() != template.generics.len() {
        return;
    }
    apply_generic_instantiation(callee, return_type, &template, &subs, ctx.instantiations);
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
        Type::Void => "void".to_string(),
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
        Type::String | Type::Str => "str".to_string(),
        Type::JoinHandle { result } => {
            if matches!(result.as_ref(), Type::Void) {
                "JoinHandle".to_string()
            } else {
                format!("JoinHandle_{}", type_name_for_mangle(result))
            }
        }
        Type::File => "File".to_string(),
        Type::Allocator => "Allocator".to_string(),
        Type::Endpoint {
            protocol,
            step,
            dual,
        } => {
            format!("Endpoint_{protocol}_{step}_{}", if *dual { 1 } else { 0 })
        }
        Type::Struct(name) | Type::Enum(name) => name.clone(),
        Type::Generic { name, params } => mangle_type_name(name, params),
        Type::Box { inner } => mangle_type_name("Box", std::slice::from_ref(inner)),
        Type::Vec { elem_type } => mangle_type_name("Vec", std::slice::from_ref(elem_type)),
        Type::Ref { inner, .. } => format!("ref_{}", type_name_for_mangle(inner)),
        Type::RawPtr { inner } => format!("ptr_{}", type_name_for_mangle(inner)),
        Type::Array { inner, size, .. } => format!("{}_{}", type_name_for_mangle(inner), size),
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
        Type::Fn {
            params,
            return_type,
        } => {
            let param_parts: Vec<String> = params.iter().map(type_name_for_mangle).collect();
            format!(
                "fn_{}_ret_{}",
                param_parts.join("_"),
                type_name_for_mangle(return_type)
            )
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
    IRFunction {
        name,
        generics: Vec::new(),
        params,
        return_type,
        blocks,
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
        defers: block
            .defers
            .iter()
            .map(|expr| substitute_types_in_expr(expr, substitutions))
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
        IRStmt::Break => IRStmt::Break,
        IRStmt::Continue => IRStmt::Continue,
        IRStmt::Expr(expr) => IRStmt::Expr(substitute_types_in_expr(expr, substitutions)),
        IRStmt::Defer(expr) => IRStmt::Defer(substitute_types_in_expr(expr, substitutions)),
        IRStmt::Spawn(spawn) => IRStmt::Spawn(IRSpawn {
            captures: spawn
                .captures
                .iter()
                .map(|(name, ty)| (name.clone(), substitute_type(ty, substitutions)))
                .collect(),
            body: substitute_types_in_block(&spawn.body, substitutions),
            result: substitute_type(&spawn.result, substitutions),
        }),
        IRStmt::Select(sel) => IRStmt::Select(IRSelect {
            recv_arms: sel
                .recv_arms
                .iter()
                .map(|arm| IRSelectRecvArm {
                    binding: arm.binding.clone(),
                    channel: substitute_types_in_expr(&arm.channel, substitutions),
                    elem_type: substitute_type(&arm.elem_type, substitutions),
                    body: substitute_types_in_block(&arm.body, substitutions),
                })
                .collect(),
            default_body: sel
                .default_body
                .as_ref()
                .map(|b| substitute_types_in_block(b, substitutions)),
            timeout_ms: sel
                .timeout_ms
                .as_ref()
                .map(|e| substitute_types_in_expr(e, substitutions)),
            timeout_body: sel
                .timeout_body
                .as_ref()
                .map(|b| substitute_types_in_block(b, substitutions)),
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
            step: ir_while
                .step
                .as_ref()
                .map(|block| substitute_types_in_block(block, substitutions)),
            continue_label: ir_while.continue_label.clone(),
        }),
        IRStmt::UnsafeBlock(unsafe_block) => IRStmt::UnsafeBlock(IRUnsafeBlock {
            body: substitute_types_in_block(&unsafe_block.body, substitutions),
        }),
        IRStmt::Scope(scope) => IRStmt::Scope(IRScope {
            body: substitute_types_in_block(&scope.body, substitutions),
        }),
    }
}

fn substitute_pattern(pattern: &IRPattern, substitutions: &HashMap<String, Type>) -> IRPattern {
    match pattern {
        IRPattern::Binding { name, ty } => IRPattern::Binding {
            name: name.clone(),
            ty: ty.as_ref().map(|ty| substitute_type(ty, substitutions)),
        },
        IRPattern::Variant {
            enum_name,
            variant,
            sub_patterns,
            named_fields,
        } => IRPattern::Variant {
            enum_name: enum_name.clone(),
            variant: variant.clone(),
            sub_patterns: sub_patterns
                .iter()
                .map(|pattern| substitute_pattern(pattern, substitutions))
                .collect(),
            named_fields: named_fields.as_ref().map(|fields| {
                fields
                    .iter()
                    .map(|(name, pattern)| {
                        (name.clone(), substitute_pattern(pattern, substitutions))
                    })
                    .collect()
            }),
        },
        IRPattern::At { name, pattern } => IRPattern::At {
            name: name.clone(),
            pattern: Box::new(substitute_pattern(pattern, substitutions)),
        },
        IRPattern::Or { alts } => IRPattern::Or {
            alts: alts
                .iter()
                .map(|pattern| substitute_pattern(pattern, substitutions))
                .collect(),
        },
        IRPattern::Struct { name, fields, rest } => IRPattern::Struct {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|(name, pattern)| (name.clone(), substitute_pattern(pattern, substitutions)))
                .collect(),
            rest: *rest,
        },
        other => other.clone(),
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
        IREexpr::Spawn {
            captures,
            body,
            result,
        } => IREexpr::Spawn {
            captures: captures
                .iter()
                .map(|(name, ty)| (name.clone(), substitute_type(ty, substitutions)))
                .collect(),
            body: substitute_types_in_block(body, substitutions),
            result: substitute_type(result, substitutions),
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
        IREexpr::AddressOf { inner, mutable, ty } => IREexpr::AddressOf {
            inner: Box::new(substitute_types_in_expr(inner, substitutions)),
            mutable: *mutable,
            ty: substitute_type(ty, substitutions),
        },
        IREexpr::BinOp {
            op,
            left,
            right,
            result_type,
        } => IREexpr::BinOp {
            op: *op,
            left: Box::new(substitute_types_in_expr(left, substitutions)),
            right: Box::new(substitute_types_in_expr(right, substitutions)),
            result_type: substitute_type(result_type, substitutions),
        },
        IREexpr::UnOp {
            op,
            operand,
            result_type,
        } => IREexpr::UnOp {
            op: *op,
            operand: Box::new(substitute_types_in_expr(operand, substitutions)),
            result_type: substitute_type(result_type, substitutions),
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
            ty,
        } => IREexpr::FieldAccess {
            base: Box::new(substitute_types_in_expr(base, substitutions)),
            field: field.clone(),
            is_pointer: *is_pointer,
            ty: substitute_type(ty, substitutions),
        },
        IREexpr::EnumLit {
            enum_name,
            variant,
            args,
            named_fields,
            ty,
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
            ty: substitute_type(ty, substitutions),
        },
        IREexpr::Match {
            expr: inner,
            enum_type,
            scrutinee_type,
            result_type,
            arms,
        } => IREexpr::Match {
            expr: Box::new(substitute_types_in_expr(inner, substitutions)),
            enum_type: enum_type.clone(),
            scrutinee_type: scrutinee_type
                .as_ref()
                .map(|ty| substitute_type(ty, substitutions)),
            result_type: substitute_type(result_type, substitutions),
            arms: arms
                .iter()
                .map(|arm| IRMatchArm {
                    pattern: substitute_pattern(&arm.pattern, substitutions),
                    guard: arm
                        .guard
                        .as_ref()
                        .map(|g| substitute_types_in_expr(g, substitutions)),
                    body: substitute_types_in_block(&arm.body, substitutions),
                })
                .collect(),
        },
        IREexpr::TupleLit {
            elements,
            elem_types,
        } => IREexpr::TupleLit {
            elements: elements
                .iter()
                .map(|element| substitute_types_in_expr(element, substitutions))
                .collect(),
            elem_types: elem_types
                .iter()
                .map(|ty| substitute_type(ty, substitutions))
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
            target_type,
        } => IREexpr::AssignIndex {
            target: Box::new(substitute_types_in_expr(target, substitutions)),
            index: Box::new(substitute_types_in_expr(index, substitutions)),
            value: Box::new(substitute_types_in_expr(value, substitutions)),
            target_type: target_type
                .as_ref()
                .map(|ty| substitute_type(ty, substitutions)),
        },
        IREexpr::AssignField {
            target,
            value,
            field_ty,
        } => IREexpr::AssignField {
            target: Box::new(substitute_types_in_expr(target, substitutions)),
            value: Box::new(substitute_types_in_expr(value, substitutions)),
            field_ty: substitute_type(field_ty, substitutions),
        },
        IREexpr::Lit(v) => IREexpr::Lit(*v),
        IREexpr::IntLimit { ty, max } => IREexpr::IntLimit {
            ty: substitute_type(ty, substitutions),
            max: *max,
        },
        IREexpr::BoolLiteral(v) => IREexpr::BoolLiteral(*v),
        IREexpr::FloatLiteral(v) => IREexpr::FloatLiteral(*v),
        IREexpr::Var(v) => IREexpr::Var(v.clone()),
        IREexpr::StringLit(v) => IREexpr::StringLit(v.clone()),
        IREexpr::FnLiteral(lit) => IREexpr::FnLiteral(IRFnLiteral {
            symbol: lit.symbol.clone(),
            params: lit
                .params
                .iter()
                .map(|p| IRParam {
                    name: p.name.clone(),
                    ty: substitute_type(&p.ty, substitutions),
                })
                .collect(),
            return_type: lit
                .return_type
                .as_ref()
                .map(|ty| substitute_type(ty, substitutions)),
            body: substitute_types_in_block(&lit.body, substitutions),
            captures: lit
                .captures
                .iter()
                .map(|(name, ty)| (name.clone(), substitute_type(ty, substitutions)))
                .collect(),
            env_struct: lit.env_struct.clone(),
        }),
    }
}

fn substitute_type(ty: &Type, substitutions: &HashMap<String, Type>) -> Type {
    crate::types_util::substitute_type(ty, substitutions)
}

/// Parse, number, type-check, and lower `src`. Panics on compile errors (tests/helpers).
pub fn lower_checked(src: &str) -> IRProgram {
    let tokens = crate::lexer::Lexer::new(src)
        .tokenize()
        .unwrap_or_else(|e| panic!("lex error: {e}"));
    let mut program = crate::parser::Parser::new(tokens)
        .parse()
        .unwrap_or_else(|e| panic!("parse error: {e}"));
    let mut next = 1;
    crate::ast::number_program(&mut program, &mut next);
    let mut checker = crate::tc::TypeChecker::new();
    let (result, errors) = checker.check_program_collecting(&program);
    assert!(
        errors.is_empty(),
        "{}",
        crate::tc::format_type_errors(&errors)
    );
    IRBuilder::build(&program, &result.type_info)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn module_qualified_generic_call_is_monomorphized() {
        let src = r#"
struct Arena<T> {
    live: int;
}
fn handle_new<T>() -> Arena<T> {
    return Arena { live: 0 };
}
fn main() -> int {
    let a: Arena<int> = handle_new();
    return a.live;
}
"#;
        let mut ir = lower_checked(src);
        {
            let main = ir
                .functions
                .iter_mut()
                .find(|f| f.name == "main")
                .expect("main");
            let init = match &mut main.blocks[0].statements[0] {
                IRStmt::Let(let_stmt) => let_stmt.init.as_mut().expect("init"),
                other => panic!("expected let, got {other:?}"),
            };
            match init {
                IREexpr::Call { callee, .. } => {
                    assert_eq!(callee, "handle_new_int");
                    *callee = "handle::new".to_string();
                }
                other => panic!("expected call, got {other:?}"),
            }
        }
        ir.functions.push(IRFunction {
            name: "handle_new".to_string(),
            generics: vec!["T".to_string()],
            params: Vec::new(),
            return_type: Some(Type::Generic {
                name: "Arena".to_string(),
                params: vec![Type::Generic {
                    name: "T".to_string(),
                    params: Vec::new(),
                }],
            }),
            blocks: Vec::new(),
        });
        monomorphize_generic_functions(&mut ir);
        assert!(
            ir.functions.iter().any(|f| f.name == "handle_new_int"),
            "expected handle_new_int after module-qualified rewrite, got {:?}",
            ir.functions
                .iter()
                .map(|f| f.name.as_str())
                .collect::<Vec<_>>()
        );
        let main = ir.functions.iter().find(|f| f.name == "main").unwrap();
        let init = match &main.blocks[0].statements[0] {
            IRStmt::Let(let_stmt) => let_stmt.init.as_ref().unwrap(),
            other => panic!("expected let, got {other:?}"),
        };
        match init {
            IREexpr::Call { callee, .. } => assert_eq!(callee, "handle_new_int"),
            other => panic!("expected call, got {other:?}"),
        }
    }

    #[test]
    fn unannotated_var_copy_keeps_checker_type() {
        let src = r#"
struct Point {
    x: int;
    y: int;
}
fn main() -> int {
    let p: Point = Point { x: 1, y: 2 };
    let q = p;
    return q.x + q.y;
}
"#;
        let ir = lower_checked(src);
        let main = ir.functions.iter().find(|f| f.name == "main").unwrap();
        let lets: Vec<&IRLetStmt> = main.blocks[0]
            .statements
            .iter()
            .filter_map(|s| match s {
                IRStmt::Let(l) => Some(l),
                _ => None,
            })
            .collect();
        assert!(
            lets.len() >= 2,
            "expected two lets, got {:?}",
            lets.iter().map(|l| &l.name).collect::<Vec<_>>()
        );
        assert!(
            matches!(&lets[1].ty, Type::Struct(n) if n == "Point"),
            "let q = p should keep Point, got {:?}",
            lets[1].ty
        );
    }
}
