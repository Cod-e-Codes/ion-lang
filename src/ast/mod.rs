use crate::lexer::Token;

/// Stable identity for an expression. Copied by `Clone` so merged and per-module
/// ASTs share ids. `0` means the numbering pass has not run.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

impl ExprId {
    pub const UNASSIGNED: ExprId = ExprId(0);

    pub fn is_assigned(self) -> bool {
        self.0 != 0
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    /// File-level documentation from leading `//` lines before imports and declarations.
    pub doc: Option<String>,
    pub imports: Vec<ImportStmt>,
    pub structs: Vec<StructDecl>,
    pub enums: Vec<EnumDecl>,
    pub type_aliases: Vec<TypeAliasDecl>,
    pub functions: Vec<FnDecl>,
    pub extern_blocks: Vec<ExternBlock>,
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub doc: Option<String>,
    pub path: String,
    pub alias: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExternBlock {
    pub linkage: String,
    pub functions: Vec<ExternFnDecl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExternFnDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub variadic: bool, // true if function has ... (variadic arguments)
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: String,
    pub bounds: Vec<String>,
}

impl TypeParam {
    pub fn simple(name: &str) -> Self {
        TypeParam {
            name: name.to_string(),
            bounds: Vec::new(),
        }
    }

    pub fn names(params: &[TypeParam]) -> Vec<String> {
        params.iter().map(|p| p.name.clone()).collect()
    }

    pub fn format_list(params: &[TypeParam]) -> String {
        if params.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                params
                    .iter()
                    .map(|p| {
                        if p.bounds.is_empty() {
                            p.name.clone()
                        } else {
                            format!("{}: {}", p.name, p.bounds.join(" + "))
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub doc: Option<String>,
    pub pub_: bool,
    pub name: String,
    pub generics: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub doc: Option<String>,
    pub pub_: bool,
    pub name: String,
    pub generics: Vec<TypeParam>,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub doc: Option<String>,
    pub pub_: bool,
    pub name: String,
    pub generics: Vec<TypeParam>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeAliasDecl {
    pub doc: Option<String>,
    pub pub_: bool,
    pub name: String,
    pub generics: Vec<TypeParam>,
    pub target: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub doc: Option<String>,
    pub name: String,
    pub payload_types: Vec<Type>, // For tuple variants: Enum::Variant(Type1, Type2)
    pub named_fields: Option<Vec<(String, Type)>>, // For struct variants: Enum::Variant { field: Type }
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub doc: Option<String>,
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Int,
    Bool,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    UInt,
    Ref {
        inner: Box<Type>,
        mutable: bool,
    },
    RawPtr {
        inner: Box<Type>,
    },
    Channel {
        elem_type: Box<Type>,
    }, // Deprecated, use Sender/Receiver
    Sender {
        elem_type: Box<Type>,
    },
    Receiver {
        elem_type: Box<Type>,
    },
    Struct(String),
    Enum(String),
    Generic {
        name: String,
        params: Vec<Type>,
    },
    Box {
        inner: Box<Type>,
    },
    Vec {
        elem_type: Box<Type>,
    },
    String,
    /// Unsized UTF-8 slice (`str`); only valid behind `&str` (Section 8.3).
    Str,
    Array {
        inner: Box<Type>,
        size: usize,
    },
    Slice {
        inner: Box<Type>,
    },
    Tuple {
        elements: Vec<Type>,
    }, // For (Type1, Type2, ...) - used for channel() return
    Fn {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(LetStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Expr(ExprStmt),
    Defer(DeferStmt),
    Spawn(SpawnStmt),
    If(IfStmt),
    While(WhileStmt),
    Loop(LoopStmt),
    For(ForStmt),
    UnsafeBlock(UnsafeBlockStmt),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: String,                   // For single variable binding
    pub name_span: Span,                // Span of the binding identifier (LSP hover/goto)
    pub patterns: Option<Vec<Pattern>>, // For tuple destructuring: let (a, b) = ...
    pub mutable: bool,
    pub type_ann: Option<Type>,
    pub init: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BreakStmt {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct DeferStmt {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SpawnStmt {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Expr,
    pub then_block: Block,
    pub else_block: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LoopStmt {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub var_name: String,
    pub iterable: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnsafeBlockStmt {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(LitExpr),
    BoolLiteral(BoolLiteralExpr),
    FloatLiteral(FloatLiteralExpr),
    Var(VarExpr),
    BinOp(BinOpExpr),
    UnOp(UnOpExpr),
    Ref(RefExpr),
    Send(SendExpr),
    Recv(RecvExpr),
    StructLit(StructLitExpr),
    FieldAccess(FieldAccessExpr),
    EnumLit(EnumLitExpr),
    Match(MatchExpr),
    Call(CallExpr),
    MethodCall(MethodCallExpr),
    StringLit(StringLitExpr),
    ArrayLiteral(ArrayLiteralExpr),
    TupleLit(TupleLitExpr),
    Index(IndexExpr),
    Cast(CastExpr),
    Assign(AssignExpr),
    FnLiteral(FnLiteralExpr),
    TypeConst(TypeConstExpr),
}

impl Expr {
    pub fn id(&self) -> ExprId {
        match self {
            Expr::Lit(e) => e.id,
            Expr::BoolLiteral(e) => e.id,
            Expr::FloatLiteral(e) => e.id,
            Expr::Var(e) => e.id,
            Expr::BinOp(e) => e.id,
            Expr::UnOp(e) => e.id,
            Expr::Ref(e) => e.id,
            Expr::Send(e) => e.id,
            Expr::Recv(e) => e.id,
            Expr::StructLit(e) => e.id,
            Expr::FieldAccess(e) => e.id,
            Expr::EnumLit(e) => e.id,
            Expr::Match(e) => e.id,
            Expr::Call(e) => e.id,
            Expr::MethodCall(e) => e.id,
            Expr::StringLit(e) => e.id,
            Expr::ArrayLiteral(e) => e.id,
            Expr::TupleLit(e) => e.id,
            Expr::Index(e) => e.id,
            Expr::Cast(e) => e.id,
            Expr::Assign(e) => e.id,
            Expr::FnLiteral(e) => e.id,
            Expr::TypeConst(e) => e.id,
        }
    }

    pub fn set_id(&mut self, id: ExprId) {
        match self {
            Expr::Lit(e) => e.id = id,
            Expr::BoolLiteral(e) => e.id = id,
            Expr::FloatLiteral(e) => e.id = id,
            Expr::Var(e) => e.id = id,
            Expr::BinOp(e) => e.id = id,
            Expr::UnOp(e) => e.id = id,
            Expr::Ref(e) => e.id = id,
            Expr::Send(e) => e.id = id,
            Expr::Recv(e) => e.id = id,
            Expr::StructLit(e) => e.id = id,
            Expr::FieldAccess(e) => e.id = id,
            Expr::EnumLit(e) => e.id = id,
            Expr::Match(e) => e.id = id,
            Expr::Call(e) => e.id = id,
            Expr::MethodCall(e) => e.id = id,
            Expr::StringLit(e) => e.id = id,
            Expr::ArrayLiteral(e) => e.id = id,
            Expr::TupleLit(e) => e.id = id,
            Expr::Index(e) => e.id = id,
            Expr::Cast(e) => e.id = id,
            Expr::Assign(e) => e.id = id,
            Expr::FnLiteral(e) => e.id = id,
            Expr::TypeConst(e) => e.id = id,
        }
    }
}

/// Assign unique `ExprId`s in `program`, starting at `*next_id` (must be >= 1).
pub fn number_program(program: &mut Program, next_id: &mut u32) {
    for function in &mut program.functions {
        number_block(&mut function.body, next_id);
    }
}

pub(crate) fn number_block(block: &mut Block, next_id: &mut u32) {
    for stmt in &mut block.statements {
        number_stmt(stmt, next_id);
    }
}

pub(crate) fn number_stmt(stmt: &mut Stmt, next_id: &mut u32) {
    match stmt {
        Stmt::Let(s) => {
            if let Some(init) = &mut s.init {
                number_expr(init, next_id);
            }
        }
        Stmt::Return(s) => {
            if let Some(value) = &mut s.value {
                number_expr(value, next_id);
            }
        }
        Stmt::Break(_) | Stmt::Continue(_) => {}
        Stmt::Expr(s) => number_expr(&mut s.expr, next_id),
        Stmt::Defer(s) => number_expr(&mut s.expr, next_id),
        Stmt::Spawn(s) => number_block(&mut s.body, next_id),
        Stmt::If(s) => {
            number_expr(&mut s.cond, next_id);
            number_block(&mut s.then_block, next_id);
            if let Some(else_block) = &mut s.else_block {
                number_block(else_block, next_id);
            }
        }
        Stmt::While(s) => {
            number_expr(&mut s.cond, next_id);
            number_block(&mut s.body, next_id);
        }
        Stmt::Loop(s) => number_block(&mut s.body, next_id),
        Stmt::For(s) => {
            number_expr(&mut s.iterable, next_id);
            number_block(&mut s.body, next_id);
        }
        Stmt::UnsafeBlock(s) => number_block(&mut s.body, next_id),
    }
}

pub(crate) fn number_expr(expr: &mut Expr, next_id: &mut u32) {
    match expr {
        Expr::BinOp(e) => {
            number_expr(&mut e.left, next_id);
            number_expr(&mut e.right, next_id);
        }
        Expr::UnOp(e) => number_expr(&mut e.operand, next_id),
        Expr::Ref(e) => number_expr(&mut e.inner, next_id),
        Expr::Send(e) => {
            number_expr(&mut e.channel, next_id);
            number_expr(&mut e.value, next_id);
        }
        Expr::Recv(e) => number_expr(&mut e.channel, next_id),
        Expr::StructLit(e) => {
            for field in &mut e.fields {
                number_expr(&mut field.value, next_id);
            }
        }
        Expr::FieldAccess(e) => number_expr(&mut e.base, next_id),
        Expr::EnumLit(e) => {
            for arg in &mut e.args {
                number_expr(arg, next_id);
            }
            if let Some(fields) = &mut e.named_fields {
                for (_, value) in fields {
                    number_expr(value, next_id);
                }
            }
        }
        Expr::Match(e) => {
            number_expr(&mut e.expr, next_id);
            for arm in &mut e.arms {
                if let Some(guard) = &mut arm.guard {
                    number_expr(guard, next_id);
                }
                number_block(&mut arm.body, next_id);
            }
        }
        Expr::Call(e) => {
            for arg in &mut e.args {
                number_expr(arg, next_id);
            }
        }
        Expr::MethodCall(e) => {
            number_expr(&mut e.receiver, next_id);
            for arg in &mut e.args {
                number_expr(arg, next_id);
            }
        }
        Expr::ArrayLiteral(e) => {
            for elem in &mut e.elements {
                number_expr(elem, next_id);
            }
            if let Some((repeat, _)) = &mut e.repeat {
                number_expr(repeat, next_id);
            }
        }
        Expr::TupleLit(e) => {
            for elem in &mut e.elements {
                number_expr(elem, next_id);
            }
        }
        Expr::Index(e) => {
            number_expr(&mut e.target, next_id);
            number_expr(&mut e.index, next_id);
        }
        Expr::Cast(e) => number_expr(&mut e.expr, next_id),
        Expr::Assign(e) => {
            number_expr(&mut e.target, next_id);
            number_expr(&mut e.value, next_id);
        }
        Expr::FnLiteral(e) => number_block(&mut e.body, next_id),
        Expr::Lit(_)
        | Expr::BoolLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::Var(_)
        | Expr::StringLit(_)
        | Expr::TypeConst(_) => {}
    }
    if !expr.id().is_assigned() {
        let id = *next_id;
        *next_id = next_id.saturating_add(1);
        expr.set_id(ExprId(id));
    }
}

#[derive(Debug, Clone)]
pub struct TypeConstExpr {
    pub id: ExprId,
    pub type_name: String,
    pub member: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnLiteralExpr {
    pub id: ExprId,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RefExpr {
    pub id: ExprId,
    pub mutable: bool,
    pub inner: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LitExpr {
    pub id: ExprId,
    pub value: i64,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BoolLiteralExpr {
    pub id: ExprId,
    pub value: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FloatLiteralExpr {
    pub id: ExprId,
    pub value: f64,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VarExpr {
    pub id: ExprId,
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinOpExpr {
    pub id: ExprId,
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnOpExpr {
    pub id: ExprId,
    pub op: UnOp,
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SendExpr {
    pub id: ExprId,
    pub channel: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RecvExpr {
    pub id: ExprId,
    pub channel: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructLitExpr {
    pub id: ExprId,
    pub type_name: String,
    pub fields: Vec<StructLitField>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructLitField {
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    pub id: ExprId,
    pub base: Box<Expr>,
    pub field: String,
    pub field_span: Span,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumLitExpr {
    pub id: ExprId,
    pub enum_name: String,
    pub enum_name_span: Span,
    pub variant: String,
    pub variant_span: Span,
    pub args: Vec<Expr>, // For tuple variants: Enum::Variant(expr1, expr2)
    pub named_fields: Option<Vec<(String, Expr)>>, // For struct variants: Enum::Variant { field: expr }
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchExpr {
    pub id: ExprId,
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variant {
        enum_name: String,
        variant: String,
        sub_patterns: Vec<Pattern>, // For tuple variants: Enum::Variant(pattern1, pattern2)
        named_fields: Option<Vec<(String, Pattern)>>, // For struct variants: Enum::Variant { field: pattern }
        span: Span,
    },
    Wildcard {
        span: Span,
    },
    Binding {
        name: String,
        span: Span,
    },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Variant { span, .. } => *span,
            Pattern::Wildcard { span } => *span,
            Pattern::Binding { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub id: ExprId,
    pub callee: String,
    pub args: Vec<Expr>,
    pub span: Span,
    /// Span of the callee identifier(s), for LSP go-to-definition.
    pub callee_span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub id: ExprId,
    pub receiver: Box<Expr>,
    pub method: String,
    pub method_span: Span,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StringLitExpr {
    pub id: ExprId,
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub id: ExprId,
    pub elements: Vec<Expr>,
    pub repeat: Option<(Box<Expr>, usize)>, // For [value; count] syntax: (value, count)
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleLitExpr {
    pub id: ExprId,
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub id: ExprId,
    pub target: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CastExpr {
    pub id: ExprId,
    pub expr: Box<Expr>,
    pub target_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub id: ExprId,
    pub target: Box<Expr>, // Can be VarExpr or IndexExpr
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,        // +
    Sub,        // -
    Mul,        // *
    Div,        // /
    Rem,        // %
    Lt,         // <
    Gt,         // >
    Le,         // <=
    Ge,         // >=
    Eq,         // ==
    Ne,         // !=
    And,        // &&
    Or,         // ||
    BitAnd,     // &
    BitOr,      // |
    BitXor,     // ^
    ShiftLeft,  // <<
    ShiftRight, // >>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Not, // !
    Neg, // - (unary minus)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn from_token(token: &Token) -> Self {
        Self {
            start: token.span.start,
            end: token.span.end,
            line: token.span.line,
            column: token.span.column,
        }
    }

    pub fn merge(&self, other: &Span) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line,
            column: self.column,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        }
    }
}
