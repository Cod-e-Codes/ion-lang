use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct Program {
    pub imports: Vec<ImportStmt>,
    pub structs: Vec<StructDecl>,
    pub enums: Vec<EnumDecl>,
    pub type_aliases: Vec<TypeAliasDecl>,
    pub functions: Vec<FnDecl>,
    pub extern_blocks: Vec<ExternBlock>,
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
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
pub struct FnDecl {
    pub pub_: bool,
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub pub_: bool,
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub pub_: bool,
    pub name: String,
    pub generics: Vec<String>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeAliasDecl {
    pub pub_: bool,
    pub name: String,
    pub generics: Vec<String>,
    pub target: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub payload_types: Vec<Type>, // For tuple variants: Enum::Variant(Type1, Type2)
    pub named_fields: Option<Vec<(String, Type)>>, // For struct variants: Enum::Variant { field: Type }
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructField {
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
    Ref { inner: Box<Type>, mutable: bool },
    RawPtr { inner: Box<Type> },
    Channel { elem_type: Box<Type> }, // Deprecated, use Sender/Receiver
    Sender { elem_type: Box<Type> },
    Receiver { elem_type: Box<Type> },
    Struct(String),
    Enum(String),
    Generic { name: String, params: Vec<Type> },
    Box { inner: Box<Type> },
    Vec { elem_type: Box<Type> },
    String,
    Array { inner: Box<Type>, size: usize },
    Slice { inner: Box<Type> },
    Tuple { elements: Vec<Type> }, // For (Type1, Type2, ...) - used for channel() return
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(LetStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
    Defer(DeferStmt),
    Spawn(SpawnStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    UnsafeBlock(UnsafeBlockStmt),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: String,                   // For single variable binding
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
    Index(IndexExpr),
    Cast(CastExpr),
    Assign(AssignExpr),
}

#[derive(Debug, Clone)]
pub struct RefExpr {
    pub mutable: bool,
    pub inner: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LitExpr {
    pub value: i64,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BoolLiteralExpr {
    pub value: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FloatLiteralExpr {
    pub value: f64,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VarExpr {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinOpExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnOpExpr {
    pub op: UnOp,
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SendExpr {
    pub channel: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RecvExpr {
    pub channel: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructLitExpr {
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
    pub base: Box<Expr>,
    pub field: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumLitExpr {
    pub enum_name: String,
    pub variant: String,
    pub args: Vec<Expr>, // For tuple variants: Enum::Variant(expr1, expr2)
    pub named_fields: Option<Vec<(String, Expr)>>, // For struct variants: Enum::Variant { field: expr }
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchExpr {
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
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
    pub callee: String,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub receiver: Box<Expr>,
    pub method: String,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StringLitExpr {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub elements: Vec<Expr>,
    pub repeat: Option<(Box<Expr>, usize)>, // For [value; count] syntax: (value, count)
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CastExpr {
    pub expr: Box<Expr>,
    pub target_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
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

#[derive(Debug, Clone, Copy)]
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
