//! Abstract Syntax Tree definitions for RetroLang

use crate::lexer::Span;

/// A complete program
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

/// Top-level items in a program
#[derive(Debug, Clone)]
pub enum Item {
    Const(ConstDecl),
    Var(VarDecl),
    Proc(ProcDecl),
    Func(FuncDecl),
}

/// Constant declaration: const NAME: TYPE = VALUE;
#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: String,
    pub ty: Type,
    pub value: Expr,
    pub span: Span,
}

/// Variable declaration: var NAME: TYPE [= VALUE];
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub ty: Type,
    pub init: Option<Expr>,
    pub span: Span,
}

/// Procedure declaration: proc NAME(params) ... end;
#[derive(Debug, Clone)]
pub struct ProcDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub locals: Vec<VarDecl>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

/// Function declaration: func NAME(params): TYPE ... end;
#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub locals: Vec<VarDecl>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

/// Function/procedure parameter
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// Type representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Byte,
    Int,
    Bool,
    Char,
    Array(Box<Type>, usize),  // element type, size
    Pointer(Box<Type>),       // pointed-to type
}

impl Type {
    /// Size in bytes
    pub fn size(&self) -> usize {
        match self {
            Type::Byte | Type::Bool | Type::Char => 1,
            Type::Int | Type::Pointer(_) => 2,
            Type::Array(elem, count) => elem.size() * count,
        }
    }

    /// Is this a 16-bit type?
    pub fn is_word(&self) -> bool {
        matches!(self, Type::Int | Type::Pointer(_))
    }
}

/// Statements
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Assignment: lvalue := expr
    Assign(LValue, Expr, Span),

    /// If statement
    If {
        cond: Expr,
        then_body: Vec<Stmt>,
        elsif_clauses: Vec<(Expr, Vec<Stmt>)>,
        else_body: Option<Vec<Stmt>>,
        span: Span,
    },

    /// While loop
    While {
        cond: Expr,
        body: Vec<Stmt>,
        span: Span,
    },

    /// For loop
    For {
        var: String,
        start: Expr,
        end: Expr,
        downto: bool,
        body: Vec<Stmt>,
        span: Span,
    },

    /// Break statement
    Break(Span),

    /// Continue statement
    Continue(Span),

    /// Return statement
    Return(Option<Expr>, Span),

    /// Procedure call
    Call(String, Vec<Expr>, Span),

    /// Inline assembly
    Asm(String, Span),
}

/// Left-hand side of assignment
#[derive(Debug, Clone)]
pub enum LValue {
    /// Simple variable
    Var(String, Span),

    /// Array element: arr[index]
    Index(String, Expr, Span),

    /// Pointer dereference: ptr^
    Deref(Box<Expr>, Span),

    /// Memory access: mem[addr]
    Mem(Expr, Span),

    /// Word memory access: memw[addr]
    Memw(Expr, Span),
}

/// Expressions
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal
    IntLit(i32, Span),

    /// Boolean literal
    BoolLit(bool, Span),

    /// Character literal
    CharLit(u8, Span),

    /// String literal
    StringLit(String, Span),

    /// Variable reference
    Var(String, Span),

    /// Array element access: arr[index]
    Index(Box<Expr>, Box<Expr>, Span),

    /// Pointer dereference: ptr^
    Deref(Box<Expr>, Span),

    /// Address-of: @var
    AddrOf(String, Span),

    /// Memory read: mem[addr]
    Mem(Box<Expr>, Span),

    /// Word memory read: memw[addr]
    Memw(Box<Expr>, Span),

    /// Binary operation
    Binary(Box<Expr>, BinOp, Box<Expr>, Span),

    /// Unary operation
    Unary(UnaryOp, Box<Expr>, Span),

    /// Function call
    Call(String, Vec<Expr>, Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::IntLit(_, s) => *s,
            Expr::BoolLit(_, s) => *s,
            Expr::CharLit(_, s) => *s,
            Expr::StringLit(_, s) => *s,
            Expr::Var(_, s) => *s,
            Expr::Index(_, _, s) => *s,
            Expr::Deref(_, s) => *s,
            Expr::AddrOf(_, s) => *s,
            Expr::Mem(_, s) => *s,
            Expr::Memw(_, s) => *s,
            Expr::Binary(_, _, _, s) => *s,
            Expr::Unary(_, _, s) => *s,
            Expr::Call(_, _, s) => *s,
        }
    }
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,

    // Logical
    And,
    Or,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl BinOp {
    /// Operator precedence (higher = tighter binding)
    pub fn precedence(&self) -> u8 {
        match self {
            BinOp::Or => 1,
            BinOp::And => 2,
            BinOp::BitOr => 3,
            BinOp::BitXor => 4,
            BinOp::BitAnd => 5,
            BinOp::Eq | BinOp::Neq => 6,
            BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => 7,
            BinOp::Shl | BinOp::Shr => 8,
            BinOp::Add | BinOp::Sub => 9,
            BinOp::Mul | BinOp::Div | BinOp::Mod => 10,
        }
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,     // -x
    Not,     // not x (logical)
    BitNot,  // ~x (bitwise)
}
