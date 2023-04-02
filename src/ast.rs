use crate::parse::SourceSpan;
use crate::string_interner::Symbol;
use crate::types::TypeToken;

pub struct NodeId(u32);

#[derive(Clone, Debug)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    GreaterThan,
    Equals,
}

#[derive(Clone, Debug)]
pub enum UnaryOpKind {
    Negate,
    Not,
}

#[derive(Debug)]
pub struct Item {
    pub attr: Attributes,
    pub kind: ItemKind,
}

impl Item {
    pub fn name_sym(&self) -> Symbol {
        match self.kind {
            ItemKind::Function(Function { name, .. }) => name,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub return_type: Type,
    pub name: Symbol,
    pub params: Vec<Param>,
    pub body: Box<Block>,
}

#[derive(Debug)]
pub enum ItemKind {
    Function(Function),
}

#[derive(Clone, Debug)]
pub struct Type {
    pub attr: Attributes,
    pub kind: TypeKind,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Identifier(Symbol),
    Int,
    Void,
}

#[derive(Clone, Debug)]
pub struct Attributes {
    pub span: SourceSpan,
    pub ty: Option<TypeToken>,
}

impl Attributes {
    pub fn from_span(span: SourceSpan) -> Attributes {
        Attributes { span, ty: None }
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub attr: Attributes,
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub attr: Attributes,
    pub kind: StatementKind,
}

#[derive(Clone, Debug)]
pub enum StatementKind {
    Let {
        sym: Symbol,
        ty: Type,
        init: Box<Expr>,
    },
    Assign {
        sym: Symbol,
        val: Box<Expr>,
    },
    Empty,
    Expr(Box<Expr>),
    Block(Block),
    If {
        cond: Box<Expr>,
        then: Block,
        otherwise: Option<Block>,
    },
    Loop(Block),
    Break,
    Continue,
    Return(Option<Box<Expr>>),
}

impl Statement {
    pub fn new(span: SourceSpan, kind: StatementKind) -> Statement {
        Statement {
            attr: Attributes { span, ty: None },
            kind,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub attr: Attributes,
    pub statements: Vec<Statement>,
}

impl Expr {
    pub fn new(span: SourceSpan, kind: ExprKind) -> Expr {
        Expr {
            attr: Attributes::from_span(span),
            kind,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub attr: Attributes,
    pub kind: ExprKind,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    IntLiteral(i64),
    Identifier(Symbol),
    UnaryOp {
        op: UnaryOpKind,
        e: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOpKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        sym: Symbol,
        args: Vec<Expr>,
    },
}
