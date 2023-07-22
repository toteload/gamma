use crate::string_interner::Symbol;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(u32);

pub struct NodeIdGenerator {
    counter: u32,
}

impl NodeIdGenerator {
    pub fn new() -> NodeIdGenerator {
        NodeIdGenerator { counter: 0 }
    }

    pub fn gen_id(&mut self) -> NodeId {
        self.counter += 1;
        NodeId(self.counter)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,

    LessEquals,
    GreaterEquals,

    LessThan,
    GreaterThan,

    Equals,
    NotEquals,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    BitwiseOr,
    Xor,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: NodeId,
    pub sym: Symbol,
}

#[derive(Debug)]
pub struct Item {
    pub id: NodeId,
    pub kind: ItemKind,
}

impl Item {
    pub fn name_sym(&self) -> Symbol {
        match &self.kind {
            ItemKind::Function { name, .. } => name.sym,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function {
        return_type: Type,
        name: Name,
        params: Vec<Param>,
        body: Box<Block>,
    },
}

#[derive(Clone, Debug)]
pub struct Type {
    pub id: NodeId,
    pub kind: TypeKind,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Int,
    Void,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub id: NodeId,
    pub name: Name,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub id: NodeId,
    pub kind: StatementKind,
}

#[derive(Clone, Debug)]
pub enum StatementKind {
    Let {
        name: Name,
        ty: Type,
        init: Box<Expr>,
    },
    Assign {
        name: Name,
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

#[derive(Clone, Debug)]
pub struct Block {
    pub id: NodeId,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    IntLiteral(i64),
    BoolLiteral(bool),
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
        name: Name,
        args: Vec<Expr>,
    },
    Cast {
        ty: Type,
        e: Box<Expr>,
    },
}
