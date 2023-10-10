use crate::string_interner::Symbol;
use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
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

#[derive(Debug, Clone, Copy, Serialize)]
pub struct Name {
    pub id: NodeId,
    pub sym: Symbol,
}

// Item has an associated TypeToken if it is a function
#[derive(Debug, Serialize)]
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

#[derive(Debug, Clone, Serialize)]
pub enum ItemKind {
    Function {
        return_type: Type,
        name: Name,
        params: Vec<Param>,
        body: Box<Block>,
    },
}

// Type has an associated TypeToken
#[derive(Clone, Debug, Serialize, Copy)]
pub struct Type {
    pub id: NodeId,
    pub kind: TypeKind,
}

#[derive(Clone, Copy, Debug, Serialize)]
pub enum TypeKind {
    Int,
    Void,
    Bool,
}

// Param has an associated TypeToken
#[derive(Clone, Copy, Debug, Serialize)]
pub struct Param {
    pub id: NodeId,
    pub name: Name,
    pub ty: Type,
}

#[derive(Clone, Debug, Serialize)]
pub struct Statement {
    pub id: NodeId,
    pub kind: StatementKind,
}

#[derive(Clone, Debug, Serialize)]
pub enum StatementKind {
    Let {
        name: Name,
        ty: Type,
        init: Option<Box<Expr>>,
    },
    Set {
        dst: Box<Expr>,
        val: Box<Expr>,
    },
    Expr(Box<Expr>),
    If {
        cond: Box<Expr>,
        then: Block,
        otherwise: Option<Block>,
    },
    Break,
    Continue,
    Return(Option<Box<Expr>>),
    Loop(Block),
}

#[derive(Clone, Debug, Serialize)]
pub struct Block {
    pub id: NodeId,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Copy, Debug, Serialize)]
pub enum BuiltinOpKind {
    Or,
    And,
    Xor,
    Not,
    Add,
    Sub,
    Mul,
    Div,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,
}

// Expr always has an associated TypeToken
#[derive(Clone, Debug, Serialize)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
}

#[derive(Clone, Debug, Serialize)]
pub enum ExprKind {
    IntLiteral(i64),
    BoolLiteral(bool),
    Identifier(Symbol),
    BuiltinOp { op: BuiltinOpKind, args: Vec<Expr> },
    Call { name: Name, args: Vec<Expr> },
    Cast { ty: Type, e: Box<Expr> },
}
