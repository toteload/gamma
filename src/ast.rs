use crate::string_interner::Symbol;
use serde::Serialize;
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct NodeId(u32);

pub struct NodeIdGenerator {
    counter: Cell<u32>,
}

impl Default for NodeIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl NodeIdGenerator {
    pub fn new() -> NodeIdGenerator {
        NodeIdGenerator {
            counter: Cell::new(0),
        }
    }

    pub fn gen_id(&self) -> NodeId {
        self.counter.replace(self.counter.get() + 1);
        NodeId(self.counter.get())
    }
}

pub type AstMap<T> = HashMap<NodeId, T>;

#[derive(Debug, Clone, Copy, Serialize)]
pub struct Name {
    pub id: NodeId,
    pub sym: Symbol,
}

pub type Label = Name;

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
            ItemKind::ExternalFunction { name, .. } => name.sym,
            ItemKind::Layout { name, .. } => name.sym,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Field {
    pub name: Name,
    pub offset: u32,
    pub ty: Type,
}

#[derive(Debug, Clone, Serialize)]
pub enum ItemKind {
    Function {
        return_type: Type,
        name: Name,
        params: Vec<Param>,
        body: Box<Block>,
    },
    ExternalFunction {
        return_type: Type,
        name: Name,
        params: Vec<Param>,
    },
    Layout {
        name: Name,
        fields: Vec<Field>,
    },
}

#[derive(Clone, Debug, Serialize)]
pub struct Type {
    pub id: NodeId,
    pub kind: TypeKind,
}

#[derive(Clone, Debug, Serialize)]
pub enum TypeKind {
    // `Internal` is a special value for compiler added type nodes.
    Internal,
    Identifier(Symbol),
    Pointer(Box<TypeKind>),
    Array(i64, Box<TypeKind>),
}

#[derive(Clone, Debug, Serialize)]
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
        init: Option<Box<Expression>>,
    },
    Set {
        dst: Box<Expression>,
        val: Box<Expression>,
    },
    Expression(Box<Expression>),
    If {
        cond: Box<Expression>,
        then: Block,
        otherwise: Option<Block>,
    },
    Break(Option<Label>),
    Continue(Option<Label>),
    Return(Option<Box<Expression>>),
    Loop(Block, Option<Label>),
}

#[derive(Clone, Debug, Serialize)]
pub struct Block {
    pub id: NodeId,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Copy, Debug, Serialize)]
pub enum BuiltinOpKind {
    Not,
    Or,
    And,

    Xor,
    BitwiseAnd,
    BitwiseOr,

    Add,
    Sub,
    Mul,
    Div,
    Remainder,

    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,

    AddressOf,
    At,
}

#[derive(Clone, Debug, Serialize)]
pub struct Expression {
    pub id: NodeId,
    pub kind: ExpressionKind,
}

#[derive(Clone, Debug, Serialize)]
pub enum ExpressionKind {
    IntLiteral(i64),
    BoolLiteral(bool),
    Identifier(Symbol),
    CompoundIdentifier(Vec<Symbol>),
    BuiltinOp {
        op: BuiltinOpKind,
        args: Vec<Expression>,
    },
    Call {
        name: Name,
        args: Vec<Expression>,
    },
    Cast {
        ty: Type,
        e: Box<Expression>,
    },
}
