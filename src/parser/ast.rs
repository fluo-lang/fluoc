use crate::helpers;

pub trait Node { }
pub trait Expr { }

macro_rules! impl_node {
    ($($t:ty),+) => {
        $(impl Node for $t { })+
    }
}

macro_rules! impl_expr {
    ($($t:ty),+) => {
        $(impl Expr for $t { })+
    }
}

// EXPRESSIONS ---------------------------------------
pub struct Integer {
    value: String,
    pos: helpers::Pos
}

pub struct RefID {
    value: String,
    pos: helpers::Pos
}

pub struct Reference {
    value: RefID,
    pos: helpers::Pos
}

// NODES ---------------------------------------
pub struct NameID {
    value: String,
    pos: helpers::Pos
}

pub struct VariableAssign {
    name: NameID,
    expr: Box<dyn Expr>,
    pos: helpers::Pos
}

pub struct VariableAssignDeclaration {
    t: Type,
    name: NameID,
    expr: Box<dyn Expr>,
    pos: helpers::Pos
}

pub struct Type {
    value: String,
    pos: helpers::Pos
}

pub struct VariableDeclaration {
    t: Type,
    name: NameID,
    pos: helpers::Pos
}

pub struct Arguments {
    positional: Vec<(String, Type)>,
    pos: helpers::Pos
    // TODO: Add more types or arguments
}

pub struct Block {
    nodes: Vec<Box<dyn Node>>,
    pos: helpers::Pos
}

pub struct FunctionDefine {
    return_type: Type,
    arguments: Arguments,
    block: Block,
    pos: helpers::Pos
}

impl_expr!(
    Integer, 
    RefID, 
    Reference,
    VariableAssign,
    VariableAssignDeclaration
);

impl_node!(
    Integer, 
    RefID, 
    Reference, 
    NameID, 
    VariableAssign, 
    VariableAssignDeclaration, 
    Type, 
    VariableDeclaration, 
    Arguments, 
    Block, 
    FunctionDefine
);
