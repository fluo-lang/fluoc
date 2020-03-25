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
    pub value: String,
    pub pos: helpers::Pos
}

pub struct RefID {
    pub value: String,
    pub pos: helpers::Pos
}

pub struct Reference {
    pub value: RefID,
    pub pos: helpers::Pos
}

// NODES ---------------------------------------
pub struct NameID {
    pub value: String,
    pub pos: helpers::Pos
}

pub struct VariableAssign {
    pub name: NameID,
    pub expr: Box<dyn Expr>,
    pub pos: helpers::Pos
}

pub struct VariableAssignDeclaration {
    pub t: Type,
    pub name: NameID,
    pub expr: Box<dyn Expr>,
    pub pos: helpers::Pos
}

pub struct VariableDeclaration {
    pub t: Type,
    pub name: NameID,
    pub pos: helpers::Pos
}

pub struct Arguments {
    pub positional: Vec<(String, Type)>,
    pub pos: helpers::Pos
    // TODO: Add more types or arguments
}

pub struct Block {
    pub nodes: Vec<Box<dyn Node>>,
    pub pos: helpers::Pos
}

pub struct FunctionDefine {
    pub return_type: Type,
    pub arguments: Arguments,
    pub block: Block,
    pub name: NameID,
    pub pos: helpers::Pos
}

pub enum TypeType {
    Type(String),
    Tuple(Vec<TypeType>)
}

pub struct Type {
    pub value: TypeType,
    pub pos: helpers::Pos
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
