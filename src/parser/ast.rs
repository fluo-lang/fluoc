use crate::helpers;
use std::fmt::Debug;

pub trait Node: Debug { }
pub trait Expr: Debug { }

macro_rules! impl_node {
    ($($t:ty),+) => {
        $(impl Node for $t {  })+
    }
}

macro_rules! impl_expr {
    ($($t:ty),+) => {
        $(impl Expr for $t {  })+
    }
}

// EXPRESSIONS ---------------------------------------

#[derive(Debug)]
/// Integer node
pub struct Integer {
    pub value: String,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Reference ID (i.e. pass by value) node
pub struct RefID {
    pub value: String,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Reference (i.e. pass by reference) node
pub struct Reference {
    pub value: RefID,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Addition Node
pub struct Add {
    pub left: Box<dyn Expr>,
    pub right: Box<dyn Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Multiplication Node
pub struct Mul {
    pub left: Box<dyn Expr>,
    pub right: Box<dyn Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Division Node
pub struct Div {
    pub left: Box<dyn Expr>,
    pub right: Box<dyn Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Addition Node
pub struct Sub {
    pub left: Box<dyn Expr>,
    pub right: Box<dyn Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct Mod {
    pub left: Box<dyn Expr>,
    pub right: Box<dyn Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct Neg {
    pub value: Box<dyn Expr>,
    pub pos: helpers::Pos
}

// NODES ---------------------------------------	

#[derive(Debug)]
/// Name ID node
pub struct NameID {
    pub value: String,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Variable Assign i.e.:
/// ```
/// x = 10;
/// ```
pub struct VariableAssign {
    pub name: NameID,
    pub expr: Box<dyn Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Variable Assign + Declaration i.e.:
/// ```
/// let x: int = 10;
/// ```
pub struct VariableAssignDeclaration {
    pub t: Type,
    pub name: NameID,
    pub expr: Box<dyn Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Arguments for function
pub struct Arguments {
    pub positional: Vec<(String, Type)>,
    pub pos: helpers::Pos
    // TODO: Add more types or arguments
}

#[derive(Debug)]
/// Block of code
pub struct Block {
    pub nodes: Vec<Box<dyn Node>>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Function definition
pub struct FunctionDefine {
    pub return_type: Type,
    pub arguments: Arguments,
    pub block: Block,
    pub name: NameID,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Box<dyn Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Type Types
pub enum TypeType {
    Type(String),
    Tuple(Vec<TypeType>)
}

#[derive(Debug)]
/// Type Node
pub struct Type {
    pub value: TypeType,
    pub pos: helpers::Pos
}

impl_node!(
    Integer, 
    RefID, 
    Reference, 
    NameID, 
    VariableAssign, 
    VariableAssignDeclaration, 
    Type, 
    Arguments, 
    Block, 
    FunctionDefine,
    ExpressionStatement
);

impl_expr!(
    Integer, 
    RefID, 
    Reference,
    VariableAssign,
    VariableAssignDeclaration,
    Add,
    Sub,
    Neg,
    Mul,
    Div,
    Mod
);
