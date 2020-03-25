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

/// Integer node
pub struct Integer {
    pub value: String,
    pub pos: helpers::Pos
}

/// Reference ID (i.e. pass by value) node
pub struct RefID {
    pub value: String,
    pub pos: helpers::Pos
}

/// Reference (i.e. pass by reference) node
pub struct Reference {
    pub value: RefID,
    pub pos: helpers::Pos
}

// NODES ---------------------------------------	

/// Name ID node
pub struct NameID {
    pub value: String,
    pub pos: helpers::Pos
}

/// Variable Assign i.e.:
/// ```
/// x = 10;
/// ```
pub struct VariableAssign {
    pub name: NameID,
    pub expr: Box<dyn Expr>,
    pub pos: helpers::Pos
}

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

/// Arguments for function
pub struct Arguments {
    pub positional: Vec<(String, Type)>,
    pub pos: helpers::Pos
    // TODO: Add more types or arguments
}

/// Block of code
pub struct Block {
    pub nodes: Vec<Box<dyn Node>>,
    pub pos: helpers::Pos
}

/// Function definition
pub struct FunctionDefine {
    pub return_type: Type,
    pub arguments: Arguments,
    pub block: Block,
    pub name: NameID,
    pub pos: helpers::Pos
}

/// Type Types
pub enum TypeType {
    Type(String),
    Tuple(Vec<TypeType>)
}

/// Type Node
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
    Arguments, 
    Block, 
    FunctionDefine
);
