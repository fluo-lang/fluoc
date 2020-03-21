
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
    value: String
}

pub struct RefID {
    value: String
}

pub struct Reference {
    value: RefID
}

// NODES ---------------------------------------
pub struct NameID {
    value: String
}

pub struct VariableAssign {
    name: NameID,
    expr: Box<dyn Expr>
}

pub struct VariableAssignDeclaration {
    t: Type,
    name: NameID,
    expr: Box<dyn Expr>
}

pub struct Type {
    value: String
}

pub struct VariableDeclaration {
    t: Type,
    name: NameID
}

pub struct Arguments {
    positional: Vec<(String, Type)>,
    // TODO: Add more types or arguments
}

pub struct Block {
    statements: Vec<Box<dyn Node>>
}

pub struct FunctionDefine {
    return_type: Type,
    arguments: Arguments,
    block: Block
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
