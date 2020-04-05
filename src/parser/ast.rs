use crate::helpers;
use std::fmt::Debug;
use std::collections::HashMap;

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
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Multiplication Node
pub struct Mul {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Division Node
pub struct Div {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Addition Node
pub struct Sub {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct Mod {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct Neg {
    pub value: Box<Expr>,
    pub pos: helpers::Pos
}

// NODES ---------------------------------------	

#[derive(Debug, Hash, PartialEq, Eq)]
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
    pub name: Namespace,
    pub expr: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Variable Assign + Declaration i.e.:
/// ```
/// let x: int = 10;
/// ```
pub struct VariableAssignDeclaration {
    pub t: Type,
    pub name: Namespace,
    pub expr: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Variable Declaration i.e.:
/// ```
/// let x: int;
/// ```
pub struct VariableDeclaration {
    pub t: Type,
    pub name: Namespace,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Arguments for function
pub struct Arguments {
    pub positional: Vec<(NameID, Type)>,
    pub pos: helpers::Pos
    // TODO: Add more types of arguments
}

#[derive(Debug)]
/// Block of code
pub struct Block {
    pub nodes: Vec<Statement>,
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
pub struct ArgumentsRun {
    pub positional: Vec<Expr>,
    pub pos: helpers::Pos
    // TODO: Add more types of arguments
}

#[derive(Debug)]
/// Function definition
pub struct FunctionCall {
    pub arguments: ArgumentsRun,
    pub name: Namespace,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Type Types
pub enum TypeType {
    Type(Namespace),
    Tuple(Vec<TypeType>)
}

#[derive(Debug)]
/// Type Node
pub struct Type {
    pub value: TypeType,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// This::is::a::namespace!
pub struct Namespace {
    pub scopes: Vec<NameID>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Impl block (for changeable syntax at compile time)
pub struct Impl {
    pub syntax_type: Namespace,
}

#[derive(Debug)]
/// Type of custom node, i.e. statement or expression
pub enum CustomType {
    Statement,
    Expression
}

#[derive(Debug)]
/// User defined syntax ast
pub struct Custom {
    pub custom_type: CustomType,
    pub values: HashMap<NameID, Node>,
    pub name: NameID,
    pub pos: helpers::Pos,
    pub rep: String,
    pub scope: Scope
}

#[derive(Debug, Clone)]
pub enum Scope {
    Block,
    Outer,
    All
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Scope::Block, Scope::Outer) => false,
            (Scope::Outer, Scope::Block) => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub enum Node {
    Integer(Integer), 
    RefID(RefID), 
    Reference(Reference), 
    NameID(NameID), 
    VariableAssign(VariableAssign), 
    VariableAssignDeclaration(VariableAssignDeclaration), 
    Type(Type), 
    Arguments(Arguments), 
    Block(Block),
    Add(Add),
    Sub(Sub),
    Neg(Neg),
    Mul(Mul),
    Div(Div),
    Mod(Mod),

    // We want the custom node to be
    // customizable in terms of where 
    // it can be put
    Custom(Custom),

    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
    FunctionDefine(FunctionDefine)
}

#[derive(Debug)]
pub enum Statement {
    Custom(Custom),

    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
    FunctionDefine(FunctionDefine)
}

impl Statement {
    pub fn pos(&self) -> helpers::Pos {
        match &self {
            Statement::Custom(val) => val.pos,
            Statement::ExpressionStatement(val) => val.pos,
            Statement::VariableDeclaration(val) => val.pos,
            Statement::FunctionDefine(val) => val.pos,
        }
    }

    pub fn to_string(&self) -> String {
        match &self {
            Statement::Custom(val) => val.rep.clone(),
            Statement::ExpressionStatement(val) => format!("{}", val.expression.to_str()),
            Statement::VariableDeclaration(_) => "variable declaration".to_string(),
            Statement::FunctionDefine(_) => "function define".to_string(),
        }
    }

    pub fn in_scope<'a>(&'a self, check_scope: &Scope) -> bool {
        Statement::get_scope(self) == check_scope
    }

    pub fn get_scope<'a>(statement: &Statement) -> &Scope {
        match statement {
            Statement::Custom(val) => &val.scope,
            Statement::ExpressionStatement(_) => &Scope::Block,
            Statement::VariableDeclaration(_) => &Scope::Block,
            Statement::FunctionDefine(_) => &Scope::Outer,
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    // We want the custom node to be
    // customizable in terms of where 
    // it can be put
    Custom(Custom),

    Integer(Integer), 

    RefID(RefID), 
    Reference(Reference),
    VariableAssign(VariableAssign),
    VariableAssignDeclaration(VariableAssignDeclaration),
    FunctionCall(FunctionCall),

    Add(Add),
    Sub(Sub),
    Neg(Neg),
    Mul(Mul),
    Div(Div),
    Mod(Mod)
}

impl Expr {
    pub fn pos(&self) -> helpers::Pos {
        match &self {
            Expr::Integer(val) => val.pos,

            Expr::Custom(val) => val.pos,
            
            Expr::RefID(val) => val.pos,
            Expr::Reference(val) => val.pos,
            Expr::VariableAssign(val) => val.pos,
            Expr::VariableAssignDeclaration(val) => val.pos,
            Expr::FunctionCall(val) => val.pos,

            Expr::Add(val) => val.pos,
            Expr::Sub(val) => val.pos,
            Expr::Neg(val) => val.pos,
            Expr::Mul(val) => val.pos,
            Expr::Div(val) => val.pos,
            Expr::Mod(val) => val.pos
        }
    }

    pub fn to_str<'a>(&'a self) -> &'a str {
        match self {
            Expr::Integer(_) => "integer",

            Expr::Custom(val) => &val.rep,
            
            Expr::RefID(_) => "ID",
            Expr::Reference(_) => "refrence",
            Expr::VariableAssign(_) => "variable assign",
            Expr::VariableAssignDeclaration(_) => "variable assignment declaration",
            Expr::FunctionCall(_) => "function call",

            Expr::Add(_) => "add",
            Expr::Sub(_) => "subtract",
            Expr::Neg(_) => "negate",
            Expr::Mul(_) => "multiply",
            Expr::Div(_) => "divide",
            Expr::Mod(_) => "modulo"
        }
    }
}
