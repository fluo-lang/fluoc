use crate::helpers;
use std::fmt::Debug;
use std::fmt;
use std::hash::{Hash, Hasher};
use crate::parser::custom_syntax::{ Custom, Pattern, Terminal, NonTerminal, Impl };

// EXPRESSIONS ---------------------------------------

#[derive(Debug)]
/// Empty Placeholder value
pub struct Empty {
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Integer node
pub struct Integer {
    pub value: String,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// String literal node
pub struct StringLiteral {
    pub value: String,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Dollar sign id (i.e. `$myvar`) node
pub struct DollarID {
    pub value: NameID,
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

#[derive(Debug, Eq)]
/// Name ID node
pub struct NameID {
    pub value: String,
    pub pos: helpers::Pos
}

impl Hash for NameID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl PartialEq for NameID {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
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

impl fmt::Display for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rep = String::new();
        for scope in &self.scopes {
            rep += &scope.value;
            rep += "::";
        }
        write!(f, "{}", rep)
    }
}

#[derive(Debug)]
pub struct Nodes {
    pub nodes: Vec<Node>,
    pub pos: helpers::Pos
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
    Pattern(Pattern),

    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
    FunctionDefine(FunctionDefine),
    ImplDefine(Impl),

    StringLiteral(StringLiteral),
    Terminal(Terminal),
    NonTerminal(NonTerminal),
    DollarID(DollarID),
    Nodes(Nodes),

    Empty(Empty)
}

#[derive(Debug)]
pub enum Statement {
    Custom(Custom),

    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
    FunctionDefine(FunctionDefine),
    ImplDefine(Impl),

    Empty(Empty)
}

impl Statement {
    pub fn pos(&self) -> helpers::Pos {
        match &self {
            Statement::Custom(val) => val.pos,
            Statement::ExpressionStatement(val) => val.pos,
            Statement::VariableDeclaration(val) => val.pos,
            Statement::FunctionDefine(val) => val.pos,
            Statement::ImplDefine(val) => val.pos,
            Statement::Empty(val) => val.pos
        }
    }

    pub fn to_string(&self) -> String {
        match &self {
            Statement::Custom(val) => val.rep.clone(),
            Statement::ExpressionStatement(val) => format!("{}", val.expression.to_str()),
            Statement::VariableDeclaration(_) => "variable declaration".to_string(),
            Statement::FunctionDefine(_) => "function define".to_string(),
            Statement::ImplDefine(_) => "impl statement".to_string(),
            Statement::Empty(_) => "empty statement".to_string()
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
            Statement::ImplDefine(_) => &Scope::Outer,
            Statement::Empty(_) => &Scope::All
        }
    }

    pub fn into_node(self) -> Node {
        match self {
            Statement::Custom(val) => Node::Custom(val),
            Statement::ExpressionStatement(val) => Node::ExpressionStatement(val),
            Statement::VariableDeclaration(val) => Node::VariableDeclaration(val),
            Statement::FunctionDefine(val) => Node::FunctionDefine(val),
            Statement::ImplDefine(val) => Node::ImplDefine(val),
            Statement::Empty(val) => Node::Empty(val)
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
    Mod(Mod),
    StringLiteral(StringLiteral),

    DollarID(DollarID),

    Empty(Empty)
}

impl Expr {
    pub fn pos(&self) -> helpers::Pos {
        match &self {
            Expr::Integer(val) => val.pos,

            Expr::Custom(val) => val.pos,
            
            Expr::RefID(val) => val.pos,
            Expr::DollarID(val) => val.pos,
            Expr::Reference(val) => val.pos,
            Expr::VariableAssign(val) => val.pos,
            Expr::VariableAssignDeclaration(val) => val.pos,
            Expr::FunctionCall(val) => val.pos,
            Expr::StringLiteral(val) => val.pos,

            Expr::Add(val) => val.pos,
            Expr::Sub(val) => val.pos,
            Expr::Neg(val) => val.pos,
            Expr::Mul(val) => val.pos,
            Expr::Div(val) => val.pos,
            Expr::Mod(val) => val.pos,

            Expr::Empty(val) => val.pos
        }
    }

    pub fn to_str<'a>(&'a self) -> &'a str {
        match self {
            Expr::Integer(_) => "integer",

            Expr::Custom(val) => &val.rep,
            
            Expr::RefID(_) => "ID",
            Expr::DollarID(_) => "dollar sign ID",
            Expr::Reference(_) => "refrence",
            Expr::VariableAssign(_) => "variable assign",
            Expr::VariableAssignDeclaration(_) => "variable assignment declaration",
            Expr::FunctionCall(_) => "function call",
            Expr::StringLiteral(_) => "string literal",

            Expr::Add(_) => "add",
            Expr::Sub(_) => "subtract",
            Expr::Neg(_) => "negate",
            Expr::Mul(_) => "multiply",
            Expr::Div(_) => "divide",
            Expr::Mod(_) => "modulo",

            Expr::Empty(_) => "empty"
        }
    }
}
