use crate::helpers;

use std::fmt::Debug;
use std::fmt;
use std::hash::{Hash, Hasher};
use crate::lexer::Token;

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
/// Tuple node
pub struct Tuple {
    pub values: Vec<Expr>,
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
pub struct Infix {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Token,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct Prefix {
    pub val: Box<Expr>,
    pub operator: Token,
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
///
/// x = 10;
pub struct VariableAssign {
    pub name: Namespace,
    pub expr: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Variable Assign + Declaration i.e.:
/// 
/// let x: int = 10;
pub struct VariableAssignDeclaration {
    pub t: Type,
    pub name: Namespace,
    pub expr: Box<Expr>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Variable Declaration i.e.:
///
/// let x: int;
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

impl Block {
    pub fn to_string(&self) -> String {
        let mut val = String::new();
        for node in &self.nodes {
            val += &node.to_string();
            val += "\n";
        }
        val
    }
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
pub struct Return {
    pub expression: Expr,
    pub pos: helpers::Pos
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
    Tuple(Vec<Type>)
}

#[derive(Debug)]
/// Type Node
pub struct Type {
    pub value: TypeType,
    pub inferred: bool,
    pub pos: helpers::Pos
}

#[derive(Debug, Hash, Eq)]
/// This::is::a::namespace!
pub struct Namespace {
    pub scopes: Vec<NameID>,
    pub pos: helpers::Pos
}

impl PartialEq for Namespace {
    fn eq(&self, other: &Self) -> bool {
        self.scopes == other.scopes
    }
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

    Infix(Infix),
    Prefix(Prefix),

    Tuple(Tuple),

    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
    FunctionDefine(FunctionDefine),

    StringLiteral(StringLiteral),
    DollarID(DollarID),
    Nodes(Nodes),
    
    Return(Return),

    Empty(Empty)
}

#[derive(Debug)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
    FunctionDefine(FunctionDefine),
    Return(Return),

    Empty(Empty)
}

impl Statement {
    pub fn pos(&self) -> helpers::Pos {
        match &self {
            Statement::ExpressionStatement(val) => val.pos,
            Statement::VariableDeclaration(val) => val.pos,
            Statement::FunctionDefine(val) => val.pos,
            Statement::Return(val) => val.pos,

            Statement::Empty(val) => val.pos
        }
    }

    pub fn to_string(&self) -> String {
        match &self {
            Statement::ExpressionStatement(val) => format!("{}", val.expression.to_str()),
            Statement::VariableDeclaration(_) => "variable declaration".to_string(),
            Statement::FunctionDefine(val) => { 
                format!("function define {{\n{}}}", val.block.to_string())
            },
            Statement::Return(_) => "return statement".to_string(),

            Statement::Empty(_) => "empty statement".to_string()
        }
    }

    pub fn in_scope<'a>(&'a self, check_scope: &Scope) -> bool {
        Statement::get_scope(self) == check_scope
    }

    pub fn get_scope<'a>(statement: &Statement) -> &Scope {
        match statement {
            Statement::ExpressionStatement(_) => &Scope::Block,
            Statement::VariableDeclaration(_) => &Scope::Block,
            Statement::FunctionDefine(_) => &Scope::All,
            Statement::Return(_) => &Scope::Block,

            Statement::Empty(_) => &Scope::All
        }
    }

    pub fn into_node(self) -> Node {
        match self {
            Statement::ExpressionStatement(val) => Node::ExpressionStatement(val),
            Statement::VariableDeclaration(val) => Node::VariableDeclaration(val),
            Statement::FunctionDefine(val) => Node::FunctionDefine(val),
            Statement::Return(val) => Node::Return(val),

            Statement::Empty(val) => Node::Empty(val)
        } 
    }
}

#[derive(Debug)]
pub enum Expr {
    Integer(Integer), 

    RefID(RefID), 
    Reference(Reference),
    VariableAssign(VariableAssign),
    VariableAssignDeclaration(VariableAssignDeclaration),
    FunctionCall(FunctionCall),

    Infix(Infix),
    Prefix(Prefix),

    StringLiteral(StringLiteral),
    Tuple(Tuple),

    DollarID(DollarID),

    Empty(Empty),

    FunctionDefine(FunctionDefine)
}

impl Expr {
    pub fn pos(&self) -> helpers::Pos {
        match &self {
            Expr::Integer(val) => val.pos,
            
            Expr::RefID(val) => val.pos,
            Expr::DollarID(val) => val.pos,
            Expr::Reference(val) => val.pos,
            Expr::VariableAssign(val) => val.pos,
            Expr::VariableAssignDeclaration(val) => val.pos,
            Expr::FunctionCall(val) => val.pos,
            Expr::StringLiteral(val) => val.pos,
            Expr::Tuple(val) => val.pos,

            Expr::Infix(val) => val.pos,
            Expr::Prefix(val) => val.pos,

            Expr::Empty(val) => val.pos,
            Expr::FunctionDefine(val) => val.pos,
        }
    }

    pub fn to_str<'a>(&'a self) -> &'a str {
        match self {
            Expr::Integer(_) => "integer",
            
            Expr::RefID(_) => "ID",
            Expr::DollarID(_) => "dollar sign ID",
            Expr::Reference(_) => "refrence",
            Expr::VariableAssign(_) => "variable assign",
            Expr::VariableAssignDeclaration(_) => "variable assignment declaration",
            Expr::FunctionCall(_) => "function call",
            Expr::StringLiteral(_) => "string literal",
            Expr::Tuple(_) => "tuple",

            Expr::Infix(_) => "infix",
            Expr::Prefix(_) => "prefix",

            Expr::Empty(_) => "empty",
            Expr::FunctionDefine(_) => "function define"
        }
    }
}
