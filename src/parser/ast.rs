use crate::helpers;
use crate::lexer::Token;
use crate::typecheck::ast_typecheck::TypeCheckType;
use std::fmt;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// EXPRESSIONS ---------------------------------------

#[derive(Debug, Clone, PartialEq)]
/// Empty Placeholder value
pub struct Empty<'a> {
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Integer node
pub struct Integer<'a> {
    pub value: &'a str,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Tuple node
pub struct Tuple<'a> {
    pub values: Vec<Expr<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// String literal node
pub struct StringLiteral<'a> {
    pub value: &'a str,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Dollar sign id (i.e. `$myvar`) node
pub struct DollarID<'a> {
    pub value: Rc<Namespace<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Reference ID (i.e. pass by value) node
pub struct RefID<'a> {
    pub value: Rc<Namespace<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Reference (i.e. pass by reference) node
pub struct Reference<'a> {
    pub value: RefID<'a>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Infix<'a> {
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
    pub operator: Token<'a>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prefix<'a> {
    pub val: Box<Expr<'a>>,
    pub operator: Token<'a>,
    pub pos: helpers::Pos<'a>,
}

// NODES ---------------------------------------

#[derive(Debug, Eq, Clone, Copy)]
/// Name ID node
pub struct NameID<'a> {
    pub value: &'a str,
    pub pos: helpers::Pos<'a>,
}

impl<'a> Hash for NameID<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<'a> PartialEq for NameID<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Variable Assign i.e.:
///
/// x = 10;
pub struct VariableAssign<'a> {
    pub name: Rc<Namespace<'a>>,
    pub expr: Box<Expr<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Type Assign i.e.:
///
/// type km = int;
pub struct TypeAssign<'a> {
    pub name: Rc<Namespace<'a>>,
    pub value: TypeCheckOrType<'a>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Variable Assign + Declaration i.e.:
///
/// let x: int = 10;
pub struct VariableAssignDeclaration<'a> {
    pub t: TypeCheckOrType<'a>,
    pub name: Rc<Namespace<'a>>,
    pub expr: Box<Expr<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Variable Declaration i.e.:
///
/// let x: int;
pub struct VariableDeclaration<'a> {
    pub t: TypeCheckOrType<'a>,
    pub name: Rc<Namespace<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Arguments for function
pub struct Arguments<'a> {
    pub positional: Vec<(NameID<'a>, TypeCheckOrType<'a>)>,
    pub pos: helpers::Pos<'a>, // TODO: Add more types of arguments
}

#[derive(Debug, Clone, PartialEq)]
/// Block of code
pub struct Block<'a> {
    pub nodes: Vec<Statement<'a>>,
    pub pos: helpers::Pos<'a>,
}

impl<'a> Block<'a> {
    pub fn to_string(&self) -> String {
        let mut val = String::new();
        for node in &self.nodes {
            val += &node.to_string();
            val += "\n";
        }
        val
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Function definition
pub struct FunctionDefine<'a> {
    pub return_type: TypeCheckOrType<'a>,
    pub arguments: Arguments<'a>,
    pub block: Block<'a>,
    pub name: Rc<NameID<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArgumentsRun<'a> {
    pub positional: Vec<Expr<'a>>,
    pub pos: helpers::Pos<'a>, // TODO: Add more types of arguments
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return<'a> {
    pub expression: Box<Expr<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Function definition
pub struct FunctionCall<'a> {
    pub arguments: ArgumentsRun<'a>,
    pub name: Rc<Namespace<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement<'a> {
    pub expression: Box<Expr<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, PartialEq, Clone)]
/// TypecheckType or TypeType
pub enum TypeCheckOrType<'a> {
    Type(Rc<Type<'a>>),
    TypeCheckType(TypeCheckType<'a>)
}

impl<'a> TypeCheckOrType<'a> {
    pub fn unwrap_type(&self) -> Rc<Type<'a>> {
        match self {
            TypeCheckOrType::Type(val) => Rc::clone(val),
            TypeCheckOrType::TypeCheckType(_) => panic!("TypeCheckType failed to unwrap on type")
        }
    }

    pub fn unwrap_type_check(self) -> TypeCheckType<'a> {
        match self {
            TypeCheckOrType::Type(_) => panic!("TypeCheckType failed to unwrap on type"),
            TypeCheckOrType::TypeCheckType(val) => val
        }
    }

    pub fn unwrap_type_check_ref<'b>(&'b self) -> &TypeCheckType<'a> {
        match self {
            TypeCheckOrType::Type(_) => panic!("TypeCheckType failed to unwrap on type"),
            TypeCheckOrType::TypeCheckType(val) => val
        }
    }

    pub fn as_typecheck_type<'b>(_self: std::borrow::Cow<'b, Self>) -> TypeCheckType<'a> {
        match _self {
            std::borrow::Cow::Borrowed(value) => match value {
                TypeCheckOrType::Type(val) => TypeCheckType::as_type(Rc::clone(&val)),
                TypeCheckOrType::TypeCheckType(val) => val.clone()
            }
            std::borrow::Cow::Owned(value) => match value {
                TypeCheckOrType::Type(val) => TypeCheckType::as_type(Rc::clone(&val)),
                TypeCheckOrType::TypeCheckType(val) => val
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Type Types
pub enum TypeType<'a> {
    Type(Rc<Namespace<'a>>),
    Tuple(Vec<Rc<Type<'a>>>),
}

impl<'a> fmt::Display for TypeType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rep = match &self {
            TypeType::Type(namespace) => namespace.to_string(),
            TypeType::Tuple(types) => {
                let mut final_string = String::new();
                for type_val in types.iter() {
                    final_string.push_str(&type_val.to_string()[..]);
                }
                final_string
            }
        };
        write!(f, "{}", rep)
    }
}

#[derive(Debug, Clone)]
/// Type Node
pub struct Type<'a> {
    pub value: TypeType<'a>,
    pub inferred: bool,
    pub pos: helpers::Pos<'a>,
}

impl<'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, Clone)]
/// This::is::a::namespace!
pub struct Namespace<'a> {
    pub scopes: Vec<NameID<'a>>,
    pub pos: helpers::Pos<'a>,
}

impl<'a> PartialEq for Namespace<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.scopes == other.scopes
    }
}

impl<'a> Hash for Namespace<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for scope in &self.scopes {
            scope.hash(state);
        }
    }
}

impl<'a> fmt::Display for Namespace<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.scopes
                .iter()
                .map(|value| value.value)
                .collect::<Vec<&str>>()
                .join("::")
        )
    }
}

impl<'a> Namespace<'a> {
    pub fn from_name_id(value: NameID<'a>) -> Rc<Namespace<'a>> {
        Rc::new(Namespace {
            pos: value.pos,
            scopes: vec![value],
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Nodes<'a> {
    pub nodes: Vec<Node<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone)]
pub enum Scope {
    Block,
    Outer,
    All,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Node<'a> {
    Integer(Integer<'a>),
    RefID(RefID<'a>),
    Reference(Reference<'a>),
    NameID(NameID<'a>),
    VariableAssign(VariableAssign<'a>),
    VariableAssignDeclaration(VariableAssignDeclaration<'a>),
    TypeAssign(TypeAssign<'a>),

    Type(Type<'a>),
    Arguments(Arguments<'a>),
    Block(Block<'a>),

    Infix(Infix<'a>),
    Prefix(Prefix<'a>),

    Tuple(Tuple<'a>),

    ExpressionStatement(ExpressionStatement<'a>),
    VariableDeclaration(VariableDeclaration<'a>),
    FunctionDefine(FunctionDefine<'a>),

    StringLiteral(StringLiteral<'a>),
    DollarID(DollarID<'a>),
    Nodes(Nodes<'a>),
    Return(Return<'a>),

    Empty(Empty<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    ExpressionStatement(ExpressionStatement<'a>),
    VariableDeclaration(VariableDeclaration<'a>),
    FunctionDefine(FunctionDefine<'a>),
    Return(Return<'a>),
    TypeAssign(TypeAssign<'a>),

    Empty(Empty<'a>),
}

impl<'a> Statement<'a> {
    pub fn pos(&self) -> helpers::Pos<'a> {
        match &self {
            Statement::ExpressionStatement(val) => val.pos,
            Statement::VariableDeclaration(val) => val.pos,
            Statement::FunctionDefine(val) => val.pos,
            Statement::Return(val) => val.pos,
            Statement::TypeAssign(val) => val.pos,

            Statement::Empty(val) => val.pos,
        }
    }

    pub fn to_string(&self) -> String {
        match &self {
            Statement::ExpressionStatement(val) => val.expression.to_str().to_string(),
            Statement::VariableDeclaration(_) => "variable declaration".to_string(),
            Statement::FunctionDefine(val) => {
                format!("function define {{\n{}}}", val.block.to_string())
            }
            Statement::Return(_) => "return statement".to_string(),
            Statement::TypeAssign(_) => "type assignment".to_string(),

            Statement::Empty(_) => "empty statement".to_string(),
        }
    }

    pub fn in_scope(&'a self, check_scope: &Scope) -> bool {
        Statement::get_scope(self) == check_scope
    }

    pub fn get_scope(statement: &Statement) -> &'a Scope {
        match statement {
            Statement::ExpressionStatement(_) => &Scope::Block,
            Statement::VariableDeclaration(_) => &Scope::Block,
            Statement::FunctionDefine(_) => &Scope::All,
            Statement::Return(_) => &Scope::Block,
            Statement::TypeAssign(_) => &Scope::All,

            Statement::Empty(_) => &Scope::All,
        }
    }

    pub fn into_node(self) -> Node<'a> {
        match self {
            Statement::ExpressionStatement(val) => Node::ExpressionStatement(val),
            Statement::VariableDeclaration(val) => Node::VariableDeclaration(val),
            Statement::FunctionDefine(val) => Node::FunctionDefine(val),
            Statement::Return(val) => Node::Return(val),
            Statement::TypeAssign(val) => Node::TypeAssign(val),

            Statement::Empty(val) => Node::Empty(val),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Integer(Integer<'a>),

    RefID(RefID<'a>),
    Reference(Reference<'a>),
    VariableAssign(VariableAssign<'a>),
    VariableAssignDeclaration(VariableAssignDeclaration<'a>),
    FunctionCall(FunctionCall<'a>),

    Infix(Infix<'a>),
    Prefix(Prefix<'a>),

    StringLiteral(StringLiteral<'a>),
    Tuple(Tuple<'a>),

    DollarID(DollarID<'a>),

    Empty(Empty<'a>),

    FunctionDefine(FunctionDefine<'a>),
}

impl<'a> Expr<'a> {
    pub fn pos(&self) -> helpers::Pos<'a> {
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

    pub fn to_str(&'a self) -> &'a str {
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
            Expr::FunctionDefine(_) => "function define",
        }
    }
}
