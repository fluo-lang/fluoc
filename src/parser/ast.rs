use crate::helpers;
use crate::lexer::Token;
use crate::logger::logger::ErrorOrVec;
use crate::typecheck::ast_typecheck::{TypeCheckSymbTab, TypeCheckType};

use inkwell::module::Linkage;
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

#[derive(Debug, Clone, PartialEq, Copy)]
/// Special Compiler Tags
pub struct Tag<'a> {
    pub content: NameID<'a>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'a> {
    pub value: &'a str,
    pub type_val: Option<TypeCheckType<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import<'a> {
    pub namespace: Namespace<'a>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Tuple node
pub struct Tuple<'a> {
    pub values: Vec<Expr<'a>>,
    pub type_val: Option<TypeCheckType<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Dollar sign id (i.e. `$myvar`) node
pub struct DollarID<'a> {
    pub value: Rc<Namespace<'a>>,
    pub type_val: Option<TypeCheckType<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Reference ID (i.e. pass by value) node
pub struct RefID<'a> {
    pub value: Rc<Namespace<'a>>,
    pub type_val: Option<TypeCheckType<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Reference (i.e. pass by reference) node
pub struct Reference<'a> {
    pub value: RefID<'a>,
    pub type_val: Option<TypeCheckType<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Infix<'a> {
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
    pub operator: Token<'a>,
    pub type_val: Option<TypeCheckType<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prefix<'a> {
    pub val: Box<Expr<'a>>,
    pub operator: Token<'a>,
    pub type_val: Option<TypeCheckType<'a>>,
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

impl<'a> NameID<'a> {
    pub fn into_namespace(self) -> Namespace<'a> {
        Namespace {
            scopes: vec![self],
            pos: self.pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Variable Assign i.e.:
///
/// x = 10;
pub struct VariableAssign<'a> {
    pub name: Rc<Namespace<'a>>,
    pub expr: Box<Expr<'a>>,
    pub type_val: Option<TypeCheckType<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Type Assign i.e.:
///
/// type km = int;
pub struct TypeAssign<'a> {
    pub name: Rc<Namespace<'a>>,
    pub value: TypeCheckOrType<'a>,
    pub visibility: Visibility,
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
pub struct Unit<'a> {
    pub name: Namespace<'a>,
    pub pos: helpers::Pos<'a>,
    pub block: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Arguments for function
pub struct Arguments<'a> {
    pub positional: Vec<(NameID<'a>, TypeCheckOrType<'a>)>,
    pub pos: helpers::Pos<'a>, // TODO: Add more types of arguments
}

#[derive(Debug, Clone, PartialEq)]
/// If/else if conditional branch
pub struct IfBranch<'a> {
    pub cond: Expr<'a>,
    pub block: Block<'a>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Else conditional branch
pub struct ElseBranch<'a> {
    pub block: Block<'a>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Conditional<'a> {
    pub if_branches: Vec<IfBranch<'a>>,
    pub else_branch: Option<ElseBranch<'a>>,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// Block of code
pub struct Block<'a> {
    pub nodes: Vec<Statement<'a>>,
    pub tags: Vec<Tag<'a>>, // For now, tags are just strings
    pub pos: helpers::Pos<'a>,
    pub insert_return: bool,
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
pub struct Units<'a> {
    pub units: Vec<Unit<'a>>,
    pub pos: helpers::Pos<'a>,
}

impl<'a> Units<'a> {
    pub fn into_block(self) -> Block<'a> {
        Block {
            nodes: self.units.into_iter().map(|x| Statement::Unit(x)).collect(),
            tags: Vec::new(),
            pos: self.pos,
            insert_return: false,
        }
    }

    pub fn into_statements(self) -> Vec<Statement<'a>> {
        self.units.into_iter().map(|x| Statement::Unit(x)).collect()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Visibility::Public => "public",
                Visibility::Private => "private",
            }
        )
    }
}

impl Visibility {
    pub fn get_linkage(&self) -> Linkage {
        match self {
            Visibility::Private => Linkage::Private,
            Visibility::Public => Linkage::External,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Function definition
pub struct FunctionDefine<'a> {
    pub return_type: TypeCheckOrType<'a>,
    pub arguments: Arguments<'a>,
    pub block: Block<'a>,
    pub name: Rc<Namespace<'a>>,
    pub visibility: Visibility,
    pub pos: helpers::Pos<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// External function definition
pub struct ExternDef<'a> {
    pub return_type: TypeCheckOrType<'a>,
    pub arguments: Arguments<'a>,
    pub name: Rc<Namespace<'a>>,
    pub visibility: Visibility,
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
    TypeCheckType(TypeCheckType<'a>),
}

impl<'a> TypeCheckOrType<'a> {
    pub fn unwrap_type(&self) -> Rc<Type<'a>> {
        match self {
            TypeCheckOrType::Type(val) => Rc::clone(val),
            TypeCheckOrType::TypeCheckType(_) => panic!("TypeCheckType failed to unwrap on type"),
        }
    }

    pub fn unwrap_type_check(self) -> TypeCheckType<'a> {
        match self {
            TypeCheckOrType::Type(_) => panic!("TypeCheckType failed to unwrap on type"),
            TypeCheckOrType::TypeCheckType(val) => val,
        }
    }

    pub fn unwrap_type_check_ref<'b>(&'b self) -> &TypeCheckType<'a> {
        match self {
            TypeCheckOrType::Type(_) => panic!("TypeCheckType failed to unwrap on type"),
            TypeCheckOrType::TypeCheckType(val) => val,
        }
    }

    pub fn as_typecheck_type<'b, 'c>(
        _self: std::borrow::Cow<'b, Self>,
        context: &'c mut TypeCheckSymbTab<'a>,
    ) -> Result<TypeCheckType<'a>, ErrorOrVec<'a>> {
        match _self {
            std::borrow::Cow::Borrowed(value) => match value {
                TypeCheckOrType::Type(val) => {
                    TypeCheckType::from_type(Rc::clone(&val), context, false)
                }
                TypeCheckOrType::TypeCheckType(val) => Ok(val.clone()),
            },
            std::borrow::Cow::Owned(value) => match value {
                TypeCheckOrType::Type(val) => {
                    TypeCheckType::from_type(Rc::clone(&val), context, false)
                }
                TypeCheckOrType::TypeCheckType(val) => Ok(val),
            },
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

    pub fn as_vec_nameid(&mut self) -> &mut Vec<NameID<'a>> {
        &mut self.scopes
    }

    pub fn prepend_namespace(&mut self, mut other: Vec<NameID<'a>>) -> Namespace<'a> {
        std::mem::swap(&mut self.scopes, &mut other); // Put into other
        self.scopes.append(&mut other); // Append self.scopes
        self.clone()
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
    RefID(RefID<'a>),
    Reference(Reference<'a>),
    NameID(NameID<'a>),

    VariableAssign(VariableAssign<'a>),
    VariableAssignDeclaration(VariableAssignDeclaration<'a>),
    VariableDeclaration(VariableDeclaration<'a>),

    TypeAssign(TypeAssign<'a>),

    Type(Type<'a>),
    Arguments(Arguments<'a>),
    Block(Block<'a>),

    Infix(Infix<'a>),
    Prefix(Prefix<'a>),

    Tuple(Tuple<'a>),

    Tag(Tag<'a>),

    ExpressionStatement(ExpressionStatement<'a>),

    FunctionDefine(FunctionDefine<'a>),
    FunctionCall(FunctionCall<'a>),

    Conditional(Conditional<'a>),

    ExternDef(ExternDef<'a>),

    Unit(Unit<'a>),

    Import(Import<'a>),

    Literal(Literal<'a>),
    DollarID(DollarID<'a>),
    Nodes(Nodes<'a>),
    Return(Return<'a>),

    Empty(Empty<'a>),
    Units(Units<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    ExpressionStatement(ExpressionStatement<'a>),
    VariableDeclaration(VariableDeclaration<'a>),

    FunctionDefine(FunctionDefine<'a>),
    ExternDef(ExternDef<'a>),

    Conditional(Conditional<'a>),

    Return(Return<'a>),
    Unit(Unit<'a>),
    TypeAssign(TypeAssign<'a>),
    Import(Import<'a>),

    Empty(Empty<'a>),
    Tag(Tag<'a>),
}

impl<'a> Statement<'a> {
    pub fn pos(&self) -> helpers::Pos<'a> {
        match &self {
            Statement::ExpressionStatement(val) => val.pos,
            Statement::VariableDeclaration(val) => val.pos,

            Statement::FunctionDefine(val) => val.pos,
            Statement::ExternDef(val) => val.pos,

            Statement::Conditional(val) => val.pos,

            Statement::Return(val) => val.pos,
            Statement::Unit(val) => val.pos,
            Statement::TypeAssign(val) => val.pos,
            Statement::Import(val) => val.pos,
            Statement::Tag(val) => val.pos,

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
            Statement::ExternDef(_) => "external function define".to_string(),

            Statement::Conditional(_) => "conditional".to_string(),

            Statement::Unit(_) => "unit".to_string(),
            Statement::Import(_) => "import".to_string(),
            Statement::Return(_) => "return statement".to_string(),
            Statement::TypeAssign(_) => "type assignment".to_string(),
            Statement::Tag(_) => "compiler tag".to_string(),

            Statement::Empty(_) => "empty statement".to_string(),
        }
    }

    pub fn in_scope(&'a self, check_scope: &Scope) -> bool {
        Statement::get_scope(self) == check_scope
    }

    pub fn get_scope(statement: &Statement<'_>) -> &'a Scope {
        match statement {
            Statement::ExpressionStatement(_) => &Scope::Block,
            Statement::VariableDeclaration(_) => &Scope::Block,

            Statement::FunctionDefine(_) => &Scope::All,
            Statement::ExternDef(_) => &Scope::All,

            Statement::Conditional(_) => &Scope::Block,

            Statement::Return(_) => &Scope::Block,
            Statement::TypeAssign(_) => &Scope::All,
            Statement::Unit(_) => &Scope::Outer,
            Statement::Import(_) => &Scope::Outer,
            Statement::Tag(_) => &Scope::All,

            Statement::Empty(_) => &Scope::All,
        }
    }

    pub fn into_node(self) -> Node<'a> {
        match self {
            Statement::ExpressionStatement(val) => Node::ExpressionStatement(val),
            Statement::VariableDeclaration(val) => Node::VariableDeclaration(val),

            Statement::FunctionDefine(val) => Node::FunctionDefine(val),
            Statement::ExternDef(val) => Node::ExternDef(val),

            Statement::Conditional(val) => Node::Conditional(val),

            Statement::Return(val) => Node::Return(val),
            Statement::TypeAssign(val) => Node::TypeAssign(val),
            Statement::Import(val) => Node::Import(val),
            Statement::Unit(val) => Node::Unit(val),
            Statement::Tag(val) => Node::Tag(val),

            Statement::Empty(val) => Node::Empty(val),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),

    RefID(RefID<'a>),
    Reference(Reference<'a>),
    VariableAssign(VariableAssign<'a>),
    VariableAssignDeclaration(VariableAssignDeclaration<'a>),
    FunctionCall(FunctionCall<'a>),

    Infix(Infix<'a>),
    Prefix(Prefix<'a>),
    Tuple(Tuple<'a>),

    DollarID(DollarID<'a>),

    Empty(Empty<'a>),

    FunctionDefine(FunctionDefine<'a>),
}

impl<'a> Expr<'a> {
    pub fn pos(&self) -> helpers::Pos<'a> {
        match &self {
            Expr::Literal(val) => val.pos,
            Expr::RefID(val) => val.pos,
            Expr::DollarID(val) => val.pos,
            Expr::Reference(val) => val.pos,
            Expr::VariableAssign(val) => val.pos,
            Expr::VariableAssignDeclaration(val) => val.pos,
            Expr::FunctionCall(val) => val.pos,
            Expr::Tuple(val) => val.pos,

            Expr::Infix(val) => val.pos,
            Expr::Prefix(val) => val.pos,

            Expr::Empty(val) => val.pos,
            Expr::FunctionDefine(val) => val.pos,
        }
    }

    pub fn type_val(&self) -> helpers::Pos<'a> {
        match &self {
            Expr::Literal(type_val) => type_val.pos,
            Expr::RefID(type_val) => type_val.pos,
            Expr::DollarID(type_val) => type_val.pos,
            Expr::Reference(type_val) => type_val.pos,
            Expr::VariableAssign(type_val) => type_val.pos,
            Expr::VariableAssignDeclaration(type_val) => type_val.pos,
            Expr::FunctionCall(type_val) => type_val.pos,
            Expr::Tuple(type_val) => type_val.pos,

            Expr::Infix(type_val) => type_val.pos,
            Expr::Prefix(type_val) => type_val.pos,

            Expr::Empty(type_val) => type_val.pos,
            Expr::FunctionDefine(type_val) => type_val.pos,
        }
    }

    pub fn to_str(&'a self) -> &'a str {
        match self {
            Expr::Literal(_) => "literal",
            Expr::RefID(_) => "ID",
            Expr::DollarID(_) => "dollar sign ID",
            Expr::Reference(_) => "refrence",
            Expr::VariableAssign(_) => "variable assign",
            Expr::VariableAssignDeclaration(_) => "variable assignment declaration",
            Expr::FunctionCall(_) => "function call",
            Expr::Tuple(_) => "tuple",

            Expr::Infix(_) => "infix",
            Expr::Prefix(_) => "prefix",

            Expr::Empty(_) => "empty",
            Expr::FunctionDefine(_) => "function define",
        }
    }
}
