use crate::helpers;
use crate::lexer::{Token, TokenType};
use crate::sourcemap::SourceMap;
use crate::tags::UnitTags;

use inkwell::module::Linkage;

use std::fmt;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// EXPRESSIONS ---------------------------------------

#[derive(Debug, Clone, PartialEq)]
/// Empty Placeholder value
pub struct Empty {
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Special Compiler Tags
pub struct Tag {
    pub content: NameID,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Custom(helpers::Pos),
    Number,
    String,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub literal_type: LiteralType,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub namespace: Namespace,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Tuple node
pub struct Tuple {
    pub values: Vec<Expr>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Dollar sign id (i.e. `$myvar`) node
pub struct DollarID {
    pub value: Rc<Namespace>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Reference ID (i.e. pass by value) node
pub struct RefID {
    pub value: Rc<Namespace>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Reference (i.e. pass by reference) node
pub struct Reference {
    pub value: RefID,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Infix {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Token,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    pub val: Box<Expr>,
    pub operator: Token,
    pub pos: helpers::Pos,
}

// NODES ---------------------------------------

#[derive(Debug)]
/// Name ID node
pub struct NameID {
    pub sourcemap: SourceMap,
    pub pos: helpers::Pos,
}

impl Clone for NameID {
    fn clone(&self) -> Self {
        Self {
            sourcemap: Rc::clone(&self.sourcemap),
            pos: self.pos,
        }
    }
}

#[cfg(test)]
mod name_id_test {
    use super::*;
    use crate::sourcemap::SourceMapInner;
    use std::path::PathBuf;

    #[test]
    fn eq() {
        let sourcemap = SourceMapInner::new();
        sourcemap
            .borrow_mut()
            .insert_file(PathBuf::from("teset.fl"), "hello_hello".to_string());
        let first = NameID {
            sourcemap: Rc::clone(&sourcemap),
            pos: helpers::Pos::new(0, 5, 0),
        };
        let second = NameID {
            sourcemap,
            pos: helpers::Pos::new(6, 11, 0),
        };
        assert_eq!(first, second);
        assert_eq!(second, first);
    }
}

impl Hash for NameID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.sourcemap.borrow().get_segment(self.pos).hash(state);
    }
}

impl PartialEq for NameID {
    fn eq(&self, other: &Self) -> bool {
        self.sourcemap.borrow().get_segment(self.pos)
            == self.sourcemap.borrow().get_segment(other.pos)
    }
}

impl Eq for NameID {}

impl NameID {
    pub fn into_namespace(self) -> Namespace {
        Namespace {
            pos: self.pos,
            scopes: vec![self],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Variable Assign i.e.:
///
/// x = 10;
pub struct VariableAssign {
    pub name: Rc<Namespace>,
    pub expr: Box<Expr>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Type Assign i.e.:
///
/// type km = int;
pub struct TypeAssign {
    pub name: Rc<Namespace>,
    pub value: Type,
    pub visibility: Visibility,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Variable Assign + Declaration i.e.:
///
/// let x: int = 10;
pub struct VariableAssignDeclaration {
    pub ty: Type,
    pub name: Rc<Namespace>,
    pub expr: Box<Expr>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Variable Declaration i.e.:
///
/// let x: int;
pub struct VariableDeclaration {
    pub ty: Type,
    pub name: Rc<Namespace>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unit {
    pub name: Namespace,
    pub pos: helpers::Pos,
    pub block: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
/// Arguments for function
pub struct Arguments {
    pub positional: Vec<(Rc<Namespace>, Type)>,
    pub pos: helpers::Pos, // TODO: Add more types of arguments
}

#[derive(Debug, Clone, PartialEq)]
/// If/else if conditional branch
pub struct IfBranch {
    pub cond: Expr,
    pub block: Block,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Else conditional branch
pub struct ElseBranch {
    pub block: Block,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub if_branches: Vec<IfBranch>,
    pub else_branch: Option<ElseBranch>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Block of code
pub struct Block {
    pub nodes: Vec<Statement>,
    pub tags: UnitTags,
    pub pos: helpers::Pos,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Units {
    pub units: Vec<Unit>,
    pub pos: helpers::Pos,
}

impl Units {
    pub fn into_block(self) -> Block {
        Block {
            nodes: self.units.into_iter().map(|x| Statement::Unit(x)).collect(),
            tags: UnitTags::new(),
            pos: self.pos,
        }
    }

    pub fn into_statements(self) -> Vec<Statement> {
        self.units.into_iter().map(|x| Statement::Unit(x)).collect()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn f(&self) -> &'static str {
        match self {
            Visibility::Public => "public",
            Visibility::Private => "private",
        }
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
pub struct FunctionDefine {
    pub return_type: Type,
    pub arguments: Arguments,
    pub block: Option<Block>, // Option if is an extern
    pub name: Rc<Namespace>,
    pub visibility: Visibility,
    pub overload_operator: Option<TokenType>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArgumentsRun {
    pub positional: Vec<Expr>,
    pub pos: helpers::Pos, // TODO: Add more types of arguments
}

#[derive(Debug, Clone, PartialEq)]
pub struct Yield {
    pub expression: Expr,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub expression: Expr,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// Function definition
pub struct FunctionCall {
    pub arguments: ArgumentsRun,
    pub name: Rc<Namespace>,
    pub mangled_name: Option<String>,
    pub mangle: bool,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Box<Expr>,
    pub pos: helpers::Pos,
}

#[derive(Debug, PartialEq, Clone)]
/// Type Types
pub enum TypeType {
    Type(Rc<Namespace>),
    Tuple(Vec<Type>),
    Unknown,
}

impl TypeType {
    pub fn f(&self, sourcemap: SourceMap) -> String {
        match &self {
            TypeType::Type(namespace) => namespace.to_string(),
            TypeType::Tuple(types) => {
                let mut final_string = String::new();
                for type_val in types.iter() {
                    final_string.push_str(&type_val.f(Rc::clone(&sourcemap))[..]);
                }
                final_string
            }
            TypeType::Unknown => "<unknown>".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
/// Type Node
pub struct Type {
    pub value: TypeType,
    pub pos: helpers::Pos,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Type {
    pub fn f(&self, sourcemap: SourceMap) -> String {
        self.value.f(sourcemap)
    }
}

#[derive(Debug, Eq, Clone)]
/// This::is::a::namespace!
pub struct Namespace {
    pub scopes: Vec<NameID>,
    pub pos: helpers::Pos,
}

impl PartialEq for Namespace {
    fn eq(&self, other: &Self) -> bool {
        self.scopes == other.scopes
    }
}

impl Hash for Namespace {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for scope in &self.scopes {
            scope.hash(state);
        }
    }
}

impl Display for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sourcemap_borrowed = self.scopes.first().unwrap().sourcemap.borrow();
        write!(
            f,
            "{}",
            self.scopes
                .iter()
                .map(|id| sourcemap_borrowed.get_segment(id.pos))
                .collect::<Vec<_>>()
                .join("::")
        )
    }
}

impl Namespace {
    pub fn from_name_id(value: NameID) -> Rc<Namespace> {
        Rc::new(Namespace {
            pos: value.pos,
            scopes: vec![value],
        })
    }

    pub fn as_vec_nameid(&mut self) -> &mut Vec<NameID> {
        &mut self.scopes
    }

    pub fn prepend_namespace(&mut self, other: &mut Vec<NameID>) -> Namespace {
        std::mem::swap(&mut self.scopes, other); // Put into other
        self.scopes.append(other); // Append self.scopes
        self.clone()
    }

    pub fn prepend_namespace_rc(&self, other: &[NameID]) -> Namespace {
        Namespace {
            scopes: other
                .iter()
                .chain(&self.scopes)
                .map(|x| x.clone())
                .collect::<Vec<_>>(),
            pos: self.pos,
        }
    }

    pub fn starts_with(&self, other: Rc<Namespace>) -> bool {
        if &self.scopes[0..other.scopes.len() - 1] == &other.scopes[..] {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Nodes {
    pub nodes: Vec<Node>,
    pub pos: helpers::Pos,
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
pub enum Node {
    RefID(RefID),
    Reference(Reference),
    NameID(NameID),

    VariableAssign(VariableAssign),
    VariableAssignDeclaration(VariableAssignDeclaration),
    VariableDeclaration(VariableDeclaration),

    TypeAssign(TypeAssign),

    Type(Type),
    Arguments(Arguments),
    Block(Block),

    Infix(Infix),
    Prefix(Prefix),

    Tuple(Tuple),

    Tag(Tag),

    ExpressionStatement(ExpressionStatement),

    FunctionDefine(FunctionDefine),
    FunctionCall(FunctionCall),

    Conditional(Conditional),

    Unit(Unit),

    Import(Import),

    Literal(Literal),
    DollarID(DollarID),
    Nodes(Nodes),

    Return(Return),
    Yield(Yield),

    Empty(Empty),
    Units(Units),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),

    FunctionDefine(FunctionDefine),

    Return(Return),
    Yield(Yield),
    Unit(Unit),
    TypeAssign(TypeAssign),
    Import(Import),

    Empty(Empty),
    Tag(Tag),
}

impl Statement {
    pub fn pos(&self) -> helpers::Pos {
        match &self {
            Statement::ExpressionStatement(val) => val.pos,
            Statement::VariableDeclaration(val) => val.pos,

            Statement::FunctionDefine(val) => val.pos,

            Statement::Return(val) => val.pos,
            Statement::Yield(val) => val.pos,

            Statement::Unit(val) => val.pos,
            Statement::TypeAssign(val) => val.pos,
            Statement::Import(val) => val.pos,
            Statement::Tag(val) => val.pos,

            Statement::Empty(val) => val.pos,
        }
    }

    pub fn to_string(&self) -> &str {
        match &self {
            Statement::ExpressionStatement(val) => val.expression.to_str(),
            Statement::VariableDeclaration(_) => "variable declaration",

            Statement::FunctionDefine(_) => "function define",

            Statement::Return(_) => "return statement",
            Statement::Yield(_) => "yield statement",

            Statement::Unit(_) => "unit",
            Statement::Import(_) => "import",
            Statement::TypeAssign(_) => "type assignment",
            Statement::Tag(_) => "compiler tag",

            Statement::Empty(_) => "empty statement",
        }
    }

    pub fn in_scope(&self, check_scope: &Scope) -> bool {
        Statement::get_scope(self) == check_scope
    }

    pub fn get_scope(statement: &Statement) -> &Scope {
        match statement {
            Statement::ExpressionStatement(_) => &Scope::Block,
            Statement::VariableDeclaration(_) => &Scope::Block,

            Statement::FunctionDefine(_) => &Scope::All,

            Statement::Return(_) => &Scope::Block,
            Statement::Yield(_) => &Scope::Block,

            Statement::TypeAssign(_) => &Scope::All,
            Statement::Unit(_) => &Scope::Outer,
            Statement::Import(_) => &Scope::Outer,
            Statement::Tag(_) => &Scope::All,

            Statement::Empty(_) => &Scope::All,
        }
    }

    pub fn into_node(self) -> Node {
        match self {
            Statement::ExpressionStatement(val) => Node::ExpressionStatement(val),
            Statement::VariableDeclaration(val) => Node::VariableDeclaration(val),

            Statement::FunctionDefine(val) => Node::FunctionDefine(val),

            Statement::Return(val) => Node::Return(val),
            Statement::Yield(val) => Node::Yield(val),

            Statement::TypeAssign(val) => Node::TypeAssign(val),
            Statement::Import(val) => Node::Import(val),
            Statement::Unit(val) => Node::Unit(val),
            Statement::Tag(val) => Node::Tag(val),

            Statement::Empty(val) => Node::Empty(val),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),

    RefID(RefID),
    Reference(Reference),
    VariableAssign(VariableAssign),
    VariableAssignDeclaration(VariableAssignDeclaration),
    FunctionCall(FunctionCall),

    Infix(Infix),
    Prefix(Prefix),
    Tuple(Tuple),

    DollarID(DollarID),

    Empty(Empty),

    FunctionDefine(FunctionDefine),
    Conditional(Conditional),
}

impl Expr {
    pub fn pos(&self) -> helpers::Pos {
        match &self {
            Expr::Literal(val) => val.pos,
            Expr::RefID(val) => val.pos,
            Expr::DollarID(val) => val.pos,
            Expr::Reference(val) => val.pos,
            Expr::VariableAssign(val) => val.pos,
            Expr::VariableAssignDeclaration(val) => val.pos,
            Expr::FunctionCall(val) => val.pos,
            Expr::Tuple(val) => val.pos,

            Expr::Conditional(val) => val.pos,

            Expr::Infix(val) => val.pos,
            Expr::Prefix(val) => val.pos,

            Expr::Empty(val) => val.pos,
            Expr::FunctionDefine(val) => val.pos,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            Expr::Literal(_) => "literal",
            Expr::RefID(_) => "ID",
            Expr::DollarID(_) => "dollar sign ID",
            Expr::Reference(_) => "refrence",
            Expr::VariableAssign(_) => "variable assign",
            Expr::VariableAssignDeclaration(_) => "variable assignment declaration",
            Expr::FunctionCall(_) => "function call",
            Expr::Tuple(_) => "tuple",

            Expr::Conditional(_) => "conditional",

            Expr::Infix(_) => "infix",
            Expr::Prefix(_) => "prefix",

            Expr::Empty(_) => "empty",
            Expr::FunctionDefine(_) => "function define",
        }
    }
}
