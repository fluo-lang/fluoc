use crate::typecheck::annotation::AnnotationType;

use crate::helpers;
use crate::lexer::Token;
use crate::sourcemap::SourceMap;
use crate::tags::UnitTags;

use inkwell::module::Linkage;

use std::fmt;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::collections::HashMap;

use smallvec::{smallvec, SmallVec};

// EXPRESSIONS ---------------------------------------

#[derive(Debug, Clone, PartialEq)]
/// Empty Placeholder value
pub struct Empty {
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// Special Compiler Tags
pub struct Tag {
    pub content: NameID,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralType {
    Number,
    String,
    Bool,
}

impl fmt::Display for LiteralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Number => "<number>",
                Self::String => "<string>",
                Self::Bool => "<bool>",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub literal_type: LiteralType,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub namespace: Namespace,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// Tuple node
pub struct Tuple {
    pub values: Vec<Expr>,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// Dollar sign id (i.e. `$myvar`) node
pub struct DollarID {
    pub value: Rc<Namespace>,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// Reference ID (i.e. pass by value) node
pub struct RefID {
    pub value: Rc<Namespace>,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// Reference (i.e. pass by reference) node
pub struct Reference {
    pub value: RefID,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Infix {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Token,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    pub val: Box<Expr>,
    pub operator: Token,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsExpr {
    pub expr: Box<Expr>,
    pub ty: Type,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IsExpr {
    pub expr: Box<Expr>,
    pub ty: Type,
    pub pos: helpers::Span,
}

// NODES ---------------------------------------

/// Name ID node
pub struct NameID {
    pub sourcemap: SourceMap,
    pub pos: helpers::Span,
}

impl fmt::Debug for NameID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("")
            .field(&get_segment!(self.sourcemap, self.pos))
            .finish()
    }
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
            pos: helpers::Span::new(0, 5, 0),
        };
        let second = NameID {
            sourcemap,
            pos: helpers::Span::new(6, 11, 0),
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
            scopes: smallvec![self],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Type Assign i.e.:
///
/// type km = int;
pub struct TypeAssign {
    pub name: Rc<Namespace>,
    pub value: Type,
    pub visibility: Visibility,
    pub pos: helpers::Span,
}


#[derive(Debug, Clone, PartialEq)]
/// Generic parameters
pub struct Generics {
    pub ty_map: HashMap<Namespace, Vec<Namespace>>
}

#[derive(Debug, Clone, PartialEq)]
/// Variable Assign + Declaration i.e.:
///
/// let x: int = 10;
pub struct VariableAssignDeclaration {
    pub ty: Type,
    pub typecheck_type: Option<AnnotationType>,
    pub name: Rc<Namespace>,
    pub expr: Option<Box<Expr>>,
    pub generics: Option<Generics>,
    pub visibility: Visibility,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unit {
    pub name: Namespace,
    pub pos: helpers::Span,
    pub block: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
/// Arguments for function
pub struct Arguments {
    pub positional: Vec<(Rc<Namespace>, Type)>,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// If/else if conditional branch
pub struct IfBranch {
    pub cond: Expr,
    pub block: Block,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// Else conditional branch
pub struct ElseBranch {
    pub block: Block,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub if_branches: Vec<IfBranch>,
    pub else_branch: Option<ElseBranch>,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// Block of code
pub struct Block {
    pub nodes: Vec<Statement>,
    pub tags: UnitTags,
    pub ty: Option<AnnotationType>,
    pub pos: helpers::Span,
}

impl Block {
    pub fn to_string(&self) -> String {
        let mut val = String::new();
        for node in &self.nodes {
            val += &node.as_str();
            val += "\n";
        }
        val
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Units {
    pub units: Vec<Unit>,
    pub pos: helpers::Span,
}

impl Units {
    pub fn into_block(self) -> Block {
        Block {
            nodes: self.units.into_iter().map(|x| Statement::Unit(x)).collect(),
            ty: None,
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
/// Function expression
pub struct Function {
    pub return_type: Type,
    pub arguments: Arguments,
    pub ty: Option<AnnotationType>,
    pub block: Box<Expr>,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArgumentsRun {
    pub positional: Vec<Expr>,
    pub pos: helpers::Span, // TODO: Add more types of arguments
}

#[derive(Debug, Clone, PartialEq)]
pub struct Yield {
    pub expression: Box<Expr>,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub expression: Box<Expr>,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
/// Function call
pub struct FunctionCall {
    pub arguments: ArgumentsRun,
    pub name: Rc<Namespace>,
    pub mangled_name: Option<String>,
    pub mangle: bool,
    pub pos: helpers::Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Box<Expr>,
    pub pos: helpers::Span,
}

#[derive(Debug, PartialEq, Clone)]
/// Type Types
pub enum TypeType {
    Type(Rc<Namespace>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Unknown,
}

impl TypeType {
    pub fn f(&self, sourcemap: SourceMap) -> String {
        match &self {
            TypeType::Type(namespace) => namespace.to_string(),
            TypeType::Tuple(types) => types
                .iter()
                .map(|type_val| type_val.f(Rc::clone(&sourcemap)))
                .collect::<Vec<_>>()
                .join(", "),
            TypeType::Unknown => "<unknown>".to_string(),
            TypeType::Function(args, ret) => format!(
                "({}) -> {}",
                args.iter()
                    .map(|type_val| type_val.f(Rc::clone(&sourcemap)))
                    .collect::<Vec<_>>()
                    .join(", "),
                ret.f(Rc::clone(&sourcemap))
            ),
        }
    }
}

#[derive(Debug, Clone)]
/// Type Node
pub struct Type {
    pub value: TypeType,
    pub pos: helpers::Span,
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

pub type NamespaceInner = SmallVec<[NameID; 5]>;

#[derive(Eq, Clone, Default)]
/// This::is::a::namespace!
pub struct Namespace {
    pub scopes: NamespaceInner,
    pub pos: helpers::Span,
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.to_string(), f)
    }
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
        if let Some(scope) = self.scopes.first() {
            let sourcemap_borrowed = scope.sourcemap.borrow();
            write!(f, "{}", sourcemap_borrowed.get_segment(self.pos))?;
        }
        Ok(())
    }
}

impl Namespace {
    pub fn from_name_id(value: NameID) -> Rc<Namespace> {
        Rc::new(Namespace {
            pos: value.pos,
            scopes: smallvec![value],
        })
    }

    pub fn as_vec_nameid(&mut self) -> &mut NamespaceInner {
        &mut self.scopes
    }

    pub fn prepend_namespace(&mut self, other: &mut NamespaceInner) -> Namespace {
        std::mem::swap(&mut self.scopes, other); // Put into other
        self.scopes.extend(other.drain(..));
        std::mem::replace(self, Namespace::default())
    }

    pub fn prepend_namespace_rc(&self, other: &[NameID]) -> Namespace {
        Namespace {
            scopes: other
                .iter()
                .chain(&self.scopes)
                .map(|x| x.clone())
                .collect::<NamespaceInner>(),
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

    pub fn mangle(&self) -> String {
        let mut mangled =
            String::with_capacity(self.scopes.iter().map(|x| x.pos.len() + 1).count());
        for sign in &self.scopes {
            mangled.push_str(&sign.pos.len().to_string());
            mangled.push_str(sign.sourcemap.borrow().get_segment(sign.pos));
            mangled.push('_')
        }
        mangled
    }
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
pub enum Statement {
    ExpressionStatement(ExpressionStatement),

    Unit(Unit),
    TypeAssign(TypeAssign),
    Import(Import),

    Empty(Empty),
    Tag(Tag),
}

impl Statement {
    pub fn pos(&self) -> helpers::Span {
        match &self {
            Statement::ExpressionStatement(val) => val.pos,

            Statement::Unit(val) => val.pos,
            Statement::TypeAssign(val) => val.pos,
            Statement::Import(val) => val.pos,
            Statement::Tag(val) => val.pos,

            Statement::Empty(val) => val.pos,
        }
    }

    pub fn as_str(&self) -> &str {
        match &self {
            Statement::ExpressionStatement(val) => val.expression.as_str(),

            Statement::Unit(_) => "unit",
            Statement::Import(_) => "import",
            Statement::TypeAssign(_) => "type assignment",
            Statement::Tag(_) => "compiler tag",

            Statement::Empty(_) => "empty statement",
        }
    }

    pub fn in_scope(&self, check_scope: &Scope) -> bool {
        self.get_scope() == check_scope
    }

    pub fn get_scope(&self) -> &Scope {
        match &self {
            Statement::ExpressionStatement(expr) => expr.expression.get_scope(),

            Statement::TypeAssign(_) => &Scope::All,
            Statement::Unit(_) => &Scope::Outer,
            Statement::Import(_) => &Scope::Outer,
            Statement::Tag(_) => &Scope::All,

            Statement::Empty(_) => &Scope::All,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),

    RefID(RefID),
    Reference(Reference),
    VariableAssignDeclaration(VariableAssignDeclaration),
    FunctionCall(FunctionCall),

    Function(Function),

    Infix(Infix),
    Prefix(Prefix),

    Return(Return),
    Yield(Yield),

    As(AsExpr),
    Is(IsExpr),

    Tuple(Tuple),

    DollarID(DollarID),

    Empty(Empty),

    Conditional(Conditional),
    Block(Block),
}

impl Expr {
    pub fn pos(&self) -> helpers::Span {
        match &self {
            Expr::Literal(val) => val.pos,
            Expr::RefID(val) => val.pos,
            Expr::DollarID(val) => val.pos,
            Expr::Reference(val) => val.pos,
            Expr::VariableAssignDeclaration(val) => val.pos,
            Expr::FunctionCall(val) => val.pos,
            Expr::Tuple(val) => val.pos,

            Expr::Conditional(val) => val.pos,

            Expr::Infix(val) => val.pos,
            Expr::Prefix(val) => val.pos,

            Expr::Return(val) => val.pos,
            Expr::Yield(val) => val.pos,

            Expr::Function(val) => val.pos,

            Expr::As(val) => val.pos,
            Expr::Is(val) => val.pos,

            Expr::Empty(val) => val.pos,

            Expr::Block(val) => val.pos,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Expr::Literal(_) => "literal",
            Expr::RefID(_) => "ID",
            Expr::DollarID(_) => "dollar sign ID",
            Expr::Reference(_) => "refrence",
            Expr::VariableAssignDeclaration(_) => "variable assignment declaration",
            Expr::FunctionCall(_) => "function call",
            Expr::Tuple(_) => "tuple",

            Expr::Conditional(_) => "conditional",

            Expr::Function(_) => "function",

            Expr::Infix(_) => "infix",
            Expr::Prefix(_) => "prefix",

            Expr::Return(_) => "return",
            Expr::Yield(_) => "yield",

            Expr::As(_) => "as cast",
            Expr::Is(_) => "is cast",

            Expr::Empty(_) => "empty",

            Expr::Block(_) => "block",
        }
    }

    pub fn get_scope(&self) -> &Scope {
        match &self {
            Expr::VariableAssignDeclaration(_) => &Scope::All,
            Expr::Function(_) => &Scope::All,
            Expr::Block(_) => &Scope::All,

            _ => &Scope::Block,
        }
    }
}
