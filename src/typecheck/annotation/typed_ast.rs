//! Representation of typed ast nodes.
use super::annotation_type::AnnotationType;
use crate::helpers;
use crate::parser::ast;

use std::rc::Rc;
use std::hash::{Hasher, Hash};
use std::fmt;

#[derive(Clone, Debug)]
pub struct TypedLiteral {
    pub value: ast::Literal,
    pub ty: AnnotationType,
}

#[derive(Clone, Debug)]
pub struct TypedRefID {
    pub name: Rc<ast::Namespace>,
    pub ty: AnnotationType,
}

#[derive(Clone, Debug)]
pub struct TypedIs {
    pub ty: AnnotationType,
    pub expr: Box<TypedExpr>,
}

#[derive(Clone, Debug)]
pub struct TypedFunctionCall {
    pub ty: AnnotationType,
    pub name: Rc<ast::Namespace>,
    pub arguments: Vec<TypedExpr>,
}

#[derive(Clone, Debug)]
pub enum TypedExprEnum {
    Block(TypedBlock),
    VariableAssign(TypedAssign),
    VariableAssignDeclaration(TypedAssign),
    Literal(TypedLiteral),
    RefID(TypedRefID),
    Is(TypedIs),
    FunctionCall(TypedFunctionCall),
    Yield(TypedYield),
    Return(TypedReturn),
    Function(TypedFunction)
}

#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub expr: TypedExprEnum,
    pub pos: helpers::Pos,
}

#[derive(Clone, Debug, Eq)]
pub struct TypedBinder {
    pub name: Rc<ast::Namespace>,
    pub ty: AnnotationType,
    pub pos: helpers::Pos,
}

impl PartialEq for TypedBinder {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Hash for TypedBinder {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.ty.hash(state);
    }
}

impl fmt::Display for TypedBinder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl TypedBinder {
    pub fn new(name: Rc<ast::Namespace>, ty: AnnotationType, pos: helpers::Pos) -> Self {
        Self { name, ty, pos }
    }
}

#[derive(Clone, Debug)]
pub struct TypedAssign {
    pub binder: TypedBinder,
    pub expr: Box<TypedExpr>,
}

#[derive(Clone, Debug)]
pub struct TypedFunction {
    pub ty: AnnotationType,
    pub block: TypedBlock,
}

#[derive(Clone, Debug)]
pub struct TypedYield {
    pub expr: Box<TypedExpr>,
}

#[derive(Clone, Debug)]
pub struct TypedReturn {
    pub expr: Box<TypedExpr>,
}

#[derive(Clone, Debug)]
pub enum TypedStmtEnum {
    Expression(TypedExpr),
    VariableDeclaration(TypedBinder),
    Tag(ast::Tag),
}

#[derive(Clone, Debug)]
pub struct TypedStmt {
    pub stmt: TypedStmtEnum,
    pub pos: helpers::Pos,
}

#[derive(Clone, Debug)]
pub struct TypedBlock {
    pub stmts: Vec<TypedStmt>,
    pub ty: AnnotationType,
    pub pos: helpers::Pos,
}
