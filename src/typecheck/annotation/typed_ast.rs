//! Representation of typed ast nodes.
use super::annotation_type::AnnotationType;
use crate::helpers;
use crate::parser::ast;

use std::rc::Rc;

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
}

#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub expr: TypedExprEnum,
    pub pos: helpers::Pos,
}

#[derive(Clone, Debug)]
pub struct TypedBinder {
    pub name: Rc<ast::Namespace>,
    pub ty: AnnotationType,
    pub pos: helpers::Pos,
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
    pub arguments: Rc<Vec<TypedBinder>>,
    pub return_ty: AnnotationType,
    pub block: Option<TypedBlock>,
}

#[derive(Clone, Debug)]
pub struct TypedYield {
    pub expr: TypedExpr,
    pub pos: helpers::Pos,
}

#[derive(Clone, Debug)]
pub struct TypedReturn {
    pub expr: TypedExpr,
    pub pos: helpers::Pos,
}

#[derive(Clone, Debug)]
pub enum TypedStmtEnum {
    Function(TypedFunction),
    Expression(TypedExpr),
    VariableDeclaration(TypedBinder),
    Yield(TypedYield),
    Return(TypedReturn),
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
