use super::AnnotationType;
use crate::helpers;
use crate::parser::ast;

use std::fmt;
use std::hash::{Hash, Hasher};
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
    pub func_ty: AnnotationType,
    pub ty: AnnotationType,
    pub name: Rc<ast::Namespace>,
    pub arguments: Vec<TypedExpr>,
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct TypedTuple {
    pub ty: AnnotationType,
    pub exprs: Vec<TypedExpr>,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Clone, Debug)]
pub enum TypedExprEnum {
    Tuple(TypedTuple),
    Block(TypedBlock),
    VariableAssignDeclaration(TypedAssign),
    Literal(TypedLiteral),
    RefID(TypedRefID),
    Is(TypedIs),
    FunctionCall(TypedFunctionCall),
    Yield(TypedYield),
    Return(TypedReturn),
    Function(TypedFunction),
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct TypedExpr {
    pub expr: TypedExprEnum,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

impl TypedExpr {
    pub fn ty(&self) -> &AnnotationType {
        match &self.expr {
            TypedExprEnum::Block(val) => &val.ty,

            TypedExprEnum::VariableAssignDeclaration(val) => &val.binder.ty,

            TypedExprEnum::Literal(val) => &val.ty,
            TypedExprEnum::RefID(val) => &val.ty,

            TypedExprEnum::Is(val) => &val.ty,

            TypedExprEnum::FunctionCall(val) => &val.ty,
            TypedExprEnum::Function(val) => &val.ty,

            TypedExprEnum::Yield(val) => &val.ty,
            TypedExprEnum::Return(val) => &val.ty,

            TypedExprEnum::Tuple(val) => &val.ty,
        }
    }

    pub fn returns(&self) -> bool {
        match &self.expr {
            TypedExprEnum::Return(_) => true,
            TypedExprEnum::Yield(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Derivative, Eq)]
#[derivative(Debug)]
pub struct TypedBinder {
    pub name: Option<Rc<ast::Namespace>>,
    pub ty: AnnotationType,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
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
        if let Some(ref name) = self.name {
            write!(f, "{}: {}", name, self.ty)
        } else {
            write!(f, "{}", self.ty)
        }
    }
}

impl TypedBinder {
    pub fn new(name: Option<Rc<ast::Namespace>>, ty: AnnotationType, pos: helpers::Span) -> Self {
        Self { name, ty, pos }
    }
}

#[derive(Clone, Debug)]
pub struct TypedAssign {
    pub binder: TypedBinder,
    pub expr: either::Either<Box<TypedExpr>, AnnotationType>,
    pub visibility: ast::Visibility,
}

#[derive(Clone, Debug)]
pub struct TypedFunction {
    pub ty: AnnotationType,
    pub arg_names: Vec<Rc<ast::Namespace>>,
    pub block: Box<TypedExpr>,
}

#[derive(Clone, Debug)]
pub struct TypedYield {
    pub expr: Box<TypedExpr>,
    pub ty: AnnotationType,
}

#[derive(Clone, Debug)]
pub struct TypedReturn {
    pub expr: Box<TypedExpr>,
    pub ty: AnnotationType,
}

#[derive(Clone, Debug)]
pub enum TypedStmtEnum {
    Expression(TypedExpr),
    VariableDeclaration(TypedBinder),
    Tag(ast::Tag),
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct TypedStmt {
    pub stmt: TypedStmtEnum,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Clone, Debug)]
pub struct TypedBlock {
    pub stmts: Vec<TypedStmt>,
    pub ty: AnnotationType,
}
