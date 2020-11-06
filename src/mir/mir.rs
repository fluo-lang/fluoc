//! Middle intermediate representation.
//!
//! This is a lower lever representation of code.

use crate::helpers;
use crate::parser::ast;
use crate::typecheck::annotation::Prim;

use either::Either;
use std::rc::Rc;

#[derive(Derivative, Clone)]
#[derivative(Debug)]
/// (i32, i32, ()) -> ()
pub struct MirFunctionSig {
    pub pos_args: Vec<MirType>,
    pub return_type: Box<MirType>,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Debug, Clone)]
pub enum MirType {
    /// Tuple types, E.g., (int, int), (str, my::type)
    Tuple(Vec<MirType>, helpers::Span),

    /// Primitives
    Primitive(Prim, helpers::Span),

    /// Function Type
    FunctionType(Vec<MirType>, Box<MirType>, helpers::Span),

    /// Null type
    Never,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct IfBranch {
    pub cond: MirExpr,
    pub block: MirBlock,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct ElseBranch {
    pub block: MirBlock,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct Conditional {
    // If Branch
    pub if_b: IfBranch,
    // Else Branch
    pub else_b: ElseBranch,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Debug, Clone)]
pub struct BlockMetadata {
    pub diverges: bool,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirBlock {
    pub stmts: Vec<MirStmt>,
    pub metadata: BlockMetadata,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
    pub ty: MirType,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirVariableAssign {
    pub var_name: Rc<ast::Namespace>,
    pub value: Either<MirExpr, MirType>,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirTag {
    pub tag: ast::Tag,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirFunctionCall {
    pub arguments: Vec<MirExpr>,
    pub mangled_name: String,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Debug, Clone)]
pub enum MirExprEnum {
    Variable(Rc<ast::Namespace>),
    Literal,
    Block(MirBlock),
    Conditional(Box<Conditional>),
    FunctionCall(MirFunctionCall),
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirExpr {
    pub value: MirExprEnum,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
    pub ty: MirType,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirFunctionDef {
    pub signature: MirFunctionSig,
    pub arg_names: Vec<Rc<ast::Namespace>>,
    pub block: Option<MirBlock>,
    pub visibility: ast::Visibility,
    pub mangled_name: String,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirReturn {
    pub value: MirExpr,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirYield {
    pub value: MirExpr,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Span,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub enum MirStmt {
    VariableAssign(Box<MirVariableAssign>),
    Yield(MirYield),
    Return(MirReturn),
    Tag(MirTag),
    Expression(MirExpr),
    FunctionDef(MirFunctionDef),
}
