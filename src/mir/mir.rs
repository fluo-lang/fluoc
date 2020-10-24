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
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub enum MirType {
    /// Tuple types, E.g., (int, int), (str, my::type)
    Tuple(Vec<MirType>, helpers::Pos),

    /// Primitives
    Primitive(Prim, helpers::Pos),

    /// Function Type
    FunctionType(Vec<MirType>, Box<MirType>, helpers::Pos),

    /// Null type
    Never,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct IfBranch {
    pub cond: MirExpr,
    pub block: MirBlock,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Pos,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct ElseBranch {
    pub block: MirBlock,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Pos,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct Conditional {
    // If Branch
    pub if_b: IfBranch,
    // Else Branch
    pub else_b: ElseBranch,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct BlockMetadata {
    pub returns: bool,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirBlock {
    pub stmts: Vec<MirStmt>,
    pub metadata: BlockMetadata,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Pos,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirVariableAssign {
    pub var_name: Rc<ast::Namespace>,
    pub value: Either<MirExpr, MirType>,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Pos,
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
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub enum MirExprEnum {
    Variable(ast::Namespace),
    Literal,
    Block(MirBlock),
    Conditional(Box<Conditional>),
    RefID(Rc<ast::Namespace>),
    FunctionCall(MirFunctionCall),
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct MirExpr {
    pub value: MirExprEnum,
    #[derivative(Debug = "ignore")]
    pub pos: helpers::Pos,
    pub ty: MirType,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub enum MirStmt {
    VariableAssign(Box<MirVariableAssign>),
    Return {
        value: MirExpr,
        #[derivative(Debug = "ignore")]
        pos: helpers::Pos,
    },
    Yield {
        value: MirExpr,
        #[derivative(Debug = "ignore")]
        pos: helpers::Pos,
    },
    Tag(MirTag),
    Expression(MirExpr),
    FunctionDef {
        signature: MirFunctionSig,
        arg_names: Vec<Rc<ast::Namespace>>,
        block: Option<MirBlock>,
        visibility: ast::Visibility,
        mangled_name: String,
    },
}
