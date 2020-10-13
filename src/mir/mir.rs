//! Middle intermediate representation.
//!
//! This is a lower lever representation of code.

use crate::helpers;
use crate::parser::ast;
use crate::typecheck::annotation::Prim;

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: ast::Namespace,
    pub ty: MirType,
}

#[derive(Debug, Clone)]
/// (i32, i32, ()) -> ()
pub struct FunctionSig {
    pub pos_args: Vec<MirType>,
    pub return_type: Box<MirType>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub enum MirType {
    /// Tuple types, E.g., (int, int), (str, my::type)
    Tuple(Vec<MirType>, helpers::Pos),

    /// Primitives
    Primitive(Prim, helpers::Pos),

    /// Function Signatures
    FunctionSig(FunctionSig, helpers::Pos),

    /// Null type
    Never,
}

#[derive(Debug, Clone)]
pub struct IfBranch {
    pub cond: MirExpr,
    pub block: Block,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct ElseBranch {
    pub block: Block,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    // If Branch
    pub if_b: IfBranch,
    // Else Branch
    pub else_b: ElseBranch,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct BlockMetadata {
    pub returns: bool,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub nodes: Vec<MirStmt>,
    pub metadata: BlockMetadata,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct FunctionExpr {
    pub signature: FunctionSig,
    pub arg_names: Vec<ast::Namespace>,
    pub block: Block,
    pub mangled_name: String,
}

#[derive(Debug, Clone)]
pub struct VariableAssignDeclaration {
    pub var_name: Rc<ast::Namespace>,
    pub value: MirExpr,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub ty: MirType,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub tag: ast::Tag,
}

#[derive(Debug, Clone)]
pub enum MirExprEnum {
    Variable(ast::Namespace),
    Literal(Literal),
    Function(Box<FunctionExpr>),
    Block(Block),
    Conditional(Box<Conditional>),
    RefID(Rc<ast::Namespace>),
    Never,
}

#[derive(Debug, Clone)]
pub struct MirExpr {
    pub value: MirExprEnum,
    pub pos: helpers::Pos,
    pub ty: MirType,
}

#[derive(Debug, Clone)]
pub enum MirStmt {
    VariableAssignDeclaration(Box<VariableAssignDeclaration>),
    Return { value: MirExpr, pos: helpers::Pos },
    Yield { value: MirExpr, pos: helpers::Pos },
    Tag(Tag),
    Expression(MirExpr),
}
