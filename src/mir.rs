//! Middle intermediate representation.
//!
//! This is a lower lever representation of code.

use crate::typecheck::context::Context;
use crate::typecheck::types;

use crate::helpers;
use crate::logger::ErrorValue;
use crate::parser::ast;

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
pub struct FunctionDefine {
    pub signature: types::FunctionSig,
    pub block: Block,
    pub mangled_name: String,
}

#[derive(Debug, Clone)]
pub struct VariableAssign {}
#[derive(Debug, Clone)]
pub struct VariableAssignDeclaration {}
#[derive(Debug, Clone)]
pub struct VariableDeclaration {}

#[derive(Debug, Clone)]
pub struct Literal {
    pub ty: types::MirType,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
enum MirExprValue {
    Variable(ast::Namespace),
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct MirExpr {
    value: MirExprValue,
    pos: helpers::Pos,
    ty: types::MirType,
}

#[derive(Debug, Clone)]
pub enum MirStmt {
    FunctionDefine(FunctionDefine),
    VariableAssign(VariableAssign),
    VariableDeclaration(VariableDeclaration),
    VariableAssignDeclaration(VariableAssignDeclaration),
    Conditional(Conditional),
}
