//! Middle intermediate representation.
//!
//! This is a lower lever representation of code.

use crate::helpers;
use crate::parser::ast;

#[derive(Debug, Clone)]
pub struct Tuple {
    pub types: Vec<MirType>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: ast::Namespace,
    pub ty: MirType,
}

#[derive(Debug, Clone)]
/// (i32, i32, ()) -> ()
pub struct FunctionSig {
    pub pos_args: Vec<Binding>,
    pub return_type: Box<MirType>,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub enum MirType {
    /// Tuple types, E.g., (int, int), (str, my::type)
    Tuple(Tuple),

    /// Primitives
    Primitive(ast::Namespace),

    /// Function Signatures
    FunctionSig(FunctionSig),
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
    pub block: Block,
    pub mangled_name: String,
}

#[derive(Debug, Clone)]
pub struct VariableAssign {
    pub var_name: ast::Namespace,
    pub ty: MirExpr,
    pub pos: helpers::Pos,
}
#[derive(Debug, Clone)]
pub struct VariableAssignDeclaration {
    pub var_name: ast::Namespace,
    pub ty: MirExpr,
    pub pos: helpers::Pos,
}
#[derive(Debug, Clone)]
pub struct VariableDeclaration {}

#[derive(Debug, Clone)]
pub struct Literal {
    pub ty: MirType,
    pub pos: helpers::Pos,
}

#[derive(Debug, Clone)]
pub struct MirTag {}

#[derive(Debug, Clone)]
enum MirExprValue {
    Variable(ast::Namespace),
    Literal(Literal),
    Function(Box<FunctionExpr>),
    VariableAssign(Box<VariableAssign>),
    VariableAssignDeclaration(Box<VariableAssignDeclaration>),
}

#[derive(Debug, Clone)]
pub struct MirExpr {
    value: MirExprValue,
    pos: helpers::Pos,
    ty: MirType,
}

#[derive(Debug, Clone)]
pub enum MirStmt {
    VariableDeclaration(VariableDeclaration),
    Conditional(Conditional),
    Tag(MirTag),
    Expression(MirExpr),
}
