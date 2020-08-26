use super::context::Context;
use super::types;

use crate::helpers;
use crate::logger::ErrorValue;
use crate::parser::ast::LiteralType;

pub struct Namespace {}

pub struct FunctionDefine {}

pub struct FunctionSig {
    pos_args: Vec<types::MirType>,
    return_type: types::MirType,
    pos: helpers::Pos,
}

pub struct VariableAssign {}

pub struct Literal {
    literal_type: LiteralType,
    pos: helpers::Pos,
}

pub enum MirExpr {
    Variable(Namespace),
    Literal(Literal),
}

impl MirExpr {
    /// Get the type of an expression
    fn get_type(
        &self,
        context: &mut Context,
        expected_value: Option<types::MirType>,
    ) -> Result<types::MirType, ErrorValue> {
        panic!();
    }
}

pub enum MirStmt {
    FunctionDefine(FunctionDefine),
}

impl MirStmt {
    fn typecheck(&self, context: &mut Context) -> Result<(), ErrorValue> {
        panic!();
    }
}
