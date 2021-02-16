use super::{Block, Expr, Ident, Type};

pub enum Statement {
    FunctionDef(FunctionDef),
    VariableDef(VariableDef),
    Return(Return),
    Expr(Expr),
}

pub struct FunctionDef {
    name: Ident,
    arguments: FunctionArguments,
    block: Block,
}

pub struct FunctionArguments {}
pub struct VariableDef {}

pub struct Binding {
    name: Ident,
    expr: Expr,
    ty: Option<Type>,
}

pub struct Return {
    expr: Expr,
    local: bool,
}
