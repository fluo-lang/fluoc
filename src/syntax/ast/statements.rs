use super::{Block, Expr, Ident, Span, Type};

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Statement {
    FunctionDef(FunctionDef),
    VariableDef(VariableDef),
    Return(Return),
    Expr(Expr),
}

#[derive(Debug)]
#[cfg_attr(test, derive(Derivative))]
#[cfg_attr(test, derivative(PartialEq))]
pub struct FunctionDef {
    pub name: Ident,
    pub arguments: FunctionArguments,
    pub return_ty: Type,
    pub block: Block,
    #[cfg_attr(test, derivative(PartialEq = "ignore"))]
    pub span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionArguments {
    pub pos_args: Vec<PosArg>,
    pub kw_args: Vec<KeywordArg>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct PosArg {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct KeywordArg {
    pub name: Ident,
    pub ty: Type,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(Derivative))]
#[cfg_attr(test, derivative(PartialEq))]
pub struct VariableDef {
    #[cfg_attr(test, derivative(PartialEq = "ignore"))]
    pub span: Span,
    pub binding: Binding,
}

#[derive(Debug)]
#[cfg_attr(test, derive(Derivative))]
#[cfg_attr(test, derivative(PartialEq))]
pub struct Binding {
    pub name: Ident,
    pub expr: Expr,
    pub ty: Type,
}

#[derive(Debug)]
#[cfg_attr(test, derive(Derivative))]
#[cfg_attr(test, derivative(PartialEq))]
pub struct Return {
    pub expr: Expr,
    pub local: bool,
    #[cfg_attr(test, derivative(PartialEq = "ignore"))]
    pub span: Span,
}
