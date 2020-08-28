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
