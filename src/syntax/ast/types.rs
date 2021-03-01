use super::{Expr, Name};

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Type {
    Name(Name),
    Tuple(Vec<Type>),
    Infer,
    Never,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Prim {
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
}
