use super::{Expr, Name};

pub enum Type {
    Primitive(Prim),
    Ident(Name),
    Tuple(Vec<Expr>),
    Infer,
    Never,
}

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
