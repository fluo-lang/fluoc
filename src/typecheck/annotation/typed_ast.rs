//! Representation of typed ast nodes.
use crate::helpers;
use super::annotation_type::AnnotationType;


pub struct TypedExpr {
    ty: AnnotationType,
    pos: helpers::Pos
}

pub struct TypedBinder {}

pub struct TypedFunction {

}

enum TypedStmtEnum {
    Function(TypedFunction)
}

pub struct TypedStmt {
    pos: helpers::Pos
}

pub struct TypedBlock {
    ty: AnnotationType,
    pos: helpers::Pos
}


