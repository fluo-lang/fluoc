use super::typed_ast;
use crate::parser::ast;

use std::rc::Rc;

#[derive(Clone, Debug)]
/// An annotation type
///
/// The `Infer` variant is assigned when the type is to be inferred.
/// At the end of the typechecking process, all the types should be `Concrete`.
/// We will then be able to convert it back into a pure `types::MirType` for codegen.
pub enum AnnotationType {
    Type(Rc<ast::Namespace>),
    Tuple(Vec<AnnotationType>),
    Function(Rc<Vec<typed_ast::TypedBinder>>, Box<AnnotationType>),
    Infer(usize),
}

