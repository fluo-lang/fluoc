use crate::typecheck::types;

#[derive(Clone, Debug)]
/// An annotation type
///
/// The `Infer` variant is assigned when the type is to be inferred.
/// At the end of the typechecking process, all the types should be `Concrete`.
/// We will then be able to convert it back into a pure `types::MirType` for codegen.
pub enum AnnotationType {
    Concrete(types::MirType),
    Infer(usize),
}

