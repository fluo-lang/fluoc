use crate::parser::ast;

use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// An annotation type
///
/// The `Infer` variant is assigned when the type is to be inferred.
/// At the end of the typechecking process, all the types should be `Concrete`.
/// We will then be able to convert it back into a pure `types::MirType` for codegen.
pub enum AnnotationType {
    Type(Rc<ast::Namespace>),
    Tuple(Vec<AnnotationType>),
    Function(Rc<Vec<AnnotationType>>, Box<AnnotationType>),
    Never,
    Infer(usize),
}

impl fmt::Display for AnnotationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnnotationType::Infer(val) => write!(f, "T{}", val),
            AnnotationType::Type(ty) => write!(f, "{}", ty),
            AnnotationType::Tuple(tup) => write!(
                f,
                "({})",
                tup.iter()
                    .map(|val| val.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            AnnotationType::Never => write!(f, "<never>"),
            AnnotationType::Function(args, ret) => write!(
                f,
                "fn ({}) => {}",
                args.iter()
                    .map(|val| val.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret
            ),
        }
    }
}
