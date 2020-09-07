use crate::helpers;
use crate::parser::ast;

use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash)]
/// An annotation type
///
/// The `Infer` variant is assigned when the type is to be inferred.
/// At the end of the typechecking process, all the types should be `Concrete`.
/// We will then be able to convert it back into a pure `types::MirType` for codegen.
///
/// NOTE: This type can be cloned relatively cheaply. (Because of Rc's)
pub enum AnnotationType {
    Type(Rc<ast::Namespace>, helpers::Pos),
    Tuple(Rc<Vec<AnnotationType>>, helpers::Pos),
    Function(Rc<Vec<AnnotationType>>, Box<AnnotationType>, helpers::Pos),
    Never(helpers::Pos),
    Infer(usize, helpers::Pos),
}

impl AnnotationType {
    pub fn pos(&self) -> helpers::Pos {
        match self {
            AnnotationType::Type(_, pos) => *pos,
            AnnotationType::Tuple(_, pos) => *pos,
            AnnotationType::Function(_, _, pos) => *pos,
            AnnotationType::Never(pos) => *pos,
            AnnotationType::Infer(_, pos) => *pos,
        }
    }
}

impl Clone for AnnotationType {
    fn clone(&self) -> Self {
        match self {
            AnnotationType::Type(name, pos) => AnnotationType::Type(Rc::clone(name), *pos),
            AnnotationType::Tuple(types, pos) => AnnotationType::Tuple(Rc::clone(types), *pos),
            AnnotationType::Function(arg_ty, ret_ty, pos) => {
                AnnotationType::Function(Rc::clone(arg_ty), (*ret_ty).clone(), *pos)
            }
            AnnotationType::Never(pos) => AnnotationType::Never(*pos),
            AnnotationType::Infer(ty_var, pos) => AnnotationType::Infer(*ty_var, *pos),
        }
    }
}

impl fmt::Display for AnnotationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnnotationType::Infer(val, _) => write!(f, "T{}", val),
            AnnotationType::Type(ty, _) => write!(f, "{}", ty),
            AnnotationType::Tuple(tup, _) => write!(
                f,
                "({})",
                tup.iter()
                    .map(|val| val.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            AnnotationType::Never(_) => write!(f, "<never>"),
            AnnotationType::Function(args, ret, _) => write!(
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
