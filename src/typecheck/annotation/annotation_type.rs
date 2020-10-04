use crate::helpers;
use crate::parser::ast;

use std::fmt;
use std::rc::Rc;

#[derive(Debug, Eq, Hash)]
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
    Function(Rc<Vec<AnnotationType>>, Rc<AnnotationType>, helpers::Pos),
    Never(helpers::Pos),
    Infer(usize, helpers::Pos),
}

impl PartialEq for AnnotationType {
    fn eq(&self, other: &Self) -> bool {
        match (&self, other) {
            (AnnotationType::Type(name1, _), AnnotationType::Type(name2, _)) => name1 == name2,
            (AnnotationType::Tuple(tys1, _), AnnotationType::Tuple(tys2, _)) => tys1 == tys2,
            (
                AnnotationType::Function(arg_tys1, ret_ty1, _),
                AnnotationType::Function(arg_tys2, ret_ty2, _),
            ) => arg_tys1 == arg_tys2 && ret_ty1 == ret_ty2,
            (AnnotationType::Never(_), AnnotationType::Never(_)) => true,
            (AnnotationType::Infer(infer_num1, _), AnnotationType::Infer(infer_num2, _)) => {
                infer_num1 == infer_num2
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Prim {
    Bool,
    I64,
    I32,
    I16,
    I8,
}

impl AnnotationType {
    pub fn is_primitive(&self) -> Option<Prim> {
        match self {
            AnnotationType::Type(ty, _) if ty.scopes.len() == 1 => {
                let first_name = ty.scopes[0].clone();
                let is_prim = match get_segment!(first_name.sourcemap, first_name.pos) {
                    "bool" => Prim::Bool,
                    "i64" => Prim::I64,
                    "i32" => Prim::I32,
                    "i16" => Prim::I16,
                    "i8" => Prim::I8,
                    _ => return None,
                };
                Some(is_prim)
            }
            _ => None,
        }
    }

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
                AnnotationType::Function(Rc::clone(arg_ty), Rc::clone(ret_ty), *pos)
            }
            AnnotationType::Never(pos) => AnnotationType::Never(*pos),
            AnnotationType::Infer(ty_var, pos) => AnnotationType::Infer(*ty_var, *pos),
        }
    }
}

impl fmt::Display for AnnotationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnnotationType::Infer(val, _) => write!(f, "_"),
            AnnotationType::Type(ty, _) => write!(f, "{}", ty),
            AnnotationType::Tuple(tup, _) => write!(
                f,
                "({}{})",
                tup.iter()
                    .map(|val| val.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                if tup.len() == 1 { "," } else { "" }
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
