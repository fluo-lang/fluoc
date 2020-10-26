use crate::helpers;
use crate::parser::ast;
use smallvec::{smallvec, SmallVec};

use std::fmt;
use std::rc::Rc;

#[derive(Derivative, Eq, Hash)]
#[derivative(Debug)]
/// An annotation type
///
/// The `Infer` variant is assigned when the type is to be inferred.
/// At the end of the typechecking process, all the types should be `Concrete`.
/// We will then be able to convert it back into a pure `types::MirType` for codegen.
///
/// NOTE: This type can be cloned relatively cheaply. (Because of Rc's)
pub enum AnnotationType {
    Type(
        Rc<ast::Namespace>,
        #[derivative(Debug = "ignore")] helpers::Span,
    ),
    Tuple(
        Rc<Vec<AnnotationType>>,
        #[derivative(Debug = "ignore")] helpers::Span,
    ),
    Function(
        Rc<Vec<AnnotationType>>,
        Rc<AnnotationType>,
        #[derivative(Debug = "ignore")] helpers::Span,
    ),
    Never(#[derivative(Debug = "ignore")] helpers::Span),
    Infer(
        usize,
        Option<AdditionalContraints>,
        #[derivative(Debug = "ignore")] helpers::Span,
    ),
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
            (
                AnnotationType::Infer(infer_num1, con1, _),
                AnnotationType::Infer(infer_num2, con2, _),
            ) => infer_num1 == infer_num2 && con1 == con2,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct AdditionalContraints(SmallVec<[AdditionalContraint; 2]>);

impl AdditionalContraints {
    /// Returns `None` if fits, otherwise returns `Some(val)` with offending constraint
    pub fn fits(&self, ty: &AnnotationType) -> Option<AdditionalContraint> {
        self.0.iter().filter(|c| !c.fits(ty)).next().cloned()
    }

    pub fn is_literal(&self) -> bool {
        self.0.iter().any(|c| c.is_literal())
    }

    pub fn get_literal(&self) -> Option<ast::LiteralType> {
        for val in &self.0 {
            if let AdditionalContraint::Literal(lit) = val {
                return Some(*lit);
            }
        }
        None
    }
}

impl fmt::Display for AdditionalContraints {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl From<AdditionalContraint> for AdditionalContraints {
    fn from(val: AdditionalContraint) -> Self {
        Self(smallvec![val])
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum AdditionalContraint {
    Literal(ast::LiteralType),
}

impl AdditionalContraint {
    pub fn fits(&self, ty: &AnnotationType) -> bool {
        match self {
            AdditionalContraint::Literal(lit) => match ty.is_primitive() {
                Some(prim) => match prim.as_lit_type() {
                    Some(prim_lit) => lit == &prim_lit,
                    None => true,
                },
                None => true,
            },
        }
    }

    fn is_literal(&self) -> bool {
        match self {
            AdditionalContraint::Literal(_) => true,
        }
    }
}

impl fmt::Display for AdditionalContraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AdditionalContraint::Literal(lit) => write!(f, "{}", lit),
        }
    }
}

impl AnnotationType {
    pub fn is_primitive(&self) -> Option<Prim> {
        match self {
            AnnotationType::Infer(_, Some(con), _) if con.is_literal() => Some(Prim::Infer),
            AnnotationType::Type(ty, _) if ty.scopes.len() == 1 => {
                let first_name = ty.scopes[0].clone();
                let is_prim = match get_segment!(first_name.sourcemap, first_name.pos) {
                    "bool" => Prim::Bool,
                    "i64" => Prim::I64,
                    "i32" => Prim::I32,
                    "i16" => Prim::I16,
                    "i8" => Prim::I8,
                    "i128" => Prim::I128,

                    "u64" => Prim::U64,
                    "u32" => Prim::U32,
                    "u16" => Prim::U16,
                    "u8" => Prim::U8,
                    "u128" => Prim::U128,
                    _ => return None,
                };
                Some(is_prim)
            }
            _ => None,
        }
    }

    pub fn pos(&self) -> helpers::Span {
        match self {
            AnnotationType::Type(_, pos) => *pos,
            AnnotationType::Tuple(_, pos) => *pos,
            AnnotationType::Function(_, _, pos) => *pos,
            AnnotationType::Never(pos) => *pos,
            AnnotationType::Infer(_, _, pos) => *pos,
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
            AnnotationType::Infer(ty_var, val, pos) => {
                AnnotationType::Infer(*ty_var, val.clone(), *pos)
            }
        }
    }
}

impl fmt::Display for AnnotationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnnotationType::Infer(_, con, _) => match con {
                Some(c) => write!(f, "_: {}", c),
                None => write!(f, "_"),
            },
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

#[derive(Debug, Clone)]
pub enum Prim {
    Bool,

    I128,
    I64,
    I32,
    I16,
    I8,

    U128,
    U64,
    U32,
    U16,
    U8,

    Infer,
}

impl Prim {
    fn as_lit_type(&self) -> Option<ast::LiteralType> {
        match self {
            Prim::Bool => Some(ast::LiteralType::Bool),
            Prim::I8
            | Prim::I16
            | Prim::I32
            | Prim::I64
            | Prim::I128
            | Prim::U8
            | Prim::U16
            | Prim::U32
            | Prim::U64
            | Prim::U128 => Some(ast::LiteralType::Number),
            Prim::Infer => None,
        }
    }
}
