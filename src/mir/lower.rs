use super::{MirExpr, MirExprEnum, MirStmt, MirType};

use crate::logger::ErrorValue;
use crate::typecheck::annotation::*;

pub fn lower_to_mir(typed_ast: Vec<TypedStmt>) -> Result<Vec<MirStmt>, Vec<ErrorValue>> {
    let mut mir = Vec::with_capacity(typed_ast.len());
    let mut errors = Vec::new();

    for typed_stmt in typed_ast.into_iter() {
        match typed_stmt.into_mir() {
            Ok(mir_stmt) => mir.push(mir_stmt),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(mir)
    } else {
        Err(errors)
    }
}

impl TypedStmt {
    fn into_mir(self) -> Result<MirStmt, ErrorValue> {
        match self.stmt {
            TypedStmtEnum::Expression(expr) => Ok(MirStmt::Expression(expr.into_mir()?)),
            _ => unimplemented!(),
        }
    }
}

impl TypedExpr {
    fn into_mir(self) -> Result<MirExpr, ErrorValue> {
        match self.expr {
            TypedExprEnum::Is(is) => {
                let mut expr = is.expr.into_mir()?;
                expr.ty = is.ty.into_mir()?;
                Ok(expr)
            }
            _ => unimplemented!(),
        }
    }
}

impl AnnotationType {
    fn into_mir(self) -> Result<MirType, ErrorValue> {
        if let Some(prim) = self.is_primitive() {
            return Ok(MirType::Primitive(prim, self.pos()));
        }

        match self {
            AnnotationType::Type(_, _) => panic!("Custom types are not implemented yet"),
            AnnotationType::Tuple(tup, pos) => Ok(MirType::Tuple(
                (*tup).clone()
                    .into_iter()
                    .map(|ty| ty.into_mir())
                    .collect::<Result<Vec<_>, _>>()?,
                pos,
            )),
            _ => unimplemented!(),
        }
    }
}
