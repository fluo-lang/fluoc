use super::{FunctionSig, MirExpr, MirExprEnum, MirStmt, MirType, Tag};

use crate::logger::ErrorValue;
use crate::typecheck::annotation::*;

use std::rc::Rc;

pub fn lower_to_mir(typed_ast: Vec<TypedStmt>) -> Result<Vec<MirStmt>, Vec<ErrorValue>> {
    let mut mir = Vec::with_capacity(typed_ast.len());
    let mut errors = Vec::new();

    for typed_stmt in typed_ast.into_iter() {
        match typed_stmt.into_mir(&mut mir) {
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
    fn into_mir(self, mir: &mut Vec<MirStmt>) -> Result<MirStmt, ErrorValue> {
        match self.stmt {
            TypedStmtEnum::Expression(expr) => Ok(MirStmt::Expression(expr.into_mir(mir)?)),
            TypedStmtEnum::Tag(tag) => Ok(MirStmt::Tag(Tag { tag })),
            _ => todo!(),
        }
    }
}

impl TypedExpr {
    fn into_mir(self, mir: &mut Vec<MirStmt>) -> Result<MirExpr, ErrorValue> {
        println!("{:?}", self);
        match self.expr {
            TypedExprEnum::Is(is) => {
                let mut expr = is.expr.into_mir(mir)?;
                expr.ty = is.ty.into_mir()?;
                Ok(expr)
            }
            TypedExprEnum::VariableAssignDeclaration(var_assign) => {
                let ty = var_assign.binder.ty.into_mir()?;
                Ok(MirExpr {
                    pos: self.pos,
                    ty: ty.clone(),
                    value: MirExprEnum::Block(super::Block {
                        nodes: vec![
                            MirStmt::VariableAssignDeclaration(Box::new(
                                super::VariableAssignDeclaration {
                                    var_name: Rc::clone(var_assign.binder.name.as_ref().unwrap()),
                                    value: var_assign.expr.into_mir(mir)?,
                                    pos: self.pos,
                                },
                            )),
                            MirStmt::Yield {
                                value: MirExpr {
                                    value: MirExprEnum::RefID(var_assign.binder.name.unwrap()),
                                    pos: self.pos,
                                    ty: ty,
                                },
                                pos: self.pos,
                            },
                        ],
                        metadata: super::BlockMetadata { returns: true },
                        pos: self.pos,
                    }),
                })
            }
            TypedExprEnum::Return(ret) => {
                let expr = ret.expr.into_mir(mir)?;

                // Push return statement onto statement list
                mir.push(MirStmt::Return {
                    value: expr,
                    pos: self.pos,
                });
                Ok(MirExpr {
                    value: MirExprEnum::Never,
                    ty: MirType::Never,
                    pos: self.pos,
                })
            }
            TypedExprEnum::Yield(ret) => {
                let expr = ret.expr.into_mir(mir)?;

                // Push yield statement onto statement list
                mir.push(MirStmt::Yield {
                    value: expr,
                    pos: self.pos,
                });
                Ok(MirExpr {
                    value: MirExprEnum::Never,
                    ty: MirType::Never,
                    pos: self.pos,
                })
            }
            _ => todo!(),
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
                (*tup)
                    .clone()
                    .into_iter()
                    .map(|ty| ty.into_mir())
                    .collect::<Result<Vec<_>, _>>()?,
                pos,
            )),
            AnnotationType::Function(args, ret, pos) => Ok(MirType::FunctionSig(
                FunctionSig {
                    pos_args: (*args)
                        .clone()
                        .into_iter()
                        .map(|ty| ty.into_mir())
                        .collect::<Result<Vec<_>, _>>()?,
                    return_type: Box::new((*ret).clone().into_mir()?),
                    pos,
                },
                pos,
            )),
            AnnotationType::Infer(_, _) => {
                panic!("reached unknown type: this shouldn't happen")
            }
            _ => todo!(),
        }
    }
}
