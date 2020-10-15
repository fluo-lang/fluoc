use super::{MirBlock, MirExpr, MirExprEnum, MirFunctionSig, MirStmt, MirType, Tag};

use crate::logger::ErrorValue;
use crate::typecheck::annotation::*;

use std::rc::Rc;

pub fn lower_to_mir(typed_ast: Vec<TypedStmt>) -> Result<Vec<MirStmt>, Vec<ErrorValue>> {
    let mut mir = Vec::with_capacity(typed_ast.len());
    let mut errors = Vec::new();

    for typed_stmt in typed_ast.into_iter() {
        match typed_stmt.into_mir(&mut mir) {
            Ok(Some(mir_stmt)) => mir.push(mir_stmt),
            Ok(None) => { /* Nothing to do */ }
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
    fn into_mir(self, mir: &mut Vec<MirStmt>) -> Result<Option<MirStmt>, ErrorValue> {
        match self.stmt {
            TypedStmtEnum::Expression(expr) => Ok(Some(MirStmt::Expression(
                match expr.into_mir(mir, ExprData::default())?.0 {
                    Some(expr) => expr,
                    None => return Ok(None),
                },
            ))),
            TypedStmtEnum::Tag(tag) => Ok(Some(MirStmt::Tag(Tag { tag }))),
            _ => todo!(),
        }
    }
}

#[derive(Copy, Clone)]
pub struct ExprData {
    is_null: bool,
}

impl ExprData {
    pub fn default() -> Self {
        Self { is_null: false }
    }

    pub fn is_null(mut self, val: bool) -> Self {
        self.is_null = val;
        self
    }
}

impl TypedExpr {
    fn into_mir(
        self,
        mir: &mut Vec<MirStmt>,
        expr_data: ExprData,
    ) -> Result<(Option<MirExpr>, ExprData), ErrorValue> {
        match self.expr {
            TypedExprEnum::Function(function) => {
                let mut scope = Vec::new();
                match function.block.into_mir(&mut scope, expr_data)?.0 {
                    Some(block) => match function.ty {
                        AnnotationType::Function(pos_args, ret_ty, pos) => {
                            mir.push(MirStmt::FunctionDef {
                                signature: MirFunctionSig {
                                    pos_args: (*pos_args)
                                        .clone()
                                        .into_iter()
                                        .map(|ty| ty.into_mir())
                                        .collect::<Result<Vec<_>, _>>()?,
                                    return_type: Box::new((*ret_ty).clone().into_mir()?),
                                    pos,
                                },
                                block: {
                                    let mut new_block = block.into_block()?;
                                    new_block.nodes.append(&mut scope);
                                    new_block
                                },
                                mangled_name: "test".to_string(),
                                arg_names: function.arg_names,
                            });
                        }
                        _ => panic!(),
                    },
                    None => {}
                };
                Ok((None, ExprData::default().is_null(true)))
            }
            TypedExprEnum::Is(is) => {
                let mut expr = match is.expr.into_mir(mir, ExprData::default())?.0 {
                    Some(expr) => expr,
                    None => return Ok((None, ExprData::default().is_null(true))),
                };
                expr.ty = is.ty.into_mir()?;

                Ok((Some(expr), expr_data))
            }
            TypedExprEnum::VariableAssignDeclaration(var_assign) => {
                let expr = match var_assign.expr.into_mir(mir, ExprData::default())?.0 {
                    Some(expr) => expr,
                    None => return Ok((None, ExprData::default().is_null(true))),
                };
                let ty = var_assign.binder.ty.into_mir()?;

                Ok((
                    Some(MirExpr {
                        pos: self.pos,
                        ty: ty.clone(),
                        value: MirExprEnum::Block(MirBlock {
                            nodes: vec![
                                MirStmt::VariableAssignDeclaration(Box::new(
                                    super::VariableAssignDeclaration {
                                        var_name: Rc::clone(
                                            var_assign.binder.name.as_ref().unwrap(),
                                        ),
                                        value: expr,
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
                    }),
                    expr_data,
                ))
            }
            TypedExprEnum::Return(ret) => {
                if let Some(expr) = ret.expr.into_mir(mir, ExprData::default())?.0 {
                    // Push return statement onto statement list
                    mir.push(MirStmt::Return {
                        value: expr,
                        pos: self.pos,
                    });
                }

                Ok((None, expr_data.is_null(true)))
            }
            TypedExprEnum::Yield(ret) => {
                if let Some(expr) = ret.expr.into_mir(mir, ExprData::default())?.0 {
                    // Push yield statement onto statement list
                    mir.push(MirStmt::Yield {
                        value: expr,
                        pos: self.pos,
                    });
                }
                Ok((None, expr_data.is_null(true)))
            }
            _ => todo!(),
        }
    }
}

impl MirExpr {
    fn into_block(self) -> Result<MirBlock, ErrorValue> {
        match self.value {
            MirExprEnum::Block(block) => Ok(block),
            MirExprEnum::RefID(_)
            | MirExprEnum::Literal(_)
            | MirExprEnum::Variable(_)
            | MirExprEnum::Conditional(_) => Ok(MirBlock {
                metadata: super::BlockMetadata { returns: true },
                pos: self.pos,
                nodes: vec![MirStmt::Return {
                    pos: self.pos,
                    value: self,
                }],
            }),
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
            AnnotationType::Infer(_, _) => panic!("reached unknown type: this shouldn't happen"),
            _ => todo!(),
        }
    }
}
