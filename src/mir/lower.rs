use super::{MirExpr, MirExprEnum, MirStmt, MirType};

use crate::logger::ErrorValue;
use crate::parser::ast;
use crate::typecheck::annotation::*;

use either::Either;
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
            TypedStmtEnum::Tag(tag) => Ok(Some(MirStmt::Tag(super::MirTag { tag }))),
            _ => todo!(),
        }
    }
}

#[derive(Clone)]
pub struct ExprData {
    is_null: bool,
    expr_name: Option<Rc<ast::Namespace>>,
    visibility: Option<ast::Visibility>,
}

impl ExprData {
    pub fn default() -> Self {
        Self {
            is_null: false,
            expr_name: None,
            visibility: None,
        }
    }

    pub fn is_null(mut self, val: bool) -> Self {
        self.is_null = val;
        self
    }

    pub fn expr_name(mut self, val: Option<Rc<ast::Namespace>>) -> Self {
        self.expr_name = val;
        self
    }

    pub fn visibility(mut self, val: Option<ast::Visibility>) -> Self {
        self.visibility = val;
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
            TypedExprEnum::FunctionCall(func_call) => {
                let mut arguments = Vec::with_capacity(func_call.arguments.len());
                for arg in func_call.arguments.into_iter() {
                    match arg.into_mir(mir, expr_data.clone())? {
                        (Some(expr), _) => {
                            arguments.push(expr);
                        }
                        (None, _) => {
                            // Some expression that will prevent this function call
                            // E.g. a return or yield expr
                            // We still want to evaluate every expression before this point;
                            mir.extend(arguments.into_iter().map(|a| MirStmt::Expression(a)));
                            return Ok((None, expr_data));
                        }
                    }
                }

                Ok((
                    Some(MirExpr {
                        value: MirExprEnum::FunctionCall(super::MirFunctionCall {
                            arguments,
                            mangled_name: func_call.name.to_string(),
                            pos: self.pos,
                        }),
                        pos: self.pos,
                        ty: func_call.ty.into_mir()?,
                    }),
                    expr_data,
                ))
            }
            TypedExprEnum::Function(function) => {
                let mut scope = Vec::new();
                match function.block.into_mir(&mut scope, expr_data.clone())?.0 {
                    Some(block) => match function.ty {
                        AnnotationType::Function(pos_args, ret_ty, pos) => {
                            mir.push(MirStmt::FunctionDef {
                                mangled_name: expr_data
                                    .expr_name
                                    .map(|name| Rc::clone(&name).to_string())
                                    .unwrap_or_else(|| "anon".to_string()),
                                signature: super::MirFunctionSig {
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
                                    new_block.stmts.append(&mut scope);
                                    Some(new_block)
                                },
                                visibility: expr_data.visibility.unwrap(),
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
                let expr = match var_assign.expr {
                    Either::Left(expr) => match expr
                        .into_mir(
                            mir,
                            ExprData::default()
                                .visibility(Some(var_assign.visibility))
                                .expr_name(var_assign.binder.name.as_ref().map(|n| Rc::clone(&n))),
                        )?
                        .0
                    {
                        Some(expr) => Either::Left(expr),
                        None => return Ok((None, ExprData::default().is_null(true))),
                    },
                    Either::Right(ty) => match ty.into_mir()? {
                        MirType::FunctionType(pos_args, return_type, pos) => {
                            mir.push(MirStmt::FunctionDef {
                                arg_names: Vec::new(),
                                block: None,
                                visibility: var_assign.visibility,
                                mangled_name: var_assign.binder.name.as_ref().unwrap().to_string(),
                                signature: super::MirFunctionSig {
                                    pos_args,
                                    return_type,
                                    pos,
                                },
                            });

                            let ty = var_assign.binder.ty.into_mir()?;
                            return Ok((
                                Some(MirExpr {
                                    pos: self.pos,
                                    ty: ty.clone(),
                                    value: MirExprEnum::RefID(var_assign.binder.name.unwrap()),
                                }),
                                expr_data,
                            ));
                        }
                        val => Either::Right(val),
                    },
                };

                let ty = var_assign.binder.ty.into_mir()?;
                let ret = Ok((
                    Some(MirExpr {
                        pos: self.pos,
                        ty: ty.clone(),
                        value: MirExprEnum::RefID(var_assign.binder.name.as_ref().map(|n| Rc::clone(&n)).unwrap()),
                    }),
                    expr_data,
                ));

                mir.push(MirStmt::VariableAssign(Box::new(
                    super::MirVariableAssign {
                        var_name: var_assign.binder.name.as_ref().map(|val| Rc::clone(&val)).unwrap(),
                        value: expr,
                        pos: self.pos,
                    },
                )));

                ret
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
            TypedExprEnum::Literal(lit) => Ok((
                Some(super::MirExpr {
                    value: MirExprEnum::Literal,
                    ty: lit.ty.into_mir()?,
                    pos: lit.value.pos,
                }),
                expr_data,
            )),
            TypedExprEnum::Block(block) => {
                let mut stmts = Vec::with_capacity(block.stmts.len());
                let mut metadata = super::BlockMetadata { returns: false };
                for stmt in block.stmts {
                    if let TypedStmtEnum::Expression(TypedExpr {
                        expr: TypedExprEnum::Return(_),
                        ..
                    }) = stmt.stmt
                    {
                        metadata.returns = true;
                    }

                    match stmt.into_mir(&mut stmts)? {
                        Some(s) => stmts.push(s),
                        None => {}
                    };
                }

                let block_mir_ty = block.ty.into_mir()?;

                Ok((
                    Some(super::MirExpr {
                        value: MirExprEnum::Block(super::MirBlock {
                            stmts,
                            metadata,
                            pos: self.pos,
                        }),
                        pos: self.pos,
                        ty: block_mir_ty,
                    }),
                    expr_data,
                ))
            }
            val => todo!("{:#?}", val),
        }
    }
}

impl MirExpr {
    fn into_block(self) -> Result<super::MirBlock, ErrorValue> {
        match self.value {
            MirExprEnum::Block(block) => Ok(block),
            MirExprEnum::RefID(_)
            | MirExprEnum::FunctionCall(_)
            | MirExprEnum::Literal
            | MirExprEnum::Variable(_)
            | MirExprEnum::Conditional(_) => Ok(super::MirBlock {
                metadata: super::BlockMetadata { returns: true },
                pos: self.pos,
                stmts: vec![MirStmt::Yield {
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
            AnnotationType::Function(args, ret, pos) => Ok(MirType::FunctionType(
                (*args)
                    .clone()
                    .into_iter()
                    .map(|ty| ty.into_mir())
                    .collect::<Result<Vec<_>, _>>()?, 
                Box::new((*ret).clone().into_mir()?),
                pos
            )),
            AnnotationType::Infer(_, _) =>
                panic!(
                    "reached unknown type: this shouldn't happen (should have been reported during the typechecking phase)"
                ),
            _ => todo!(),
        }
    }
}
