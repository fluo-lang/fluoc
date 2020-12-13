use super::{MirExpr, MirExprEnum, MirStmt, MirType};

use crate::logger::{ErrorAnnotation, ErrorDisplayType, ErrorType, ErrorValue};
use crate::parser::{ast, ast::Namespace};
use crate::typecheck::annotation::*;

use either::Either;
use std::rc::Rc;

pub fn lower_to_mir(typed_ast: Vec<TypedStmt>) -> Result<Vec<MirStmt>, Vec<ErrorValue>> {
    let mut outer_mir = Vec::new();
    let mut inner_mir = Vec::with_capacity(typed_ast.len());
    let mut errors = Vec::new();

    for typed_stmt in typed_ast.into_iter() {
        match typed_stmt.into_mir(&mut outer_mir, &mut inner_mir) {
            Ok(Some(mir_stmt)) => inner_mir.push(mir_stmt),
            Ok(None) => { /* Nothing to do */ }
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        outer_mir.append(&mut inner_mir);
        Ok(outer_mir)
    } else {
        Err(errors)
    }
}

impl TypedStmt {
    fn into_mir(
        self,
        outer_mir: &mut Vec<MirStmt>,
        inner_mir: &mut Vec<MirStmt>,
    ) -> Result<Option<MirStmt>, ErrorValue> {
        match self.stmt {
            TypedStmtEnum::Expression(expr) => Ok(Some(MirStmt::Expression(
                match expr.into_mir(outer_mir, inner_mir, ExprData::default())?.0 {
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
    expr_name: Option<Rc<Namespace>>,
    visibility: Option<ast::Visibility>,
}

impl Default for ExprData {
    fn default() -> Self {
        Self {
            is_null: false,
            expr_name: None,
            visibility: None,
        }
    }
}

impl ExprData {
    fn is_null(mut self, val: bool) -> Self {
        self.is_null = val;
        self
    }

    fn expr_name(mut self, val: Option<Rc<Namespace>>) -> Self {
        self.expr_name = val;
        self
    }

    fn visibility(mut self, val: Option<ast::Visibility>) -> Self {
        self.visibility = val;
        self
    }
}

impl MirStmt {
    pub fn diverges(&self) -> bool {
        match self {
            MirStmt::Return(_) => true,
            MirStmt::Expression(expr) => expr.diverges(),
            MirStmt::VariableAssign(var) => var
                .value
                .as_ref()
                .map_left(|expr| expr.diverges())
                .left_or(false),
            _ => false,
        }
    }
}

impl MirExpr {
    fn diverges(&self) -> bool {
        match &self.value {
            MirExprEnum::Block(bl) => bl.stmts.iter().any(|stmt| stmt.diverges()),
            _ => false,
        }
    }
}

/// Evaluate a list of expressions. If the expression has type `!` (e.g. return expressions),
/// we evaluate all expressions before it.
fn convert_exprs(
    outer_mir: &mut Vec<MirStmt>,
    inner_mir: &mut Vec<MirStmt>,
    exprs: Vec<TypedExpr>,
    expr_data: ExprData,
) -> Result<Option<Vec<MirExpr>>, ErrorValue> {
    let mut mir_exprs = Vec::with_capacity(exprs.len());
    for expr in exprs {
        match expr.into_mir(outer_mir, inner_mir, expr_data.clone())? {
            (Some(expr), _) => {
                mir_exprs.push(expr);
            }
            (None, _) => {
                // Some expression that will prevent this function call
                // E.g. a return or yield expr
                // We still want to evaluate every expression before this point;
                inner_mir.extend(mir_exprs.into_iter().map(|a| MirStmt::Expression(a)));
                return Ok(None);
            }
        }
    }
    Ok(Some(mir_exprs))
}

impl TypedExpr {
    fn into_mir(
        self,
        outer_mir: &mut Vec<MirStmt>,
        inner_mir: &mut Vec<MirStmt>,
        expr_data: ExprData,
    ) -> Result<(Option<MirExpr>, ExprData), ErrorValue> {
        match self.expr {
            TypedExprEnum::FunctionCall(func_call) => {
                let mut arguments = Vec::with_capacity(func_call.arguments.len());
                for arg in func_call.arguments.into_iter() {
                    match arg.into_mir(outer_mir, inner_mir, expr_data.clone())? {
                        (Some(expr), _) => {
                            arguments.push(expr);
                        }
                        (None, _) => {
                            // Some expression that will prevent this function call
                            // E.g. a return or yield expr
                            // We still want to evaluate every expression before this point;
                            inner_mir.extend(arguments.into_iter().map(|a| MirStmt::Expression(a)));
                            return Ok((None, expr_data));
                        }
                    }
                }

                Ok((
                    Some(MirExpr {
                        value: MirExprEnum::FunctionCall(super::MirFunctionCall {
                            arguments,
                            mangled_name: func_call.name.to_string(),
                        }),
                        pos: self.pos,
                        ty: func_call.ty.into_mir()?,
                    }),
                    expr_data,
                ))
            }
            TypedExprEnum::Function(function) => {
                let mut scope = Vec::new();
                match function
                    .block
                    .into_mir(outer_mir, &mut scope, expr_data.clone())?
                    .0
                {
                    Some(block) => match function.ty {
                        AnnotationType::Function(pos_args, ret_ty, pos) => {
                            outer_mir.push(MirStmt::FunctionDef(super::MirFunctionDef {
                                mangled_name: expr_data
                                    .expr_name
                                    .map(|name| Rc::clone(&name).to_string())
                                    .unwrap_or_else(|| format!("anon_{}_{}", pos.s, pos.e)),
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
                            }));
                        }
                        _ => panic!(),
                    },
                    None => {}
                };
                Ok((None, ExprData::default().is_null(true)))
            }
            TypedExprEnum::Is(is) => {
                let mut expr = match is
                    .expr
                    .into_mir(outer_mir, inner_mir, ExprData::default())?
                    .0
                {
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
                            outer_mir,
                            inner_mir,
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
                            inner_mir.push(MirStmt::FunctionDef(super::MirFunctionDef {
                                arg_names: Vec::new(),
                                block: None,
                                visibility: var_assign.visibility,
                                mangled_name: var_assign.binder.name.as_ref().unwrap().to_string(),
                                signature: super::MirFunctionSig {
                                    pos_args,
                                    return_type,
                                    pos,
                                },
                            }));

                            let ty = var_assign.binder.ty.into_mir()?;
                            return Ok((
                                Some(MirExpr {
                                    pos: self.pos,
                                    ty: ty.clone(),
                                    value: MirExprEnum::Variable(var_assign.binder.name.unwrap()),
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
                        value: MirExprEnum::Variable(
                            var_assign
                                .binder
                                .name
                                .as_ref()
                                .map(|n| Rc::clone(&n))
                                .unwrap(),
                        ),
                    }),
                    expr_data,
                ));

                inner_mir.push(MirStmt::VariableAssign(Box::new(
                    super::MirVariableAssign {
                        var_name: var_assign
                            .binder
                            .name
                            .as_ref()
                            .map(|val| Rc::clone(&val))
                            .unwrap(),
                        value: expr,
                        pos: self.pos,
                    },
                )));

                ret
            }
            TypedExprEnum::RefID(id) => Ok((
                Some(MirExpr {
                    pos: self.pos,
                    value: MirExprEnum::Variable(id.name),
                    ty: id.ty.into_mir()?,
                }),
                expr_data,
            )),
            TypedExprEnum::Return(ret) => {
                if let Some(expr) = ret
                    .expr
                    .into_mir(outer_mir, inner_mir, ExprData::default())?
                    .0
                {
                    // Push return statement onto statement list
                    inner_mir.push(MirStmt::Return(super::MirReturn {
                        value: expr,
                        pos: self.pos,
                    }));
                }

                Ok((None, expr_data.is_null(true)))
            }
            TypedExprEnum::Yield(ret) => {
                if let Some(expr) = ret
                    .expr
                    .into_mir(outer_mir, inner_mir, ExprData::default())?
                    .0
                {
                    // Push yield statement onto statement list
                    inner_mir.push(MirStmt::Yield(super::MirYield {
                        value: expr,
                        pos: self.pos,
                    }));
                }
                Ok((None, expr_data.is_null(true)))
            }
            TypedExprEnum::Literal(lit) => Ok((
                Some(MirExpr {
                    value: MirExprEnum::Literal,
                    ty: lit.ty.into_mir()?,
                    pos: lit.value.pos,
                }),
                expr_data,
            )),
            TypedExprEnum::Block(block) => {
                let mut stmts = Vec::with_capacity(block.stmts.len());

                for stmt in block.stmts {
                    match stmt.into_mir(outer_mir, &mut stmts)? {
                        Some(s) => stmts.push(s),
                        None => {}
                    };
                }

                let metadata = super::BlockMetadata {
                    diverges: stmts.iter().any(|stmt: &MirStmt| stmt.diverges()),
                };

                let block_mir_ty = block.ty.into_mir()?;

                Ok((
                    Some(super::MirExpr {
                        value: MirExprEnum::Block(super::MirBlock {
                            stmts,
                            metadata,
                            ty: block_mir_ty.clone(),
                            pos: self.pos,
                        }),
                        pos: self.pos,
                        ty: block_mir_ty,
                    }),
                    expr_data,
                ))
            }
            TypedExprEnum::Tuple(tup) => {
                let mir_exprs =
                    match convert_exprs(outer_mir, inner_mir, tup.exprs, expr_data.clone())? {
                        Some(fields) => fields,
                        None => return Ok((None, expr_data)),
                    };

                Ok((
                    Some(super::MirExpr {
                        ty: MirType::Union(
                            mir_exprs.iter().map(|e| e.ty.clone()).collect(),
                            tup.pos,
                        ),
                        value: MirExprEnum::Struct(super::MirStruct { fields: mir_exprs }),
                        pos: tup.pos,
                    }),
                    expr_data,
                ))
            }
        }
    }
}

impl MirExpr {
    fn into_block(self) -> Result<super::MirBlock, ErrorValue> {
        let var_name = match self.value {
            MirExprEnum::Block(block) => Ok(block),
            MirExprEnum::FunctionCall(_)
            | MirExprEnum::Literal
            | MirExprEnum::Variable(_)
            | MirExprEnum::Struct(_)
            | MirExprEnum::Conditional(_) => Ok(super::MirBlock {
                metadata: super::BlockMetadata {
                    diverges: self.diverges(),
                },
                pos: self.pos,
                ty: self.ty.clone(),
                stmts: vec![MirStmt::Yield(super::MirYield {
                    pos: self.pos,
                    value: self,
                })],
            }),
        };
        var_name
    }
}

impl AnnotationType {
    fn into_mir(self) -> Result<MirType, ErrorValue> {
        if let Some(prim) = self.is_primitive() {
            return Ok(MirType::Primitive(prim, self.pos()));
        }

        match self {
            AnnotationType::Type(_, _) => panic!("Custom types are not implemented yet"),
            AnnotationType::Tuple(tup, pos) => Ok(MirType::Union(
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
                pos,
            )),
            AnnotationType::Infer(_, _, pos) => Err(ErrorValue::new(
                format!("reached unknown type {:?}: this shouldn't happen", self),
                ErrorType::InternalError,
                pos,
                ErrorDisplayType::Error,
                vec![ErrorAnnotation::new(None, pos, ErrorDisplayType::Error)],
            )),
            _ => todo!(),
        }
    }
}
