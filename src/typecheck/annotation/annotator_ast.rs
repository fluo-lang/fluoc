use super::{typed_ast::*, AdditionalContraint, AnnotationType, Annotator};

use crate::logger::{not_a_err, ErrorAnnotation, ErrorDisplayType, ErrorType, ErrorValue};
use crate::parser::ast;
use crate::typecheck::context::{Context, TOption};

use either::Either;
use std::rc::Rc;

impl<'a> TOption<&'a AnnotationType> {
    fn undefined_symbol(name: &Rc<ast::Namespace>) -> ErrorValue {
        let err_msg = format!("undefined symbol `{}`", name.to_string());
        ErrorValue::new(
            err_msg,
            ErrorType::UndefinedSymbol,
            name.pos,
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                None,
                name.pos,
                ErrorDisplayType::Error,
            )],
        )
    }

    fn symbol(self, name: &Rc<ast::Namespace>) -> Result<&'a AnnotationType, ErrorValue> {
        match *self {
            Some(value) => Ok(value),
            None => Err(Self::undefined_symbol(name)),
        }
    }
}

impl ast::Function {
    pub fn pass_1(
        &mut self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<AnnotationType, ErrorValue> {
        self.block.pass_1(annotator, context)?;
        let args: Vec<_> = self
            .arguments
            .positional
            .iter()
            .map(|(_, ty)| annotator.annon_type(ty))
            .collect();

        let ty = AnnotationType::Function(
            Rc::new(args),
            Rc::new(annotator.annon_type(&self.return_type)),
            self.pos,
        );
        self.ty = Some(ty.clone());
        Ok(ty)
    }

    pub fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        let args = match self.ty.clone().unwrap() {
            AnnotationType::Function(args, _, _) => Rc::clone(&args),
            _ => unreachable!(),
        };

        let mut new_context = context.clone();
        let mut arg_names = Vec::with_capacity(args.len());

        for (ty, (name, _)) in args.iter().zip(self.arguments.positional) {
            arg_names.push(Rc::clone(&name));
            new_context.set_local(name, ty.clone());
        }

        let block = self.block.pass_2(annotator, &mut new_context)?;

        Ok(TypedExpr {
            expr: TypedExprEnum::Function(TypedFunction {
                ty: self.ty.unwrap(),
                block: Box::new(block),
                arg_names,
            }),
            pos: self.pos,
        })
    }
}

impl ast::Block {
    pub fn pass_1(
        &mut self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<AnnotationType, ErrorValue> {
        let mut returns = false;
        let mut new_context = context.clone();

        for stmt in self.nodes.iter_mut() {
            stmt.pass_1(annotator, &mut new_context)?;
            if let ast::Statement::ExpressionStatement(expr) = stmt {
                if let ast::Expr::Yield(_) | ast::Expr::Return(_) = *expr.expression {
                    returns = true;
                }
            }
        }

        if !returns {
            self.nodes.push(ast::Statement::ExpressionStatement(
                ast::ExpressionStatement {
                    expression: Box::new(ast::Expr::Yield(ast::Yield {
                        expression: Box::new(ast::Expr::Tuple(ast::Tuple {
                            values: Vec::new(),
                            pos: self.pos,
                        })),
                        pos: self.pos,
                    })),
                    pos: self.pos,
                },
            ))
        }

        let ty = annotator.unique(self.pos);
        self.ty = Some(ty.clone());
        Ok(ty)
    }

    pub fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        let mut new_context = context.clone();
        Ok(TypedExpr {
            expr: TypedExprEnum::Block(TypedBlock {
                ty: self.ty.unwrap_or(annotator.unique(self.pos)),
                stmts: self
                    .nodes
                    .into_iter()
                    .map(|node| node.pass_2(annotator, &mut new_context))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            pos: self.pos,
        })
    }
}

impl ast::RefID {
    fn pass_2(
        self,
        _annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::RefID(TypedRefID {
                ty: context.get_local(&self.value).symbol(&self.value)?.clone(),
                name: self.value,
            }),
        })
    }
}

impl ast::VariableAssignDeclaration {
    fn pass_1(
        &mut self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<AnnotationType, ErrorValue> {
        let typed_type = match annotator.annon_type(&self.ty) {
            ty @ AnnotationType::Infer(_, _, _) => {
                // Let's see if the expr has a type...
                if let Some(expr) = &mut self.expr {
                    let possible = expr.pass_1(annotator, context)?;
                    // ...and make sure it's not an infer
                    if let AnnotationType::Infer(_, _, _) = possible {
                        ty
                    } else {
                        possible
                    }
                } else {
                    ty
                }
            }
            ty => ty,
        };

        context.set_local(Rc::clone(&self.name), typed_type.clone());
        self.typecheck_type = Some(typed_type.clone());
        Ok(typed_type)
    }

    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        let typed_type = match self.typecheck_type {
            Some(ty) => ty,
            None => annotator.annon_type(&self.ty),
        };
        let typed_expr = match self.expr {
            Some(expr) => Either::Left(Box::new(expr.pass_2(annotator, context)?)),
            None => Either::Right(typed_type.clone()),
        };

        context.set_local(Rc::clone(&self.name), typed_type.clone());

        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::VariableAssignDeclaration(TypedAssign {
                expr: typed_expr,
                visibility: self.visibility,
                binder: TypedBinder {
                    name: Some(Rc::clone(&self.name)),
                    ty: typed_type,
                    pos: self.pos,
                },
            }),
        })
    }
}

impl ast::Literal {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        _context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::Literal(TypedLiteral {
                ty: annotator.unique_literal(
                    self.pos,
                    Some(AdditionalContraint::Literal(self.literal_type).into()),
                ),
                value: self,
            }),
        })
    }
}

impl ast::FunctionCall {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        let func_sig = context.get_local(&self.name);
        let func_ty = func_sig.symbol(&self.name)?.clone();

        let ret_ty = match func_ty {
            AnnotationType::Function(_, ref ret, _) => (**ret).clone(),
            AnnotationType::Infer(_, ref con, _) => {
                annotator.unique_literal(self.pos, dbg!(con).clone())
            }
            AnnotationType::Never(_) => func_ty.clone(),
            _ => return Err(not_a_err(&self.name, "function")),
        };

        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::FunctionCall(TypedFunctionCall {
                ty: ret_ty,
                func_ty,
                name: Rc::clone(&self.name),
                arguments: self
                    .arguments
                    .positional
                    .into_iter()
                    .map(|expr| expr.pass_2(annotator, context))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
        })
    }
}

impl ast::IsExpr {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::Is(TypedIs {
                ty: annotator.annon_type(&self.ty),
                expr: Box::new(self.expr.pass_2(annotator, context)?),
            }),
        })
    }
}

impl ast::Tuple {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::Tuple(TypedTuple {
                ty: annotator.unique(self.pos),
                exprs: self
                    .values
                    .into_iter()
                    .map(|expr| expr.pass_2(annotator, context))
                    .collect::<Result<Vec<_>, _>>()?,
                pos: self.pos,
            }),
        })
    }
}

impl ast::Expr {
    fn pass_1(
        &mut self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<AnnotationType, ErrorValue> {
        match self {
            ast::Expr::Function(func) => func.pass_1(annotator, context),
            ast::Expr::VariableAssignDeclaration(var_dec) => var_dec.pass_1(annotator, context),
            ast::Expr::Block(block) => block.pass_1(annotator, context),
            ast::Expr::Yield(yield_val) => yield_val.pass_1(annotator, context),
            ast::Expr::Return(return_val) => return_val.pass_1(annotator, context),

            _ => Ok(annotator.unique(self.pos())),
        }
    }

    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        match self {
            ast::Expr::RefID(id) => id.pass_2(annotator, context),
            ast::Expr::VariableAssignDeclaration(var_dec) => var_dec.pass_2(annotator, context),
            ast::Expr::Literal(lit) => lit.pass_2(annotator, context),
            ast::Expr::Is(is) => is.pass_2(annotator, context),
            ast::Expr::FunctionCall(func_call) => func_call.pass_2(annotator, context),
            ast::Expr::Yield(yield_val) => yield_val.pass_2(annotator, context),
            ast::Expr::Return(return_val) => return_val.pass_2(annotator, context),
            ast::Expr::Function(func) => func.pass_2(annotator, context),
            ast::Expr::Block(block) => block.pass_2(annotator, context),
            ast::Expr::Tuple(tuple) => tuple.pass_2(annotator, context),
            _ => panic!("Unimplemented {}", self.as_str()),
        }
    }
}

impl ast::ExpressionStatement {
    pub fn pass_1(
        &mut self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        self.expression.pass_1(annotator, context)?;
        Ok(())
    }

    pub fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedStmt, ErrorValue> {
        Ok(TypedStmt {
            pos: self.pos,
            stmt: TypedStmtEnum::Expression(self.expression.pass_2(annotator, context)?),
        })
    }
}

impl ast::Return {
    fn pass_1(
        &mut self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<AnnotationType, ErrorValue> {
        Ok(self.expression.pass_1(annotator, context)?)
    }

    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::Return(TypedReturn {
                expr: Box::new(self.expression.pass_2(annotator, context)?),
                ty: AnnotationType::Never(self.pos),
            }),
        })
    }
}

impl ast::Yield {
    fn pass_1(
        &mut self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<AnnotationType, ErrorValue> {
        Ok(self.expression.pass_1(annotator, context)?)
    }

    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::Yield(TypedYield {
                expr: Box::new(self.expression.pass_2(annotator, context)?),
                ty: AnnotationType::Never(self.pos),
            }),
        })
    }
}

impl ast::Statement {
    fn pass_1(
        &mut self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        match self {
            ast::Statement::ExpressionStatement(expr_stmt) => expr_stmt.pass_1(annotator, context),
            _ => panic!("Unimplemented {}", self.as_str()),
        }
    }

    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedStmt, ErrorValue> {
        match self {
            ast::Statement::ExpressionStatement(expr_stmt) => expr_stmt.pass_2(annotator, context),
            _ => panic!("Unimplemented {}", self.as_str()),
        }
    }
}
