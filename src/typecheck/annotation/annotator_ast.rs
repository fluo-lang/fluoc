use super::{typed_ast::*, AnnotationType, Annotator};

use crate::logger::{not_a_err, ErrorAnnotation, ErrorDisplayType, ErrorType, ErrorValue};
use crate::parser::ast;
use crate::typecheck::context::{Context, TOption};

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

        for (ty, (name, _)) in args.iter().zip(self.arguments.positional) {
            new_context.set_local(name, ty.clone())
        }

        let block = self.block.pass_2(annotator, &mut new_context)?;

        Ok(TypedExpr {
            expr: TypedExprEnum::Function(TypedFunction {
                ty: self.ty.unwrap(),
                block: Box::new(block),
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
        for stmt in self.nodes.iter_mut() {
            stmt.pass_1(annotator, context)?;
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
        Ok(TypedExpr {
            expr: TypedExprEnum::Block(TypedBlock {
                ty: self.ty.unwrap(),
                stmts: self
                    .nodes
                    .into_iter()
                    .map(|node| node.pass_2(annotator, context))
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
        let typed_type = self.expr.pass_1(annotator, context)?;

        context.set_local(Rc::clone(&self.name), typed_type.clone());
        Ok(typed_type)
    }

    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        let typed_type = annotator.annon_type(&self.ty);
        let typed_expr = self.expr.pass_2(annotator, context)?;

        context.set_local(Rc::clone(&self.name), typed_type.clone());

        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::VariableAssignDeclaration(TypedAssign {
                expr: Box::new(typed_expr),
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
                ty: annotator.unique(self.pos),
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
            AnnotationType::Infer(_, _) => annotator.unique(self.pos),
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
