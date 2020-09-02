use super::{typed_ast::*, AnnotationType, Annotator};

use crate::helpers::Pos;
use crate::logger::{ErrorAnnotation, ErrorDisplayType, ErrorType, ErrorValue};
use crate::parser::ast;
use crate::typecheck::context::{Context, TOption};

use std::rc::Rc;

impl<'a> TOption<&'a AnnotationType> {
    fn unwrap_function(self) -> (Rc<Vec<TypedBinder>>, &'a AnnotationType) {
        match *self {
            Some(value) => match value {
                AnnotationType::Function(args, ret) => (Rc::clone(&args), &*ret),
                _ => panic!("Annon type was not a function"),
            },
            None => panic!("Unwrapped none values on unwrap_function"),
        }
    }

    fn not_a_err(name: &Rc<ast::Namespace>, err_name: &'static str) -> ErrorValue {
        let err_msg = format!("`{}` is not a {}", name.to_string(), err_name);
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

    fn function(
        self,
        name: &Rc<ast::Namespace>,
    ) -> Result<(Rc<Vec<TypedBinder>>, &'a AnnotationType), ErrorValue> {
        match *self {
            Some(value) => match value {
                AnnotationType::Function(args, ret) => Ok((Rc::clone(args), &*ret)),
                _ => Err(Self::not_a_err(name, "function")),
            },
            None => Err(Self::undefined_symbol(name)),
        }
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
        &self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        self.block.pass_1(annotator, context);
        Ok(())
    }

    pub fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedStmt, ErrorValue> {
        let args: Vec<_> = self
            .arguments
            .positional
            .iter()
            .map(|(name, ty)| {
                TypedBinder::new(
                    Rc::clone(name),
                    annotator.annon_type(ty),
                    Pos::calc(name.pos, ty.pos),
                )
            })
            .collect();
        let mut new_context = context.clone();

        for binding in args.iter() {
            new_context.set_local(Rc::clone(&binding.name), binding.ty.clone())
        }

        let block = self.block.pass_2(annotator, &mut new_context)?;

        Ok(TypedStmt {
            stmt: TypedStmtEnum::Function(TypedFunction {
                ty: AnnotationType::Function(
                    Rc::new(args),
                    Box::new(annotator.annon_type(&self.return_type)),
                ),
                block,
            }),
            pos: self.pos,
        })
    }
}

impl ast::Block {
    pub fn pass_1(
        &self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        for stmt in &self.nodes {
            stmt.pass_1(annotator, context)?;
        }

        Ok(())
    }

    pub fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedBlock, ErrorValue> {
        Ok(TypedBlock {
            stmts: self
                .nodes
                .into_iter()
                .map(|node| node.pass_2(annotator, context))
                .collect::<Result<Vec<_>, _>>()?,
            ty: annotator.unique(),
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
        &self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        let typed_type = annotator.annon_type(&self.ty);
        let typed_expr = self.expr.pass_1(annotator, context)?;

        context.set_local(Rc::clone(&self.name), typed_type.clone());
        Ok(())
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
                    name: Rc::clone(&self.name),
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
                ty: annotator.unique(),
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
        let ret_ty = func_sig.function(&self.name)?.1.clone();

        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::FunctionCall(TypedFunctionCall {
                ty: ret_ty,
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

impl ast::Expr {
    fn pass_1(
        &self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        match self {
            ast::Expr::Function(func) => func.pass_1(annotator, context),
            ast::Expr::VariableAssignDeclaration(var_dec) => var_dec.pass_1(annotator, context),
            _ => panic!("Unimplemented {}", self.as_str()),
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
            _ => panic!("Unimplemented {}", self.as_str()),
        }
    }
}

impl ast::ExpressionStatement {
    pub fn pass_1(
        &self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        self.expression.pass_1(annotator, context)
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
            }),
        })
    }
}

impl ast::Statement {
    fn pass_1(
        &self,
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
