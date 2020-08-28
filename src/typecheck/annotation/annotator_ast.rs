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
                _ => panic!("Annon type was not a function")
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
                _ => Err(Self::not_a_err(name, "function"))
            },
            None => Err(Self::undefined_symbol(name))
        }
    }

    fn symbol(
        self,
        name: &Rc<ast::Namespace>,
    ) -> Result<&'a AnnotationType, ErrorValue> {
        match *self {
            Some(value) => Ok(value),
            None => Err(Self::undefined_symbol(name))
        }
    }
}

impl ast::FunctionDefine {
    pub fn pass_1(
        &self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        context.set_local(
            Rc::clone(&self.name),
            AnnotationType::Function(
                // Generate typed binder
                Rc::new(self.arguments
                    .positional
                    .iter()
                    .map(|(name, ty)| {
                        TypedBinder::new(
                            Rc::clone(name),
                            annotator.annon_type(ty),
                            Pos::calc(name.pos, ty.pos),
                        )
                    })
                    .collect()),
                Box::new(annotator.annon_type(&self.return_type)),
            ),
        );
        Ok(())
    }

    pub fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedStmt, ErrorValue> {
        let typed = context.get_local(&self.name).unwrap_function();
        let mut new_context = context.clone();
        for binding in typed.0.iter() {
            new_context.set_local(Rc::clone(&binding.name), binding.ty.clone())
        }

        let typed_block = match self.block {
            Some(block) => {
                Some(block.pass_2(annotator, &mut new_context)?)
            }
            None => None
        };

        Ok(TypedStmt {
            stmt: TypedStmtEnum::Function(TypedFunction {
                block: typed_block,
                arguments: Rc::clone(&typed.0),
                return_ty: typed.1.clone(),
                pos: self.pos
            }),
            pos: self.pos,
        })
    }
}

impl ast::Block {
    pub fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedBlock, ErrorValue> {
        Ok(TypedBlock {
            stmts: self.nodes.into_iter().map(|node| node.pass_2(annotator, context)).collect::<Result<Vec<_>, _>>()?,
            ty: annotator.unique(),
            pos: self.pos
        })
    }
}

impl ast::RefID {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::RefID(TypedRefID {
                ty: context.get_local(&self.value).symbol(&self.value)?.clone(),
                name: self.value,
            })
        })
    }
}

impl ast::VariableAssignDeclaration {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        let typed_expr = self.expr.pass_2(annotator, context)?;
        Ok(TypedExpr {
            pos: self.pos,
            expr: TypedExprEnum::VariableAssignDeclaration(TypedAssign {
                expr: Box::new(typed_expr),
                binder: TypedBinder {
                    name: Rc::clone(&self.name),
                    ty: annotator.annon_type(&self.ty),
                    pos: self.pos
                }
            })
        })
    }
}

impl ast::Literal {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        unimplemented!()
    }
}

impl ast::Expr {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedExpr, ErrorValue> {
        match self {
            ast::Expr::RefID(id) => id.pass_2(annotator, context),
            ast::Expr::VariableAssignDeclaration(var_dec) => var_dec.pass_2(annotator, context),
            ast::Expr::Literal(lit) => lit.pass_2(annotator, context),
            _ => unimplemented!()
        }
    }

}

impl ast::ExpressionStatement {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedStmt, ErrorValue> {
        Ok(TypedStmt {
            pos: self.pos,
            stmt: TypedStmtEnum::Expression(self.expression.pass_2(annotator, context)?)
        })
    }
}

impl ast::Statement {
    fn pass_2(
        self,
        annotator: &mut Annotator,
        context: &mut Context<AnnotationType>,
    ) -> Result<TypedStmt, ErrorValue> {
        match self {
            ast::Statement::FunctionDefine(func_def) => func_def.pass_2(annotator, context),
            ast::Statement::ExpressionStatement(expr_stmt) => expr_stmt.pass_2(annotator, context),
            _ => unimplemented!()
        }
    }
}

