use crate::logger::logger::{Error, ErrorAnnotation, ErrorType, ErrorDisplayType, ErrorOrVec};
use crate::helpers;
use crate::parser::ast;
use crate::typecheck::{ ast_typecheck, typecheck };
use std::rc::Rc;

use inkwell::{builder, context, module, types};

/// Module object
///
/// There is one module object per file (yes, each file has its own codegen, parser, lexer object).
pub struct CodeGenModule<'a> {
    pub module: module::Module<'a>,
    pub context: &'a context::Context,
    pub builder: builder::Builder<'a>,
    pub typecheck: typecheck::TypeCheckModule<'a>,
}

impl<'a> CodeGenModule<'a> {
    /// Return new module object.
    ///
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(
        module: module::Module<'a>,
        builder: builder::Builder<'a>,
        context: &'a context::Context,
        filename: &'a str,
        file_contents: &'a str,
    ) -> CodeGenModule<'a> {
        let typecheck = typecheck::TypeCheckModule::new(filename, file_contents);
        CodeGenModule {
            module,
            context,
            builder,
            typecheck,
        }
    }

    pub fn generate(&mut self) -> Result<(), Vec<Error<'a>>> {
        self.typecheck.type_check()?;

        let mut statements = None;
        std::mem::swap(&mut self.typecheck.parser.ast, &mut statements);
        let mut errors = Vec::new();
        
        for statement in statements.unwrap().nodes {
            match self.gen_stmt(statement) {
                Ok(_) => {},
                Err(e) => {
                    errors.append(&mut e.as_vec());
                }
            }
        }

        if !errors.is_empty() {
            Err(helpers::get_high_priority(errors))
        } else {
            Ok(())
        }
    }

    fn gen_stmt(&mut self, statement: ast::Statement) -> Result<(), ErrorOrVec<'a>> {
        match statement {
            ast::Statement::ExpressionStatement(expr_stmt) => {
                panic!("statement codegen not implemented for expr_stmt")
            }
            ast::Statement::FunctionDefine(func_def) => self.gen_function_define(func_def),
            ast::Statement::Return(ret) => panic!("statement codegen not implemented for return"),
            ast::Statement::TypeAssign(type_assign) => {
                panic!("statement codegen not implemented for type_assign")
            }
            ast::Statement::VariableDeclaration(var_dec) => {
                panic!("statement codegen not implemented for variable_dec")
            }
            ast::Statement::Empty(_) => Ok(()),
        }
    }

    fn gen_function_define(&mut self, func_def: ast::FunctionDefine) -> Result<(), ErrorOrVec<'a>> {
        let ret_type = self.get_type(func_def.return_type.unwrap_type_check())?;
        Ok(())
    }

    fn get_type(&mut self, type_ast: ast_typecheck::TypeCheckType) -> Result<types::BasicTypeEnum<'a>, ErrorOrVec<'a>> {
        match type_ast.value {
            ast_typecheck::TypeCheckTypeType::SingleType(value) => {
                match value.value.is_basic_type() {
                    Ok(basic_type) => match basic_type {
                        "int" => Ok(types::BasicTypeEnum::IntType(self.context.i32_type())),
                        "str" => panic!("str type not implemented yet!"),
                        val => panic!("`{}` type not implemented yet!", val),
                    }
                    Err(_) => panic!("Tried to get basic type from SingleType, but its not a basic type")
                }
            }
            ast_typecheck::TypeCheckTypeType::CustomType(_) => {
                panic!("Custom types not implemented for codegen yet!");
            }
            ast_typecheck::TypeCheckTypeType::ArrayType(_, _) => {
                panic!("Array type not implemented for codegen yet!");
            }
            ast_typecheck::TypeCheckTypeType::TupleType(_) => {
                println!("{:?}", type_ast);
                panic!("Tuples not implemented for codegen yet!");
            }
            ast_typecheck::TypeCheckTypeType::FunctionSig(_, _) => {
                panic!("Function pointers not implemented for codegen yet!");
            }
        }
    }
}
