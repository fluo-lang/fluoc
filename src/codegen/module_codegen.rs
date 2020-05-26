use crate::helpers;
use crate::logger::logger::{Error, Logger};
use crate::parser::ast;
use crate::typecheck::{ast_typecheck, typecheck};

use inkwell::types::BasicType;
use inkwell::values::BasicValue;
use inkwell::{builder, context, module, types, values};

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Deref;
use std::path;
use std::rc::Rc;
use std::time::Instant;

#[derive(Clone)]
pub struct CodeGenSymbTab<'a> {
    items: HashMap<Rc<ast::Namespace<'a>>, values::BasicValueEnum<'a>>,
}

impl<'a> CodeGenSymbTab<'a> {
    fn new() -> CodeGenSymbTab<'a> {
        CodeGenSymbTab {
            items: HashMap::new(),
        }
    }

    fn clear(&mut self) {
        self.items.clear();
    }

    fn insert(&mut self, name: Rc<ast::Namespace<'a>>, value: values::BasicValueEnum<'a>) {
        self.items.insert(name, value);
    }

    fn get(&mut self, name: Rc<ast::Namespace<'a>>) -> values::BasicValueEnum<'a> {
        *self.items.get(&name).unwrap()
    }
}

/// Module object
///
/// There is one module object per file (yes, each file has its own codegen, parser, lexer object).
pub struct CodeGenModule<'a> {
    pub module: module::Module<'a>,
    pub context: &'a context::Context,
    pub typecheck: typecheck::TypeCheckModule<'a>,
    pub builder: builder::Builder<'a>,
    pub symbtab: CodeGenSymbTab<'a>,
    pub output_file: &'a path::Path,
}

impl<'a> CodeGenModule<'a> {
    /// Return new module object.
    ///
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(
        module: module::Module<'a>,
        context: &'a context::Context,
        filename: &'a path::Path,
        file_contents: &'a str,
        logger: Rc<RefCell<Logger<'a>>>,
        output_file: &'a path::Path,
    ) -> Result<CodeGenModule<'a>, Vec<Error<'a>>> {
        let typecheck = typecheck::TypeCheckModule::new(filename, file_contents, logger, true);
        Ok(CodeGenModule {
            module,
            context,
            typecheck: typecheck?,
            builder: context.create_builder(),
            symbtab: CodeGenSymbTab::new(),
            output_file,
        })
    }

    pub fn generate(&mut self) -> Result<(), Vec<Error<'a>>> {
        self.typecheck.type_check()?;

        let gen_start = Instant::now();
        self.generate_std();

        let mut statements = None;
        std::mem::swap(&mut self.typecheck.parser.ast, &mut statements);
        let statements = statements.unwrap();
        println!("{:#?}", statements.nodes);
        for statement in &statements.nodes {
            self.gen_stmt_pass_1(statement)
        }

        for statement in &statements.nodes {
            self.gen_stmt_pass_2(statement)
        }

        println!("{}", self.module.print_to_string().to_string());
        self.typecheck.parser.logger.borrow().log_verbose(&|| {
            format!(
                "{}: LLVM IR generated",
                helpers::display_duration(gen_start.elapsed())
            )
        }); // Lazily run it so no impact on performance

        Ok(())
    }

    fn gen_stmt_pass_1(&mut self, statement: &ast::Statement<'a>) {
        match statement {
            ast::Statement::FunctionDefine(func_def) => self.gen_function_prototype(func_def),
            ast::Statement::Unit(unit) => self.get_unit_proto(unit),
            _ => {}
        }
    }

    fn gen_stmt_pass_2(&mut self, statement: &ast::Statement<'a>) {
        match statement {
            ast::Statement::FunctionDefine(func_def) => self.gen_function_define(
                func_def,
                self.module
                    .get_function(&func_def.name.to_string()[..])
                    .unwrap(),
            ),
            ast::Statement::Unit(unit) => self.get_unit(unit),
            _ => {}
        }
    }

    fn gen_inner_stmt(&mut self, statement: &ast::Statement<'a>) {
        match statement {
            ast::Statement::ExpressionStatement(expr_stmt) => {
                self.eval_expr(&expr_stmt.expression);
            }
            ast::Statement::Return(ret) => self.gen_return(ret),
            ast::Statement::TypeAssign(_type_assign) => {
                panic!("statement codegen not implemented for type_assign")
            }
            ast::Statement::VariableDeclaration(var_dec) => {
                self.gen_variable_dec(var_dec);
            }
            _ => {}
        };
    }

    fn eval_expr(&mut self, expr: &ast::Expr<'a>) -> values::BasicValueEnum<'a> {
        match expr {
            ast::Expr::VariableAssignDeclaration(variable_assign_dec) => {
                self.eval_variable_assignment_dec(variable_assign_dec)
            }
            ast::Expr::VariableAssign(variable_assign) => {
                self.eval_variable_assign(variable_assign)
            }
            ast::Expr::RefID(ref_id) => self.builder.build_load(
                self.symbtab
                    .get(Rc::clone(&ref_id.value))
                    .into_pointer_value(),
                "temp",
            ),
            ast::Expr::Literal(lit) => self.eval_literal(lit),
            ast::Expr::FunctionCall(func_call) => self.eval_func_call(func_call),
            ast::Expr::Tuple(tuple) => self.eval_tuple(tuple),
            _ => panic!("{:?} not implemented yet", expr.to_str()),
        }
    }

    fn eval_tuple(&mut self, tuple: &ast::Tuple<'a>) -> values::BasicValueEnum<'a> {
        let values: Vec<values::BasicValueEnum> = tuple
            .values
            .iter()
            .map(|value| self.eval_expr(value))
            .collect();
        self.get_type(tuple.type_val.as_ref().unwrap())
            .into_struct_type()
            .const_named_struct(&values[..])
            .into()
    }

    fn eval_func_call(&mut self, func_call: &ast::FunctionCall<'a>) -> values::BasicValueEnum<'a> {
        let arguments: Vec<values::BasicValueEnum<'a>> = func_call
            .arguments
            .positional
            .iter()
            .map(|argument| self.eval_expr(argument))
            .collect();
        let str_val = func_call.name.to_string();
        self.builder
            .build_call(
                self.module.get_function(&str_val[..]).unwrap(),
                &arguments[..],
                &str_val[..],
            )
            .try_as_basic_value()
            .left()
            .unwrap()
    }

    fn eval_variable_assign(
        &mut self,
        var_assign: &ast::VariableAssign<'a>,
    ) -> values::BasicValueEnum<'a> {
        let addr = self
            .symbtab
            .get(Rc::clone(&var_assign.name))
            .into_pointer_value();
        let evaled_expr = self.eval_expr(&var_assign.expr);
        self.builder.build_store(addr, evaled_expr);
        self.symbtab
            .insert(Rc::clone(&var_assign.name), addr.as_basic_value_enum());
        addr.as_basic_value_enum()
    }

    fn eval_literal(&mut self, literal: &ast::Literal<'a>) -> values::BasicValueEnum<'a> {
        match self.get_type(&literal.type_val.as_ref().unwrap()) {
            int_val if int_val.is_int_type() => std::convert::From::from(
                int_val.into_int_type().const_int(
                    literal
                        .value
                        .parse::<u64>()
                        .expect(&format!("Cannot convert `{}` to int", literal.value)[..]),
                    false,
                ),
            ),
            _ => panic!("literal {:?} not implemented yet", literal),
        }
    }

    fn eval_variable_assignment_dec(
        &mut self,
        variable_assign_dec: &ast::VariableAssignDeclaration<'a>,
    ) -> values::BasicValueEnum<'a> {
        let assigned_type = self.get_type(variable_assign_dec.t.unwrap_type_check_ref());
        let expr_alloca = self.builder.build_alloca(
            assigned_type,
            &variable_assign_dec.name.deref().to_string()[..],
        );
        let assigned_val = self.eval_expr(&variable_assign_dec.expr);
        self.builder.build_store(expr_alloca, assigned_val);
        self.symbtab.insert(
            Rc::clone(&variable_assign_dec.name),
            expr_alloca.as_basic_value_enum(),
        );
        expr_alloca.as_basic_value_enum()
    }

    fn gen_variable_dec(&mut self, var_dec: &ast::VariableDeclaration<'a>) {
        let var_type = self.get_type(var_dec.t.unwrap_type_check_ref());
        let var_addr = self
            .builder
            .build_alloca(var_type, &var_dec.name.deref().to_string()[..]);
        self.symbtab
            .insert(Rc::clone(&var_dec.name), std::convert::From::from(var_addr));
    }

    fn get_unit_proto(&mut self, unit: &ast::Unit<'a>) {
        for node in &unit.block.nodes {
            self.gen_stmt_pass_1(node);
        }
    }

    fn get_unit(&mut self, unit: &ast::Unit<'a>) {
        for node in &unit.block.nodes {
            self.gen_stmt_pass_2(node);
        }
    }

    fn gen_function_prototype(&mut self, func_def: &ast::FunctionDefine) {
        let func_name = &func_def.name.to_string()[..];
        let return_type = self.get_type(func_def.return_type.unwrap_type_check_ref());
        let fn_type = return_type.fn_type(
            &func_def
                .arguments
                .positional
                .iter()
                .map(|arg| self.get_type(arg.1.unwrap_type_check_ref()))
                .collect::<Vec<types::BasicTypeEnum>>()[..],
            false,
        );

        let function = self.module.add_function(func_name, fn_type, None);
        function.set_linkage(func_def.visibility.get_linkage());
    }

    fn gen_function_define(
        &mut self,
        func_def: &ast::FunctionDefine<'a>,
        func_val: values::FunctionValue,
    ) {
        self.symbtab.clear();

        let entry_bb = self.context.append_basic_block(func_val, "entry");
        let builder = self.context.create_builder();
        builder.position_at_end(entry_bb);

        for (idx, (argument, type_val)) in func_def.arguments.positional.iter().enumerate() {
            let arg_type = self.get_type(type_val.unwrap_type_check_ref());
            let argument_alloca = builder.build_alloca(arg_type, &argument.value[..]);
            builder.build_store(
                argument_alloca,
                func_val.get_nth_param(idx.try_into().unwrap()).unwrap(),
            );

            self.symbtab.insert(
                Rc::new(argument.into_namespace()),
                argument_alloca.as_basic_value_enum(),
            );
        }

        self.builder = builder;
        for statement in &func_def.block.nodes {
            self.gen_inner_stmt(statement);
        }
    }

    fn gen_return(&mut self, ret: &ast::Return<'a>) {
        let ret_val = self.eval_expr(&ret.expression);
        self.builder.build_return(Some(&ret_val));
    }

    fn get_type(&mut self, type_ast: &ast_typecheck::TypeCheckType) -> types::BasicTypeEnum<'a> {
        match &type_ast.value {
            ast_typecheck::TypeCheckTypeType::SingleType(value) => {
                match value.value.is_basic_type() {
                    Ok(basic_type) => match basic_type {
                        "int" => self.context.i32_type().into(),
                        "str" => self.context.i8_type().array_type(10).into(),
                        val => panic!("`{}` type not implemented yet!", val),
                    },
                    Err(_) => {
                        panic!("Tried to get basic type from SingleType, but its not a basic type",)
                    }
                }
            }
            ast_typecheck::TypeCheckTypeType::CustomType(_, value) => {
                self.get_type(&*(value.as_ref().unwrap()))
            }
            ast_typecheck::TypeCheckTypeType::ArrayType(_, _) => {
                panic!("Array type not implemented for codegen yet!");
            }
            ast_typecheck::TypeCheckTypeType::TupleType(tuple_union) => {
                let item_types: Vec<types::BasicTypeEnum> = tuple_union
                    .types
                    .iter()
                    .map(|type_val| self.get_type(type_val))
                    .collect();
                self.context.struct_type(&item_types[..], false).into()
            }
            ast_typecheck::TypeCheckTypeType::FunctionSig(_, _, _) => {
                panic!("Function pointers not implemented for codegen yet!");
            }
            ast_typecheck::TypeCheckTypeType::Placeholder => {
                panic!("Tried to turn placeholder value into llvm type")
            }
        }
    }
}
