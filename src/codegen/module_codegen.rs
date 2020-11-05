use crate::helpers;
use crate::logger::{ErrorValue, Logger};
use crate::mir::*;
use crate::parser::ast;
use crate::sourcemap::SourceMap;
use crate::typecheck::{Prim, TypeCheckModule};

use inkwell::types::BasicType;
use inkwell::values::BasicValue;
use inkwell::{builder, context, module, types, values};

use either::Either;

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Deref;
use std::path;
use std::rc::Rc;
use std::time::Instant;

#[derive(Clone)]
pub struct CodeGenSymbTab<'a> {
    items: HashMap<Rc<ast::Namespace>, values::BasicValueEnum<'a>>,
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

    fn insert(&mut self, name: Rc<ast::Namespace>, value: values::BasicValueEnum<'a>) {
        self.items.insert(name, value);
    }

    fn get(&mut self, name: Rc<ast::Namespace>) -> values::BasicValueEnum<'a> {
        println!("{}", name);
        *self.items.get(&name).unwrap()
    }
}

/// Module object
///
/// There is one module object per unit
pub struct CodeGenModule<'a> {
    pub module: module::Module<'a>,
    context: &'a context::Context,
    typecheck: TypeCheckModule,
    builder: builder::Builder<'a>,
    symbtab: CodeGenSymbTab<'a>,
    output_ir: &'a path::Path,
    pub output_obj: &'a path::Path,
    sourcemap: SourceMap,
    logger: Logger,
    current_function: Option<inkwell::values::FunctionValue<'a>>,
}

impl<'a> CodeGenModule<'a> {
    /// Return new module object.
    pub fn new(
        module: module::Module<'a>,
        context: &'a context::Context,
        sourcemap: SourceMap,
        filename_id: usize,
        logger: Logger,
        output_ir: &'a path::Path,
        output_obj: &'a path::Path,
    ) -> CodeGenModule<'a> {
        let typecheck =
            TypeCheckModule::new(filename_id, Rc::clone(&logger), Rc::clone(&sourcemap));
        CodeGenModule {
            module,
            context,
            typecheck,
            builder: context.create_builder(),
            symbtab: CodeGenSymbTab::new(),
            output_ir,
            output_obj,
            sourcemap,
            logger,
            current_function: None,
        }
    }

    pub fn generate(&mut self) -> Result<(), Vec<ErrorValue>> {
        let typed_ast = self.typecheck.type_check()?;
        let mir_rep = lower_to_mir(typed_ast)?;

        let gen_start = Instant::now();

        for statement in &mir_rep {
            self.gen_stmt_pass_1(statement)
        }

        for statement in mir_rep.iter() {
            self.gen_stmt_pass_2(statement)
        }

        println!("{}", self.module.print_to_string().to_string());

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: LLVM IR generated",
                helpers::display_duration(gen_start.elapsed())
            )
        }); // Lazily run it so no impact on performance

        Ok(())
    }

    fn gen_stmt_pass_1(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::FunctionDef(fd) => self.gen_function_prototype(fd),
            MirStmt::VariableAssign(var_assign) => self.gen_var_prototype(var_assign),
            MirStmt::Tag(tag) => {}
            s => todo!("Not implemented: {:?}", s),
        }
    }

    fn gen_stmt_pass_2(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::FunctionDef(fd) => self.gen_function_concrete(fd),
            MirStmt::VariableAssign(var_assign) => self.gen_var_concrete(&*var_assign),
            MirStmt::Tag(tag) => {}
            MirStmt::Return(ret) => self.gen_return_stmt(&ret),

            // Blocks deal with this
            MirStmt::Yield(_) => {}
            MirStmt::Expression(_) => {}
            s => todo!("{:?}", s),
        }
    }

    fn gen_return_stmt(&mut self, ret: &MirReturn) {
        let expr = self.eval_expr(&ret.value);
        self.builder.build_return(Some(&expr));
    }

    fn gen_function_concrete(&mut self, fd: &MirFunctionDef) {
        self.symbtab.clear();

        if let Some(ref block) = fd.block {
            let func = self.module.get_function(&fd.mangled_name).unwrap();
            self.current_function = Some(func);
            let entry_block = self.context.append_basic_block(func, "entry");
            let builder = self.context.create_builder();
            builder.position_at_end(entry_block);
            for (idx, (argument, type_val)) in fd
                .arg_names
                .iter()
                .zip(fd.signature.pos_args.iter())
                .enumerate()
            {
                let arg_type = self.from_mir_ty(type_val);
                let argument_alloca = builder.build_alloca(arg_type, &argument.to_string());
                builder.build_store(
                    argument_alloca,
                    func.get_nth_param(idx.try_into().unwrap()).unwrap(),
                );

                self.symbtab
                    .insert(Rc::clone(&argument), argument_alloca.as_basic_value_enum());
            }
            self.builder = builder;
            self.gen_block(&block);
            self.current_function = None;
        }
    }

    fn gen_function_prototype(&mut self, fd: &MirFunctionDef) {
        let ret_ty = self.from_mir_ty(fd.signature.return_type.as_ref());
        let function_tys: Vec<types::BasicTypeEnum<'_>> = fd
            .signature
            .pos_args
            .iter()
            .map(|arg| self.from_mir_ty(arg))
            .collect();

        self.module
            .add_function(&fd.mangled_name, ret_ty.fn_type(&function_tys, false), None);
    }

    fn gen_var_prototype(&mut self, var_assign: &MirVariableAssign) {
        let ty = self.from_mir_ty(match &var_assign.value {
            Either::Left(expr) => &expr.ty,
            Either::Right(ty) => ty,
        });

        let pointer = self.builder.build_alloca(ty, &var_assign.var_name.mangle());

        self.symbtab.insert(
            Rc::clone(&var_assign.var_name),
            inkwell::values::BasicValueEnum::PointerValue(pointer),
        )
    }

    fn gen_var_concrete(&mut self, var_assign: &MirVariableAssign) {
        match &var_assign.value {
            Either::Left(expr) => {
                let inkwell_expr = self.eval_expr(expr);
                // let var_ptr = self
                //     .symbtab
                //     .get(Rc::clone(&var_assign.var_name))
                //     .into_pointer_value();
                let ty = self.from_mir_ty(&expr.ty);
                let var_ptr = self
                    .builder
                    .build_alloca(ty, &var_assign.var_name.to_string());
                self.builder.build_store(var_ptr, inkwell_expr);
            }
            Either::Right(_) => {}
        }
    }

    fn eval_expr(&mut self, expr: &MirExpr) -> inkwell::values::BasicValueEnum<'a> {
        match &expr.value {
            MirExprEnum::Block(bl) => self.gen_block(&bl),
            MirExprEnum::Variable(var) => self
                .symbtab
                .get(Rc::clone(&var))
                .into_pointer_value()
                .into(),
            MirExprEnum::Literal => self.eval_literal(expr),
            MirExprEnum::FunctionCall(call) => self.eval_function(call),
            _ => todo!(),
        }
    }

    fn eval_function(&mut self, call: &MirFunctionCall) -> inkwell::values::BasicValueEnum<'a> {
        let arguments = &call
            .arguments
            .iter()
            .map(|expr| self.eval_expr(expr))
            .collect::<Vec<_>>();
        self.builder
            .build_call(
                self.module.get_function(&call.mangled_name).unwrap(),
                arguments,
                "call",
            )
            .try_as_basic_value()
            .left()
            .unwrap()
    }

    fn eval_literal(&mut self, expr: &MirExpr) -> inkwell::values::BasicValueEnum<'a> {
        let ty = self.from_mir_ty(&expr.ty);
        match ty {
            inkwell::types::BasicTypeEnum::IntType(int) => {
                if int.get_bit_width() != 1 {
                    int.const_int_from_string(
                        get_segment!(self.sourcemap, expr.pos),
                        inkwell::types::StringRadix::Decimal,
                    )
                    .unwrap()
                    .as_basic_value_enum()
                } else {
                    // Boolean
                    int.const_int(
                        (get_segment!(self.sourcemap, expr.pos) == "true") as u64,
                        false,
                    )
                    .as_basic_value_enum()
                }
            }
            _ => panic!(),
        }
    }

    fn gen_block(&mut self, block: &MirBlock) -> inkwell::values::BasicValueEnum<'a> {
        let ty = self.from_mir_ty(&block.ty);
        let alloca = self.builder.build_alloca(ty, "block");

        for stmt in block.stmts.iter() {
            if let MirStmt::Yield(yield_stmt) = stmt {
                let expr = self.eval_expr(&yield_stmt.value);
                self.builder.build_store(alloca, expr);
                continue;
            }
            self.gen_stmt_pass_2(stmt);
        }

        alloca.as_basic_value_enum()
    }

    fn from_mir_ty(&mut self, mir_ty: &MirType) -> types::BasicTypeEnum<'a> {
        match mir_ty {
            MirType::Primitive(prim, _) => match prim {
                Prim::I8 => self.context.i8_type().into(),
                Prim::I16 => self.context.i16_type().into(),
                Prim::I32 => self.context.i32_type().into(),
                Prim::I64 => self.context.i64_type().into(),
                Prim::I128 => self.context.i128_type().into(),

                Prim::U8 => self.context.i8_type().into(),
                Prim::U16 => self.context.i16_type().into(),
                Prim::U32 => self.context.i32_type().into(),
                Prim::U64 => self.context.i64_type().into(),
                Prim::U128 => self.context.i128_type().into(),

                Prim::Bool => self.context.bool_type().into(),
                Prim::Infer => panic!(),
            },
            MirType::Tuple(tuple, _) => self
                .context
                .struct_type(
                    &tuple
                        .iter()
                        .map(|ty| self.from_mir_ty(ty).into())
                        .collect::<Vec<types::BasicTypeEnum<'_>>>(),
                    false,
                )
                .into(),
            MirType::FunctionType(_, _, _) => panic!(),
            MirType::Never => panic!(),
        }
    }
}
