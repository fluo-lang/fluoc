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
        }
    }

    pub fn generate(&mut self) -> Result<(), Vec<ErrorValue>> {
        let typed_ast = self.typecheck.type_check()?;
        let mir_rep = lower_to_mir(typed_ast)?;

        let gen_start = Instant::now();

        for statement in &mir_rep {
            self.gen_stmt_pass_1(statement)
        }

        /*
        for statement in &typed_ast {
            self.gen_stmt_pass_2(statement)
        }
        */

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: LLVM IR generated",
                helpers::display_duration(gen_start.elapsed())
            )
        }); // Lazily run it so no impact on performance

        Ok(())
    }

    pub fn gen_stmt_pass_1(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::FunctionDef {
                signature,
                mangled_name,
                visibility,
                ..
            } => self.gen_function_prototype(signature, mangled_name, *visibility),
            MirStmt::VariableAssign(var_assign) => self.gen_var_prototype(var_assign),
            _ => todo!(),
        }
    }

    pub fn gen_function_prototype<'b>(
        &mut self,
        sig: &MirFunctionSig,
        name: &'b str,
        visibility: ast::Visibility,
    ) {
        let ret_ty = self.from_mir_ty(sig.return_type.as_ref());
        let function_tys: Vec<types::BasicTypeEnum<'_>> = sig
            .pos_args
            .iter()
            .map(|arg| self.from_mir_ty(arg))
            .collect();

        let function = self
            .module
            .add_function(name, ret_ty.fn_type(&function_tys, false), None);
        function.set_linkage(visibility.get_linkage());
    }

    pub fn gen_var_prototype(&mut self, var_assign: &MirVariableAssign) {
        let ty = self.from_mir_ty(match &var_assign.value {
            Either::Left(expr) => &expr.ty,
            Either::Right(ty) => ty,
        });
        self.module.add_global(
            ty,
            Some(inkwell::AddressSpace::Const),
            &var_assign.var_name.to_string(),
        );
    }

    pub fn eval_expr(&mut self, expr: &MirExpr) -> inkwell::values::BasicValueEnum<'a> {
        todo!()
    }

    pub fn from_mir_ty(&mut self, mir_ty: &MirType) -> types::BasicTypeEnum<'a> {
        match mir_ty {
            MirType::Primitive(prim, _) => match prim {
                Prim::I8 => self.context.i8_type().into(),
                Prim::I16 => self.context.i16_type().into(),
                Prim::I32 => self.context.i32_type().into(),
                Prim::I64 => self.context.i64_type().into(),
                Prim::I128 => self.context.i128_type().into(),
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
