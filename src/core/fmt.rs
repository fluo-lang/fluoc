// Some of the std that cannot be written in fluo, and are more convenient to write in llvm directly
use crate::codegen::module_codegen;
use inkwell::AddressSpace;

impl<'a> module_codegen::CodeGenModule<'a> {
    pub fn generate_std(&mut self) {
        self.generate_printf();
        self.generate_print_i32();
    }

    fn generate_printf(&mut self) {
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();

        let first_param_type = i8_type.ptr_type(AddressSpace::Generic);
        let fn_type = i32_type.fn_type(&[first_param_type.into()], true);

        self.module.add_function("printf", fn_type, None);
    }

    fn generate_print_i32(&mut self) {
        let i32_type = self.context.i32_type();
        let empty_tuple = self.context.struct_type(&[], false);

        let fn_type = empty_tuple.fn_type(&[i32_type.into()], false);
        let fn_addr = self.module.add_function("print_i32", fn_type, None);

        let entry_block = self.context.append_basic_block(fn_addr, "entry");

        self.builder.position_at_end(entry_block);

        let format = self.builder.build_global_string_ptr("%i\n", "format");

        self.builder.build_call(
            self.module
                .get_function("printf")
                .expect("There is no printf defined??"),
            &[
                inkwell::values::BasicValueEnum::PointerValue(format.as_pointer_value()),
                fn_addr.get_nth_param(0).unwrap().into_int_value().into(),
            ],
            "temp2",
        );

        self.builder
            .build_return(Some(&inkwell::values::BasicValueEnum::StructValue(
                empty_tuple.const_named_struct(&[]),
            )));
    }
}
