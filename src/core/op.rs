// Some of the std that cannot be written in fluo, and are more convenient to write in llvm directly
use crate::codegen::module_codegen;

impl<'a> module_codegen::CodeGenModule<'a> {
    pub fn generate_op(&mut self) {
        self.generate_add_int();
        self.generate_mul_int();
        self.generate_sub_int();
    }

    fn generate_add_int(&mut self) {
        // print_i32 ( int ) -> ()
        let i32_type = self.context.i32_type();

        let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);
        let fn_addr = self.module.add_function("core::op::add_int", fn_type, None);

        let entry_block = self.context.append_basic_block(fn_addr, "entry");

        self.builder.position_at_end(entry_block);

        let add_result = self.builder.build_int_add(
            fn_addr.get_nth_param(0).unwrap().into_int_value().into(),
            fn_addr.get_nth_param(1).unwrap().into_int_value().into(),
            "int_addition_temp",
        );

        self.builder
            .build_return(Some(&inkwell::values::BasicValueEnum::IntValue(add_result)));
    }

    fn generate_mul_int(&mut self) {
        // print_i32 ( int ) -> ()
        let i32_type = self.context.i32_type();

        let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);
        let fn_addr = self.module.add_function("core::op::mul_int", fn_type, None);

        let entry_block = self.context.append_basic_block(fn_addr, "entry");

        self.builder.position_at_end(entry_block);

        let add_result = self.builder.build_int_mul(
            fn_addr.get_nth_param(0).unwrap().into_int_value().into(),
            fn_addr.get_nth_param(1).unwrap().into_int_value().into(),
            "int_multiplication_temp",
        );

        self.builder
            .build_return(Some(&inkwell::values::BasicValueEnum::IntValue(add_result)));
    }

    fn generate_sub_int(&mut self) {
        // print_i32 ( int ) -> ()
        let i32_type = self.context.i32_type();

        let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);
        let fn_addr = self.module.add_function("core::op::sub_int", fn_type, None);

        let entry_block = self.context.append_basic_block(fn_addr, "entry");

        self.builder.position_at_end(entry_block);

        let add_result = self.builder.build_int_sub(
            fn_addr.get_nth_param(0).unwrap().into_int_value().into(),
            fn_addr.get_nth_param(1).unwrap().into_int_value().into(),
            "int_subtraction_temp",
        );

        self.builder
            .build_return(Some(&inkwell::values::BasicValueEnum::IntValue(add_result)));
    }
}
