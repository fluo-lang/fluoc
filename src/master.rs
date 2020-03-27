use inkwell::context::Context;
use crate::codegen::module_codegen::CodeGenModule;
use std::collections::HashMap;
use crate::logger::color;
use std::process;

pub struct Master<'a> {
    context: &'a Context,
    modules: HashMap<String, CodeGenModule<'a>>,
}

impl<'a> Master<'a> {
    pub fn new(context: &'a Context) -> Master<'a> {
        Master { context, modules: HashMap::new() }
    }

    pub fn add_file(&mut self, filename: &'a str) {

        let code_gen_mod = CodeGenModule::new(&filename, self.context.create_module(filename));

        match code_gen_mod {
            Ok(mut code_gen_safe) => { 
                code_gen_safe.generate();
                self.modules.insert(filename.to_string().clone(), code_gen_safe);
            },
            Err(e) => {
                println!("{}{}Fatal Error{}: {}: `{}`", color::RED, color::BOLD, color::RESET, e, filename);
                process::exit(1);
            }
        };
    }
}
