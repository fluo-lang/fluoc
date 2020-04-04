use inkwell::context::Context;
use crate::codegen::module_codegen::CodeGenModule;
use std::collections::HashMap;
use crate::logger::buffer_writer::color;
use crate::logger;
use std::process;

pub struct Master<'a> {
    context: &'a Context,
    logger: logger::logger::Logger,
    modules: HashMap<String, CodeGenModule<'a>>,
}

impl<'a> Master<'a> {
    pub fn new(context: &'a Context) -> Master<'a> {
        let logger = logger::logger::Logger::new();
        Master { context, modules: HashMap::new(), logger }
    }

    pub fn add_file(&mut self, filename: String) {
        let code_gen_mod = CodeGenModule::new(self.context.create_module(&filename[..]), filename.clone());
        
        match code_gen_mod {
            Ok(mut code_gen_safe) => { 
                self.logger.add_file(filename.clone(), code_gen_safe.parser.lexer.file_contents.clone());
                let module = code_gen_safe.generate();
                match module {
                    Ok(_) => {},
                    Err(errors) => {
                        for error in errors {
                            self.logger.error(error);
                        }
                        self.logger.raise();
                        process::exit(1);
                    }
                };
                self.modules.insert(filename, code_gen_safe);
            },
            Err(e) => {
                println!("{}{}Fatal Error{}: {}: `{}`", color::RED, color::BOLD, color::RESET, e, filename);
                process::exit(1);
            }
        };
    }
}
