use crate::codegen::module_codegen::CodeGenModule;
use crate::logger;

use std::cell::RefCell;
use std::collections::HashMap;
use std::process;
use std::rc::Rc;

use inkwell::context::Context;

pub struct Master<'a> {
    context: &'a Context,
    logger: Rc<RefCell<logger::logger::Logger<'a>>>,
    modules: HashMap<&'a str, CodeGenModule<'a>>,
}

impl<'a> Master<'a> {
    pub fn new(context: &'a Context) -> Master<'a> {
        Master {
            context,
            modules: HashMap::new(),
            logger: Rc::new(RefCell::new(logger::logger::Logger::new())),
        }
    }

    pub fn add_file(&mut self, filename: &'a str, file_contents: &'a str) {
        let mut code_gen_mod = CodeGenModule::new(
            self.context.create_module(filename),
            self.context,
            filename,
            file_contents,
            Rc::clone(&self.logger),
        );

        self.logger
            .as_ref()
            .borrow_mut()
            .add_file(filename, code_gen_mod.typecheck.parser.lexer.file_contents);
        match code_gen_mod.generate() {
            Ok(_) => {}
            Err(errors) => {
                for error in errors {
                    self.logger.as_ref().borrow_mut().error(error);
                }
                self.logger.as_ref().borrow_mut().raise();
                process::exit(1);
            }
        };
        self.modules.insert(filename, code_gen_mod);
    }
}
