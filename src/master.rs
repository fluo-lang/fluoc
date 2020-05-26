use crate::codegen::module_codegen::CodeGenModule;
use crate::helpers;
use crate::logger;

use std::cell::RefCell;
use std::collections::HashMap;
use std::path;
use std::rc::Rc;
use std::time::Instant;

use inkwell::context::Context;
use inkwell::module;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};

pub struct Master<'a> {
    context: &'a Context,
    pub logger: Rc<RefCell<logger::logger::Logger<'a>>>,
    modules: HashMap<&'a path::Path, CodeGenModule<'a>>,
}

impl<'a> Master<'a> {
    pub fn new(context: &'a Context, verbose: bool) -> Master<'a> {
        Master {
            context,
            modules: HashMap::new(),
            logger: Rc::new(RefCell::new(logger::logger::Logger::new(verbose))),
        }
    }

    pub fn generate_file(
        &mut self,
        filename: &'a path::Path,
        file_contents: &'a str,
        output_file: &'a path::Path,
    ) {
        let module = self
            .context
            .create_module(filename.to_str().expect("Filename specified is not valid"));
        
        self.init_passes(&module);
        let mut code_gen_mod = helpers::error_or_other(
            CodeGenModule::new(
                module,
                self.context,
                &filename,
                file_contents,
                Rc::clone(&self.logger),
                output_file,
            ),
            Rc::clone(&self.logger),
        );

        self.logger
            .as_ref()
            .borrow_mut()
            .add_file(&filename, code_gen_mod.typecheck.parser.lexer.file_contents);

        helpers::error_or_other(code_gen_mod.generate(), Rc::clone(&self.logger));
        self.modules.insert(&filename, code_gen_mod);
        
        self.write_obj_file(&filename);
    }

    fn init_passes(&self, module: &module::Module) {
        let fpm = PassManager::create(module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_tail_call_elimination_pass();

        fpm.initialize();
    }

    fn write_obj_file(&self, filename: &'a path::Path) {
        let module = self.modules.get(filename).unwrap();

        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");
        let default_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&default_triple).expect("Failed to get target");

        let target_machine = target
            .create_target_machine(
                &default_triple,
                "x86-64",
                "",
                inkwell::OptimizationLevel::Aggressive,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();

        let write_obj_start = Instant::now();
        target_machine
            .write_to_file(
                &module.module,
                inkwell::targets::FileType::Object,
                &path::Path::new(module.output_file),
            )
            .expect("Error writing object");
        self.logger.borrow().log_verbose(&|| format!("Object file written in {}ms!", write_obj_start.elapsed().as_millis())); // Lazily run it so no impact on performance
    }
}
