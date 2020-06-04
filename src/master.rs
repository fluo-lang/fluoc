use crate::codegen::module_codegen::CodeGenModule;
use crate::helpers;
use crate::logger;
use crate::logger::buffer_writer::color;
use crate::paths;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path;
use std::process;
use std::process::Command;
use std::rc::Rc;
use std::time::Instant;

use inkwell::context::Context;
use inkwell::module;
use inkwell::passes::PassManager;
use inkwell::passes::PassManagerSubType;
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

        let pass_manager = self.init_passes();
        pass_manager.run_on(&code_gen_mod.module);
        self.modules.insert(&filename, code_gen_mod);

        self.write_obj_file(&filename);
        self.link_objs(&filename);
    }

    fn init_passes(&self) -> PassManager<module::Module<'a>> {
        let fpm: PassManager<module::Module<'_>> = PassManager::create(());

        fpm.add_instruction_combining_pass();
        fpm.add_tail_call_elimination_pass();
        fpm.add_reassociate_pass();
        fpm.add_constant_propagation_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_tail_call_elimination_pass();
        fpm.add_instruction_combining_pass();

        return fpm;
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
        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: Object file written",
                helpers::display_duration(write_obj_start.elapsed())
            )
        }); // Lazily run it so no impact on performance
    }

    fn link_objs(&self, filename: &'a path::Path) {
        // DYLD_LIBRARY_PATH="$(rustc --print sysroot)/lib:$DYLD_LIBRARY_PATH" ./a.out
        let link_start = Instant::now();
        let module = self.modules.get(filename).unwrap();
        let mut core_loc = helpers::get_core_loc();
        core_loc.pop();

        let paths = match fs::read_dir(&core_loc) {
            Ok(val) => val,
            Err(e) => paths::file_error(e, core_loc.display()),
        };

        let mut args = vec![
            paths::path_to_str(&module.output_file).to_string(),
            "-e".to_string(),
            "_N5entry_R2t0_A0".to_string(),
            "-no-pie".to_string(),
            "-Qunused-arguments".to_string(),
        ];

        args.append(
            &mut paths
                .into_iter()
                .filter(|val| {
                    val.is_ok()
                        && (val
                            .as_ref()
                            .unwrap()
                            .path()
                            .extension()
                            .unwrap_or(OsStr::new(""))
                            == OsStr::new("o"))
                })
                .map(|obj_path| paths::pathbuf_to_string(obj_path.unwrap().path()))
                .collect(),
        );

        match Command::new("gcc").args(&args[..]).spawn() {
            Ok(_) => {}
            Err(e) => {
                eprintln!("{}Error when linking:{} {}", color::RED, color::RESET, e);
                process::exit(1);
            }
        };

        self.logger.borrow().log_verbose(&|| {
            format!(
                "Linker command invoked: {}`gcc {}`",
                color::RESET,
                args.join(" ")
            )
        });

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: Objects linked",
                helpers::display_duration(link_start.elapsed())
            )
        });
    }
}
