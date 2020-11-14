use crate::codegen::CodeGenModule;
use crate::helpers;
use crate::logger::{Color, Font, Logger, LoggerInner};
use crate::paths;
use crate::sourcemap::{SourceMap, SourceMapInner};

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path;
use std::process;
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::time::Instant;

use inkwell::context::Context;
use inkwell::module;
use inkwell::passes::PassManager;
use inkwell::targets::TargetMachine;

pub struct Master<'a> {
    context: &'a Context,
    pub logger: Logger,
    modules: HashMap<usize, CodeGenModule<'a>>,
    sourcemap: SourceMap,
}

impl<'a> Master<'a> {
    pub fn new(context: &'a Context, verbose: bool) -> Master<'a> {
        let sourcemap = SourceMapInner::new();
        Master {
            context,
            modules: HashMap::new(),
            logger: LoggerInner::new(verbose, Rc::clone(&sourcemap)),
            sourcemap,
        }
    }

    pub fn generate_file(
        &mut self,
        filename: path::PathBuf,
        contents: String,
        output_file_ir: &'a path::Path,
        output_file_obj: &'a path::Path,
    ) {
        let module = self
            .context
            .create_module(filename.to_str().expect("Filename specified is not valid"));

        let default_triple = TargetMachine::get_default_triple();
        module.set_triple(&default_triple);

        let filename_id = insert_file!(self.sourcemap, filename, contents);

        let mut code_gen_mod = CodeGenModule::new(
            module,
            self.context,
            Rc::clone(&self.sourcemap),
            filename_id,
            Rc::clone(&self.logger),
            output_file_ir,
            output_file_obj,
        );

        helpers::error_or_other(code_gen_mod.generate(), Rc::clone(&self.logger));

        self.modules.insert(filename_id, code_gen_mod);

        self.link_ir(filename_id);

        let pass_manager = self.init_passes();
        pass_manager.run_on(&self.modules[&filename_id].module);
        println!("{}", &self.modules[&filename_id].module.print_to_string().to_string());

        self.link_to_obj(
            filename_id,
            self.modules[&filename_id].module.print_to_string(),
        );
        self.link_objs(filename_id);
    }

    fn init_passes(&self) -> PassManager<module::Module<'a>> {
        let fpm: PassManager<module::Module<'_>> = PassManager::create(());

        fpm.add_verifier_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_function_inlining_pass();
        fpm.add_reassociate_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_tail_call_elimination_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        return fpm;
    }

    fn link_ir(&self, filename_id: usize) {
        let write_obj_start = Instant::now();
        let module = self.modules.get(&filename_id).unwrap();

        let mut core_loc: path::PathBuf = helpers::CORE_LOC.to_owned();
        core_loc.pop();

        let paths = match fs::read_dir(&core_loc) {
            Ok(val) => val,
            Err(e) => paths::file_error(e, core_loc.display()),
        };

        for path in &mut paths
            .into_iter()
            .filter(|val| {
                val.is_ok()
                    && (val
                        .as_ref()
                        .unwrap()
                        .path()
                        .extension()
                        .unwrap_or(OsStr::new(""))
                        == OsStr::new("bc"))
            })
            .map(|obj_path| paths::pathbuf_to_string(obj_path.unwrap().path()))
        {
            let lib_module = inkwell::module::Module::parse_bitcode_from_path(path, self.context)
                .expect("Library IR failed to convert");
            module
                .module
                .link_in_module(lib_module)
                .expect("Failed to link llvm ir");
        }

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: IR linked",
                helpers::display_duration(write_obj_start.elapsed())
            )
        });
    }

    fn link_to_obj(&self, filename_id: usize, pipe: inkwell::support::LLVMString) {
        let ir_to_asm = Instant::now();
        let module = self.modules.get(&filename_id).unwrap();

        match Command::new("llc")
            .args(&[
                "-filetype".to_string(),
                "obj".to_string(),
                "-O3".to_string(),
                "-o".to_string(),
                paths::path_to_str(&module.output_obj).to_string(),
            ])
            .stdin(Stdio::piped())
            .spawn()
        {
            Ok(mut child) => {
                child
                    .stdin
                    .as_mut()
                    .expect("Failed to get std from llvm-link")
                    .write_all(pipe.as_ref().to_bytes())
                    .expect("Failed to write to stdin");
                match child.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!(
                            "{}Error when turning LLVM into asm:{} {}",
                            Color::Red,
                            Font::Reset,
                            e
                        );
                        process::exit(1);
                    }
                }
            }
            Err(e) => {
                eprintln!(
                    "{}Error when turning LLVM into asm:{} {}",
                    Color::Red,
                    Font::Reset,
                    e
                );
                process::exit(1);
            }
        };

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: IR file turned into asm",
                helpers::display_duration(ir_to_asm.elapsed())
            )
        });
    }

    fn link_objs(&self, filename_id: usize) {
        let link_start = Instant::now();
        let module = self.modules.get(&filename_id).unwrap();

        let args = vec![
            paths::path_to_str(&module.output_obj).to_string(),
            "-no-pie".to_string(),
            "-g".to_string(),
        ];

        match Command::new("gcc").args(&args[..]).spawn() {
            Ok(mut child) => match child.wait() {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}Error when linking:{} {}", Color::Red, Font::Reset, e);
                    process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("{}Error when linking:{} {}", Color::Red, Font::Reset, e);
                process::exit(1);
            }
        };

        self.logger.borrow().log_verbose(&|| {
            format!(
                "Linker command invoked: {}`gcc {}`",
                Font::Reset,
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
