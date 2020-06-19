use crate::codegen::module_codegen::CodeGenModule;
use crate::helpers;
use crate::logger;
use crate::logger::buffer_writer::color;
use crate::paths;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path;
use std::process;
use std::process::{Command, Output, Stdio};
use std::rc::Rc;
use std::time::Instant;

use inkwell::context::Context;
use inkwell::module;
use inkwell::passes::PassManager;

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
        output_file_ir: &'a path::Path,
        output_file_obj: &'a path::Path,
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
                output_file_ir,
                output_file_obj,
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

        //println!("{}", code_gen_mod.module.print_to_string().to_string());
        self.modules.insert(&filename, code_gen_mod);

        self.link_to_obj(&filename, self.write_ir_file(&filename));
        self.link_objs(&filename);
    }

    fn init_passes(&self) -> PassManager<module::Module<'a>> {
        let fpm: PassManager<module::Module<'_>> = PassManager::create(());

        fpm.add_instruction_combining_pass();
        fpm.add_tail_call_elimination_pass();
        fpm.add_reassociate_pass();
        fpm.add_function_inlining_pass();
        fpm.add_constant_propagation_pass();
        fpm.add_gvn_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_tail_call_elimination_pass();
        fpm.add_instruction_combining_pass();

        return fpm;
    }

    fn write_ir_file(&self, filename: &'a path::Path) -> Output {
        let write_obj_start = Instant::now();
        let module = self.modules.get(filename).unwrap();

        module
            .module
            .print_to_file(module.output_ir)
            .expect("Failed to write llvm ir to file");

        let mut core_loc: path::PathBuf = helpers::CORE_LOC.to_owned();
        core_loc.pop();

        let paths = match fs::read_dir(&core_loc) {
            Ok(val) => val,
            Err(e) => paths::file_error(e, core_loc.display()),
        };

        let mut args = vec![paths::path_to_str(&module.output_ir).to_string()];

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
                            == OsStr::new("ll"))
                })
                .map(|obj_path| paths::pathbuf_to_string(obj_path.unwrap().path()))
                .collect(),
        );

        println!("{:?}", args);

        let cmd = match Command::new("llvm-link")
            .args(&args[..])
            .stdout(Stdio::piped())
            .spawn()
        {
            Ok(child) => child,
            Err(e) => {
                eprintln!(
                    "{}Error linking llvm ir:{} {}",
                    color::RED,
                    color::RESET,
                    e
                );
                process::exit(1);
            }
        };

        let output = cmd.wait_with_output().expect("Failed to read stdout");

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: IR linker command invoked: {}`llvm-link {}`",
                helpers::display_duration(write_obj_start.elapsed()),
                color::RESET,
                args.join(" ")
            )
        });

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: IR linked",
                helpers::display_duration(write_obj_start.elapsed())
            )
        });

        output
    }

    fn link_to_obj(&self, filename: &'a path::Path, pipe: Output) {
        let ir_to_asm = Instant::now();
        let module = self.modules.get(filename).unwrap();

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
                    .write_all(&pipe.stdout)
                    .expect("Failed to write to stdin");
                match child.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!(
                            "{}Error when turning LLVM into asm:{} {}",
                            color::RED,
                            color::RESET,
                            e
                        );
                        process::exit(1);
                    }
                }
            }
            Err(e) => {
                eprintln!(
                    "{}Error when turning LLVM into asm:{} {}",
                    color::RED,
                    color::RESET,
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

    fn link_objs(&self, filename: &'a path::Path) {
        // DYLD_LIBRARY_PATH="$(rustc --print sysroot)/lib:$DYLD_LIBRARY_PATH" ./a.out
        let link_start = Instant::now();
        let module = self.modules.get(filename).unwrap();

        let args = vec![
            paths::path_to_str(&module.output_obj).to_string(),
            "-e".to_string(),
            "_N5entry_A0_R2t0".to_string(),
            "-no-pie".to_string(),
            "-g".to_string(),
        ];

        match Command::new("gcc").args(&args[..]).spawn() {
            Ok(mut child) => match child.wait() {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}Error when linking:{} {}", color::RED, color::RESET, e);
                    process::exit(1);
                }
            },
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
