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
use inkwell::targets::TargetMachine;

use clap::ArgMatches;
use rand::distributions::Alphanumeric;
use rand::Rng;

pub struct Master<'a> {
    context: &'a Context,
    pub logger: Logger,
    modules: HashMap<usize, CodeGenModule<'a>>,
    sourcemap: SourceMap,
    args: ArgMatches<'a>,
}

impl<'a> Master<'a> {
    pub fn new(context: &'a Context, args: ArgMatches<'a>) -> Master<'a> {
        let sourcemap = SourceMapInner::new();
        Master {
            context,
            modules: HashMap::new(),
            logger: LoggerInner::new(args.is_present("verbose"), Rc::clone(&sourcemap)),
            sourcemap,
            args,
        }
    }

    pub fn generate_file(
        &mut self,
        filename: path::PathBuf,
        contents: String,
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
        );

        helpers::error_or_other(code_gen_mod.generate(), Rc::clone(&self.logger));

        self.modules.insert(filename_id, code_gen_mod);

        self.link_ir(filename_id);

        let filename = format!(
            "__temp__fl_obj_{}.o",
            rand::thread_rng()
                .sample_iter(Alphanumeric)
                .take(10)
                .collect::<String>()
        );

        self.link_to_obj(
            &filename,
            self.modules[&filename_id].module.print_to_string(),
        );
        self.link_objs(&filename);

        if self.args.occurrences_of("object-file") != 0 {
            let _ = fs::rename(&filename, self.args.value_of("object-file").unwrap());
        } else {
            let _ = fs::remove_file(&filename);
        }
        // Just a reminder not to try and use it beyond this point
        // (deleted!)
        std::mem::drop(filename);
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

    fn link_to_obj(&self, filename: &str, pipe: inkwell::support::LLVMString) {
        let ir_to_asm = Instant::now();

        match Command::new("llc")
            .args(&["-filetype", "obj", "-O3", "-o", filename])
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

    fn link_objs(&self, filename: &str) {
        let link_start = Instant::now();
        let args = [filename, "-o", "a.out"];
        // TODO: package our own libstd instead of using glibc
        match Command::new("clang").args(&args).spawn() {
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
                "{}: Objects linked with `clang {}`",
                helpers::display_duration(link_start.elapsed()),
                args.join(" ")
            )
        });
    }
}
