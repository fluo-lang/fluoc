#![warn(rust_2018_idioms)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate derivative;

#[macro_use]
pub mod sourcemap;

pub mod codegen;
pub mod helpers;
pub mod lexer;
pub mod logger;
// pub mod mangle;
pub mod context;
pub mod master;
pub mod mir;
pub mod opts;
pub mod parser;
pub mod paths;
pub mod segmentation;
pub mod tags;
pub mod typecheck;

use inkwell::context::Context;

use logger::{Color, Font};

use std::io::{self, Read};
use std::panic;
use std::path;
use std::time::Instant;

use clap::Clap;

fn main() {
    let master_start = Instant::now();
    panic::set_hook(Box::new(|value| {
        eprintln!(
            "{}.\n{}This is likely a problem with the fluo compiler and not your code. Please report the issue to the fluo github: https://github.com/fluo-lang/fluoc{}",
            value,
            Color::Red,
            Font::Reset
        );
    }));

    let opts = opts::Opts::parse();

    let context = Context::create();

    let read_file_start = Instant::now();
    let (source, filename) = match (&opts.code, &opts.entry) {
        (Some(ref code), _) => (code.clone(), path::PathBuf::from("<string>")),
        (_, Some(ref filename)) if filename == "-" => {
            let mut buffer = String::new();
            let mut stdin = io::stdin(); // We get `Stdin` here.
            stdin
                .read_to_string(&mut buffer)
                .expect("Failed to read from stdin");
            (buffer, path::PathBuf::from("<stdin>"))
        }
        (_, Some(ref f)) => {
            let filename = paths::process_str(f);
            (paths::read_file(filename.as_path()), filename)
        }
        _ => panic!(),
    };

    let mut master = master::Master::new(&context, opts);
    master.logger.borrow().log_verbose(&|| {
        format!(
            "{}: Read file",
            helpers::display_duration(read_file_start.elapsed())
        )
    }); // Lazily run it so no impact on performance

    master.generate_file(filename, source);

    master.logger.borrow().log(format!(
        "{}: All Done",
        helpers::display_duration(master_start.elapsed())
    ));
}
