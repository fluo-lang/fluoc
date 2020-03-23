pub mod helpers;
pub mod lexer;
pub mod logger;
pub mod module;
pub mod parser;

use termion::{color, style};
use std::process;

#[macro_use]
extern crate clap;
use clap::App;

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let filename = matches.value_of("entry").unwrap();
    let module = module::Module::new(&filename);
    //let filename = "./examples/simple_tests.fluo";

    if let Err(e) = module {
        println!("{}{}Fatal Error{}: {}: `{}`", color::Fg(color::Rgb(255, 117, 117)), style::Bold, style::Reset, e, filename);
        process::exit(1);
    } else {
        let mut module = module.unwrap();
        module.parser.parse();
    }
}
