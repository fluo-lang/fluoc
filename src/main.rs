pub mod helpers;
pub mod lexer;
pub mod logger;
pub mod module;

use termion::{color, style};
use std::process;

#[macro_use]
extern crate clap;
use clap::App;

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let filename = "./examples/simple_tests.fluo";
    let module = module::Module::new(filename);
    if let Err(e) = module {
        println!("{}{}Fatal Error{}: {}: `{}`", color::Fg(color::Rgb(255, 117, 117)), style::Bold, style::Reset, e, filename);
        process::exit(1);
    } else {
        println!("{:?}", module.unwrap().lexer);
    }
}
