use std::fs;

use crate::diagnostics::Sources;
use crate::syntax::{Lexer, Parser};

/// Run the main compilation pipleline, from arguments to ouputted binary
pub fn run() {
    let mut sources = Sources::new();
    let id = sources
        .add_source(fs::read_to_string("test.fl").expect("Something went wrong reading the file"));
    let lexer = Lexer::new(&sources, id);
    let mut parser = Parser::new(lexer);

    println!("{:?}", parser.parse());
}
