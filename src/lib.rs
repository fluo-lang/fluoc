#![feature(bindings_after_at)]
// For benchmarks
#[macro_use]
extern crate lazy_static;

#[macro_use]
pub mod sourcemap;

pub mod codegen;
pub mod helpers;
pub mod lexer;
pub mod logger;
//pub mod mangle;
pub mod mir;
pub mod parser;
pub mod paths;
pub mod segmentation;
pub mod tags;
pub mod typecheck;
