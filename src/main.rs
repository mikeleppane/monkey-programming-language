extern crate core;

mod ast;
mod lexer;
mod parser;
mod repl;
mod tokens;

use repl::*;

fn main() {
    start();
}
