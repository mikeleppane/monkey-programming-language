extern crate core;

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod tokens;

use repl::*;

fn main() {
    start();
}
