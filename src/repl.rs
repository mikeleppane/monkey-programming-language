use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::prelude::*;
use std::{env, io};

const PROMPT: &str = ">> ";
const EXIT: &str = "exit";
const HELP: &str = "help";

fn print_welcome() {
    println!(
        "Hello {}! This is the Monkey programming language!",
        env::var("USER").expect("'USER' environment variable not defined!")
    );
    println!("Feel free to type in commands:");
}

fn print_goodbye() {
    println!(
        "Goodbye {}!",
        env::var("USER").expect("'USER' environment variable not defined!")
    );
}

fn print_help() {
    println!("Commands:");
    println!("\texit: exit the REPL");
    println!("\thelp: print this help message");
}

fn print_prompt() {
    print!("{}", PROMPT);
    io::stdout().flush().unwrap();
}

pub fn start() {
    let stdin = io::stdin();
    print_welcome();
    print_prompt();
    for line_result in stdin.lock().lines() {
        if line_result.is_err() {
            println!("🐒");
            println!("Woops! We ran into some monkey business here!");
            println!("\t{}", line_result.unwrap_err());
            print_prompt();
            continue;
        }
        let line = line_result.unwrap();
        let line_trimmed = line.trim();
        if line_trimmed == EXIT {
            print_goodbye();
            return;
        }
        if line_trimmed == HELP {
            print_help();
            print_prompt();
            continue;
        }
        let lexer = Lexer::new(line_trimmed);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            print_parser_errors(parser.errors());
            print_prompt();
            continue;
        }
        let evaluated = eval_program(&program);
        if let Some(evaluated) = evaluated {
            println!("🐵");
            println!("{}", evaluated.inspect());
        }
        print_prompt();
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    println!("🐒");
    println!("Woops! We ran into some monkey business here!");
    println!(" parser errors:");
    for error in errors {
        println!("\t{}", error);
    }
}
