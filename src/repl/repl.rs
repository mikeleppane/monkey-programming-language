use crate::lexer::lexer::*;
use crate::token::token::TokenType::Empty;
use std::io;
use std::io::prelude::*;

const PROMPT: &str = ">> ";
const EXIT: &str = "exit";

pub fn start() {
    let stdin = io::stdin();
    print!("{}", PROMPT);
    io::stdout().flush().unwrap();
    for line_result in stdin.lock().lines() {
        let line = line_result.unwrap().trim().to_string();
        if line == EXIT {
            return;
        }
        let mut lexer = Lexer::new(&line);
        loop {
            let tok = lexer.next_token();
            if tok.r#type == Empty {
                break;
            }
            println!("{:#?}", tok);
        }
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
    }
}
