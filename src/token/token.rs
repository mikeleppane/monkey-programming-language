#![allow(dead_code)]
#![allow(non_camel_case_types)]

use std::string::ToString;
#[derive(PartialEq, Eq, Debug)]
pub struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) literal: String,
}

impl Token {
    pub(crate) fn new() -> Self {
        Token {
            r#type: TokenType::EMPTY,
            literal: EMPTY.to_string(),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum TokenType {
    ASSIGN,
    SEMICOLON,
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    PLUS,
    COMMA,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    EMPTY,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    EQ,
    NOT_EQ,
}

pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::FUNCTION,
        "let" => TokenType::LET,
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "return" => TokenType::RETURN,
        _ => TokenType::IDENT,
    }
}

#[allow(dead_code)]
pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";
pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";
pub const EMPTY: &str = "";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERISK: &str = "*";
pub const SLASH: &str = "/";
pub const LT: &str = "<";
pub const GT: &str = ">";
pub const EQ: &str = "==";
pub const NOT_EQ: &str = "!=";
