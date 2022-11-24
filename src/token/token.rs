#![allow(dead_code)]
#![allow(non_camel_case_types)]

use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Assign(String),
    Semicolon(String),
    Illegal(String),
    Eof(String),
    Ident(String),
    Int(String),
    Plus(String),
    Comma(String),
    Lparen(String),
    Rparen(String),
    Lbrace(String),
    Rbrace(String),
    Function(String),
    Let(String),
    Empty(String),
    Minus(String),
    Bang(String),
    Asterisk(String),
    Slash(String),
    LT(String),
    GT(String),
    True(String),
    False(String),
    IF(String),
    Else(String),
    Return(String),
    EQ(String),
    NOT_EQ(String), //pub(crate) r#type: TokenType,
                    //pub(crate) literal: String,
}

impl FromStr for Token {
    type Err = String;

    fn from_str(input: &str) -> Result<Token, Self::Err> {
        match input {
            "=" => Ok(Token::Assign("=".to_string())),
            "let" => Ok(Token::Let("let".to_string())),
            "" => Ok(Token::Empty("".to_string())),
            ";" => Ok(Token::Semicolon(";".to_string())),
            _ => Err(format!(
                "Could not construct Token from the given string: {}",
                input
            )),
        }
    }
}

impl Token {
    pub(crate) fn lookup_ident(ident: &str) -> Token {
        match ident {
            "fn" => Token::Function("fn".to_string()),
            "let" => Token::Let("let".to_string()),
            "true" => Token::True("true".to_string()),
            "false" => Token::False("false".to_string()),
            "if" => Token::IF("if".to_string()),
            "else" => Token::Else("else".to_string()),
            "return" => Token::Return("return".to_string()),
            _ => Token::Ident(ident.to_string()),
        }
    }

    pub(crate) fn literal(&self) -> &str {
        match self {
            Token::Assign(s) => s,
            Token::Semicolon(s) => s,
            Token::Illegal(s) => s,
            Token::Eof(s) => s,
            Token::Ident(s) => s,
            Token::Int(s) => s,
            Token::Plus(s) => s,
            Token::Minus(s) => s,
            Token::Comma(s) => s,
            Token::Lparen(s) => s,
            Token::Rparen(s) => s,
            Token::Lbrace(s) => s,
            Token::Rbrace(s) => s,
            Token::Function(s) => s,
            Token::Let(s) => s,
            Token::Empty(s) => s,
            Token::True(s) => s,
            Token::False(s) => s,
            Token::Bang(s) => s,
            Token::Asterisk(s) => s,
            Token::Slash(s) => s,
            Token::LT(s) => s,
            Token::GT(s) => s,
            Token::EQ(s) => s,
            Token::NOT_EQ(s) => s,
            Token::IF(s) => s,
            Token::Else(s) => s,
            Token::Return(s) => s,
        }
    }

    pub(crate) fn matches(&self, token: &Token) -> bool {
        match self {
            Token::Ident(_) => {
                return match token {
                    Token::Ident(_) => true,
                    _ => false,
                }
            }

            _ => token == self,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum TokenType {
    Assign,
    Semicolon,
    Illegal,
    Eof,
    Ident,
    Int,
    Plus,
    Comma,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Function,
    Let,
    Empty,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    True,
    False,
    IF,
    Else,
    Return,
    EQ,
    NOT_EQ,
}

pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::IF,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Ident,
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
