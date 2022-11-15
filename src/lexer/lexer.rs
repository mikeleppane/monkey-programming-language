use crate::token::token::*;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: String,
}

fn is_letter(ch: &str) -> bool {
    ("a"..="z").contains(&ch) || ("A"..="Z").contains(&ch) || ch == "_"
}

fn is_digit(ch: &str) -> bool {
    ("0"..="9").contains(&ch)
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: String::new(),
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = String::new();
        } else {
            self.ch = self
                .input
                .chars()
                .nth(self.read_position)
                .unwrap_or_else(|| {
                    panic!("Cannot read character from position {}", self.read_position)
                })
                .to_string()
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let mut tok = Token::new();
        self.skip_whitespace();
        match self.ch.as_str() {
            ASSIGN => {
                if self.peek_char() == "=" {
                    self.read_char();
                    tok = Token {
                        r#type: TokenType::EQ,
                        literal: String::from("=="),
                    }
                } else {
                    tok = Token {
                        r#type: TokenType::Assign,
                        literal: ASSIGN.to_string(),
                    }
                }
            }
            PLUS => {
                tok = Token {
                    r#type: TokenType::Plus,
                    literal: PLUS.to_string(),
                }
            }
            MINUS => {
                tok = Token {
                    r#type: TokenType::Minus,
                    literal: MINUS.to_string(),
                }
            }
            SLASH => {
                tok = Token {
                    r#type: TokenType::Slash,
                    literal: SLASH.to_string(),
                }
            }
            ASTERISK => {
                tok = Token {
                    r#type: TokenType::Asterisk,
                    literal: ASTERISK.to_string(),
                }
            }
            BANG => {
                if self.peek_char() == "=" {
                    self.read_char();
                    tok = Token {
                        r#type: TokenType::NOT_EQ,
                        literal: String::from("!="),
                    }
                } else {
                    tok = Token {
                        r#type: TokenType::Bang,
                        literal: BANG.to_string(),
                    }
                }
            }
            LT => {
                tok = Token {
                    r#type: TokenType::LT,
                    literal: LT.to_string(),
                }
            }
            GT => {
                tok = Token {
                    r#type: TokenType::GT,
                    literal: GT.to_string(),
                }
            }
            SEMICOLON => {
                tok = Token {
                    r#type: TokenType::Semicolon,
                    literal: SEMICOLON.to_string(),
                }
            }
            LPAREN => {
                tok = Token {
                    r#type: TokenType::Lparen,
                    literal: LPAREN.to_string(),
                }
            }
            RPAREN => {
                tok = Token {
                    r#type: TokenType::Rparen,
                    literal: RPAREN.to_string(),
                }
            }
            COMMA => {
                tok = Token {
                    r#type: TokenType::Comma,
                    literal: COMMA.to_string(),
                }
            }
            LBRACE => {
                tok = Token {
                    r#type: TokenType::Lbrace,
                    literal: LBRACE.to_string(),
                }
            }
            RBRACE => {
                tok = Token {
                    r#type: TokenType::Rbrace,
                    literal: RBRACE.to_string(),
                }
            }
            EMPTY => {}
            _ => {
                if is_letter(self.ch.as_str()) {
                    tok.literal = self.read_identifier();
                    tok.r#type = lookup_ident(tok.literal.as_str());
                    return tok;
                } else if is_digit(self.ch.as_str()) {
                    tok.r#type = TokenType::Int;
                    tok.literal = self.read_number();
                    return tok;
                } else {
                    tok = Token {
                        r#type: TokenType::Illegal,
                        literal: self.ch.clone(),
                    }
                }
            }
        }
        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch.as_str()) {
            self.read_char();
        }
        self.input.as_str()[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        fn is_whitespace(ch: &str) -> bool {
            ch == " " || ch == "\t" || ch == "\n" || ch == "\r"
        }
        while is_whitespace(self.ch.as_str()) {
            self.read_char()
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch.as_str()) {
            self.read_char();
        }
        self.input.as_str()[position..self.position].to_string()
    }

    fn peek_char(&self) -> String {
        if self.read_position >= self.input.len() {
            String::from("")
        } else {
            self.input.as_str()[self.read_position..self.read_position + 1].to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::assert_eq;

    #[test]
    fn test_next_token_with_basic_token_types() {
        let test_input = "=+(){},;-/*<>";
        let inputs = vec![
            (TokenType::Assign, "="),
            (TokenType::Plus, "+"),
            (TokenType::Lparen, "("),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Rbrace, "}"),
            (TokenType::Comma, ","),
            (TokenType::Semicolon, ";"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::LT, "<"),
            (TokenType::GT, ">"),
        ];
        let mut lexer = Lexer::new(test_input);
        for (token_type, literal) in inputs.iter() {
            let tok = lexer.next_token();
            assert_eq!(tok.r#type, *token_type);
            assert_eq!(tok.literal, *literal);
        }
    }
    #[test]
    fn test_next_token_with_real_test_code() {
        let test_input = "let five = 5;\
        let ten = 10;\
        \
        let add = fn(x, y) {\
          x + y;\
        }; \
        \
        let result = add(five, ten);\
        !-/*5;\
        5 < 10 > 5; \
        \
        if (5 < 10) {\
            return true;\
        } else {\
            return false;\
        }\
        \
        10 == 10;\
        10 != 9;\
        ";
        let inputs = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::Rparen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::GT, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::IF, "if"),
            (TokenType::Lparen, "("),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::EQ, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NOT_EQ, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::Empty, ""),
        ];
        let mut lexer = Lexer::new(test_input);
        for (token_type, literal) in inputs.iter() {
            let tok = lexer.next_token();
            assert_eq!(tok.r#type, *token_type);
            assert_eq!(tok.literal, *literal);
        }
    }
}
