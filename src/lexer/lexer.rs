use crate::token::token::*;

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: String,
}

fn is_letter(ch: &str) -> bool {
    ("a"..="z").contains(&ch) || ("A"..="Z").contains(&ch) || ch == "_"
}

fn is_digit(ch: &str) -> bool {
    ("0".."9").contains(&ch)
}

impl Lexer {
    fn new(input: &str) -> Self {
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

    fn next_token(&mut self) -> Token {
        let mut tok = Token::new();
        self.skip_whitespace();
        match self.ch.as_str() {
            ASSIGN => {
                tok = Token {
                    r#type: TokenType::ASSIGN,
                    literal: ASSIGN.to_string(),
                }
            }
            PLUS => {
                tok = Token {
                    r#type: TokenType::PLUS,
                    literal: PLUS.to_string(),
                }
            }
            MINUS => {
                tok = Token {
                    r#type: TokenType::MINUS,
                    literal: MINUS.to_string(),
                }
            }
            BANG => {
                tok = Token {
                    r#type: TokenType::BANG,
                    literal: BANG.to_string(),
                }
            }
            SLASH => {
                tok = Token {
                    r#type: TokenType::SLASH,
                    literal: SLASH.to_string(),
                }
            }
            ASTERISK => {
                tok = Token {
                    r#type: TokenType::ASTERISK,
                    literal: ASTERISK.to_string(),
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
                    r#type: TokenType::SEMICOLON,
                    literal: SEMICOLON.to_string(),
                }
            }
            LPAREN => {
                tok = Token {
                    r#type: TokenType::LPAREN,
                    literal: LPAREN.to_string(),
                }
            }
            RPAREN => {
                tok = Token {
                    r#type: TokenType::RPAREN,
                    literal: RPAREN.to_string(),
                }
            }
            COMMA => {
                tok = Token {
                    r#type: TokenType::COMMA,
                    literal: COMMA.to_string(),
                }
            }
            LBRACE => {
                tok = Token {
                    r#type: TokenType::LBRACE,
                    literal: LBRACE.to_string(),
                }
            }
            RBRACE => {
                tok = Token {
                    r#type: TokenType::RBRACE,
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
                    tok.r#type = TokenType::INT;
                    tok.literal = self.read_number();
                    return tok;
                } else {
                    tok = Token {
                        r#type: TokenType::ILLEGAL,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::assert_eq;

    #[test]
    fn test_next_token_with_basic_token_types() {
        let test_input = "=+(){},;-/*<>";
        let inputs = vec![
            (TokenType::ASSIGN, "="),
            (TokenType::PLUS, "+"),
            (TokenType::LPAREN, "("),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RBRACE, "}"),
            (TokenType::COMMA, ","),
            (TokenType::SEMICOLON, ";"),
            (TokenType::MINUS, "-"),
            (TokenType::SLASH, "/"),
            (TokenType::ASTERISK, "*"),
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
        ";
        let inputs = vec![
            (TokenType::LET, "let"),
            (TokenType::IDENT, "five"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "ten"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "add"),
            (TokenType::ASSIGN, "="),
            (TokenType::FUNCTION, "fn"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "x"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "y"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::IDENT, "x"),
            (TokenType::PLUS, "+"),
            (TokenType::IDENT, "y"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "result"),
            (TokenType::ASSIGN, "="),
            (TokenType::IDENT, "add"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "five"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "ten"),
            (TokenType::RPAREN, ")"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::BANG, "!"),
            (TokenType::MINUS, "-"),
            (TokenType::SLASH, "/"),
            (TokenType::ASTERISK, "*"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::GT, ">"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::IF, "if"),
            (TokenType::LPAREN, "("),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::TRUE, "true"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::ELSE, "else"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::FALSE, "false"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::EMPTY, ""),
        ];
        let mut lexer = Lexer::new(test_input);
        for (token_type, literal) in inputs.iter() {
            let tok = lexer.next_token();
            assert_eq!(tok.r#type, *token_type);
            assert_eq!(tok.literal, *literal);
        }
    }
}
