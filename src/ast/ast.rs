use crate::token::token::*;

trait Node {
    fn token_literal(&self) -> String;
}

pub trait Identifiable {
    fn value(&self) -> String;
    fn token(&self) -> Token;
}

pub trait Statement {
    fn token_literal(&self) -> String;
    fn identifier(&self) -> Option<&dyn Identifiable>;
    fn statement_node(&self);
    fn to_string(&self) -> String;
}

pub trait Expression {
    fn expression_node(&self);
    fn token_literal(&self) -> String;
}

pub struct Program {
    pub(crate) statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        todo!()
    }
}

impl Statement for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            String::from("")
        }
    }

    fn identifier(&self) -> Option<&dyn Identifiable> {
        if !self.statements.is_empty() {
            self.statements[0].identifier()
        } else {
            None
        }
    }

    fn statement_node(&self) {
        todo!()
    }

    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[allow(dead_code)]
impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
    pub fn value(&self) -> String {
        self.value.clone()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn identifier(&self) -> Option<&dyn Identifiable> {
        None
    }
    fn statement_node(&self) {}

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl Identifiable for Identifier {
    fn value(&self) -> String {
        self.value.clone()
    }

    fn token(&self) -> Token {
        self.token.clone()
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Statement>,
}

impl LetStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            name: Identifier::new(Token::new(), "".to_string()),
            value: Box::new(Identifier::new(Token::new(), "".to_string())),
        }
    }
}

impl Statement for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn identifier(&self) -> Option<&dyn Identifiable> {
        Some(&self.name)
    }
    fn statement_node(&self) {}

    fn to_string(&self) -> String {
        let mut string = "".to_string();
        string.push_str(self.token_literal().as_str());
        string.push(' ');
        string.push_str(self.name.to_string().as_str());
        string.push_str(" = ");
        string.push_str(self.value.to_string().as_str());
        string.push(';');
        string
    }
}

pub struct ReturnStatement {
    token: Token,
    return_value: Box<dyn Statement>,
}

impl ReturnStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            return_value: Box::new(Identifier::new(Token::new(), "".to_string())),
        }
    }
}

impl Statement for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn identifier(&self) -> Option<&dyn Identifiable> {
        None
    }
    fn statement_node(&self) {}

    fn to_string(&self) -> String {
        let mut string = "".to_string();
        string.push_str(self.token_literal().as_str());
        string.push(' ');
        string.push_str(self.return_value.to_string().as_str());
        string.push(';');
        string
    }
}

pub struct ExpressionStatement {
    token: Token,
    expression: Box<dyn Statement>,
}

#[allow(dead_code)]
impl ExpressionStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            expression: Box::new(Identifier::new(Token::new(), "".to_string())),
        }
    }
}

impl Statement for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn identifier(&self) -> Option<&dyn Identifiable> {
        self.expression.identifier()
    }
    fn statement_node(&self) {}

    fn to_string(&self) -> String {
        self.expression.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::token::TokenType::{Ident, Let};
    use std::assert_eq;

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token {
                    r#type: Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        r#type: Ident,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Box::new(Identifier {
                    token: Token {
                        r#type: Ident,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                }),
            })],
        };
        assert_eq!(program.to_string().as_str(), "let myVar = anotherVar;")
    }
}
