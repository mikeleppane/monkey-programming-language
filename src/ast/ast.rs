use crate::token::token::*;

trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement {
    fn token_literal(&self) -> String;
    fn identifier(&self) -> Option<Identifier>;
    fn statement_node(&self);
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

    fn identifier(&self) -> Option<Identifier> {
        if !self.statements.is_empty() {
            self.statements[0].identifier().clone()
        } else {
            None
        }
    }

    fn statement_node(&self) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
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
    fn identifier(&self) -> Option<Identifier> {
        Some(self.name.clone())
    }
    fn statement_node(&self) {}
}
