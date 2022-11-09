use crate::token::token::*;

trait Node {
    fn token_literal(&self) -> String;
}

trait Statement {
    fn token_literal(&self) -> String;
    fn statement_node(&self);
}

trait Expression {
    fn expression_node(&self);
    fn token_literal(&self) -> String;
}

struct Program<T: Statement> {
    statements: Vec<T>,
}

impl<T: Statement> Node for Program<T> {
    fn token_literal(&self) -> String {
        todo!()
    }
}

impl<T: Statement> Statement for Program<T> {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            String::from("")
        }
    }

    fn statement_node(&self) {
        todo!()
    }
}

struct Identifier {
    token: Token,
    value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {}
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

struct LetStatement {
    token: Token,
    name: Identifier,
    value: Box<dyn Expression>,
}

impl Statement for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn statement_node(&self) {}
}
