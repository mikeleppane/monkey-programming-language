use crate::token::token::*;
use std::any::Any;
use std::fmt;

#[derive(PartialEq, Eq, Debug)]
pub enum AstType {
    Let,
    Return,
    ExprStatement,
    Int,
    Prefix,
    Infix,
    Identifier,
}

pub trait Statement {
    fn token_literal(&self) -> &str;
    fn statement_node(&self);
    fn to_string(&self) -> String;
    fn get_type(&self) -> AstType;
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression {
    fn expression_node(&self);
    fn token_literal(&self) -> &str;
    fn get_type(&self) -> AstType;
    fn to_string(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

#[allow(dead_code)]
impl Program {
    fn token_literal(&self) -> &str {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|s| { s.to_string() })
                .collect::<Vec<String>>()
                .join("")
                .trim()
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[allow(dead_code)]
impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}

    fn token_literal(&self) -> &str {
        self.token.literal()
    }
    fn get_type(&self) -> AstType {
        AstType::Identifier
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct LetStatement {
    token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl LetStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            name: Identifier::new(
                Token::Empty("".to_string()),
                "".to_string(), /* std::string::String */
            ),
            value: Box::new(Identifier::new(
                Token::Empty("".to_string()),
                "".to_string(), /* std::string::String */
            )),
        }
    }
}

impl Statement for LetStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
    fn statement_node(&self) {}

    fn to_string(&self) -> String {
        let mut string = "".to_string();
        string.push_str(self.token_literal());
        string.push(' ');
        string.push_str(self.name.to_string().as_str());
        string.push_str(" = ");
        string.push_str(self.value.to_string().as_str());
        string.push(';');
        string
    }
    fn get_type(&self) -> AstType {
        AstType::Let
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct ReturnStatement {
    token: Token,
    return_value: Box<dyn Expression>,
}

impl ReturnStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            return_value: Box::new(Identifier::new(
                Token::Empty("".to_string()),
                "".to_string(),
            )),
        }
    }
}

impl Statement for ReturnStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
    fn statement_node(&self) {}
    fn to_string(&self) -> String {
        let mut string = "".to_string();
        string.push_str(self.token_literal());
        string.push(' ');
        string.push_str(self.return_value.to_string().as_str());
        string.push(';');
        string
    }
    fn get_type(&self) -> AstType {
        AstType::Return
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct ExpressionStatement {
    token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

#[allow(dead_code)]
impl ExpressionStatement {
    pub fn new(token: Token, expression: Option<Box<dyn Expression>>) -> Self {
        Self { token, expression }
    }
}

impl Statement for ExpressionStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn statement_node(&self) {
        todo!()
    }
    fn to_string(&self) -> String {
        let mut string = "".to_string();
        match self.expression.as_ref() {
            None => string,
            Some(expr) => {
                string.push_str(expr.to_string().as_str());
                string
            }
        }
    }

    fn get_type(&self) -> AstType {
        AstType::ExprStatement
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        Self { token, value }
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {
        todo!()
    }

    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_type(&self) -> AstType {
        AstType::Int
    }

    fn to_string(&self) -> String {
        self.token.literal().to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct PrefixExpression {
    token: Token,
    pub operator: String,
    pub(crate) right: Option<Box<dyn Expression>>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: &str, right: Option<Box<dyn Expression>>) -> Self {
        Self {
            token,
            operator: operator.to_string(),
            right,
        }
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {
        todo!()
    }

    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_type(&self) -> AstType {
        AstType::Prefix
    }

    fn to_string(&self) -> String {
        format!(
            "{}{}{}{}",
            "(",
            self.operator.as_str(),
            self.right.as_ref().unwrap().to_string(),
            ")"
        )
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct InfixExpression {
    token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl InfixExpression {
    pub fn new(
        token: Token,
        left: Box<dyn Expression>,
        operator: &str,
        right: Option<Box<dyn Expression>>,
    ) -> Self {
        Self {
            token,
            left,
            operator: operator.to_string(),
            right,
        }
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {
        todo!()
    }

    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_type(&self) -> AstType {
        AstType::Infix
    }

    fn to_string(&self) -> String {
        format!(
            "{}{} {} {}{}",
            "(",
            self.left.as_ref().to_string(),
            self.operator.as_str(),
            self.right.as_ref().unwrap().to_string(),
            ")"
        )
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::assert_eq;
    use std::str::FromStr;

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token::from_str("let").unwrap(),
                name: Identifier {
                    token: Token::Ident("myVar".to_string()),
                    value: "myVar".to_string(),
                },
                value: Box::new(Identifier {
                    token: Token::Ident("anotherVar".to_string()),
                    value: "anotherVar".to_string(),
                }),
            })],
        };
        assert_eq!(program.to_string().as_str(), "let myVar = anotherVar;")
    }
}
