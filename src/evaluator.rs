use crate::ast::{
    Boolean, Expression, ExpressionStatement, IntegerLiteral, PrefixExpression, Program, Statement,
};
use crate::object::{Boolean as BooleanObject, Integer, Null, Object};

const TRUE: BooleanObject = BooleanObject { value: true };
const FALSE: BooleanObject = BooleanObject { value: false };
const NULL: Null = Null {};

pub fn eval_program(program: &Program) -> Option<Box<dyn Object>> {
    let mut result = None;
    for statement in &program.statements {
        result = Some(eval(statement.as_ref()));
    }
    result
}

pub fn eval(node: &dyn Statement) -> Box<dyn Object> {
    match node.as_any().downcast_ref::<ExpressionStatement>() {
        Some(expr) => {
            let expr = expr.expression.as_ref().unwrap().as_ref();
            eval_expression(expr)
        }
        None => unimplemented!(),
    }
}

/* pub fn eval_statement() -> Box<dyn Object> {
    unimplemented!()
} */

pub fn eval_expression(node: &dyn Expression) -> Box<dyn Object> {
    if let Some(expr) = node.as_any().downcast_ref::<IntegerLiteral>() {
        return Box::new(Integer { value: expr.value });
    }
    if let Some(expr) = node.as_any().downcast_ref::<Boolean>() {
        if expr.value {
            return Box::new(TRUE);
        }
        return Box::new(FALSE);
    }
    if let Some(expr) = node.as_any().downcast_ref::<PrefixExpression>() {
        let right = eval_expression(expr.right.as_ref().unwrap().as_ref());
        return eval_prefix_expression(&expr.operator, right);
    }

    unreachable!("unimplemented Expression: {:?}", node.to_string())
}

fn eval_prefix_expression(operator: &str, right: Box<dyn Object>) -> Box<dyn Object> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Box::new(NULL),
    }
}

fn eval_bang_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    if let Some(boolean) = right.as_any().downcast_ref::<BooleanObject>() {
        if boolean.value {
            return Box::new(FALSE);
        }
        return Box::new(TRUE);
    }
    if let Some(integer) = right.as_any().downcast_ref::<Integer>() {
        if integer.value == 0 {
            return Box::new(TRUE);
        }
        return Box::new(FALSE);
    }
    Box::new(FALSE)
}

fn eval_minus_prefix_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    match right.as_any().downcast_ref::<Integer>() {
        Some(integer) => Box::new(Integer {
            value: -integer.value,
        }),
        None => Box::new(NULL),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn test_eval(input: &str) -> Option<Box<dyn Object>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval_program(&program)
    }

    fn check_integer_object(obj: Box<dyn Object>, expected: i64) {
        match obj.as_any().downcast_ref::<Integer>() {
            Some(integer) => assert_eq!(integer.value, expected),
            None => panic!("object is not Integer. got={}", obj.type_name().to_string()),
        }
    }

    fn check_boolean_object(obj: Box<dyn Object>, expected: bool) {
        match obj.as_any().downcast_ref::<BooleanObject>() {
            Some(boolean) => assert_eq!(boolean.value, expected),
            None => panic!("object is not Boolean. got={}", obj.type_name().to_string()),
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10), ("-5", -5), ("-10", -10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Some(evaluated) = evaluated {
                check_integer_object(evaluated, expected)
            } else {
                panic!("evaluated is None")
            }
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true", true), ("false", false)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Some(evaluated) = evaluated {
                check_boolean_object(evaluated, expected)
            } else {
                panic!("evaluated is None")
            }
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Some(evaluated) = evaluated {
                check_boolean_object(evaluated, expected)
            } else {
                panic!("evaluated is None")
            }
        }
    }
}
