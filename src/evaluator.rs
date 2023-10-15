use std::rc::Rc;

use crate::ast::{
    BlockStatement, Boolean, Expression, ExpressionStatement, IfExpression, InfixExpression,
    IntegerLiteral, PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::object::{
    Boolean as BooleanObject, Error, Integer, Null, Object, ObjectType, ReturnValue,
};

const TRUE: BooleanObject = BooleanObject { value: true };
const FALSE: BooleanObject = BooleanObject { value: false };
const NULL: Null = Null {};

pub fn eval_program(program: &Program) -> Option<Rc<dyn Object>> {
    eval_statements(&program.statements)
}

pub fn eval(node: &dyn Statement) -> Rc<dyn Object> {
    if let Some(expr) = node.as_any().downcast_ref::<ExpressionStatement>() {
        if let Some(expr) = expr.expression.as_ref() {
            return eval_expression(expr.as_ref());
        }
    }

    if let Some(stmt) = node.as_any().downcast_ref::<BlockStatement>() {
        let mut result = None;
        for item in &stmt.statements {
            let ret = eval(item.as_ref());
            if ret.as_ref().type_name() == ObjectType::ReturnValue
                || ret.as_ref().type_name() == ObjectType::Error
            {
                return ret;
            }
            result = Some(ret);
        }
        if let Some(ret) = result {
            return ret;
        }
        return Rc::new(Error::new("block statement is empty".to_string()));
    }

    if let Some(stmt) = node.as_any().downcast_ref::<ReturnStatement>() {
        if let Some(ret_value) = stmt.return_value.as_ref() {
            let value = eval_expression(ret_value.as_ref());
            if Error::is_error(value.as_ref()) {
                return value;
            }
            return Rc::new(ReturnValue { value });
        }
        return Rc::new(Error::new("return statement is empty".to_string()));
    }
    Rc::new(Error::new(format!(
        "unimplemented Statement: {:?}",
        node.to_string()
    )))
}

fn eval_statements(stmts: &Vec<Box<dyn Statement>>) -> Option<Rc<dyn Object>> {
    let mut result = None;
    for statement in stmts {
        let ret = eval(statement.as_ref());
        if let Some(ret_value) = ret.as_any().downcast_ref::<ReturnValue>() {
            return Some(ret_value.get_return_value());
        }
        if let Some(error) = ret.as_any().downcast_ref::<Error>() {
            return Some(error.into());
        }
        result = Some(ret);
    }
    result
}

pub fn eval_expression(node: &dyn Expression) -> Rc<dyn Object> {
    if let Some(expr) = node.as_any().downcast_ref::<IntegerLiteral>() {
        return Rc::new(Integer { value: expr.value });
    }
    if let Some(expr) = node.as_any().downcast_ref::<Boolean>() {
        if expr.value {
            return Rc::new(TRUE);
        }
        return Rc::new(FALSE);
    }
    if let Some(expr) = node.as_any().downcast_ref::<PrefixExpression>() {
        if let Some(right) = expr.right.as_ref() {
            let right = eval_expression(right.as_ref());
            if Error::is_error(right.as_ref()) {
                return right;
            }
            return eval_prefix_expression(&expr.operator, right);
        }
        return Rc::new(Error::new("right expression is empty".to_string()));
    }

    if let Some(expr) = node.as_any().downcast_ref::<InfixExpression>() {
        let right = if let Some(right) = expr.right.as_ref() {
            eval_expression(right.as_ref())
        } else {
            Rc::new(Error::new("right expression is empty".to_string()))
        };
        if Error::is_error(right.as_ref()) {
            return right;
        }

        let left = if let Some(left) = expr.left.as_ref() {
            if Error::is_error(right.as_ref()) {
                return right;
            }
            eval_expression(left.as_ref())
        } else {
            Rc::new(Error::new("left expression is empty".to_string()))
        };
        if Error::is_error(left.as_ref()) {
            return left;
        }
        return eval_infix_expression(&expr.operator, left, right);
    }

    if let Some(expr) = node.as_any().downcast_ref::<IfExpression>() {
        return eval_if_expression(expr);
    }

    unreachable!("unimplemented Expression: {:?}", node.to_string())
}

fn eval_if_expression(ie: &IfExpression) -> Rc<dyn Object> {
    let condition = if let Some(condition) = ie.condition.as_ref() {
        eval_expression(condition.as_ref())
    } else {
        Rc::new(NULL)
    };
    if Error::is_error(condition.as_ref()) {
        return condition;
    }
    if is_truthy(condition) {
        let consequence = if let Some(consequence) = ie.consequence.as_ref() {
            consequence
        } else {
            return Rc::new(NULL);
        };
        return eval(consequence);
    } else if let Some(alt) = ie.alternative.as_ref() {
        return eval(alt);
    }
    Rc::new(NULL)
}

fn is_truthy(obj: Rc<dyn Object>) -> bool {
    if let Some(boolean) = obj.as_any().downcast_ref::<BooleanObject>() {
        return boolean.value;
    }
    if let Some(integer) = obj.as_any().downcast_ref::<Integer>() {
        return integer.value != 0;
    }
    false
}

fn eval_infix_expression(
    operator: &str,
    left: Rc<dyn Object>,
    right: Rc<dyn Object>,
) -> Rc<dyn Object> {
    if let Some(left) = left.as_any().downcast_ref::<Integer>() {
        if let Some(right) = right.as_any().downcast_ref::<Integer>() {
            return eval_integer_infix_expression(operator, left, right);
        }
    }
    if let Some(left) = left.as_any().downcast_ref::<BooleanObject>() {
        if let Some(right) = right.as_any().downcast_ref::<BooleanObject>() {
            if operator == "==" {
                return Rc::new(native_bool_to_boolean_object(left == right));
            }
            if operator == "!=" {
                return Rc::new(native_bool_to_boolean_object(left != right));
            }
            return Rc::new(Error::new(format!(
                "unknown operator: {} {} {}",
                left.type_name().to_string(),
                operator,
                right.type_name().to_string()
            )));
        }
    }
    Rc::new(Error::new(format!(
        "type mismatch: {} {} {}",
        left.type_name().to_string(),
        operator,
        right.type_name().to_string()
    )))
}

fn native_bool_to_boolean_object(input: bool) -> impl Object {
    if input {
        return TRUE;
    }
    FALSE
}

fn eval_integer_infix_expression(
    operator: &str,
    left: &Integer,
    right: &Integer,
) -> Rc<dyn Object> {
    match operator {
        "+" => Rc::new(Integer {
            value: left.value + right.value,
        }),
        "-" => Rc::new(Integer {
            value: left.value - right.value,
        }),
        "*" => Rc::new(Integer {
            value: left.value * right.value,
        }),
        "/" => Rc::new(Integer {
            value: left.value / right.value,
        }),
        "<" => {
            if left.value < right.value {
                return Rc::new(TRUE);
            }
            Rc::new(FALSE)
        }
        ">" => {
            if left.value > right.value {
                return Rc::new(TRUE);
            }
            Rc::new(FALSE)
        }
        "==" => {
            if left.value == right.value {
                return Rc::new(TRUE);
            }
            Rc::new(FALSE)
        }
        "!=" => {
            if left.value != right.value {
                return Rc::new(TRUE);
            }
            Rc::new(FALSE)
        }
        _ => Rc::new(Error::new(format!(
            "unknown operator: {} {} {}",
            left.type_name().to_string(),
            operator,
            right.type_name().to_string()
        ))),
    }
}

fn eval_prefix_expression(operator: &str, right: Rc<dyn Object>) -> Rc<dyn Object> {
    match operator {
        "!" => Rc::new(eval_bang_operator_expression(right)),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Rc::new(Error::new(format!(
            "unknown operator: {}{}",
            operator,
            right.type_name().to_string()
        ))),
    }
}

fn eval_bang_operator_expression(right: Rc<dyn Object>) -> impl Object {
    if let Some(boolean) = right.as_any().downcast_ref::<BooleanObject>() {
        if boolean.value {
            return FALSE;
        }
        return TRUE;
    }
    if let Some(integer) = right.as_any().downcast_ref::<Integer>() {
        if integer.value == 0 {
            return TRUE;
        }
        return FALSE;
    }
    FALSE
}

fn eval_minus_prefix_operator_expression(right: Rc<dyn Object>) -> Rc<dyn Object> {
    match right.as_any().downcast_ref::<Integer>() {
        Some(integer) => Rc::new(Integer {
            value: -integer.value,
        }),
        None => Rc::new(Error::new(format!(
            "unknown operator: -{}",
            right.type_name().to_string()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn test_eval(input: &str) -> Option<Rc<dyn Object>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", program);

        eval_program(&program)
    }

    fn check_integer_object(obj: Rc<dyn Object>, expected: i64) {
        match obj.as_any().downcast_ref::<Integer>() {
            Some(integer) => assert_eq!(integer.value, expected),
            None => panic!("object is not Integer. got={}", obj.type_name().to_string()),
        }
    }

    fn check_boolean_object(obj: Rc<dyn Object>, expected: bool) {
        match obj.as_any().downcast_ref::<BooleanObject>() {
            Some(boolean) => assert_eq!(boolean.value, expected),
            None => panic!("object is not Boolean. got={}", obj.type_name().to_string()),
        }
    }

    fn check_null_object(obj: Rc<dyn Object>) {
        match obj.as_any().downcast_ref::<Null>() {
            Some(_) => (),
            None => panic!("object is not Null. got={}", obj.type_name().to_string()),
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
            ("500 * 500 - 100 * 2 + (3 + 4) * 4", 249828),
            ("0 + 0 + 0", 0),
            ("-1 + 1", 0),
            ("-1 - 1", -2),
            ("--1 - 1", 0),
            ("--2 + 2", 4),
        ];

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
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
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

    #[test]
    fn test_if_else_expressions_() {
        let tests = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (false) { 10 } else { 20 }", Some(20)),
            ("if (true) { 10 } else { 20 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (false) { 10 }", None),
            ("if (1 < 2) { 10 }", Some(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Some(evaluated) = evaluated {
                if let Some(expected) = expected {
                    check_integer_object(evaluated, expected)
                } else {
                    check_null_object(evaluated)
                }
            } else {
                panic!("evaluated is None")
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10;9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) {return 10;} return 1;}", 10),
            (
                "if (10 > 1) { if (10 > 1) { if (true) { return -1;} return 10;} return 1;}",
                -1,
            ),
        ];

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
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { if (10 > 1) {return true + false;} return 1;}",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Some(evaluated) = evaluated {
                match evaluated.as_any().downcast_ref::<Error>() {
                    Some(error) => assert_eq!(error.message, expected),
                    None => panic!(
                        "object is not Error. got={}",
                        evaluated.type_name().to_string()
                    ),
                }
            } else {
                panic!("evaluated is None")
            }
        }
    }
}
