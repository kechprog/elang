use elang_interpreter::{ast, interpreter, parser};

fn eval_source(source: &str) -> Result<ast::Value, String> {
    let ast = parser::parse(source)?;
    let interpreter = interpreter::Interpreter::new();
    // Assuming we are testing single expressions for now
    interpreter.eval(&ast[0])
}

#[test]
fn test_literal_number() {
    let result = eval_source("42");
    assert_eq!(result, Ok(ast::Value::Number(42.0)));
}

#[test]
fn test_simple_addition() {
    let result = eval_source("(+ 1 2)");
    assert_eq!(result, Ok(ast::Value::Number(3.0)));
}

#[test]
fn test_unbound_variable() {
    let result = eval_source("x");
    assert!(result.is_err());
    if let Err(e) = result {
        assert_eq!(e, "Unbound variable: x");
    }
}

#[test]
fn test_if_true() {
    let result = eval_source("(if true 1 2)");
    assert_eq!(result, Ok(ast::Value::Number(1.0)));
}

#[test]
fn test_if_false() {
    let result = eval_source("(if false 1 2)");
    assert_eq!(result, Ok(ast::Value::Number(2.0)));
}

#[test]
fn test_if_with_expression() {
    let result = eval_source("(if (= 2 (+ 1 1)) 10 20)");
    assert_eq!(result, Ok(ast::Value::Number(10.0)));
}

#[test]
fn test_if_type_error() {
    let result = eval_source("(if 1 10 20)");
    assert!(result.is_err());
    if let Err(e) = result {
        assert_eq!(e, "If condition must be a boolean");
    }
}

#[test]
fn test_let_binding() {
    let result = eval_source("(let [(x 10)] x)");
    assert_eq!(result, Ok(ast::Value::Number(10.0)));
}

#[test]
fn test_let_sequential() {
    let result = eval_source("(let* [(x 10) (y (+ x 5))] y)");
    assert_eq!(result, Ok(ast::Value::Number(15.0)));
}

#[test]
fn test_let_parallel() {
    // y cannot access x in a parallel let
    let result = eval_source("(let [(x 10) (y (+ x 5))] y)");
    assert!(result.is_err());
}

#[test]
fn test_incorrect_type_in_addition() {
    let result = eval_source("(+ 1 true)");
    assert!(result.is_err());
}