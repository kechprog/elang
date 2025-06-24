use elang_interpreter::{ast, interpreter};

fn eval_source(source: &str) -> Result<ast::Value, String> {
    let mut interpreter = interpreter::Interpreter::new();
    let module = elang_parser::parse(source)?;
    if module.body.is_empty() {
        return Ok(ast::Value::Optional(None));
    }
    let mut final_val = ast::Value::Optional(None);
    for item in module.body {
        final_val = interpreter.eval(&item)?;
    }
    Ok(final_val)
}

#[test]
fn test_literal_number() {
    let result = eval_source("42");
    assert_eq!(result, Ok(ast::Value::Int(42)));
}

#[test]
fn test_simple_addition() {
    let result = eval_source("(+ 1 2)");
    assert_eq!(result, Ok(ast::Value::Int(3)));
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
    assert_eq!(result, Ok(ast::Value::Int(1)));
}

#[test]
fn test_if_false() {
    let result = eval_source("(if false 1 2)");
    assert_eq!(result, Ok(ast::Value::Int(2)));
}

#[test]
fn test_if_with_expression() {
    let result = eval_source("(if (= 2 (+ 1 1)) 10 20)");
    assert_eq!(result, Ok(ast::Value::Int(10)));
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
    assert_eq!(result, Ok(ast::Value::Int(10)));
}

#[test]
fn test_let_sequential() {
    let result = eval_source("(let* [(x 10) (y (+ x 5))] y)");
    assert_eq!(result, Ok(ast::Value::Int(15)));
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