use elang_interpreter::{ast::{self, Value}, interpreter};

fn eval_source(source: &str) -> Result<ast::Value, String> {
    let mut interpreter = interpreter::Interpreter::new();
    let module = elang_interpreter::parser::parse(source)?;
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
fn test_if_true_branch() {
    let result = eval_source("(if true 1 2)");
    assert_eq!(result, Ok(Value::Int(1)));
}

#[test]
fn test_if_false_branch() {
    let result = eval_source("(if false 1 2)");
    assert_eq!(result, Ok(Value::Int(2)));
}

#[test]
fn test_if_with_expression_condition() {
    let result = eval_source("(if (= (+ 1 1) 2) 10 20)");
    assert_eq!(result, Ok(Value::Int(10)));
}

#[test]
fn test_if_with_non_boolean_condition() {
    let result = eval_source("(if 1 10 20)");
    assert!(result.is_err(), "Expected a type error for non-boolean condition");
}

#[test]
fn test_if_let_some_branch() {
    let source = "
    (fun (get-val) :(Option int) (some 42))
    (if-let (x (get-val)) x 0)
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(42)));
}

#[test]
fn test_if_let_none_branch() {
    let source = "
    (fun (get-val) :(Option int) none)
    (if-let (x (get-val)) x 0)
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(0)));
}

#[test]
fn test_if_let_binding_is_scoped() {
    let source = "
    (fun (get-val) :(Option int) (some 10))
    (if-let (x (get-val)) x 0)
    x ;; x should be unbound here
    ";
    let result = eval_source(source);
    assert!(result.is_err(), "Expected unbound variable error for x");
}