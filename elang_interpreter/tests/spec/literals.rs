use elang_interpreter::{ast::{self, Value}, interpreter};

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
fn test_integer_literal() {
    let result = eval_source("42");
    assert_eq!(result, Ok(Value::Int(42)));
}

#[test]
fn test_negative_integer_literal() {
    let result = eval_source("-10");
    assert_eq!(result, Ok(Value::Int(-10)));
}

#[test]
fn test_float_literal() {
    let result = eval_source("3.14");
    assert_eq!(result, Ok(Value::Float(3.14)));
}

#[test]
fn test_negative_float_literal() {
    let result = eval_source("-0.5");
    assert_eq!(result, Ok(Value::Float(-0.5)));
}

#[test]
fn test_boolean_true_literal() {
    let result = eval_source("true");
    assert_eq!(result, Ok(Value::Bool(true)));
}

#[test]
fn test_boolean_false_literal() {
    let result = eval_source("false");
    assert_eq!(result, Ok(Value::Bool(false)));
}

#[test]
fn test_char_literal() {
    let result = eval_source("(quote a)");
    assert_eq!(result, Ok(Value::Symbol("a".to_string())));
}

#[test]
fn test_newline_char_literal() {
    let result = eval_source("(quote #\\newline)");
    assert_eq!(result, Ok(Value::Char('\n')));
}