use elang_interpreter::{ast, interpreter};

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
fn test_if_let_some() {
    let source = r#"
    (if-let (x (some 10))
        x
        -1)
    "#;
    let result = eval_source(source);
    assert_eq!(result, Ok(ast::Value::Int(10)));
}

#[test]
fn test_if_let_none() {
    let source = r#"
    (if-let (x none)
        x
        -1)
    "#;
    let result = eval_source(source);
    assert_eq!(result, Ok(ast::Value::Int(-1)));
}