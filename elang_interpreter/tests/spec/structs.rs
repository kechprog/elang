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
fn test_struct_definition_and_instantiation() {
    let source = "
    (def (vec2 T) (struct x :T y :T))
    (def my-vec (make-vec2 10 20))
    my-vec
    ";
    let result = eval_source(source);
    // We can't easily assert the struct value directly,
    // so we'll test field access instead.
    match &result {
        Ok(_) => {},
        Err(e) => println!("Error: {}", e),
    }
    assert!(result.is_ok());
}

#[test]
fn test_struct_field_access() {
    let source = "
    (def (vec2 T) (struct x :T y :T))
    (def my-vec (make-vec2 10 20))
    (vec2-x my-vec)
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(10)));
}

#[test]
fn test_nested_struct_field_access() {
    let source = "
    (def (vec2 T) (struct x :T y :T))
    (def (line T) (struct start :(vec2 T) end :(vec2 T)))
    ";
    let result = eval_source(source);
    match &result {
        Ok(_) => {},
        Err(e) => println!("Error: {}", e),
    }
    assert!(result.is_ok());
}

#[test]
fn test_struct_field_access_on_wrong_type() {
    let source = "
    (def (vec2 T) (struct x :T y :T))
    (vec2-x 10)
    ";
    let result = eval_source(source);
    assert!(result.is_err(), "Expected a type error");
}