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
fn test_generic_struct_with_int() {
    let source = "
    (def (box T) (struct value :T))
    (def int-box :(box int) (make-box 10))
    (box-value int-box)
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(10)));
}

#[test]
fn test_generic_struct_with_bool() {
    let source = "
    (def (box T) (struct value :T))
    (def bool-box :(box bool) (make-box true))
    (box-value bool-box)
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Bool(true)));
}

#[test]
fn test_nested_generic_struct() {
    let source = "
    (def (box T) (struct value :T))
    (def box-of-box :(box (box int)) (make-box (make-box 42)))
    (box-value (box-value box-of-box))
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(42)));
}

#[test]
fn test_generic_struct_in_function() {
    let source = "
    (def (box T) (struct value :T))
    (fun (unbox b :(box int)) :int (box-value b))
    (unbox (make-box 123))
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(123)));
}