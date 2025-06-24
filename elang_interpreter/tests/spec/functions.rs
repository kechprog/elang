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
fn test_simple_function_call() {
    let source = "
    (fun (add a :int b :int) :int (+ a b))
    (add 2 3)
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(5)));
}

#[test]
fn test_higher_order_function() {
    let source = "
    (fun (apply f :(fun-type (int) int) x :int) :int (f x))
    (fun (double y :int) :int (* y 2))
    (apply double 5)
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(10)));
}

#[test]
fn test_closure_captures_environment() {
    let source = "
    (fun (make-adder x :int) :(fun-type (int) int)
      (fun (add-y y :int) :int (+ x y)))
    (let [(add5 (make-adder 5))]
      (add5 10))
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(15)));
}

#[test]
fn test_closure_with_multiple_captures() {
    let source = "
    (fun (make-multiplier x :int y :int) :(fun-type (int) int)
      (fun (mult-z z :int) :int (* x y z)))
    (let [(mult6 (make-multiplier 2 3))]
      (mult6 4))
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(24)));
}

#[test]
fn test_function_wrong_arg_count() {
    let source = "
    (fun (add a :int b :int) :int (+ a b))
    (add 1)
    ";
    let result = eval_source(source);
    assert!(result.is_err(), "Expected an error for wrong argument count");
}