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
fn test_let_simple_binding() {
    let result = eval_source("(let [(x 10)] x)");
    assert_eq!(result, Ok(Value::Int(10)));
}

#[test]
fn test_let_multiple_bindings() {
    let result = eval_source("(let [(x 10) (y 20)] (+ x y))");
    assert_eq!(result, Ok(Value::Int(30)));
}

#[test]
fn test_let_is_parallel() {
    // y should not be able to see x in the same binding block
    let result = eval_source("(let [(x 10) (y x)] y)");
    assert!(result.is_err(), "Expected an unbound variable error");
}

#[test]
fn test_let_star_sequential_binding() {
    let result = eval_source("(let* [(x 10) (y x)] y)");
    assert_eq!(result, Ok(Value::Int(10)));
}

#[test]
fn test_let_star_multiple_sequential_bindings() {
    let result = eval_source("(let* [(x 10) (y (+ x 5)) (z (* y 2))] z)");
    assert_eq!(result, Ok(Value::Int(30)));
}

#[test]
fn test_let_destructuring() {
    let source = "
    (def (vec2 T) (struct x :T y :T))
    (def my-vec :(vec2 int) (make-vec2 10 20))
    (let [((vec2 x y) my-vec)] (+ x y))
    ";
    let result = eval_source(source);
    assert_eq!(result, Ok(Value::Int(30)));
}

#[test]
#[should_panic]
fn test_let_destructuring_fail() {
    let source = "
    (def (vec2 T) (struct x :T y :T))
    (def my-vec :(vec2 int) (make-vec2 10 20))
    (let [((vec3 x y z) my-vec)] (+ x y))
    ";
    eval_source(source).unwrap();
}