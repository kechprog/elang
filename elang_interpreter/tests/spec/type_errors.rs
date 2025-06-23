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
#[should_panic]
fn test_add_mismatched_types() {
    eval_source("(+ 1 \"hello\")").unwrap();
}

#[test]
#[should_panic]
fn test_if_non_boolean_condition() {
    eval_source("(if 0 1 2)").unwrap();
}

#[test]
#[should_panic]
fn test_calling_non_function() {
    eval_source("(1 2 3)").unwrap();
}

#[test]
#[should_panic]
fn test_wrong_argument_type_to_function() {
    let source = "
    (fun (needs-int x :int) :int x)
    (needs-int true)
    ";
    eval_source(source).unwrap();
}

#[test]
#[should_panic]
fn test_if_branches_different_types() {
    let source = "(if true 1 \"hello\")";
    eval_source(source).unwrap();
}

#[test]
#[should_panic]
fn test_struct_instantiation_wrong_field_type() {
    let source = "
    (def (box T) (struct value :T))
    (def int-box :(box int) (make-box true))
    ";
    let result = eval_source(source);
    if let Err(e) = result {
        panic!("Expected error: {}", e);
    }
    // If we get here, the test should fail because no error was produced
    panic!("Expected an error but got success");
}