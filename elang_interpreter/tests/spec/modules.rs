use elang_interpreter::{ast::{self, Value}, interpreter};
use std::fs;
use std::io::Write;

fn eval_source_with_modules(main_source: &str, modules: &[(&str, &str)]) -> Result<ast::Value, String> {
    // Create a temporary directory for our test files
    let dir = tempfile::Builder::new().prefix("elang_tests").tempdir().unwrap();

    for (name, content) in modules {
        let mut file = fs::File::create(dir.path().join(name)).unwrap();
        writeln!(file, "{}", content).unwrap();
    }

    let mut interpreter = interpreter::Interpreter::new_with_path(dir.path().to_str().unwrap().to_string());
    let module = elang_interpreter::parser::parse(main_source)?;
    
    // Process requires first
    for require_path in module.requires {
        let require_item = elang_interpreter::ast::TopLevel::Require(require_path);
        interpreter.eval(&require_item)?;
    }
    
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
fn test_simple_require() {
    let math_module = (
        "math.elang",
        "(provides add) (fun (add a :int b :int) :int (+ a b))"
    );
    let main_source = "
    (require \"math.elang\")
    (add 10 20)
    ";
    let result = eval_source_with_modules(main_source, &[math_module]);
    println!("Result: {:?}", result);
    assert_eq!(result, Ok(Value::Int(30)));
}

#[test]
fn test_require_only_brings_provided_symbols() {
    let math_module = (
        "math.elang",
        "(provides add) 
         (fun (add a :int b :int) :int (+ a b))
         (fun (subtract a :int b :int) :int (- a b))"
    );
    let main_source = "
    (require \"math.elang\")
    (subtract 10 5)
    ";
    let result = eval_source_with_modules(main_source, &[math_module]);
    assert!(result.is_err(), "subtract should not be defined");
}

#[test]
fn test_require_does_not_execute_top_level() {
    let side_effect_module = (
        "side_effect.elang",
        "(provides x) (def x 10) (+ 1 1) ;; this addition should not be evaluated"
    );
    let main_source = "
    (require \"side_effect.elang\")
    x
    ";
     // The main test is that `eval_source_with_modules` doesn't panic or return an
    // unexpected value from the module's top-level.
    let result = eval_source_with_modules(main_source, &[side_effect_module]);
    assert_eq!(result, Ok(Value::Int(10)));
}