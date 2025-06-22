use elang_interpreter::{ast, interpreter, parser};
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let source = match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", file_path, e);
            return;
        }
    };

    let ast = match parser::parse(&source) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parsing error: {}", e);
            return;
        }
    };

    let interpreter = interpreter::Interpreter::new();
    let mut last_result = Ok(ast::Value::Nil);

    for expr in ast {
        last_result = interpreter.eval(&expr);
        if let Err(e) = &last_result {
            eprintln!("Runtime error: {}", e);
            return;
        }
    }

    if let Ok(value) = last_result {
        println!("{}", value);
    }
}
