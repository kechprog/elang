use elang_interpreter::interpreter;
use std::env;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let mut interpreter = interpreter::Interpreter::new_with_path(file_path.clone());
    match interpreter.eval_file(Path::new(file_path)) {
        Ok(value) => println!("{}", value),
        Err(e) => eprintln!("Error: {}", e),
    }
}
