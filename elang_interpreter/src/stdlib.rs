use crate::ast;
use std::collections::HashMap;

pub fn populate_env(env: &mut ast::Environment) {
    let built_ins: HashMap<String, ast::Value> = HashMap::from([
        (
            "+".to_string(),
            ast::Value::NativeFunc(|args| {
                let mut sum = 0.0;
                for arg in args {
                    if let ast::Value::Number(n) = arg {
                        sum += n;
                    } else {
                        return ast::Value::Error("Invalid argument type for '+'".to_string());
                    }
                }
                ast::Value::Number(sum)
            }),
        ),
        (
            "-".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.is_empty() {
                    return ast::Value::Error("'-' requires at least one argument".to_string());
                }
                if let ast::Value::Number(first) = args[0] {
                    if args.len() == 1 {
                        return ast::Value::Number(-first);
                    }
                    let mut result = first;
                    for arg in &args[1..] {
                        if let ast::Value::Number(n) = arg {
                            result -= n;
                        } else {
                            return ast::Value::Error(
                                "Invalid argument type for '-'".to_string(),
                            );
                        }
                    }
                    ast::Value::Number(result)
                } else {
                    ast::Value::Error("Invalid argument type for '-'".to_string())
                }
            }),
        ),
        (
            "*".to_string(),
            ast::Value::NativeFunc(|args| {
                let mut product = 1.0;
                for arg in args {
                    if let ast::Value::Number(n) = arg {
                        product *= n;
                    } else {
                        return ast::Value::Error("Invalid argument type for '*'".to_string());
                    }
                }
                ast::Value::Number(product)
            }),
        ),
        (
            "/".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 2 {
                    return ast::Value::Error("'/' requires exactly two arguments".to_string());
                }
                if let (ast::Value::Number(a), ast::Value::Number(b)) = (&args[0], &args[1]) {
                    if *b == 0.0 {
                        return ast::Value::Error("Division by zero".to_string());
                    }
                    ast::Value::Number(a / b)
                } else {
                    ast::Value::Error("Invalid argument type for '/'".to_string())
                }
            }),
        ),
        (
            "print".to_string(),
            ast::Value::NativeFunc(|args| {
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        print!(" ");
                    }
                    print!("{}", arg);
                }
                println!();
                ast::Value::Nil
            }),
        ),
        (
            "mod".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 2 {
                    return ast::Value::Error("'mod' requires exactly two arguments".to_string());
                }
                if let (ast::Value::Number(a), ast::Value::Number(b)) = (&args[0], &args[1]) {
                    ast::Value::Number(a % b)
                } else {
                    ast::Value::Error("Invalid argument type for 'mod'".to_string())
                }
            }),
        ),
        (
            "div".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 2 {
                    return ast::Value::Error("'div' requires exactly two arguments".to_string());
                }
                if let (ast::Value::Number(a), ast::Value::Number(b)) = (&args[0], &args[1]) {
                    if *b == 0.0 {
                        return ast::Value::Error("Division by zero".to_string());
                    }
                    ast::Value::Number((a / b).floor())
                } else {
                    ast::Value::Error("Invalid argument type for 'div'".to_string())
                }
            }),
        ),
        (
            "=".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 2 {
                    return ast::Value::Error("'=' requires exactly two arguments".to_string());
                }
                ast::Value::Bool(args[0] == args[1])
            }),
        ),
    ]);
    for (name, func) in built_ins {
        env.define(name, func);
    }
}