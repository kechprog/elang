use crate::ast::{self, Value};
use std::collections::HashMap;

pub fn populate_env(env: &mut ast::Environment) {
    let built_ins: HashMap<String, ast::Value> = HashMap::from([
        (
            "+".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.is_empty() {
                    return ast::Value::Int(0);
                }
                match args[0] {
                    ast::Value::Int(_) => {
                        let mut sum = 0;
                        for arg in args {
                            if let ast::Value::Int(n) = arg {
                                sum += n;
                            } else {
                                return ast::Value::Error("All arguments must be integers for integer addition".to_string());
                            }
                        }
                        ast::Value::Int(sum)
                    }
                    ast::Value::Float(_) => {
                        let mut sum = 0.0;
                        for arg in args {
                            if let ast::Value::Float(n) = arg {
                                sum += n;
                            } else {
                                return ast::Value::Error("All arguments must be floats for float addition".to_string());
                            }
                        }
                        ast::Value::Float(sum)
                    }
                    _ => ast::Value::Error("Invalid argument type for '+'".to_string()),
                }
            }),
        ),
        (
            "-".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.is_empty() {
                    return ast::Value::Error("'-' requires at least one argument".to_string());
                }
                match args[0] {
                    ast::Value::Int(first) => {
                        if args.len() == 1 {
                            return ast::Value::Int(-first);
                        }
                        let mut result = first;
                        for arg in &args[1..] {
                            if let ast::Value::Int(n) = arg {
                                result -= n;
                            } else {
                                return ast::Value::Error("All arguments must be integers for integer subtraction".to_string());
                            }
                        }
                        ast::Value::Int(result)
                    }
                    ast::Value::Float(first) => {
                        if args.len() == 1 {
                            return ast::Value::Float(-first);
                        }
                        let mut result = first;
                        for arg in &args[1..] {
                            if let ast::Value::Float(n) = arg {
                                result -= n;
                            } else {
                                return ast::Value::Error("All arguments must be floats for float subtraction".to_string());
                            }
                        }
                        ast::Value::Float(result)
                    }
                    _ => ast::Value::Error("Invalid argument type for '-'".to_string()),
                }
            }),
        ),
        (
            "*".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.is_empty() {
                    return ast::Value::Int(1);
                }
                match args[0] {
                    ast::Value::Int(_) => {
                        let mut product = 1;
                        for arg in args {
                            if let ast::Value::Int(n) = arg {
                                product *= n;
                            } else {
                                return ast::Value::Error("All arguments must be integers for integer multiplication".to_string());
                            }
                        }
                        ast::Value::Int(product)
                    }
                    ast::Value::Float(_) => {
                        let mut product = 1.0;
                        for arg in args {
                            if let ast::Value::Float(n) = arg {
                                product *= n;
                            } else {
                                return ast::Value::Error("All arguments must be floats for float multiplication".to_string());
                            }
                        }
                        ast::Value::Float(product)
                    }
                    _ => ast::Value::Error("Invalid argument type for '*'".to_string()),
                }
            }),
        ),
        (
            "/".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 2 {
                    return ast::Value::Error("'/' requires exactly two arguments".to_string());
                }
                match (&args[0], &args[1]) {
                    (ast::Value::Int(a), ast::Value::Int(b)) => {
                        if *b == 0 {
                            return ast::Value::Error("Division by zero".to_string());
                        }
                        ast::Value::Int(a / b)
                    }
                    (ast::Value::Float(a), ast::Value::Float(b)) => {
                        if *b == 0.0 {
                            return ast::Value::Error("Division by zero".to_string());
                        }
                        ast::Value::Float(a / b)
                    }
                    _ => ast::Value::Error("Invalid argument types for '/'".to_string()),
                }
            }),
        ),
        (
            "to-int".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 1 {
                    return ast::Value::Error("'to-int' requires exactly one argument".to_string());
                }
                match args[0] {
                    ast::Value::Float(f) => ast::Value::Int(f as i64),
                    ast::Value::Int(i) => ast::Value::Int(i),
                    _ => ast::Value::Error("Invalid argument type for 'to-int'".to_string()),
                }
            }),
        ),
        (
            "to-float".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 1 {
                    return ast::Value::Error("'to-float' requires exactly one argument".to_string());
                }
                match args[0] {
                    ast::Value::Int(i) => ast::Value::Float(i as f64),
                    ast::Value::Float(f) => ast::Value::Float(f),
                    _ => ast::Value::Error("Invalid argument type for 'to-float'".to_string()),
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
                ast::Value::Optional(None)
            }),
        ),
        (
            "mod".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 2 {
                    return ast::Value::Error("'mod' requires exactly two arguments".to_string());
                }
                if let (ast::Value::Int(a), ast::Value::Int(b)) = (&args[0], &args[1]) {
                    ast::Value::Int(a % b)
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
                if let (ast::Value::Int(a), ast::Value::Int(b)) = (&args[0], &args[1]) {
                    if *b == 0 {
                        return ast::Value::Error("Division by zero".to_string());
                    }
                    ast::Value::Int(a / b)
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
        (
            "some".to_string(),
            ast::Value::NativeFunc(|args| {
                if args.len() != 1 {
                    return ast::Value::Error("'some' requires exactly one argument".to_string());
                }
                ast::Value::Optional(Some(Box::new(args[0].clone())))
            }),
        ),
        ("none".to_string(), ast::Value::Optional(None)),
    ]);
    for (name, func) in built_ins {
        env.define(name, func);
    }
}