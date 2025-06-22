use crate::ast::{self, Expr, Literal, Pattern, Type};
use lexpr::{Value, Parser, parse::Error};

pub fn parse(source: &str) -> Result<Vec<ast::Expr>, String> {
    let mut parser = Parser::from_str(source);
    let mut exprs = Vec::new();

    while let Some(sexpr_res) = parser.next() {
        let sexpr = sexpr_res.map_err(|e: Error| e.to_string())?;
        let expr = parse_expr(&sexpr)?;
        exprs.push(expr);
    }
    
    Ok(exprs)
}

fn to_proper_list(value: &Value) -> Result<Vec<Value>, String> {
    if let Some(cons) = value.as_cons() {
        let (vec, tail) = cons.to_vec();
        if tail.is_null() {
            Ok(vec)
        } else {
            Err(format!("Expected a proper list, but got an improper list ending with {}", tail))
        }
    } else {
        Err(format!("Expected a list, but got {}", value))
    }
}

fn parse_expr(value: &Value) -> Result<Expr, String> {
    match value {
        Value::Symbol(s) => match s.as_ref() {
            "true" => Ok(Expr::Literal(Literal::Bool(true))),
            "false" => Ok(Expr::Literal(Literal::Bool(false))),
            "none" => Ok(Expr::Literal(Literal::Nil)),
            _ => Ok(Expr::Identifier(s.to_string())),
        },
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(Expr::Literal(Literal::Int(i)))
            } else if let Some(f) = n.as_f64() {
                Ok(Expr::Literal(Literal::Float(f)))
            } else {
                Err(format!("Unsupported number type: {}", n))
            }
        }
        Value::Bool(b) => Ok(Expr::Literal(Literal::Bool(*b))),
        Value::Char(c) => Ok(Expr::Literal(Literal::Char(*c))),
        Value::Null => Ok(Expr::Literal(Literal::Nil)),
        Value::Cons(_) => {
            let list = to_proper_list(value)?;
            if list.is_empty() {
                return Err("Unexpected empty list".to_string());
            }

            let head = &list[0];
            let tail = &list[1..];

            if let Some(op) = head.as_symbol() {
                match op.as_ref() {
                    "if" => parse_if(tail),
                    "let" | "let*" => parse_let(op.as_ref(), tail),
                    "fn" => parse_function(tail),
                    _ => parse_call(head, tail),
                }
            } else {
                parse_call(head, tail)
            }
        }
        _ => Err(format!("Unsupported expression type: {:?}", value)),
    }
}

fn parse_if(parts: &[Value]) -> Result<Expr, String> {
    if parts.len() != 3 {
        return Err(format!("'if' expects 3 arguments, got {}", parts.len()));
    }
    let condition = Box::new(parse_expr(&parts[0])?);
    let then_branch = Box::new(parse_expr(&parts[1])?);
    let else_branch = Box::new(parse_expr(&parts[2])?);
    Ok(Expr::If {
        condition,
        then_branch,
        else_branch,
    })
}

fn parse_let(op: &str, parts: &[Value]) -> Result<Expr, String> {
    if parts.len() != 2 {
        return Err(format!("'{}' expects 2 arguments, got {}", op, parts.len()));
    }

    let bindings_sexpr = &parts[0];
    let body_sexpr = &parts[1];

    let mut bindings = Vec::new();
    let binding_list = to_proper_list(bindings_sexpr)?;
    
    for pair_val in binding_list {
        let p = to_proper_list(&pair_val)?;
        if p.len() != 2 {
            return Err("let binding must be a pair".to_string());
        }
        let pattern = parse_pattern(&p[0])?;
        let expr = parse_expr(&p[1])?;
        bindings.push((pattern, expr));
    }

    let body = Box::new(parse_expr(body_sexpr)?);
    let sequential = op == "let*";

    Ok(Expr::Let {
        bindings,
        body,
        sequential,
    })
}

fn parse_function(parts: &[Value]) -> Result<Expr, String> {
    if parts.len() != 3 {
        return Err(format!(
            "Function definition expects 3 arguments, got {}",
            parts.len()
        ));
    }

    let params_sexpr = &parts[0];
    let ret_type_sexpr = &parts[1];
    let body_sexpr = &parts[2];

    let mut params = Vec::new();
    if !params_sexpr.is_null() {
        let p_list = to_proper_list(params_sexpr)?;
        for p_val in p_list {
            let pair = to_proper_list(&p_val)?;
            if pair.len() != 2 {
                return Err("Function parameter must be a (pattern type) pair".to_string());
            }
            let pattern = parse_pattern(&pair[0])?;
            let type_ = parse_type(&pair[1])?;
            params.push((pattern, type_));
        }
    }

    let return_type = parse_type(ret_type_sexpr)?;
    let body = Box::new(parse_expr(body_sexpr)?);

    Ok(Expr::Function {
        params,
        return_type,
        body,
    })
}

fn parse_call(head: &Value, tail: &[Value]) -> Result<Expr, String> {
    let function = Box::new(parse_expr(head)?);
    let arguments = tail
        .iter()
        .map(parse_expr)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::Call {
        function,
        arguments,
    })
}

fn parse_pattern(value: &Value) -> Result<Pattern, String> {
    match value {
        Value::Symbol(s) => Ok(Pattern::Identifier(s.to_string())),
        Value::Cons(_) => {
            let list = to_proper_list(value)?;
            if list.is_empty() {
                return Err("Unexpected empty list for pattern".to_string());
            }
            if let Some(name) = list[0].as_symbol() {
                let fields = list[1..]
                    .iter()
                    .map(|s_val| {
                        s_val.as_symbol()
                            .map(|s| s.to_string())
                            .ok_or_else(|| "Struct pattern fields must be identifiers".to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Pattern::Struct {
                    name: name.to_string(),
                    fields,
                })
            } else {
                Err("Struct pattern must start with a name".to_string())
            }
        }
        _ => Err(format!("Invalid pattern: {:?}", value)),
    }
}

fn parse_type(value: &Value) -> Result<Type, String> {
    match value {
        Value::Symbol(s) => match s.as_ref() {
            "int" => Ok(Type::Int),
            "float" => Ok(Type::Float),
            "bool" => Ok(Type::Bool),
            "char" => Ok(Type::Char),
            "void" => Ok(Type::Void),
            _ => Ok(Type::Named {
                name: s.to_string(),
                type_params: Vec::new(),
            }),
        },
        Value::Cons(_) => {
            let list = to_proper_list(value)?;
            if list.is_empty() {
                return Err("Unexpected empty list for type".to_string());
            }
            let head = &list[0];
            let tail = &list[1..];
            
            if let Some(op) = head.as_symbol() {
                match op.as_ref() {
                    "Option" => {
                        if tail.len() != 1 {
                            return Err(format!(
                                "'Option' type expects 1 argument, got {}",
                                tail.len()
                            ));
                        }
                        Ok(Type::Option(Box::new(parse_type(&tail[0])?)))
                    }
                    "fun-type" => {
                        if tail.len() != 2 {
                            return Err(format!(
                                "'fun-type' expects 2 arguments, got {}",
                                tail.len()
                            ));
                        }
                        
                        let param_types_sexpr = &tail[0];
                        let return_type_sexpr = &tail[1];

                        let param_types = if !param_types_sexpr.is_null() {
                            to_proper_list(param_types_sexpr)?
                                .iter()
                                .map(parse_type)
                                .collect::<Result<Vec<_>,_>>()?
                        } else {
                            Vec::new()
                        };

                        let return_type = Box::new(parse_type(return_type_sexpr)?);

                        Ok(Type::Function {
                            param_types,
                            return_type,
                        })
                    }
                    _ => {
                        // User-defined generic type, e.g., `(vec3 float)`
                        let type_params = tail
                            .iter()
                            .map(parse_type)
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(Type::Named {
                            name: op.to_string(),
                            type_params,
                        })
                    }
                }
            } else {
                Err("Type must start with an atom".to_string())
            }
        }
        _ => Err("Invalid type expression".to_string()),
    }
}