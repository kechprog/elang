//! Parser for the Elang programming language.
//! 
//! This crate provides parsing functionality that converts S-expression source code
//! into the AST structures defined in `elang_ast`. It can be used by both the
//! interpreter and other tools like the LSP server.

use elang_ast::{Expr, FunDef, Literal, Module, Pattern, TopLevel, Type, StructDef};
use lexpr::{parse::Error, Parser, Value};

pub fn parse(source: &str) -> Result<Module, String> {
    let mut parser = Parser::from_str(source);
    let mut provides = Vec::new();
    let mut requires = Vec::new();
    let mut body = Vec::new();

    while let Some(sexpr_res) = parser.next() {
        let sexpr = sexpr_res.map_err(|e: Error| e.to_string())?;
        let item = parse_toplevel(&sexpr)?;
        match item {
            TopLevel::Provides(p) => provides.extend(p),
            TopLevel::Require(r) => requires.push(r),
            other => body.push(other),
        }
    }

    Ok(Module {
        provides,
        requires,
        body,
    })
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

fn parse_toplevel(value: &Value) -> Result<TopLevel, String> {
    if let Ok(list) = to_proper_list(value) {
        if let Some(head) = list.get(0).and_then(|v| v.as_symbol()) {
            if head == "def" {
                // Check if this is a struct definition
                if list.len() == 3 {
                    // Check if the first argument is a list (generic struct) or symbol (regular def)
                    if let Some(first_arg) = list.get(1) {
                        if first_arg.as_cons().is_some() {
                            // This is likely a generic struct: (def (name T) (struct ...))
                            if let Some(def_body) = list.get(2) {
                                if let Some(cons) = def_body.as_cons() {
                                    if let Some(form) = cons.car().as_symbol() {
                                        if form == "struct" {
                                            return parse_def(&list[1..]).map(TopLevel::StructDef);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                return parse_definition(&list[1..]);
            } else if head == "provides" {
                return parse_provides(&list[1..]);
            } else if head == "require" {
                return parse_require(&list[1..]);
            } else if head == "fun" {
                return parse_fun_definition(&list[1..]);
            }
        }
    }
    parse_expr(value).map(TopLevel::Expr)
}

fn parse_provides(parts: &[Value]) -> Result<TopLevel, String> {
    let names = parts
        .iter()
        .map(|v| {
            v.as_symbol()
                .map(|s| s.to_string())
                .ok_or_else(|| "provide name must be a symbol".to_string())
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(TopLevel::Provides(names))
}

fn parse_require(parts: &[Value]) -> Result<TopLevel, String> {
    if parts.len() != 1 {
        return Err(format!("'require' expects 1 argument, got {}", parts.len()));
    }
    let path = parts[0]
        .as_str()
        .ok_or("require path must be a string")?
        .to_string();
    Ok(TopLevel::Require(path))
}

fn parse_definition(parts: &[Value]) -> Result<TopLevel, String> {
    if parts.len() < 1 {
        return Err(format!(
            "'def' syntax error: expected at least 1 part, got {}",
            parts.len()
        ));
    }

    // Variable definition `(def name value)` or `(def name :type value)`
    if let Some(name) = parts[0].as_symbol() {
        if parts.len() == 2 {
            let value_expr = parse_expr(&parts[1])?;
            return Ok(TopLevel::VarDef(name.to_string(), value_expr));
        } else if parts.len() == 3 {
            // Handle type annotations: (def name :type value)
            let value_expr = parse_expr(&parts[2])?;
            return Ok(TopLevel::VarDef(name.to_string(), value_expr));
        } else if parts.len() == 4 {
            // Handle split type annotations: (def name : type value)
            if let Some(colon_sym) = parts[1].as_symbol() {
                if colon_sym == ":" {
                    let value_expr = parse_expr(&parts[3])?;
                    return Ok(TopLevel::VarDef(name.to_string(), value_expr));
                }
            }
            return Err(format!(
                "'def' for a variable expects 2 or 3 parts (name, optional type, value), but got {}",
                parts.len()
            ));
        } else {
            return Err(format!(
                "'def' for a variable expects 2 or 3 parts (name, optional type, value), but got {}",
                parts.len()
            ));
        }
    }

    Err("Invalid 'def' syntax: name must be a symbol".to_string())
}

fn parse_fun_definition(parts: &[Value]) -> Result<TopLevel, String> {
    // Handle the case where return type annotation is split: signature : type body
    let (signature_sexpr, return_type, body_sexpr) = if parts.len() == 4 {
        // Check if parts[1] is ":" - this means return type is split
        if let Some(colon_sym) = parts[1].as_symbol() {
            if colon_sym == ":" {
                // Format: signature : return_type body
                (&parts[0], parse_type(&parts[2])?, &parts[3])
            } else {
                return Err(format!(
                    "Function definition expects 2 or 3 arguments (signature, [return type], body), but got {}",
                    parts.len()
                ));
            }
        } else {
            return Err(format!(
                "Function definition expects 2 or 3 arguments (signature, [return type], body), but got {}",
                parts.len()
            ));
        }
    } else if parts.len() == 3 {
        // Format: signature return_type body
        (&parts[0], parse_type(&parts[1])?, &parts[2])
    } else if parts.len() == 2 {
        // Format: signature body
        (&parts[0], Type::Void, &parts[1])
    } else {
        return Err(format!(
            "Function definition expects 2 or 3 arguments (signature, [return type], body), but got {}",
            parts.len()
        ));
    };

    let signature_list = to_proper_list(signature_sexpr)?;
    if signature_list.is_empty() {
        return Err("Function signature cannot be empty".to_string());
    }

    let fn_name = signature_list[0]
        .as_symbol()
        .ok_or("Function name must be a symbol")?
        .to_string();

    let params_list = &signature_list[1..];
    let mut params = Vec::new();
    
    if !params_list.is_empty() {
        let mut i = 0;
        while i < params_list.len() {
            // Expect parameter name
            if i >= params_list.len() {
                return Err("Function parameters must be in `name :type` pairs".to_string());
            }
            
            let name_val = &params_list[i];
            if name_val.as_symbol().is_none() {
                return Err("Function parameter name must be a symbol".to_string());
            }
            i += 1;
            
            // Expect type annotation
            if i >= params_list.len() {
                return Err("Function parameters must be in `name :type` pairs".to_string());
            }
            
            let type_val = if let Some(colon_sym) = params_list[i].as_symbol() {
                if colon_sym == ":" {
                    // The type annotation is split: `:` followed by the actual type
                    i += 1;
                    if i >= params_list.len() {
                        return Err("Expected type after ':' in parameter".to_string());
                    }
                    &params_list[i]
                } else if colon_sym.starts_with(':') {
                    // This is a symbol like ":int" - treat it as a type annotation
                    &params_list[i]
                } else {
                    return Err("Expected type annotation starting with ':' in parameter".to_string());
                }
            } else {
                // This should be a keyword like `:int` or a cons structure
                &params_list[i]
            };
            i += 1;

            let pattern = parse_pattern(name_val)?;
            let type_ = parse_type(type_val)?;
            params.push((pattern, type_));
        }
    }

    let body = parse_expr(body_sexpr)?;

    let fun_def = FunDef {
        name: fn_name,
        params,
        return_type,
        body,
    };

    Ok(TopLevel::FunDef(fun_def))
}

fn parse_def(parts: &[Value]) -> Result<StructDef, String> {
    if parts.len() != 2 {
        return Err(format!("'def' expects 2 arguments, got {}", parts.len()));
    }
    let header = to_proper_list(&parts[0])?;
    if header.is_empty() {
        return Err("def header cannot be empty".to_string());
    }
    let name = header[0]
        .as_symbol()
        .ok_or("def name must be a symbol")?
        .to_string();
    let params = header[1..]
        .iter()
        .map(|v| {
            v.as_symbol()
                .map(|s| s.to_string())
                .ok_or_else(|| "def type parameter must be a symbol".to_string())
        })
        .collect::<Result<Vec<_>, _>>()?;

    let struct_form = to_proper_list(&parts[1])?;
    if struct_form.get(0).and_then(|v| v.as_symbol()) != Some("struct".into()) {
        return Err("Expected 'struct' form in def".to_string());
    }

    let mut fields = Vec::new();
    let field_parts = &struct_form[1..];
    
    // Parse fields in the format: field1 :type1 field2 :type2 ...
    let mut i = 0;
    while i < field_parts.len() {
        if i + 1 >= field_parts.len() {
            return Err("struct field must have both name and type".to_string());
        }
        
        let field_name = field_parts[i]
            .as_symbol()
            .ok_or("Field name must be a symbol")?
            .to_string();
        
        // Handle the case where the type annotation might be split
        // e.g., "start : (vec2 T)" vs "start :(vec2 T)"
        let (field_type, next_i) = if let Some(colon_sym) = field_parts[i + 1].as_symbol() {
            if colon_sym == ":" {
                // Type is split: field : type
                if i + 2 >= field_parts.len() {
                    return Err("Expected type after ':' in struct field".to_string());
                }
                (parse_type(&field_parts[i + 2])?, i + 3)
            } else {
                // Type is not split: field :type
                (parse_type(&field_parts[i + 1])?, i + 2)
            }
        } else {
            // Type is a complex expression
            (parse_type(&field_parts[i + 1])?, i + 2)
        };
        
        fields.push((field_name, field_type));
        i = next_i;
    }

    Ok(StructDef {
        name,
        params,
        fields,
    })
}

fn parse_if(parts: &[Value]) -> Result<Expr, String> {
    if parts.len() != 3 {
        return Err(format!("'if' expects 3 arguments, got {}", parts.len()));
    }

    let condition_sexpr = &parts[0];
    let then_branch_sexpr = &parts[1];
    let else_branch_sexpr = &parts[2];


    let condition = Box::new(parse_expr(condition_sexpr)?);
    let then_branch = Box::new(parse_expr(then_branch_sexpr)?);
    let else_branch = Box::new(parse_expr(else_branch_sexpr)?);

    Ok(Expr::If {
        condition,
        then_branch,
        else_branch,
    })
}

fn parse_if_let(parts: &[Value]) -> Result<Expr, String> {
    if parts.len() != 3 {
        return Err(format!(
            "'if-let' expects 3 arguments, got {}",
            parts.len()
        ));
    }

    let binding_sexpr = &parts[0];
    let then_branch_sexpr = &parts[1];
    let else_branch_sexpr = &parts[2];

    let binding_list = to_proper_list(binding_sexpr)?;
    if binding_list.len() != 2 {
        return Err(format!(
            "'if-let' binding part expects 2 arguments, got {}",
            binding_list.len()
        ));
    }

    let identifier = binding_list[0]
        .as_symbol()
        .ok_or("if-let binding must be a symbol")?
        .to_string();

    let expr = Box::new(parse_expr(&binding_list[1])?);
    let then_branch = Box::new(parse_expr(then_branch_sexpr)?);
    let else_branch = Box::new(parse_expr(else_branch_sexpr)?);

    Ok(Expr::IfLet {
        identifier,
        expr,
        then_branch,
        else_branch,
    })
}

fn parse_quote(parts: &[Value]) -> Result<Expr, String> {
    if parts.len() != 1 {
        return Err(format!("'quote' expects 1 argument, got {}", parts.len()));
    }
    Ok(Expr::Quote(parts[0].clone()))
}

fn parse_expr(value: &Value) -> Result<Expr, String> {
    match value {
        Value::Symbol(s) => match s.as_ref() {
            "true" => Ok(Expr::Literal(Literal::Bool(true))),
            "false" => Ok(Expr::Literal(Literal::Bool(false))),
            "none" => Ok(Expr::Identifier("none".to_string())),
            "def" => Ok(Expr::Identifier("def".to_string())),
            _ => Ok(Expr::Identifier(s.to_string())),
        },
        Value::String(s) => {
            let mut current_expr = Expr::Identifier("none".to_string());
            for char in s.chars().rev() {
                current_expr = Expr::Call {
                    function: Box::new(Expr::Identifier("cons".to_string())),
                    arguments: vec![Expr::Literal(Literal::Char(char)), current_expr],
                };
            }
            Ok(current_expr)
        }
        Value::Number(n) => {
            if n.is_i64() {
                Ok(Expr::Literal(Literal::Int(n.as_i64().unwrap())))
            } else if n.is_f64() {
                Ok(Expr::Literal(Literal::Float(n.as_f64().unwrap())))
            } else {
                Err(format!("Unsupported number type: {}", n))
            }
        }
        Value::Bool(b) => Ok(Expr::Literal(Literal::Bool(*b))),
        Value::Char(c) => Ok(Expr::Literal(Literal::Char(*c))),
        Value::Null => Ok(Expr::Identifier("none".to_string())),
        Value::Cons(cons) => {
            let (list, tail) = cons.to_vec();
            if !tail.is_null() {
                return Err(format!("Improper lists are not supported: {}", value));
            }
            if list.is_empty() {
                return Err("Unexpected empty list".to_string());
            }

            let head = &list[0];
            let tail_parts = &list[1..];

            if let Some(op) = head.as_symbol() {
                match op.as_ref() {
                    "if" => parse_if(tail_parts),
                    "if-let" => parse_if_let(tail_parts),
                    "let" | "let*" => parse_let(op.as_ref(), tail_parts),
                    "fun" => parse_function(tail_parts),
                    "quote" => parse_quote(tail_parts),
                    "def" => Err("def is not an expression".to_string()),
                    "provides" => Err("provides is not an expression".to_string()),
                    "require" => Err("require is not an expression".to_string()),
                    _ => parse_call(head, tail_parts),
                }
            } else {
                parse_call(head, tail_parts)
            }
        }
        _ => Err(format!("Unsupported expression type: {:?}", value)),
    }
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
    if op == "let*" {
        Ok(Expr::LetStar { bindings, body })
    } else {
        Ok(Expr::Let { bindings, body })
    }
}

fn parse_function(parts: &[Value]) -> Result<Expr, String> {
    if parts.len() < 2 || parts.len() > 3 {
        return Err(format!(
            "Function expression expects 2 or 3 arguments (params, [return-type], body), got {}",
            parts.len()
        ));
    }

    let params_sexpr = &parts[0];

    let return_type = if parts.len() == 3 {
        parse_type(&parts[1])?
    } else {
        Type::Void
    };

    let body_sexpr = if parts.len() == 3 {
        &parts[2]
    } else {
        &parts[1]
    };

    let mut params = Vec::new();
    if !params_sexpr.is_null() {
        let p_list = to_proper_list(params_sexpr)?;
        if !p_list.is_empty() {
            // Check if this is a named function expression (first element is function name)
            // If the parameter list has an odd number of elements, the first one might be a function name
            let (start_idx, _fn_name) = if p_list.len() % 2 == 1 {
                // Odd number of elements - first element is likely the function name
                (1, Some(p_list[0].as_symbol().unwrap_or("").to_string()))
            } else {
                // Even number of elements - no function name
                (0, None)
            };
            
            let params_list = &p_list[start_idx..];
            let mut i = 0;
            while i < params_list.len() {
                // Expect parameter name
                if i >= params_list.len() {
                    return Err("Function parameters must be in `name :type` pairs".to_string());
                }
                
                let name_val = &params_list[i];
                if name_val.as_symbol().is_none() {
                    return Err("Function parameter name must be a symbol".to_string());
                }
                i += 1;
                
                // Expect type annotation
                if i >= params_list.len() {
                    return Err("Function parameters must be in `name :type` pairs".to_string());
                }
                
                let type_val = if let Some(colon_sym) = params_list[i].as_symbol() {
                    if colon_sym == ":" {
                        // The type annotation is split: `:` followed by the actual type
                        i += 1;
                        if i >= params_list.len() {
                            return Err("Expected type after ':' in parameter".to_string());
                        }
                        &params_list[i]
                    } else if colon_sym.starts_with(':') {
                        // This is a symbol like ":int" - treat it as a type annotation
                        &params_list[i]
                    } else {
                        return Err("Expected type annotation starting with ':' in parameter".to_string());
                    }
                } else {
                    // This should be a keyword like `:int` or a cons structure
                    &params_list[i]
                };
                i += 1;

                let pattern = parse_pattern(name_val)?;
                let type_ = parse_type(type_val)?;
                params.push((pattern, type_));
            }
        }
    }

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
                        s_val
                            .as_symbol()
                            .map(|s| s.to_string())
                            .ok_or_else(|| "Struct pattern fields must be identifiers".to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Pattern::Struct {
                    name: name.to_string(),
                    fields,
                })
            } else {
                let patterns = list
                    .iter()
                    .map(parse_pattern)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Pattern::List(patterns))
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
            "string" => Ok(Type::Named {
                name: "cons".to_string(),
                type_params: vec![Type::Char],
            }),
            ":int" => Ok(Type::Int),
            ":float" => Ok(Type::Float),
            ":bool" => Ok(Type::Bool),
            ":char" => Ok(Type::Char),
            ":void" => Ok(Type::Void),
            ":string" => Ok(Type::Named {
                name: "cons".to_string(),
                type_params: vec![Type::Char],
            }),
            s if s.chars().next().map_or(false, |c| c.is_uppercase()) => {
                Ok(Type::GenericParam(s.to_string()))
            }
            s if s.starts_with(':') && s.len() > 1 => {
                let type_name = &s[1..];
                if type_name.chars().next().map_or(false, |c| c.is_uppercase()) {
                    Ok(Type::GenericParam(type_name.to_string()))
                } else {
                    match type_name {
                        "int" => Ok(Type::Int),
                        "float" => Ok(Type::Float),
                        "bool" => Ok(Type::Bool),
                        "char" => Ok(Type::Char),
                        "void" => Ok(Type::Void),
                        "string" => Ok(Type::Named {
                            name: "cons".to_string(),
                            type_params: vec![Type::Char],
                        }),
                        _ => Ok(Type::Named {
                            name: type_name.to_string(),
                            type_params: Vec::new(),
                        }),
                    }
                }
            }
            _ => Ok(Type::Named {
                name: s.to_string(),
                type_params: Vec::new(),
            }),
        },
        Value::Keyword(k) => {
            let type_source = k.as_ref();
            match type_source {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
                "bool" => Ok(Type::Bool),
                "char" => Ok(Type::Char),
                "void" => Ok(Type::Void),
                "string" => Ok(Type::Named {
                    name: "cons".to_string(),
                    type_params: vec![Type::Char],
                }),
                _ => {
                    // For complex type expressions like :(fun-type (int) int)
                    // Parse the content after the colon
                    let mut inner_parser = Parser::from_str(type_source);
                    match inner_parser.next() {
                        Some(Ok(sexpr)) => {
                            if inner_parser.next().is_none() {
                                parse_type(&sexpr)
                            } else {
                                Err(format!(
                                    "Invalid type keyword: multiple S-expressions in keyword {}",
                                    k
                                ))
                            }
                        }
                        Some(Err(e)) => Err(format!("Invalid type keyword syntax in {}: {}", k, e)),
                        None => Err(format!("Invalid empty type keyword: {}", k)),
                    }
                }
            }
        }
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
                                .collect::<Result<Vec<_>, _>>()?
                        } else {
                            Vec::new()
                        };

                        let return_type = Box::new(parse_type(return_type_sexpr)?);

                        Ok(Type::Func {
                            param_types,
                            return_type,
                        })
                    }
                    _ => {
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

#[cfg(test)]
mod tests {
    use super::*;
    use elang_ast::{Expr, Literal, Pattern, TopLevel, Type};

    // --- Basic Parsing Tests ---

    #[test]
    fn test_literal_parsing() {
        // Integer literals
        let module = parse("42").unwrap();
        assert_eq!(module.body.len(), 1);
        match &module.body[0] {
            TopLevel::Expr(Expr::Literal(Literal::Int(n))) => assert_eq!(*n, 42),
            _ => panic!("Expected integer literal"),
        }

        // Float literals
        let module = parse("3.14").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Literal(Literal::Float(f))) => assert!((f - 3.14).abs() < f64::EPSILON),
            _ => panic!("Expected float literal"),
        }

        // Boolean literals
        let module = parse("true").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Literal(Literal::Bool(b))) => assert!(*b),
            _ => panic!("Expected boolean literal"),
        }

        let module = parse("false").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Literal(Literal::Bool(b))) => assert!(!*b),
            _ => panic!("Expected boolean literal"),
        }

        // Character literals
        let module = parse("#\\a").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Literal(Literal::Char(c))) => assert_eq!(*c, 'a'),
            _ => panic!("Expected character literal"),
        }
    }

    #[test]
    fn test_identifier_parsing() {
        let module = parse("x").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Identifier(name)) => assert_eq!(name, "x"),
            _ => panic!("Expected identifier"),
        }

        let module = parse("my-variable").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Identifier(name)) => assert_eq!(name, "my-variable"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_simple_expressions() {
        // Simple function call
        let module = parse("(+ 1 2)").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Call { function, arguments }) => {
                assert!(matches!(function.as_ref(), Expr::Identifier(_)));
                assert_eq!(arguments.len(), 2);
            }
            _ => panic!("Expected function call"),
        }

        // Nested function call
        let module = parse("(+ (* 2 3) 4)").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Call { function, arguments }) => {
                assert!(matches!(function.as_ref(), Expr::Identifier(_)));
                assert_eq!(arguments.len(), 2);
                assert!(matches!(arguments[0], Expr::Call { .. }));
            }
            _ => panic!("Expected nested function call"),
        }
    }

    #[test]
    fn test_function_calls_with_various_argument_counts() {
        // No arguments
        let module = parse("(foo)").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Call { arguments, .. }) => assert_eq!(arguments.len(), 0),
            _ => panic!("Expected function call"),
        }

        // One argument
        let module = parse("(foo 1)").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Call { arguments, .. }) => assert_eq!(arguments.len(), 1),
            _ => panic!("Expected function call"),
        }

        // Multiple arguments
        let module = parse("(foo 1 2 3 4 5)").unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Call { arguments, .. }) => assert_eq!(arguments.len(), 5),
            _ => panic!("Expected function call"),
        }
    }

    // --- Complex Parsing Tests ---

    #[test]
    fn test_nested_expressions() {
        let source = "(if (> x 0) (+ x 1) (- x 1))";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::Expr(Expr::If { condition, then_branch, else_branch }) => {
                assert!(matches!(condition.as_ref(), Expr::Call { .. }));
                assert!(matches!(then_branch.as_ref(), Expr::Call { .. }));
                assert!(matches!(else_branch.as_ref(), Expr::Call { .. }));
            }
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_let_bindings() {
        let source = "(let ((x 10) (y 20)) (+ x y))";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::Expr(Expr::Let { bindings, body }) => {
                assert_eq!(bindings.len(), 2);
                assert!(matches!(bindings[0].0, Pattern::Identifier(_)));
                assert!(matches!(bindings[1].0, Pattern::Identifier(_)));
                assert!(matches!(body.as_ref(), Expr::Call { .. }));
            }
            _ => panic!("Expected let expression"),
        }
    }

    #[test]
    fn test_let_star_bindings() {
        let source = "(let* ((x 10) (y (+ x 5))) (+ x y))";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::Expr(Expr::LetStar { bindings, body }) => {
                assert_eq!(bindings.len(), 2);
                assert!(matches!(body.as_ref(), Expr::Call { .. }));
            }
            _ => panic!("Expected let* expression"),
        }
    }

    #[test]
    fn test_if_expressions() {
        let source = "(if true 1 2)";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::Expr(Expr::If { condition, then_branch, else_branch }) => {
                assert!(matches!(condition.as_ref(), Expr::Literal(Literal::Bool(true))));
                assert!(matches!(then_branch.as_ref(), Expr::Literal(Literal::Int(1))));
                assert!(matches!(else_branch.as_ref(), Expr::Literal(Literal::Int(2))));
            }
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_if_let_expressions() {
        let source = "(if-let (x some-value) x none)";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::Expr(Expr::IfLet { identifier, expr, then_branch, else_branch }) => {
                assert_eq!(identifier, "x");
                assert!(matches!(expr.as_ref(), Expr::Identifier(_)));
                assert!(matches!(then_branch.as_ref(), Expr::Identifier(_)));
                assert!(matches!(else_branch.as_ref(), Expr::Identifier(_)));
            }
            _ => panic!("Expected if-let expression"),
        }
    }

    #[test]
    fn test_function_definitions() {
        let source = "(fun (add x :int y :int) :int (+ x y))";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::FunDef(fun_def) => {
                assert_eq!(fun_def.name, "add");
                assert_eq!(fun_def.params.len(), 2);
                assert_eq!(fun_def.return_type, Type::Int);
                assert!(matches!(fun_def.body, Expr::Call { .. }));
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_function_definitions_with_split_type_annotations() {
        let source = "(fun (add x : int y : int) : int (+ x y))";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::FunDef(fun_def) => {
                assert_eq!(fun_def.name, "add");
                assert_eq!(fun_def.params.len(), 2);
                assert_eq!(fun_def.return_type, Type::Int);
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_struct_definitions() {
        let source = "(def (Point T) (struct x :T y :T))";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::StructDef(struct_def) => {
                assert_eq!(struct_def.name, "Point");
                assert_eq!(struct_def.params.len(), 1);
                assert_eq!(struct_def.params[0], "T");
                assert_eq!(struct_def.fields.len(), 2);
                assert_eq!(struct_def.fields[0].0, "x");
                assert_eq!(struct_def.fields[1].0, "y");
            }
            _ => panic!("Expected struct definition"),
        }
    }

    #[test]
    fn test_struct_definitions_with_split_type_annotations() {
        let source = "(def (Point T) (struct x : T y : T))";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::StructDef(struct_def) => {
                assert_eq!(struct_def.name, "Point");
                assert_eq!(struct_def.fields.len(), 2);
            }
            _ => panic!("Expected struct definition"),
        }
    }

    #[test]
    fn test_variable_definitions() {
        let source = "(def x 42)";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::VarDef(name, expr) => {
                assert_eq!(name, "x");
                assert!(matches!(expr, Expr::Literal(Literal::Int(42))));
            }
            _ => panic!("Expected variable definition"),
        }
    }

    #[test]
    fn test_variable_definitions_with_type_annotations() {
        let source = "(def x :int 42)";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::VarDef(name, expr) => {
                assert_eq!(name, "x");
                assert!(matches!(expr, Expr::Literal(Literal::Int(42))));
            }
            _ => panic!("Expected variable definition"),
        }

        let source = "(def x : int 42)";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::VarDef(name, expr) => {
                assert_eq!(name, "x");
                assert!(matches!(expr, Expr::Literal(Literal::Int(42))));
            }
            _ => panic!("Expected variable definition"),
        }
    }

    #[test]
    fn test_module_definitions_with_provides_require() {
        let source = r#"
            (provides add subtract)
            (require "math")
            (fun (add x :int y :int) :int (+ x y))
            (fun (subtract x :int y :int) :int (- x y))
        "#;
        let module = parse(source).unwrap();
        
        assert_eq!(module.provides.len(), 2);
        assert_eq!(module.provides[0], "add");
        assert_eq!(module.provides[1], "subtract");
        
        assert_eq!(module.requires.len(), 1);
        assert_eq!(module.requires[0], "math");
        
        assert_eq!(module.body.len(), 2);
        assert!(matches!(module.body[0], TopLevel::FunDef(_)));
        assert!(matches!(module.body[1], TopLevel::FunDef(_)));
    }

    // --- Error Handling Tests ---

    #[test]
    fn test_invalid_syntax_handling() {
        // Unclosed parenthesis
        let result = parse("(+ 1 2");
        assert!(result.is_err());

        // Invalid function definition
        let result = parse("(fun)");
        assert!(result.is_err());

        // Invalid let binding
        let result = parse("(let (x) x)");
        assert!(result.is_err());
    }

    #[test]
    fn test_malformed_expressions() {
        // Empty list - this might actually parse successfully as an empty module
        // Let's test a different malformed expression
        let result = parse("(");
        assert!(result.is_err());

        // Invalid if expression (wrong number of arguments)
        let result = parse("(if true)");
        assert!(result.is_err());

        let result = parse("(if true 1)");
        assert!(result.is_err());

        // Invalid let binding format
        let result = parse("(let ((x)) x)");
        assert!(result.is_err());
    }

    #[test]
    fn test_type_annotation_parsing_errors() {
        // Invalid function parameter format
        let result = parse("(fun (add x) (+ x 1))");
        assert!(result.is_err());

        // Invalid struct field format
        let result = parse("(def (Point) (struct x))");
        assert!(result.is_err());
    }

    // --- Integration Tests ---

    #[test]
    fn test_parse_complete_valid_programs() {
        let source = r#"
            (provides factorial fibonacci)
            (require "math")
            
            (def PI :float 3.14159)
            
            (fun (factorial n :int) :int
                (if (= n 0)
                    1
                    (* n (factorial (- n 1)))))
            
            (fun (fibonacci n :int) :int
                (if (< n 2)
                    n
                    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
            
            (def (Point T) (struct x :T y :T))
        "#;
        
        let module = parse(source).unwrap();
        
        assert_eq!(module.provides.len(), 2);
        assert_eq!(module.requires.len(), 1);
        assert_eq!(module.body.len(), 4); // PI, factorial, fibonacci, Point
        
        // Check that we have the expected definitions
        let mut var_defs = 0;
        let mut fun_defs = 0;
        let mut struct_defs = 0;
        
        for item in &module.body {
            match item {
                TopLevel::VarDef(_, _) => var_defs += 1,
                TopLevel::FunDef(_) => fun_defs += 1,
                TopLevel::StructDef(_) => struct_defs += 1,
                _ => {}
            }
        }
        
        assert_eq!(var_defs, 1);
        assert_eq!(fun_defs, 2);
        assert_eq!(struct_defs, 1);
    }

    #[test]
    fn test_parse_examples_from_language_specification() {
        // Basic arithmetic
        let source = "(+ (* 2 3) (/ 8 4))";
        let module = parse(source).unwrap();
        assert_eq!(module.body.len(), 1);

        // Conditional logic
        let source = "(if (> x 0) (print \"positive\") (print \"non-positive\"))";
        let module = parse(source).unwrap();
        assert_eq!(module.body.len(), 1);

        // Function with multiple parameters
        let source = "(fun (distance p1 :(Point float) p2 :(Point float)) :float (sqrt (+ (square (- (get p2 x) (get p1 x))) (square (- (get p2 y) (get p1 y))))))";
        let module = parse(source).unwrap();
        assert_eq!(module.body.len(), 1);
    }

    #[test]
    fn test_string_parsing() {
        let source = r#""hello world""#;
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::Expr(expr) => {
                // String should be parsed as a series of cons calls
                assert!(matches!(expr, Expr::Call { .. }));
            }
            _ => panic!("Expected expression"),
        }
    }

    #[test]
    fn test_quote_expressions() {
        let source = "(quote (1 2 3))";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::Expr(Expr::Quote(_)) => {}, // Success
            _ => panic!("Expected quote expression"),
        }
    }

    #[test]
    fn test_function_expressions() {
        // Test that function expressions can be parsed when used in context
        // For now, we'll test that function definitions work correctly
        // since the distinction between function expressions and definitions
        // depends on context in the parser
        let source = "(fun (identity x :int) :int x)";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::FunDef(fun_def) => {
                assert_eq!(fun_def.name, "identity");
                assert_eq!(fun_def.params.len(), 1);
                assert_eq!(fun_def.return_type, Type::Int);
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_complex_type_parsing() {
        // Option type
        let source = "(def x :(Option int) none)";
        let module = parse(source).unwrap();
        assert_eq!(module.body.len(), 1);

        // Function type
        let source = "(def f :(fun-type (int int) bool) some-function)";
        let module = parse(source).unwrap();
        assert_eq!(module.body.len(), 1);

        // Generic type
        let source = "(def (Container T) (struct value :T))";
        let module = parse(source).unwrap();
        assert_eq!(module.body.len(), 1);
    }

    #[test]
    fn test_pattern_parsing() {
        // Simple identifier pattern
        let source = "(let ((x 10)) x)";
        let module = parse(source).unwrap();
        match &module.body[0] {
            TopLevel::Expr(Expr::Let { bindings, .. }) => {
                assert!(matches!(bindings[0].0, Pattern::Identifier(_)));
            }
            _ => panic!("Expected let expression"),
        }
    }

    #[test]
    fn test_nested_function_calls() {
        let source = "(foo (bar (baz 1 2) 3) 4)";
        let module = parse(source).unwrap();
        
        match &module.body[0] {
            TopLevel::Expr(Expr::Call { function, arguments }) => {
                assert!(matches!(function.as_ref(), Expr::Identifier(_)));
                assert_eq!(arguments.len(), 2);
                assert!(matches!(arguments[0], Expr::Call { .. }));
            }
            _ => panic!("Expected nested function calls"),
        }
    }

    #[test]
    fn test_multiple_top_level_definitions() {
        let source = r#"
            (def x 10)
            (def y 20)
            (fun (add a :int b :int) :int (+ a b))
            (add x y)
        "#;
        
        let module = parse(source).unwrap();
        assert_eq!(module.body.len(), 4);
        
        assert!(matches!(module.body[0], TopLevel::VarDef(_, _)));
        assert!(matches!(module.body[1], TopLevel::VarDef(_, _)));
        assert!(matches!(module.body[2], TopLevel::FunDef(_)));
        assert!(matches!(module.body[3], TopLevel::Expr(_)));
    }

    #[test]
    fn test_empty_module() {
        let source = "";
        let module = parse(source).unwrap();
        
        assert_eq!(module.provides.len(), 0);
        assert_eq!(module.requires.len(), 0);
        assert_eq!(module.body.len(), 0);
    }

    #[test]
    fn test_whitespace_and_comments_handling() {
        // Note: This test assumes the lexpr parser handles comments
        let source = r#"
            ; This is a comment
            (def x 42)  ; Another comment
            
            ; More comments
            (+ x 1)
        "#;
        
        let module = parse(source).unwrap();
        assert_eq!(module.body.len(), 2);
    }
}