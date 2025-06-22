use crate::ast::{self, Environment, Expr, Literal, Pattern, Value};
use crate::stdlib;
use std::sync::Arc;

pub struct Interpreter {
    env: Arc<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new(None);
        stdlib::populate_env(&mut env);
        Interpreter {
            env: Arc::new(env),
        }
    }

    pub fn eval(&self, expr: &Expr) -> Result<Value, String> {
        self.eval_with_env(expr, Arc::clone(&self.env))
    }

    fn eval_with_env(&self, expr: &Expr, env: Arc<Environment>) -> Result<Value, String> {
        match expr {
            Expr::Literal(lit) => Ok(Interpreter::eval_literal(lit)),
            Expr::Identifier(name) => self.eval_identifier(name, env),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => self.eval_if(condition, then_branch, else_branch, env),
            Expr::Let {
                bindings,
                body,
                sequential,
            } => self.eval_let(bindings, body, *sequential, env),
            Expr::Function {
                params,
                return_type: _,
                body,
            } => {
                let function_value = ast::FunctionValue {
                    name: None,
                    params: params.clone(),
                    body: *body.clone(),
                    captured_env: env,
                };
                Ok(Value::Function(Arc::new(function_value)))
            }
            Expr::Call {
                function,
                arguments,
            } => self.eval_call(function, arguments, env),
        }
    }

    fn eval_literal(lit: &Literal) -> Value {
        match lit {
            Literal::Int(i) => Value::Number(*i as f64),
            Literal::Float(f) => Value::Number(*f),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Char(c) => Value::Char(*c),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Nil => Value::Nil,
        }
    }

    fn eval_identifier(&self, name: &str, env: Arc<Environment>) -> Result<Value, String> {
        env.get(name).ok_or_else(|| format!("Unbound variable: {}", name))
    }

    fn eval_if(
        &self,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: &Expr,
        env: Arc<Environment>,
    ) -> Result<Value, String> {
        let cond_val = self.eval_with_env(condition, Arc::clone(&env))?;
        match cond_val {
            Value::Bool(true) => self.eval_with_env(then_branch, env),
            Value::Bool(false) => self.eval_with_env(else_branch, env),
            _ => Err("If condition must be a boolean".to_string()),
        }
    }

    fn eval_let(
        &self,
        bindings: &[(Pattern, Expr)],
        body: &Expr,
        sequential: bool,
        env: Arc<Environment>,
    ) -> Result<Value, String> {
        let mut new_env = Environment::new(Some(Arc::clone(&env)));
        if sequential {
            for (pattern, expr) in bindings {
                let value = self.eval_with_env(expr, Arc::new(new_env.clone()))?;
                self.bind_pattern(&mut new_env, pattern, value)?;
            }
        } else {
            let mut new_bindings = Vec::new();
            for (pattern, expr) in bindings {
                let val = self.eval_with_env(expr, Arc::clone(&env))?;
                new_bindings.push((pattern.clone(), val));
            }
            for (pattern, value) in new_bindings {
                self.bind_pattern(&mut new_env, &pattern, value)?;
            }
        }
        self.eval_with_env(body, Arc::new(new_env))
    }

    fn bind_pattern(
        &self,
        env: &mut Environment,
        pattern: &Pattern,
        value: Value,
    ) -> Result<(), String> {
        match (pattern, value) {
            (Pattern::Identifier(name), val) => {
                env.define(name.clone(), val);
                Ok(())
            }
            (
                Pattern::Struct {
                    name: pat_name,
                    fields: pat_fields,
                },
                Value::Struct(struct_val),
            ) => {
                if &struct_val.name != pat_name {
                    return Err(format!(
                        "Mismatched struct types in pattern matching: expected {}, got {}",
                        pat_name, struct_val.name
                    ));
                }

                if pat_fields.len() != struct_val.fields.len() {
                    return Err("Mismatched number of fields in struct pattern".to_string());
                }

                for field_name in pat_fields {
                    if let Some(field_val) = struct_val.fields.get(field_name) {
                        env.define(field_name.clone(), field_val.clone());
                    } else {
                        return Err(format!(
                            "Field '{}' not found in struct for pattern matching",
                            field_name
                        ));
                    }
                }
                Ok(())
            }
            _ => Err("Pattern did not match value".to_string()),
        }
    }

    fn eval_call(
        &self,
        function_expr: &Expr,
        arg_exprs: &[Expr],
        env: Arc<Environment>,
    ) -> Result<Value, String> {
        let func_val = self.eval_with_env(function_expr, Arc::clone(&env))?;

        match func_val {
            Value::Function(func) => {
                if func.params.len() != arg_exprs.len() {
                    return Err(format!(
                        "Expected {} arguments, but got {}",
                        func.params.len(),
                        arg_exprs.len()
                    ));
                }

                let mut call_env = Environment::new(Some(func.captured_env.clone()));

                for ((pattern, _), arg_expr) in func.params.iter().zip(arg_exprs) {
                    let arg_val = self.eval_with_env(arg_expr, Arc::clone(&env))?;
                    self.bind_pattern(&mut call_env, pattern, arg_val)?;
                }

                self.eval_with_env(&func.body, Arc::new(call_env))
            }
            Value::NativeFunc(native_fn) => {
                let mut args = Vec::new();
                for arg_expr in arg_exprs {
                    let arg_val = self.eval_with_env(arg_expr, Arc::clone(&env))?;
                    args.push(arg_val);
                }
                let result = native_fn(args);
                if let Value::Error(e) = result {
                    Err(e)
                } else {
                    Ok(result)
                }
            }
            _ => Err("Expression is not a function and cannot be called".to_string()),
        }
    }
}