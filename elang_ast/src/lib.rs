//! Core AST definitions for the Elang programming language.
//! 
//! This crate contains the fundamental language constructs that are shared
//! between the interpreter and other tools like the LSP server. It includes
//! type definitions, expressions, patterns, and module structures, but excludes
//! runtime-specific components like values and environments.

// --- AST (Abstract Syntax Tree) Nodes ---

#[derive(Debug, Clone)]
pub enum TopLevel {
    Expr(Expr),
    StructDef(StructDef),
    VarDef(String, Expr),
    FunDef(FunDef),
    Provides(Vec<String>),
    Require(String),
}

#[derive(Debug, Clone)]
pub struct Module {
    pub provides: Vec<String>,
    pub requires: Vec<String>,
    pub body: Vec<TopLevel>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub params: Vec<String>,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct FunDef {
    pub name: String,
    pub params: Vec<(Pattern, Type)>,
    pub return_type: Type,
    pub body: Expr,
}

/// Represents an expression that can be evaluated to a `Value`.
#[derive(Debug, Clone)]
pub enum Expr {
    // Atoms
    Literal(Literal),
    Identifier(String),

    // Control Flow
    Quote(lexpr::Value),
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    IfLet {
        identifier: String,
        expr: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    // Bindings
    Let {
        bindings: Vec<(Pattern, Expr)>,
        body: Box<Expr>,
    },

    LetStar {
        bindings: Vec<(Pattern, Expr)>,
        body: Box<Expr>,
    },

    // Operations
    Call {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
    
    // A function definition expression (lambda).
    Function {
        params: Vec<(Pattern, Type)>,
        return_type: Type,
        body: Box<Expr>,
    },
}

/// Represents a literal value in the source code.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Nil,
}

/// A destructuring pattern used in `let` bindings.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Identifier(String),
    Struct {
        name: String,
        fields: Vec<String>,
    },
    List(Vec<Pattern>),
}

/// Represents a type annotation in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    Bool,
    Char,
    Void,
    GenericParam(String),
    /// A user-defined type, like `string` or `(vec3 float)`.
    Named {
        name: String,
        type_params: Vec<Type>,
    },
    /// A function's type signature, e.g., `(fun-type (int) bool)`.
    Func {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
    /// The built-in `(Option T)` type.
    Option(Box<Type>),
}

impl TopLevel {
    pub fn as_expr(&self) -> Option<&Expr> {
        match self {
            TopLevel::Expr(expr) => Some(expr),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Type System Tests ---

    #[test]
    fn test_basic_type_creation_and_equality() {
        assert_eq!(Type::Int, Type::Int);
        assert_eq!(Type::Float, Type::Float);
        assert_eq!(Type::Bool, Type::Bool);
        assert_eq!(Type::Char, Type::Char);
        assert_eq!(Type::Void, Type::Void);
        
        assert_ne!(Type::Int, Type::Float);
        assert_ne!(Type::Bool, Type::Char);
    }

    #[test]
    fn test_generic_type_construction() {
        let generic_t = Type::GenericParam("T".to_string());
        let generic_u = Type::GenericParam("U".to_string());
        
        assert_eq!(generic_t, Type::GenericParam("T".to_string()));
        assert_ne!(generic_t, generic_u);
    }

    #[test]
    fn test_struct_type_creation() {
        let vec2_type = Type::Named {
            name: "vec2".to_string(),
            type_params: vec![Type::Float],
        };
        
        let vec3_type = Type::Named {
            name: "vec3".to_string(),
            type_params: vec![Type::Float],
        };
        
        assert_eq!(vec2_type, Type::Named {
            name: "vec2".to_string(),
            type_params: vec![Type::Float],
        });
        assert_ne!(vec2_type, vec3_type);
    }

    #[test]
    fn test_option_type_handling() {
        let int_option = Type::Option(Box::new(Type::Int));
        let float_option = Type::Option(Box::new(Type::Float));
        
        assert_eq!(int_option, Type::Option(Box::new(Type::Int)));
        assert_ne!(int_option, float_option);
    }

    #[test]
    fn test_function_type_creation() {
        let func_type = Type::Func {
            param_types: vec![Type::Int, Type::Float],
            return_type: Box::new(Type::Bool),
        };
        
        let same_func_type = Type::Func {
            param_types: vec![Type::Int, Type::Float],
            return_type: Box::new(Type::Bool),
        };
        
        let different_func_type = Type::Func {
            param_types: vec![Type::Int],
            return_type: Box::new(Type::Bool),
        };
        
        assert_eq!(func_type, same_func_type);
        assert_ne!(func_type, different_func_type);
    }

    #[test]
    fn test_type_display_formatting() {
        // Test Debug formatting works
        let int_type = Type::Int;
        let debug_str = format!("{:?}", int_type);
        assert!(debug_str.contains("Int"));
        
        let complex_type = Type::Named {
            name: "vec2".to_string(),
            type_params: vec![Type::GenericParam("T".to_string())],
        };
        let complex_debug = format!("{:?}", complex_type);
        assert!(complex_debug.contains("vec2"));
        assert!(complex_debug.contains("T"));
    }

    // --- AST Node Tests ---

    #[test]
    fn test_literal_variants() {
        let int_lit = Literal::Int(42);
        let float_lit = Literal::Float(3.14);
        let bool_lit = Literal::Bool(true);
        let char_lit = Literal::Char('a');
        let nil_lit = Literal::Nil;
        
        assert_eq!(int_lit, Literal::Int(42));
        assert_eq!(float_lit, Literal::Float(3.14));
        assert_eq!(bool_lit, Literal::Bool(true));
        assert_eq!(char_lit, Literal::Char('a'));
        assert_eq!(nil_lit, Literal::Nil);
        
        assert_ne!(int_lit, Literal::Int(43));
        assert_ne!(bool_lit, Literal::Bool(false));
    }

    #[test]
    fn test_expr_construction() {
        // Test identifier
        let ident = Expr::Identifier("x".to_string());
        match ident {
            Expr::Identifier(name) => assert_eq!(name, "x"),
            _ => panic!("Expected identifier"),
        }
        
        // Test literal
        let lit_expr = Expr::Literal(Literal::Int(42));
        match lit_expr {
            Expr::Literal(Literal::Int(n)) => assert_eq!(n, 42),
            _ => panic!("Expected literal int"),
        }
        
        // Test function call
        let call = Expr::Call {
            function: Box::new(Expr::Identifier("add".to_string())),
            arguments: vec![
                Expr::Literal(Literal::Int(1)),
                Expr::Literal(Literal::Int(2)),
            ],
        };
        
        match call {
            Expr::Call { function, arguments } => {
                assert!(matches!(function.as_ref(), Expr::Identifier(_)));
                assert_eq!(arguments.len(), 2);
            }
            _ => panic!("Expected function call"),
        }
    }

    #[test]
    fn test_if_expression() {
        let if_expr = Expr::If {
            condition: Box::new(Expr::Literal(Literal::Bool(true))),
            then_branch: Box::new(Expr::Literal(Literal::Int(1))),
            else_branch: Box::new(Expr::Literal(Literal::Int(2))),
        };
        
        match if_expr {
            Expr::If { condition, then_branch, else_branch } => {
                assert!(matches!(condition.as_ref(), Expr::Literal(Literal::Bool(true))));
                assert!(matches!(then_branch.as_ref(), Expr::Literal(Literal::Int(1))));
                assert!(matches!(else_branch.as_ref(), Expr::Literal(Literal::Int(2))));
            }
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_let_bindings() {
        let let_expr = Expr::Let {
            bindings: vec![
                (Pattern::Identifier("x".to_string()), Expr::Literal(Literal::Int(42))),
                (Pattern::Identifier("y".to_string()), Expr::Literal(Literal::Float(3.14))),
            ],
            body: Box::new(Expr::Identifier("x".to_string())),
        };
        
        match let_expr {
            Expr::Let { bindings, body } => {
                assert_eq!(bindings.len(), 2);
                assert!(matches!(bindings[0].0, Pattern::Identifier(_)));
                assert!(matches!(body.as_ref(), Expr::Identifier(_)));
            }
            _ => panic!("Expected let expression"),
        }
    }

    #[test]
    fn test_function_expression() {
        let func_expr = Expr::Function {
            params: vec![
                (Pattern::Identifier("x".to_string()), Type::Int),
                (Pattern::Identifier("y".to_string()), Type::Float),
            ],
            return_type: Type::Bool,
            body: Box::new(Expr::Literal(Literal::Bool(true))),
        };
        
        match func_expr {
            Expr::Function { params, return_type, body } => {
                assert_eq!(params.len(), 2);
                assert_eq!(return_type, Type::Bool);
                assert!(matches!(body.as_ref(), Expr::Literal(Literal::Bool(true))));
            }
            _ => panic!("Expected function expression"),
        }
    }

    // --- Pattern Tests ---

    #[test]
    fn test_pattern_identifier() {
        let pattern = Pattern::Identifier("x".to_string());
        assert_eq!(pattern, Pattern::Identifier("x".to_string()));
        assert_ne!(pattern, Pattern::Identifier("y".to_string()));
    }

    #[test]
    fn test_pattern_struct_destructuring() {
        let struct_pattern = Pattern::Struct {
            name: "Point".to_string(),
            fields: vec!["x".to_string(), "y".to_string()],
        };
        
        match struct_pattern {
            Pattern::Struct { name, fields } => {
                assert_eq!(name, "Point");
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0], "x");
                assert_eq!(fields[1], "y");
            }
            _ => panic!("Expected struct pattern"),
        }
    }

    #[test]
    fn test_pattern_list() {
        let list_pattern = Pattern::List(vec![
            Pattern::Identifier("head".to_string()),
            Pattern::Identifier("tail".to_string()),
        ]);
        
        match list_pattern {
            Pattern::List(patterns) => {
                assert_eq!(patterns.len(), 2);
                assert!(matches!(patterns[0], Pattern::Identifier(_)));
                assert!(matches!(patterns[1], Pattern::Identifier(_)));
            }
            _ => panic!("Expected list pattern"),
        }
    }

    // --- TopLevel Tests ---

    #[test]
    fn test_toplevel_expr() {
        let expr = Expr::Literal(Literal::Int(42));
        let toplevel = TopLevel::Expr(expr.clone());
        
        assert!(toplevel.as_expr().is_some());
        match toplevel.as_expr().unwrap() {
            Expr::Literal(Literal::Int(n)) => assert_eq!(*n, 42),
            _ => panic!("Expected literal int"),
        }
    }

    #[test]
    fn test_toplevel_var_def() {
        let var_def = TopLevel::VarDef(
            "x".to_string(),
            Expr::Literal(Literal::Int(42)),
        );
        
        match var_def {
            TopLevel::VarDef(name, expr) => {
                assert_eq!(name, "x");
                assert!(matches!(expr, Expr::Literal(Literal::Int(42))));
            }
            _ => panic!("Expected variable definition"),
        }
    }

    #[test]
    fn test_toplevel_fun_def() {
        let fun_def = TopLevel::FunDef(FunDef {
            name: "add".to_string(),
            params: vec![
                (Pattern::Identifier("x".to_string()), Type::Int),
                (Pattern::Identifier("y".to_string()), Type::Int),
            ],
            return_type: Type::Int,
            body: Expr::Call {
                function: Box::new(Expr::Identifier("+".to_string())),
                arguments: vec![
                    Expr::Identifier("x".to_string()),
                    Expr::Identifier("y".to_string()),
                ],
            },
        });
        
        match fun_def {
            TopLevel::FunDef(def) => {
                assert_eq!(def.name, "add");
                assert_eq!(def.params.len(), 2);
                assert_eq!(def.return_type, Type::Int);
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_toplevel_struct_def() {
        let struct_def = TopLevel::StructDef(StructDef {
            name: "Point".to_string(),
            params: vec!["T".to_string()],
            fields: vec![
                ("x".to_string(), Type::GenericParam("T".to_string())),
                ("y".to_string(), Type::GenericParam("T".to_string())),
            ],
        });
        
        match struct_def {
            TopLevel::StructDef(def) => {
                assert_eq!(def.name, "Point");
                assert_eq!(def.params.len(), 1);
                assert_eq!(def.fields.len(), 2);
            }
            _ => panic!("Expected struct definition"),
        }
    }

    #[test]
    fn test_toplevel_provides_require() {
        let provides = TopLevel::Provides(vec!["func1".to_string(), "func2".to_string()]);
        let require = TopLevel::Require("math".to_string());
        
        match provides {
            TopLevel::Provides(names) => {
                assert_eq!(names.len(), 2);
                assert_eq!(names[0], "func1");
                assert_eq!(names[1], "func2");
            }
            _ => panic!("Expected provides"),
        }
        
        match require {
            TopLevel::Require(path) => assert_eq!(path, "math"),
            _ => panic!("Expected require"),
        }
    }

    // --- Module Tests ---

    #[test]
    fn test_module_creation() {
        let module = Module {
            provides: vec!["add".to_string(), "subtract".to_string()],
            requires: vec!["math".to_string()],
            body: vec![
                TopLevel::FunDef(FunDef {
                    name: "add".to_string(),
                    params: vec![
                        (Pattern::Identifier("x".to_string()), Type::Int),
                        (Pattern::Identifier("y".to_string()), Type::Int),
                    ],
                    return_type: Type::Int,
                    body: Expr::Identifier("dummy".to_string()),
                }),
                TopLevel::VarDef("PI".to_string(), Expr::Literal(Literal::Float(3.14))),
            ],
        };
        
        assert_eq!(module.provides.len(), 2);
        assert_eq!(module.requires.len(), 1);
        assert_eq!(module.body.len(), 2);
        assert_eq!(module.provides[0], "add");
        assert_eq!(module.requires[0], "math");
    }

    #[test]
    fn test_struct_def_construction() {
        let struct_def = StructDef {
            name: "Vec2".to_string(),
            params: vec!["T".to_string()],
            fields: vec![
                ("x".to_string(), Type::GenericParam("T".to_string())),
                ("y".to_string(), Type::GenericParam("T".to_string())),
            ],
        };
        
        assert_eq!(struct_def.name, "Vec2");
        assert_eq!(struct_def.params.len(), 1);
        assert_eq!(struct_def.params[0], "T");
        assert_eq!(struct_def.fields.len(), 2);
        assert_eq!(struct_def.fields[0].0, "x");
        assert_eq!(struct_def.fields[1].0, "y");
    }

    #[test]
    fn test_fun_def_construction() {
        let fun_def = FunDef {
            name: "distance".to_string(),
            params: vec![
                (Pattern::Identifier("p1".to_string()), Type::Named {
                    name: "Point".to_string(),
                    type_params: vec![Type::Float],
                }),
                (Pattern::Identifier("p2".to_string()), Type::Named {
                    name: "Point".to_string(),
                    type_params: vec![Type::Float],
                }),
            ],
            return_type: Type::Float,
            body: Expr::Literal(Literal::Float(0.0)),
        };
        
        assert_eq!(fun_def.name, "distance");
        assert_eq!(fun_def.params.len(), 2);
        assert_eq!(fun_def.return_type, Type::Float);
        assert!(matches!(fun_def.body, Expr::Literal(Literal::Float(_))));
    }

    // --- Complex Integration Tests ---

    #[test]
    fn test_nested_expressions() {
        let nested_expr = Expr::Let {
            bindings: vec![
                (Pattern::Identifier("x".to_string()), Expr::Literal(Literal::Int(10))),
            ],
            body: Box::new(Expr::If {
                condition: Box::new(Expr::Call {
                    function: Box::new(Expr::Identifier(">".to_string())),
                    arguments: vec![
                        Expr::Identifier("x".to_string()),
                        Expr::Literal(Literal::Int(5)),
                    ],
                }),
                then_branch: Box::new(Expr::Literal(Literal::Bool(true))),
                else_branch: Box::new(Expr::Literal(Literal::Bool(false))),
            }),
        };
        
        match nested_expr {
            Expr::Let { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                match body.as_ref() {
                    Expr::If { .. } => {}, // Success
                    _ => panic!("Expected if expression in let body"),
                }
            }
            _ => panic!("Expected let expression"),
        }
    }

    #[test]
    fn test_complex_type_structures() {
        let complex_type = Type::Func {
            param_types: vec![
                Type::Named {
                    name: "List".to_string(),
                    type_params: vec![Type::GenericParam("T".to_string())],
                },
                Type::Func {
                    param_types: vec![Type::GenericParam("T".to_string())],
                    return_type: Box::new(Type::Bool),
                },
            ],
            return_type: Box::new(Type::Option(Box::new(Type::GenericParam("T".to_string())))),
        };
        
        match complex_type {
            Type::Func { param_types, return_type } => {
                assert_eq!(param_types.len(), 2);
                assert!(matches!(param_types[0], Type::Named { .. }));
                assert!(matches!(param_types[1], Type::Func { .. }));
                assert!(matches!(return_type.as_ref(), Type::Option(_)));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_clone_and_debug_traits() {
        let original_expr = Expr::Literal(Literal::Int(42));
        let cloned_expr = original_expr.clone();
        
        // Test that clone works
        match (original_expr, cloned_expr) {
            (Expr::Literal(Literal::Int(a)), Expr::Literal(Literal::Int(b))) => {
                assert_eq!(a, b);
            }
            _ => panic!("Clone failed"),
        }
        
        // Test that Debug works
        let debug_str = format!("{:?}", Expr::Identifier("test".to_string()));
        assert!(debug_str.contains("Identifier"));
        assert!(debug_str.contains("test"));
    }
}