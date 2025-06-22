use std::collections::HashMap;
use std::sync::Arc;

// --- Runtime Values ---

/// Represents a runtime value in the Elang interpreter.
/// Heap-allocated values are wrapped in an `Arc` for shared ownership,
/// as specified by the memory model.
#[derive(Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Char(char),
    String(String),
    /// Represents the 'none' value, used with `(Option T)`.
    Nil,
    /// A struct instance.
    Struct(Arc<StructInstance>),
    /// A first-class function, which includes its captured environment (a closure).
    Function(Arc<FunctionValue>),
    NativeFunc(fn(Vec<Value>) -> Value),
    Error(String),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "Number({})", n),
            Value::Bool(b) => write!(f, "Bool({})", b),
            Value::Char(c) => write!(f, "Char({})", c),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Nil => write!(f, "Nil"),
            Value::Struct(s) => write!(f, "Struct({:?})", s),
            Value::Function(_) => write!(f, "Function"),
            Value::NativeFunc(_) => write!(f, "NativeFunc"),
            Value::Error(e) => write!(f, "Error({})", e),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "{}", c),
            Value::String(s) => write!(f, "{}", s),
            Value::Nil => write!(f, "nil"),
            Value::Struct(s) => write!(f, "<struct {}>", s.name),
            Value::Function(_) => write!(f, "<function>"),
            Value::NativeFunc(_) => write!(f, "<native-function>"),
            Value::Error(e) => write!(f, "Error: {}", e),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

/// Represents a runtime instance of a struct.
#[derive(Debug, Clone)]
pub struct StructInstance {
    /// The name of the struct definition (e.g., "vec3").
    pub name: String,
    pub fields: HashMap<String, Value>,
}

/// Represents a closure: a function with its captured lexical environment.
#[derive(Clone)]
pub struct FunctionValue {
    pub name: Option<String>,
    pub params: Vec<(Pattern, Type)>,
    pub body: Expr,
    pub captured_env: Arc<Environment>,
}

impl std::fmt::Debug for FunctionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionValue")
            .field("name", &self.name)
            .field("params", &self.params)
            .field("body", &self.body)
            .field("captured_env", &"...")
            .finish()
    }
}

// --- Environment ---

#[derive(Debug, Clone)]
pub struct Environment {
    pub bindings: HashMap<String, Value>,
    pub parent: Option<Arc<Environment>>,
}

impl Environment {
    pub fn new(parent: Option<Arc<Environment>>) -> Self {
        Environment {
            bindings: HashMap::new(),
            parent,
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.get(name) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }
}


// --- AST (Abstract Syntax Tree) Nodes ---

/// Represents an expression that can be evaluated to a `Value`.
#[derive(Debug, Clone)]
pub enum Expr {
    // Atoms
    Literal(Literal),
    Identifier(String),

    // Control Flow
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },

    // Bindings
    Let {
        bindings: Vec<(Pattern, Expr)>,
        body: Box<Expr>,
        /// true for `let*` (sequential), false for `let` (parallel).
        sequential: bool,
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
    String(String),
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
}

/// Represents a type annotation in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    Bool,
    Char,
    Void,
    /// A user-defined type, like `string` or `(vec3 float)`.
    Named {
        name: String,
        type_params: Vec<Type>,
    },
    /// A function's type signature, e.g., `(fun-type (int) bool)`.
    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
    /// The built-in `(Option T)` type.
    Option(Box<Type>),
}