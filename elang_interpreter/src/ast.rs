use std::collections::HashMap;
use std::sync::Arc;

// --- Runtime Values ---

/// Represents a runtime value in the Elang interpreter.
/// Heap-allocated values are wrapped in an `Arc` for shared ownership,
/// as specified by the memory model.
#[derive(Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Optional(Option<Box<Value>>),
    /// A struct instance.
    Struct(Arc<StructInstance>),
    /// A first-class function, which includes its captured environment (a closure).
    Function(Arc<FunctionValue>),
    NativeFunc(fn(Vec<Value>) -> Value),
    NativeClosure(Arc<dyn Fn(Vec<Value>) -> Value + Send + Sync>),
    Error(String),
    GenericStructDef(Arc<StructDef>),
    Symbol(String),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "Int({})", n),
            Value::Float(n) => write!(f, "Float({})", n),
            Value::Bool(b) => write!(f, "Bool({})", b),
            Value::Char(c) => write!(f, "Char({})", c),
            Value::Optional(opt) => write!(f, "Optional({:?})", opt),
            Value::Struct(s) => write!(f, "Struct({:?})", s),
            Value::Function(_) => write!(f, "Function"),
            Value::NativeFunc(_) => write!(f, "NativeFunc"),
            Value::NativeClosure(_) => write!(f, "NativeClosure"),
            Value::Error(e) => write!(f, "Error({})", e),
            Value::GenericStructDef(d) => write!(f, "GenericStructDef({:?})", d),
            Value::Symbol(s) => write!(f, "Symbol({})", s),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "{}", c),
            Value::Optional(opt) => match opt {
                Some(val) => write!(f, "(some {})", val),
                None => write!(f, "none"),
            },
            Value::Struct(s) => write!(f, "<struct {}>", s.name),
            Value::Function(_) => write!(f, "<function>"),
            Value::NativeFunc(_) => write!(f, "<native-function>"),
            Value::NativeClosure(_) => write!(f, "<native-closure>"),
            Value::Error(e) => write!(f, "Error: {}", e),
            Value::GenericStructDef(d) => write!(f, "<generic-struct-def {}>", d.name),
            Value::Symbol(s) => write!(f, "'{}", s),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Optional(a), Value::Optional(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            _ => false,
        }
    }
}

/// Represents a runtime instance of a struct.
#[derive(Debug, Clone)]
pub struct StructInstance {
    /// The name of the struct definition (e.g., "vec3").
    pub name: String,
    pub fields: Vec<(String, Value)>,
}

/// Represents a closure: a function with its captured lexical environment.
#[derive(Clone)]
pub struct FunctionValue {
    pub name: Option<String>,
    pub params: Vec<(Pattern, Type)>,
    pub return_type: Type,
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