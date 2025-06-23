use crate::ast::{self, Environment, Expr, Literal, Pattern, StructDef, TopLevel, Type, Value};
use crate::parser;
use crate::stdlib;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

impl TopLevel {
    pub fn as_expr(&self) -> Option<&Expr> {
        match self {
            TopLevel::Expr(expr) => Some(expr),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct Interpreter {
    pub global_env: Arc<Environment>,
    pub loaded_files: HashSet<PathBuf>,
    pub root_path: PathBuf,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new(None);
        stdlib::populate_env(&mut env);
        Interpreter {
            global_env: Arc::new(env),
            loaded_files: HashSet::new(),
            root_path: PathBuf::new(),
        }
    }

    pub fn new_with_path(path: String) -> Self {
        let mut env = Environment::new(None);
        stdlib::populate_env(&mut env);
        let mut root_path = PathBuf::new();
        root_path.push(path);
        Interpreter {
            global_env: Arc::new(env),
            loaded_files: HashSet::new(),
            root_path,
        }
    }

    pub fn eval_file(&mut self, path: &Path) -> Result<Value, String> {
        let mut main_env = Environment::new(Some(self.global_env.clone()));
        self.load_module(path, &mut main_env)?;

        let source = fs::read_to_string(path).map_err(|e| e.to_string())?;
        let module = parser::parse(&source)?;

        let mut last_val = Value::Optional(None);
        for item in module.body {
            if let Some(expr) = item.as_expr() {
                last_val = self.eval_with_env(&expr, Arc::new(main_env.clone()))?;
            }
        }
        Ok(last_val)
    }

    fn load_module(&mut self, path: &Path, env: &mut Environment) -> Result<(), String> {
        let absolute_path = fs::canonicalize(path).map_err(|e| e.to_string())?;

        if self.loaded_files.contains(&absolute_path) {
            return Ok(());
        }
        self.loaded_files.insert(absolute_path.clone());

        let source = fs::read_to_string(&absolute_path).map_err(|e| e.to_string())?;
        let module = parser::parse(&source)?;

        // First, load all dependencies of this module.
        for require_path_str in module.requires {
            let require_path = self.resolve_path(&absolute_path, &require_path_str)?;
            self.load_module(&require_path, env)?;
        }
        
        let mut module_env = Environment::new(Some(self.global_env.clone()));
        for (name, val) in env.bindings.iter() {
            module_env.define(name.clone(), val.clone());
        }

        // Second, evaluate the definitions in the current module's scope.
        // Only evaluate definitions, not expressions (to prevent side effects)
        for item in &module.body {
            match item {
                TopLevel::StructDef(_) | TopLevel::VarDef(_, _) | TopLevel::FunDef(_) => {
                    eval_def(self, item, &mut module_env)?;
                }
                TopLevel::Expr(_) => {
                    // Skip expressions to prevent side effects during module loading
                }
                _ => {}
            }
        }

        // Third, provide the specified bindings to the parent environment.
        for name in module.provides {
            if let Some(value) = module_env.get(&name) {
                env.define(name, value);
            } else {
                return Err(format!("Module '{}' does not provide binding '{}'", absolute_path.display(), name));
            }
        }

        Ok(())
    }
    
    fn resolve_path(&self, current_path: &Path, require_str: &str) -> Result<PathBuf, String> {
        // If we have a root path set, resolve relative to that
        if !self.root_path.as_os_str().is_empty() {
            let mut new_path = self.root_path.clone();
            new_path.push(require_str);
            Ok(new_path)
        } else {
            // Otherwise resolve relative to the current file's directory
            let current_dir = current_path.parent()
                .ok_or("Cannot determine directory of current file")?;
            Ok(current_dir.join(require_str))
        }
    }

}

/// Evaluates a definition (struct or function) and adds it to the environment.
/// Ignores expressions that are not definitions.
fn eval_def(
    interpreter: &Interpreter,
    item: &TopLevel,
    env: &mut Environment,
) -> Result<Value, String> {
    match item {
        TopLevel::StructDef(def) => {
            env.define(
                def.name.clone(),
                Value::GenericStructDef(Arc::new(def.clone())),
            );
            // Create the generic constructor function
            interpreter.create_generic_constructor(def, env)?;
            Ok(Value::Optional(None))
        }
        TopLevel::VarDef(name, expr) => {
            let value = interpreter.eval_with_env(expr, Arc::new(env.clone()))?;
            env.define(name.clone(), value);
            Ok(Value::Optional(None))
        }
        TopLevel::FunDef(def) => {
            let func = ast::FunctionValue {
                name: Some(def.name.clone()),
                params: def.params.clone(),
                return_type: def.return_type.clone(),
                body: def.body.clone(),
                captured_env: Arc::new(env.clone()),
            };

            let mut func_env = Environment::new(Some(Arc::new(env.clone())));
            func_env.define(def.name.clone(), Value::Function(Arc::new(func.clone())));

            let final_func = Value::Function(Arc::new(func));
            env.define(def.name.clone(), final_func);

            Ok(Value::Optional(None))
        }
        _ => Ok(Value::Optional(None)),
    }
}

impl Interpreter {
    pub fn eval(&mut self, item: &TopLevel) -> Result<Value, String> {
        match item {
            TopLevel::Expr(expr) => self.eval_with_env(expr, self.global_env.clone()),
            TopLevel::Require(path) => {
                let mut env = (*self.global_env).clone();
                // For the main eval, use the root path as the base
                let require_path = if !self.root_path.as_os_str().is_empty() {
                    self.root_path.join(path)
                } else {
                    PathBuf::from(path)
                };
                self.load_module(&require_path, &mut env)?;
                self.global_env = Arc::new(env);
                Ok(Value::Optional(None))
            }
            _ => {
                let mut env = (*self.global_env).clone();
                let res = eval_def(self, item, &mut env)?;
                self.global_env = Arc::new(env);
                Ok(res)
            }
        }
    }

    fn eval_with_env(&self, expr: &Expr, env: Arc<Environment>) -> Result<Value, String> {
        match expr {
            Expr::Literal(lit) => Ok(Interpreter::eval_literal(lit)),
            Expr::Identifier(name) => self.eval_identifier(name, env),
            Expr::Quote(val) => Ok(lexpr_to_value(val)),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => self.eval_if(condition, then_branch, else_branch, env),
            Expr::IfLet {
                identifier,
                expr,
                then_branch,
                else_branch,
            } => self.eval_if_let(identifier, expr, then_branch, else_branch, env),
            Expr::Let {
                bindings,
                body,
            } => self.eval_let(bindings, body, env),
            Expr::LetStar {
                bindings,
                body,
            } => self.eval_let_star(bindings, body, env),
            Expr::Function {
                params,
                return_type,
                body,
            } => {
                let function_value = ast::FunctionValue {
                    name: None,
                    params: params.clone(),
                    return_type: return_type.clone(),
                    body: *body.clone(),
                    captured_env: Arc::clone(&env),
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
            Literal::Int(i) => Value::Int(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Char(c) => Value::Char(*c),
            Literal::Nil => Value::Optional(None),
        }
    }

    fn eval_identifier(&self, name: &str, env: Arc<Environment>) -> Result<Value, String> {
        if let Some(value) = env.get(name) {
            return Ok(value);
        }

        if let Some((struct_name, type_args_str)) = name.split_once('-') {
            if let Some(Value::GenericStructDef(def)) = env.get(struct_name) {
                let type_args: Vec<_> = type_args_str.split('-').collect();
                if type_args.len() == def.params.len() {
                    let mut type_map = HashMap::new();
                    for (param_name, arg_name) in def.params.iter().zip(type_args) {
                        let arg_type = self
                            .resolve_type_by_name(arg_name, &env)?
                            .ok_or(format!("Cannot find type '{}'", arg_name))?;
                        type_map.insert(param_name.clone(), arg_type);
                    }
                    self.monomorphize_struct(&def, &type_map, env.clone()).map(|_| Value::Optional(None))
                } else {
                    Err(format!(
                        "Mismatched number of type arguments for {}",
                        name
                    ))
                }
            } else {
                Err(format!("Unbound variable: {}", name))
            }
        } else {
            Err(format!("Unbound variable: {}", name))
        }
    }

    fn monomorphize_struct(
        &self,
        def: &StructDef,
        type_map: &HashMap<String, Type>,
        env: Arc<Environment>,
    ) -> Result<Value, String> {
        let concrete_name = self.monomorphize_name(&def.name, &def.params, type_map);

        if let Some(val) = env.get(&format!("make-{}", concrete_name)) {
            return Ok(val);
        }

        let mut concrete_fields = Vec::new();
        for (field_name, field_type) in &def.fields {
            concrete_fields.push((field_name.clone(), self.substitute_type(field_type, type_map)?));
        }

        let concrete_fields = Arc::new(concrete_fields);
        let concrete_name_arc = Arc::new(concrete_name.clone());
        let constructor_fields = Arc::clone(&concrete_fields);
        let constructor_name_for_closure = Arc::clone(&concrete_name_arc);
        let constructor = Value::NativeClosure(Arc::new(move |args: Vec<Value>| {
            if args.len() != constructor_fields.len() {
                return Value::Error(format!(
                    "Constructor for {} expects {} arguments, got {}",
                    constructor_name_for_closure,
                    constructor_fields.len(),
                    args.len()
                ));
            }
            let fields = constructor_fields
                .iter()
                .zip(args)
                .map(|((name, _), val)| (name.clone(), val))
                .collect::<Vec<_>>();

            Value::Struct(Arc::new(ast::StructInstance {
                name: (*constructor_name_for_closure).clone(),
                fields,
            }))
        }));

        let mut mut_env = (*env).clone();
        let constructor_name = format!("make-{}", concrete_name);
        mut_env.define(constructor_name.clone(), constructor);

        let accessors = self.create_accessors(&concrete_name, &concrete_fields, &mut mut_env);
        for (name, func) in accessors {
            mut_env.define(name, func);
        }
 
        Ok(Value::Optional(None))
    }

    fn create_generic_constructor(
        &self,
        def: &StructDef,
        env: &mut Environment,
    ) -> Result<(), String> {
        let constructor_name = format!("make-{}", def.name);
        
        // Check if constructor already exists
        if env.get(&constructor_name).is_some() {
            return Ok(());
        }

        let def_clone = def.clone();
        let env_clone = Arc::new(env.clone());
        let constructor = Value::NativeClosure(Arc::new(move |args: Vec<Value>| {
            if args.len() != def_clone.fields.len() {
                return Value::Error(format!(
                    "Constructor for {} expects {} arguments, got {}",
                    def_clone.name,
                    def_clone.fields.len(),
                    args.len()
                ));
            }

            // Perform type deduction based on the argument types
            let mut type_map = HashMap::new();
            
            // For each field, try to deduce the type parameter from the argument
            for (i, ((field_name, field_type), arg_val)) in def_clone.fields.iter().zip(&args).enumerate() {
                if let Err(e) = Self::deduce_type_from_value(field_type, arg_val, &mut type_map, &env_clone) {
                    return Value::Error(format!(
                        "Type error in struct instantiation: field '{}' (argument {}) - {}",
                        field_name, i + 1, e
                    ));
                }
            }

            // Validate that all type parameters have been deduced
            for param in &def_clone.params {
                let clean_param = if param.starts_with(':') {
                    &param[1..]
                } else {
                    param
                };
                
                if !type_map.contains_key(clean_param) {
                    return Value::Error(format!(
                        "Could not deduce type for parameter '{}' in struct instantiation", clean_param
                    ));
                }
            }

            // Create the concrete struct name based on deduced types
            let concrete_name = if def_clone.params.is_empty() {
                def_clone.name.clone()
            } else {
                let mut parts = vec![def_clone.name.clone()];
                for param in &def_clone.params {
                    // Clean up the parameter name (remove leading colon if present)
                    let clean_param = if param.starts_with(':') {
                        &param[1..]
                    } else {
                        param
                    };
                    
                    if let Some(concrete_type) = type_map.get(clean_param) {
                        parts.push(Self::type_to_string_static(concrete_type));
                    } else {
                        return Value::Error(format!(
                            "Could not deduce type for parameter '{}'", clean_param
                        ));
                    }
                }
                parts.join("-")
            };

            let fields = def_clone.fields
                .iter()
                .zip(args)
                .map(|((name, _), val)| (name.clone(), val))
                .collect::<Vec<_>>();

            Value::Struct(Arc::new(ast::StructInstance {
                name: concrete_name,
                fields,
            }))
        }));

        env.define(constructor_name, constructor);
        
        // Create generic accessors
        self.create_generic_accessors(def, env)?;
        Ok(())
    }

    fn create_generic_accessors(
        &self,
        def: &StructDef,
        env: &mut Environment,
    ) -> Result<(), String> {
        for (field_name, _) in &def.fields {
            let accessor_name = format!("{}-{}", def.name, field_name);
            
            // Check if accessor already exists
            if env.get(&accessor_name).is_some() {
                continue;
            }

            let field_name_clone = field_name.clone();
            let struct_name_clone = def.name.clone();
            let accessor_name_clone = accessor_name.clone();

            let accessor = Value::NativeClosure(Arc::new(move |args: Vec<Value>| {
                if args.len() != 1 {
                    return Value::Error(format!(
                        "Accessor {} expects 1 argument",
                        accessor_name_clone
                    ));
                }
                match &args[0] {
                    Value::Struct(instance) => {
                        // Check if the struct name starts with the expected base name
                        // This allows for type-deduced struct names like "vec2-int" to match "vec2"
                        if instance.name == struct_name_clone || instance.name.starts_with(&format!("{}-", struct_name_clone)) {
                            instance
                                .fields
                                .iter()
                                .find(|(name, _)| name == &field_name_clone)
                                .map(|(_, val)| val.clone())
                                .unwrap_or(Value::Optional(None))
                        } else {
                            Value::Error(format!(
                                "Invalid argument for accessor {}: expected struct of type '{}', got '{}'",
                                accessor_name_clone, struct_name_clone, instance.name
                            ))
                        }
                    }
                    _ => Value::Error(format!(
                        "Invalid argument for accessor {}: expected struct, got {}",
                        accessor_name_clone,
                        match &args[0] {
                            Value::Int(_) => "int",
                            Value::Float(_) => "float",
                            Value::Bool(_) => "bool",
                            Value::Char(_) => "char",
                            _ => "unknown type"
                        }
                    )),
                }
            }));
            env.define(accessor_name, accessor);
        }
        Ok(())
    }
 
    fn substitute_type(
        &self,
        t: &ast::Type,
        type_map: &HashMap<String, ast::Type>,
    ) -> Result<ast::Type, String> {
        match t {
            ast::Type::GenericParam(name) => type_map
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Unresolved generic parameter: {}", name)),
            ast::Type::Named { name, type_params } => {
                let new_params = type_params
                    .iter()
                    .map(|p| self.substitute_type(p, type_map))
                    .collect::<Result<_, _>>()?;
                Ok(ast::Type::Named {
                    name: name.clone(),
                    type_params: new_params,
                })
            }
            ast::Type::Func {
                param_types,
                return_type,
            } => {
                let new_params = param_types
                    .iter()
                    .map(|p| self.substitute_type(p, type_map))
                    .collect::<Result<_, _>>()?;
                let new_return = self.substitute_type(return_type, type_map)?;
                Ok(ast::Type::Func {
                    param_types: new_params,
                    return_type: Box::new(new_return),
                })
            }
            ast::Type::Option(inner) => {
                let new_inner = self.substitute_type(inner, type_map)?;
                Ok(ast::Type::Option(Box::new(new_inner)))
            }
            _ => Ok(t.clone()),
        }
    }

    fn resolve_type_by_name(
        &self,
        name: &str,
        _env: &Arc<Environment>,
    ) -> Result<Option<ast::Type>, String> {
        match name {
            "int" => Ok(Some(ast::Type::Int)),
            "float" => Ok(Some(ast::Type::Float)),
            "bool" => Ok(Some(ast::Type::Bool)),
            "char" => Ok(Some(ast::Type::Char)),
            "string" => Ok(Some(ast::Type::Named {
                name: "cons".to_string(),
                type_params: vec![Type::Char],
            })),
            _ => Ok(None),
        }
    }

    fn create_accessors(
        &self,
        struct_name: &str,
        fields: &[(String, Type)],
        _env: &mut Environment,
    ) -> HashMap<String, Value> {
        let mut accessors = HashMap::new();
        for (field_name, _) in fields {
            let accessor_name = format!("{}-{}", struct_name, field_name);
            let field_name_clone = field_name.clone();
            let struct_name_clone = Arc::new(struct_name.to_string());
            let accessor_name_clone = Arc::new(accessor_name.clone());

            let accessor = Value::NativeClosure(Arc::new(move |args: Vec<Value>| {
                if args.len() != 1 {
                    return Value::Error(format!(
                        "Accessor {} expects 1 argument",
                        accessor_name_clone
                    ));
                }
                match &args[0] {
                    Value::Struct(instance) if instance.name == *struct_name_clone => instance
                        .fields
                        .iter()
                        .find(|(name, _)| name == &field_name_clone)
                        .map(|(_, val)| val.clone())
                        .unwrap_or(Value::Optional(None)),
                    _ => Value::Error(format!(
                        "Invalid argument for accessor {}",
                        accessor_name_clone
                    )),
                }
            }));
            accessors.insert(accessor_name, accessor);
        }
        accessors
    }

    fn monomorphize_name(&self, base_name: &str, params: &[String], type_map: &HashMap<String, Type>) -> String {
        let mut parts = Vec::new();
        for p_name in params.iter() {
            if let Some(ty) = type_map.get(p_name) {
                parts.push(self.type_to_string(ty));
            }
        }
        format!("{}-{}", base_name, parts.join("-"))
    }

    fn type_to_string(&self, ty: &ast::Type) -> String {
        match ty {
            ast::Type::Int => ":int".to_string(),
            ast::Type::Float => ":float".to_string(),
            ast::Type::Bool => ":bool".to_string(),
            ast::Type::Char => ":char".to_string(),
            ast::Type::Void => ":void".to_string(),
            ast::Type::GenericParam(p) => p.clone(),
            ast::Type::Named { name, type_params } => {
                if type_params.is_empty() {
                    name.clone()
                } else {
                    let params: Vec<String> = type_params.iter().map(|t| self.type_to_string_inner(t)).collect();
                    format!("({} {})", name, params.join(" "))
                }
            }
            ast::Type::Func {
                param_types,
                return_type,
            } => {
                let params: Vec<String> = param_types.iter().map(|t| self.type_to_string_inner(t)).collect();
                format!(
                    "(fun-type ({}) {})",
                    params.join(" "),
                    self.type_to_string_inner(return_type)
                )
            }
            ast::Type::Option(t) => format!("(Option {})", self.type_to_string_inner(t)),
        }
    }

    fn type_to_string_inner(&self, ty: &ast::Type) -> String {
        match ty {
            ast::Type::Int => "int".to_string(),
            ast::Type::Float => "float".to_string(),
            ast::Type::Bool => "bool".to_string(),
            ast::Type::Char => "char".to_string(),
            ast::Type::Void => "void".to_string(),
            ast::Type::GenericParam(p) => p.clone(),
            ast::Type::Named { name, type_params } => {
                if type_params.is_empty() {
                    name.clone()
                } else {
                    let params: Vec<String> = type_params.iter().map(|t| self.type_to_string_inner(t)).collect();
                    format!("({} {})", name, params.join(" "))
                }
            }
            ast::Type::Func {
                param_types,
                return_type,
            } => {
                let params: Vec<String> = param_types.iter().map(|t| self.type_to_string_inner(t)).collect();
                format!(
                    "(fun-type ({}) {})",
                    params.join(" "),
                    self.type_to_string_inner(return_type)
                )
            }
            ast::Type::Option(t) => format!("(Option {})", self.type_to_string_inner(t)),
        }
    }

    fn type_to_string_static(ty: &ast::Type) -> String {
        match ty {
            ast::Type::Int => "int".to_string(),
            ast::Type::Float => "float".to_string(),
            ast::Type::Bool => "bool".to_string(),
            ast::Type::Char => "char".to_string(),
            ast::Type::Void => "void".to_string(),
            ast::Type::GenericParam(p) => p.clone(),
            ast::Type::Named { name, type_params } => {
                if type_params.is_empty() {
                    name.clone()
                } else {
                    let params: Vec<String> = type_params.iter().map(Self::type_to_string_static).collect();
                    format!("({} {})", name, params.join(" "))
                }
            }
            ast::Type::Func {
                param_types,
                return_type,
            } => {
                let params: Vec<String> = param_types.iter().map(Self::type_to_string_static).collect();
                format!(
                    "(fun-type ({}) {})",
                    params.join(" "),
                    Self::type_to_string_static(return_type)
                )
            }
            ast::Type::Option(t) => format!("(Option {})", Self::type_to_string_static(t)),
        }
    }

    fn deduce_type_from_value(
        expected_type: &ast::Type,
        value: &Value,
        type_map: &mut HashMap<String, ast::Type>,
        env: &Arc<Environment>,
    ) -> Result<(), String> {
        match expected_type {
            ast::Type::GenericParam(param_name) => {
                // Deduce the concrete type from the value
                let concrete_type = Self::value_to_type(value, env)?;
                
                // Clean up the parameter name (remove leading colon if present)
                let clean_param_name = if param_name.starts_with(':') {
                    &param_name[1..]
                } else {
                    param_name
                };
                
                // Check if we already have a binding for this parameter
                if let Some(existing_type) = type_map.get(clean_param_name) {
                    if !Self::types_equal(existing_type, &concrete_type) {
                        return Err(format!(
                            "Type parameter '{}' bound to conflicting types: {} and {}",
                            clean_param_name,
                            Self::type_to_string_static(existing_type),
                            Self::type_to_string_static(&concrete_type)
                        ));
                    }
                } else {
                    type_map.insert(clean_param_name.to_string(), concrete_type);
                }
                Ok(())
            }
            ast::Type::Named { name, type_params } => {
                // For named types with parameters, we need to check the value matches
                let value_type = Self::value_to_type(value, env)?;
                if let ast::Type::Named { name: ref value_name, type_params: ref value_params } = value_type {
                    if name == value_name && type_params.len() == value_params.len() {
                        for (expected_param, value_param) in type_params.iter().zip(value_params.iter()) {
                            Self::deduce_type_from_type(expected_param, value_param, type_map)?;
                        }
                        Ok(())
                    } else {
                        Err(format!(
                            "Expected type {} but got {}",
                            Self::type_to_string_static(expected_type),
                            Self::type_to_string_static(&value_type)
                        ))
                    }
                } else {
                    Err(format!(
                        "Expected named type {} but got {}",
                        Self::type_to_string_static(expected_type),
                        Self::type_to_string_static(&value_type)
                    ))
                }
            }
            ast::Type::Option(inner_type) => {
                match value {
                    Value::Optional(Some(inner_value)) => {
                        Self::deduce_type_from_value(inner_type, inner_value, type_map, env)
                    }
                    Value::Optional(None) => {
                        // None can match any Option type, so we don't need to deduce anything
                        Ok(())
                    }
                    _ => Err(format!(
                        "Expected Option type but got {}",
                        Self::type_to_string_static(&Self::value_to_type(value, env)?)
                    ))
                }
            }
            _ => {
                // For concrete types, just verify the value matches
                let value_type = Self::value_to_type(value, env)?;
                if Self::types_equal(expected_type, &value_type) {
                    Ok(())
                } else {
                    Err(format!(
                        "Expected type {} but got {}",
                        Self::type_to_string_static(expected_type),
                        Self::type_to_string_static(&value_type)
                    ))
                }
            }
        }
    }

    fn deduce_type_from_type(
        expected_type: &ast::Type,
        actual_type: &ast::Type,
        type_map: &mut HashMap<String, ast::Type>,
    ) -> Result<(), String> {
        match expected_type {
            ast::Type::GenericParam(param_name) => {
                // Clean up the parameter name (remove leading colon if present)
                let clean_param_name = if param_name.starts_with(':') {
                    &param_name[1..]
                } else {
                    param_name
                };
                
                if let Some(existing_type) = type_map.get(clean_param_name) {
                    if !Self::types_equal(existing_type, actual_type) {
                        return Err(format!(
                            "Type parameter '{}' bound to conflicting types: {} and {}",
                            clean_param_name,
                            Self::type_to_string_static(existing_type),
                            Self::type_to_string_static(actual_type)
                        ));
                    }
                } else {
                    type_map.insert(clean_param_name.to_string(), actual_type.clone());
                }
                Ok(())
            }
            ast::Type::Named { name, type_params } => {
                if let ast::Type::Named { name: actual_name, type_params: actual_params } = actual_type {
                    if name == actual_name && type_params.len() == actual_params.len() {
                        for (expected_param, actual_param) in type_params.iter().zip(actual_params.iter()) {
                            Self::deduce_type_from_type(expected_param, actual_param, type_map)?;
                        }
                        Ok(())
                    } else {
                        Err(format!(
                            "Type mismatch: expected {} but got {}",
                            Self::type_to_string_static(expected_type),
                            Self::type_to_string_static(actual_type)
                        ))
                    }
                } else {
                    Err(format!(
                        "Type mismatch: expected {} but got {}",
                        Self::type_to_string_static(expected_type),
                        Self::type_to_string_static(actual_type)
                    ))
                }
            }
            _ => {
                if Self::types_equal(expected_type, actual_type) {
                    Ok(())
                } else {
                    Err(format!(
                        "Type mismatch: expected {} but got {}",
                        Self::type_to_string_static(expected_type),
                        Self::type_to_string_static(actual_type)
                    ))
                }
            }
        }
    }

    fn value_to_type(value: &Value, env: &Arc<Environment>) -> Result<ast::Type, String> {
        match value {
            Value::Int(_) => Ok(ast::Type::Int),
            Value::Float(_) => Ok(ast::Type::Float),
            Value::Bool(_) => Ok(ast::Type::Bool),
            Value::Char(_) => Ok(ast::Type::Char),
            Value::Optional(Some(inner)) => {
                let inner_type = Self::value_to_type(inner, env)?;
                Ok(ast::Type::Option(Box::new(inner_type)))
            }
            Value::Optional(None) => {
                // For None, we can't determine the inner type, so we use a generic parameter
                Ok(ast::Type::Option(Box::new(ast::Type::GenericParam("T".to_string()))))
            }
            Value::Struct(instance) => {
                // Try to parse the struct name to extract type parameters
                let parts: Vec<&str> = instance.name.split('-').collect();
                if parts.len() > 1 {
                    let base_name = parts[0];
                    let type_params = parts[1..].iter().map(|part| {
                        match *part {
                            "int" => ast::Type::Int,
                            "float" => ast::Type::Float,
                            "bool" => ast::Type::Bool,
                            "char" => ast::Type::Char,
                            _ => ast::Type::Named {
                                name: part.to_string(),
                                type_params: vec![],
                            }
                        }
                    }).collect();
                    
                    Ok(ast::Type::Named {
                        name: base_name.to_string(),
                        type_params,
                    })
                } else {
                    Ok(ast::Type::Named {
                        name: instance.name.clone(),
                        type_params: vec![],
                    })
                }
            }
            _ => Err("Cannot determine type of value".to_string()),
        }
    }

    fn types_equal(a: &ast::Type, b: &ast::Type) -> bool {
        match (a, b) {
            (ast::Type::Int, ast::Type::Int) => true,
            (ast::Type::Float, ast::Type::Float) => true,
            (ast::Type::Bool, ast::Type::Bool) => true,
            (ast::Type::Char, ast::Type::Char) => true,
            (ast::Type::Void, ast::Type::Void) => true,
            (ast::Type::GenericParam(a), ast::Type::GenericParam(b)) => a == b,
            (ast::Type::Named { name: a_name, type_params: a_params },
             ast::Type::Named { name: b_name, type_params: b_params }) => {
                a_name == b_name &&
                a_params.len() == b_params.len() &&
                a_params.iter().zip(b_params.iter()).all(|(a, b)| Self::types_equal(a, b))
            }
            (ast::Type::Option(a), ast::Type::Option(b)) => Self::types_equal(a, b),
            (ast::Type::Func { param_types: a_params, return_type: a_ret },
             ast::Type::Func { param_types: b_params, return_type: b_ret }) => {
                a_params.len() == b_params.len() &&
                a_params.iter().zip(b_params.iter()).all(|(a, b)| Self::types_equal(a, b)) &&
                Self::types_equal(a_ret, b_ret)
            }
            _ => false,
        }
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
            Value::Bool(true) => {
                let then_val = self.eval_with_env(then_branch, Arc::clone(&env))?;
                let else_val = self.eval_with_env(else_branch, Arc::clone(&env))?;
                
                // Type check: both branches must have the same type
                let then_type = self.get_type(&then_val, &env)?;
                let else_type = self.get_type(&else_val, &env)?;
                
                if !self.are_types_compatible(&then_type, &else_type) && !self.are_types_compatible(&else_type, &then_type) {
                    return Err(format!(
                        "If branches have incompatible types: then branch has type {}, else branch has type {}",
                        self.type_to_string(&then_type),
                        self.type_to_string(&else_type)
                    ));
                }
                
                Ok(then_val)
            },
            Value::Bool(false) => {
                let then_val = self.eval_with_env(then_branch, Arc::clone(&env))?;
                let else_val = self.eval_with_env(else_branch, Arc::clone(&env))?;
                
                // Type check: both branches must have the same type
                let then_type = self.get_type(&then_val, &env)?;
                let else_type = self.get_type(&else_val, &env)?;
                
                if !self.are_types_compatible(&then_type, &else_type) && !self.are_types_compatible(&else_type, &then_type) {
                    return Err(format!(
                        "If branches have incompatible types: then branch has type {}, else branch has type {}",
                        self.type_to_string(&then_type),
                        self.type_to_string(&else_type)
                    ));
                }
                
                Ok(else_val)
            },
            _ => Err("If condition must be a boolean".to_string()),
        }
    }

    fn eval_if_let(
        &self,
        identifier: &str,
        expr: &Expr,
        then_branch: &Expr,
        else_branch: &Expr,
        env: Arc<Environment>,
    ) -> Result<Value, String> {
        let value = self.eval_with_env(expr, Arc::clone(&env))?;
 
        if let Value::Optional(Some(val)) = value {
            let mut new_env = Environment::new(Some(Arc::clone(&env)));
            new_env.define(identifier.to_string(), (*val).clone());
            return self.eval_with_env(then_branch, Arc::new(new_env));
        }
 
        self.eval_with_env(else_branch, env)
    }

    fn eval_let(
        &self,
        bindings: &[(Pattern, Expr)],
        body: &Expr,
        env: Arc<Environment>,
    ) -> Result<Value, String> {
        let mut new_env = Environment::new(Some(Arc::clone(&env)));
        let mut new_bindings = Vec::new();
        for (pattern, expr) in bindings {
            let val = self.eval_with_env(expr, Arc::clone(&env))?;
            new_bindings.push((pattern.clone(), val));
        }
        for (pattern, value) in new_bindings {
            self.bind_pattern(&mut new_env, &pattern, value)?;
        }
        self.eval_with_env(body, Arc::new(new_env))
    }


    fn eval_let_star(
        &self,
        bindings: &[(Pattern, Expr)],
        body: &Expr,
        env: Arc<Environment>,
    ) -> Result<Value, String> {
        let mut new_env = Environment::new(Some(Arc::clone(&env)));
        for (pattern, expr) in bindings {
            let value = self.eval_with_env(expr, Arc::new(new_env.clone()))?;
            self.bind_pattern(&mut new_env, pattern, value)?;
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
                if !struct_val.name.starts_with(pat_name) {
                    return Err(format!(
                        "Mismatched struct types in pattern matching: expected {}, got {}",
                        pat_name, struct_val.name
                    ));
                }

                if pat_fields.len() != struct_val.fields.len() {
                    return Err(format!(
                        "Mismatched number of fields in struct pattern for {}: expected {}, got {}",
                        pat_name,
                        pat_fields.len(),
                        struct_val.fields.len()
                    ));
                }
                
                for (field_name_to_bind, (_, field_val)) in pat_fields.iter().zip(struct_val.fields.iter()) {
                    env.define(field_name_to_bind.clone(), field_val.clone());
                }

                Ok(())
            }
            (Pattern::List(patterns), value) => {
                let values = value_to_vec(value)?;
                if patterns.len() != values.len() {
                    return Err(format!(
                        "Mismatched number of elements in list pattern: expected {}, got {}",
                        patterns.len(),
                        values.len()
                    ));
                }
                for (pat, val) in patterns.iter().zip(values) {
                    self.bind_pattern(env, pat, val)?;
                }
                Ok(())
            }
            (Pattern::Struct { .. }, _) => Err("Pattern did not match struct value".to_string()),
        }
    }

    fn eval_call(
        &self,
        function_expr: &Expr,
        arg_exprs: &[Expr],
        env: Arc<Environment>,
    ) -> Result<Value, String> {
        let func_val = self.eval_with_env(function_expr, Arc::clone(&env))?;

        let mut args = Vec::new();
        for arg_expr in arg_exprs {
            args.push(self.eval_with_env(arg_expr, Arc::clone(&env))?);
        }

        if let Value::Function(func) = &func_val {
            if func.params.len() != args.len() {
                return Err(format!(
                    "Expected {} arguments, but got {}",
                    func.params.len(),
                    args.len()
                ));
            }
            for ((_, expected_type), arg_val) in func.params.iter().zip(&args) {
                if !self.type_check(arg_val, expected_type, &env)? {
                    let arg_type = self.get_type(arg_val, &env)?;
                    return Err(format!(
                        "Type mismatch: expected {}, got {}. Got value {}",
                        self.type_to_string(expected_type),
                        self.type_to_string(&arg_type),
                        arg_val,
                    ));
                }
            }
        }

        self.apply_function(func_val, &args, env)
    }

    fn apply_function(
        &self,
        func_val: Value,
        args: &[Value],
        _env: Arc<Environment>,
    ) -> Result<Value, String> {
        match func_val {
            Value::Function(func) => {
                let mut call_env = Environment::new(Some(func.captured_env.clone()));
                if let Some(name) = &func.name {
                    call_env.define(name.clone(), Value::Function(func.clone()));
                }
                for ((pattern, _), arg_val) in func.params.iter().zip(args) {
                    self.bind_pattern(&mut call_env, pattern, arg_val.clone())?;
                }
                self.eval_with_env(&func.body, Arc::new(call_env))
            }
            Value::NativeFunc(native_fn) => self.apply_native_function(&native_fn, args),
            Value::NativeClosure(native_closure) => self.apply_native_function(&*native_closure, args),
            _ => Err("Expression is not a function and cannot be called".to_string()),
        }
    }

    fn apply_native_function(
        &self,
        native_fn: &dyn Fn(Vec<Value>) -> Value,
        args: &[Value],
    ) -> Result<Value, String> {
        let result = native_fn(args.to_vec());
        if let Value::Error(e) = result {
            Err(e)
        } else {
            Ok(result)
        }
    }

    fn get_type(&self, value: &Value, env: &Arc<Environment>) -> Result<Type, String> {
        match value {
            Value::Int(_) => Ok(Type::Int),
            Value::Float(_) => Ok(Type::Float),
            Value::Bool(_) => Ok(Type::Bool),
            Value::Char(_) => Ok(Type::Char),
            Value::Optional(Some(val)) => Ok(Type::Option(Box::new(self.get_type(val, env)?))),
            Value::Optional(None) => Ok(Type::Option(Box::new(Type::GenericParam("T".to_string())))), // Represents `none`
            Value::Struct(instance) => {
                // Check if this is a concrete instantiation of a generic struct
                if let Some(dash_pos) = instance.name.rfind('-') {
                    let base_name = &instance.name[..dash_pos];
                    let type_suffix = &instance.name[dash_pos + 1..];
                    
                    // Convert the type suffix to a proper type
                    let concrete_type = match type_suffix {
                        "int" => Type::Int,
                        "float" => Type::Float,
                        "bool" => Type::Bool,
                        "char" => Type::Char,
                        _ => Type::Named {
                            name: type_suffix.to_string(),
                            type_params: vec![],
                        }
                    };
                    
                    Ok(Type::Named {
                        name: base_name.to_string(),
                        type_params: vec![concrete_type],
                    })
                } else {
                    let type_params = instance
                        .fields
                        .iter()
                        .map(|(_, v)| self.get_type(v, env))
                        .collect::<Result<_, _>>()?;

                    Ok(Type::Named {
                        name: instance.name.clone(),
                        type_params,
                    })
                }
            }
            Value::Function(func) => {
                let param_types = func
                    .params
                    .iter()
                    .map(|(_, ty)| ty.clone())
                    .collect();
                Ok(Type::Func {
                    param_types,
                    return_type: Box::new(func.return_type.clone()),
                })
            }
            _ => Err("Cannot determine type of value".to_string()),
        }
    }
    
    fn type_check(&self, value: &Value, expected: &Type, env: &Arc<Environment>) -> Result<bool, String> {
        let actual = self.get_type(value, env)?;
        Ok(self.are_types_compatible(&actual, expected))
    }

    fn are_types_compatible(&self, actual: &Type, expected: &Type) -> bool {
        match (actual, expected) {
            // Direct type equality
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Char, Type::Char) => true,
            (Type::Void, Type::Void) => true,
            (Type::Option(actual_inner), Type::Option(expected_inner)) => {
                if let Type::GenericParam(_) = **actual_inner {
                    return true;
                }
                if let Type::GenericParam(_) = **expected_inner {
                    return true;
                }
                self.are_types_compatible(actual_inner, expected_inner)
            }
            (Type::Func { param_types: actual_params, return_type: actual_ret },
             Type::Func { param_types: expected_params, return_type: expected_ret }) => {
                if actual_params.len() != expected_params.len() {
                    return false;
                }
                for (actual_param, expected_param) in actual_params.iter().zip(expected_params.iter()) {
                    if !self.are_types_compatible(actual_param, expected_param) {
                        return false;
                    }
                }
                self.are_types_compatible(actual_ret, expected_ret)
            }
            (
                Type::Named {
                    name: actual_name,
                    type_params: actual_params,
                },
                Type::Named {
                    name: expected_name,
                    type_params: expected_params,
                },
            ) => {
                // Check for exact match first
                if actual_name == expected_name &&
                   actual_params.len() == expected_params.len() &&
                   actual_params.iter().zip(expected_params.iter())
                       .all(|(a, e)| self.are_types_compatible(a, e)) {
                    return true;
                }
                
                // Check if actual is a concrete instantiation of expected generic type
                // e.g., actual: "box-int" vs expected: "box" with type_params: [int]
                if actual_params.is_empty() && !expected_params.is_empty() {
                    if let Some(dash_pos) = actual_name.rfind('-') {
                        let base_name = &actual_name[..dash_pos];
                        if base_name == expected_name {
                            // Extract type arguments from the concrete name
                            let type_suffix = &actual_name[dash_pos + 1..];
                            let concrete_type = match type_suffix {
                                "int" => Type::Int,
                                "float" => Type::Float,
                                "bool" => Type::Bool,
                                "char" => Type::Char,
                                _ => Type::Named {
                                    name: type_suffix.to_string(),
                                    type_params: vec![],
                                }
                            };
                            
                            // Check if the concrete type matches the expected type parameters
                            if expected_params.len() == 1 {
                                return self.are_types_compatible(&concrete_type, &expected_params[0]);
                            }
                        }
                    }
                }
                
                false
            }
            _ => false,
        }
    }
}

fn lexpr_to_value(val: &lexpr::Value) -> Value {
    match val {
        lexpr::Value::Number(n) => {
            if n.is_i64() {
                Value::Int(n.as_i64().unwrap())
            } else {
                Value::Float(n.as_f64().unwrap())
            }
        }
        lexpr::Value::Bool(b) => Value::Bool(*b),
        lexpr::Value::Char(c) => Value::Char(*c),
        lexpr::Value::Symbol(s) => Value::Symbol(s.to_string()),
        lexpr::Value::Cons(c) => {
            let (list, tail) = c.to_vec();
            if tail.is_null() {
                let mut current = Value::Optional(None);
                for item in list.iter().rev() {
                    let fields = vec![
                        ("car".to_string(), lexpr_to_value(item)),
                        ("cdr".to_string(), current),
                    ];
                    current = Value::Struct(Arc::new(ast::StructInstance {
                        name: "cons".to_string(),
                        fields,
                    }));
                }
                current
            } else {
                Value::Error("Cannot convert improper list to value".to_string())
            }
        }
        _ => Value::Optional(None),
    }
}

fn value_to_vec(mut val: Value) -> Result<Vec<Value>, String> {
    let mut vec = Vec::new();
    loop {
        match val {
            Value::Optional(None) => break,
            Value::Struct(s) if s.name == "cons" => {
                let car = s
                    .fields
                    .iter()
                    .find(|(name, _)| name == "car")
                    .map(|(_, val)| val.clone())
                    .unwrap_or(Value::Optional(None));
                let cdr = s
                    .fields
                    .iter()
                    .find(|(name, _)| name == "cdr")
                    .map(|(_, val)| val.clone())
                    .unwrap_or(Value::Optional(None));
                vec.push(car);
                val = cdr;
            }
            _ => return Err("Value is not a proper list".to_string()),
        }
    }
    Ok(vec)
}