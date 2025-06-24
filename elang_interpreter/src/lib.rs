pub mod ast;
pub mod interpreter;
pub mod stdlib;

use ast::Value;
use interpreter::Interpreter;

impl Interpreter {
    pub fn eval_source(&mut self, source: &str) -> Result<Value, String> {
        let module = elang_parser::parse(source)?;
        if module.body.is_empty() {
            return Ok(Value::Optional(None));
        }

        let mut last_val = Value::Optional(None);
        for item in module.body {
            last_val = self.eval(&item)?;
        }
        Ok(last_val)
    }
}
