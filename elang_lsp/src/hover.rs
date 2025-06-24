use elang_ast::{Expr, Module, TopLevel, Literal};
use tower_lsp::lsp_types::*;
use tracing::debug;

/// Provides hover information for Elang code
pub struct HoverProvider;

impl HoverProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide hover information at the given position
    pub async fn provide_hover(&self, content: &str, position: Position) -> Option<Hover> {
        debug!("Providing hover at position {:?}", position);

        let lines: Vec<&str> = content.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }

        let current_line = lines[position.line as usize];
        let char_pos = position.character as usize;

        // Get the word at the cursor position
        let (word_start, word_end) = self.find_word_bounds(current_line, char_pos);
        if word_start >= word_end {
            return None;
        }

        let word = &current_line[word_start..word_end];
        debug!("Hover word: '{}'", word);

        // Try to parse the document to get context
        if let Ok(module) = elang_parser::parse(content) {
            // Look for the symbol in the parsed AST
            if let Some(hover_info) = self.find_symbol_info(&module, word, position) {
                return Some(hover_info);
            }
        }

        // Provide hover for built-in keywords and types
        self.get_builtin_hover(word)
    }

    /// Find word boundaries at the given position
    fn find_word_bounds(&self, line: &str, pos: usize) -> (usize, usize) {
        let chars: Vec<char> = line.chars().collect();
        let pos = pos.min(chars.len());

        // Find start of word
        let mut start = pos;
        while start > 0 {
            let ch = chars[start - 1];
            if ch.is_alphanumeric() || ch == '_' {
                start -= 1;
            } else {
                break;
            }
        }

        // Find end of word
        let mut end = pos;
        while end < chars.len() {
            let ch = chars[end];
            if ch.is_alphanumeric() || ch == '_' {
                end += 1;
            } else {
                break;
            }
        }

        (start, end)
    }

    /// Find symbol information in the parsed AST
    fn find_symbol_info(&self, module: &Module, symbol: &str, _position: Position) -> Option<Hover> {
        // Search through top-level items to find symbol definitions
        for item in &module.body {
            if let Some(hover) = self.check_top_level_for_symbol(item, symbol) {
                return Some(hover);
            }
        }

        None
    }

    /// Check a top-level item for symbol definitions
    fn check_top_level_for_symbol(&self, item: &TopLevel, symbol: &str) -> Option<Hover> {
        match item {
            TopLevel::VarDef(name, expr) => {
                if name == symbol {
                    let mut content = format!("**Variable:** `{}`", name);
                    
                    // Try to infer type from value
                    if let Some(inferred_type) = self.infer_expression_type(expr) {
                        content.push_str(&format!("\n\n**Inferred Type:** `{}`", inferred_type));
                    }

                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: content,
                        }),
                        range: None,
                    });
                }
            }
            TopLevel::FunDef(fun_def) => {
                if fun_def.name == symbol {
                    let param_str = fun_def.params
                        .iter()
                        .map(|(pattern, type_ann)| {
                            let name = match pattern {
                                elang_ast::Pattern::Identifier(n) => n.clone(),
                                _ => "pattern".to_string(),
                            };
                            format!("{}: {:?}", name, type_ann)
                        })
                        .collect::<Vec<_>>()
                        .join(", ");

                    let return_str = format!(" -> {:?}", fun_def.return_type);

                    let content = format!(
                        "**Function:** `fn {}({}){}`\n\n{}",
                        fun_def.name,
                        param_str,
                        return_str,
                        "User-defined function"
                    );

                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: content,
                        }),
                        range: None,
                    });
                }
            }
            TopLevel::StructDef(struct_def) => {
                if struct_def.name == symbol {
                    let field_str = struct_def.fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            format!("  {}: {:?}", field_name, field_type)
                        })
                        .collect::<Vec<_>>()
                        .join(",\n");

                    let content = format!(
                        "**Struct:** `{}`\n\n```elang\nstruct {} {{\n{}\n}}\n```",
                        struct_def.name, struct_def.name, field_str
                    );

                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: content,
                        }),
                        range: None,
                    });
                }
            }
            _ => {}
        }

        None
    }

    /// Infer the type of an expression (simplified)
    fn infer_expression_type(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Literal(literal) => {
                match literal {
                    Literal::Bool(_) => Some("bool".to_string()),
                    Literal::Int(_) => Some("int".to_string()),
                    Literal::Float(_) => Some("float".to_string()),
                    Literal::Char(_) => Some("char".to_string()),
                    Literal::Nil => Some("nil".to_string()),
                }
            }
            Expr::Identifier(name) => {
                // Could look up the identifier in scope, but for now just return unknown
                Some(format!("identifier({})", name))
            }
            Expr::Call { .. } => {
                // Could analyze function return type, but for now return unknown
                Some("unknown".to_string())
            }
            Expr::If { .. } => {
                // Could analyze branches to determine common type
                Some("unknown".to_string())
            }
            Expr::Function { return_type, .. } => {
                Some(format!("function -> {:?}", return_type))
            }
            _ => None,
        }
    }

    /// Get hover information for built-in symbols
    fn get_builtin_hover(&self, word: &str) -> Option<Hover> {
        let builtin_info = match word {
            // Keywords
            "def" => ("**Keyword:** `def`", "Declares a variable or function"),
            "fun" => ("**Keyword:** `fun`", "Declares a function"),
            "if" => ("**Keyword:** `if`", "Conditional expression"),
            "let" => ("**Keyword:** `let`", "Local variable binding"),
            "let*" => ("**Keyword:** `let*`", "Sequential local variable binding"),
            "quote" => ("**Keyword:** `quote`", "Quote expression"),
            "true" => ("**Boolean:** `true`", "Boolean true value"),
            "false" => ("**Boolean:** `false`", "Boolean false value"),
            "none" => ("**Null:** `none`", "Null/empty value"),

            // Built-in types
            "int" => ("**Type:** `int`", "Integer type"),
            "float" => ("**Type:** `float`", "Floating point type"),
            "bool" => ("**Type:** `bool`", "Boolean type (true/false)"),
            "char" => ("**Type:** `char`", "Character type"),
            "void" => ("**Type:** `void`", "Void type"),
            "string" => ("**Type:** `string`", "String type"),

            // Built-in functions (common Lisp-style functions)
            "+" => ("**Function:** `+`", "Addition operator"),
            "-" => ("**Function:** `-`", "Subtraction operator"),
            "*" => ("**Function:** `*`", "Multiplication operator"),
            "/" => ("**Function:** `/`", "Division operator"),
            "=" => ("**Function:** `=`", "Equality comparison"),
            "<" => ("**Function:** `<`", "Less than comparison"),
            ">" => ("**Function:** `>`", "Greater than comparison"),
            "cons" => ("**Function:** `cons`", "Construct a pair/list"),
            "car" => ("**Function:** `car`", "Get first element of pair"),
            "cdr" => ("**Function:** `cdr`", "Get second element of pair"),

            _ => return None,
        };

        let content = format!("{}\n\n{}", builtin_info.0, builtin_info.1);

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        })
    }
}

impl Default for HoverProvider {
    fn default() -> Self {
        Self::new()
    }
}