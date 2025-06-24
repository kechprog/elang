use tower_lsp::lsp_types::*;
use tracing::debug;

/// Provides auto-completion suggestions for Elang code
pub struct CompletionProvider {
    keywords: Vec<&'static str>,
    builtin_functions: Vec<&'static str>,
    builtin_types: Vec<&'static str>,
}

impl CompletionProvider {
    pub fn new() -> Self {
        Self {
            keywords: vec![
                "let", "fn", "if", "else", "while", "for", "match", "return",
                "struct", "impl", "true", "false", "null", "self", "Self",
                "pub", "mod", "use", "as", "where", "type", "trait", "enum",
                "const", "static", "mut", "ref", "move", "async", "await",
            ],
            builtin_functions: vec![
                "print", "println", "len", "push", "pop", "get", "set",
                "map", "filter", "reduce", "fold", "sort", "reverse",
                "clone", "copy", "drop", "size_of", "type_of",
            ],
            builtin_types: vec![
                "i32", "i64", "f32", "f64", "bool", "str", "String",
                "Vec", "Array", "HashMap", "Option", "Result", "Box",
                "Rc", "Arc", "Cell", "RefCell", "Mutex", "RwLock",
            ],
        }
    }

    /// Provide completion suggestions at the given position
    pub async fn provide_completions(
        &self,
        content: &str,
        position: Position,
    ) -> Vec<CompletionItem> {
        debug!("Providing completions at position {:?}", position);

        let mut completions = Vec::new();

        // Get the current line and character position
        let lines: Vec<&str> = content.lines().collect();
        if position.line as usize >= lines.len() {
            return completions;
        }

        let current_line = lines[position.line as usize];
        let char_pos = position.character as usize;

        // Get the word being typed
        let word_start = self.find_word_start(current_line, char_pos);
        let word_end = self.find_word_end(current_line, char_pos);
        let current_word = if word_start < word_end {
            &current_line[word_start..word_end]
        } else {
            ""
        };

        debug!("Current word: '{}'", current_word);

        // Add keyword completions
        for keyword in &self.keywords {
            if keyword.starts_with(current_word) {
                completions.push(CompletionItem {
                    label: keyword.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("Keyword".to_string()),
                    documentation: Some(Documentation::String(format!(
                        "Elang keyword: {}",
                        keyword
                    ))),
                    insert_text: Some(keyword.to_string()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    ..Default::default()
                });
            }
        }

        // Add builtin function completions
        for function in &self.builtin_functions {
            if function.starts_with(current_word) {
                completions.push(CompletionItem {
                    label: format!("{}()", function),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some("Built-in function".to_string()),
                    documentation: Some(Documentation::String(format!(
                        "Built-in function: {}",
                        function
                    ))),
                    insert_text: Some(format!("{}($0)", function)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }

        // Add builtin type completions
        for type_name in &self.builtin_types {
            if type_name.starts_with(current_word) {
                completions.push(CompletionItem {
                    label: type_name.to_string(),
                    kind: Some(CompletionItemKind::TYPE_PARAMETER),
                    detail: Some("Built-in type".to_string()),
                    documentation: Some(Documentation::String(format!(
                        "Built-in type: {}",
                        type_name
                    ))),
                    insert_text: Some(type_name.to_string()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    ..Default::default()
                });
            }
        }

        // Add context-specific completions
        self.add_context_completions(current_line, char_pos, current_word, &mut completions);

        // Add snippet completions
        self.add_snippet_completions(current_word, &mut completions);

        completions
    }

    /// Find the start of the current word
    fn find_word_start(&self, line: &str, pos: usize) -> usize {
        let chars: Vec<char> = line.chars().collect();
        let mut start = pos.min(chars.len());

        while start > 0 {
            let ch = chars[start - 1];
            if ch.is_alphanumeric() || ch == '_' {
                start -= 1;
            } else {
                break;
            }
        }

        start
    }

    /// Find the end of the current word
    fn find_word_end(&self, line: &str, pos: usize) -> usize {
        let chars: Vec<char> = line.chars().collect();
        let mut end = pos.min(chars.len());

        while end < chars.len() {
            let ch = chars[end];
            if ch.is_alphanumeric() || ch == '_' {
                end += 1;
            } else {
                break;
            }
        }

        end
    }

    /// Add context-specific completions based on the current line
    fn add_context_completions(
        &self,
        line: &str,
        pos: usize,
        current_word: &str,
        completions: &mut Vec<CompletionItem>,
    ) {
        // Check if we're in a function parameter list
        if line.contains("fn ") && line.contains('(') && !line.contains(')') {
            // Add parameter type suggestions
            for type_name in &self.builtin_types {
                if type_name.starts_with(current_word) {
                    completions.push(CompletionItem {
                        label: type_name.to_string(),
                        kind: Some(CompletionItemKind::TYPE_PARAMETER),
                        detail: Some("Parameter type".to_string()),
                        insert_text: Some(type_name.to_string()),
                        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                        ..Default::default()
                    });
                }
            }
        }

        // Check if we're after a dot (field access)
        if pos > 0 && line.chars().nth(pos - 1) == Some('.') {
            // Add common method suggestions
            let methods = vec!["len", "push", "pop", "get", "set", "clone", "to_string"];
            for method in methods {
                if method.starts_with(current_word) {
                    completions.push(CompletionItem {
                        label: format!("{}()", method),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some("Method".to_string()),
                        insert_text: Some(format!("{}($0)", method)),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        ..Default::default()
                    });
                }
            }
        }

        // Check if we're in a struct definition
        if line.trim_start().starts_with("struct ") {
            // Add struct field suggestions
            if current_word.is_empty() || "pub".starts_with(current_word) {
                completions.push(CompletionItem {
                    label: "pub".to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("Visibility modifier".to_string()),
                    insert_text: Some("pub ".to_string()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    ..Default::default()
                });
            }
        }
    }

    /// Add code snippet completions
    fn add_snippet_completions(&self, current_word: &str, completions: &mut Vec<CompletionItem>) {
        let snippets = vec![
            (
                "fn",
                "fn ${1:name}(${2:params}) -> ${3:ReturnType} {\n    ${0}\n}",
                "Function definition",
            ),
            (
                "if",
                "if ${1:condition} {\n    ${0}\n}",
                "If statement",
            ),
            (
                "ifelse",
                "if ${1:condition} {\n    ${2}\n} else {\n    ${0}\n}",
                "If-else statement",
            ),
            (
                "while",
                "while ${1:condition} {\n    ${0}\n}",
                "While loop",
            ),
            (
                "for",
                "for ${1:item} in ${2:iterable} {\n    ${0}\n}",
                "For loop",
            ),
            (
                "match",
                "match ${1:expr} {\n    ${2:pattern} => ${3:value},\n    _ => ${0}\n}",
                "Match expression",
            ),
            (
                "struct",
                "struct ${1:Name} {\n    ${0}\n}",
                "Struct definition",
            ),
            (
                "impl",
                "impl ${1:Type} {\n    ${0}\n}",
                "Implementation block",
            ),
            (
                "let",
                "let ${1:name} = ${0};",
                "Variable binding",
            ),
            (
                "letmut",
                "let mut ${1:name} = ${0};",
                "Mutable variable binding",
            ),
        ];

        for (trigger, snippet, description) in snippets {
            if trigger.starts_with(current_word) {
                completions.push(CompletionItem {
                    label: trigger.to_string(),
                    kind: Some(CompletionItemKind::SNIPPET),
                    detail: Some(description.to_string()),
                    documentation: Some(Documentation::String(format!(
                        "Code snippet: {}",
                        description
                    ))),
                    insert_text: Some(snippet.to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }
    }
}

impl Default for CompletionProvider {
    fn default() -> Self {
        Self::new()
    }
}