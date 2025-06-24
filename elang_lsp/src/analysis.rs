use elang_ast::{Expr, Module, TopLevel, Type, FunDef, StructDef};
use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tracing::{debug, error};

use crate::DocumentStore;

/// Document analyzer that provides semantic analysis for Elang files
pub struct DocumentAnalyzer {
    documents: DocumentStore,
}

impl DocumentAnalyzer {
    pub fn new(documents: DocumentStore) -> Self {
        Self { documents }
    }

    /// Analyze a document and return diagnostics
    pub async fn analyze_document(&self, uri: &Url) -> Result<Vec<Diagnostic>, String> {
        let content = self
            .documents
            .get(uri)
            .ok_or_else(|| format!("Document not found: {}", uri))?
            .clone();

        debug!("Analyzing document: {}", uri);

        let mut diagnostics = Vec::new();

        // Parse the document
        match elang_parser::parse(&content) {
            Ok(module) => {
                debug!("Successfully parsed document: {}", uri);
                
                // Perform semantic analysis
                self.analyze_module(&module, &mut diagnostics);
            }
            Err(parse_error) => {
                error!("Parse error in {}: {:?}", uri, parse_error);
                
                // Convert parse error to diagnostic
                let diagnostic = Diagnostic {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: u32::MAX,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("parse_error".to_string())),
                    source: Some("elang".to_string()),
                    message: format!("Parse error: {}", parse_error),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                };
                diagnostics.push(diagnostic);
            }
        }

        Ok(diagnostics)
    }

    /// Perform semantic analysis on a parsed module
    fn analyze_module(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        for top_level in &module.body {
            self.analyze_top_level(top_level, diagnostics);
        }
    }

    /// Analyze a top-level item for semantic errors
    fn analyze_top_level(&self, item: &TopLevel, diagnostics: &mut Vec<Diagnostic>) {
        match item {
            TopLevel::Expr(expr) => {
                self.analyze_expression(expr, diagnostics);
            }
            TopLevel::VarDef(name, expr) => {
                self.analyze_expression(expr, diagnostics);
                
                // Check for unused variables (simplified)
                if name.starts_with('_') {
                    // Variables starting with _ are intentionally unused
                } else {
                    // Could add more sophisticated unused variable detection
                }
            }
            TopLevel::FunDef(fun_def) => {
                self.analyze_expression(&fun_def.body, diagnostics);
            }
            TopLevel::StructDef(_) => {
                // Struct analysis can be added here
            }
            TopLevel::Provides(_) | TopLevel::Require(_) => {
                // Module declarations don't need analysis
            }
        }
    }

    /// Analyze an expression for semantic errors
    fn analyze_expression(&self, expr: &Expr, diagnostics: &mut Vec<Diagnostic>) {
        match expr {
            Expr::Call { function, arguments } => {
                self.analyze_expression(function, diagnostics);
                for arg in arguments {
                    self.analyze_expression(arg, diagnostics);
                }
            }
            Expr::If { condition, then_branch, else_branch } => {
                self.analyze_expression(condition, diagnostics);
                self.analyze_expression(then_branch, diagnostics);
                self.analyze_expression(else_branch, diagnostics);
            }
            Expr::IfLet { expr, then_branch, else_branch, .. } => {
                self.analyze_expression(expr, diagnostics);
                self.analyze_expression(then_branch, diagnostics);
                self.analyze_expression(else_branch, diagnostics);
            }
            Expr::Let { bindings, body } | Expr::LetStar { bindings, body } => {
                for (_, binding_expr) in bindings {
                    self.analyze_expression(binding_expr, diagnostics);
                }
                self.analyze_expression(body, diagnostics);
            }
            Expr::Function { body, .. } => {
                self.analyze_expression(body, diagnostics);
            }
            Expr::Literal(_) | Expr::Identifier(_) | Expr::Quote(_) => {
                // Literals, identifiers, and quotes don't need analysis
            }
        }
    }

    /// Find the definition of a symbol at the given position
    pub async fn find_definition(&self, content: &str, position: Position) -> Option<Location> {
        // This is a simplified implementation
        // In a real LSP, you'd need to:
        // 1. Parse the document
        // 2. Find the symbol at the position
        // 3. Resolve the symbol to its definition
        // 4. Return the location of the definition
        
        debug!("Finding definition at position {:?}", position);
        
        // For now, return None - this can be implemented later
        None
    }

    /// Find all references to a symbol at the given position
    pub async fn find_references(&self, content: &str, position: Position) -> Vec<Location> {
        debug!("Finding references at position {:?}", position);
        
        // This is a simplified implementation
        // In a real LSP, you'd need to:
        // 1. Parse the document
        // 2. Find the symbol at the position
        // 3. Search for all references to that symbol
        // 4. Return the locations of all references
        
        vec![]
    }

    /// Get document symbols for outline view
    pub async fn get_document_symbols(&self, content: &str) -> Vec<DocumentSymbol> {
        debug!("Getting document symbols");
        
        let mut symbols = Vec::new();
        
        match elang_parser::parse(content) {
            Ok(module) => {
                for item in &module.body {
                    if let Some(symbol) = self.top_level_to_symbol(item) {
                        symbols.push(symbol);
                    }
                }
            }
            Err(_) => {
                // If parsing fails, return empty symbols
            }
        }
        
        symbols
    }

    /// Convert a top-level item to a document symbol
    fn top_level_to_symbol(&self, item: &TopLevel) -> Option<DocumentSymbol> {
        match item {
            TopLevel::FunDef(fun_def) => {
                let range = Range {
                    start: Position { line: 0, character: 0 },
                    end: Position { line: 0, character: 0 },
                };
                
                Some(DocumentSymbol {
                    name: fun_def.name.clone(),
                    detail: Some(format!("fn {}({}) -> {:?}", fun_def.name, fun_def.params.len(), fun_def.return_type)),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    deprecated: Some(false),
                    range,
                    selection_range: range,
                    children: None,
                })
            }
            TopLevel::StructDef(struct_def) => {
                let range = Range {
                    start: Position { line: 0, character: 0 },
                    end: Position { line: 0, character: 0 },
                };
                
                Some(DocumentSymbol {
                    name: struct_def.name.clone(),
                    detail: Some(format!("struct {}", struct_def.name)),
                    kind: SymbolKind::STRUCT,
                    tags: None,
                    deprecated: Some(false),
                    range,
                    selection_range: range,
                    children: None,
                })
            }
            TopLevel::VarDef(name, _) => {
                let range = Range {
                    start: Position { line: 0, character: 0 },
                    end: Position { line: 0, character: 0 },
                };
                
                Some(DocumentSymbol {
                    name: name.clone(),
                    detail: Some(format!("def {}", name)),
                    kind: SymbolKind::VARIABLE,
                    tags: None,
                    deprecated: Some(false),
                    range,
                    selection_range: range,
                    children: None,
                })
            }
            _ => None,
        }
    }
}