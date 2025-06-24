use dashmap::DashMap;
use serde_json::Value;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{error, info, warn};

mod analysis;
mod completion;
mod hover;

use analysis::DocumentAnalyzer;
use completion::CompletionProvider;
use hover::HoverProvider;

/// Document storage for the LSP server
type DocumentStore = Arc<DashMap<Url, String>>;

/// Main LSP backend for the Elang language
pub struct ElangLanguageServer {
    client: Client,
    documents: DocumentStore,
    analyzer: DocumentAnalyzer,
    completion_provider: CompletionProvider,
    hover_provider: HoverProvider,
}

impl ElangLanguageServer {
    pub fn new(client: Client) -> Self {
        let documents = Arc::new(DashMap::new());
        let analyzer = DocumentAnalyzer::new(documents.clone());
        let completion_provider = CompletionProvider::new();
        let hover_provider = HoverProvider::new();

        Self {
            client,
            documents,
            analyzer,
            completion_provider,
            hover_provider,
        }
    }

    /// Get document content by URI
    fn get_document(&self, uri: &Url) -> Option<String> {
        self.documents.get(uri).map(|entry| entry.value().clone())
    }

    /// Update document content and trigger analysis
    async fn update_document(&self, uri: Url, content: String) {
        self.documents.insert(uri.clone(), content);
        
        // Analyze the document and publish diagnostics
        match self.analyzer.analyze_document(&uri).await {
            Ok(diagnostics) => {
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            }
            Err(err) => {
                error!("Failed to analyze document {}: {}", uri, err);
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for ElangLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        info!("Initializing Elang Language Server");
        
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::FULL,
            )),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                work_done_progress_options: Default::default(),
                all_commit_characters: None,
                completion_item: None,
            }),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            workspace_symbol_provider: Some(OneOf::Left(true)),
            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
            code_lens_provider: Some(CodeLensOptions {
                resolve_provider: Some(false),
            }),
            document_formatting_provider: Some(OneOf::Left(true)),
            document_range_formatting_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Left(true)),
            folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
            execute_command_provider: Some(ExecuteCommandOptions {
                commands: vec!["elang.restart".to_string()],
                work_done_progress_options: Default::default(),
            }),
            workspace: Some(WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    change_notifications: Some(OneOf::Left(true)),
                }),
                file_operations: None,
            }),
            ..Default::default()
        };

        Ok(InitializeResult {
            capabilities,
            server_info: Some(ServerInfo {
                name: "Elang Language Server".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("Elang Language Server initialized");
        
        self.client
            .log_message(MessageType::INFO, "Elang Language Server started")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Shutting down Elang Language Server");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        info!("Document opened: {}", params.text_document.uri);
        
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        
        self.update_document(uri, content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        info!("Document changed: {}", params.text_document.uri);
        
        let uri = params.text_document.uri;
        
        // Since we use FULL sync, we expect exactly one change with the full content
        if let Some(change) = params.content_changes.into_iter().next() {
            self.update_document(uri, change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        info!("Document closed: {}", params.text_document.uri);
        
        // Remove document from storage and clear diagnostics
        self.documents.remove(&params.text_document.uri);
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        if let Some(content) = self.get_document(uri) {
            return Ok(self.hover_provider.provide_hover(&content, position).await);
        }
        
        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        
        if let Some(content) = self.get_document(uri) {
            let items = self.completion_provider.provide_completions(&content, position).await;
            return Ok(Some(CompletionResponse::Array(items)));
        }
        
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        if let Some(content) = self.get_document(uri) {
            if let Some(location) = self.analyzer.find_definition(&content, position).await {
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
        }
        
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        
        if let Some(content) = self.get_document(uri) {
            let references = self.analyzer.find_references(&content, position).await;
            return Ok(Some(references));
        }
        
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        
        if let Some(content) = self.get_document(uri) {
            let symbols = self.analyzer.get_document_symbols(&content).await;
            return Ok(Some(DocumentSymbolResponse::Nested(symbols)));
        }
        
        Ok(None)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        
        if let Some(_content) = self.get_document(uri) {
            // For now, return empty - formatting can be implemented later
            return Ok(Some(vec![]));
        }
        
        Ok(None)
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        match params.command.as_str() {
            "elang.restart" => {
                info!("Restarting Elang Language Server");
                self.client
                    .log_message(MessageType::INFO, "Elang Language Server restarted")
                    .await;
                Ok(Some(Value::Null))
            }
            _ => {
                warn!("Unknown command: {}", params.command);
                Ok(None)
            }
        }
    }
}