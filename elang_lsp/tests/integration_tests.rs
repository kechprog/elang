use elang_lsp::ElangLanguageServer;
use tower_lsp::lsp_types::*;
use tower_lsp::{LspService, LanguageServer};

#[tokio::test]
async fn test_lsp_service_creation() {
    // Test that we can create an LSP service
    let (service, _socket) = LspService::new(|client| ElangLanguageServer::new(client));
    
    // Test that the service was created successfully
    assert!(!std::ptr::eq(service.inner(), std::ptr::null()));
}

#[tokio::test]
async fn test_lsp_initialization() {
    let (service, _socket) = LspService::new(|client| ElangLanguageServer::new(client));
    let server = service.inner();
    
    let params = InitializeParams {
        process_id: Some(1234),
        root_path: None,
        root_uri: Some(Url::parse("file:///test").unwrap()),
        initialization_options: None,
        capabilities: ClientCapabilities::default(),
        trace: Some(TraceValue::Off),
        workspace_folders: None,
        client_info: None,
        locale: None,
    };
    
    let result = server.initialize(params).await;
    assert!(result.is_ok());
    
    let init_result = result.unwrap();
    assert!(init_result.capabilities.text_document_sync.is_some());
    assert!(init_result.capabilities.hover_provider.is_some());
    assert!(init_result.capabilities.completion_provider.is_some());
}

#[tokio::test]
async fn test_document_operations() {
    let (service, _socket) = LspService::new(|client| ElangLanguageServer::new(client));
    let server = service.inner();
    
    // Test with valid Elang code
    let valid_code = r#"
let x = 42;
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
"#;
    
    let uri = Url::parse("file:///test.elang").unwrap();
    let params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "elang".to_string(),
            version: 1,
            text: valid_code.to_string(),
        },
    };
    
    // Test that document opening doesn't panic
    server.did_open(params).await;
    
    // Test document close
    let close_params = DidCloseTextDocumentParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
    };
    
    server.did_close(close_params).await;
}

#[tokio::test]
async fn test_hover_functionality() {
    let (service, _socket) = LspService::new(|client| ElangLanguageServer::new(client));
    let server = service.inner();
    
    let code = r#"
let x = 42;
fn test() -> i32 {
    return x;
}
"#;
    
    let uri = Url::parse("file:///test.elang").unwrap();
    let params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "elang".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    
    server.did_open(params).await;
    
    // Test hover on keyword
    let hover_params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position { line: 1, character: 0 }, // "let" keyword
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    };
    
    let hover_result = server.hover(hover_params).await;
    assert!(hover_result.is_ok());
    
    let hover = hover_result.unwrap();
    assert!(hover.is_some());
}

#[tokio::test]
async fn test_completion_functionality() {
    let (service, _socket) = LspService::new(|client| ElangLanguageServer::new(client));
    let server = service.inner();
    
    let code = r#"
let x = 42;
fn test() {
    
}
"#;
    
    let uri = Url::parse("file:///test.elang").unwrap();
    let params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "elang".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    
    server.did_open(params).await;
    
    // Test completion inside function body
    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position { line: 3, character: 4 }, // Inside function body
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let completion_result = server.completion(completion_params).await;
    assert!(completion_result.is_ok());
    
    let completions = completion_result.unwrap();
    assert!(completions.is_some());
    
    if let Some(CompletionResponse::Array(items)) = completions {
        assert!(!items.is_empty());
        
        // Should contain keywords like "let", "if", "return", etc.
        let labels: Vec<String> = items.iter().map(|item| item.label.clone()).collect();
        assert!(labels.contains(&"let".to_string()));
        assert!(labels.contains(&"if".to_string()));
        assert!(labels.contains(&"return".to_string()));
    }
}