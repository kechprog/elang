use elang_lsp::ElangLanguageServer;
use tower_lsp::{LspService, Server};
use tracing_subscriber;

#[tokio::main]
async fn main() {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    // Create the language server
    let (service, socket) = LspService::new(|client| ElangLanguageServer::new(client));
    
    // Start the server
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
        .serve(service)
        .await;
}