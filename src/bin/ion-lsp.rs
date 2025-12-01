use ion_compiler::lsp::server::IonLanguageServer;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| IonLanguageServer {
        client,
        file_cache: Arc::new(Mutex::new(HashMap::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
