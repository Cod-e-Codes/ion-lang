use ion_compiler::lsp::server::IonLanguageServer;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| IonLanguageServer { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}
