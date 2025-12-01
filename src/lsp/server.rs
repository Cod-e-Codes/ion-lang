use crate::lexer;
use crate::parser;
use crate::tc::{self, TypeCheckError};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

pub struct IonLanguageServer {
    pub client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for IonLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                definition_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Ion Language Server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.check_file(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            self.check_file(params.text_document.uri, change.text).await;
        }
    }

    async fn goto_definition(
        &self,
        _params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        // TODO: Implement using symbol table
        Ok(None)
    }

    async fn hover(&self, _params: HoverParams) -> Result<Option<Hover>> {
        // TODO: Implement using type checker info
        Ok(None)
    }

    async fn completion(&self, _params: CompletionParams) -> Result<Option<CompletionResponse>> {
        // TODO: Implement autocomplete
        Ok(None)
    }
}

impl IonLanguageServer {
    async fn check_file(&self, uri: Url, text: String) {
        // Lex
        let mut lexer = lexer::Lexer::new(&text);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(e) => {
                // Report lexer error
                let diagnostic = Diagnostic {
                    range: Range::default(),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("Lexer error: {:?}", e),
                    source: Some("ion-lexer".to_string()),
                    ..Default::default()
                };
                self.client
                    .publish_diagnostics(uri, vec![diagnostic], None)
                    .await;
                return;
            }
        };

        // Parse
        let mut parser = parser::Parser::new(tokens);
        match parser.parse() {
            Ok(ast) => {
                // Type Check
                let mut checker = tc::TypeChecker::new();
                match checker.check_program(&ast) {
                    Ok(_) => {
                        // Clear diagnostics
                        self.client.publish_diagnostics(uri, vec![], None).await;
                    }
                    Err(err) => {
                        // Extract span and message from TypeCheckError
                        let (span, message) = match &err {
                            TypeCheckError::UndefinedVariable { span, name } => {
                                (*span, format!("Undefined variable: {}", name))
                            }
                            TypeCheckError::TypeMismatch {
                                span,
                                expected,
                                got,
                            } => (
                                *span,
                                format!("Type mismatch: expected {}, got {}", expected, got),
                            ),
                            TypeCheckError::UseAfterMove { span, name } => {
                                (*span, format!("Use after move: {}", name))
                            }
                            TypeCheckError::ReferenceEscape { span, description } => {
                                (*span, format!("Reference escape: {}", description))
                            }
                            TypeCheckError::Message(msg) => {
                                (crate::ast::Span::default(), msg.clone())
                            }
                        };

                        let diagnostic = Diagnostic {
                            range: Range {
                                start: Position {
                                    line: (span.line.saturating_sub(1)) as u32,
                                    character: (span.column.saturating_sub(1)) as u32,
                                },
                                end: Position {
                                    line: (span.line.saturating_sub(1)) as u32,
                                    character: (span.end.saturating_sub(span.start)
                                        + span.column.saturating_sub(1))
                                        as u32,
                                },
                            },
                            severity: Some(DiagnosticSeverity::ERROR),
                            message,
                            source: Some("ion-tc".to_string()),
                            ..Default::default()
                        };
                        self.client
                            .publish_diagnostics(uri, vec![diagnostic], None)
                            .await;
                    }
                }
            }
            Err(err) => {
                // Report parse error
                let (span, message) = match &err {
                    parser::ParseError::UnexpectedToken {
                        span,
                        expected,
                        got,
                    } => (
                        *span,
                        format!("Unexpected token: expected {}, got {:?}", expected, got),
                    ),
                    parser::ParseError::UnexpectedEOF => {
                        (crate::ast::Span::default(), "Unexpected EOF".to_string())
                    }
                    parser::ParseError::Message(msg) => (crate::ast::Span::default(), msg.clone()),
                };

                let diagnostic = Diagnostic {
                    range: Range {
                        start: Position {
                            line: (span.line.saturating_sub(1)) as u32,
                            character: (span.column.saturating_sub(1)) as u32,
                        },
                        end: Position {
                            line: (span.line.saturating_sub(1)) as u32,
                            character: (span.end.saturating_sub(span.start)
                                + span.column.saturating_sub(1))
                                as u32,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    message,
                    source: Some("ion-parser".to_string()),
                    ..Default::default()
                };
                self.client
                    .publish_diagnostics(uri, vec![diagnostic], None)
                    .await;
            }
        }
    }
}
