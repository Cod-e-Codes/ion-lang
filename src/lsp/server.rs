use crate::lexer;
use crate::parser;
use crate::tc::{self, TypeCheckError};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

pub struct IonLanguageServer {
    pub client: Client,
    // Cache of LSP info per file
    pub file_cache: Arc<Mutex<HashMap<Url, tc::LspInfo>>>,
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
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get LSP info for this file
        let lsp_info = {
            if let Ok(cache) = self.file_cache.lock() {
                cache.get(&uri).cloned()
            } else {
                None
            }
        };

        if let Some(info) = lsp_info {
            // Find the symbol at the requested position
            // LSP positions are 0-indexed, but our Spans are 1-indexed
            let target_line = (position.line + 1) as usize;
            let target_column = (position.character + 1) as usize;

            // Search through references to find one at this position
            for (reference_span, definition_span) in &info.references {
                // Check if the position is within this reference span
                if reference_span.line == target_line
                    && target_column >= reference_span.column
                    && target_column
                        < reference_span.column + (reference_span.end - reference_span.start)
                {
                    // Found it! Return the definition location
                    let location = Location {
                        uri,
                        range: Range {
                            start: Position {
                                line: (definition_span.line.saturating_sub(1)) as u32,
                                character: (definition_span.column.saturating_sub(1)) as u32,
                            },
                            end: Position {
                                line: (definition_span.line.saturating_sub(1)) as u32,
                                character: (definition_span
                                    .end
                                    .saturating_sub(definition_span.start)
                                    + definition_span.column.saturating_sub(1))
                                    as u32,
                            },
                        },
                    };
                    return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                }
            }
        }

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
                    Ok(result) => {
                        // Store LSP info for goto_definition
                        if let Ok(mut cache) = self.file_cache.lock() {
                            cache.insert(uri.clone(), result.lsp_info);
                        }
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
