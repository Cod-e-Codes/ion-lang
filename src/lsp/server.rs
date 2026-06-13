use crate::compiler;
use crate::lexer;
use crate::parser;
use crate::tc::{self, TypeCheckError, type_to_string};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

const KEYWORDS: &[&str] = &[
    "fn", "let", "mut", "struct", "enum", "if", "else", "return", "break", "continue", "while",
    "for", "loop", "match", "spawn", "defer", "unsafe", "pub", "import", "extern", "type", "as",
    "true", "false", "int", "bool", "String", "Vec", "Box", "channel", "send", "recv",
];

const BUILTINS: &[&str] = &[
    "Vec::new",
    "Vec::push",
    "Vec::pop",
    "Vec::len",
    "Vec::get",
    "Vec::set",
    "String::new",
    "String::from",
    "String::len",
    "String::push_str",
    "String::push_byte",
    "Box::new",
    "Box::unwrap",
    "channel",
    "send",
    "recv",
];

pub struct IonLanguageServer {
    pub client: Client,
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
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    ..CompletionOptions::default()
                }),
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

        let lsp_info = {
            if let Ok(cache) = self.file_cache.lock() {
                cache.get(&uri).cloned()
            } else {
                None
            }
        };

        if let Some(info) = lsp_info {
            let target_line = (position.line + 1) as usize;
            let target_column = (position.character + 1) as usize;

            for (reference_span, target) in &info.references {
                if reference_span.line == target_line
                    && target_column >= reference_span.column
                    && target_column
                        < reference_span.column + (reference_span.end - reference_span.start)
                {
                    let target_uri = target
                        .file
                        .as_ref()
                        .and_then(|p| Url::from_file_path(p).ok())
                        .unwrap_or_else(|| uri.clone());
                    let location = Location {
                        uri: target_uri,
                        range: span_to_range(&target.span),
                    };
                    return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                }
            }
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let lsp_info = {
            if let Ok(cache) = self.file_cache.lock() {
                cache.get(&uri).cloned()
            } else {
                None
            }
        };

        let Some(info) = lsp_info else {
            return Ok(None);
        };

        let target_line = (position.line + 1) as usize;
        let target_column = (position.character + 1) as usize;

        for (span, ty) in &info.types {
            if span.line == target_line
                && target_column >= span.column
                && target_column < span.column + (span.end - span.start)
            {
                return Ok(Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(format!(
                        "`{}`",
                        type_to_string(ty)
                    ))),
                    range: Some(span_to_range(span)),
                }));
            }
        }

        for (span, doc) in &info.hover_docs {
            if span.line == target_line
                && target_column >= span.column
                && target_column < span.column + (span.end - span.start)
            {
                return Ok(Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(doc.clone())),
                    range: Some(span_to_range(span)),
                }));
            }
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let lsp_info = {
            if let Ok(cache) = self.file_cache.lock() {
                cache.get(&uri).cloned()
            } else {
                None
            }
        };

        let mut items: Vec<CompletionItem> = KEYWORDS
            .iter()
            .map(|kw| CompletionItem {
                label: kw.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            })
            .collect();

        items.extend(BUILTINS.iter().map(|name| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            ..Default::default()
        }));

        if let Some(info) = lsp_info {
            for symbol in &info.completion_symbols {
                items.push(CompletionItem {
                    label: symbol.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    ..Default::default()
                });
            }
        }

        let _ = position;
        Ok(Some(CompletionResponse::Array(items)))
    }
}

impl IonLanguageServer {
    async fn check_file(&self, uri: Url, text: String) {
        let mut lexer = lexer::Lexer::new(&text);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(e) => {
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

        let mut parser = parser::Parser::new(tokens);
        match parser.parse() {
            Ok(ast) => {
                let mut checker = tc::TypeChecker::new();

                if let Ok(file_path) = uri.to_file_path() {
                    let mut compiler = compiler::Compiler::new();
                    if compiler.register_imports(&file_path, &ast.imports).is_ok() {
                        checker.set_module_exports(compiler.get_module_exports().clone());
                        let mut module_paths = HashMap::new();
                        for import in &ast.imports {
                            let path = compiler.resolve_import_path(&import.path, &file_path);
                            module_paths.insert(import.alias.clone(), path);
                        }
                        checker.set_module_paths(module_paths);
                    }
                }

                let (result, tc_errors) = checker.check_program_collecting(&ast);
                if tc_errors.is_empty() {
                    if let Ok(mut cache) = self.file_cache.lock() {
                        cache.insert(uri.clone(), result.lsp_info);
                    }
                    self.client.publish_diagnostics(uri, vec![], None).await;
                } else {
                    let diagnostics: Vec<Diagnostic> = tc_errors
                        .iter()
                        .map(|err| {
                            let (span, message) = diagnostic_from_tc_error(err);
                            diagnostic_for_span(span, message)
                        })
                        .collect();
                    if let Ok(mut cache) = self.file_cache.lock() {
                        cache.insert(uri.clone(), result.lsp_info);
                    }
                    self.client
                        .publish_diagnostics(uri, diagnostics, None)
                        .await;
                }
            }
            Err(err) => {
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
                self.client
                    .publish_diagnostics(uri, vec![diagnostic_for_span(span, message)], None)
                    .await;
            }
        }
    }
}

fn span_to_range(span: &crate::ast::Span) -> Range {
    Range {
        start: Position {
            line: (span.line.saturating_sub(1)) as u32,
            character: (span.column.saturating_sub(1)) as u32,
        },
        end: Position {
            line: (span.line.saturating_sub(1)) as u32,
            character: (span.end.saturating_sub(span.start) + span.column.saturating_sub(1)) as u32,
        },
    }
}

fn diagnostic_from_tc_error(err: &TypeCheckError) -> (crate::ast::Span, String) {
    match err {
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
        TypeCheckError::UseAfterMove { span, name } => (*span, format!("Use after move: {}", name)),
        TypeCheckError::ReferenceEscape { span, description } => {
            (*span, format!("Reference escape: {}", description))
        }
        TypeCheckError::Message(msg) => (crate::ast::Span::default(), msg.clone()),
    }
}

fn diagnostic_for_span(span: crate::ast::Span, message: String) -> Diagnostic {
    Diagnostic {
        range: span_to_range(&span),
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        source: Some("ion".to_string()),
        ..Default::default()
    }
}
