use crate::compiler;
use crate::lexer;
use crate::parser;
use crate::tc::{self, TypeCheckError};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use super::util::{
    compile_error_message, completion_context, completion_items, diagnostic_for_span,
    document_symbols, find_references, find_target_at_position, hover_at_position,
    position_in_span, semantic_tokens, semantic_tokens_legend, span_to_range,
};

#[derive(Default)]
struct ServerState {
    files: HashMap<Url, FileState>,
}

struct FileState {
    text: String,
    lsp_info: tc::LspInfo,
    dependencies: Vec<PathBuf>,
}

pub struct IonLanguageServer {
    client: Client,
    state: Arc<Mutex<ServerState>>,
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
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    ..SignatureHelpOptions::default()
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_tokens_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            ..SemanticTokensOptions::default()
                        },
                    ),
                ),
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

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Ok(mut state) = self.state.lock() {
            state.files.remove(&uri);
        }
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let changed: Vec<PathBuf> = params
            .changes
            .iter()
            .filter_map(|change| change.uri.to_file_path().ok())
            .collect();
        if changed.is_empty() {
            return;
        }

        let to_refresh: Vec<(Url, String)> = {
            let state = match self.state.lock() {
                Ok(state) => state,
                Err(_) => return,
            };
            state
                .files
                .iter()
                .filter(|(_, file)| {
                    file.dependencies.iter().any(|dep| {
                        changed
                            .iter()
                            .any(|changed_path| paths_match(dep, changed_path))
                    })
                })
                .map(|(uri, file)| (uri.clone(), file.text.clone()))
                .collect()
        };

        for (uri, text) in to_refresh {
            self.check_file(uri, text).await;
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let lsp_info = self.lsp_info_for_uri(&uri);
        let Some(info) = lsp_info else {
            return Ok(None);
        };
        let Some(target) = find_target_at_position(&info, position) else {
            return Ok(None);
        };
        let target_uri = target
            .file
            .as_ref()
            .and_then(|p| Url::from_file_path(p).ok())
            .unwrap_or_else(|| uri.clone());
        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri: target_uri,
            range: span_to_range(&target.span),
        })))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let lsp_info = self.lsp_info_for_uri(&uri);
        let Some(info) = lsp_info else {
            return Ok(None);
        };
        let Some(target) = find_target_at_position(&info, position) else {
            return Ok(Some(vec![]));
        };
        Ok(Some(find_references(&info, &uri, target)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let lsp_info = self.lsp_info_for_uri(&uri);
        let Some(info) = lsp_info else {
            return Ok(None);
        };

        let Some((span, text)) = hover_at_position(&info, position) else {
            return Ok(None);
        };
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(text)),
            range: Some(span_to_range(&span)),
        }))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let text = self.file_text(&uri).unwrap_or_default();
        let lsp_info = self.lsp_info_for_uri(&uri);
        let context = completion_context(&text, position);
        let items = completion_items(lsp_info.as_ref(), &context);
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let lsp_info = self.lsp_info_for_uri(&uri);
        let Some(info) = lsp_info else {
            return Ok(None);
        };

        for (span, sig) in &info.signatures {
            if position_in_span(position, span) {
                return Ok(Some(SignatureHelp {
                    signatures: vec![SignatureInformation {
                        label: sig.label.clone(),
                        documentation: None,
                        parameters: None,
                        active_parameter: sig.active_parameter,
                    }],
                    active_signature: Some(0),
                    active_parameter: sig.active_parameter,
                }));
            }
        }
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let lsp_info = self.lsp_info_for_uri(&uri);
        let Some(info) = lsp_info else {
            return Ok(None);
        };
        Ok(Some(DocumentSymbolResponse::Nested(document_symbols(
            &info,
        ))))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let lsp_info = self.lsp_info_for_uri(&uri);
        let Some(info) = lsp_info else {
            return Ok(None);
        };
        Ok(Some(SemanticTokensResult::Tokens(semantic_tokens(&info))))
    }
}

impl IonLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            state: Arc::new(Mutex::new(ServerState::default())),
        }
    }

    fn lsp_info_for_uri(&self, uri: &Url) -> Option<tc::LspInfo> {
        self.state
            .lock()
            .ok()
            .and_then(|state| state.files.get(uri).map(|file| file.lsp_info.clone()))
    }

    fn file_text(&self, uri: &Url) -> Option<String> {
        self.state
            .lock()
            .ok()
            .and_then(|state| state.files.get(uri).map(|file| file.text.clone()))
    }

    async fn check_file(&self, uri: Url, text: String) {
        let mut diagnostics = Vec::new();

        let mut lexer = lexer::Lexer::new(&text);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(e) => {
                diagnostics.push(Diagnostic {
                    range: Range::default(),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("Lexer error: {e}"),
                    source: Some("ion-lexer".to_string()),
                    ..Default::default()
                });
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
                return;
            }
        };

        let mut parser = parser::Parser::with_source(tokens, &text);
        match parser.parse() {
            Ok(ast) => {
                let mut checker = tc::TypeChecker::new();
                let mut dependencies = Vec::new();
                let program = if let Ok(file_path) = uri.to_file_path() {
                    let (stdlib_paths, project_root) =
                        crate::build::discover_import_config(&file_path);
                    let mut compiler =
                        compiler::Compiler::with_import_config(stdlib_paths, project_root);
                    for import in &ast.imports {
                        dependencies.push(compiler.resolve_import_path(&import.path, &file_path));
                    }
                    let import_errors = compiler.load_imports(&file_path, &ast.imports);
                    for (span, err) in import_errors {
                        diagnostics.push(diagnostic_for_span(span, compile_error_message(&err)));
                    }
                    checker.set_module_exports(compiler.get_module_exports().clone());
                    let mut module_paths = HashMap::new();
                    for import in &ast.imports {
                        let path = compiler.resolve_import_path(&import.path, &file_path);
                        module_paths.insert(import.alias.clone(), path);
                    }
                    checker.set_module_paths(module_paths);
                    compiler.merge_modules(&ast, &file_path)
                } else {
                    ast.clone()
                };

                let (result, tc_errors) =
                    checker.check_program_collecting_with_source(&program, &ast);
                for err in tc_errors {
                    let (span, message) = diagnostic_from_tc_error(&err);
                    diagnostics.push(diagnostic_for_span(span, message));
                }

                if let Ok(mut state) = self.state.lock() {
                    state.files.insert(
                        uri.clone(),
                        FileState {
                            text,
                            lsp_info: result.lsp_info,
                            dependencies,
                        },
                    );
                }
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            }
            Err(err) => {
                let (span, message) = match &err {
                    parser::ParseError::UnexpectedToken {
                        span,
                        expected,
                        got,
                    } => (
                        *span,
                        format!("Unexpected token: expected {expected}, got {got:?}"),
                    ),
                    parser::ParseError::UnexpectedEOF => {
                        (crate::ast::Span::default(), "Unexpected EOF".to_string())
                    }
                    parser::ParseError::Message(msg) => (crate::ast::Span::default(), msg.clone()),
                };
                diagnostics.push(diagnostic_for_span(span, message));
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            }
        }
    }
}

fn paths_match(a: &Path, b: &Path) -> bool {
    a.canonicalize()
        .unwrap_or_else(|_| a.to_path_buf())
        .eq(&b.canonicalize().unwrap_or_else(|_| b.to_path_buf()))
}

fn diagnostic_from_tc_error(err: &TypeCheckError) -> (crate::ast::Span, String) {
    match err {
        TypeCheckError::UndefinedVariable { span, name } => {
            (*span, format!("Undefined variable: {name}"))
        }
        TypeCheckError::TypeMismatch {
            span,
            expected,
            got,
        } => (
            *span,
            format!("Type mismatch: expected {expected}, got {got}"),
        ),
        TypeCheckError::UseAfterMove { span, name } => (*span, format!("Use after move: {name}")),
        TypeCheckError::BorrowConflict {
            span,
            name,
            description,
        } => (
            *span,
            format!("Borrow conflict: cannot borrow '{name}' {description}"),
        ),
        TypeCheckError::ReferenceEscape { span, description } => {
            (*span, format!("Reference escape: {description}"))
        }
        TypeCheckError::ClosureCapture { span, names } => (
            *span,
            format!(
                "Fn literal cannot capture variables from outer scope: {}",
                names.join(", ")
            ),
        ),
        TypeCheckError::TraitBoundNotSatisfied {
            span,
            type_name,
            bound,
            context,
        } => (
            *span,
            format!(
                "Trait bound not satisfied: type '{type_name}' does not satisfy '{bound}' in {context}"
            ),
        ),
        TypeCheckError::UnknownTraitBound { span, bound } => {
            (*span, format!("Unknown trait bound: '{bound}'"))
        }
        TypeCheckError::Message(msg) => (crate::ast::Span::default(), msg.clone()),
    }
}
