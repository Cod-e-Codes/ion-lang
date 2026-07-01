use crate::ast::Span;
use crate::compiler::CompileError;
use crate::tc::{
    LspCompletionItem, LspDocumentSymbol, LspInfo, LspSymbolKind, LspTarget, type_to_string,
};
use tower_lsp::lsp_types::*;

pub const KEYWORDS: &[&str] = &[
    "fn", "let", "mut", "struct", "enum", "if", "else", "return", "break", "continue", "while",
    "for", "in", "loop", "match", "spawn", "defer", "unsafe", "pub", "import", "extern", "type",
    "as", "true", "false", "int", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "uint",
    "f32", "f64", "bool", "void", "String", "Vec", "Box", "Sender", "Receiver", "channel", "send",
    "recv",
];

pub const BUILTINS: &[&str] = &[
    "Vec::new",
    "Vec::with_capacity",
    "Vec::push",
    "Vec::pop",
    "Vec::len",
    "Vec::capacity",
    "Vec::get",
    "Vec::get_ref",
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

pub const BUILTIN_TYPE_MEMBERS: &[(&str, &[&str])] = &[
    (
        "Vec",
        &[
            "new",
            "with_capacity",
            "push",
            "pop",
            "len",
            "capacity",
            "get",
            "get_ref",
            "set",
        ],
    ),
    ("String", &["new", "from", "len", "push_str", "push_byte"]),
    ("Box", &["new", "unwrap"]),
    ("int", &["MIN", "MAX"]),
    ("i8", &["MIN", "MAX"]),
    ("i16", &["MIN", "MAX"]),
    ("i32", &["MIN", "MAX"]),
    ("i64", &["MIN", "MAX"]),
    ("u8", &["MIN", "MAX"]),
    ("u16", &["MIN", "MAX"]),
    ("u32", &["MIN", "MAX"]),
    ("u64", &["MIN", "MAX"]),
    ("uint", &["MIN", "MAX"]),
];

pub fn span_to_range(span: &Span) -> Range {
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

pub fn diagnostic_for_span(span: Span, message: String) -> Diagnostic {
    Diagnostic {
        range: span_to_range(&span),
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        source: Some("ion".to_string()),
        ..Default::default()
    }
}

pub fn compile_error_message(err: &CompileError) -> String {
    err.to_string()
}

pub fn position_in_span(position: Position, span: &Span) -> bool {
    let line = (position.line + 1) as usize;
    let column = (position.character + 1) as usize;
    if span.line != line {
        return false;
    }
    let width = span.end.saturating_sub(span.start);
    column >= span.column && column < span.column + width
}

pub fn hover_at_position(info: &LspInfo, position: Position) -> Option<(Span, String)> {
    let mut matches: Vec<(Span, String, bool)> = Vec::new();
    for (span, ty) in &info.types {
        if position_in_span(position, span) {
            matches.push((*span, format!("`{}`", type_to_string(ty)), true));
        }
    }
    for (span, doc) in &info.hover_docs {
        if position_in_span(position, span) {
            matches.push((*span, doc.clone(), false));
        }
    }
    matches.sort_by_key(|(span, _, is_type)| (*is_type, span.end - span.start));
    matches.first().map(|(span, text, _)| (*span, text.clone()))
}

pub fn find_target_at_position(info: &LspInfo, position: Position) -> Option<&LspTarget> {
    let mut best: Option<(&Span, &LspTarget)> = None;
    for (span, target) in &info.references {
        if position_in_span(position, span) {
            let width = span.end.saturating_sub(span.start);
            if best.is_none_or(|(best_span, _)| width < best_span.end - best_span.start) {
                best = Some((span, target));
            }
        }
    }
    best.map(|(_, target)| target)
}

pub fn find_references(info: &LspInfo, uri: &Url, target: &LspTarget) -> Vec<Location> {
    info.references
        .iter()
        .filter(|(_, t)| spans_equal(&t.span, &target.span) && t.file == target.file)
        .map(|(use_span, _)| Location {
            uri: uri.clone(),
            range: span_to_range(use_span),
        })
        .collect()
}

fn spans_equal(a: &Span, b: &Span) -> bool {
    a.line == b.line && a.column == b.column && a.start == b.start && a.end == b.end
}

pub enum CompletionContext {
    TopLevel { prefix: String },
    ModuleItems { alias: String, prefix: String },
    TypeMembers { type_name: String, prefix: String },
}

pub fn completion_context(text: &str, position: Position) -> CompletionContext {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize).copied().unwrap_or("");
    let byte_col = line
        .char_indices()
        .nth(position.character as usize)
        .map(|(i, _)| i)
        .unwrap_or(line.len());
    let prefix_line = &line[..byte_col];

    if let Some(idx) = prefix_line.rfind("::") {
        let alias = prefix_line[..idx]
            .rsplit(|c: char| !c.is_ascii_alphanumeric() && c != '_')
            .next()
            .unwrap_or("")
            .to_string();
        let partial = prefix_line[idx + 2..].to_string();
        return CompletionContext::ModuleItems {
            alias,
            prefix: partial,
        };
    }

    if let Some(dot_idx) = prefix_line.rfind('.') {
        let before_dot = &prefix_line[..dot_idx];
        let type_name = before_dot
            .rsplit(|c: char| !c.is_ascii_alphanumeric() && c != '_')
            .next()
            .unwrap_or("")
            .to_string();
        let partial = prefix_line[dot_idx + 1..].to_string();
        return CompletionContext::TypeMembers {
            type_name,
            prefix: partial,
        };
    }

    let prefix = prefix_line
        .rsplit(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .next()
        .unwrap_or("")
        .to_string();
    CompletionContext::TopLevel { prefix }
}

fn matches_prefix(label: &str, prefix: &str) -> bool {
    prefix.is_empty() || label.starts_with(prefix)
}

pub fn completion_items(
    info: Option<&LspInfo>,
    context: &CompletionContext,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let prefix = match context {
        CompletionContext::TopLevel { prefix } => prefix.as_str(),
        CompletionContext::ModuleItems { prefix, .. } => prefix.as_str(),
        CompletionContext::TypeMembers { prefix, .. } => prefix.as_str(),
    };

    match context {
        CompletionContext::ModuleItems { alias, .. } => {
            if let Some(info) = info
                && let Some(module_items) = info.module_items.get(alias)
            {
                for item in module_items {
                    if matches_prefix(&item.label, prefix) {
                        items.push(completion_item_from_lsp(item));
                    }
                }
            }
            return items;
        }
        CompletionContext::TypeMembers { type_name, .. } => {
            if let Some(members) = BUILTIN_TYPE_MEMBERS
                .iter()
                .find(|(name, _)| *name == type_name)
            {
                for member in members.1 {
                    let label = format!("{type_name}::{member}");
                    if matches_prefix(member, prefix) {
                        items.push(CompletionItem {
                            label: member.to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some(label),
                            ..Default::default()
                        });
                    }
                }
            }
            if let Some(info) = info {
                if let Some(fields) = info.struct_members.get(type_name) {
                    for field in fields {
                        if matches_prefix(&field.label, prefix) {
                            items.push(completion_item_from_lsp(field));
                        }
                    }
                }
                if let Some(variants) = info.enum_variants.get(type_name) {
                    for variant in variants {
                        if matches_prefix(&variant.label, prefix) {
                            items.push(completion_item_from_lsp(variant));
                        }
                    }
                }
            }
            return items;
        }
        CompletionContext::TopLevel { .. } => {}
    }

    for kw in KEYWORDS {
        if matches_prefix(kw, prefix) {
            items.push(CompletionItem {
                label: kw.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }
    }
    for builtin in BUILTINS {
        if matches_prefix(builtin, prefix) {
            items.push(CompletionItem {
                label: builtin.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                ..Default::default()
            });
        }
    }
    if let Some(info) = info {
        for item in &info.completions {
            if matches_prefix(&item.label, prefix) {
                items.push(completion_item_from_lsp(item));
            }
        }
    }
    items
}

fn completion_item_from_lsp(item: &LspCompletionItem) -> CompletionItem {
    CompletionItem {
        label: item.label.clone(),
        kind: Some(lsp_symbol_kind(item.kind)),
        detail: item.detail.clone(),
        documentation: item.documentation.as_ref().map(|doc| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc.clone(),
            })
        }),
        ..Default::default()
    }
}

fn lsp_symbol_kind(kind: LspSymbolKind) -> CompletionItemKind {
    match kind {
        LspSymbolKind::Function | LspSymbolKind::Builtin => CompletionItemKind::FUNCTION,
        LspSymbolKind::Struct => CompletionItemKind::STRUCT,
        LspSymbolKind::Enum => CompletionItemKind::ENUM,
        LspSymbolKind::Field => CompletionItemKind::FIELD,
        LspSymbolKind::Variant => CompletionItemKind::ENUM_MEMBER,
        LspSymbolKind::TypeAlias => CompletionItemKind::TYPE_PARAMETER,
        LspSymbolKind::Variable => CompletionItemKind::VARIABLE,
        LspSymbolKind::Module => CompletionItemKind::MODULE,
    }
}

pub fn document_symbols(info: &LspInfo) -> Vec<DocumentSymbol> {
    info.document_symbols
        .iter()
        .map(document_symbol_from_lsp)
        .collect()
}

fn document_symbol_from_lsp(sym: &LspDocumentSymbol) -> DocumentSymbol {
    #[allow(deprecated)]
    DocumentSymbol {
        name: sym.name.clone(),
        detail: None,
        kind: document_symbol_kind(sym.kind),
        tags: None,
        deprecated: None,
        range: span_to_range(&sym.span),
        selection_range: span_to_range(&sym.span),
        children: Some(sym.children.iter().map(document_symbol_from_lsp).collect()),
    }
}

fn document_symbol_kind(kind: LspSymbolKind) -> SymbolKind {
    match kind {
        LspSymbolKind::Function | LspSymbolKind::Builtin => SymbolKind::FUNCTION,
        LspSymbolKind::Struct => SymbolKind::STRUCT,
        LspSymbolKind::Enum => SymbolKind::ENUM,
        LspSymbolKind::Field => SymbolKind::FIELD,
        LspSymbolKind::Variant => SymbolKind::ENUM_MEMBER,
        LspSymbolKind::TypeAlias => SymbolKind::TYPE_PARAMETER,
        LspSymbolKind::Variable => SymbolKind::VARIABLE,
        LspSymbolKind::Module => SymbolKind::MODULE,
    }
}

pub fn semantic_tokens(info: &LspInfo) -> SemanticTokens {
    let mut data = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    let mut entries: Vec<(Span, u32)> = Vec::new();
    for sym in &info.document_symbols {
        collect_symbol_tokens(sym, &mut entries);
    }
    for span in info.types.keys() {
        entries.push((*span, 3)); // TYPE
    }
    entries.sort_by_key(|(span, _)| (span.line, span.column));

    for (span, token_type) in entries {
        let line = (span.line.saturating_sub(1)) as u32;
        let start = (span.column.saturating_sub(1)) as u32;
        let length = (span.end - span.start) as u32;
        let delta_line = line.saturating_sub(prev_line);
        let delta_start = if delta_line == 0 {
            start.saturating_sub(prev_start)
        } else {
            start
        };
        data.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: 0,
        });
        prev_line = line;
        prev_start = start;
    }

    SemanticTokens {
        data,
        result_id: None,
    }
}

fn collect_symbol_tokens(sym: &LspDocumentSymbol, out: &mut Vec<(Span, u32)>) {
    let token_type = match sym.kind {
        LspSymbolKind::Function | LspSymbolKind::Builtin => 0,
        LspSymbolKind::Struct => 1,
        LspSymbolKind::Enum => 2,
        LspSymbolKind::Field | LspSymbolKind::Variant => 6,
        LspSymbolKind::TypeAlias => 3,
        LspSymbolKind::Variable => 4,
        LspSymbolKind::Module => 5,
    };
    out.push((sym.span, token_type));
    for child in &sym.children {
        collect_symbol_tokens(child, out);
    }
}

pub fn semantic_tokens_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::FUNCTION,
            SemanticTokenType::STRUCT,
            SemanticTokenType::ENUM,
            SemanticTokenType::TYPE,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::NAMESPACE,
            SemanticTokenType::PROPERTY,
        ],
        token_modifiers: vec![],
    }
}
