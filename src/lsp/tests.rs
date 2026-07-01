//! LSP tests: spawn ion-lsp and check diagnostics over the protocol.

use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use url::Url;

struct LspSession {
    _child: Child,
    writer: mpsc::Sender<String>,
    messages: mpsc::Receiver<Value>,
}

impl LspSession {
    fn spawn() -> Self {
        let bin = ion_lsp_binary();
        let mut child = Command::new(&bin)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .unwrap_or_else(|e| panic!("failed to spawn {}: {e}", bin.display()));

        let stdout = child.stdout.take().expect("stdout");
        let mut stdin = child.stdin.take().expect("stdin");

        let (write_tx, write_rx) = mpsc::channel::<String>();
        thread::spawn(move || {
            while let Ok(payload) = write_rx.recv() {
                if stdin.write_all(payload.as_bytes()).is_err() {
                    break;
                }
            }
        });

        let (msg_tx, msg_rx) = mpsc::channel::<Value>();
        thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            loop {
                match read_lsp_message(&mut reader) {
                    Ok(Some(value)) => {
                        if msg_tx.send(value).is_err() {
                            break;
                        }
                    }
                    Ok(None) => break,
                    Err(_) => break,
                }
            }
        });

        Self {
            _child: child,
            writer: write_tx,
            messages: msg_rx,
        }
    }

    fn send(&self, message: Value) {
        let body = serde_json::to_string(&message).expect("serialize");
        let payload = format!("Content-Length: {}\r\n\r\n{}", body.len(), body);
        self.writer.send(payload).expect("writer thread alive");
    }

    fn request(&self, id: i64, method: &str, params: Value) {
        self.send(json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": params,
        }));
    }

    fn notify(&self, method: &str, params: Value) {
        self.send(json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        }));
    }

    fn wait_for<F>(&self, timeout: Duration, mut pred: F) -> Value
    where
        F: FnMut(&Value) -> bool,
    {
        let deadline = Instant::now() + timeout;
        while Instant::now() < deadline {
            match self.messages.recv_timeout(Duration::from_millis(100)) {
                Ok(value) if pred(&value) => return value,
                Ok(_) => {}
                Err(mpsc::RecvTimeoutError::Timeout) => {}
                Err(mpsc::RecvTimeoutError::Disconnected) => break,
            }
        }
        panic!("timed out waiting for LSP message");
    }

    fn initialize(&self, root: &Path) {
        let root_uri = Url::from_directory_path(root)
            .expect("root uri")
            .to_string();
        self.request(
            1,
            "initialize",
            json!({
                "processId": std::process::id(),
                "rootUri": root_uri,
                "capabilities": {},
            }),
        );
        self.wait_for(Duration::from_secs(5), |msg| {
            msg.get("id").and_then(|v| v.as_i64()) == Some(1)
        });
        self.notify("initialized", json!({}));
    }

    fn open_document(&self, uri: &Url, version: i64, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": uri.to_string(),
                    "languageId": "ion",
                    "version": version,
                    "text": text,
                }
            }),
        );
    }

    fn change_document(&self, uri: &Url, version: i64, text: &str) {
        self.notify(
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": uri.to_string(), "version": version },
                "contentChanges": [{ "text": text }],
            }),
        );
    }

    fn wait_diagnostics(&self, uri: &Url) -> Vec<Value> {
        let uri_string = uri.to_string();
        let msg = self.wait_for(Duration::from_secs(10), |msg| {
            msg.get("method").and_then(|m| m.as_str()) == Some("textDocument/publishDiagnostics")
                && msg
                    .pointer("/params/uri")
                    .and_then(|u| u.as_str())
                    .map(|u| u == uri_string)
                    .unwrap_or(false)
        });
        msg.pointer("/params/diagnostics")
            .and_then(|d| d.as_array())
            .cloned()
            .unwrap_or_default()
    }
}

fn ion_lsp_binary() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ion-lsp") {
        return PathBuf::from(path);
    }
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join(profile)
        .join(format!("ion-lsp{}", std::env::consts::EXE_SUFFIX))
}

fn read_lsp_message<R: BufRead>(reader: &mut R) -> std::io::Result<Option<Value>> {
    let mut content_length: Option<usize> = None;
    loop {
        let mut header = String::new();
        if reader.read_line(&mut header)? == 0 {
            return Ok(None);
        }
        if header == "\r\n" || header == "\n" {
            break;
        }
        if let Some(value) = header.strip_prefix("Content-Length:") {
            content_length = value.trim().parse().ok();
        }
    }

    let content_length = content_length.ok_or_else(|| {
        std::io::Error::new(std::io::ErrorKind::InvalidData, "missing Content-Length")
    })?;

    let mut body = vec![0u8; content_length];
    reader.read_exact(&mut body)?;
    let value: Value = serde_json::from_slice(&body)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
    Ok(Some(value))
}

fn write_fixture(dir: &Path, name: &str, contents: &str) -> (Url, PathBuf) {
    let path = dir.join(name);
    std::fs::write(&path, contents).expect("write fixture");
    let uri = Url::from_file_path(&path).expect("file uri");
    (uri, path)
}

#[test]
fn lsp_reports_type_error_diagnostics() {
    let root = tempfile::tempdir().expect("tempdir");
    let session = LspSession::spawn();
    session.initialize(root.path());

    let source = r#"fn main() -> int {
    let x = Box::new(10);
    let y = x;
    let z = x;
    return 0;
}"#;
    let (uri, _) = write_fixture(root.path(), "type_error.ion", source);
    session.open_document(&uri, 1, source);

    let diags = session.wait_diagnostics(&uri);
    assert!(
        !diags.is_empty(),
        "expected at least one diagnostic, got none"
    );
    let messages: Vec<String> = diags
        .iter()
        .filter_map(|d| d.get("message").and_then(|m| m.as_str()))
        .map(str::to_string)
        .collect();
    assert!(
        messages.iter().any(|m| m.contains("Use after move")),
        "expected Use after move diagnostic, got: {messages:?}"
    );
    let ranges: Vec<&Value> = diags.iter().filter_map(|d| d.get("range")).collect();
    assert!(!ranges.is_empty(), "expected diagnostic ranges");
}

#[test]
fn lsp_clears_stale_diagnostics_after_fix() {
    let root = tempfile::tempdir().expect("tempdir");
    let session = LspSession::spawn();
    session.initialize(root.path());

    let broken = "fn main() -> int { let x = ; return 0; }";
    let fixed = "fn main() -> int { return 0; }";
    let (uri, _) = write_fixture(root.path(), "syntax_fix.ion", broken);
    session.open_document(&uri, 1, broken);

    let broken_diags = session.wait_diagnostics(&uri);
    assert!(
        !broken_diags.is_empty(),
        "expected parse diagnostics for broken source"
    );

    session.change_document(&uri, 2, fixed);
    let fixed_diags = session.wait_diagnostics(&uri);
    assert!(
        fixed_diags.is_empty(),
        "expected stale diagnostics to clear after fix, got: {fixed_diags:?}"
    );
}

#[test]
fn lsp_reports_import_failure_at_import_span() {
    let root = tempfile::tempdir().expect("tempdir");
    let session = LspSession::spawn();
    session.initialize(root.path());

    let source = r#"import "missing_module.ion" as missing;

fn main() -> int {
    return 0;
}"#;
    let (uri, _) = write_fixture(root.path(), "import_error.ion", source);
    session.open_document(&uri, 1, source);

    let diags = session.wait_diagnostics(&uri);
    assert!(!diags.is_empty(), "expected import diagnostic, got none");
    let messages: Vec<String> = diags
        .iter()
        .filter_map(|d| d.get("message").and_then(|m| m.as_str()))
        .map(str::to_string)
        .collect();
    assert!(
        messages.iter().any(|m| m.contains("file not found")),
        "expected file not found import diagnostic, got: {messages:?}"
    );

    let import_line = source
        .lines()
        .position(|line| line.starts_with("import "))
        .expect("import line") as u64;
    let import_diag = diags
        .iter()
        .find(|d| {
            d.pointer("/range/start/line")
                .and_then(|l| l.as_u64())
                .map(|l| l == import_line)
                .unwrap_or(false)
        })
        .expect("diagnostic at import span");
    assert!(
        import_diag
            .pointer("/range/start/character")
            .and_then(|c| c.as_u64())
            .is_some(),
        "expected import diagnostic range: {import_diag:?}"
    );
}
