use std::fs;
use std::path::{Path, PathBuf};

use super::paths::{collect_stdlib_paths, find_project_root};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuildMode {
    Single,
    Multi,
}

impl BuildMode {
    pub fn parse(s: &str) -> Result<Self, String> {
        match s {
            "single" => Ok(BuildMode::Single),
            "multi" => Ok(BuildMode::Multi),
            other => Err(format!(
                "invalid mode '{other}' in ion.toml (expected 'single' or 'multi')"
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Manifest {
    pub name: String,
    pub main: PathBuf,
    pub output: String,
    pub mode: BuildMode,
    pub out_dir: PathBuf,
    pub stdlib_paths: Vec<PathBuf>,
    pub cflags: Vec<String>,
    pub ldflags: Vec<String>,
    pub emit_in_source: bool,
}

#[derive(Debug)]
pub enum ManifestError {
    NotFound { start: PathBuf },
    Io { path: PathBuf, message: String },
    Parse { path: PathBuf, message: String },
    Invalid { path: PathBuf, message: String },
}

impl std::fmt::Display for ManifestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ManifestError::NotFound { start } => {
                write!(
                    f,
                    "no ion.toml found (searched upward from {})",
                    start.display()
                )
            }
            ManifestError::Io { path, message } => {
                write!(f, "failed to read {}: {}", path.display(), message)
            }
            ManifestError::Parse { path, message } => {
                write!(f, "failed to parse {}: {}", path.display(), message)
            }
            ManifestError::Invalid { path, message } => {
                write!(f, "invalid {}: {}", path.display(), message)
            }
        }
    }
}

impl std::error::Error for ManifestError {}

pub struct Project {
    pub root: PathBuf,
    pub manifest: Manifest,
    pub stdlib_paths: Vec<PathBuf>,
}

impl Project {
    pub fn discover(start: &Path) -> Result<Self, ManifestError> {
        let start = start.canonicalize().unwrap_or_else(|_| start.to_path_buf());
        let root = find_project_root(&start).ok_or_else(|| ManifestError::NotFound {
            start: start.clone(),
        })?;
        Self::from_root(&root)
    }

    pub fn from_root(root: &Path) -> Result<Self, ManifestError> {
        Self::from_manifest_file(&root.join("ion.toml"), root)
    }

    pub fn from_manifest_file(manifest_path: &Path, root: &Path) -> Result<Self, ManifestError> {
        let manifest = parse_manifest_file(manifest_path, root)?;
        let stdlib_paths = collect_stdlib_paths(root, &manifest.stdlib_paths);
        Ok(Project {
            root: root.to_path_buf(),
            manifest,
            stdlib_paths,
        })
    }

    pub fn main_path(&self) -> PathBuf {
        self.root.join(&self.manifest.main)
    }

    pub fn output_dir(&self) -> PathBuf {
        if self.manifest.emit_in_source {
            self.main_path()
                .parent()
                .unwrap_or(&self.root)
                .to_path_buf()
        } else {
            self.root.join(&self.manifest.out_dir)
        }
    }

    pub fn executable_path(&self) -> PathBuf {
        let name = &self.manifest.output;
        let mut path = self.output_dir().join(name);
        if cfg!(windows) && !name.ends_with(".exe") {
            path.set_extension("exe");
        }
        path
    }
}

fn parse_manifest_file(path: &Path, root: &Path) -> Result<Manifest, ManifestError> {
    let content = fs::read_to_string(path).map_err(|e| ManifestError::Io {
        path: path.to_path_buf(),
        message: e.to_string(),
    })?;

    let table: toml::Table = toml::from_str(&content).map_err(|e| ManifestError::Parse {
        path: path.to_path_buf(),
        message: e.to_string(),
    })?;

    let name = require_string(&table, "name", path)?;
    let main = require_string(&table, "main", path)?;
    let output = require_string(&table, "output", path)?;

    let mode = match table.get("mode") {
        Some(toml::Value::String(s)) => {
            BuildMode::parse(s).map_err(|msg| ManifestError::Invalid {
                path: path.to_path_buf(),
                message: msg,
            })?
        }
        Some(_) => {
            return Err(ManifestError::Invalid {
                path: path.to_path_buf(),
                message: "mode must be a string ('single' or 'multi')".to_string(),
            });
        }
        None => BuildMode::Single,
    };

    let out_dir = match table.get("out_dir") {
        Some(toml::Value::String(s)) => PathBuf::from(s),
        Some(_) => {
            return Err(ManifestError::Invalid {
                path: path.to_path_buf(),
                message: "out_dir must be a string".to_string(),
            });
        }
        None => PathBuf::from("target"),
    };

    let stdlib_paths = parse_path_list(table.get("stdlib_paths"), path, "stdlib_paths")?;
    let cflags = parse_string_list(table.get("cflags"), path, "cflags")?;
    let ldflags = parse_string_list(table.get("ldflags"), path, "ldflags")?;

    let emit_in_source = match table.get("emit_in_source") {
        Some(toml::Value::Boolean(b)) => *b,
        Some(_) => {
            return Err(ManifestError::Invalid {
                path: path.to_path_buf(),
                message: "emit_in_source must be a boolean".to_string(),
            });
        }
        None => false,
    };

    let main_path = root.join(&main);
    if !main_path.is_file() {
        return Err(ManifestError::Invalid {
            path: path.to_path_buf(),
            message: format!("main file not found: {}", main_path.display()),
        });
    }

    Ok(Manifest {
        name,
        main: PathBuf::from(main),
        output,
        mode,
        out_dir,
        stdlib_paths,
        cflags,
        ldflags,
        emit_in_source,
    })
}

fn require_string(table: &toml::Table, key: &str, path: &Path) -> Result<String, ManifestError> {
    match table.get(key) {
        Some(toml::Value::String(s)) => Ok(s.clone()),
        Some(_) => Err(ManifestError::Invalid {
            path: path.to_path_buf(),
            message: format!("{key} must be a string"),
        }),
        None => Err(ManifestError::Invalid {
            path: path.to_path_buf(),
            message: format!("missing required field '{key}'"),
        }),
    }
}

fn parse_string_list(
    value: Option<&toml::Value>,
    path: &Path,
    key: &str,
) -> Result<Vec<String>, ManifestError> {
    let Some(value) = value else {
        return Ok(Vec::new());
    };
    let toml::Value::Array(items) = value else {
        return Err(ManifestError::Invalid {
            path: path.to_path_buf(),
            message: format!("{key} must be an array of strings"),
        });
    };
    let mut out = Vec::new();
    for item in items {
        let toml::Value::String(s) = item else {
            return Err(ManifestError::Invalid {
                path: path.to_path_buf(),
                message: format!("{key} must be an array of strings"),
            });
        };
        out.push(s.clone());
    }
    Ok(out)
}

fn parse_path_list(
    value: Option<&toml::Value>,
    path: &Path,
    key: &str,
) -> Result<Vec<PathBuf>, ManifestError> {
    parse_string_list(value, path, key).map(|items| items.into_iter().map(PathBuf::from).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_root_manifest() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let manifest = parse_manifest_file(&root.join("ion.toml"), root).expect("parse");
        assert_eq!(manifest.name, "hello_world");
        assert_eq!(
            manifest.main,
            PathBuf::from("examples/hello_world_safe/hello_world_safe.ion")
        );
        assert_eq!(manifest.mode, BuildMode::Single);
    }

    #[test]
    fn parse_data_lib_manifest() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/data_lib");
        let manifest = parse_manifest_file(&root.join("ion.toml"), &root).expect("parse data_lib");
        assert_eq!(manifest.mode, BuildMode::Multi);
        assert_eq!(manifest.output, "data_lib");
    }
}
