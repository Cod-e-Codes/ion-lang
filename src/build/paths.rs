use std::env;
use std::path::{Path, PathBuf};

/// Collect stdlib search directories in deterministic priority order.
///
/// 1. Paths from `ion.toml` `stdlib_paths` (relative to project root)
/// 2. `ION_STDLIB` environment variable entries
/// 3. `{project_root}/stdlib` when that directory exists
/// 4. Install-relative `stdlib/` next to the running executable (walk up to repo root)
pub fn collect_stdlib_paths(project_root: &Path, manifest_paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    let mut seen = Vec::new();

    let mut push_unique = |path: PathBuf| {
        let canonical = path.canonicalize().unwrap_or(path);
        if canonical.is_dir() && !seen.contains(&canonical) {
            seen.push(canonical.clone());
            paths.push(canonical);
        }
    };

    for rel in manifest_paths {
        let abs = if rel.is_absolute() {
            rel.clone()
        } else {
            project_root.join(rel)
        };
        push_unique(abs);
    }

    if let Ok(env_val) = env::var("ION_STDLIB") {
        for part in env_val.split(if cfg!(windows) { ';' } else { ':' }) {
            let part = part.trim();
            if !part.is_empty() {
                push_unique(PathBuf::from(part));
            }
        }
    }

    let mut dir = project_root.to_path_buf();
    loop {
        push_unique(dir.join("stdlib"));
        if !dir.pop() {
            break;
        }
    }

    if let Ok(exe) = env::current_exe()
        && let Some(exe_dir) = exe.parent()
    {
        push_unique(exe_dir.join("stdlib"));
        let mut dir = exe_dir.to_path_buf();
        while dir.pop() {
            push_unique(dir.join("stdlib"));
        }
    }

    paths
}

fn with_ion_extension(path: PathBuf) -> PathBuf {
    if path.extension().map(|e| e == "ion").unwrap_or(false) {
        path
    } else {
        path.with_extension("ion")
    }
}

fn exists_ion(path: &Path) -> bool {
    path.is_file()
}

/// Resolve an import string to a filesystem path.
///
/// Order:
/// 1. Explicit relative (`./`, `../`)
/// 2. Same directory as the importing file
/// 3. Each stdlib search path (`stdlib/io.ion` and `io.ion` variants)
/// 4. Project root joined with the import path
pub fn resolve_import_path(
    import_path: &str,
    from_file: &Path,
    stdlib_paths: &[PathBuf],
    project_root: Option<&Path>,
) -> PathBuf {
    let from_parent = from_file.parent().unwrap_or(Path::new("."));

    if let Some(stripped) = import_path.strip_prefix("./") {
        return canonicalize_import(with_ion_extension(from_parent.join(stripped)));
    }
    if let Some(stripped) = import_path.strip_prefix("../") {
        return canonicalize_import(with_ion_extension(from_parent.join("..").join(stripped)));
    }

    let same_dir = with_ion_extension(from_parent.join(import_path));
    if exists_ion(&same_dir) {
        return canonicalize_import(same_dir);
    }

    let stripped_stdlib = import_path.strip_prefix("stdlib/").unwrap_or(import_path);

    for search in stdlib_paths {
        for candidate in [
            with_ion_extension(search.join(import_path)),
            with_ion_extension(search.join(stripped_stdlib)),
        ] {
            if exists_ion(&candidate) {
                return canonicalize_import(candidate);
            }
        }
    }

    if let Some(root) = project_root {
        let root_candidate = with_ion_extension(root.join(import_path));
        if exists_ion(&root_candidate) {
            return canonicalize_import(root_candidate);
        }
    }

    canonicalize_import(same_dir)
}

fn canonicalize_import(path: PathBuf) -> PathBuf {
    path.canonicalize().unwrap_or(path)
}

fn normalize_path_display(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

/// Repo root for portable paths: directory containing `Cargo.toml`.
pub fn find_repo_root(start: &Path) -> Option<PathBuf> {
    let start = start.canonicalize().unwrap_or_else(|_| start.to_path_buf());
    let mut dir = if start.is_dir() {
        start
    } else {
        start.parent()?.to_path_buf()
    };

    loop {
        if dir.join("Cargo.toml").is_file() {
            return Some(dir);
        }
        if !dir.pop() {
            break;
        }
    }
    None
}

/// Source path for generated C banners: relative to cwd or repo root, never a home directory.
pub fn portable_source_label(path: &Path) -> String {
    let absolute = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());

    if path.is_relative() {
        return normalize_path_display(path);
    }

    if let Ok(cwd) = std::env::current_dir() {
        let cwd = cwd.canonicalize().unwrap_or(cwd);
        if let Ok(rel) = absolute.strip_prefix(&cwd) {
            return normalize_path_display(rel);
        }
    }

    if let Some(repo) = find_repo_root(&absolute)
        && let Ok(rel) = absolute.strip_prefix(&repo)
    {
        return normalize_path_display(rel);
    }

    absolute
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| normalize_path_display(&absolute))
}

/// Import resolution config for a source file (shared by CLI, LSP, and ion build).
pub fn discover_import_config(from_file: &Path) -> (Vec<PathBuf>, Option<PathBuf>) {
    if let Some(root) = find_project_root(from_file) {
        let manifest_paths = crate::build::manifest::Project::from_root(&root)
            .map(|p| p.manifest.stdlib_paths)
            .unwrap_or_default();
        return (collect_stdlib_paths(&root, &manifest_paths), Some(root));
    }

    let start = from_file
        .canonicalize()
        .unwrap_or_else(|_| from_file.to_path_buf());
    let mut dir = if start.is_dir() {
        start
    } else {
        start.parent().unwrap_or(Path::new(".")).to_path_buf()
    };

    loop {
        if dir.join("stdlib").is_dir() {
            return (collect_stdlib_paths(&dir, &[]), Some(dir));
        }
        if !dir.pop() {
            break;
        }
    }

    let fallback = from_file.parent().unwrap_or(Path::new(".")).to_path_buf();
    (collect_stdlib_paths(&fallback, &[]), None)
}

/// Walk upward from `start` looking for `ion.toml`.
pub fn find_project_root(start: &Path) -> Option<PathBuf> {
    let start = start.canonicalize().unwrap_or_else(|_| start.to_path_buf());
    let mut dir = if start.is_dir() {
        start
    } else {
        start.parent()?.to_path_buf()
    };

    loop {
        if dir.join("ion.toml").is_file() {
            return Some(dir);
        }
        if !dir.pop() {
            break;
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn resolve_stdlib_import_from_nested_file() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let stdlib_paths = collect_stdlib_paths(root, &[]);
        let from = root.join("examples/data_lib/main.ion");
        let resolved = resolve_import_path("stdlib/io.ion", &from, &stdlib_paths, Some(root));
        assert_eq!(resolved, root.join("stdlib/io.ion").canonicalize().unwrap());
    }

    #[test]
    fn resolve_same_directory_import() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let stdlib_paths = collect_stdlib_paths(root, &[]);
        let from = root.join("examples/data_lib/main.ion");
        let resolved = resolve_import_path("catalog.ion", &from, &stdlib_paths, Some(root));
        assert_eq!(
            resolved,
            root.join("examples/data_lib/catalog.ion")
                .canonicalize()
                .unwrap()
        );
    }

    #[test]
    fn portable_source_label_strips_absolute_path_to_repo_relative() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let abs = root.join("examples/hello_world_safe/hello_world_safe.ion");
        let label = portable_source_label(&abs);
        assert_eq!(label, "examples/hello_world_safe/hello_world_safe.ion");
        assert!(!label.contains("Users"));
    }

    #[test]
    fn find_project_root_walks_up() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let nested = root.join("examples/data_lib");
        assert_eq!(
            find_project_root(&nested),
            Some(nested.canonicalize().unwrap())
        );
    }
}
