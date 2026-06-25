use std::path::{Path, PathBuf};
use std::process::Command;

/// Locate `runtime/` by walking up from the current working directory.
pub fn find_runtime_dir() -> Option<PathBuf> {
    let mut dir = std::env::current_dir().ok()?;
    loop {
        let header = dir.join("runtime").join("ion_runtime.h");
        if header.exists() {
            return Some(dir.join("runtime"));
        }
        if !dir.pop() {
            break;
        }
    }
    None
}

/// Locate runtime relative to a known project root (walks upward).
pub fn find_runtime_dir_from(root: &Path) -> Option<PathBuf> {
    let mut dir = if root.is_dir() {
        root.to_path_buf()
    } else {
        root.parent()?.to_path_buf()
    };

    loop {
        let header = dir.join("runtime").join("ion_runtime.h");
        if header.exists() {
            return Some(dir.join("runtime"));
        }
        if !dir.pop() {
            break;
        }
    }
    find_runtime_dir()
}

pub fn runtime_include_args(runtime_dir: &Path) -> Vec<String> {
    let mut args = vec![
        "-I.".to_string(),
        "-I..".to_string(),
        "-Iruntime".to_string(),
        "-I../runtime".to_string(),
    ];
    if let Some(parent) = runtime_dir.parent() {
        args.push(format!("-I{}", path_for_cc(parent)));
    }
    args.push(format!("-I{}", path_for_cc(runtime_dir)));
    args
}

pub fn cc_binary() -> String {
    std::env::var("CC").unwrap_or_else(|_| "gcc".to_string())
}

/// GCC/Clang on Windows treat backslashes in args as escapes (`\U` in `\Users`, etc.).
fn path_for_cc(path: &Path) -> String {
    let mut s = path.to_string_lossy().to_string();
    if let Some(stripped) = s.strip_prefix(r"\\?\") {
        s = stripped.to_string();
    }
    s.replace('\\', "/")
}

pub fn default_link_libs() -> Vec<String> {
    let mut libs = vec!["-lm".to_string(), "-lpthread".to_string()];
    if cfg!(windows) {
        libs.push("-lws2_32".to_string());
    }
    libs
}

pub fn compile_to_object(
    c_file: &Path,
    object_file: &Path,
    include_dir: &Path,
    runtime_dir: &Path,
    extra_cflags: &[String],
) -> Result<(), String> {
    let cc = cc_binary();

    let mut cmd = Command::new(&cc);
    cmd.arg("-c")
        .arg(path_for_cc(c_file))
        .arg("-o")
        .arg(path_for_cc(object_file))
        .arg("-I.")
        .arg(format!("-I{}", path_for_cc(include_dir)));
    for include in runtime_include_args(runtime_dir) {
        cmd.arg(include);
    }
    for flag in extra_cflags {
        cmd.arg(flag);
    }

    let output = cmd
        .output()
        .map_err(|e| format!("Failed to execute {cc}: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Compilation failed: {stderr}"));
    }

    Ok(())
}

pub fn link_executable(
    object_files: &[PathBuf],
    executable: &Path,
    runtime_dir: Option<&Path>,
    extra_ldflags: &[String],
) -> Result<(), String> {
    let cc = cc_binary();

    let mut args: Vec<String> = vec!["-o".to_string(), path_for_cc(executable)];
    for obj in object_files {
        args.push(path_for_cc(obj));
    }

    if let Some(runtime_dir) = runtime_dir {
        let runtime_c = runtime_dir.join("ion_runtime.c");
        if runtime_c.exists() {
            args.push(path_for_cc(&runtime_c));
        }
    } else if Path::new("runtime/ion_runtime.c").exists() {
        args.push("runtime/ion_runtime.c".to_string());
    } else if Path::new("../runtime/ion_runtime.c").exists() {
        args.push("../runtime/ion_runtime.c".to_string());
    }

    args.extend(default_link_libs());
    args.extend(extra_ldflags.iter().cloned());

    let output = Command::new(&cc)
        .args(&args)
        .output()
        .map_err(|e| format!("Failed to execute {cc}: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Linking failed: {stderr}"));
    }

    Ok(())
}
