use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::cgen;
use crate::compiler;
use crate::ir;
use crate::tc;

use super::c_toolchain::{compile_to_object, find_runtime_dir_from, link_executable};
use super::manifest::{BuildMode, ManifestError, Project};
use super::paths::portable_source_label;

#[derive(Debug)]
pub enum BuildError {
    Manifest(ManifestError),
    Compile(compiler::CompileError),
    TypeCheck(String),
    Io { path: PathBuf, message: String },
    CCompile(String),
    Link(String),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Manifest(err) => write!(f, "{err}"),
            BuildError::Compile(err) => write!(f, "{err}"),
            BuildError::TypeCheck(msg) => write!(f, "{msg}"),
            BuildError::Io { path, message } => {
                write!(f, "IO error ({}): {message}", path.display())
            }
            BuildError::CCompile(msg) => write!(f, "{msg}"),
            BuildError::Link(msg) => write!(f, "{msg}"),
        }
    }
}

impl From<ManifestError> for BuildError {
    fn from(value: ManifestError) -> Self {
        BuildError::Manifest(value)
    }
}

impl From<compiler::CompileError> for BuildError {
    fn from(value: compiler::CompileError) -> Self {
        BuildError::Compile(value)
    }
}

pub struct BuildResult {
    pub executable: PathBuf,
}

pub fn build_project(project: &Project) -> Result<BuildResult, BuildError> {
    let main_path = project.main_path();
    let output_dir = project.output_dir();

    if !project.manifest.emit_in_source {
        fs::create_dir_all(&output_dir).map_err(|e| BuildError::Io {
            path: output_dir.clone(),
            message: e.to_string(),
        })?;
    }

    let runtime_dir = find_runtime_dir_from(&project.root).ok_or_else(|| BuildError::Io {
        path: project.root.join("runtime"),
        message: "runtime/ion_runtime.h not found".to_string(),
    })?;

    match project.manifest.mode {
        BuildMode::Single => build_single(project, &main_path, &output_dir, &runtime_dir),
        BuildMode::Multi => build_multi(project, &main_path, &output_dir, &runtime_dir),
    }
}

fn abs_path(path: PathBuf) -> PathBuf {
    path.canonicalize().unwrap_or(path)
}

fn type_check(
    compiler: &compiler::Compiler,
    ast: &crate::ast::Program,
    main_path: &Path,
) -> Result<(), BuildError> {
    let mut checker = tc::TypeChecker::new();
    checker.set_module_exports(compiler.get_module_exports().clone());
    let merged = compiler.merge_modules(ast, main_path);
    let (_result, errors) = checker.check_program_collecting(&merged);
    if !errors.is_empty() {
        return Err(BuildError::TypeCheck(tc::format_type_errors(&errors)));
    }
    Ok(())
}

fn build_single(
    project: &Project,
    main_path: &Path,
    output_dir: &Path,
    runtime_dir: &Path,
) -> Result<BuildResult, BuildError> {
    let mut compiler = compiler::Compiler::with_import_config(
        project.stdlib_paths.clone(),
        Some(project.root.clone()),
    );
    let ast = compiler.parse_module(main_path)?;
    type_check(&compiler, &ast, main_path)?;

    let merged = compiler.merge_modules(&ast, main_path);
    let ir = ir::IRBuilder::build(&merged);
    let mut codegen = cgen::Codegen::new();
    let source_label = portable_source_label(main_path);
    let c_code = codegen.generate(&ir, &source_label);

    let stem = main_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");
    let c_file = abs_path(output_dir.join(format!("{stem}.c")));
    fs::write(&c_file, c_code).map_err(|e| BuildError::Io {
        path: c_file.clone(),
        message: e.to_string(),
    })?;

    let object_file = abs_path(output_dir.join(format!("{stem}.o")));
    let out_abs = abs_path(output_dir.to_path_buf());
    compile_to_object(
        &c_file,
        &object_file,
        &out_abs,
        runtime_dir,
        &project.manifest.effective_cflags(),
    )
    .map_err(BuildError::CCompile)?;

    let executable = abs_path(project.executable_path());
    link_executable(
        &[object_file],
        &executable,
        Some(runtime_dir),
        &project.manifest.ldflags,
    )
    .map_err(BuildError::Link)?;

    Ok(BuildResult { executable })
}

fn build_multi(
    project: &Project,
    main_path: &Path,
    output_dir: &Path,
    runtime_dir: &Path,
) -> Result<BuildResult, BuildError> {
    let mut compiler = compiler::Compiler::with_import_config(
        project.stdlib_paths.clone(),
        Some(project.root.clone()),
    );
    let ast = compiler.parse_module(main_path)?;
    type_check(&compiler, &ast, main_path)?;

    let modules = compiler.get_modules();
    let module_aliases = compiler.import_aliases_from_main(main_path, &ast);

    let mut module_names: HashMap<PathBuf, String> = HashMap::new();
    for module_path in modules.keys() {
        let file_stem = module_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("module")
            .to_string();
        let canonical = module_path
            .canonicalize()
            .unwrap_or_else(|_| module_path.clone());
        module_names.insert(canonical, file_stem);
    }

    let out_abs = abs_path(output_dir.to_path_buf());
    let mut compile_jobs = Vec::new();

    for (module_path, module_program) in modules {
        let ir = ir::IRBuilder::build(module_program);
        let file_stem = module_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("module")
            .to_string();
        let canonical_module = module_path
            .canonicalize()
            .unwrap_or_else(|_| module_path.clone());
        let symbol_prefix = module_aliases
            .get(&canonical_module)
            .cloned()
            .unwrap_or_else(|| file_stem.clone());

        let mut import_names = Vec::new();
        for import in &module_program.imports {
            let import_path = compiler.resolve_import_path(&import.path, module_path);
            let canonical_import = import_path.canonicalize().unwrap_or(import_path);
            if let Some(imported_module_name) = module_names.get(&canonical_import) {
                import_names.push(imported_module_name.clone());
            }
        }

        let mut codegen = cgen::Codegen::new();
        let source_ion = portable_source_label(module_path);
        let c_code = codegen.generate_module_source(
            &ir,
            &symbol_prefix,
            &source_ion,
            &import_names,
            &file_stem,
        );
        let c_file = abs_path(output_dir.join(format!("{file_stem}.c")));
        fs::write(&c_file, c_code).map_err(|e| BuildError::Io {
            path: c_file.clone(),
            message: e.to_string(),
        })?;

        let header_code =
            codegen.generate_module_header(module_program, &symbol_prefix, &file_stem);
        let h_file = output_dir.join(format!("{file_stem}.h"));
        fs::write(&h_file, header_code).map_err(|e| BuildError::Io {
            path: h_file,
            message: e.to_string(),
        })?;

        compile_jobs.push((c_file, abs_path(output_dir.join(format!("{file_stem}.o")))));
    }

    let mut object_files = Vec::new();
    for (c_file, object_file) in compile_jobs {
        compile_to_object(
            &c_file,
            &object_file,
            &out_abs,
            runtime_dir,
            &project.manifest.effective_cflags(),
        )
        .map_err(BuildError::CCompile)?;
        object_files.push(object_file);
    }

    let executable = abs_path(project.executable_path());
    link_executable(
        &object_files,
        &executable,
        Some(runtime_dir),
        &project.manifest.ldflags,
    )
    .map_err(BuildError::Link)?;

    Ok(BuildResult { executable })
}
