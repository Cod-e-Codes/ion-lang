use ion_compiler::build::{self, BuildError};
use std::env;
use std::path::Path;
use std::process;

fn print_usage() {
    eprintln!("Usage: ion-build [build] [--manifest <path>]");
    eprintln!("       ion-build --version");
    eprintln!();
    eprintln!("Build an Ion project using ion.toml (transpile, compile C, link runtime).");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() >= 2 && (args[1] == "--version" || args[1] == "-V") {
        println!("ion-build {}", env!("CARGO_PKG_VERSION"));
        return;
    }

    let mut manifest_path: Option<String> = None;
    let mut subcommand = "build";

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "build" => {
                subcommand = "build";
                i += 1;
            }
            "--manifest" => {
                if i + 1 >= args.len() {
                    eprintln!("Error: --manifest requires a path");
                    process::exit(1);
                }
                manifest_path = Some(args[i + 1].clone());
                i += 2;
            }
            "--help" | "-h" => {
                print_usage();
                return;
            }
            other => {
                eprintln!("Error: unexpected argument: {other}");
                print_usage();
                process::exit(1);
            }
        }
    }

    if subcommand != "build" {
        eprintln!("Error: unknown subcommand");
        print_usage();
        process::exit(1);
    }

    let cwd = env::current_dir().unwrap_or_else(|_| Path::new(".").to_path_buf());

    let project = if let Some(path) = manifest_path {
        let manifest = Path::new(&path);
        let root = manifest
            .parent()
            .filter(|p| !p.as_os_str().is_empty())
            .unwrap_or(Path::new("."));
        build::Project::from_manifest_file(manifest, root)
    } else {
        build::Project::discover(&cwd)
    };

    let project = match project {
        Ok(p) => p,
        Err(err) => {
            eprintln!("Error: {err}");
            process::exit(1);
        }
    };

    match build::build_project(&project) {
        Ok(result) => {
            println!("Built '{}'", result.executable.display());
        }
        Err(err) => {
            eprintln!("Build failed: {err}");
            match err {
                BuildError::Manifest(_) => process::exit(2),
                BuildError::Compile(_) | BuildError::TypeCheck(_) => process::exit(3),
                BuildError::CCompile(_) | BuildError::Link(_) => process::exit(4),
                BuildError::Io { .. } => process::exit(5),
            }
        }
    }
}
