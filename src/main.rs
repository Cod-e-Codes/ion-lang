use ion_compiler::*;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() >= 2 && (args[1] == "--version" || args[1] == "-V") {
        println!("ion-compiler {}", env!("CARGO_PKG_VERSION"));
        return;
    }

    if args.len() < 2 {
        eprintln!("Usage: ion-compiler [--mode <single|multi>] [--output <name>] <input.ion>");
        eprintln!("       ion-compiler --version");
        eprintln!("Compiles an Ion source file to C code.");
        process::exit(1);
    }

    // Parse command-line arguments
    let mut mode = "single".to_string(); // Default to single-file mode for backward compatibility
    let mut output_name: Option<String> = None;
    let mut input_file: Option<String> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--mode" => {
                if i + 1 < args.len() {
                    mode = args[i + 1].clone();
                    i += 2;
                } else {
                    eprintln!("Error: --mode requires an argument (single or multi)");
                    process::exit(1);
                }
            }
            "--output" => {
                if i + 1 < args.len() {
                    output_name = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    eprintln!("Error: --output requires an argument");
                    process::exit(1);
                }
            }
            arg if arg.ends_with(".ion") => {
                input_file = Some(arg.to_string());
                i += 1;
            }
            _ => {
                eprintln!("Error: Unexpected argument: {}", args[i]);
                process::exit(1);
            }
        }
    }

    let input_file = input_file
        .ok_or_else(|| {
            eprintln!("Error: No input file specified");
            process::exit(1);
        })
        .unwrap();
    let input_path = Path::new(&input_file);

    let (stdlib_paths, project_root) = build::discover_import_config(input_path);

    // Use compiler to parse module and its imports
    let mut compiler = compiler::Compiler::with_import_config(stdlib_paths, project_root.clone());
    let ast = match compiler.parse_module(input_path) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("{}", err);
            process::exit(1);
        }
    };

    // Type check all modules
    let mut checker = tc::TypeChecker::new();
    // Set module exports for qualified name resolution
    let module_exports = compiler.get_module_exports().clone();
    checker.set_module_exports(module_exports);

    // Type check merged program (main + imported modules)
    let merged_program = compiler.merge_modules(&ast, input_path);
    if let Err(err) = checker.check_program(&merged_program) {
        eprintln!("{}", err);
        process::exit(1);
    }

    if mode == "multi" {
        // Multi-file mode: generate separate .c and .h files for each module
        let modules = compiler.get_modules();

        // Build a map of module names for header includes
        let mut module_names: std::collections::HashMap<PathBuf, String> =
            std::collections::HashMap::new();
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

        let module_aliases = compiler.import_aliases_from_main(input_path, &ast);

        // Generate .c and .h files for each module
        let mut object_files = Vec::new();
        for (module_path, module_program) in modules {
            // Build IR for this module
            let ir = ir::IRBuilder::build(module_program);

            // Generate module name from file path
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

            // Collect imported module names for header includes
            let mut import_names = Vec::new();
            for import in &module_program.imports {
                // Resolve import path to get the actual module path
                let import_path = compiler.resolve_import_path(&import.path, module_path);
                let canonical_import = import_path.canonicalize().unwrap_or(import_path);
                if let Some(imported_module_name) = module_names.get(&canonical_import) {
                    import_names.push(imported_module_name.clone());
                }
            }

            // Generate .c file
            let mut codegen = cgen::Codegen::new();
            let source_ion = build::portable_source_label(module_path);
            let c_code = codegen.generate_module_source(
                &ir,
                &symbol_prefix,
                &source_ion,
                &import_names,
                &file_stem,
            );
            let c_file = format!("{}.c", file_stem);
            if let Err(err) = fs::write(&c_file, c_code) {
                eprintln!("Error writing C file '{}': {}", c_file, err);
                process::exit(1);
            }

            // Generate .h file
            let header_code =
                codegen.generate_module_header(module_program, &symbol_prefix, &file_stem);
            let h_file = format!("{}.h", file_stem);
            if let Err(err) = fs::write(&h_file, header_code) {
                eprintln!("Error writing header file '{}': {}", h_file, err);
                process::exit(1);
            }

            let object_file = format!("{}.o", file_stem);
            object_files.push((c_file, object_file));
        }

        // Now compile all .c files to .o files (all headers are now generated)
        let mut compiled_objects = Vec::new();
        let runtime_dir = build::c_toolchain::find_runtime_dir()
            .or_else(|| {
                build::c_toolchain::find_runtime_dir_from(
                    project_root.as_deref().unwrap_or(Path::new(".")),
                )
            })
            .unwrap_or_else(|| PathBuf::from("runtime"));
        let include_dir = env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .canonicalize()
            .unwrap_or_else(|_| PathBuf::from("."));

        for (c_file, object_file) in object_files {
            if let Err(err) = build::c_toolchain::compile_to_object(
                Path::new(&c_file),
                Path::new(&object_file),
                &include_dir,
                &runtime_dir,
                &[],
            ) {
                eprintln!("Error compiling '{}': {}", c_file, err);
                process::exit(1);
            }
            compiled_objects.push(PathBuf::from(object_file));
        }

        // Link all object files into executable
        let executable_name = output_name.unwrap_or_else(|| {
            input_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("a")
                .to_string()
        });

        if let Err(err) = build::c_toolchain::link_executable(
            &compiled_objects,
            Path::new(&executable_name),
            None,
            &[],
        ) {
            eprintln!("Error linking executable: {}", err);
            process::exit(1);
        }

        println!(
            "Compiled and linked '{}' to '{}'",
            input_file, executable_name
        );
    } else {
        // Single-file mode: merge all modules and generate single .c file
        let merged_program = compiler.merge_modules(&ast, input_path);

        // Build IR from merged program
        let ir = ir::IRBuilder::build(&merged_program);

        // Generate C code
        let mut codegen = cgen::Codegen::new();
        let c_code = codegen.generate(&ir, &build::portable_source_label(input_path));

        // Determine output filename
        let output_file = if input_file.ends_with(".ion") {
            input_file[..input_file.len() - 4].to_string() + ".c"
        } else {
            input_file.to_string() + ".c"
        };

        // Write output
        if let Err(err) = fs::write(&output_file, c_code) {
            eprintln!("Error writing output file '{}': {}", output_file, err);
            process::exit(1);
        }

        println!("Compiled '{}' to '{}'", input_file, output_file);
    }
}
