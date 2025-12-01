use ion_compiler::*;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: ion [--mode <single|multi>] [--output <name>] <input.ion>");
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

    // Use compiler to parse module and its imports
    let mut compiler = compiler::Compiler::new();
    let ast = match compiler.parse_module(input_path) {
        Ok(program) => program,
        Err(err) => {
            match err {
                compiler::CompileError::ParseError(parse_err) => {
                    eprintln!("Parser error: {:?}", parse_err);
                }
                compiler::CompileError::ImportCycle { path } => {
                    eprintln!("Import cycle detected involving: {:?}", path);
                }
                compiler::CompileError::FileNotFound { path } => {
                    eprintln!("File not found: {:?}", path);
                }
                compiler::CompileError::IoError(msg) => {
                    eprintln!("IO error: {}", msg);
                }
            }
            process::exit(1);
        }
    };

    // Type check all modules
    let mut checker = tc::TypeChecker::new();
    // Set module exports for qualified name resolution
    let module_exports = compiler.get_module_exports().clone();
    checker.set_module_exports(module_exports);

    // Type check main module
    if let Err(err) = checker.check_program(&ast) {
        eprintln!("Type check error: {:?}", err);
        process::exit(1);
    }

    // Type check all imported modules
    for (module_path, module_program) in compiler.get_modules() {
        if module_path
            != &input_path
                .canonicalize()
                .unwrap_or_else(|_| input_path.to_path_buf())
            && let Err(err) = checker.check_program(module_program)
        {
            eprintln!("Type check error in module {:?}: {:?}", module_path, err);
            process::exit(1);
        }
    }

    if mode == "multi" {
        // Multi-file mode: generate separate .c and .h files for each module
        let modules = compiler.get_modules();

        // Build a map of module names for header includes
        let mut module_names: std::collections::HashMap<PathBuf, String> =
            std::collections::HashMap::new();
        for module_path in modules.keys() {
            let module_name = module_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("module")
                .to_string();
            module_names.insert(module_path.clone(), module_name);
        }

        // Generate .c and .h files for each module
        let mut object_files = Vec::new();
        for (module_path, module_program) in modules {
            // Build IR for this module
            let ir = ir::IRBuilder::build(module_program);

            // Generate module name from file path
            let module_name = module_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("module")
                .to_string();

            // Collect imported module names for header includes
            let mut import_names = Vec::new();
            for import in &module_program.imports {
                // Resolve import path to get the actual module path
                let import_path = compiler.resolve_import_path(&import.path, module_path);
                if let Some(imported_module_name) = module_names.get(&import_path) {
                    import_names.push(imported_module_name.clone());
                }
            }

            // Generate .c file
            let mut codegen = cgen::Codegen::new();
            let c_code = codegen.generate_module_source(&ir, &module_name, &import_names);
            let c_file = format!("{}.c", module_name);
            if let Err(err) = fs::write(&c_file, c_code) {
                eprintln!("Error writing C file '{}': {}", c_file, err);
                process::exit(1);
            }

            // Generate .h file
            let header_code = codegen.generate_module_header(module_program, &module_name);
            let h_file = format!("{}.h", module_name);
            if let Err(err) = fs::write(&h_file, header_code) {
                eprintln!("Error writing header file '{}': {}", h_file, err);
                process::exit(1);
            }

            let object_file = format!("{}.o", module_name);
            object_files.push((c_file, object_file));
        }

        // Now compile all .c files to .o files (all headers are now generated)
        let mut compiled_objects = Vec::new();
        for (c_file, object_file) in object_files {
            if let Err(err) = compile_to_object(&c_file, &object_file) {
                eprintln!("Error compiling '{}': {}", c_file, err);
                process::exit(1);
            }
            compiled_objects.push(object_file);
        }

        let object_files = compiled_objects;

        // Link all object files into executable
        let executable_name = output_name.unwrap_or_else(|| {
            input_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("a")
                .to_string()
        });

        if let Err(err) = link_executable(&object_files, &executable_name) {
            eprintln!("Error linking executable: {}", err);
            process::exit(1);
        }

        println!(
            "Compiled and linked '{}' to '{}'",
            input_file, executable_name
        );
    } else {
        // Single-file mode: merge all modules and generate single .c file (Phase 2 compatible)
        let merged_program = compiler.merge_modules(&ast, input_path);

        // Build IR from merged program
        let ir = ir::IRBuilder::build(&merged_program);

        // Generate C code
        let mut codegen = cgen::Codegen::new();
        let c_code = codegen.generate(&ir);

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

fn compile_to_object(c_file: &str, object_file: &str) -> Result<(), String> {
    use std::process::Command;

    // Detect C compiler (gcc, clang, or user-specified via CC env var)
    let cc = std::env::var("CC").unwrap_or_else(|_| "gcc".to_string());

    // Determine runtime include path based on current directory
    // Try to find runtime directory relative to current working directory
    let runtime_include = if Path::new("runtime/ion_runtime.h").exists() {
        "runtime"
    } else if Path::new("../runtime/ion_runtime.h").exists() {
        "../runtime"
    } else {
        "runtime" // Default, compiler will search
    };

    // Get the current working directory (where all files are generated)
    let current_dir = std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .canonicalize()
        .unwrap_or_else(|_| PathBuf::from("."));

    let output = Command::new(&cc)
        .arg("-c")
        .arg(c_file)
        .arg("-o")
        .arg(object_file)
        .arg("-I.") // Current directory (where headers should be)
        .arg(format!("-I{}", current_dir.display())) // Explicit current directory as absolute path
        .arg("-I..")
        .arg("-Iruntime")
        .arg("-I../runtime")
        .arg(format!("-I{}", runtime_include))
        .output()
        .map_err(|e| format!("Failed to execute {}: {}", cc, e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Compilation failed: {}", stderr));
    }

    Ok(())
}

fn link_executable(object_files: &[String], executable_name: &str) -> Result<(), String> {
    use std::process::Command;

    // Detect C compiler (gcc, clang, or user-specified via CC env var)
    let cc = std::env::var("CC").unwrap_or_else(|_| "gcc".to_string());

    // Check if runtime exists and add it
    let mut args = vec!["-o", executable_name];
    for obj_file in object_files {
        args.push(obj_file);
    }

    // Add runtime if it exists
    if Path::new("../runtime/ion_runtime.c").exists() {
        args.push("../runtime/ion_runtime.c");
    }

    // Add standard libraries
    args.push("-lm"); // Math library
    args.push("-lpthread"); // Thread library for channels

    let output = Command::new(&cc)
        .args(&args)
        .output()
        .map_err(|e| format!("Failed to execute {}: {}", cc, e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Linking failed: {}", stderr));
    }

    Ok(())
}
