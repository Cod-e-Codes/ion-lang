use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::{ParseError, Parser};
use crate::tc::ModuleExports;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

pub struct Compiler {
    modules: HashMap<PathBuf, Program>,
    visiting: HashSet<PathBuf>,
    module_exports: HashMap<String, ModuleExports>, // Maps import alias to exports
}

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
    ImportCycle { path: PathBuf },
    FileNotFound { path: PathBuf },
    IoError(String),
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            visiting: HashSet::new(),
            module_exports: HashMap::new(),
        }
    }

    /// Resolve an import path relative to the importing file
    pub fn resolve_import_path(&self, import_path: &str, from_file: &Path) -> PathBuf {
        let mut resolved = from_file.parent().unwrap_or(Path::new(".")).to_path_buf();

        // Handle relative paths
        if let Some(stripped) = import_path.strip_prefix("./") {
            resolved.push(stripped);
        } else if let Some(stripped) = import_path.strip_prefix("../") {
            resolved.push("..");
            resolved.push(stripped);
        } else {
            // Simple filename - same directory
            resolved.push(import_path);
        }

        // Ensure .ion extension
        if !resolved.extension().map(|e| e == "ion").unwrap_or(false) {
            resolved.set_extension("ion");
        }

        resolved.canonicalize().unwrap_or(resolved)
    }

    /// Parse a module file and recursively parse its imports
    pub fn parse_module(&mut self, path: &Path) -> Result<Program, CompileError> {
        let canonical_path = path
            .canonicalize()
            .map_err(|_| CompileError::FileNotFound {
                path: path.to_path_buf(),
            })?;

        // Check if already parsed
        if let Some(module) = self.modules.get(&canonical_path) {
            return Ok(module.clone());
        }

        // Check for cycles
        if self.visiting.contains(&canonical_path) {
            return Err(CompileError::ImportCycle {
                path: canonical_path,
            });
        }

        // Mark as visiting
        self.visiting.insert(canonical_path.clone());

        // Read file
        let content = std::fs::read_to_string(&canonical_path).map_err(|e| {
            CompileError::IoError(format!("Failed to read file {:?}: {}", canonical_path, e))
        })?;

        // Lex
        let mut lexer = Lexer::new(&content);
        let tokens = lexer.tokenize().map_err(|e| {
            CompileError::ParseError(ParseError::Message(format!("Lexer error: {}", e)))
        })?;

        // Parse
        let mut parser = Parser::new(tokens);
        let program = parser.parse().map_err(CompileError::ParseError)?;

        // Recursively parse imports and build export maps
        for import in &program.imports {
            let import_path = self.resolve_import_path(&import.path, &canonical_path);
            let imported_module = self.parse_module(&import_path)?;

            // Build exports for this imported module
            let mut exports = ModuleExports {
                functions: HashMap::new(),
                structs: HashMap::new(),
                enums: HashMap::new(),
                all_functions: HashMap::new(),
            };

            // Collect public functions and track all functions for error messages
            for func in &imported_module.functions {
                exports.all_functions.insert(func.name.clone(), func.pub_);
                if func.pub_ {
                    exports.functions.insert(func.name.clone(), func.clone());
                }
            }

            // Collect public structs
            for s in &imported_module.structs {
                if s.pub_ {
                    exports.structs.insert(s.name.clone(), s.clone());
                }
            }

            // Collect public enums
            for e in &imported_module.enums {
                if e.pub_ {
                    exports.enums.insert(e.name.clone(), e.clone());
                }
            }

            // Store exports by import alias
            self.module_exports.insert(import.alias.clone(), exports);
        }

        // Remove from visiting set
        self.visiting.remove(&canonical_path);

        // Cache and return
        self.modules.insert(canonical_path, program.clone());
        Ok(program)
    }

    /// Get all parsed modules
    pub fn get_modules(&self) -> &HashMap<PathBuf, Program> {
        &self.modules
    }

    /// Get module exports map (for type checker)
    pub fn get_module_exports(&self) -> &HashMap<String, ModuleExports> {
        &self.module_exports
    }

    /// Merge all modules into a single Program for codegen
    /// This collects all functions, structs, enums, and extern blocks from all modules
    pub fn merge_modules(&self, main_program: &Program, main_path: &Path) -> Program {
        let mut merged = Program {
            imports: Vec::new(), // Imports are not needed in merged program
            structs: main_program.structs.clone(),
            enums: main_program.enums.clone(),
            type_aliases: main_program.type_aliases.clone(),
            functions: main_program.functions.clone(),
            extern_blocks: main_program.extern_blocks.clone(),
        };

        // Merge all imported modules' public items (skip the main program to avoid duplicates)
        let main_canonical = main_path
            .canonicalize()
            .unwrap_or_else(|_| main_path.to_path_buf());
        for (module_path, module_program) in &self.modules {
            // Skip the main program - we already have its items
            if module_path
                .canonicalize()
                .unwrap_or_else(|_| module_path.clone())
                == main_canonical
            {
                continue;
            }
            // Add public structs
            for s in &module_program.structs {
                if s.pub_
                    && !merged
                        .structs
                        .iter()
                        .any(|existing| existing.name == s.name)
                {
                    merged.structs.push(s.clone());
                }
            }

            // Add public enums
            for e in &module_program.enums {
                if e.pub_ && !merged.enums.iter().any(|existing| existing.name == e.name) {
                    merged.enums.push(e.clone());
                }
            }

            // Add public functions
            for f in &module_program.functions {
                if f.pub_
                    && !merged
                        .functions
                        .iter()
                        .any(|existing| existing.name == f.name)
                {
                    merged.functions.push(f.clone());
                }
            }

            // Add extern blocks (all extern blocks are included, but deduplicate by function name)
            for extern_block in &module_program.extern_blocks {
                // Check if we already have functions from this extern block
                let mut block_to_add = extern_block.clone();
                block_to_add.functions.retain(|ext_fn| {
                    // Check if this function is already declared in any existing extern block
                    !merged.extern_blocks.iter().any(|existing_block| {
                        existing_block
                            .functions
                            .iter()
                            .any(|existing_fn| existing_fn.name == ext_fn.name)
                    })
                });
                if !block_to_add.functions.is_empty() {
                    merged.extern_blocks.push(block_to_add);
                }
            }
        }

        merged
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
