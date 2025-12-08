//! RetroLang Compiler
//!
//! A simple systems programming language for Z80 microprocessors.

mod ast;
mod codegen;
mod lexer;
mod parser;

use clap::Parser as ClapParser;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

#[derive(ClapParser, Debug)]
#[command(name = "retrolang")]
#[command(author = "Alex Jokela")]
#[command(version = "0.1.0")]
#[command(about = "RetroLang compiler for Z80", long_about = None)]
struct Args {
    /// Input source file
    #[arg(value_name = "FILE")]
    input: PathBuf,

    /// Output file (default: input with .asm or .bin extension)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Assemble to binary (requires FantASM)
    #[arg(short, long)]
    binary: bool,

    /// Keep intermediate .asm file when producing binary
    #[arg(long)]
    keep_asm: bool,

    /// Path to FantASM assembler (default: fantasm in PATH or ./fantasm_src/target/release/fantasm)
    #[arg(long)]
    fantasm: Option<PathBuf>,

    /// Print tokens (for debugging)
    #[arg(long)]
    tokens: bool,

    /// Print AST (for debugging)
    #[arg(long)]
    ast: bool,
}

fn main() {
    let args = Args::parse();

    // Read source file
    let source = match fs::read_to_string(&args.input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", args.input.display(), e);
            std::process::exit(1);
        }
    };

    // Tokenize
    let tokens = lexer::tokenize(&source);

    if args.tokens {
        println!("=== Tokens ===");
        for tok in &tokens {
            println!("{:?}", tok);
        }
        println!();
    }

    // Parse
    let program = match parser::parse(&source) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    if args.ast {
        println!("=== AST ===");
        println!("{:#?}", program);
        println!();
    }

    // Generate code
    let mut codegen = codegen::CodeGen::new();
    let asm = codegen.compile(&program);

    // Determine output paths
    let (asm_path, bin_path) = if args.binary {
        let bin_path = args.output.clone().unwrap_or_else(|| {
            let mut p = args.input.clone();
            p.set_extension("bin");
            p
        });
        let asm_path = {
            let mut p = bin_path.clone();
            p.set_extension("asm");
            p
        };
        (asm_path, Some(bin_path))
    } else {
        let asm_path = args.output.clone().unwrap_or_else(|| {
            let mut p = args.input.clone();
            p.set_extension("asm");
            p
        });
        (asm_path, None)
    };

    // Write assembly output
    match fs::write(&asm_path, &asm) {
        Ok(_) => {
            if bin_path.is_none() {
                println!("Compiled {} -> {}", args.input.display(), asm_path.display());
            }
        }
        Err(e) => {
            eprintln!("Error writing {}: {}", asm_path.display(), e);
            std::process::exit(1);
        }
    }

    // Assemble to binary if requested
    if let Some(ref bin_path) = bin_path {
        let fantasm_path = find_fantasm(&args.fantasm);

        match fantasm_path {
            Some(fantasm) => {
                let status = Command::new(&fantasm)
                    .arg(&asm_path)
                    .arg(bin_path)
                    .status();

                match status {
                    Ok(s) if s.success() => {
                        println!("Compiled {} -> {}", args.input.display(), bin_path.display());

                        // Remove intermediate .asm file unless --keep-asm
                        if !args.keep_asm {
                            let _ = fs::remove_file(&asm_path);
                        }
                    }
                    Ok(s) => {
                        eprintln!("FantASM failed with exit code: {:?}", s.code());
                        std::process::exit(1);
                    }
                    Err(e) => {
                        eprintln!("Failed to run FantASM at {}: {}", fantasm.display(), e);
                        std::process::exit(1);
                    }
                }
            }
            None => {
                eprintln!("FantASM assembler not found. Install it or use --fantasm to specify path.");
                eprintln!("Assembly file saved to: {}", asm_path.display());
                std::process::exit(1);
            }
        }
    }
}

/// Find the FantASM assembler executable
fn find_fantasm(explicit_path: &Option<PathBuf>) -> Option<PathBuf> {
    // 1. Use explicit path if provided
    if let Some(path) = explicit_path {
        if path.exists() {
            return Some(path.clone());
        }
    }

    // 2. Check next to this executable (workspace build puts both in same dir)
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            let fantasm = exe_dir.join("fantasm");
            if fantasm.exists() {
                return Some(fantasm);
            }
        }
    }

    // 3. Check for fantasm in PATH
    if let Ok(output) = Command::new("which").arg("fantasm").output() {
        if output.status.success() {
            let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !path.is_empty() {
                return Some(PathBuf::from(path));
            }
        }
    }

    // 4. Check current working directory
    let cwd_fantasm = PathBuf::from("fantasm");
    if cwd_fantasm.exists() {
        return Some(cwd_fantasm);
    }

    None
}
