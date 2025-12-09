//! RetroLang Compiler
//!
//! A simple systems programming language for Z80 microprocessors.

mod ast;
mod codegen;
mod lexer;
mod parser;

use clap::{Parser as ClapParser, ValueEnum};
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use codegen::SerialDriver;

#[derive(Debug, Clone, Copy, ValueEnum)]
enum SerialDriverArg {
    /// MC6850 ACIA (ports $80/$81)
    Acia,
    /// Intel 8251 USART (ports $00/$01)
    Intel,
}

#[derive(ClapParser, Debug)]
#[command(name = "retrolang")]
#[command(author = "Alex Jokela")]
#[command(version = "0.1.1")]
#[command(about = "RetroLang compiler for Z80", long_about = None)]
struct Args {
    /// Input source file
    #[arg(value_name = "FILE")]
    input: PathBuf,

    /// Output file (default: input with .asm or .bin extension)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Assemble to binary (requires retrolang-asm)
    #[arg(short, long)]
    binary: bool,

    /// Keep intermediate .asm file when producing binary
    #[arg(long)]
    keep_asm: bool,

    /// Path to assembler (default: retrolang-asm in PATH)
    #[arg(long)]
    assembler: Option<PathBuf>,

    /// Serial driver to use for I/O
    #[arg(long, value_enum, default_value = "acia")]
    serial: SerialDriverArg,

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
    let serial_driver = match args.serial {
        SerialDriverArg::Acia => SerialDriver::Acia,
        SerialDriverArg::Intel => SerialDriver::Intel8251,
    };
    let mut codegen = codegen::CodeGen::new(serial_driver);
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
        let asm_exe = find_assembler(&args.assembler);

        match asm_exe {
            Some(assembler) => {
                let status = Command::new(&assembler)
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
                        eprintln!("Assembler failed with exit code: {:?}", s.code());
                        std::process::exit(1);
                    }
                    Err(e) => {
                        eprintln!("Failed to run assembler at {}: {}", assembler.display(), e);
                        std::process::exit(1);
                    }
                }
            }
            None => {
                eprintln!("Assembler not found. Install with: cargo install retrolang-asm");
                eprintln!("Or use --assembler to specify path to any Z80 assembler.");
                eprintln!("Assembly file saved to: {}", asm_path.display());
                std::process::exit(1);
            }
        }
    }
}

/// Find the assembler executable (retrolang-asm)
fn find_assembler(explicit_path: &Option<PathBuf>) -> Option<PathBuf> {
    // 1. Use explicit path if provided
    if let Some(path) = explicit_path {
        if path.exists() {
            return Some(path.clone());
        }
    }

    // 2. Check next to this executable
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            let retrolang_asm = exe_dir.join("retrolang-asm");
            if retrolang_asm.exists() {
                return Some(retrolang_asm);
            }
        }
    }

    // 3. Check for retrolang-asm in PATH
    if let Ok(output) = Command::new("which").arg("retrolang-asm").output() {
        if output.status.success() {
            let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !path.is_empty() {
                return Some(PathBuf::from(path));
            }
        }
    }

    // 4. Check current working directory
    let path = PathBuf::from("retrolang-asm");
    if path.exists() {
        return Some(path);
    }

    None
}
