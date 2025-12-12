//! bin2c - Convert binary ROM files to C arrays for Arduino sketches

use clap::Parser;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "bin2c")]
#[command(author = "Alex Jokela")]
#[command(version = "0.1.1")]
#[command(about = "Convert Z80 binary ROMs to C arrays for Arduino sketches")]
struct Args {
    /// Input binary file
    #[arg(value_name = "FILE")]
    input: PathBuf,

    /// Output C header file (default: stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Array name (default: derived from filename)
    #[arg(short, long)]
    name: Option<String>,

    /// Bytes per line in output (default: 16)
    #[arg(short = 'w', long, default_value = "16")]
    width: usize,

    /// Use PROGMEM attribute (for AVR Arduino)
    #[arg(short, long)]
    progmem: bool,

    /// Generate as const uint8_t[] instead of unsigned char[]
    #[arg(long)]
    uint8: bool,
}

fn main() {
    let args = Args::parse();

    // Read input file
    let data = match fs::read(&args.input) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("Error reading {}: {}", args.input.display(), e);
            std::process::exit(1);
        }
    };

    // Derive array name from filename if not specified
    let array_name = args.name.unwrap_or_else(|| {
        args.input
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| sanitize_identifier(s))
            .unwrap_or_else(|| "rom_data".to_string())
    });

    // Generate C code
    let output = generate_c_array(&data, &array_name, args.width, args.progmem, args.uint8);

    // Write output
    match args.output {
        Some(path) => {
            if let Err(e) = fs::write(&path, &output) {
                eprintln!("Error writing {}: {}", path.display(), e);
                std::process::exit(1);
            }
            eprintln!("Generated {} ({} bytes)", path.display(), data.len());
        }
        None => {
            io::stdout().write_all(output.as_bytes()).unwrap();
        }
    }
}

/// Sanitize a string to be a valid C identifier
fn sanitize_identifier(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_ascii_alphanumeric() || c == '_' {
            if i == 0 && c.is_ascii_digit() {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
        } else {
            result.push('_');
        }
    }
    if result.is_empty() {
        result = "data".to_string();
    }
    result
}

/// Generate C array code from binary data
fn generate_c_array(data: &[u8], name: &str, width: usize, progmem: bool, uint8: bool) -> String {
    let mut output = String::new();

    // Header guard and includes
    let guard = format!("{}_H", name.to_uppercase());
    output.push_str(&format!("#ifndef {}\n", guard));
    output.push_str(&format!("#define {}\n\n", guard));

    if progmem {
        output.push_str("#include <avr/pgmspace.h>\n\n");
    }

    if uint8 {
        output.push_str("#include <stdint.h>\n\n");
    }

    // Size constant
    output.push_str(&format!("#define {}_SIZE {}\n\n", name.to_uppercase(), data.len()));

    // Array declaration
    let type_str = if uint8 { "const uint8_t" } else { "const unsigned char" };
    let progmem_str = if progmem { " PROGMEM" } else { "" };

    output.push_str(&format!("{} {}[{}]{} = {{\n", type_str, name, data.len(), progmem_str));

    // Array contents
    for (i, chunk) in data.chunks(width).enumerate() {
        output.push_str("    ");
        for (j, byte) in chunk.iter().enumerate() {
            output.push_str(&format!("0x{:02X}", byte));
            if i * width + j + 1 < data.len() {
                output.push(',');
            }
            if j + 1 < chunk.len() {
                output.push(' ');
            }
        }
        output.push('\n');
    }

    output.push_str("};\n\n");
    output.push_str(&format!("#endif // {}\n", guard));

    output
}
