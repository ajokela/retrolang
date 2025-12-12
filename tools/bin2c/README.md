# bin2c

A command-line tool to convert binary ROM files into C header files for embedding in Arduino sketches and other embedded projects.

## Overview

bin2c reads a binary file (such as a Z80 ROM image) and outputs a C header file containing the data as a constant array. This is useful for embedding compiled programs directly into Arduino firmware for the RetroShield platform.

## Installation

```bash
cargo install --path .
```

Or build from source:

```bash
cargo build --release
```

## Usage

```bash
# Basic usage - output to stdout
bin2c program.bin

# Write to a header file
bin2c program.bin -o program.h

# Specify custom array name
bin2c program.bin -o rom.h --name my_program

# Generate with PROGMEM attribute (for AVR Arduino)
bin2c program.bin -o rom.h --progmem

# Use uint8_t instead of unsigned char
bin2c program.bin -o rom.h --uint8

# Adjust bytes per line (default: 16)
bin2c program.bin -o rom.h --width 12
```

## Options

| Option | Short | Description |
|--------|-------|-------------|
| `--output` | `-o` | Output file path (default: stdout) |
| `--name` | `-n` | Array name (default: derived from filename) |
| `--width` | `-w` | Bytes per line in output (default: 16) |
| `--progmem` | `-p` | Add PROGMEM attribute for AVR Arduino |
| `--uint8` | | Use `uint8_t` instead of `unsigned char` |

## Example Output

Given a binary file `hello.bin`, running:

```bash
bin2c hello.bin -o hello.h --progmem
```

Produces:

```c
#ifndef HELLO_H
#define HELLO_H

#include <avr/pgmspace.h>

#define HELLO_SIZE 256

const unsigned char hello[256] PROGMEM = {
    0x3E, 0x48, 0xD3, 0x81, 0x3E, 0x65, 0xD3, 0x81, 0x3E, 0x6C, 0xD3, 0x81, 0x3E, 0x6C, 0xD3, 0x81,
    0x3E, 0x6F, 0xD3, 0x81, 0x76, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    // ... remaining bytes
};

#endif // HELLO_H
```

## Use with RetroShield

This tool is designed to work with the RetroShield Z80 platform. A typical workflow:

1. Compile your Z80 program using retrolang, kz80_c, or another compiler
2. Convert the binary to a C header with bin2c
3. Include the header in your Arduino sketch
4. Flash the ROM data to the RetroShield

```bash
# Compile a RetroLang program
retrolang program.rl --binary -o program.bin

# Convert to Arduino header
bin2c program.bin -o program.h --progmem

# Include in Arduino sketch and upload
```

## License

BSD-3-Clause
