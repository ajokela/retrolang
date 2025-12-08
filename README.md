# RetroLang

A simple systems programming language and compiler for Z80 microprocessors, designed for the RetroShield platform.

## Features

- Pascal/C-like syntax that's easy to read and write
- Compiles directly to Z80 assembly
- Built-in serial I/O for RetroShield
- Supports functions with recursion
- Integer (16-bit) and byte (8-bit) types
- Arrays and pointers
- Inline assembly escape hatch

## Quick Start

```bash
# Build the compiler
cargo build --release

# Compile a program to assembly
./target/release/retrolang examples/hello.rl
# Produces hello.asm

# Compile directly to binary (requires FantASM)
./target/release/retrolang examples/hello.rl --binary
# Produces hello.bin ready to load on RetroShield
```

## Language Overview

### Types

```
byte      // 8-bit unsigned (0-255)
int       // 16-bit signed (-32768 to 32767)
bool      // true/false
char      // 8-bit character
byte[10]  // array of 10 bytes
^int      // pointer to int
```

### Variables and Constants

```
const MAX: int = 100;
var counter: int = 0;
var buffer: byte[80];
```

### Control Flow

```
// If statement
if x > 0 then
    y := 1;
elsif x < 0 then
    y := -1;
else
    y := 0;
end;

// While loop
while x > 0 do
    x := x - 1;
end;

// For loop
for i := 1 to 10 do
    print("*");
end;
```

### Functions and Procedures

```
// Procedure (no return value)
proc greet(name: ^char)
    print("Hello, ");
    print(name);
    println();
end;

// Function (returns a value)
func add(a: int, b: int): int
    return a + b;
end;
```

### Built-in I/O

```
print("Hello");     // Print string
println();          // Print newline
printc('A');        // Print character
printi(42);         // Print integer
readc();            // Read character (returns char)
```

### Inline Assembly

```
proc delay()
    asm
        ld b, 255
    loop:
        djnz loop
    end;
end;
```

## Example: Factorial

```
func factorial(n: int): int
    if n <= 1 then
        return 1;
    else
        return n * factorial(n - 1);
    end;
end;

proc main()
    var i: int;

    print("Factorials:");
    println();

    for i := 1 to 8 do
        printi(i);
        print("! = ");
        printi(factorial(i));
        println();
    end;
end;
```

## Building from Source

Requires Rust 1.70 or later. The build includes both RetroLang and the FantASM assembler:

```bash
cargo build --release
```

Both `retrolang` and `fantasm` binaries are placed in `target/release/`.

## Compiling Programs

### Direct Binary Output (Recommended)

```bash
# Compile to binary (automatically finds fantasm)
./target/release/retrolang program.rl --binary

# Keep intermediate .asm file for inspection
./target/release/retrolang program.rl --binary --keep-asm
```

### Assembly Only

```bash
# Output assembly file only
./target/release/retrolang program.rl

# Then assemble with any Z80 assembler
./target/release/fantasm program.asm program.bin
```

## Memory Layout

The generated code uses this memory layout for RetroShield:

```
0x0000 - 0x7FFF   Program code and data
0x8000 - 0xFFFF   Variables (grows up) / Stack (grows down)
```

## Hardware Requirements

Designed for the RetroShield Z80 with Teensy adapter (256KB RAM). The Arduino Mega version has limited RAM (~4KB) which restricts program size.

## License

RetroLang is licensed under the BSD 3-Clause License. See [LICENSE](LICENSE) for details.

### Third-Party Components

This project includes [FantASM](https://github.com/CaptainBlack/FantASM), a Z80 assembler by Guy Black, licensed under the BSD 2-Clause License. See [fantasm_src/LICENSE](fantasm_src/LICENSE) for details.
