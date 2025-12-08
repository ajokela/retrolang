# RetroLang Language Specification

A simple systems programming language for 8-bit microprocessors, designed for the RetroShield Z80 platform.

## Design Goals

1. **Simple** - Easy to learn, easy to implement
2. **Efficient** - Generates compact Z80 code
3. **Practical** - Useful for real embedded programs
4. **Familiar** - Pascal/C-like syntax

## Types

```
byte      // 8-bit unsigned (0-255)
int       // 16-bit signed (-32768 to 32767)
bool      // true/false
char      // 8-bit character
```

Arrays:
```
byte[10]  // array of 10 bytes
int[5]    // array of 5 integers
```

Pointers:
```
^byte     // pointer to byte
^int      // pointer to int
```

## Literals

```
42        // decimal integer
0xFF      // hexadecimal
0b1010    // binary
'A'       // character (byte value 65)
"hello"   // string literal (null-terminated)
true      // boolean
false     // boolean
```

## Variables

```
var x: int;              // declare integer
var y: byte = 42;        // declare with initializer
var arr: byte[10];       // array
var ptr: ^int;           // pointer
```

## Constants

```
const MAX: int = 100;
const ACIA_DATA: byte = 0x80;
```

## Operators

Arithmetic:
```
+  -  *  /  %   // add, sub, mul, div, mod
```

Comparison:
```
=  <>  <  >  <=  >=   // equal, not equal, less, greater, etc.
```

Logical:
```
and  or  not
```

Bitwise:
```
&  |  ^  ~  <<  >>   // and, or, xor, not, shift left/right
```

Pointer/Address:
```
@x        // address of x
ptr^      // dereference pointer
```

## Control Flow

### If/Else

```
if x > 0 then
    y := 1;
elsif x < 0 then
    y := -1;
else
    y := 0;
end;
```

### While Loop

```
while x > 0 do
    x := x - 1;
end;
```

### For Loop

```
for i := 0 to 9 do
    arr[i] := 0;
end;

for i := 10 downto 1 do
    print(i);
end;
```

### Loop Control

```
break;      // exit loop
continue;   // next iteration
```

## Functions and Procedures

Procedures (no return value):
```
proc greet(name: ^char)
    print("Hello, ");
    print(name);
    println();
end;
```

Functions (return value):
```
func add(a: int, b: int): int
    return a + b;
end;

func max(a: int, b: int): int
    if a > b then
        return a;
    else
        return b;
    end;
end;
```

## Inline Assembly

```
asm
    ld a, 0x42
    out (0x80), a
end;
```

With inputs/outputs:
```
func inp(port: byte): byte
    var result: byte;
    asm
        ld c, {port}
        in a, (c)
        ld {result}, a
    end;
    return result;
end;
```

## Memory Access

Direct memory access:
```
mem[0x8000] := 0x42;      // write byte to address
x := mem[0x8000];         // read byte from address
```

Word access:
```
memw[0x8000] := 0x1234;   // write 16-bit word
x := memw[0x8000];        // read 16-bit word
```

## Built-in Functions

### I/O
```
print(s: ^char)           // print string
println()                 // print newline
printc(c: char)           // print character
printi(n: int)            // print integer
printb(n: byte)           // print byte as hex

readc(): char             // read character
readln(buf: ^char, max: byte)  // read line into buffer
readi(): int              // read integer
```

### Memory
```
peek(addr: int): byte     // read byte from address
poke(addr: int, val: byte) // write byte to address
peekw(addr: int): int     // read word from address
pokew(addr: int, val: int) // write word to address
```

### Utility
```
halt()                    // stop execution
delay(ms: int)            // delay milliseconds (approximate)
```

## Program Structure

```
// Constants and imports at top
const BUFFER_SIZE: int = 80;

// Global variables
var buffer: char[80];
var count: int;

// Function definitions
func factorial(n: int): int
    if n <= 1 then
        return 1;
    else
        return n * factorial(n - 1);
    end;
end;

// Main entry point
proc main()
    var i: int;

    println("Factorial calculator");

    for i := 1 to 10 do
        print("fact(");
        printi(i);
        print(") = ");
        printi(factorial(i));
        println();
    end;
end;
```

## Example: Blink LED

```
const LED_PORT: byte = 0x01;

proc delay_loop(count: int)
    var i: int;
    for i := 0 to count do
        // busy wait
    end;
end;

proc main()
    var led_state: byte = 0;

    while true do
        asm
            ld a, {led_state}
            out (LED_PORT), a
        end;

        led_state := led_state ^ 0xFF;  // toggle
        delay_loop(10000);
    end;
end;
```

## Example: Serial Echo

```
proc main()
    var c: char;

    println("RetroShield Echo");
    println("Type something:");

    while true do
        c := readc();
        printc(c);

        if c = 13 then  // Enter key
            println();
        end;
    end;
end;
```

## Memory Layout (RetroShield Z80)

```
0x0000 - 0x00FF   Vectors and init
0x0100 - 0x7FFF   Program code and data
0x8000 - 0xFFFF   RAM (stack grows down from 0xFFFF)
```

## Calling Convention

- Arguments passed on stack, right to left
- Return value in HL (16-bit) or A (8-bit)
- Caller cleans up stack
- BC, DE preserved by callee
- AF, HL used as scratch

## Future Extensions

- Structs/records
- Enums
- Modules/imports
- Interrupt handlers
- Signed byte type
