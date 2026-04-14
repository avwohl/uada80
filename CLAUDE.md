# UADA80 - Ada Compiler for Z80

Ada compiler targeting Z80/CP/M. Compiles Ada source to Z80 assembly, links with runtime library, runs on CP/M or MP/M.

## Quick Start

```bash
# Compile Ada to assembly
python -m uada80 program.ada -o program.asm

# Assemble and link
um80 program.asm -o program.rel
ul80 program.rel -L runtime/ -l libada.lib -o program.com

# Run on CP/M emulator
cpmemu --z80 program.com
```

## Project Structure

```
uada80/           # Compiler source
  lexer.py        # Tokenizer
  parser.py       # Ada parser → AST
  semantic.py     # Type checking, name resolution
  lowering.py     # AST → IR
  ir.py           # Intermediate representation
  codegen/        # IR → Z80 assembly

runtime/          # Z80 runtime library (MACRO-80 format)
  runtime.mac     # Core: arithmetic, I/O, exceptions
  float64.mac     # IEEE 754 double precision
  fileio.mac      # Text_IO, Sequential_IO
  tasking.mac     # Task support (CP/M stubs)
  containers.mac  # Ada.Containers

adalib/           # Ada standard library specs (~1000 packages)
tests/            # pytest test suite
```

## Running Tests

**Always run execution tests.** The CP/M emulator (`cpmemu`) is built and available at `../cpmemu/src/cpmemu`. Make sure it is on PATH when running tests. Do not skip execution tests.

```bash
# Full test suite (requires cpmemu on PATH)
PATH="../cpmemu/src:$PATH" pytest tests/ -v -o addopts=""

# Just execution tests
PATH="../cpmemu/src:$PATH" pytest tests/test_execution.py -v -o addopts=""

# ACATS conformance tests (end-to-end compile+run on cpmemu)
PATH="../cpmemu/src:$PATH" pytest tests/test_acats_execution.py -v -o addopts=""
```

The `-o addopts=""` flag overrides pyproject.toml defaults (coverage plugins not needed for dev runs).

## Z80 Assembly Conventions

### Symbol Mangling

All user symbols are prefixed with `_` and lowercased to avoid Z80 mnemonic collisions:
- Ada `Add` → `_add:`
- Ada `Counter` → `_counter:`

### Assembly Format

- Mnemonics: lowercase (`ld`, `push`, `call`, `ret`)
- Directives: uppercase (`CSEG`, `DSEG`, `DB`, `DW`, `EXTRN`, `PUBLIC`)
- `.Z80` directive enables Z80 instruction set (um80 defaults to 8080)

### 8-Character Symbol Limit

um80 truncates symbols to 8 characters. All runtime PUBLIC symbols must be unique within 8 chars:
- `_f64_exp` and `_f64_exp_int` both truncate to `_F64_EXP` - collision!
- Solution: renamed to `_f64_e2x` (8 unique chars)

## Runtime Library

Rebuild after changes:
```bash
cd runtime && make clean && make
```

Key modules:
- `_mul16`, `_div16`, `_mod16` - 16-bit arithmetic
- `_put_line`, `_put_int`, `_get_line` - Console I/O
- `_heap_alloc`, `_heap_free` - Dynamic memory
- `_raise_constraint_error` - Exception handling
- `_f64_*` - Float64 operations

## Target Platforms

- **CP/M 2.2+**: `.com` files, single-threaded
- **MP/M II**: `.prl` files, multi-process with OS threading

MP/M provides real tasking via BDOS calls (P_CREATE, Q_MAKE, etc.).

## CI

- Tests run on Python 3.10+
- Execution tests require cpmemu (at `../cpmemu/src/cpmemu`)
- Tasking tests use MP/M II emulator (at `../mpm2/`)
- Pylint threshold: 9.5/10

## Memory Layout (CP/M)

```
0x0000-0x00FF  System area
0x0100-0xFBFF  TPA (~57KB for programs)
0xFC00+        BDOS/BIOS
```

## External Tools — Do Not Modify

**Do not modify cpmemu without permission.** It lives at `../cpmemu/` and is a separate project. If you need different end-of-line or text/binary handling for test output, use a `.cfg` file instead of changing the emulator.

### cpmemu .cfg Files

cpmemu accepts a `.cfg` configuration file that controls file mode handling, EOL conversion, drive mappings, and more. Use this to get the output behavior you need for tests:

```cfg
# Program to run
program = test.com

# Working directory
cd = /tmp/test_output

# EOL handling
default_mode = auto      # auto, text, or binary
eol_convert = true       # Convert Unix \n <-> CP/M \r\n

# File type mappings
*.ADA = text
*.ASM = text
*.COM = binary
*.REL = binary
*.LIB = binary

# Drive mappings
drive_A = .
drive_B = /path/to/source

# Verbosity: 0=quiet, 1=load/exit, 2=files, 3=FCB, 5=all
verbose = 0
```

Run with: `cpmemu examples/mytest.cfg`

See `../cpmemu/examples/` for more examples: `example.cfg`, `compiler.cfg`, `simple_test.cfg`.

## Float64 Runtime Calling Convention

Float64 functions use pointer-based arguments passed on stack via PUSH. After `push ix; ld ix,0; add ix,sp`, arguments are at IX+4, IX+6, IX+8 (last pushed = IX+4).

- **Binary ops** (`_f64_add/sub/mul/div`): Push order: result_ptr first (IX+8), b_ptr second (IX+6), a_ptr last (IX+4). Runtime reads a from IX+4, b from IX+6, stores result via IX+8.
- **`_f64_cmp`**: Push b_ptr first (IX+6), a_ptr last (IX+4). Returns A=-1 (a<b), 0 (a==b), 1 (a>b).
- **`_f64_copy`**: Push src_ptr first (IX+6), dest_ptr last (IX+4).
- **`_f64_itof`**: Push dest_ptr first (IX+6), int_value last (IX+4).
- **`_f64_neg/abs`**: Push src_ptr first (IX+6), dest_ptr last (IX+4).

## Common Issues

1. **um80 not found**: Install with `pip install um80`
2. **Link errors**: Rebuild runtime with `make -C runtime`
3. **Timeout**: Z80 is slow; increase timeout for complex programs
