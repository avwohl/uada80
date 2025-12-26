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

```bash
# Full test suite
pytest tests/ -v -o addopts=""

# Just execution tests (requires cpmemu)
pytest tests/test_execution.py -v -o addopts=""
```

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

- Tests run on Python 3.10-3.12
- Execution tests skipped in CI (require cpmemu)
- Pylint threshold: 9.5/10

## Memory Layout (CP/M)

```
0x0000-0x00FF  System area
0x0100-0xFBFF  TPA (~57KB for programs)
0xFC00+        BDOS/BIOS
```

## Common Issues

1. **um80 not found**: Install with `pip install um80`
2. **Link errors**: Rebuild runtime with `make -C runtime`
3. **Timeout**: Z80 is slow; increase timeout for complex programs
