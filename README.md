# uada80 - Ada Compiler for Z80/CP/M

[![Tests](https://github.com/avwohl/uada80/actions/workflows/pytest.yml/badge.svg)](https://github.com/avwohl/uada80/actions/workflows/pytest.yml)
[![Pylint](https://github.com/avwohl/uada80/actions/workflows/pylint.yml/badge.svg)](https://github.com/avwohl/uada80/actions/workflows/pylint.yml)

An Ada compiler targeting the Z80 processor and CP/M 2.2 operating system, aiming for ACATS (Ada Conformity Assessment Test Suite) compliance.

## Project Status

🔧 **Alpha** - Core compiler functionality implemented

## Overview

uada80 is a compiler for the Ada programming language that generates code for the Z80 8-bit microprocessor running CP/M 2.2. The project aims to support a substantial subset of Ada 2012 and pass the ACATS conformance tests.

**Target Platform**: CP/M 2.2 on Z80
- Programs load at 0x0100
- Access to CP/M BDOS for file I/O and console operations
- Approximately 57K TPA on typical 64K system

### Goals

1. Compile Ada source code to Z80 assembly/machine code
2. Generate CP/M .COM executables
3. Pass ACATS test suite (or as many tests as feasible for Z80/CP/M)
4. Generate efficient code suitable for CP/M systems
5. Provide clear error messages and diagnostics

### Inspiration

This project builds on experience from [uplm80](https://github.com/yourusername/uplm80), a PL/M-80 compiler for Z80, reusing proven optimization techniques.

## Features

### Phase 1 (MVP) ✅
- [x] Project structure
- [x] Lexer and parser
- [x] Basic types: Integer, Boolean, Character
- [x] Procedures and functions
- [x] Control flow: if, case, loop, for
- [x] Arrays and records
- [x] Z80 code generation

### Phase 2 (Expanded) ✅
- [x] Packages
- [x] Enumeration types
- [x] Access types (pointers)
- [x] Derived types
- [x] Unconstrained arrays
- [x] AST optimization

### Phase 3 (ACATS Compliance) 🔧
- [x] Generics
- [x] Exception handling
- [x] Full attribute support
- [x] Representation clauses
- [ ] Standard library (adapted for Z80)
- [ ] ACATS test validation

## Architecture

```
Ada Source → Lexer → Parser → AST → Semantic Analysis → Optimizer → Code Gen → Z80 Assembly
```

See [ARCHITECTURE.md](ARCHITECTURE.md) for detailed design documentation.

## Building

### Requirements

- Python 3.10 or later
- Optional: Z80 assembler (z80asm, sjasmplus, or similar)
- Optional: Z80 emulator for testing (e.g., MAME, z80emu)

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/uada80.git
cd uada80

# Create virtual environment
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install in development mode
pip install -e ".[dev]"
```

### Running Tests

```bash
pytest
```

## Usage

```bash
# Compile an Ada source file
uada80 hello.ada -o hello.asm

# With optimization
uada80 hello.ada -o hello.asm -O2

# Generate listing
uada80 hello.ada -o hello.asm --listing
```

## Example Programs

### Hello World

```ada
with Ada.Text_IO;

procedure Hello is
begin
   Ada.Text_IO.Put_Line("Hello from Ada on Z80!");
end Hello;
```

### Fibonacci

```ada
procedure Fibonacci is
   A, B, Temp : Integer;
   N : Integer := 10;
begin
   A := 0;
   B := 1;

   for I in 1 .. N loop
      Temp := A + B;
      A := B;
      B := Temp;
   end loop;
end Fibonacci;
```

See [examples/](examples/) for more examples.

## Documentation

### Compiler Documentation
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) - Compiler architecture and design
- [docs/AST_DESIGN.md](docs/AST_DESIGN.md) - Abstract syntax tree structure
- [docs/OPTIMIZATION_ANALYSIS.md](docs/OPTIMIZATION_ANALYSIS.md) - Optimization strategies
- [docs/LANGUAGE_SUBSET.md](docs/LANGUAGE_SUBSET.md) - Supported Ada language features

### CP/M Target Platform
- [docs/CPM_RUNTIME.md](docs/CPM_RUNTIME.md) - **Complete Ada/CP/M runtime specification**
- [docs/CPM_QUICK_REFERENCE.md](docs/CPM_QUICK_REFERENCE.md) - **CP/M quick reference for developers**
- [docs/cpm22_bdos_calls.pdf](docs/cpm22_bdos_calls.pdf) - BDOS system call reference
- [docs/cpm22_bios_calls.pdf](docs/cpm22_bios_calls.pdf) - BIOS hardware interface
- [docs/cpm22_memory_layout.pdf](docs/cpm22_memory_layout.pdf) - CP/M memory organization

### Ada Language Specifications
- [specs/](specs/) - Ada language specifications and ACATS tests

## ACATS Testing

The Ada Conformity Assessment Test Suite is included in [acats/](acats/).

```bash
# Run ACATS tests (when implemented)
python tests/run_acats.py
```

## Limitations

Due to the Z80's 8-bit architecture and limited resources:

- No floating-point arithmetic (unless software implementation)
- Integer sizes limited to 8-bit and 16-bit
- Reduced standard library
- Tasking support limited or unavailable
- Limited heap (small memory space)

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Run the test suite
5. Submit a pull request

## License

This project is licensed under the GNU General Public License v2.0 - see LICENSE for details.

## References

- [Ada Reference Manual (Ada 2012)](https://www.adaic.org/resources/add_content/standards/12rm/RM-Final.pdf)
- [ACATS Test Suite](http://www.ada-auth.org/acats.html)
- [Z80 CPU User Manual](http://www.z80.info/zip/z80cpu_um.pdf)
- [uplm80 - PL/M Compiler](https://github.com/yourusername/uplm80)

## Related Projects

- [GNAT](https://www.adacore.com/gnatpro) - Production Ada compiler
- [AVR-Ada](https://avr-ada.sourceforge.net/) - Ada for AVR microcontrollers
- [cc65](https://cc65.github.io/) - C compiler for 6502

## Acknowledgments

- The Ada programming language community
- The Z80 retrocomputing community
- ANTLR parser generator team
- uplm80 optimization techniques

## Status Updates

See [CHANGELOG.md](CHANGELOG.md) for development progress.

---

**Note**: This is an educational and hobbyist project. For production Ada development, please use [GNAT](https://www.adacore.com/gnatpro) or other mature Ada compilers.
