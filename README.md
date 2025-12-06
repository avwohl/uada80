# uada80 - Ada Compiler for Z80

An Ada compiler targeting the Z80 processor, aiming for ACATS (Ada Conformity Assessment Test Suite) compliance.

## Project Status

🚧 **Pre-Alpha** - Currently in initial development

## Overview

uada80 is a compiler for the Ada programming language that generates code for the Z80 8-bit microprocessor. The project aims to support a substantial subset of Ada 2012 and pass the ACATS conformance tests.

### Goals

1. Compile Ada source code to Z80 assembly/machine code
2. Pass ACATS test suite (or as many tests as feasible for Z80)
3. Generate efficient code suitable for embedded/retro systems
4. Provide clear error messages and diagnostics

### Inspiration

This project builds on experience from [uplm80](https://github.com/yourusername/uplm80), a PL/M-80 compiler for Z80, reusing proven optimization techniques.

## Features (Planned)

### Phase 1 (MVP)
- [x] Project structure
- [ ] Lexer and parser
- [ ] Basic types: Integer, Boolean, Character
- [ ] Procedures and functions
- [ ] Control flow: if, case, loop, for
- [ ] Arrays and records
- [ ] Z80 code generation

### Phase 2 (Expanded)
- [ ] Packages
- [ ] Enumeration types
- [ ] Access types (pointers)
- [ ] Derived types
- [ ] Unconstrained arrays
- [ ] Advanced optimization

### Phase 3 (ACATS Compliance)
- [ ] Generics
- [ ] Exception handling
- [ ] Full attribute support
- [ ] Representation clauses
- [ ] Standard library (adapted for Z80)

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

- [ARCHITECTURE.md](ARCHITECTURE.md) - Compiler architecture and design
- [OPTIMIZATION_ANALYSIS.md](OPTIMIZATION_ANALYSIS.md) - Optimization strategies
- [LANGUAGE_SUBSET.md](LANGUAGE_SUBSET.md) - Supported Ada language features
- [specs/](specs/) - Ada language specifications

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
