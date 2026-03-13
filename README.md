# uada80 - Ada Compiler for Z80/CP/M

[![Tests](https://github.com/avwohl/uada80/actions/workflows/pytest.yml/badge.svg)](https://github.com/avwohl/uada80/actions/workflows/pytest.yml)
[![Pylint](https://github.com/avwohl/uada80/actions/workflows/pylint.yml/badge.svg)](https://github.com/avwohl/uada80/actions/workflows/pylint.yml)

An Ada compiler targeting the Z80 processor and CP/M 2.2 operating system, aiming for ACATS (Ada Conformity Assessment Test Suite) compliance.

## Project Status

🔧 **Alpha** - Core compiler functionality implemented

## Overview

uada80 is a compiler for the Ada programming language that generates code for the Z80 8-bit microprocessor running CP/M 2.2. The project aims to support a substantial subset of Ada 2012 and pass the ACATS conformance tests.

**Target Platforms**: CP/M 2.2 and MP/M II on Z80
- CP/M 2.2: `.com` executables, single-threaded, programs load at 0x0100
- MP/M II: `.prl` relocatable executables, preemptive multitasking via OS primitives
- Access to BDOS for file I/O and console operations
- Approximately 57K TPA on typical 64K system

### Goals

1. Compile Ada source code to Z80 assembly/machine code
2. Generate CP/M .COM executables
3. Pass ACATS test suite (or as many tests as feasible for Z80/CP/M)
4. Generate efficient code suitable for CP/M systems
5. Provide clear error messages and diagnostics

### Inspiration

This project builds on experience from [uplm80](https://github.com/avwohl/uplm80), a PL/M-80 compiler for Z80, reusing proven optimization techniques.

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

See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for detailed design documentation.

## Building

### Requirements

- Python 3.10 or later
- [um80_and_friends](https://github.com/avwohl/um80_and_friends) - Z80 assembler and linker (`um80`, `ul80`)
- [cpmemu](https://github.com/avwohl/cpmemu) - CP/M emulator for running compiled programs

### Installation

```bash
# Clone the repository
git clone https://github.com/avwohl/uada80.git
cd uada80

# Create virtual environment
python3 -m venv venv
source venv/bin/activate

# Install in development mode
pip install -e ".[dev]"

# Install assembler/linker
pip install um80
```

### Running Tests

```bash
pytest tests/ -v -o addopts=""
```

## Usage

```bash
# Compile Ada to Z80 assembly
python -m uada80 hello.ada -o hello.asm

# Assemble and link
um80 -o hello.rel hello.asm
ul80 -o hello.com hello.rel -L runtime/ -l libada.lib

# Run on CP/M emulator
cpmemu --z80 hello.com
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

See [learn-ada-z80](https://github.com/avwohl/learn-ada-z80) for 99 example programs covering basics through tasking, generics, and applications.

## Ada Tasking on MP/M II

Ada tasking (tasks, entries, rendezvous, protected types) runs on [MP/M II](https://github.com/avwohl/mpm2) using OS-native primitives for preemptive multitasking. On CP/M 2.2 (single-user), the runtime provides cooperative tasking stubs.

### How It Works

Ada tasks become MP/M II subprocesses sharing one memory bank. The OS handles preemptive scheduling, and all inter-task communication uses BDOS queue operations:

| Ada Construct | MP/M II Primitive |
|---|---|
| Task creation | P_CREATE (BDOS 144) |
| Entry call | Q_WRITE (BDOS 139) - blocking send to queue |
| Accept statement | Q_READ (BDOS 137) - blocking receive from queue |
| Select/else | Q_CREAD (BDOS 138) - conditional (non-blocking) read |
| Protected object | MX queue (mutual exclusion) |
| Delay | P_DELAY (BDOS 141) |
| Abort | P_ABORT (BDOS 157) |

### Building for MP/M II

```bash
# Compile Ada to assembly
python -m uada80 program.ada -o program.asm

# Assemble
um80 -o program.rel program.asm

# Link as PRL (relocatable for MP/M II)
ul80 --prl -p 0 -o program.prl program.rel -L runtime/ -l libada_mpm.lib

# Or link as COM (also works under MP/M)
ul80 -o program.com program.rel -L runtime/ -l libada_mpm.lib
```

### Running on the MP/M II Emulator

The [mpm2](https://github.com/avwohl/mpm2) emulator provides a full MP/M II environment with SSH terminal access:

```bash
# Start the emulator (up to 4 concurrent consoles)
mpm2_emu --no-auth -p 127.0.0.1:2222 -d A:disks/mpm2_system_work.img

# Connect via SSH
ssh -p 2222 user@localhost

# Upload and run
sftp -P 2222 user@localhost
put program.prl /A.0/PROGRAM.PRL
```

### Runtime Libraries

Two runtime libraries are provided:

- **`libada.lib`** - CP/M 2.2 runtime with cooperative tasking stubs
- **`libada_mpm.lib`** - MP/M II runtime with OS-native preemptive tasking

Rebuild after changes:
```bash
cd runtime && make clean && make
```

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

## Testing

### ACATS End-to-End Execution

The [ACATS 4.2](http://www.ada-auth.org/acats.html) test suite is included in [tests/acats/](tests/acats/). Tests are compiled to Z80 assembly, assembled with um80, linked with ul80, and executed on [cpmemu](https://github.com/avwohl/cpmemu).

**579 ACATS tests pass end-to-end** (compile + assemble + link + execute on cpmemu):

```
$ pytest tests/test_acats_execution.py -o addopts=""
===== 257 failed, 579 passed, 624 skipped in 5197s =====
```

| Result | Count | Description |
|---|---:|---|
| Passed | 579 | Compiled, ran on cpmemu, output contains PASSED |
| Failed | 257 | Compiled and ran but produced wrong results (codegen bugs) |
| Skipped | 624 | Compile/link/timeout failures (multi-file deps, missing features) |

### ACATS Front-End

All 5,787 legal ACATS files pass parsing and semantic analysis:

```
$ pytest tests/test_acats.py -o addopts=""
======================= 5,787 passed in 248s =======================
```

### learn-ada-z80 Programs

The [learn-ada-z80](https://github.com/avwohl/learn-ada-z80) companion project has 99 example programs. **98 of 99 compile to Z80 assembly** (full pipeline: parse, semantic analysis, lowering, code generation). One program fails due to a codegen bug with negative array bounds.

### Execution Tests

End-to-end execution tests (compile + assemble + link + run on cpmemu) are in `tests/test_execution.py`:

```bash
pytest tests/test_execution.py -v -o addopts=""
```

## Limitations

Due to the Z80's 8-bit architecture and limited resources:

- Integer sizes limited to 8-bit and 16-bit (32-bit via library)
- Software floating-point only (IEEE 754 double via `float64.mac`)
- Reduced standard library
- Tasking requires MP/M II (CP/M 2.2 provides stubs only)
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
- [uplm80 - PL/M Compiler](https://github.com/avwohl/uplm80)
## Related Projects

- [80un](https://github.com/avwohl/80un) - Unpacker for CP/M compression and archive formats (LBR, ARC, squeeze, crunch, CrLZH)
- [cpmdroid](https://github.com/avwohl/cpmdroid) - Z80/CP/M emulator for Android with RomWBW HBIOS compatibility and VT100 terminal
- [cpmemu](https://github.com/avwohl/cpmemu) - CP/M 2.2 emulator with Z80/8080 CPU emulation and BDOS/BIOS translation to Unix filesystem
- [ioscpm](https://github.com/avwohl/ioscpm) - Z80/CP/M emulator for iOS and macOS with RomWBW HBIOS compatibility
- [learn-ada-z80](https://github.com/avwohl/learn-ada-z80) - Ada programming examples for the uada80 compiler targeting Z80/CP/M
- [mbasic](https://github.com/avwohl/mbasic) - Modern MBASIC 5.21 Interpreter & Compilers
- [mbasic2025](https://github.com/avwohl/mbasic2025) - MBASIC 5.21 source code reconstruction - byte-for-byte match with original binary
- [mbasicc](https://github.com/avwohl/mbasicc) - C++ implementation of MBASIC 5.21
- [mbasicc_web](https://github.com/avwohl/mbasicc_web) - WebAssembly MBASIC 5.21
- [mpm2](https://github.com/avwohl/mpm2) - MP/M II multi-user CP/M emulator with SSH terminal access and SFTP file transfer
- [romwbw_emu](https://github.com/avwohl/romwbw_emu) - Hardware-level Z80 emulator for RomWBW with 512KB ROM + 512KB RAM banking and HBIOS support
- [scelbal](https://github.com/avwohl/scelbal) - SCELBAL BASIC interpreter - 8008 to 8080 translation
- [uc80](https://github.com/avwohl/uc80) - ANSI C compiler targeting Z80 processor and CP/M 2.2 operating system
- [ucow](https://github.com/avwohl/ucow) - Unix/Linux Cowgol to Z80 compiler
- [um80_and_friends](https://github.com/avwohl/um80_and_friends) - Microsoft MACRO-80 compatible toolchain for Linux: assembler, linker, librarian, disassembler
- [upeepz80](https://github.com/avwohl/upeepz80) - Universal peephole optimizer for Z80 compilers
- [uplm80](https://github.com/avwohl/uplm80) - PL/M-80 compiler targeting Intel 8080 and Zilog Z80 assembly language
- [z80cpmw](https://github.com/avwohl/z80cpmw) - Z80 CP/M emulator for Windows (RomWBW)

## See Also

- [GNAT](https://www.adacore.com/gnatpro) - For production Ada development, use GNAT or other mature Ada compilers

