# uada80 - Ada Compiler for Z80/CP/M

[![Tests](https://github.com/avwohl/uada80/actions/workflows/pytest.yml/badge.svg)](https://github.com/avwohl/uada80/actions/workflows/pytest.yml)
[![Pylint](https://github.com/avwohl/uada80/actions/workflows/pylint.yml/badge.svg)](https://github.com/avwohl/uada80/actions/workflows/pylint.yml)

An Ada compiler targeting the Z80 processor and CP/M 2.2 operating system, aiming for ACATS (Ada Conformity Assessment Test Suite) compliance.

**Status: Alpha** - Core compiler functionality implemented.

uada80 is a compiler for the Ada programming language that generates code for the Z80 8-bit microprocessor running CP/M 2.2. The project aims to support a substantial subset of Ada 2012 and pass the ACATS conformance tests.

**Target Platforms**: CP/M 2.2 and MP/M II on Z80
- CP/M 2.2: `.com` executables, single-threaded, programs load at 0x0100
- MP/M II: `.prl` relocatable executables, preemptive multitasking via OS primitives
- Access to BDOS for file I/O and console operations
- Approximately 57K TPA on typical 64K system

This project builds on experience from [uplm80](https://github.com/avwohl/uplm80), a PL/M-80 compiler for Z80, reusing proven optimization techniques.

## Features

Phases 1 and 2 are complete: lexer, parser, basic types, procedures and
functions, control flow, arrays, records, packages, enumeration types, access
types, derived types, unconstrained arrays, and AST optimization. Phase 3 (ACATS
compliance) has generics, exception handling, attributes and representation
clauses. The Z80 standard library and ACATS validation are still open.
**579 ACATS tests pass end-to-end** on cpmemu. Ada tasking runs on MP/M II with
OS-native preemptive multitasking.

Integers are limited to 8-bit and 16-bit (32-bit via library), floating point is
software only, and the heap is small. See [docs/FEATURES.md](docs/FEATURES.md)
for the goals, the full checklist and all limitations.

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
git clone https://github.com/avwohl/uada80.git
cd uada80
python3 -m venv venv
source venv/bin/activate
pip install -e ".[dev]"
pip install um80
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

A Hello World program and more examples are in
[docs/EXAMPLES.md](docs/EXAMPLES.md) and in
[learn-ada-z80](https://github.com/avwohl/learn-ada-z80).

## Documentation

- [docs/FEATURES.md](docs/FEATURES.md) - Goals, feature status by phase, limitations
- [docs/EXAMPLES.md](docs/EXAMPLES.md) - Example programs
- [docs/TESTING.md](docs/TESTING.md) - Running tests, ACATS and learn-ada-z80 results
- [docs/MPM2_TASKING.md](docs/MPM2_TASKING.md) - Ada tasking on MP/M II, building for MP/M II, runtime libraries
- [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md) - How to contribute
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) - Compiler architecture and design
- [docs/AST_DESIGN.md](docs/AST_DESIGN.md) - Abstract syntax tree structure
- [docs/OPTIMIZATION_ANALYSIS.md](docs/OPTIMIZATION_ANALYSIS.md) - Optimization strategies
- [docs/LANGUAGE_SUBSET.md](docs/LANGUAGE_SUBSET.md) - Supported Ada language features
- [docs/CPM_RUNTIME.md](docs/CPM_RUNTIME.md) - **Complete Ada/CP/M runtime specification**
- [docs/CPM_QUICK_REFERENCE.md](docs/CPM_QUICK_REFERENCE.md) - **CP/M quick reference for developers**
- [cpm22_bdos_calls.pdf](https://github.com/avwohl/retro_docs/blob/main/cpmemu/cpm22_bdos_calls.pdf) - BDOS system call reference
- [cpm22_bios_calls.pdf](https://github.com/avwohl/retro_docs/blob/main/cpmemu/cpm22_bios_calls.pdf) - BIOS hardware interface
- [cpm22_memory_layout.pdf](https://github.com/avwohl/retro_docs/blob/main/cpmemu/cpm22_memory_layout.pdf) - CP/M memory organization
- [specs/](specs/) - Ada language specifications and ACATS tests

## License

This project is licensed under the GNU General Public License v2.0 - see LICENSE for details.

## References

- [Ada Reference Manual (Ada 2012)](https://www.adaic.org/resources/add_content/standards/12rm/RM-Final.pdf)
- [ACATS Test Suite](http://www.ada-auth.org/acats.html)
- [Z80 CPU User Manual](http://www.z80.info/zip/z80cpu_um.pdf)
- [uplm80 - PL/M Compiler](https://github.com/avwohl/uplm80)
## Related Projects

- [80un](https://github.com/avwohl/80un) - Unpacker for the CP/M archive and compression formats LBR, ARC, squeeze, crunch, and CrLZH.
- [cpmdroid](https://github.com/avwohl/cpmdroid) - Z80/CP/M emulator for Android phones and tablets. It emulates the RomWBW HBIOS interface and a VT100 terminal.
- [cpmemu](https://github.com/avwohl/cpmemu) - Z80/CP/M emulator for Linux and Windows, with Z80 and 8080 CPU cores. It translates the BDOS and BIOS calls of CP/M 2.2 programs to the host file system.
- [ioscpm](https://github.com/avwohl/ioscpm) - Z80/CP/M emulator for iOS and macOS. It emulates the RomWBW HBIOS interface and runs CP/M 2.2 and CP/M 3.
- [learn-ada-z80](https://github.com/avwohl/learn-ada-z80) - Collection of more than 90 Ada example programs for uada80, the Ada compiler for the Z80 processor and CP/M.
- [mbasic](https://github.com/avwohl/mbasic) - Python interpreter for MBASIC 5.21, the Microsoft BASIC-80 for CP/M. Two compiler backends compile the programs to CP/M .COM files or to JavaScript.
- [mbasic2025](https://github.com/avwohl/mbasic2025) - Reconstruction of the lost source code of MBASIC 5.21, the Microsoft BASIC-80 for CP/M. The MACRO-80 source code assembles to a binary that matches mbasic.com byte for byte.
- [mbasicc](https://github.com/avwohl/mbasicc) - C++17 interpreter for MBASIC 5.21, the Microsoft BASIC-80 for CP/M. It runs on Linux and macOS.
- [mbasicc_web](https://github.com/avwohl/mbasicc_web) - Web browser interpreter for MBASIC 5.21, the Microsoft BASIC-80 for CP/M. Emscripten compiles the mbasicc interpreter to WebAssembly.
- [mpm2](https://github.com/avwohl/mpm2) - Z80 emulator for MP/M II, the multi-user CP/M operating system. Users connect over SSH, and SFTP clients transfer files.
- [romwbw_emu](https://github.com/avwohl/romwbw_emu) - Hardware-level Z80/CP/M emulator for Linux and macOS. It emulates the RomWBW HBIOS interface and switches banks in 512 KB of ROM and 512 KB of RAM.
- [scelbal](https://github.com/avwohl/scelbal) - Floating-point BASIC interpreter for the 8080 processor and CP/M. A translator converts the original 8008 source code to 8080 source code.
- [uc80](https://github.com/avwohl/uc80) - C compiler for the Z80 processor and CP/M. It optimizes for small code size.
- [ucow](https://github.com/avwohl/ucow) - Cowgol compiler for the Z80 processor and CP/M. It runs on Linux in Python.
- [um80_and_friends](https://github.com/avwohl/um80_and_friends) - Linux toolchain that is compatible with Microsoft MACRO-80. It has an assembler, a linker, a librarian, and a disassembler.
- [upeepz80](https://github.com/avwohl/upeepz80) - Peephole optimizer for Z80 compilers that write lowercase Z80 assembly language. It shortens jumps to jr, builds djnz loops, and removes dead stores.
- [uplm80](https://github.com/avwohl/uplm80) - PL/M-80 compiler for the Z80 processor and CP/M. It writes Intel 8080 and Zilog Z80 assembly language.
- [z80cpmw](https://github.com/avwohl/z80cpmw) - Z80/CP/M emulator for Windows. It emulates the RomWBW HBIOS interface and boots CP/M from disk images.

## See Also

- [GNAT](https://www.adacore.com/gnatpro) - For production Ada development, use GNAT or other mature Ada compilers
