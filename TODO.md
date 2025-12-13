# UADA80 - Remaining Work for Full Ada/ACATS

## Current Status (2024-12-13)

| Phase | Pass Rate | Notes |
|-------|-----------|-------|
| Parsing | 2849/2849 (100%) | Full Ada syntax supported |
| Semantic | 2742/2742 (100%) | All grouped ACATS tests pass |
| Lowering | 105/105 (100%) | All lowering tests pass |
| Codegen | 24/24 (100%) | All code generation tests pass |
| Unit Tests | 6775/6775 (100%) | All pass |

## Recent Fixes (2024-12-13)

1. [x] Ada 2022 declare expressions in expression functions
   - Parser now handles `(declare ... begin Expr)` syntax in expression function bodies

## Implemented Features

### Core Language (Ada 83/95)
- [x] All basic types (Integer, Float, Boolean, Character, String)
- [x] Enumeration types with attributes
- [x] Array types (constrained and unconstrained)
- [x] Record types (simple and variant)
- [x] Access types (pointers)
- [x] Subprograms (procedures and functions)
- [x] Packages (specification and body)
- [x] Exception handling
- [x] Generics (packages, procedures, functions)
- [x] Tasking (task types, protected types, entries)

### Ada 2005 Features
- [x] Interfaces
- [x] Null procedures
- [x] Overriding indicators

### Ada 2012 Features
- [x] Expression functions
- [x] Conditional expressions (if/case)
- [x] Quantified expressions (for all/some)
- [x] Membership tests with multiple choices
- [x] Contract aspects (Pre, Post)

### Ada 2022 Features
- [x] Declare expressions
- [x] Parallel blocks (`parallel do ... and do ... end parallel`)
- [x] Parallel loops (`parallel for`)
- [x] Delta aggregates
- [x] Container aggregates

### Type System
- [x] Type checking edge cases (Universal_Real, type conversion)
- [x] Overload resolution with expected type context
- [x] Enumeration literal overloading
- [x] Generic instantiation type matching
- [x] Discriminant handling
- [x] Array/record constraint checking

### Standard Library
- [x] Wide_Character / Wide_String types
- [x] System.Address type
- [x] Predefined exceptions

## Remaining Work

### ACATS Test Suite Support

The ACATS test suite requires certain support libraries that use GNAT-specific packages:

1. [ ] ACATS support library (Report.a) requires Ada.Text_IO, Ada.Calendar
2. [ ] Impdef (implementation defined values)
3. [ ] Spprt13 (support for chapter 13 tests)
4. [ ] Various FCxx packages (foundation code)

### Optional Standard Library Extensions

These can be added for full GNAT compatibility but are not required for core Ada:

- Ada.Text_IO and child packages (CP/M runtime provided instead)
- Ada.Strings and child packages
- Ada.Containers and child packages
- Ada.Calendar
- Ada.Numerics

## Z80/CP/M Target Notes

This compiler generates Z80 assembly for CP/M 2.2. Key characteristics:

- 16-bit integers (default)
- Software floating point (48-bit)
- 64KB address space (~57KB usable TPA)
- Cooperative multitasking (for tasking support)
- CP/M BDOS interface for I/O

## Usage

```bash
# Compile Ada source to Z80 assembly
uada80 program.ada -o program.asm

# Assemble with Z80 assembler (e.g., SLR's Z80ASM)
z80asm program.asm -o program.rel

# Link with runtime library
ul80 program.rel -l libada.lib -o program.com
```
