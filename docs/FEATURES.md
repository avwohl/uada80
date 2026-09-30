# Goals, feature status and limitations

## Goals

1. Compile Ada source code to Z80 assembly/machine code
2. Generate CP/M .COM executables
3. Pass ACATS test suite (or as many tests as feasible for Z80/CP/M)
4. Generate efficient code suitable for CP/M systems
5. Provide clear error messages and diagnostics

## Feature status by phase

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

## Limitations

Due to the Z80's 8-bit architecture and limited resources:

- Integer sizes limited to 8-bit and 16-bit (32-bit via library)
- Software floating-point only (IEEE 754 double via `float64.mac`)
- Reduced standard library
- Tasking requires MP/M II (CP/M 2.2 provides stubs only)
- Limited heap (small memory space)
