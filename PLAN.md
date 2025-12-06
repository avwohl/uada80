# uada80 - Ada Compiler for Z80

## Project Overview

Full Ada language compiler targeting Z80/8080 processors. Primary goal: pass ACATS (Ada Conformance Assessment Test Suite) 4.2A.

## Resources Acquired

- **Ada 2022 Reference Manual**: `docs/ada-spec/ada-rm-2022.pdf` (3.9 MB)
- **ACATS 4.2A Test Suite**: `tests/acats/` (4,725 test files)
  - B tests: Compiler error detection tests
  - C tests: Execution/conformance tests
  - A tests: Additional conformance tests

## Architecture

Based on successful `uplm80` PLM-80 compiler architecture:

```
Source Code (.ada/.adb/.ads)
      |
[1] Lexer (ada_lexer.py)
      |
Token Stream
      |
[2] Parser (ada_parser.py)
      |
Abstract Syntax Tree
      |
[3] Semantic Analysis (ada_semantic.py)  <- Ada-specific, complex
      |
Validated/Decorated AST
      |
[4] AST Optimizer (ada_ast_optimizer.py)
      |
Optimized AST
      |
[5] Code Generator (ada_codegen.py)
      |
Assembly Code (.mac)
      |
[6] Peephole Optimizer (shared/z80_peephole.py)  <- REUSE from uplm80
      |
Optimized Assembly
      |
[7] Post-Assembly Optimizer (shared/z80_postopt.py)  <- REUSE from uplm80
      |
Final Assembly
      |
[8] Assembler (um80) -> Object (.rel)
      |
[9] Linker (ul80) -> Executable (.com)
```

## Code Reuse from uplm80

### Immediate Extraction (Phase 1)

| Component | Source | Lines | Reusability | Effort |
|-----------|--------|-------|-------------|--------|
| Post-Assembly Optimizer | `uplm80/postopt.py` | 792 | 99% | 2-3 hours |
| Peephole Optimizer | `uplm80/peephole.py` | 2,877 | 90% | 6-8 hours |
| Symbol Table | `uplm80/symbols.py` | 190 | 90% | 1-2 hours |
| Error Handling | `uplm80/errors.py` | 104 | 100% | 30 min |

**Total reusable: ~3,900 lines (80% as-is)**

### Key Optimizations Inherited

**From postopt.py:**
- Tail merging with skip trick (DB 21H)
- JP to JR conversion (3-byte to 2-byte jumps)
- Multi-pass optimization loop

**From peephole.py:**
- Push/pop elimination
- Redundant load elimination
- Register tracking state machine
- Tail call optimization (CALL x; RET -> JP x)
- Strength reduction (MVI A,0 -> XRA A)
- Z80 mnemonic translation
- Relative jump conversion

## Project Structure

```
uada80/
├── uada80/
│   ├── __init__.py
│   ├── compiler.py          # Main driver
│   ├── ada_lexer.py         # Ada tokenizer
│   ├── ada_tokens.py        # Token definitions
│   ├── ada_parser.py        # Recursive descent parser
│   ├── ada_ast.py           # AST node definitions
│   ├── ada_semantic.py      # Type checking, overload resolution
│   ├── ada_ast_optimizer.py # High-level optimizations
│   ├── ada_codegen.py       # Z80 code generation
│   └── ada_runtime.py       # Runtime library routines
│
├── shared/                   # Extracted from uplm80
│   ├── __init__.py
│   ├── z80_peephole.py      # Pattern-based optimizer
│   ├── z80_postopt.py       # Post-assembly optimizer
│   ├── symbols.py           # Symbol table
│   ├── errors.py            # Error reporting
│   └── target.py            # Target enum (Z80/8080)
│
├── runtime/                  # Z80 runtime library
│   ├── startup.asm          # CRT0 equivalent
│   ├── math.asm             # Arithmetic routines
│   ├── string.asm           # String operations
│   ├── exception.asm        # Exception handling
│   ├── tasking.asm          # Tasking support (if feasible)
│   └── io.asm               # I/O operations
│
├── docs/
│   └── ada-spec/
│       └── ada-rm-2022.pdf
│
├── tests/
│   ├── acats/               # ACATS 4.2A test suite
│   └── unit/                # Unit tests for compiler
│
└── tools/
    └── run_acats.py         # ACATS test runner adapter
```

## Implementation Phases

### Phase 1: Infrastructure & Reuse Setup
- [ ] Extract and adapt shared components from uplm80
- [ ] Set up project structure
- [ ] Create build/test infrastructure
- [ ] Adapt assembler/linker (um80/ul80) if needed

### Phase 2: Core Language Front-End
- [ ] Ada lexer (reserved words, identifiers, literals, operators)
- [ ] Ada parser - Core subset:
  - [ ] Package specifications and bodies
  - [ ] Subprogram declarations and bodies
  - [ ] Type declarations (scalar, array, record)
  - [ ] Object declarations
  - [ ] Statements (assignment, if, case, loop, return)
  - [ ] Expressions (arithmetic, logical, relational)

### Phase 3: Semantic Analysis
- [ ] Symbol table with Ada scoping rules
- [ ] Type system (strong typing, subtypes, derived types)
- [ ] Overload resolution
- [ ] Visibility rules
- [ ] Generic instantiation (basic)

### Phase 4: Code Generation
- [ ] Expression evaluation (stack-based, HL for addresses)
- [ ] Statement code generation
- [ ] Subprogram calling conventions
- [ ] Parameter passing (in, out, in out)
- [ ] Array and record access
- [ ] Exception handling (Constraint_Error, etc.)

### Phase 5: Runtime Library
- [ ] Basic I/O (Text_IO subset)
- [ ] Arithmetic operations (32-bit integer, if needed)
- [ ] String handling
- [ ] Exception propagation
- [ ] Memory management (simple heap)

### Phase 6: Advanced Features (for ACATS compliance)
- [ ] Access types (pointers)
- [ ] Generics
- [ ] Tasking (minimal subset, if feasible on Z80)
- [ ] Representation clauses
- [ ] Pragma support

### Phase 7: ACATS Testing & Fixes
- [ ] Run ACATS B tests (error detection)
- [ ] Run ACATS C tests (execution)
- [ ] Fix failures iteratively
- [ ] Document unsupported features

## Ada Language Challenges for Z80

### Memory Model
- Z80: 64KB address space
- Ada default Integer: 16-bit sufficient
- Long_Integer: 32-bit (require library support)
- Access types: 16-bit addresses

### Tasking
- Z80 is single-threaded
- Options:
  1. Not support (mark as unsupported in ACATS)
  2. Cooperative multitasking via explicit yields
  3. Timer interrupt-based preemption (complex)

### Exceptions
- Implement via jump tables or propagation chain
- Stack unwinding needed
- Keep simple: single exception handler per frame

### Floating Point
- No hardware FP on Z80
- Options:
  1. Software FP library (slow, large)
  2. Not support (many ACATS tests won't run)
  3. Fixed-point only

### Generic Instantiation
- Compile-time expansion (like C++ templates)
- Generate specialized code for each instantiation

## ACATS Test Categories

From ACATS 4.2A README:
- **Expected passes**: ~2,544 tests
- **Expected failures**: ~1,493 (platform-specific)
- **Unsupported**: ~124 (annexes not implemented)

Target: Pass core language tests; document Annex limitations.

## Z80 Calling Convention

Adopt from uplm80:
- Parameters: Pushed right-to-left on stack
- Return value: HL (16-bit) or HL:DE (32-bit)
- Preserved: BC, IX, IY
- Scratch: A, DE, HL

## Key Differences from PLM-80

| Aspect | PLM-80 | Ada |
|--------|--------|-----|
| Type system | Weak | Strong |
| Overloading | No | Yes |
| Generics | No | Yes |
| Exceptions | No | Yes |
| Tasking | No | Yes (optional) |
| Separate compilation | No | Yes |
| Packages | No | Yes |
| Visibility | Simple | Complex |

## Estimated Effort

| Phase | Effort |
|-------|--------|
| Phase 1: Infrastructure | 1 week |
| Phase 2: Front-end | 4-6 weeks |
| Phase 3: Semantic analysis | 4-6 weeks |
| Phase 4: Code generation | 4-6 weeks |
| Phase 5: Runtime | 2-3 weeks |
| Phase 6: Advanced features | 4-8 weeks |
| Phase 7: ACATS testing | 4-8 weeks |
| **Total** | **23-38 weeks** |

## Next Steps

1. Extract shared optimizer code from uplm80
2. Set up basic project structure
3. Implement minimal Ada lexer
4. Build parser for small Ada subset
5. Get "Hello World" compiling and running
6. Iterate towards ACATS compliance

## References

- Ada 2022 Reference Manual (ISO/IEC 8652:2023)
- ACATS 4.2A User Guide: `tests/acats/docs/`
- uplm80 source: `~/src/uplm80`
