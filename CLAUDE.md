# Claude Code Notes for UADA80

## Work In Progress (2025-12-17)

**Session accomplished:**
- Parser: 100% (2849/2849 ACATS files)
- Semantic: 100% (2742/2742 ACATS tests pass)
- Execution tests: 120/120 pass (12 Float64 tests, all passing)
- All tests: 6905/6905 pass
- Complete Float64 IEEE 754 double precision support for Z80

**Float64 features implemented:**
- Arithmetic: add, sub, mul, div, neg, abs
- Comparison: eq, ne, lt, le, gt, ge (all operators)
- Conversion: itof (Integer→Long_Float), ftoi (Long_Float→Integer)
- Rounding: floor, ceiling, truncation, rounding attributes
- Math: sqrt
- Constants: zero, one
- Copy and type operations

**Key fixes this session:**
1. **Float64 identifier lowering** - `_lower_identifier` returns address for Float64 locals:
   - Was returning `local.vreg` (2-byte value) instead of stack address
   - Now uses LEA to compute actual 8-byte value address
   - Fixed `_resolve_local_type` for SubtypeIndication AST nodes
2. **Float64 ceiling function** - Fixed `_f64_ceil`:
   - Added `had_frac` flag to track if any fractional bytes were non-zero
   - Only adds 1.0 if there were actual fractional bits (ceiling(3.0) = 3, not 4)
   - Uses `_f64_one` to write 1.0 at runtime instead of `_const_one`
3. **Float64 rounding attribute** - Implemented `_f64_round`:
   - Round half away from zero: if x >= 0, trunc(x + 0.5); if x < 0, trunc(x - 0.5)
   - Fixed JR out-of-range errors (changed to JP for long jumps)

**Previous session fixes:**
1. **Float64 multiply** - 4-bit right shift to align bit 56 to bit 52
2. **Float64 divide** - Save/restore A register around shift calls
3. **Float64 itof** - Fixed argument order and bit packing
4. **Float64 comparison operators** - All 6 comparison operators working
5. **Float64 unary operators** - Negation, abs, plus working

**Next steps to consider:**
- Float64 exponentiation (Float ** Integer)
- Float64 remainder/mod operations
- Elementary math functions (sin, cos, exp, log) - complex for Z80
- MP/M tasking support

---

## ACATS Compliance Progress: 80%

**Last Updated:** 2025-12-16 (Semantic 100%: c34005j fixed with aggregate function call detection)

### Progress Summary

| Component          | Complete | Notes                                      |
|--------------------|----------|--------------------------------------------|
| **Parser**         | 100%     | 2849/2849 ACATS files parse (100%)         |
| **Semantic**       | 100%     | 2742/2742 ACATS tests pass (all chapters)  |
| **Code Gen**       | 90%      | Full Z80 assembly output, runtime calls    |
| **Runtime**        | 70%      | Basic ops, I/O, exceptions; no tasking     |
| **Standard Lib**   | 95%      | 1,094 packages in adalib/                  |
| **Execution Tests**| 100%     | 120/120 pass (12 Float64 tests)            |
| **OVERALL**        | **75%**  | Estimated ~3,200/4,725 ACATS tests         |

### Feature Completion by Category

```
Legend: [====] 100%  [=== ] 75%  [==  ] 50%  [=   ] 25%  [    ] 0%

Basic Types (Integer, Boolean, etc.)    [====] 100%
Control Structures (if/case/loop)       [====] 100%
Arrays (1D, multi-dim, slices)          [====] 100%
Records (simple, variant, discriminant) [====] 100%
Access Types (pointers)                 [====] 98%
Packages (spec, body, child, private)   [====] 100%
Subprograms (procedures, functions)     [====] 100%
Operators (overloading, renaming)       [====] 100%
Exception Handling                      [====] 100%
Attributes (100+ implemented)           [====] 98%
Pragmas (40+ handled)                   [=== ] 95%
Tagged Types / OOP                      [=== ] 95%
Generics                                [=== ] 85%
Contracts (Pre/Post/Invariant)          [=== ] 90%
Ada 2012 Features                       [=== ] 95%
Ada 2022 Features                       [==  ] 70%
Tasking                                 [=   ] 30%
Protected Types                         [=   ] 30%
Real-Time / Concurrency                 [    ] 0%
```

### What Blocks 100% Compliance

1. **ACATS infrastructure** - ✓ DONE: Report, ImpDef, 70+ foundation packages in acats/
2. **Parser** - ✓ DONE: 100% of ACATS files parse (2849/2849)
3. **Semantic** - ✓ DONE: 2742/2742 ACATS semantic tests pass (100%)
4. **Tasking/protected types** - Requires MP/M target (CP/M is single-threaded)

**Cross-file is solved:** Multi-file compilation (`python -m uada80 *.ada`) parses all files into one AST. The "not found" errors are missing ACATS support packages, not a symbol resolution issue.

**Note:** Full 100% compliance is achievable by targeting MP/M instead of CP/M. MP/M provides OS-level threading, mutexes, and queues. The ul80 linker already supports .prl output for MP/M. All existing CP/M tests remain compatible (upward compatible).

**Memory:** The 64KB TPA limit can be extended via 512KB bank switching (supported by cpmemu and common Z80 hardware). Bank overlay management is complex - determining what code/data goes in which bank requires careful analysis. Deferred to last.

### Recent Changes (2025-12-16)

**Semantic at 100% (2742/2742 ACATS tests pass)**

**Semantic improvements (this session):**
1. **Generic subprogram body context** - Set `current_subprogram` for return statement validation in generic function bodies (semantic.py:1267-1277)
2. **c34005j: Function call with aggregate argument** - Detects `F((aggregate))` pattern in IndexedComponent (semantic.py:4468-4479)
3. **Overload resolution across scopes** - `all_overloads()` now searches ALL visible scopes including USE'd packages (symbol_table.py:6313-6347)
3. **c32115a: Constrained access types** - `_resolve_type()` handles IndexedComponent/Slice (semantic.py:2914-2921)
4. **c34004c: Integer * Universal_Real** - Added to `common_type()` for fixed-point context (type_system.py:1238-1251)
5. **Array type derivation** - Creates new ArrayType with name and base_type chain (semantic.py:2764-2774, type_system.py:299)
6. **Array type conversion** - `can_convert()` checks same component type + dimensions (type_system.py:1183-1192)
7. **Boolean array NOT** - Element-wise negation for Boolean arrays (semantic.py:4359-4376)
8. **Boolean array AND/OR/XOR** - Element-wise logical ops with base_type chain check (semantic.py:4284-4306)
9. **Aggregate expected_type** - `_analyze_aggregate()` now accepts and uses expected_type (semantic.py:3962-3984)

**Previous parser/lexer fixes:**
1. **Aggregate PIPE choices** - Fixed `(A|B => Value)` syntax in aggregates and named associations
2. **Child unit dotted names** - `procedure Ada.Unchecked_Deallocation` now parses correctly
3. **Multi-name generic formals** - `F, L : E;` in generic formals parsed correctly
4. **Character literals after RANGE** - `Type RANGE 'A'..'E'` now lexes correctly
5. **Entry family syntax** - `entry F(1..3)(Params)` classic Ada entry family now parses
6. **Access type with range constraint** - `access T range Low..High` now parses
7. **Array component with range constraint** - `array (...) of Integer range Low..High` parses

**Previous semantic fixes:**
1. **Subtype indication with constraints** - `subtype Arr is Array_Type(1..10)` resolves types
2. **Access-to-array dereference** - Implicit dereference for indexing, slicing, and attributes
3. **Generic formal visibility** - Separate bodies see generic formals from spec
4. **String literal compatibility** - String literals assign to array-of-character types
5. **Derived type conversion** - Type conversion between derived types and ancestors
6. **SubtypeIndication in _resolve_type** - Record component types resolve correctly

### ACATS Chapter Test Results (2025-12-16)

| Chapter | Pass Rate | Notes |
|---------|-----------|-------|
| c3 (Types) | 100/100 (100%) | All type tests pass |
| c4 (Names) | 357/357 (100%) | Names and expressions |
| c5 (Statements) | 123/123 (100%) | Control flow |
| c6 (Subprograms) | 119/119 (100%) | Procedures, functions |
| c7 (Packages) | 82/82 (100%) | Packages, visibility |
| c8 (Visibility) | all pass | Visibility rules |
| c9 (Tasks) | all pass | Task/protected types |
| ca (Elaboration) | all pass | Compilation order |
| cb (Interfaces) | all pass | Library interfaces |
| cc (Generics) | all pass | Generic units |
| cd (Rep clauses) | all pass | Representation |
| ce (IO) | all pass | Input/Output |

**Overall ACATS Semantic: 2742/2742 (100%)**

Note: All semantic tests pass. The parser handles 100% of ACATS files (2849/2849). Test suite total: 6810/6810 tests pass.

### Known Codegen Issues

All previously known issues have been resolved:
- ~~String concatenation~~ - Fixed (uses static buffer)
- ~~Put_Line dispatch~~ - Fixed (improved `_is_string_type()` detection)
- ~~String representation~~ - Fixed (standardized on null-terminated C-style)
- ~~Integer'Image~~ - Fixed (capture result before popping arg)

Current remaining items:
1. **Float64 arithmetic precision** - Some edge cases may have rounding differences
2. **Tasking not implemented** - Requires MP/M target for OS-level threading

### How to Update This Section

When features are added or fixed, update:
1. The percentage in the header
2. The component table
3. The feature progress bars
4. The "What Blocks" list if items are resolved

### Running ACATS Tests Locally

```bash
# Parse all ACATS files (tests parser coverage)
python -m uada80.tests.test_acats_parsing

# Run semantic analysis tests
pytest tests/test_semantic.py -v

# Run execution tests (31 tests)
pytest tests/test_execution.py -v

# Full test suite
pytest tests/ -v
```

### Roadmap to Higher Compliance

#### Phase 1: 65% → 75% (ACATS Infrastructure + Parser) ✓ COMPLETE
**Impact: +10% (~470 tests)**

| Task | Effort | Status |
|------|--------|--------|
| Multi-file CLI | Done | ✓ `main.py` accepts multiple files |
| Build ACATS Report.a stub | Done | ✓ `acats/report.ada` |
| Build ACATS ImpDef stub | Done | ✓ `acats/impdef.ada` |
| Foundation packages (fXXXX) | Done | ✓ 70+ packages in acats/ |
| **Parser 100% ACATS** | Done | ✓ 2849/2849 files parse |

**Multi-file compilation works:** `python -m uada80 file1.ada file2.ada file3.ada -o out.asm`

All files are parsed into a single AST, so cross-file symbol resolution is automatic.

#### Phase 2: 75% → 85% (Semantic Edge Cases) ✓ COMPLETE
**Impact: +10% (~470 tests)**

| Task | Effort | Status |
|------|--------|--------|
| Fix "not a type" errors | Done | ✓ All resolved |
| Improve static expression evaluation | Done | ✓ Working |
| Fix discriminant-dependent components | Done | ✓ Working |
| Better generic formal type matching | Done | ✓ Working |
| **Semantic 100% ACATS** | Done | ✓ 2742/2742 tests pass |

#### Phase 3: 85% → 95% (Runtime Completeness)
**Impact: +10% (~470 tests)**

| Task | Effort | Files |
|------|--------|-------|
| Full floating-point (64-bit) | High | `runtime/float64.mac` |
| File I/O beyond console | Medium | `runtime/fileio.mac` |
| Complete Calendar package | Low | `runtime/calendar.mac` |
| All Text_IO formatting | Medium | `runtime/textio.mac` |

#### Phase 4: 95% → 100% (MP/M Tasking)
**Impact: +5% (~235 tests) - Achievable with MP/M target**

| Task | Effort | Notes |
|------|--------|-------|
| MP/M process spawning | High | Use MP/M BDOS 147 (P_CREATE) |
| MP/M queue-based messaging | High | BDOS 135-141 for task entries |
| Protected type via MP/M mutex | Medium | BDOS 132 (S_LOCK) / 133 (S_UNLOCK) |
| Delay via MP/M dispatcher | Low | BDOS 141 (P_DELAY) |
| Build .prl output (ul80 ready) | Done | ul80 supports .prl format |

**MP/M BDOS Functions for Tasking:**
- 132: S_LOCK (mutex lock)
- 133: S_UNLOCK (mutex unlock)
- 135: Q_MAKE (create queue)
- 137: Q_WRITE (send to queue)
- 138: Q_READ (receive from queue)
- 141: P_DELAY (timed wait)
- 142: P_DISPATCH (yield)
- 147: P_CREATE (spawn process)
- 148: P_PRIORITY (set priority)
- 149: P_ABORT (terminate process)

#### Phase 5: Banked Memory (Stretch Goal)
**Impact: Enables very large programs beyond 64KB TPA**

| Task | Effort | Notes |
|------|--------|-------|
| Bank switching runtime | Very High | 512KB via port I/O |
| Overlay manager | Very High | Decide what lives in each bank |
| Code/data bank placement | Very High | Static analysis or hints |
| Cross-bank call thunks | High | Trampoline code for bank switches |

**Deferred:** Bank overlay management is complex. The compiler would need to analyze call graphs and data access patterns to decide bank placement. Leave for last - most programs fit in 64KB.

### Runtime Library Status

**Implemented in `runtime/runtime.mac` (1,693 lines):**
- `_mul16`, `_div16`, `_mod16` - 16-bit arithmetic
- `_put_line`, `_put_int`, `_put_char` - Console output
- `_get_int`, `_get_char`, `_get_line` - Console input
- `_heap_alloc`, `_heap_free` - Dynamic memory (bump allocator)
- `_raise_constraint_error`, `_raise_program_error`, `_raise_storage_error`
- `_exc_do_raise`, `_exc_get_name` - Exception handling
- `_dispatch_call` - Tagged type dispatch
- `_fixed_add`, `_fixed_sub`, `_fixed_mul`, `_fixed_div` - Q16.16 fixed-point
- `_str_len`, `_int_to_str`, `_str_hash`, `_int_hash` - String/hash utilities

**Additional modules:**
- `containers.mac` (1,909 lines) - Vector, list, map, set
- `fileio.mac` (1,242 lines) - Text_IO, Sequential_IO
- `float48.mac` (346 lines) - 48-bit floating-point (z88dk compatible)
- `float64.mac` (~700 lines) - 64-bit IEEE 754 double precision
- `tasking.mac` (893 lines) - Task scheduling stubs
- `lists.mac`, `vectors.mac` - Container implementations

**Missing/incomplete:**
- Float64 full arithmetic (add, sub, mul, div need complete implementations)
- Real file I/O (only console works in CP/M mode)
- MP/M tasking runtime (`runtime/mpm_task.mac` - not yet written)
- MP/M protected type runtime (`runtime/mpm_protected.mac` - not yet written)

**Target Platforms:**
- CP/M 2.2+ (current) - .com files, single-threaded
- MP/M II (planned) - .prl files, multi-process with OS threading

---

## Running Z80 Executables

### Toolchain Overview

The full toolchain for compiling and running Ada programs on Z80:

```
Ada Source (.ada/.adb/.ads)
    ↓ uada80 (this compiler)
Z80 Assembly (.asm)
    ↓ um80 (assembler)
Relocatable Object (.rel)
    ↓ ul80 (linker) + libada.lib
    ├──→ CP/M Executable (.com) - single-threaded
    └──→ MP/M Executable (.prl) - multi-process, OS threading
    ↓ cpmemu (emulator)
Output
```

**Target Selection:**
- `.com` (default): Standard CP/M 2.2+ executable
- `.prl`: MP/M II page-relocatable executable with full tasking support

### Tool Locations

- **uada80**: This project (`python -m uada80`)
- **um80**: `~/src/um80_and_friends/` or `~/.local/bin/um80`
- **ul80**: `~/src/um80_and_friends/` or `~/.local/bin/ul80`
- **ulib80**: Library manager at same location
- **cpmemu**: `~/src/cpmemu/src/cpmemu`

### Compiling and Running an Ada Program

```bash
# 1. Compile Ada to Z80 assembly
python -m uada80 program.ada -o program.asm

# 2. Assemble to relocatable object
um80 program.asm -o program.rel

# 3. Link with runtime library
ul80 program.rel -L runtime/ -l libada.lib -o program.com

# 4. Run on CP/M emulator (Z80 mode for full instruction set)
~/src/cpmemu/src/cpmemu --z80 program.com
```

### Quick Test Command

For a simple test without the runtime library:

```bash
# Compile, assemble, and run
python -m uada80 test.ada -o test.asm && \
um80 test.asm -o test.rel && \
ul80 test.rel -o test.com && \
~/src/cpmemu/src/cpmemu --z80 test.com
```

### cpmemu Options

```bash
# Basic execution
~/src/cpmemu/src/cpmemu program.com

# Z80 mode (required for Z80-specific instructions)
~/src/cpmemu/src/cpmemu --z80 program.com

# With command line arguments
~/src/cpmemu/src/cpmemu --z80 program.com arg1 arg2

# Progress reporting (useful for long-running tests)
~/src/cpmemu/src/cpmemu --z80 --progress=100 program.com
```

### Environment Variables for cpmemu

```bash
export CPM_PROGRESS=100        # Report every 100M instructions
export CPM_PRINTER=/tmp/lst    # Redirect printer output
export CPM_DEBUG_BDOS=9,20,21  # Debug specific BDOS calls
```

### Runtime Library

The Ada runtime library is in `runtime/`:

- `runtime/libada.lib` - Prebuilt library
- `runtime/*.mac` - Source files (macro assembler format)
- `runtime/Makefile` - Build instructions

To rebuild the runtime:

```bash
cd runtime/
make clean && make
```

### Testing Compiled Programs

When adding execution tests, use this pattern:

```python
import subprocess
import tempfile
import os

def run_ada_program(source_code: str) -> tuple[int, str, str]:
    """Compile and run Ada source, return (exit_code, stdout, stderr)."""
    with tempfile.TemporaryDirectory() as tmpdir:
        # Write source
        ada_file = os.path.join(tmpdir, "test.ada")
        with open(ada_file, "w") as f:
            f.write(source_code)

        # Compile
        asm_file = os.path.join(tmpdir, "test.asm")
        subprocess.run(["python", "-m", "uada80", ada_file, "-o", asm_file], check=True)

        # Assemble
        rel_file = os.path.join(tmpdir, "test.rel")
        subprocess.run(["um80", asm_file, "-o", rel_file], check=True)

        # Link
        com_file = os.path.join(tmpdir, "test.com")
        subprocess.run(["ul80", rel_file, "-o", com_file], check=True)

        # Run
        result = subprocess.run(
            [os.path.expanduser("~/src/cpmemu/src/cpmemu"), "--z80", com_file],
            capture_output=True,
            text=True,
            timeout=30
        )
        return result.returncode, result.stdout, result.stderr
```

### CP/M Exit Codes

- Exit code 0: Success (program called BDOS function 0 or returned normally)
- Exit code 1: Error or assertion failure
- The emulator returns the value passed to BDOS warm boot

### Debugging Tips

1. **Check assembly output**: The generated .asm file shows the Z80 code
2. **Use progress reporting**: `--progress` shows instruction count
3. **Debug BDOS calls**: `CPM_DEBUG_BDOS=2` shows console output calls
4. **Ctrl+C**: Press 5 times rapidly to exit cpmemu if program hangs

### Memory Layout

```
0x0000-0x00FF  System area (BDOS vectors, FCBs, DMA)
0x0100-0xFBFF  TPA - where .com programs load and run
0xFC00+        BDOS/BIOS (emulated)
```

Maximum program size is approximately 57KB.

### Common Issues

1. **"um80 not found"**: Add `~/.local/bin` to PATH or use full path
2. **Link errors**: Ensure libada.lib is built and -L path is correct
3. **Timeout in tests**: Z80 is slow; increase timeout for complex programs
4. **Missing runtime**: Some Ada features need runtime support in libada.lib

### um80 Z80 Mode

**IMPORTANT**: um80 defaults to 8080 mode. The code generator emits a `.Z80` directive at the start of each assembly file to enable Z80 instructions.

Key points:
- The `.Z80` directive must appear before any Z80-specific instructions
- Instruction mnemonics must be UPPERCASE for um80 (lowercase won't work)
- um80 does NOT support undocumented Z80 instructions (IXH, IXL, IYH, IYL)
- Use `PUSH IX / POP HL` instead of `LD H, IXH / LD L, IXL`

### Symbol Mangling

User-defined symbols (functions, procedures, global variables) are prefixed with `_` in the generated assembly to avoid collisions with Z80 instruction mnemonics.

**Why this matters**: Z80 mnemonics like `ADD`, `SUB`, `INC`, `DEC`, `CALL`, `RET`, `PUSH`, `POP`, etc. are valid Ada identifiers. Without mangling, an Ada function named `Add` would generate `CALL Add` which um80 interprets as an invalid instruction rather than a call to a label.

**How it works**:
- Ada function `Add` → assembly label `_Add:`
- Ada function `Test` → assembly label `_Test:`
- Ada global `Counter` → assembly label `_Counter:`
- Runtime symbols already start with `_` (e.g., `_mul16`, `_put_line`) and are unchanged

**Example**:
```ada
-- This Ada code works correctly:
function Add(A, B : Integer) return Integer is
begin
    return A + B;
end Add;
```

Generates:
```asm
_Add:
    ; function body
    RET
```

The mangling is handled automatically by the code generator in `uada80/codegen/__init__.py` via the `_mangle_symbol()` helper.

### Execution Tests

End-to-end execution tests are in `tests/test_execution.py`. They:
1. Compile Ada source to Z80 assembly
2. Assemble with um80
3. Link with ul80
4. Run with cpmemu
5. Verify correct execution

Some tests are marked `xfail` because they need runtime library functions that aren't yet built for um80 format.
