# Claude Code Notes for UADA80

## Session (2025-12-20) - Protected Entry Barriers

**Session accomplished:**
- Implemented protected entry barrier support (`entry Get when Ready is`)
- Fixed parser to create EntryBody nodes instead of SubprogramBody
- Fixed entry body parameter setup for out parameters
- Protected operations, entries with barriers, all working

**Protected Entry Implementation:**
1. **Parser changes** - Create `EntryBody` with barrier, params, family index
2. **Barrier code generation**:
   - `barrier_recheck:` - Acquire lock, evaluate barrier, release lock
   - If barrier false: call `_PROT_ENQ`, loop back to recheck
   - `barrier_ok:` - Lock, execute body, unlock, call `_PROT_REB`, return
3. **Parameter setup** - Implemented `_setup_parameters` to call `_lower_parameter`
   - Out parameters correctly passed by reference (pointer in ix+6)

**Generated code flow:**
```
barrier_recheck:
    call _PROT_LCK       ; acquire lock
    ; evaluate barrier condition
    call _PROT_ULK       ; release lock
    jr nz, barrier_ok    ; if true, continue
    call _PROT_ENQ       ; enqueue this call
    jp barrier_recheck   ; loop back

barrier_ok:
    call _PROT_LCK       ; acquire lock
    ; execute entry body
    call _PROT_ULK       ; release lock
    call _PROT_REB       ; re-evaluate all barriers
    ret
```

**Files Modified:**
- `uada80/parser.py` - Create EntryBody for entry bodies in protected body
- `uada80/lowering.py` - Entry barrier checking, _setup_parameters

**Tests:** 6932/6932 tests pass

---

## Previous Session (2025-12-20) - Protected Entry Support

**Session accomplished:**
- Fixed protected operation call handling (Buffer.Put, Counter.Increment)
- Fixed Text_IO SelectedName procedure call handling
- Fixed protected operation parameter stack offsets
- Fixed ActualParameter unwrapping in _lower_expr
- Fixed Boolean literal evaluation in _eval_static_expr
- Fixed local procedure renaming visibility during lowering

**Key Fixes:**
1. **ActualParameter handling** - `_lower_expr` now unwraps ActualParameter nodes
2. **Boolean initialization** - `_eval_static_expr` handles True/False literals
3. **Protected operation parameter offsets** - Added prot_obj to `ir_func.params`
4. **SelectedName procedure calls** - Added handling for Text_IO package calls
5. **Protected call push order** - Push args first, then protected object last
6. **Procedure renaming** - Added alias_for resolution for Text_IO renames
7. **Local procedure renaming** - Added `local_renamings` tracking

**Tests:** 6932/6932 tests pass

---

## Previous Session (2025-12-20) - Protected Types Working

**Session accomplished:**
- Fixed protected type implementation - basic protected objects now work correctly
- Created separate `protlock.mac` module to fix linker symbol resolution issue
- Added ProtectedType handling in semantic analyzer for `Counter.Increment` syntax
- Added `test_protected_type_basic` execution test

**Protected Type Implementation:**
- Single protected objects (protected Counter is...) compile and execute correctly
- Protected procedures and functions work with proper lock/unlock semantics
- Lock byte initialization and component access fixed in lowering.py
- Byte store generation fixed in codegen for protected lock byte

**Key Fixes:**
1. **Linker symbol resolution** - TASKING module symbols pointed to wrong addresses
   - Created `runtime/protlock.mac` with just `_PROT_LCK` and `_PROT_ULK`
   - Linked before TASKING module so correct symbols take precedence
2. **Semantic analysis** - `_analyze_selected_name` didn't handle ProtectedType
   - Added lookup of operations in protected type for `Counter.Increment` syntax
   - Returns function return type or None for procedures
3. **Protected object initialization** - Lock byte and components properly initialized
4. **Byte store generation** - Fixed `_gen_store` to handle `IRType.BYTE` correctly

**Files Modified:**
- `runtime/protlock.mac` (new) - Minimal protected lock/unlock module
- `runtime/tasking.mac` - Removed lock/unlock code (now in protlock.mac)
- `runtime/Makefile` - Added protlock.mac to build
- `uada80/semantic.py` - Added ProtectedType handling
- `uada80/lowering.py` - Protected object initialization and component access
- `uada80/codegen/__init__.py` - Byte store generation fix

**Tests:** 147 execution tests pass, 6932 total tests pass

---

## Previous Session (2025-12-20) - MP/M II Tasking

**Session accomplished:**
- Implemented MP/M II tasking runtime (`runtime/mpm_task.mac`)
- Updated lowering.py to use 8-character symbol names for um80 compatibility
- Updated CP/M tasking runtime with symbol aliases for compatibility
- Both runtimes (CP/M and MP/M II) build successfully

**MP/M II Tasking Runtime:**
- Ada tasks map to MP/M subprocesses (P_CREATE, BDOS 144)
- Task entries map to MP/M message queues (Q_MAKE/Q_READ/Q_WRITE, BDOS 134-140)
- Delay statement uses P_DELAY (BDOS 141)
- Protected types use system flags (DEV_WAITFLAG/DEV_SETFLAG, BDOS 132-133)

**Key MP/M II BDOS Calls Used:**
- 132: DEV_WAITFLAG - Wait on flag (for protected types)
- 133: DEV_SETFLAG - Set flag (signal protected type unlock)
- 134: Q_MAKE - Create message queue
- 137: Q_READ - Read from queue (blocking)
- 139: Q_WRITE - Write to queue (blocking)
- 141: P_DELAY - Delay process
- 142: P_DISPATCH - Yield to scheduler
- 143: P_TERM - Terminate process
- 144: P_CREATE - Create subprocess

**Symbol Name Changes (8-char limit for um80):**
- `_TASK_TERMINATE` → `_TASK_TRM`
- `_ENTRY_CALL` → `_ENTRY_CL`
- `_ENTRY_ACCEPT` → `_ENTRY_AC`
- `_PROTECTED_LOCK` → `_PROT_LCK`
- Full mapping in lowering.py and runtime/*.mac

**Build Options:**
- `make` or `make TARGET=cpm` - Build libada.lib for CP/M
- `make TARGET=mpm` - Build libada_mpm.lib for MP/M II

**Tests:** 146 execution tests pass, 6931 total tests pass

---

## Previous Session (2025-12-20)

**Session accomplished:**
- Fixed remaining Float64 hyperbolic functions: cosh, tanh, coth
- Implemented CP/M file I/O support (Text_IO and Sequential_IO)
- Implemented stream attributes (T'Read, T'Write, T'Input, T'Output)
- All 146 execution tests pass, all 6931 total tests pass

**File I/O Implementation:**
- Text_IO.Create, Open, Close, Delete, Reset, Flush now call `_file_*` runtime
- File-based Put, Put_Line, Get, Get_Line for Text_IO.File_Type
- Sequential_IO.Read/Write use `_file_read`/`_file_write`
- Helper methods: `_is_file_type()`, `_get_lvalue_address()`, `_get_type_size()`, `_sizeof_type()`

**Stream Attributes:**
- T'Write, T'Read, T'Input, T'Output now use file I/O functions
- Streams are treated as file handles on Z80/CP/M

**Critical fix: Local constants for hyperbolic functions**
- `_lower_float64_cosh`, `_lower_float64_tanh`, `_lower_float64_coth` were using
  runtime constants `Label("_const_one_f64")` and `Label("_const_2")` which don't work
  correctly with `_f64_call_binary`
- **Fix:** Generate local constants via `_lower_float64_literal()` in each function
- All hyperbolic functions now work: sinh, cosh, tanh, coth, arcsinh, arccosh, arctanh, arccoth

**Tests:** 146 execution tests pass, 6931 total tests pass

---

## Previous Session (2025-12-19)

**Session accomplished:**
- Fixed sin/cos bug: functions were returning sinh/cosh values
- Fixed Float64 assignment bug for literal values (e.g., `Result := 1.0`)
- Inlined sin/cos Taylor series in lowering.py with local constants
- sin(1.0)=841, cos(1.0)=540, tan(1.0)=1557, cot(1.0)=642 all correct

**Critical fix: Float64 assignment from literals**
- `_lower_assignment` was calling `_lower_expr` which returns fixed-point for RealLiteral
- But Float64 assignment expects a pointer from `_lower_float64_operand`
- **Fix:** Check target type first; use `_lower_float64_operand` for Float64 targets

**Critical fix: Inlined trig functions use local constants**
- Runtime constants (Label("_const_one_f64"), etc.) don't work correctly with `_f64_call_binary`
- **Fix:** Generate local constants via `_lower_float64_literal()` in each inlined function
- Applied to: `_sin_from_ptr`, `_cos_from_ptr`, `_lower_float64_arctanh`, `_lower_float64_arccoth`

---

## Previous Session (2025-12-19)

**Session accomplished:**
- Fixed Float64 exponentiation (**) regression caused by symbol collision
- Added tan, exp, log, arctan functions for Long_Float (Float64)

**Critical fix: 8-character symbol truncation bug**
- um80 assembler truncates symbols to 8 characters
- `_f64_exp` (8 chars) and `_f64_exp_int` (12 chars) both truncated to `_F64_EXP`
- This caused ** operator to call Taylor series exp() instead of integer exponentiation
- **Fix:** Renamed `_f64_exp` to `_f64_e2x` (8 unique chars)
- Lesson: All PUBLIC symbols in runtime/*.mac must be unique within 8 characters

**New Float64 functions:**
- `_f64_tan`: tan(x) = sin(x)/cos(x)
- `_f64_atan`: arctan(x) using Taylor series with argument reduction for |x| > 1
- `_f64_e2x`: e^x using Taylor series with argument reduction via ln(2)
- `_f64_log`: ln(x) using IEEE 754 decomposition + Taylor series

**Complete Float64 feature set:**
- Arithmetic: add, sub, mul, div, rem, mod, neg, abs, exp_int (**)
- Comparison: eq, ne, lt, le, gt, ge
- Conversion: itof, ftoi
- Rounding: floor, ceiling, truncation, rounding
- Math: sqrt, sin, cos, tan, arctan, exp, log

**Next steps to consider:**
- Fix remaining hyperbolic functions (cosh, tanh, coth need local constants)
- Additional elementary functions (asin, acos)
- MP/M tasking support

---

## Previous Session (2025-12-19)

**Session accomplished:**
- Float64 remainder (rem) and modulo (mod) operations
- Fixed Float64 floor for negative numbers
- Fixed Float64 trunc partial byte masking
- All tests: 124 execution tests pass

**Float64 rem/mod implementation:**
- `_f64_rem`: Remainder with sign following dividend (X rem Y = X - Y * trunc(X/Y))
- `_f64_mod`: Modulo with sign following divisor (X mod Y = X - Y * floor(X/Y))
- Added execution tests for both operations

**Float64 floor fix:**
- Moved `_f64_neg_one` constant from DSEG to CSEG (was uninitialized memory!)
- Added `_had_frac` flag to track if fractional bits were non-zero
- Only subtract 1 for negative numbers that actually have fractional parts
- floor(-3.0) now correctly returns -3 (was incorrectly returning -4)

---

## Previous Session (2025-12-19)

**Session accomplished:**
- Switched code generator to pure Z80 lowercase assembly output (for upeepz80 compatibility)
- Integrated upeepz80 peephole optimizer into compilation pipeline
- All tests: 6907/6907 pass (122 execution tests)

**Z80 Assembly Changes:**
- All mnemonics now lowercase: `ld`, `push`, `pop`, `jp`, `jr`, `call`, `ret`, etc.
- All register names lowercase: `hl`, `de`, `bc`, `sp`, `ix`, `iy`, `a`, `b`, `c`, `d`, `e`, `h`, `l`
- All condition codes lowercase: `z`, `nz`, `c`, `nc`, `p`, `m`, `pe`, `po`
- Assembler directives remain uppercase: `CSEG`, `DSEG`, `DS`, `DW`, `DB`, `EXTRN`, `PUBLIC`

**Peephole Optimizer Integration:**
- Uses upeepz80 (sister project at ../upeepz80) for Z80-specific optimizations
- Applied as Phase 6 after code generation
- Tracks optimization statistics in `CompilationResult.peephole_stats`
- Optimizations include: xor a, jr conversion, push/pop elimination, djnz, jump threading

---

## Session (2025-12-17)

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

---

## Continuous Integration

GitHub Actions CI is configured for automated testing on every push and pull request to `main`.

### CI Workflows

| Workflow | File | Purpose |
|----------|------|---------|
| **Tests** | `.github/workflows/pytest.yml` | Run pytest on Python 3.10, 3.11, 3.12 |
| **Pylint** | `.github/workflows/pylint.yml` | Code quality check (threshold: 9.5/10) |

### CI Badges

The README displays live CI status badges:
```markdown
[![Tests](https://github.com/avwohl/uada80/actions/workflows/pytest.yml/badge.svg)](https://github.com/avwohl/uada80/actions/workflows/pytest.yml)
[![Pylint](https://github.com/avwohl/uada80/actions/workflows/pylint.yml/badge.svg)](https://github.com/avwohl/uada80/actions/workflows/pylint.yml)
```

### Test Configuration

The pytest workflow:
- Installs `um80` from PyPI (includes ul80 linker)
- Skips execution tests (require cpmemu which isn't pip-installable)
- Uses `-o addopts=""` to override pyproject.toml coverage settings

Execution tests in `tests/test_execution.py` use a module-level skip marker:
```python
pytestmark = skip_if_no_tools  # Skip all tests if cpmemu unavailable
```

### Running CI Locally

```bash
# Run tests (same as CI)
pytest tests/ -v --tb=short -o addopts=""

# Run pylint (same as CI)
pylint uada80/ --fail-under=9.5

# Check current pylint score
pylint uada80/
```

### Current Status

- **Pylint score**: 10.00/10
- **Tests**: 6932/6932 passing
- **Execution tests**: 147 pass (skipped in CI, run locally with cpmemu)

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
| **Execution Tests**| 100%     | 147/147 pass (incl. protected types)       |
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
Tasking                                 [==  ] 50%
Protected Types                         [=== ] 65%
Real-Time / Concurrency                 [    ] 0%
```

### What Blocks 100% Compliance

1. **ACATS infrastructure** - ✓ DONE: Report, ImpDef, 70+ foundation packages in acats/
2. **Parser** - ✓ DONE: 100% of ACATS files parse (2849/2849)
3. **Semantic** - ✓ DONE: 2742/2742 ACATS semantic tests pass (100%)
4. **Tasking/protected types** - MP/M II runtime implemented, needs testing on real MP/M

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
2. **Tasking runtime testing** - MP/M II runtime implemented, needs testing on actual MP/M

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

| Task | Effort | Status |
|------|--------|--------|
| MP/M process spawning | High | ✓ Done (mpm_task.mac: P_CREATE) |
| MP/M queue-based messaging | High | ✓ Done (mpm_task.mac: Q_MAKE/Q_READ/Q_WRITE) |
| Protected type via MP/M flags | Medium | ✓ Done (DEV_WAITFLAG/DEV_SETFLAG) |
| Delay via MP/M dispatcher | Low | ✓ Done (P_DELAY) |
| Build .prl output (ul80 ready) | Done | ul80 supports .prl format |
| Test on actual MP/M | Medium | Pending - needs MP/M environment |

**MP/M BDOS Functions Used in mpm_task.mac:**
- 132: DEV_WAITFLAG (wait on system flag)
- 133: DEV_SETFLAG (set system flag)
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
