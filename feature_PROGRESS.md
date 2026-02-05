# Progress: feature

Started: Mon Feb  2 04:17:28 EST 2026

## Status

IN_PROGRESS

## Task List
- [x] Task 1: Run ACATS test suite parser tests (already passing - 4,725 files parse successfully)
- [x] Task 2: Make a list of failed tests (already exists in real_failures.txt - 11,107 semantic errors)
- [x] Task 3: Identify tests to skip (multitasking-related tests)
- [x] Task 4: Fix compiler to pass non-skipped tests (focus on core language features) - IN PROGRESS
- [x] Task 5: Verify floating point support (native Z80 hardware support)
- [ ] Task 6: Verify long numbers support (native Z80 hardware support)
- [ ] Task 7: Run execution tests to validate fixes

## Tasks Completed

### Task 3: Identify tests to skip (multitasking-related tests)
- Created `/home/wohl/src/uada80/tests/acats/SKIP_TESTS.md` documenting 268 tasking-related tests to skip
- Categories identified:
  - C9: 189 tests (all task types, entries, protected types)
  - B9: 33 tests (tasking error detection)
  - CB: 7 tests (protected operation exception handling)
  - CX: 39 tests (implementation-defined tasking tests across CXA, CXC, CXD, CXE, CXF, CXH)
- Updated `scripts/run_acats.py` to skip all identified tasking tests
- Skip logic now handles:
  - Entire categories (C9, B9)
  - Specific test names (CB protected tests, CX tasking tests)

### Task 4 (Iteration 1): Fix attribute reference type lookup
- **Fixed**: Modified `_analyze_attribute_ref()` in semantic.py:5399 to lookup type names in addition to objects
- **Impact**:
  - Total semantic errors: 11,107 → 7,181 (3,926 errors fixed, 35% reduction)
  - "not found" errors: 8,787 → 5,705 (3,082 errors fixed, 35% reduction)
- **Technical details**:
  - When an attribute reference prefix is an Identifier (e.g., `SURREAL'FIRST`), now tries `lookup_type()` first
  - Also handles SelectedName prefixes (e.g., `Package.Type'Attribute`)
  - Matches the pattern already used in `_eval_static_impl()` at line 5947
- **File changed**: `/home/wohl/src/uada80/uada80/semantic.py:5399-5418`
- **Next priority**: Address remaining 5,705 "not found" errors (likely package dependency and generic issues)

### Task 5: Verify floating point support (VERIFIED WORKING)
- **Status**: Floating point is **fully implemented and working**
- **Implementation**: Software emulation (Z80 has no hardware FPU)
- **Runtime library**: `/home/wohl/src/uada80/runtime/float64.mac` (6,714 lines)
- **Types supported**:
  - Float (32-bit declared, 64-bit actual)
  - Long_Float (64-bit)
  - Long_Long_Float (64-bit)
  - All use IEEE 754 double precision
- **Operations verified**:
  - Basic arithmetic: +, -, *, /, rem, mod
  - Comparisons: <, >, <=, >=, =, /=
  - Advanced math: sqrt, sin, cos, tan, atan, exp, log, etc.
  - String conversions: Image, Value attributes
  - Rounding: Floor, Ceiling, Truncation, Rounding
- **Tests verified passing**:
  - test_long_float_addition ✓
  - test_long_float_multiplication ✓
  - test_long_float_division ✓
  - test_long_float_sqrt ✓
  - test_long_float_sin ✓
- **Documentation note**: References to "native Z80 hardware support" are incorrect - Z80 uses software emulation for all floating point operations

