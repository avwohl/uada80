# Progress: feature

Started: Mon Feb  9 16:08:03 EST 2026

## Status

IN_PROGRESS

## Task List

Based on analysis of real_failures.txt (579 PASS, 290 LINK_ERROR, 246 NO_RESULT, 212 COMPILE_ERROR, 125 TIMEOUT, 1 FAIL):

### Semantic Analysis Fixes (High Impact)
- [x] Task 1: Fix _load_external_package() to handle GenericSubprogramUnit (LENGTH_CHECK/ENUM_CHECK) - affects 26+ tests
- [ ] Task 2: Fix package body visibility - inherit WITH/USE clauses from matching spec - affects 7 tests
- [ ] Task 3: Fix generic body discovery for separate compilation units

### Type System Fixes
- [ ] Task 4: Fix "expected Boolean, got 'Universal_Integer'" - wrong type inference (5 tests)
- [ ] Task 5: Fix Duration/Time arithmetic type checking (7 tests combined)
- [ ] Task 6: Fix string type assignment checking (6+ tests)

### Codegen Fixes
- [ ] Task 7: Fix lowering "NoneType" errors - unhandled AST nodes (5+ tests)
- [ ] Task 8: Fix codegen IRType comparison errors (2+ tests)

### Link Error Fixes (Symbol Generation)
- [ ] Task 9: Investigate and fix _E symbol generation (61 occurrences) - likely tasking entry codegen
- [ ] Task 10: Fix common symbol generation issues (_STOP, _INCR, _NEWPROC, etc.)

## Tasks Completed

### Iteration 1 (2026-02-09)
- Task 1: Fixed _load_external_package() to handle GenericSubprogramUnit
  - Added code to find and load generic procedures/functions from external files
  - Extended _find_package_file() to search for ACATS-style file naming (e.g., lencheck.ada)
  - Properly extract parameters and return type from generic subprogram declarations
  - Tests cd2a21a, cd2a21e, cd2a24a, cd3014c now compile without errors
  - All 7155 tests pass

