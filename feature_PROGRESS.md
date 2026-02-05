# Progress: feature

Started: Mon Feb  2 04:17:28 EST 2026

## Status

IN_PROGRESS

## Task List
- [x] Task 1: Run ACATS test suite parser tests (already passing - 4,725 files parse successfully)
- [x] Task 2: Make a list of failed tests (already exists in real_failures.txt - 11,107 semantic errors)
- [x] Task 3: Identify tests to skip (multitasking-related tests)
- [ ] Task 4: Fix compiler to pass non-skipped tests (focus on core language features)
- [ ] Task 5: Verify floating point support (native Z80 hardware support)
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

