# UADA80 - Remaining Work for Full Ada/ACATS

## Current Status (2024-12)

| Phase | Pass Rate | Notes |
|-------|-----------|-------|
| Parsing | 2849/2849 (100%) | Full Ada syntax supported |
| Semantic | 1031/2849 (36.2%) | ~1818 failures |
| Lowering | 1008/2849 (35.4%) | ~8 failures after semantic pass |
| Unit Tests | 4033/4033 (100%) | All pass |

## Recent Fixes (2024-12-12)

1. [x] Universal_Real to Float type compatibility
2. [x] Type conversion Float(5) was incorrectly parsed as array access
3. [x] Case expression selector now correctly handles UNIVERSAL_INTEGER
4. [x] Enumeration literal overloading - same literal in different enum types
5. [x] Overload resolution with expected type context (initialization/assignment)
6. [x] Child package visibility - parent's declarations now visible
7. [x] Universal_Integer and Universal_Real now recognized as numeric types (fixes unary minus)

## Issues to Fix (Non-GNAT)

### High Priority - Type System Edge Cases

These are actual bugs in the semantic analyzer that can be fixed without GNAT:

1. [x] Type checking edge cases (Universal_Real, type conversion)
2. [x] Overload resolution issues (enum literal overloading)
3. [ ] Generic instantiation type matching
4. [ ] Discriminant handling
5. [ ] Array/record constraint checking

### Medium Priority - Missing Standard Package Members

Some Standard package items may be missing:

1. [ ] Wide_Character / Wide_String types
2. [ ] Wide_Wide_Character / Wide_Wide_String types
3. [ ] Additional predefined exceptions
4. [ ] Address type and related operations

### Lower Priority - Multi-file Compilation

Multi-file compilation is fully supported (pass multiple files on command line).

1. [x] Basic multi-file compilation works
2. [x] Inter-file dependencies work when files are compiled together
3. [x] ACATS test runner groups related files automatically
4. [ ] ACATS support library (Report.a) requires Ada.Text_IO, Ada.Calendar (GNAT)

**Current ACATS semantic results (excluding GNAT-dependency errors):**
- Effective pass rate: ~48% (would pass if GNAT libs available)
- Real failures: ~52% (actual semantic bugs or missing features)

## Requires GNAT Standard Library

These need the full GNAT runtime and cannot be fixed without it:

- Ada.Text_IO and child packages
- Ada.Strings and child packages
- Ada.Containers and child packages
- Ada.Calendar
- Ada.Numerics
- System package details
- Interfaces package

## ACATS Support Library

ACATS tests use these support packages that need to be provided:

- Report (test reporting)
- Impdef (implementation defined values)
- Spprt13 (support for chapter 13 tests)
- Various FCxx packages (foundation code)

## Investigation Needed

Run this to see current error breakdown:
```bash
python -c "
from pathlib import Path
from uada80.parser import parse
from uada80.semantic import SemanticAnalyzer
...
"
```
