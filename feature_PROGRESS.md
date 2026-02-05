# Progress: feature

Started: Thu Feb  5 08:58:37 EST 2026

## Status

IN_PROGRESS

## Task List

Based on real_failures.txt, there are 7181 total ACATS semantic errors to fix:

- [ ] Fix "not found" errors (5705 errors) - missing package/entity resolution
- [ ] Fix "not a X" errors (531 errors) - incorrect type category checks
- [ ] Fix "type mismatch" errors (318 errors) - incompatible type operations
- [ ] Fix "expected X" errors (278 errors) - wrong types/arguments
- [ ] Fix "static" errors (170 errors) - non-static expressions in static contexts
- [ ] Fix "other" errors (128 errors) - misc issues like dereference, component access
- [ ] Fix "cannot convert" errors (49 errors) - invalid type conversions
- [x] Fix "already defined" errors (2 errors) - duplicate definitions

## Tasks Completed

- "already defined" errors (2 errors): Fixed by allowing task types and named number declarations to shadow enumeration literals

## Completed This Iteration

Fixed "already defined" errors (2 errors total):
- c34005g.ada: Named number 'N' conflicted with Character enumeration literal 'N'
- c87b41a.ada: Task type 'T' conflicted with Character enumeration literal 'T'

Root cause: When derived enumeration types (like `type NEW_CHAR is new Character`) are declared, all
their enumeration literals (including ASCII characters like 'N' and 'T') are added to the symbol table.
When subsequent declarations use those same names, the compiler incorrectly rejected them as duplicates.

Fix: Modified semantic.py to allow task type declarations and named number declarations to shadow
enumeration literals, which is legal in Ada. Added checks in:
- _analyze_task_type_decl() at line 2282-2286
- _analyze_number_decl() at line 1755-1762

## Notes

