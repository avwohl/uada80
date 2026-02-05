# Progress: feature

Started: Thu Feb  5 08:58:37 EST 2026

## Status

IN_PROGRESS

## Task List

Based on ACATS test suite results:
- Initial: 7,181 errors across 2,742 test groups
- Current: 1,614 errors (81.9% pass rate)

- [x] Fix "not found" errors (5705 errors) - missing package/entity resolution - COMPLETE
- [ ] Fix "type mismatch" errors (376 errors) - assignment/parameter/initialization mismatches
- [ ] Fix "static" errors (170 errors) - non-static expressions in static contexts
- [ ] Fix "not a generic" errors (123 errors) - incorrect generic instantiation attempts
- [ ] Fix "incompatible types" errors (122 errors) - arithmetic/operation type issues
- [ ] Fix "wrong argument count" errors (103 errors) - function call parameter issues
- [ ] Fix "package not found" errors (98 errors) - missing package specs/bodies
- [ ] Fix "other/unclassified" errors (622 errors) - misc issues requiring investigation
- [x] Fix "already defined" errors (2 errors) - duplicate definitions - COMPLETE

## Tasks Completed

- "already defined" errors (2 errors): Fixed by allowing task types and named number declarations to shadow enumeration literals

## Completed This Iteration

Task: Verified "not found" errors are complete and investigated remaining error categories.

Verification:
- Ran full ACATS test suite (2,742 test groups)
- All 5,705 "not found" errors are resolved
- Current state: 1,614 errors remaining (77.5% reduction from 7,181)
- Pass rate: 81.9% (2,247/2,742 test groups pass semantic analysis)

Investigation of remaining errors:
1. Type mismatch errors (376): Require fixes to:
   - Universal integer/modular type conversions in arithmetic
   - Access type to class-wide type assignments
   - Implicit type conversions for derived/tagged types

2. Static expression errors (170):
   - SYSTEM.FINE_DELTA not recognized as static when referenced
   - Named constants from packages losing static property

3. Wrong argument count (103):
   - Generic procedure instantiations losing parameter signatures
   - Example: Ada.Unchecked_Deallocation instantiation shows 0 params instead of 1

4. Not a generic (123):
   - Tests trying to instantiate non-generic packages/procedures

All remaining error categories require complex fixes to:
- Generic instantiation system
- Type compatibility and implicit conversions
- Static expression propagation across package boundaries

## Notes

Next priority: Fix "type mismatch" errors (376 total)
- Assignment type mismatches: 205
- Parameter type mismatches: 99
- Initialization type mismatches: 52
- Other type mismatches: 20

These are likely related to implicit type conversions that should be allowed in Ada,
particularly for derived types, access types, and class-wide types.

