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

Partial fix for "not found" errors - Hierarchical package lookup:
- Fixed STANDARD.ASCII access (use STANDARD.ASCII; now works)
- Fixed hierarchical package resolution in use clauses (use P.Q.R;)
- Fixed hierarchical package resolution in use type clauses (use type P.Q.T;)

Root cause: The _analyze_use_clause() function was using simple lookup() for package names,
which doesn't handle hierarchical names like "STANDARD.ASCII". It should use
_resolve_hierarchical_package() which walks the package hierarchy.

Changes made in semantic.py:
1. Line 242: Added ASCII to STANDARD.public_symbols so STANDARD.ASCII is accessible
2. Line 745: Changed use type clause to use _resolve_hierarchical_package() for prefix
3. Line 780: Changed use clause to use _resolve_hierarchical_package() instead of simple lookup()

This fixes hierarchical package access in use clauses, though many "not found" errors remain
due to other issues (child packages, external package loading, etc.)

## Notes

Root cause analysis of "not found" errors (5705 total):
1. STANDARD.ASCII not accessible - FIXED in this iteration
2. Child package parent not available (~1200 errors) - needs parent body analysis first
3. Parent package symbols not visible in children (~2500 errors) - visibility issue
4. Task entry names in SELECT statements (~700 errors) - scope issue
5. Package specifications not found (~500 errors) - search path issue

Next priority: Fix child/parent package visibility (fixes ~3700 errors)

