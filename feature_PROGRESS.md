# Progress: feature

Started: Mon Feb  9 16:08:03 EST 2026

## Status

IN_PROGRESS

## Task List

Based on analysis of real_failures.txt (579 PASS, 290 LINK_ERROR, 246 NO_RESULT, 212 COMPILE_ERROR, 125 TIMEOUT, 1 FAIL):

### Semantic Analysis Fixes (High Impact)
- [x] Task 1: Fix _load_external_package() to handle GenericSubprogramUnit (LENGTH_CHECK/ENUM_CHECK) - affects 26+ tests
- [x] Task 2: Fix package body visibility - auto-load spec, pass source dirs as search paths - affects 7 tests
- [x] Task 3: Fix generic body discovery - body stubs no longer overwrite generic specs

### Type System Fixes
- [x] Task 4: Fix "expected Boolean, got 'Universal_Integer'" - function calls in IndexedComponent now resolved with overloading (5 tests)
- [x] Task 5: Fix Duration/Time arithmetic type checking - fixed ada-calendar.ads operator signatures (7 tests)
- [x] Task 6: Fix string type assignment checking - slice returns base type, String subtype compatibility, element concatenation (6+ tests)

### Codegen Fixes
- [x] Task 7: Fix lowering "NoneType" errors - added None checks for dynamic array bounds (5+ tests)
- [x] Task 8: Fix codegen IRType comparison errors - fixed VReg constructor argument order (2+ tests)

### Link Error Fixes (Symbol Generation)
- [x] Task 9: Enhanced task entry call detection - broader fallback for task_id lookup
- [x] Task 10: Fixed _container_first/_container_last name mismatch (8-char limit)
- [ ] Task 10b: Package-qualified implicit operators (_OP_EQ, etc.) still generate calls instead of inline code
- [ ] Task 10c: Generic instantiation bodies not always generated (_PROC*, _NEWPROC, etc.)

## Tasks Completed

### Iteration 1 (2026-02-09)
- Task 1: Fixed _load_external_package() to handle GenericSubprogramUnit
  - Added code to find and load generic procedures/functions from external files
  - Extended _find_package_file() to search for ACATS-style file naming (e.g., lencheck.ada)
  - Properly extract parameters and return type from generic subprogram declarations
  - Tests cd2a21a, cd2a21e, cd2a24a, cd3014c now compile without errors
  - All 7155 tests pass

### Iteration 2 (2026-02-09)
- Task 2: Fixed package body auto-loading and search paths
  - `_analyze_package_body` now calls `_load_external_package` when spec not found
  - `compile()` and `compile_files()` pass source directories as search paths
  - Fixes c38108c2, c83f01c1, c83f03c1, ca1102a1

- Task 3: Fixed generic body discovery for separate compilation units
  - `_analyze_body_stub` no longer overwrites existing GENERIC_PACKAGE symbols
  - Same fix applied to procedure/function stubs
  - Fixes ca2009a, ca2009d, and similar "not a generic" errors

- Task 4: Fixed function call resolution in IndexedComponent
  - Added function call detection with proper overload resolution
  - `IDENT_INT(10)` now correctly resolved as function call returning Integer
  - Fixes c67002a-e "expected Boolean, got Universal_Integer" (partial - full fix needs context-dependent overload resolution)

- Task 5: Fixed Duration/Time arithmetic in ada-calendar.ads
  - Changed operator signatures from Integer to Duration
  - Added missing "+"(Duration, Time) operator
  - Fixes c96005a/d, c96006a, c97301b-e

- Task 6: Fixed string type assignment checking
  - `_analyze_slice` now returns unconstrained base type (follows base_type chain)
  - `types_compatible` handles String subtypes (String(1..5) vs String(5..9))
  - Element concatenation creates anonymous array types instead of defaulting to String
  - Fixes c52103c/h, c45347a-d

- Task 7: Fixed lowering NoneType errors for dynamic array bounds
  - Added None checks at lowering.py lines 1432 and 11238
  - Guarded ArrayType creation with `not has_dynamic_bounds` check
  - Fixes c43204e/i, c83e02a, c87b23a, cda201a/b

- Task 8: Fixed codegen IRType comparison errors
  - Fixed VReg constructor at line 16367: `VReg("hl", IRType.WORD)` → `self.builder.new_vreg(IRType.WORD, "hl")`
  - Fixed VReg constructor at line 2773: `VReg(0, IRType.PTR, "SP")` → `self.builder.new_vreg(IRType.PTR, "SP")`
  - Fixes c45611a, c49024a

- Task 9: Enhanced task entry call detection
  - Broadened fallback: try `_lower_expr(prefix)` when task_id not in ctx.task_objects or ctx.locals

- Task 10: Fixed container symbol name mismatch
  - `_container_first` → `_cont_first`, `_container_last` → `_cont_last` (8-char limit)

- All 7155 tests pass, no regressions

