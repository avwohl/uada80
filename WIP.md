# WIP State — 2026-04-11

## Standing Directive
"Go on fix this compiler to pass all ACATS, even the tasking"

## Recent Fixes (uncommitted)

### 1. Generic formal modular type resolution in `_lower_binary` (~line 19938 in lowering.py)
- When `_get_expr_type(expr)` doesn't return ModularType, checks `_generic_actual_types` dict
- Walks base_type/parent_type chain to find the actual ModularType
- Enables AND masking for modular arithmetic inside generic bodies

### 2. Generic actual type resolution in `_resolve_local_type` (~line 22065 in lowering.py)
- Added `_generic_actual_types` check BEFORE symbol table lookup when resolving Identifier type nodes
- Root cause: Inside generic bodies, `Mod_Type` is a formal — symbol table doesn't have it
- Fix checks `_generic_actual_types` first, finding the concrete ModularType

### 3. Modular masking on assignment in `_lower_assignment` (~line 7811 in lowering.py)
- Added modular masking BEFORE range check for modular target types
- Skips range check for modular types (already masked)
- Fixes `V := 0 - 1` where V is modular — literal operands have IntegerType, no masking in `_lower_binary`
- Power-of-2: AND mask; non-power-of-2: MOD instruction

## Test Results After Fixes
- 174/174 regression tests pass
- Full ACATS run was in progress (PID 66799) — check /tmp/acats_full3.txt
- Another ACATS run for c8-cd tests (PID 67454) — check /tmp/acats_c89abcd.txt

## Next Priority: Function call resolution inside generic bodies

### The Bug
In generic body instantiations, calls to imported package functions (e.g., `Report.Equal(4, 12)`) 
are NOT generating `call _equal` instructions. Instead, the generated code reads uninitialized 
stack memory at `(ix-2)/(ix-1)`.

### Evidence
In compiled c453001, the `_test_byte_ident_mod` function (line 623-710 of assembly output):
- Function body starts with `ld l, (ix-2); ld h, (ix-1)` 
- Should be result of `Report.Equal(4, 12)` but there's NO `call _equal`
- Condition reads garbage from uninitialized stack
- Causes `Ident_Mod` to always return 1 instead of the parameter value

### Impact
- c453001 (modular arithmetic wrapping) fails — AND masking now works but ident_mod returns wrong values
- Likely affects many other generic-heavy ACATS tests

### Where to Look
- `lowering.py` — how function calls inside generic bodies are resolved
- The issue is that `Report.Equal` (a dotted function call from a WITH'd package) isn't being 
  resolved to a callable inside the generic body instantiation context
- Check how `_lower_call` or `_lower_function_call` handles dotted names when inside a generic

## ACATS Failure Categories (from analysis)

| Category | Count | Notes |
|----------|-------|-------|
| Link errors (missing symbols) | 344 | _IS_OPEN, _ADD, _DELETE etc |
| Timeouts | 354 | Large programs or infinite loops |
| "'X' is not an array" | 54 | Type conversion vs array indexing in semantic.py |
| CONSTRAINT_ERROR NOT RAISED | many | Dynamic bounds not supported |
| "expression is not static" | 14 | Static expression evaluator too limited |
| String bounds ('First/'Last/'Length) | varies | Deep type system issue |

## Files Modified (all uncommitted)
- uada80/lowering.py (3 edits above)
- uada80/semantic.py
- uada80/type_system.py
- uada80/codegen/__init__.py
- uada80/ir.py
- uada80/ast_nodes.py
- uada80/runtime_manager.py
- runtime/runtime.mac, runtime/float64.mac, runtime/libada.lib, runtime/runtime.rel
- adalib/ada-integer_text_io.adb
- tests/test_execution.py, tests/test_acats_execution.py
- CLAUDE.md
