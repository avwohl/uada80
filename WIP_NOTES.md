# Work in Progress Notes - 2024-12-12

## Session 2 Summary (continued)

### Completed Fixes This Session

1. **Type conversion for subtypes** (`semantic.py:3106`)
   - Problem: Type conversions like `Subtype(Expr)` failed with "'Integer' is not an array"
   - Fix: Changed check from `symbol.kind == SymbolKind.TYPE` to `symbol.kind in (SymbolKind.TYPE, SymbolKind.SUBTYPE)`
   - Now `STAT(J)` works correctly when `STAT` is a subtype

2. **Task type incomplete completion** (`semantic.py:1406-1458`)
   - Problem: `TYPE T1; TASK TYPE T1 IS...` failed with "task type 'T1' is already defined"
   - Fix: Check if existing symbol is an incomplete type before erroring
   - Update existing symbol instead of creating new one when completing

3. **Protected type incomplete completion** (`semantic.py:1547-1626`)
   - Problem: Same issue as task types
   - Fix: Same pattern as task type fix

---

## Session 1 Summary (previous)

### Completed Fixes

1. **Fixed Ada.Unchecked_Deallocation generic instantiation** (`semantic.py:689-710`)
   - Problem: `_analyze_generic_subprogram_instantiation` didn't handle `SelectedName` for qualified generic names like `Ada.Unchecked_Deallocation`
   - Fix: Added `elif isinstance(inst.generic_name, SelectedName)` branch with `lookup_selected()` call
   - This mirrors the fix already in `_analyze_generic_instantiation` (line 584-594)

2. **Registered Unchecked_* generics at root level** (`symbol_table.py:1073-1089`)
   - Problem: Ada allows `WITH UNCHECKED_DEALLOCATION;` directly (not just `WITH Ada.Unchecked_Deallocation;`)
   - The symbol was only registered under the `Ada` package, not at the root scope
   - Fix: Added both `Unchecked_Conversion` and `Unchecked_Deallocation` to the root scope in `_init_unchecked_ops()`
   - Verified: `ada101a.ada` went from 17 errors (14 UNCHECKED_DEALLOCATION errors) to 3 errors (0 UNCHECKED_DEALLOCATION)

3. **Static expression evaluation for constants** (`semantic.py:3274-3393`)
   - Problem: `_eval_static_expr` didn't handle Identifier lookups for constants
   - Fix: Added `_try_eval_static` and `_eval_static_impl` with support for:
     - Identifier lookup (uses `symbol.value` for constants)
     - RealLiteral, TypeConversion, QualifiedExpr
     - More AttributeReference types (Size, Length, Pos, Val)
     - MOD and REM binary operators
   - Constants now store their static value in `symbol.value`

4. **Generic formal type kind** (`semantic.py:541-565`)
   - Problem: All generic formal types got `TypeKind.PRIVATE`, even `type T is range <>`
   - Fix: Check `formal.constraint` to assign correct type kind:
     - `range` -> `TypeKind.INTEGER`
     - `mod` -> `TypeKind.MODULAR`
     - `digits` -> `TypeKind.FLOAT`
     - `discrete` -> `TypeKind.ENUMERATION`
   - This allows `Universal_Integer` literals to work with generic integer types

5. **Generic default parameters** (`semantic.py:640-655, 759-774`)
   - Problem: Generic instantiation required exact parameter count, ignoring defaults
   - Fix: Count formals with `default_value`, allow `min_required` to `num_formals` actuals

6. **Record discriminant visibility** (`semantic.py:1013-1035, 1049-1065`)
   - Problem: Discriminants weren't added to RecordType when processing TypeDecl
   - Fix: After `_build_type`, add discriminants from `decl.discriminants` to `ada_type.discriminants`

### Current Status

- All 2742 ACATS semantic tests pass
- Key semantic fixes applied:
  - Subtype type conversions work
  - Incomplete type completions (task/protected) work
  - Generic default parameters work
  - Deferred constants work
  - Static expression evaluation improved
- Remaining ACATS issues are primarily:
  - Parse errors (block labels, Ada 2005/2012 features)
  - Inter-file dependencies (GNAT libraries, REPORT package)

### Previous Session Fixes (already committed)

1. Universal_Real/Float arithmetic compatibility in `common_type()`
2. 'First/'Last attributes returning correct type for Float types
3. `types_compatible()` checking subtype relationships
4. Universal types recognized as numeric in `is_numeric()`

### Error Counts Before This Session

From `real_failures.txt`:
- 8787 "not found" errors
- 1004 "not a X" errors
- 432 "static" errors
- 283 "other" errors
- 208 "expected X" errors
- 188 "type mismatch" errors (reduced to ~158)
- 159 "incompatible" errors (reduced to ~49)
- 42 "already defined" errors
- 4 "cannot convert" errors

### Next Steps

1. Run ACATS semantic tests to verify improvement from generic fix:
   ```bash
   .venv/bin/pytest tests/test_acats.py::TestACATSSemanticGrouped -q --tb=no
   ```

2. Regenerate `real_failures.txt` to see new error counts

3. Continue with remaining error categories:
   - "static" expression errors (432)
   - "not found" errors (inter-file dependencies)

### Key Files Modified

- `uada80/semantic.py` - Multiple fixes:
  - `_analyze_generic_subprogram_instantiation` for SelectedName handling
  - `_eval_static_impl` for constant lookup and more expression types
  - `_analyze_generic_formal` for correct type kinds
  - Generic instantiation parameter count with defaults
  - `_analyze_type_decl` for discriminant visibility
- `uada80/symbol_table.py` - Root-level registration of Unchecked_* generics
