# ACATS Remaining Issues (Non-GNAT)

Based on analysis of `real_failures.txt` - 11,107 total errors.

## Error Categories

| Category | Count | Description |
|----------|-------|-------------|
| not found | 8787 | Inter-file dependencies, missing symbols |
| not a X | 1004 | Type/kind mismatches (some fixed) |
| static | 432 | Static expression not recognized |
| other | 283 | Various (records, arrays, etc.) |
| expected X | 208 | Wrong argument/parameter counts |
| type mismatch | 188 | Type compatibility issues |
| incompatible | 159 | Arithmetic type mismatches |
| already defined | 42 | Scope/completion issues |
| cannot convert | 4 | Array conversion issues |

## Priority Fixes

### 1. Static Expression Evaluation (432 errors)
Expressions that should be recognized as static aren't:
- Attribute references (`'First`, `'Last`, `'Size`, `'Length`)
- Constants declared with static expressions
- Numeric type attributes

Sample errors:
```
a35402a.ada: error: expression is not static
ad7101a.ada: error: expression is not static
```

### 2. Record/Discriminant Handling (~100+ errors)
- `record 'X' has no component 'Y'` - discriminant-dependent components
- Variant record support incomplete
- Derived type component inheritance

Sample errors:
```
c32001c.ada: error: record 'REC' has no component 'D1'
c37305a.ada: error: record 'REC' has no component 'DISC'
```

### 3. Generic Instantiation (~200+ errors)
- Default generic parameters not handled
- Generic formal type matching issues
- Nested generic visibility

Sample errors:
```
c39006d.ada: error: wrong number of generic parameters for 'P': expected 1, got 0
c3a0020.a: error: wrong number of generic parameters for 'GPack': expected 1, got 0
```

### 4. Type Compatibility (~200 errors)
- Integer literals with derived integer types
- `Universal_Real` / `Float` remaining issues
- Array type conversions

Sample errors:
```
a49027b.ada: error: type mismatch: expected 'NUMBER_TYPE', got 'Universal_Integer'
c3a0006.a: error: incompatible types for arithmetic: 'Float' and 'Universal_Real'
```

### 5. Already Defined (42 errors)
- Task type completions
- Deferred constants
- Overloading resolution

Sample errors:
```
c38102d.ada: error: task type 'T1' is already defined
c452006.a: error: 'Null_Lim_Handle' is already defined in this scope
```

## Files to Investigate

- `uada80/semantic.py` - Main semantic analysis
- `uada80/type_system.py` - Type compatibility, static evaluation
- `uada80/symbol_table.py` - Symbol resolution, scoping
