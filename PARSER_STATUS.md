# Parser Status Notes

## ACATS Test Results (as of 2025-12-08)

**Results: 2711 passed, 39 failed, 99 timeouts** (out of 2849 files)
**Pass rate: ~95.2%**

### Recent Changes
- Added SEPARATE subunit support (`parse_subunit()`, `parse_task_body_impl()`, `parse_protected_body_impl()`)
- File `aa2010a.ada` (previously timing out) now parses successfully

### Failure Categories

#### 1. Lexer Errors (2 files)
- `a2a031a.ada`: Unexpected character `!`
- `c2a021b.ada`: Unexpected character `%` (Ada 83 alternate delimiters)

#### 2. Number Parsing Errors (8 files)
Issues with based literals:
- `c24002d.ada`: `12e1`
- `c24003a.ada`: Large exponent
- `c24106a.ada`: `0E10`
- `c46032a.ada`: `#` character
- `cc1221b.ada`: `0E8`
- `cc3012a.ada`: `1E3`

#### 3. Missing `parse_formal_parameters` (26 files)
Files using generic formal parameters - **largest category**:
- `c392010.a`, `c392d03.a`
- `c3a0001.a` through `c3a0029.a`
- `c3a2004.a`, `c410001.a`, `c640001.a`
- `c851001.a`, `cb20003.a`
- `cc54003.a`, `cc70003.a`
- `cxc7006.a`, `cxe4002.a`, `cxe4004.a`
- `cxf2a01.a`, `cxf2a02.a`
- `cxh3001.a`, `cxh3002.a`
- `lxh40092.a`

#### 4. Timeouts (99 files)
Files taking >30 seconds to parse, often due to:
- Parser getting stuck on unsupported constructs
- Complex nested structures
- SEPARATE subunits (some should now work after implementation)

### Priority Fixes
1. **Implement `parse_formal_parameters`** - Would fix 26 failures (67% of failures)
2. **Fix number literal parsing** - Would fix 8 failures
3. **Support Ada 83 alternate delimiters** - Would fix 2 failures
4. **Investigate remaining timeouts** - May have reduced after SEPARATE fix

### Test Files
- Unit tests: `tests/test_parser.py` (118 tests)
- ACATS tests: `tests/test_acats.py`
- Quick ACATS runner: `/tmp/acats_test.py`
