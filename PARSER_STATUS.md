# Parser Status Notes

## ACATS Test Results (as of 2025-12-09)

**Results: ~2800+ passed** (out of 2849 files)
**Pass rate: ~98%+** (pending full suite run)

### Recent Changes (2025-12-09)

#### Bug Fixes
1. **Fixed based literal exponent parsing** - Exponents in based literals (e.g., `16#1E.5#E3`) are now correctly parsed by finding the exponent only after the closing `#`

2. **Added generic formal type constraints** - Full support for:
   - `type T is (<>)` - discrete type constraint
   - `type T is range <>` - signed integer type
   - `type T is mod <>` - modular type
   - `type T is digits <>` - floating point type
   - `type T is delta <>` - fixed point type
   - `type T is delta <> digits <>` - decimal fixed point
   - `type T is new Parent [with private]` - derived type
   - Discriminant parts on generic formal types

3. **Added anonymous access return types** - Functions can now return `access Type`:
   - `function F return access Integer;`
   - `not null access Type`
   - `access constant Type`

4. **Fixed keyword attribute parsing** - Attributes like `'Access`, `'Range`, `'Delta`, `'Digits`, `'Mod` are now correctly parsed (these are keywords that can also be attribute names)

5. **Added aliased array component types** - `array (Index) of aliased Type` now parses correctly

6. **Fixed task/protected declaration routing** - Corrected method name references for top-level task/protected declarations

7. **Fixed SELECT statement parsing for selective accept** - Proper handling of:
   - `SELECT ACCEPT ... OR ACCEPT ... END SELECT;`
   - `OR DELAY ...` alternatives
   - `TERMINATE;` alternative
   - `ELSE` clause for conditional entry call
   - `THEN ABORT` for asynchronous select

8. **Added operator names in selected components** - `Package."="` now parses correctly for qualified operator calls

9. **Added operator names in generic instantiation** - `"=" => "="` now works as named association for operator parameters

### Previously Fixed
- SEPARATE subunit support (`parse_subunit()`, `parse_task_body_impl()`, `parse_protected_body_impl()`)
- Ada 83 alternate delimiters (`!` for `|`, `%` for string delimiters)

### Remaining Known Issues

#### Timeouts (estimated ~40-50 files)
Some files with complex nested structures or edge cases still timeout:
- Files with very deep nesting
- Some complex predicate expressions
- Edge cases in expression parsing

### Test Files
- Unit tests: `tests/test_parser.py` (118 tests) - All passing
- Lexer tests: `tests/test_lexer.py` (24 tests) - All passing
- ACATS tests: `tests/test_acats.py` (2849 tests)
- Total unit tests: 142 passing

### Parser Coverage
- 65%+ code coverage in parser unit tests
- Comprehensive support for Ada 2012 constructs
- Basic Ada 2022 support (parallel blocks, declare expressions, etc.)
