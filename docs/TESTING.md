# Testing

## Running Tests

```bash
pytest tests/ -v -o addopts=""
```

## ACATS End-to-End Execution

The [ACATS 4.2](http://www.ada-auth.org/acats.html) test suite is included in [tests/acats/](../tests/acats/). Tests are compiled to Z80 assembly, assembled with um80, linked with ul80, and executed on [cpmemu](https://github.com/avwohl/cpmemu).

**579 ACATS tests pass end-to-end** (compile + assemble + link + execute on cpmemu):

```
$ pytest tests/test_acats_execution.py -o addopts=""
===== 257 failed, 579 passed, 624 skipped in 5197s =====
```

| Result | Count | Description |
|---|---:|---|
| Passed | 579 | Compiled, ran on cpmemu, output contains PASSED |
| Failed | 257 | Compiled and ran but produced wrong results (codegen bugs) |
| Skipped | 624 | Compile/link/timeout failures (multi-file deps, missing features) |

## ACATS Front-End

All 5,787 legal ACATS files pass parsing and semantic analysis:

```
$ pytest tests/test_acats.py -o addopts=""
======================= 5,787 passed in 248s =======================
```

## learn-ada-z80 Programs

The [learn-ada-z80](https://github.com/avwohl/learn-ada-z80) companion project has 99 example programs. **98 of 99 compile to Z80 assembly** (full pipeline: parse, semantic analysis, lowering, code generation). One program fails due to a codegen bug with negative array bounds.

## Execution Tests

End-to-end execution tests (compile + assemble + link + run on cpmemu) are in `tests/test_execution.py`:

```bash
pytest tests/test_execution.py -v -o addopts=""
```
