#!/usr/bin/env python3
"""ACATS test runner for UADA80.

Actually compiles, assembles, links, and runs ACATS tests on Z80/CP/M.
"""

import argparse
import subprocess
import sys
import tempfile
from pathlib import Path
from dataclasses import dataclass
from typing import Optional

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from uada80.compiler import Compiler, OutputFormat

RUNTIME_PATH = Path(__file__).parent.parent / "runtime"
ADALIB_PATH = Path(__file__).parent.parent / "adalib"
ACATS_PATH = Path(__file__).parent.parent / "tests" / "acats" / "tests"

# Categories that require tasking (skip for now)
TASKING_CATEGORIES = {"c9"}


@dataclass
class TestResult:
    name: str
    status: str  # "passed", "failed", "error", "skipped"
    output: str
    error: Optional[str] = None


def load_report_package() -> str:
    """Load the REPORT package spec and body."""
    spec = (ADALIB_PATH / "report.ads").read_text()
    body = (ADALIB_PATH / "report.adb").read_text()
    return spec + "\n\n" + body


def run_acats_test(test_path: Path, timeout: float = 30.0) -> TestResult:
    """Compile and run a single ACATS test."""
    test_name = test_path.stem.upper()

    # Skip tasking tests for now
    category = test_path.parent.name.lower()
    if category in TASKING_CATEGORIES:
        return TestResult(test_name, "skipped", "", "Tasking test - requires interrupt support")

    try:
        # Read and combine sources
        test_source = test_path.read_text()
        report_package = load_report_package()
        combined = report_package + "\n\n" + test_source

        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Step 1: Compile to assembly
            compiler = Compiler(output_format=OutputFormat.ASM, optimize=True)
            result = compiler.compile(combined)

            if not result.success:
                return TestResult(
                    test_name, "error", "",
                    f"Compilation failed: {'; '.join(str(e) for e in result.errors[:5])}"
                )

            asm_file = tmpdir / "test.asm"
            rel_file = tmpdir / "test.rel"
            com_file = tmpdir / "test.com"

            asm_file.write_text(result.output)

            # Step 2: Assemble
            proc = subprocess.run(
                ["um80", "-o", str(rel_file), str(asm_file)],
                capture_output=True, text=True, timeout=30
            )
            if proc.returncode != 0:
                return TestResult(
                    test_name, "error", proc.stdout,
                    f"Assembly failed: {proc.stderr}"
                )

            # Step 3: Link
            libada = RUNTIME_PATH / "libada.lib"
            proc = subprocess.run(
                ["ul80", "-o", str(com_file), str(rel_file), str(libada)],
                capture_output=True, text=True, timeout=30
            )
            if proc.returncode != 0:
                return TestResult(
                    test_name, "error", proc.stdout,
                    f"Linking failed: {proc.stderr}"
                )

            # Step 4: Run on emulator
            proc = subprocess.run(
                ["cpmemu", "--z80", str(com_file)],
                capture_output=True, text=True, timeout=timeout
            )

            output = proc.stdout

            # Check result
            if "==== PASSED ====" in output:
                return TestResult(test_name, "passed", output)
            elif "**** FAILED ****" in output:
                return TestResult(test_name, "failed", output)
            elif "++++ NOT" in output:  # NOT-APPLICABLE or NOT APPLICABLE
                return TestResult(test_name, "skipped", output, "Not applicable")
            else:
                return TestResult(
                    test_name, "error", output,
                    "No PASSED/FAILED result found in output"
                )

    except subprocess.TimeoutExpired:
        return TestResult(test_name, "error", "", f"Timeout after {timeout}s")
    except Exception as e:
        return TestResult(test_name, "error", "", str(e))


def find_tests(categories: list[str] = None, pattern: str = None) -> list[Path]:
    """Find ACATS test files."""
    tests = []

    if categories:
        for cat in categories:
            cat_path = ACATS_PATH / cat.lower()
            if cat_path.exists():
                tests.extend(cat_path.glob("*.ada"))
    else:
        tests = list(ACATS_PATH.glob("*/*.ada"))

    if pattern:
        tests = [t for t in tests if pattern.lower() in t.name.lower()]

    return sorted(tests)


def main():
    parser = argparse.ArgumentParser(description="Run ACATS tests")
    parser.add_argument("--category", "-c", action="append",
                       help="Test category (e.g., c2, c3). Can be repeated.")
    parser.add_argument("--pattern", "-p", help="Filter by filename pattern")
    parser.add_argument("--timeout", "-t", type=float, default=30.0,
                       help="Timeout per test in seconds")
    parser.add_argument("--verbose", "-v", action="store_true",
                       help="Show detailed output")
    parser.add_argument("--stop-on-fail", "-x", action="store_true",
                       help="Stop on first failure")
    args = parser.parse_args()

    tests = find_tests(args.category, args.pattern)
    print(f"Found {len(tests)} tests")

    results = {"passed": 0, "failed": 0, "error": 0, "skipped": 0}
    failed_tests = []
    error_tests = []

    for i, test_path in enumerate(tests, 1):
        result = run_acats_test(test_path, args.timeout)
        results[result.status] += 1

        status_symbol = {
            "passed": ".",
            "failed": "F",
            "error": "E",
            "skipped": "S"
        }[result.status]

        if args.verbose or result.status in ("failed", "error"):
            print(f"\n[{i}/{len(tests)}] {result.name}: {result.status.upper()}")
            if result.error:
                print(f"  Error: {result.error}")
            if args.verbose and result.output:
                for line in result.output.strip().split('\n')[:10]:
                    print(f"  {line}")
        else:
            print(status_symbol, end="", flush=True)

        if result.status == "failed":
            failed_tests.append(result)
            if args.stop_on_fail:
                break
        elif result.status == "error":
            error_tests.append(result)
            if args.stop_on_fail:
                break

    print("\n")
    print("=" * 60)
    print(f"Results: {results['passed']} passed, {results['failed']} failed, "
          f"{results['error']} errors, {results['skipped']} skipped")
    print(f"Total: {len(tests)} tests")

    if failed_tests:
        print("\nFailed tests:")
        for r in failed_tests[:20]:
            print(f"  {r.name}")

    if error_tests:
        print("\nError tests:")
        for r in error_tests[:20]:
            print(f"  {r.name}: {r.error}")

    return 0 if results["failed"] == 0 and results["error"] == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
