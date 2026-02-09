#!/usr/bin/env python3
"""Run all ACATS C-tests and generate real_failures.txt.

This script runs all ACATS C-tests through the full pipeline:
compile -> assemble -> link -> execute on cpmemu

Results are written incrementally to real_failures.txt with format:
TEST_NAME: STATUS [details]

STATUS can be:
- PASS: Test executed and reported "PASSED" or "NOT-APPLICABLE"
- FAIL: Test executed but reported "FAILED"
- COMPILE_ERROR: Failed during Ada->Z80 compilation
- ASSEMBLE_ERROR: Failed during um80 assembly
- LINK_ERROR: Failed during ul80 linking
- TIMEOUT: Execution exceeded 5 second timeout
- NO_RESULT: Executed but no PASSED/FAILED found in output
"""

import sys
import time
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

from tests.test_acats_execution import (
    get_acats_c_tests, compile_and_run_acats, have_tools
)


def classify_result(stage, success, output):
    """Classify test result into a status category."""
    if not success:
        if stage == "compile":
            return "COMPILE_ERROR", output[:200].replace('\n', ' ')
        elif stage == "assemble":
            return "ASSEMBLE_ERROR", output[:200].replace('\n', ' ')
        elif stage == "link":
            return "LINK_ERROR", output[:200].replace('\n', ' ')
        elif output == "TIMEOUT":
            return "TIMEOUT", ""
        else:
            return f"{stage.upper()}_ERROR", output[:200].replace('\n', ' ')

    # Execution succeeded - check output
    if "PASSED" in output:
        return "PASS", ""
    elif "NOT-APPLICABLE" in output:
        return "PASS", "NOT-APPLICABLE"
    elif "FAILED" in output:
        # Extract failure message
        lines = output.split('\n')
        for line in lines:
            if "FAILED" in line:
                return "FAIL", line.strip()[:150]
        return "FAIL", output[:150].replace('\n', ' ')
    else:
        return "NO_RESULT", output[:150].replace('\n', ' ')


def main():
    if not have_tools():
        print("ERROR: Required tools not available (um80, ul80, cpmemu, libada.lib)")
        sys.exit(1)

    tests = get_acats_c_tests()
    print(f"Running {len(tests)} ACATS C-tests...")

    output_file = Path(__file__).parent / "real_failures.txt"

    # Stats
    stats = {
        "PASS": 0,
        "FAIL": 0,
        "COMPILE_ERROR": 0,
        "ASSEMBLE_ERROR": 0,
        "LINK_ERROR": 0,
        "TIMEOUT": 0,
        "NO_RESULT": 0,
    }

    start_time = time.time()

    with open(output_file, 'w') as f:
        f.write("# ACATS C-Test Results\n")
        f.write(f"# Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"# Total tests: {len(tests)}\n")
        f.write("#\n")
        f.write("# Format: TEST_NAME: STATUS [details]\n")
        f.write("#\n\n")

        for i, test_file in enumerate(tests):
            name = test_file.stem

            # Run test
            stage, success, output = compile_and_run_acats([test_file])
            status, details = classify_result(stage, success, output)

            # Update stats
            if status in stats:
                stats[status] += 1
            else:
                stats["NO_RESULT"] += 1

            # Write result
            if details:
                f.write(f"{name}: {status} [{details}]\n")
            else:
                f.write(f"{name}: {status}\n")
            f.flush()

            # Progress
            if (i + 1) % 50 == 0:
                elapsed = time.time() - start_time
                rate = (i + 1) / elapsed
                remaining = (len(tests) - i - 1) / rate if rate > 0 else 0
                print(f"  {i+1}/{len(tests)} ({100*(i+1)//len(tests)}%) - "
                      f"PASS:{stats['PASS']} FAIL:{stats['FAIL']} "
                      f"SKIP:{stats['COMPILE_ERROR']+stats['LINK_ERROR']+stats['TIMEOUT']} "
                      f"- ETA: {remaining/60:.1f}min")

        # Write summary at end
        f.write("\n# Summary\n")
        for status, count in sorted(stats.items(), key=lambda x: -x[1]):
            f.write(f"# {status}: {count}\n")
        f.write(f"# Total: {len(tests)}\n")

    elapsed = time.time() - start_time
    print(f"\nDone in {elapsed/60:.1f} minutes")
    print(f"Results written to {output_file}")
    print("\nSummary:")
    for status, count in sorted(stats.items(), key=lambda x: -x[1]):
        print(f"  {status}: {count}")


if __name__ == "__main__":
    main()
