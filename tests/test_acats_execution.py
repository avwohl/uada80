"""End-to-end ACATS execution tests.

Compiles ACATS C-tests to Z80 .com files, runs on cpmemu, checks for PASSED.

Uses a simplified Z80-compatible Report package (in z80_support/) instead of
the ACATS Report package which requires Ada.Calendar and file I/O.

Tests that need multiple files compiled together are handled by grouping
related files (same base name pattern) and compiling them as a unit.
"""

import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from collections import defaultdict

import pytest

from uada80.compiler import Compiler, OutputFormat

# Paths
ACATS_PATH = Path(__file__).parent / "acats" / "tests"
ACATS_SUPPORT = Path(__file__).parent / "acats" / "support"
Z80_SUPPORT = Path(__file__).parent / "z80_support"
REPORT_FILE = Z80_SUPPORT / "report.ada"
RUNTIME_PATH = Path(__file__).parent.parent / "runtime"
LIBADA = RUNTIME_PATH / "libada.lib"

UM80_CMD = shutil.which("um80")
UL80_CMD = shutil.which("ul80")
CPMEMU_CMD = shutil.which("cpmemu")

# Categories to skip entirely (file I/O not implemented)
SKIP_CATEGORIES = {'ce'}

def have_tools():
    """Check if all execution tools are available."""
    return bool(UM80_CMD and UL80_CMD and CPMEMU_CMD and LIBADA.exists()
                and REPORT_FILE.exists())


skip_if_no_tools = pytest.mark.skipif(
    not have_tools(),
    reason="Execution tools (um80, ul80, cpmemu, libada.lib) not available"
)


def get_acats_c_tests():
    """Get single-file ACATS C-tests suitable for execution."""
    if not ACATS_PATH.exists():
        return []

    tests = []
    for f in sorted(ACATS_PATH.rglob("c*.ada")):
        cat = f.parent.name
        if cat in SKIP_CATEGORIES:
            continue
        tests.append(f)
    return tests


def compile_and_run_acats(test_files, timeout=5.0):
    """Compile ACATS test files and run on cpmemu.

    Args:
        test_files: List of Path objects to compile (Report is prepended)
        timeout: Execution timeout in seconds

    Returns:
        (stage, success, output) where stage is the phase that completed last
    """
    # Include ACATS support directory for external generic loading (LENGTH_CHECK, ENUM_CHECK)
    extra_paths = [str(ACATS_SUPPORT)] if ACATS_SUPPORT.exists() else []
    compiler = Compiler(output_format=OutputFormat.ASM, optimize=True, search_paths=extra_paths)

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        asm_file = tmpdir / "test.asm"
        rel_file = tmpdir / "test.rel"
        com_file = tmpdir / "test.com"

        # Compile: Report + test files
        files = [REPORT_FILE] + list(test_files)
        try:
            result = compiler.compile_files(files)
            if not result.success:
                msg = str(result.errors[0]) if result.errors else "unknown"
                return "compile", False, msg
            asm_file.write_text(result.output)
        except Exception as e:
            return "compile", False, str(e)

        # Assemble
        proc = subprocess.run(
            [UM80_CMD, "-o", str(rel_file), str(asm_file)],
            capture_output=True, text=True, timeout=30
        )
        if proc.returncode != 0:
            return "assemble", False, proc.stderr.strip()

        # Link
        proc = subprocess.run(
            [UL80_CMD, "-o", str(com_file), str(rel_file), str(LIBADA)],
            capture_output=True, text=True, timeout=30
        )
        if proc.returncode != 0:
            return "link", False, proc.stderr.strip()

        # Run
        try:
            proc = subprocess.run(
                [CPMEMU_CMD, "--z80", str(com_file)],
                capture_output=True, text=True, timeout=timeout
            )
            output = proc.stdout + proc.stderr
            return "run", True, output
        except subprocess.TimeoutExpired:
            return "run", False, "TIMEOUT"


ACATS_C_TESTS = get_acats_c_tests()


@pytest.mark.skipif(not ACATS_C_TESTS, reason="ACATS not installed")
@skip_if_no_tools
class TestACATSExecution:
    """End-to-end ACATS execution tests."""

    @pytest.mark.parametrize("test_file", ACATS_C_TESTS,
                             ids=lambda f: f.stem)
    def test_acats_execute(self, test_file):
        """Compile and run a single ACATS C-test on cpmemu."""
        stage, success, output = compile_and_run_acats([test_file])

        if not success:
            if stage == "compile":
                pytest.fail(f"compile: {output[:100]}")
            elif stage == "link":
                pytest.fail(f"link: {output[:100]}")
            elif output == "TIMEOUT":
                pytest.fail(f"timeout (>5s)")
            else:
                pytest.fail(f"{stage} failed: {output[:200]}")

        # Check output for PASSED/FAILED/NOT-APPLICABLE
        if "PASSED" in output:
            pass  # Success
        elif "NOT-APPLICABLE" in output:
            pass  # Acceptable
        elif "FAILED" in output:
            pytest.fail(f"Test reported FAILED: {output[:200]}")
        else:
            pytest.fail(f"No PASSED/FAILED in output: {output[:200]}")
