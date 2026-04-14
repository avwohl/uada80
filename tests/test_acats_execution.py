"""End-to-end ACATS execution tests.

Compiles ACATS C-tests to Z80 .com files, runs on cpmemu, checks for PASSED.

Uses a simplified Z80-compatible Report package (in z80_support/) instead of
the ACATS Report package which requires Ada.Calendar and file I/O.

Tests that need multiple files compiled together are handled by grouping
related files (same base name pattern) and compiling them as a unit.
"""

import multiprocessing
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

# Build index of support .a files: unit_name -> Path
_SUPPORT_INDEX = None

def _get_support_index():
    """Build/return index mapping lowercase unit names to support file paths."""
    global _SUPPORT_INDEX
    if _SUPPORT_INDEX is not None:
        return _SUPPORT_INDEX
    _SUPPORT_INDEX = {}
    if not ACATS_SUPPORT.exists():
        return _SUPPORT_INDEX
    for f in ACATS_SUPPORT.iterdir():
        if f.suffix in ('.a', '.ada', '.ads'):
            _SUPPORT_INDEX[f.stem.lower()] = f
    return _SUPPORT_INDEX


def resolve_support_files(test_files):
    """Scan test files for with-clauses and resolve to support .a files.

    Returns list of support file Paths to prepend to compilation, in
    dependency order (support files' own dependencies resolved first).
    """
    index = _get_support_index()
    if not index:
        return []

    def _find_support(unit_name):
        """Find support file for a unit, with fallback for multi-unit files.

        E.g., f341a00_0 -> look for f341a00_0.a first, then f341a00.a
        """
        if unit_name in index:
            return unit_name
        # Try stripping trailing _N suffix (multi-unit files like f341a00.a
        # contain F341A00_0, F341A00_1, etc.)
        base = re.sub(r'_\d+$', '', unit_name)
        if base != unit_name and base in index:
            return base
        return None

    def _find_dotted_support(dotted_name):
        """Find support files for a dotted unit name like ImpDef.Annex_D.

        Returns list of index keys to include (parent + child).
        ACATS convention: ImpDef.Annex_D -> impdefd.a (parent stem + first
        letter of last component).
        """
        results = []
        parts = dotted_name.split('.')
        # Always include the parent
        parent = _find_support(parts[0])
        if parent:
            results.append(parent)
        # Try ACATS child package convention: parent_stem + last_char
        # e.g., ImpDef.Annex_D -> impdef + d = impdefd
        if len(parts) >= 2:
            child_key = parts[0] + parts[-1][-1]  # e.g., impdef + d
            found = _find_support(child_key)
            if found:
                results.append(found)
            # Also try Ada child file convention: parent-child
            hyphen_key = '-'.join(parts)  # e.g., impdef-annex_d
            found = _find_support(hyphen_key)
            if found:
                results.append(found)
            # Try underscore join: impdef_annex_d
            under_key = '_'.join(parts)
            found = _find_support(under_key)
            if found:
                results.append(found)
        return results

    # Scan all test files for with clauses (capture dotted names)
    needed = set()
    for f in test_files:
        try:
            text = f.read_text(errors='replace')
        except OSError:
            continue
        for m in re.finditer(r'(?i)\bwith\s+([A-Za-z][A-Za-z0-9_.]*)', text):
            unit = m.group(1).lower().rstrip('.')
            if unit == 'report':
                continue
            if '.' in unit:
                for key in _find_dotted_support(unit):
                    needed.add(key)
            else:
                found = _find_support(unit)
                if found:
                    needed.add(found)

    if not needed:
        return []

    # Resolve transitive dependencies from support files themselves
    resolved = []
    visited = set()

    def _resolve(unit):
        if unit in visited:
            return
        visited.add(unit)
        path = index.get(unit)
        if not path:
            return
        # Scan this support file for its own dependencies
        try:
            text = path.read_text(errors='replace')
        except OSError:
            return
        for m in re.finditer(r'(?i)\bwith\s+([A-Za-z][A-Za-z0-9_.]*)', text):
            dep = m.group(1).lower().rstrip('.')
            if dep == 'report' or dep == unit:
                continue
            if '.' in dep:
                for dep_key in _find_dotted_support(dep):
                    _resolve(dep_key)
            else:
                found_dep = _find_support(dep)
                if found_dep:
                    _resolve(found_dep)
        resolved.append(path)

    for unit in sorted(needed):
        _resolve(unit)

    return resolved

def have_tools():
    """Check if all execution tools are available."""
    return bool(UM80_CMD and UL80_CMD and CPMEMU_CMD and LIBADA.exists()
                and REPORT_FILE.exists())


skip_if_no_tools = pytest.mark.skipif(
    not have_tools(),
    reason="Execution tools (um80, ul80, cpmemu, libada.lib) not available"
)


def get_acats_c_tests():
    """Get ACATS C-tests grouped into compilable units.

    Returns list of groups, where each group is a list of Path objects.
    The last file in each group is the main test file.
    Files within a group should be compiled together in order.

    Grouping rules:
    - .am files define families: the .am is main, .a files with same
      prefix (stem minus last digit) are support files
    - .ada files ending in a digit with 2+ siblings sharing the same
      prefix form families (e.g., ca1011a0.ada through ca1011a6.ada)
    - All other files are standalone (single-file groups)
    """
    if not ACATS_PATH.exists():
        return []

    # Collect all candidate files by extension
    ada_files = {}  # stem -> Path
    a_files = {}    # stem -> Path
    am_files = {}   # stem -> Path

    for f in sorted(ACATS_PATH.rglob("c*")):
        cat = f.parent.name
        if cat in SKIP_CATEGORIES:
            continue
        if f.suffix not in ('.ada', '.a', '.am'):
            continue
        stem = f.stem.lower()
        if f.suffix == '.ada':
            ada_files[stem] = f
        elif f.suffix == '.am':
            am_files[stem] = f
        elif f.suffix == '.a':
            a_files[stem] = f

    groups = []
    claimed_a = set()    # .a stems claimed by .am families
    claimed_ada = set()  # .ada stems claimed by .ada families

    # 1. Build .am families: .am file + .a files with same prefix
    for am_stem in sorted(am_files):
        am_path = am_files[am_stem]
        if not am_stem[-1:].isdigit():
            groups.append([am_path])
            continue
        base = am_stem[:-1]
        family = []
        for a_stem in sorted(a_files):
            if (a_stem.startswith(base) and
                    len(a_stem) == len(am_stem) and
                    a_stem[-1:].isdigit() and
                    a_stem != am_stem):
                family.append(a_files[a_stem])
                claimed_a.add(a_stem)
        family.append(am_path)  # main (.am) goes last
        groups.append(family)

    # 2. Build .ada families: files ending in digit grouped by stem[:-1]
    ada_by_base = defaultdict(list)
    for stem in sorted(ada_files):
        if stem[-1:].isdigit():
            ada_by_base[stem[:-1]].append(stem)

    for base in sorted(ada_by_base):
        members = ada_by_base[base]
        if len(members) >= 2:
            # Multi-file family - compile in order, main is last
            family = [ada_files[s] for s in sorted(members)]
            for s in members:
                claimed_ada.add(s)
            groups.append(family)

    # 3. Standalone files (not claimed by any family)
    for stem in sorted(ada_files):
        if stem not in claimed_ada:
            groups.append([ada_files[stem]])

    for stem in sorted(a_files):
        if stem not in claimed_a:
            groups.append([a_files[stem]])

    return groups


def _compile_worker(compiler, files, result_dict):
    """Worker function for compilation with timeout."""
    try:
        result = compiler.compile_files(files)
        result_dict['result'] = result
    except Exception as e:
        result_dict['error'] = str(e)


def _compile_with_timeout(compiler, files, timeout=60):
    """Run compiler.compile_files with a timeout using multiprocessing.

    Returns the compilation result, or None if timed out.
    """
    manager = multiprocessing.Manager()
    result_dict = manager.dict()
    p = multiprocessing.Process(target=_compile_worker, args=(compiler, files, result_dict))
    p.start()
    p.join(timeout)
    if p.is_alive():
        p.terminate()
        p.join(2)
        if p.is_alive():
            p.kill()
            p.join(1)
        return None
    if 'error' in result_dict:
        raise RuntimeError(result_dict['error'])
    return result_dict.get('result')


def compile_and_run_acats(test_files, timeout=15.0):
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

        # Compile: Report + support files + test files
        support = resolve_support_files(test_files)
        files = [REPORT_FILE] + support + list(test_files)
        try:
            result = _compile_with_timeout(compiler, files, timeout=90)
            if result is None:
                return "compile", False, "TIMEOUT: compilation hung (>90s)"
            if not result.success:
                msg = str(result.errors[0]) if result.errors else "unknown"
                return "compile", False, msg
            asm_file.write_text(result.output)
        except Exception as e:
            return "compile", False, str(e)

        # Assemble
        try:
            proc = subprocess.run(
                [UM80_CMD, "-o", str(rel_file), str(asm_file)],
                capture_output=True, text=True, timeout=60
            )
            if proc.returncode != 0:
                return "assemble", False, proc.stderr.strip()
        except subprocess.TimeoutExpired:
            return "assemble", False, "TIMEOUT: assembler hung (>60s)"

        # Link
        try:
            proc = subprocess.run(
                [UL80_CMD, "-o", str(com_file), str(rel_file), str(LIBADA)],
                capture_output=True, text=True, timeout=60
            )
            if proc.returncode != 0:
                return "link", False, proc.stderr.strip()
        except subprocess.TimeoutExpired:
            return "link", False, "TIMEOUT: linker hung (>60s)"

        # Run — enable timer interrupts if program uses tasking
        run_cmd = [CPMEMU_CMD, "--z80"]
        asm_text = asm_file.read_text()
        if "TASK_INI" in asm_text or "_TASK_INI" in asm_text:
            run_cmd.extend(["--int-cycles=10000"])
        run_cmd.append(str(com_file))
        try:
            proc = subprocess.run(
                run_cmd,
                capture_output=True, text=True, timeout=timeout,
                cwd=str(tmpdir)
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

    @pytest.mark.parametrize("test_files", ACATS_C_TESTS,
                             ids=lambda files: files[-1].stem)
    def test_acats_execute(self, test_files):
        """Compile and run an ACATS C-test (possibly multi-file) on cpmemu."""
        stage, success, output = compile_and_run_acats(test_files)

        if not success:
            if stage == "compile":
                pytest.fail(f"compile: {output[:100]}")
            elif stage == "link":
                pytest.fail(f"link: {output[:100]}")
            elif output == "TIMEOUT":
                pytest.fail(f"timeout (>15s)")
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
