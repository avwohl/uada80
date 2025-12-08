"""Tests using ACATS test suite for parser validation.

Tests ONLY valid Ada files (A, C, D, E, L prefixes).
B-tests are intentionally invalid Ada for testing error detection - excluded.
"""

import signal
import pytest
from pathlib import Path
from uada80.parser import parse

# Path to ACATS tests
ACATS_PATH = Path("/home/wohl/src/acats/tests")


class ParserTimeout(Exception):
    pass


def timeout_handler(signum, frame):
    raise ParserTimeout("Parsing timed out")


def get_legal_acats_files():
    """Collect legal Ada test files (A, C, D, E, L prefixes only).

    B-tests are intentionally invalid Ada for testing compiler error detection.
    We exclude them since they're not meant to parse successfully.
    """
    if not ACATS_PATH.exists():
        return []
    files = []
    for ext in ("*.ada", "*.a"):
        files.extend(ACATS_PATH.rglob(ext))
    # Only include valid Ada tests
    legal_prefixes = {'A', 'C', 'D', 'E', 'L'}
    legal_files = [f for f in files if f.stem[0].upper() in legal_prefixes]
    return sorted(legal_files)


LEGAL_ACATS_FILES = get_legal_acats_files()


@pytest.mark.skipif(not LEGAL_ACATS_FILES, reason="ACATS not installed")
class TestACATSParsing:
    """Test that our parser can handle ACATS test files."""

    @pytest.mark.parametrize("test_file", LEGAL_ACATS_FILES, ids=lambda f: f.stem)
    def test_parse_acats_file(self, test_file):
        """Test parsing an ACATS test file - must parse without exceptions."""
        # Read file with encoding fallback
        try:
            source = test_file.read_text(encoding='utf-8')
        except UnicodeDecodeError:
            source = test_file.read_text(encoding='latin-1')

        # Set up 30 second timeout - fail if exceeded
        old_handler = signal.signal(signal.SIGALRM, timeout_handler)
        signal.alarm(30)
        try:
            ast = parse(source)
            signal.alarm(0)
            assert ast is not None, "Parser returned None"
        except ParserTimeout:
            signal.alarm(0)
            pytest.fail(f"Parser timeout (>30s) on {test_file.name}")
        finally:
            signal.signal(signal.SIGALRM, old_handler)
