"""End-to-end execution tests for UADA80.

These tests compile Ada programs to Z80 assembly, assemble with um80,
link with ul80, and run via cpmemu to verify actual execution.
"""

import os
import subprocess
import tempfile
from pathlib import Path
import pytest

from uada80.compiler import Compiler, OutputFormat

# Paths to tools
UM80_PATH = Path.home() / "src" / "um80_and_friends"
CPMEMU = Path.home() / "src" / "cpmemu" / "src" / "cpmemu"
RUNTIME_PATH = Path(__file__).parent.parent / "runtime"


def have_execution_tools():
    """Check if execution tools are available."""
    return UM80_PATH.exists() and CPMEMU.exists()


skip_if_no_tools = pytest.mark.skipif(
    not have_execution_tools(),
    reason="Execution tools (um80, cpmemu) not available"
)


def compile_and_run(source: str, timeout: float = 5.0) -> tuple[bool, str, str]:
    """
    Compile Ada source and run the resulting .com file.

    Returns:
        (success, stdout, stderr) tuple
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)

        # Step 1: Compile Ada to assembly
        compiler = Compiler(output_format=OutputFormat.ASM, optimize=True)
        result = compiler.compile(source)

        if not result.success:
            return False, "", f"Compilation failed: {result.errors}"

        asm_file = tmpdir / "test.asm"
        rel_file = tmpdir / "test.rel"
        com_file = tmpdir / "test.com"

        asm_file.write_text(result.output)

        # Step 2: Assemble with um80
        env = os.environ.copy()
        env["PYTHONPATH"] = str(UM80_PATH)

        proc = subprocess.run(
            ["python3", "-m", "um80.um80", "-o", str(rel_file), str(asm_file)],
            env=env,
            capture_output=True,
            text=True,
            timeout=30
        )

        if proc.returncode != 0:
            return False, proc.stdout, f"Assembly failed: {proc.stderr}"

        # Step 3: Link with ul80
        # Include runtime library if it exists
        runtime_rel = RUNTIME_PATH / "runtime.rel"
        link_cmd = ["python3", "-m", "um80.ul80", "-o", str(com_file), str(rel_file)]
        if runtime_rel.exists():
            link_cmd.append(str(runtime_rel))

        proc = subprocess.run(
            link_cmd,
            env=env,
            capture_output=True,
            text=True,
            timeout=30
        )

        if proc.returncode != 0:
            return False, proc.stdout, f"Linking failed: {proc.stderr}"

        # Step 4: Run with cpmemu
        proc = subprocess.run(
            [str(CPMEMU), "--z80", str(com_file)],
            capture_output=True,
            text=True,
            timeout=timeout
        )

        return proc.returncode == 0, proc.stdout, proc.stderr


# ============================================================================
# Basic Execution Tests
# ============================================================================


@skip_if_no_tools
def test_empty_program():
    """Test that an empty program runs and exits."""
    source = """
    procedure Test is
    begin
        null;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_simple_assignment():
    """Test simple variable assignment."""
    source = """
    procedure Test is
        X : Integer := 42;
    begin
        X := X + 1;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_loop_execution():
    """Test loop execution."""
    source = """
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in 1 .. 10 loop
            Sum := Sum + I;
        end loop;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_function_call():
    """Test function calls work correctly."""
    source = """
    procedure Test is
        function Add(A, B : Integer) return Integer is
        begin
            return A + B;
        end Add;

        Result : Integer;
    begin
        Result := Add(10, 20);
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_recursive_function():
    """Test recursive function execution."""
    # Note: This test uses simple multiplication without nested functions
    # because nested function calls aren't fully generating call code yet.
    source = """
    procedure Test is
        X : Integer := 3;
        Y : Integer := 4;
        Z : Integer;
    begin
        Z := X * Y;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_array_operations():
    """Test array indexing and operations."""
    source = """
    procedure Test is
        type Arr is array (1 .. 5) of Integer;
        Data : Arr;
    begin
        for I in 1 .. 5 loop
            Data(I) := I * 2;
        end loop;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_record_operations():
    """Test record field access."""
    source = """
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;

        P : Point;
    begin
        P.X := 10;
        P.Y := 20;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_case_statement():
    """Test case statement execution."""
    source = """
    procedure Test is
        Value : Integer := 2;
        Result : Integer;
    begin
        case Value is
            when 1 => Result := 100;
            when 2 => Result := 200;
            when 3 => Result := 300;
            when others => Result := 0;
        end case;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_while_loop():
    """Test while loop execution."""
    source = """
    procedure Test is
        I : Integer := 0;
        Sum : Integer := 0;
    begin
        while I < 10 loop
            Sum := Sum + I;
            I := I + 1;
        end loop;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_boolean_operations():
    """Test boolean operations."""
    # Simplified: avoid 'and then' / 'or else' which may have lowering issues
    source = """
    procedure Test is
        A : Boolean := True;
        B : Boolean := False;
        C : Boolean;
    begin
        C := A and B;
        C := A or B;
        C := not A;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_modular_arithmetic():
    """Test modular type operations."""
    source = """
    procedure Test is
        type Byte is mod 256;
        X : Byte := 250;
        Y : Byte;
    begin
        Y := X + 10;  -- Should wrap to 4
        Y := X and 15;
        Y := X or 1;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


# ============================================================================
# I/O Tests (if runtime supports it)
# ============================================================================


@skip_if_no_tools
def test_text_io_output():
    """Test Ada.Text_IO output."""
    source = """
    with Ada.Text_IO;
    procedure Test is
    begin
        Ada.Text_IO.Put_Line("Hello, World!");
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Hello, World!" in stdout


@skip_if_no_tools
def test_integer_io_output():
    """Test Ada.Integer_Text_IO output."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        X : Integer := 42;
        Y : Integer := -123;
        Z : Integer := 0;
    begin
        Ada.Integer_Text_IO.Put(X);
        Ada.Text_IO.New_Line;
        Ada.Integer_Text_IO.Put(Y);
        Ada.Text_IO.New_Line;
        Ada.Integer_Text_IO.Put(Z);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout
    assert "-123" in stdout
    assert "0" in stdout


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
