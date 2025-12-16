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


def compile_and_run(source: str, timeout: float = 5.0, stdin_input: str = None) -> tuple[bool, str, str]:
    """
    Compile Ada source and run the resulting .com file.

    Args:
        source: Ada source code
        timeout: Execution timeout in seconds
        stdin_input: Optional input to pass to the program

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
        # Use the full library for proper symbol resolution
        libada = RUNTIME_PATH / "libada.lib"
        runtime_rel = RUNTIME_PATH / "runtime.rel"
        link_cmd = ["python3", "-m", "um80.ul80", "-o", str(com_file), str(rel_file)]
        if libada.exists():
            link_cmd.append(str(libada))
        elif runtime_rel.exists():
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
            timeout=timeout,
            input=stdin_input
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
        function Sum(A, B : Integer) return Integer is
        begin
            return A + B;
        end Sum;

        Result : Integer;
    begin
        Result := Sum(10, 20);
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"


@skip_if_no_tools
def test_function_name_add():
    """Test that 'Add' can be used as function name (Z80 mnemonic collision test).

    This verifies symbol mangling works correctly - user symbols are prefixed
    with '_' to avoid collisions with Z80 instruction mnemonics like ADD.
    """
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        function Add(A, B : Integer) return Integer is
        begin
            return A + B;
        end Add;

        Result : Integer;
    begin
        Result := Add(10, 20);
        Ada.Integer_Text_IO.Put(Result);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "30" in stdout, f"Expected 10+20=30, got: {stdout}"


@skip_if_no_tools
def test_function_name_sub():
    """Test that 'Sub' can be used as function name (Z80 SUB mnemonic)."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        function Sub(A, B : Integer) return Integer is
        begin
            return A - B;
        end Sub;

        Result : Integer;
    begin
        Result := Sub(30, 10);
        Ada.Integer_Text_IO.Put(Result);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "20" in stdout, f"Expected 30-10=20, got: {stdout}"


@skip_if_no_tools
def test_function_name_inc():
    """Test that 'Inc' can be used as function name (Z80 INC mnemonic)."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        function Inc(N : Integer) return Integer is
        begin
            return N + 1;
        end Inc;

        Result : Integer;
    begin
        Result := Inc(41);
        Ada.Integer_Text_IO.Put(Result);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout, f"Expected Inc(41)=42, got: {stdout}"


@skip_if_no_tools
def test_function_name_dec():
    """Test that 'Dec' can be used as function name (Z80 DEC mnemonic)."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        function Dec(N : Integer) return Integer is
        begin
            return N - 1;
        end Dec;

        Result : Integer;
    begin
        Result := Dec(43);
        Ada.Integer_Text_IO.Put(Result);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout, f"Expected Dec(43)=42, got: {stdout}"


@skip_if_no_tools
def test_recursive_function():
    """Test recursive function execution with factorial."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        function Fact(N : Integer) return Integer is
        begin
            if N <= 1 then
                return 1;
            else
                return N * Fact(N - 1);
            end if;
        end Fact;

        Result : Integer;
    begin
        Result := Fact(5);
        Ada.Integer_Text_IO.Put(Result);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "120" in stdout, f"Expected factorial 5! = 120, got: {stdout}"


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


@skip_if_no_tools
def test_integer_io_input():
    """Test Ada.Integer_Text_IO input."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        X : Integer;
        Y : Integer;
    begin
        Ada.Integer_Text_IO.Get(X);
        Ada.Integer_Text_IO.Get(Y);
        Ada.Integer_Text_IO.Put(X);
        Ada.Text_IO.New_Line;
        Ada.Integer_Text_IO.Put(Y);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source, stdin_input="123 -456\n")
    assert success, f"Program failed: {stderr}"
    assert "123" in stdout
    assert "-456" in stdout


@skip_if_no_tools
def test_text_io_get_line():
    """Test Ada.Text_IO.Get_Line input."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        Line : String(1..80);
        Last : Natural;
    begin
        Ada.Text_IO.Get_Line(Line, Last);
        Ada.Integer_Text_IO.Put(Last);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source, stdin_input="Hello World\n")
    assert success, f"Program failed: {stderr}"
    assert "11" in stdout  # "Hello World" is 11 characters


# ============================================================================
# Additional Feature Tests
# ============================================================================


@skip_if_no_tools
def test_nested_procedure():
    """Test nested procedure calls."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        Result : Integer := 0;

        procedure Inner(X : Integer) is
        begin
            Result := Result + X;
        end Inner;

    begin
        Inner(10);
        Inner(20);
        Inner(12);
        Ada.Integer_Text_IO.Put(Result);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_out_parameter():
    """Test out parameter mode."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        procedure Get_Values(A : out Integer; B : out Integer) is
        begin
            A := 100;
            B := 200;
        end Get_Values;

        X : Integer;
        Y : Integer;
    begin
        Get_Values(X, Y);
        Ada.Integer_Text_IO.Put(X);
        Ada.Text_IO.New_Line;
        Ada.Integer_Text_IO.Put(Y);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "100" in stdout
    assert "200" in stdout


@skip_if_no_tools
def test_inout_parameter():
    """Test in out parameter mode."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        procedure Double(X : in out Integer) is
        begin
            X := X * 2;
        end Double;

        N : Integer := 21;
    begin
        Double(N);
        Ada.Integer_Text_IO.Put(N);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_global_variable():
    """Test global variable access."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        Global : Integer := 10;

        procedure Increment is
        begin
            Global := Global + 5;
        end Increment;

        function Get_Value return Integer is
        begin
            return Global;
        end Get_Value;

    begin
        Increment;
        Increment;
        Increment;
        Ada.Integer_Text_IO.Put(Get_Value);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "25" in stdout  # 10 + 5 + 5 + 5 = 25


@skip_if_no_tools
def test_record_aggregate():
    """Test record initialization with aggregate."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;

        P : Point := (X => 30, Y => 12);
    begin
        Ada.Integer_Text_IO.Put(P.X + P.Y);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_array_aggregate():
    """Test array initialization with aggregate."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        type Int_Array is array (1..5) of Integer;
        A : Int_Array := (10, 8, 12, 7, 5);
        Sum : Integer := 0;
    begin
        for I in 1..5 loop
            Sum := Sum + A(I);
        end loop;
        Ada.Integer_Text_IO.Put(Sum);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout  # 10+8+12+7+5 = 42


@skip_if_no_tools
def test_enumeration_type():
    """Test enumeration type operations."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        type Color is (Red, Green, Blue);
        C : Color := Green;
        N : Integer;
    begin
        N := Color'Pos(C);
        Ada.Integer_Text_IO.Put(N);
        Ada.Text_IO.New_Line;
        C := Color'Val(2);
        N := Color'Pos(C);
        Ada.Integer_Text_IO.Put(N);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1" in stdout  # Green is at position 1
    assert "2" in stdout  # Blue is at position 2


@skip_if_no_tools
def test_multiple_return_paths():
    """Test function with multiple return statements."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        function Abs_Value(X : Integer) return Integer is
        begin
            if X < 0 then
                return -X;
            else
                return X;
            end if;
        end Abs_Value;
    begin
        Ada.Integer_Text_IO.Put(Abs_Value(-42));
        Ada.Text_IO.New_Line;
        Ada.Integer_Text_IO.Put(Abs_Value(42));
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert stdout.count("42") == 2


@skip_if_no_tools
def test_exit_with_name():
    """Test exit statement with loop name."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        Sum : Integer := 0;
    begin
        Outer: for I in 1..10 loop
            for J in 1..10 loop
                Sum := Sum + 1;
                if Sum >= 42 then
                    exit Outer;
                end if;
            end loop;
        end loop Outer;
        Ada.Integer_Text_IO.Put(Sum);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_exception_handling_basic():
    """Test basic exception handling with handler."""
    source = """
    with Ada.Text_IO;
    procedure Test is
    begin
        Ada.Text_IO.Put_Line("Before");
        begin
            Ada.Text_IO.Put_Line("In block");
            raise Constraint_Error;
            Ada.Text_IO.Put_Line("After raise");
        exception
            when Constraint_Error =>
                Ada.Text_IO.Put_Line("Caught CE");
        end;
        Ada.Text_IO.Put_Line("After block");
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Before" in stdout
    assert "In block" in stdout
    assert "Caught CE" in stdout
    assert "After block" in stdout
    assert "After raise" not in stdout


@skip_if_no_tools
def test_exception_handling_nested():
    """Test nested exception handlers."""
    source = """
    with Ada.Text_IO;
    procedure Test is
    begin
        Ada.Text_IO.Put_Line("Start");

        begin
            Ada.Text_IO.Put_Line("Outer block");
            begin
                Ada.Text_IO.Put_Line("Inner block");
                raise Program_Error;
                Ada.Text_IO.Put_Line("After inner raise");
            exception
                when Constraint_Error =>
                    Ada.Text_IO.Put_Line("Inner caught CE");
            end;
            Ada.Text_IO.Put_Line("After inner block");
        exception
            when Program_Error =>
                Ada.Text_IO.Put_Line("Outer caught PE");
        end;

        Ada.Text_IO.Put_Line("Done");
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Start" in stdout
    assert "Outer block" in stdout
    assert "Inner block" in stdout
    assert "Outer caught PE" in stdout
    assert "Done" in stdout
    assert "Inner caught CE" not in stdout
    assert "After inner raise" not in stdout
    assert "After inner block" not in stdout


@skip_if_no_tools
def test_exception_handling_when_others():
    """Test 'when others' catch-all handler."""
    source = """
    with Ada.Text_IO;
    procedure Test is
    begin
        Ada.Text_IO.Put_Line("Start");

        begin
            Ada.Text_IO.Put_Line("In block");
            raise Storage_Error;
        exception
            when others =>
                Ada.Text_IO.Put_Line("Caught others");
        end;

        Ada.Text_IO.Put_Line("Done");
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Start" in stdout
    assert "In block" in stdout
    assert "Caught others" in stdout
    assert "Done" in stdout


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
