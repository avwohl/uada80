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


# ============================================================================
# String Concatenation Tests
# ============================================================================


@skip_if_no_tools
def test_string_concatenation():
    """Test string concatenation with & operator."""
    source = """
    with Ada.Text_IO;
    procedure Test is
    begin
        Ada.Text_IO.Put_Line("Hello" & " " & "World");
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Hello World" in stdout


@skip_if_no_tools
def test_string_variable_concatenation():
    """Test string concatenation with variables."""
    source = """
    with Ada.Text_IO;
    procedure Test is
        S1 : String(1..5) := "Hello";
        S2 : String(1..5) := "World";
    begin
        Ada.Text_IO.Put_Line(S1 & " " & S2);
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Hello World" in stdout


@skip_if_no_tools
def test_integer_image():
    """Test Integer'Image attribute."""
    # Test direct Integer'Image output (without concatenation first)
    source = """
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 42;
    begin
        Ada.Text_IO.Put_Line(Integer'Image(X));
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_integer_image_concat():
    """Test Integer'Image with string concatenation."""
    source = """
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 42;
    begin
        Ada.Text_IO.Put_Line("Value is " & Integer'Image(X));
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Value is" in stdout
    assert "42" in stdout


@skip_if_no_tools
def test_access_type_basic():
    """Test basic access type (pointer) operations."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer'(42);
        Ada.Integer_Text_IO.Put(P.all);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_access_type_modify():
    """Test modifying value through access type."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer'(10);
        P.all := P.all + 32;
        Ada.Integer_Text_IO.Put(P.all);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_array_slice():
    """Test array slicing operations."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        type Int_Array is array (1..10) of Integer;
        A : Int_Array := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
        Sum : Integer := 0;
    begin
        -- Sum elements 3..5
        for I in 3..5 loop
            Sum := Sum + A(I);
        end loop;
        Ada.Integer_Text_IO.Put(Sum);  -- Should be 12 (3+4+5)
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "12" in stdout


@skip_if_no_tools
def test_nested_loops():
    """Test nested loop execution."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in 1..3 loop
            for J in 1..3 loop
                Sum := Sum + 1;
            end loop;
        end loop;
        Ada.Integer_Text_IO.Put(Sum);  -- Should be 9
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "9" in stdout


@skip_if_no_tools
def test_loop_exit_when():
    """Test exit when condition in loops."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        I : Integer := 0;
    begin
        loop
            I := I + 1;
            exit when I >= 5;
        end loop;
        Ada.Integer_Text_IO.Put(I);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "5" in stdout


@skip_if_no_tools
def test_if_elsif():
    """Test if-elsif-else chains."""
    source = """
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 42;
    begin
        if X < 10 then
            Ada.Text_IO.Put_Line("Small");
        elsif X < 50 then
            Ada.Text_IO.Put_Line("Medium");
        else
            Ada.Text_IO.Put_Line("Large");
        end if;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Medium" in stdout


@skip_if_no_tools
def test_function_return_record():
    """Test function returning a record type."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        type Point is record
            X, Y : Integer;
        end record;

        function Make_Point(X, Y : Integer) return Point is
        begin
            return (X => X, Y => Y);
        end Make_Point;

        P : Point;
    begin
        P := Make_Point(10, 20);
        Ada.Integer_Text_IO.Put(P.X + P.Y);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "30" in stdout


@skip_if_no_tools
def test_procedure_multiple_out():
    """Test procedure with multiple out parameters."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        procedure Swap(A, B : in out Integer) is
            Temp : Integer;
        begin
            Temp := A;
            A := B;
            B := Temp;
        end Swap;

        X : Integer := 10;
        Y : Integer := 20;
    begin
        Swap(X, Y);
        Ada.Integer_Text_IO.Put(X);
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Y);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "20" in stdout
    assert "10" in stdout


@skip_if_no_tools
def test_modular_wraparound():
    """Test modular type wraparound behavior."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        type Byte is mod 256;
        B : Byte := 250;
    begin
        B := B + 10;  -- Should wrap to 4
        Ada.Integer_Text_IO.Put(Integer(B));
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "4" in stdout


@skip_if_no_tools
def test_character_operations():
    """Test character operations and attributes."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        C : Character := 'A';
    begin
        Ada.Text_IO.Put(C);
        Ada.Integer_Text_IO.Put(Character'Pos(C));
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "A" in stdout
    assert "65" in stdout


@skip_if_no_tools
def test_for_loop_reverse():
    """Test for loop with reverse iteration."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in reverse 1..4 loop
            Sum := Sum * 10 + I;
        end loop;
        Ada.Integer_Text_IO.Put(Sum);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    # reverse 1..4 means: 4, 3, 2, 1
    # Sum = 0*10+4=4, 4*10+3=43, 43*10+2=432, 432*10+1=4321
    assert "4321" in stdout


@skip_if_no_tools
def test_array_length_attribute():
    """Test array 'Length attribute."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array (1..10) of Integer;
        A : Arr := (others => 0);
    begin
        Ada.Integer_Text_IO.Put(A'Length);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "10" in stdout


@skip_if_no_tools
def test_array_first_last():
    """Test array 'First and 'Last attributes."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array (5..15) of Integer;
        A : Arr := (others => 0);
    begin
        Ada.Integer_Text_IO.Put(A'First);
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(A'Last);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "5" in stdout
    assert "15" in stdout


@skip_if_no_tools
def test_derived_type():
    """Test derived types with arithmetic."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Distance is new Integer;
        D1 : Distance := 100;
        D2 : Distance := 50;
        D3 : Distance;
    begin
        D3 := D1 + D2;
        Ada.Integer_Text_IO.Put(Integer(D3));
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "150" in stdout


@skip_if_no_tools
def test_subtype_constraint():
    """Test subtype with range constraint."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        subtype Small is Integer range 1..100;
        X : Small := 50;
        Y : Small := 25;
    begin
        X := X + Y;
        Ada.Integer_Text_IO.Put(X);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "75" in stdout


@skip_if_no_tools
def test_array_2d():
    """Test two-dimensional array."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Matrix is array (1..3, 1..3) of Integer;
        M : Matrix := ((1, 2, 3), (4, 5, 6), (7, 8, 9));
        Sum : Integer := 0;
    begin
        for I in 1..3 loop
            for J in 1..3 loop
                Sum := Sum + M(I, J);
            end loop;
        end loop;
        Ada.Integer_Text_IO.Put(Sum);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    # Sum of 1+2+3+4+5+6+7+8+9 = 45
    assert "45" in stdout


@skip_if_no_tools
def test_rem_operator():
    """Test REM operator (remainder with sign of dividend)."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
    begin
        -- REM keeps sign of dividend
        Ada.Integer_Text_IO.Put(7 rem 3);       -- 1
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put((-7) rem 3);    -- -1
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(7 rem (-3));    -- 1
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put((-7) rem (-3)); -- -1
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1 -1 1 -1" in stdout


@skip_if_no_tools
def test_exponentiation():
    """Test exponentiation operator (**)."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
    begin
        Ada.Integer_Text_IO.Put(2 ** 0);   -- 1
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(2 ** 1);   -- 2
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(2 ** 4);   -- 16
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(3 ** 3);   -- 27
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1 2 16 27" in stdout


@skip_if_no_tools
def test_enumeration_succ_pred():
    """Test 'Succ and 'Pred attributes for enumeration types."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Color is (Red, Green, Blue, Yellow);
    begin
        -- Succ: successor of an enumeration
        Ada.Integer_Text_IO.Put(Color'Pos(Color'Succ(Red)));    -- 1 (Green)
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Color'Pos(Color'Succ(Green)));  -- 2 (Blue)
        Ada.Text_IO.Put(" ");
        -- Pred: predecessor of an enumeration
        Ada.Integer_Text_IO.Put(Color'Pos(Color'Pred(Yellow))); -- 2 (Blue)
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Color'Pos(Color'Pred(Blue)));   -- 1 (Green)
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1 2 2 1" in stdout


@skip_if_no_tools
def test_min_max_attributes():
    """Test 'Min and 'Max attributes."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
    begin
        Ada.Integer_Text_IO.Put(Integer'Min(10, 5));   -- 5
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Integer'Max(10, 5));   -- 10
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Integer'Min(-3, -7));  -- -7
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Integer'Max(-3, -7));  -- -3
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "5 10 -7 -3" in stdout


@skip_if_no_tools
def test_conditional_expression():
    """Test conditional expressions (if-expression)."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 10;
        Y : Integer := 5;
    begin
        -- Simple if-expression
        Ada.Integer_Text_IO.Put((if X > Y then 1 else 0));
        Ada.Text_IO.Put(" ");
        -- Nested if-expression
        Ada.Integer_Text_IO.Put((if X > 15 then 3 elsif X > 8 then 2 else 1));
        Ada.Text_IO.Put(" ");
        -- If-expression with computation
        Ada.Integer_Text_IO.Put((if X > 0 then X + Y else X - Y));
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1 2 15" in stdout


@skip_if_no_tools
def test_case_expression():
    """Test case expressions."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        N : Integer := 2;
    begin
        Ada.Integer_Text_IO.Put((case N is when 1 => 10, when 2 => 20, when others => 0));
        Ada.Text_IO.Put(" ");
        N := 5;
        Ada.Integer_Text_IO.Put((case N is when 1 => 10, when 2 => 20, when others => 99));
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "20 99" in stdout


@skip_if_no_tools
def test_bitwise_xor():
    """Test XOR operator for modular types."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Byte is mod 256;
        A : Byte := 16#0F#;   -- 00001111
        B : Byte := 16#55#;   -- 01010101
    begin
        Ada.Integer_Text_IO.Put(Integer(A xor B));  -- 01011010 = 90
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Integer(A and B));  -- 00000101 = 5
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Integer(A or B));   -- 01011111 = 95
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "90 5 95" in stdout


@skip_if_no_tools
def test_target_name():
    """Test target name (@) in Ada 2022."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 10;
    begin
        X := @ + 5;   -- X := X + 5
        Ada.Integer_Text_IO.Put(X);  -- 15
        Ada.Text_IO.Put(" ");
        X := @ * 2;   -- X := X * 2
        Ada.Integer_Text_IO.Put(X);  -- 30
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "15 30" in stdout


@skip_if_no_tools
def test_string_slicing():
    """Test string slicing operations."""
    source = """
    with Ada.Text_IO;
    procedure Test is
        S : String(1..10) := "HelloWorld";
    begin
        Ada.Text_IO.Put_Line(S(1..5));   -- Hello
        Ada.Text_IO.Put_Line(S(6..10));  -- World
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "Hello" in stdout
    assert "World" in stdout


@skip_if_no_tools
def test_named_parameters():
    """Test named parameter association in function/procedure calls."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        function Compute(A, B, C : Integer) return Integer is
        begin
            return A * 100 + B * 10 + C;
        end Compute;

        R : Integer;
    begin
        -- Positional
        R := Compute(1, 2, 3);
        Ada.Integer_Text_IO.Put(R);
        Ada.Text_IO.Put(" ");
        -- All named, in order
        R := Compute(A => 4, B => 5, C => 6);
        Ada.Integer_Text_IO.Put(R);
        Ada.Text_IO.Put(" ");
        -- Named, out of order
        R := Compute(C => 9, A => 7, B => 8);
        Ada.Integer_Text_IO.Put(R);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "123 456 789" in stdout


@skip_if_no_tools
def test_mixed_array_aggregate():
    """Test array aggregate with mixed positional and others."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array (1..5) of Integer;
        A : Arr := (1, 2, others => 0);
        Sum : Integer := 0;
    begin
        for I in 1..5 loop
            Sum := Sum + A(I);
        end loop;
        Ada.Integer_Text_IO.Put(Sum);  -- 1+2+0+0+0 = 3
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "3" in stdout


@skip_if_no_tools
def test_operator_overloading():
    """Test user-defined operator overloading."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Vector is record
            X, Y : Integer;
        end record;

        function "+"(A, B : Vector) return Vector is
        begin
            return (X => A.X + B.X, Y => A.Y + B.Y);
        end "+";

        V1 : Vector := (X => 10, Y => 20);
        V2 : Vector := (X => 5, Y => 7);
        V3 : Vector;
    begin
        V3 := V1 + V2;
        Ada.Integer_Text_IO.Put(V3.X);
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(V3.Y);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "15 27" in stdout


@skip_if_no_tools
def test_array_range_iteration():
    """Test for loop with array 'Range attribute."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array (5..9) of Integer;
        A : Arr := (10, 20, 30, 40, 50);
        Sum : Integer := 0;
    begin
        for I in A'Range loop
            Sum := Sum + A(I);
        end loop;
        Ada.Integer_Text_IO.Put(Sum);  -- 10+20+30+40+50 = 150
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "150" in stdout


@skip_if_no_tools
def test_default_parameters():
    """Test function with default parameter values."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        function Add(A : Integer; B : Integer := 10; C : Integer := 100) return Integer is
        begin
            return A + B + C;
        end Add;
    begin
        Ada.Integer_Text_IO.Put(Add(1));          -- 1+10+100 = 111
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Add(1, 2));       -- 1+2+100 = 103
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(Add(1, 2, 3));    -- 1+2+3 = 6
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "111 103 6" in stdout


@skip_if_no_tools
def test_abs_negation():
    """Test abs attribute and unary negation."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := -42;
        Y : Integer := 17;
    begin
        Ada.Integer_Text_IO.Put(abs X);    -- 42
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(abs Y);    -- 17
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(-Y);       -- -17
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(-(-X));    -- -42
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42 17 -17 -42" in stdout


@skip_if_no_tools
def test_nested_function_access_outer():
    """Test nested function accessing outer scope variables."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        Outer_Var : Integer := 100;

        function Inner(X : Integer) return Integer is
        begin
            return X + Outer_Var;
        end Inner;
    begin
        Ada.Integer_Text_IO.Put(Inner(5));   -- 5 + 100 = 105
        Ada.Text_IO.Put(" ");
        Outer_Var := 200;
        Ada.Integer_Text_IO.Put(Inner(5));   -- 5 + 200 = 205
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "105 205" in stdout


@skip_if_no_tools
def test_null_statement():
    """Test null statement."""
    source = """
    with Ada.Text_IO;
    procedure Test is
    begin
        null;
        Ada.Text_IO.Put_Line("OK");
        null;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "OK" in stdout


@skip_if_no_tools
def test_integer_comparison():
    """Test integer comparison operators."""
    source = """
    with Ada.Text_IO;
    procedure Test is
    begin
        if 5 < 10 then Ada.Text_IO.Put("A"); end if;
        if 10 > 5 then Ada.Text_IO.Put("B"); end if;
        if 5 <= 5 then Ada.Text_IO.Put("C"); end if;
        if 5 >= 5 then Ada.Text_IO.Put("D"); end if;
        if 5 = 5 then Ada.Text_IO.Put("E"); end if;
        if 5 /= 6 then Ada.Text_IO.Put("F"); end if;
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "ABCDEF" in stdout


@skip_if_no_tools
def test_boolean_short_circuit():
    """Test and then / or else short-circuit evaluation."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        Counter : Integer := 0;

        function Increment return Boolean is
        begin
            Counter := Counter + 1;
            return True;
        end Increment;
    begin
        -- Short-circuit: second operand not evaluated when first is False
        if False and then Increment then
            null;
        end if;
        Ada.Integer_Text_IO.Put(Counter);  -- 0 (not incremented)
        Ada.Text_IO.Put(" ");

        -- Short-circuit: second operand not evaluated when first is True
        if True or else Increment then
            null;
        end if;
        Ada.Integer_Text_IO.Put(Counter);  -- 0 (not incremented)
        Ada.Text_IO.Put(" ");

        -- Non-short-circuit: both evaluated
        if True and then Increment then
            null;
        end if;
        Ada.Integer_Text_IO.Put(Counter);  -- 1 (incremented)
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "0 0 1" in stdout


@skip_if_no_tools
def test_loop_name_and_exit():
    """Test named loops with exit from outer loop."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        Total : Integer := 0;
    begin
        Outer: for I in 1..10 loop
            for J in 1..10 loop
                Total := Total + 1;
                if Total = 15 then
                    exit Outer;
                end if;
            end loop;
        end loop Outer;
        Ada.Integer_Text_IO.Put(Total);  -- 15
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "15" in stdout


@skip_if_no_tools
def test_constant_declaration():
    """Test constant declarations."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        Pi_Times_100 : constant Integer := 314;
        Max_Size : constant := 1000;  -- Universal integer
        Result : Integer;
    begin
        Result := Pi_Times_100 + Max_Size;
        Ada.Integer_Text_IO.Put(Result);  -- 1314
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1314" in stdout


@skip_if_no_tools
def test_type_conversion():
    """Test type conversion between related types."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Small is range 0..100;
        type Big is range 0..10000;

        S : Small := 50;
        B : Big;
    begin
        B := Big(S) * 100;
        Ada.Integer_Text_IO.Put(Integer(B));  -- 5000
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "5000" in stdout


def test_array_attributes_range():
    """Test array 'Range attribute in for loop."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array(1..5) of Integer;
        A : Arr := (10, 20, 30, 40, 50);
        Sum : Integer := 0;
    begin
        for I in A'Range loop
            Sum := Sum + A(I);
        end loop;
        Ada.Integer_Text_IO.Put(Sum);  -- 150
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "150" in stdout


def test_string_length():
    """Test string 'Length attribute."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        S : String(1..5) := "Hello";
    begin
        Ada.Integer_Text_IO.Put(S'Length);  -- 5
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "5" in stdout


def test_record_with_default():
    """Test record components with default values."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Point is record
            X : Integer := 10;
            Y : Integer := 20;
        end record;
        P : Point;
    begin
        Ada.Integer_Text_IO.Put(P.X + P.Y);  -- 30
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "30" in stdout


def test_function_overloading():
    """Test function overloading with different parameter types."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        function Double(X : Integer) return Integer is
        begin
            return X * 2;
        end Double;

        function Double(X : Integer; Y : Integer) return Integer is
        begin
            return (X + Y) * 2;
        end Double;

        R1 : Integer;
        R2 : Integer;
    begin
        R1 := Double(10);      -- 20
        R2 := Double(10, 5);   -- 30
        Ada.Integer_Text_IO.Put(R1 + R2);  -- 50
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "50" in stdout


def test_declare_block():
    """Test declare block with local variables."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 10;
    begin
        declare
            Y : Integer := 20;
        begin
            X := X + Y;
        end;
        Ada.Integer_Text_IO.Put(X);  -- 30
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "30" in stdout


def test_named_block():
    """Test named block statement."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 0;
    begin
        Compute:
        begin
            X := X + 100;
        end Compute;
        Ada.Integer_Text_IO.Put(X);  -- 100
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "100" in stdout


def test_not_operator():
    """Test NOT operator on boolean."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        B : Boolean := False;
    begin
        if not B then
            Ada.Integer_Text_IO.Put(1);
        else
            Ada.Integer_Text_IO.Put(0);
        end if;
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1" in stdout


def test_in_range():
    """Test 'in range' membership test."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 5;
    begin
        if X in 1..10 then
            Ada.Integer_Text_IO.Put(1);
        else
            Ada.Integer_Text_IO.Put(0);
        end if;
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1" in stdout


def test_subtype_declaration():
    """Test subtype with range constraint."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        subtype Small is Integer range 1..100;
        X : Small := 50;
    begin
        Ada.Integer_Text_IO.Put(X * 2);  -- 100
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "100" in stdout


def test_constant_object():
    """Test constant object declaration."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        Pi_100 : constant Integer := 314;
    begin
        Ada.Integer_Text_IO.Put(Pi_100);  -- 314
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "314" in stdout


def test_unary_minus():
    """Test unary minus operator."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 42;
        Y : Integer := -X;
    begin
        Ada.Integer_Text_IO.Put(Y);  -- -42
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "-42" in stdout


def test_numeric_underscores():
    """Test numeric literals with underscores."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 1_000;
        Y : Integer := 2_000;
    begin
        Ada.Integer_Text_IO.Put(X + Y);  -- 3000
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "3000" in stdout


def test_array_of_records():
    """Test array of records."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Point is record
            X, Y : Integer;
        end record;
        type Points is array (1..3) of Point;
        P : Points := ((1, 2), (3, 4), (5, 6));
    begin
        Ada.Integer_Text_IO.Put(P(2).X + P(2).Y);  -- 3+4=7
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "7" in stdout


def test_record_with_array():
    """Test record containing an array."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array (1..3) of Integer;
        type R is record
            Values : Arr;
            Sum : Integer;
        end record;
        X : R := (Values => (10, 20, 30), Sum => 60);
    begin
        Ada.Integer_Text_IO.Put(X.Values(2));  -- 20
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "20" in stdout


def test_null_access():
    """Test null access value."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr := null;
    begin
        if P = null then
            Ada.Integer_Text_IO.Put(1);
        else
            Ada.Integer_Text_IO.Put(0);
        end if;
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1" in stdout


def test_multiple_variables_one_line():
    """Test multiple variable declarations on one line."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        A, B, C : Integer := 10;
    begin
        Ada.Integer_Text_IO.Put(A + B + C);  -- 30
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "30" in stdout


def test_and_then_or_else():
    """Test short-circuit boolean operators."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 0;
        function Inc return Boolean is
        begin
            X := X + 1;
            return True;
        end Inc;
    begin
        -- and then: second should not evaluate
        if False and then Inc then
            null;
        end if;
        Ada.Integer_Text_IO.Put(X);  -- 0
        Ada.Text_IO.New_Line;
        -- or else: second should not evaluate
        if True or else Inc then
            null;
        end if;
        Ada.Integer_Text_IO.Put(X);  -- still 0
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    # X should be 0 both times
    assert stdout.strip().split()[0] == "0"
    assert stdout.strip().split()[1] == "0"


def test_array_index_non_one():
    """Test array with non-1 starting index."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array (5..8) of Integer;
        A : Arr := (5 => 10, 6 => 20, 7 => 30, 8 => 40);
    begin
        Ada.Integer_Text_IO.Put(A'First);  -- 5
        Ada.Text_IO.New_Line;
        Ada.Integer_Text_IO.Put(A'Last);   -- 8
        Ada.Text_IO.New_Line;
        Ada.Integer_Text_IO.Put(A(6));     -- 20
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    lines = stdout.strip().split()
    assert lines[0] == "5"
    assert lines[1] == "8"
    assert lines[2] == "20"


def test_nested_record():
    """Test nested record type."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Inner is record
            X : Integer;
        end record;
        type Outer is record
            I : Inner;
            Y : Integer;
        end record;
        O : Outer := (I => (X => 10), Y => 20);
    begin
        Ada.Integer_Text_IO.Put(O.I.X + O.Y);  -- 30
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "30" in stdout


def test_positive_subtype():
    """Test Positive subtype (1..Integer'Last)."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Positive := 42;
    begin
        Ada.Integer_Text_IO.Put(X);  -- 42
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


def test_natural_subtype():
    """Test Natural subtype (0..Integer'Last)."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Natural := 0;
    begin
        Ada.Integer_Text_IO.Put(X);  -- 0
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "0" in stdout.strip()


@skip_if_no_tools
def test_discriminated_record():
    """Test discriminated record type."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Buffer (Size : Positive) is record
            Count : Natural := 0;
            Data  : Integer;
        end record;
        B : Buffer(10);
    begin
        B.Count := 5;
        B.Data := 42;
        Ada.Integer_Text_IO.Put(B.Size);  -- 10
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(B.Count);  -- 5
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(B.Data);  -- 42
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "10" in stdout
    assert "5" in stdout
    assert "42" in stdout


@skip_if_no_tools
def test_generic_function():
    """Test generic function instantiation."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        generic
            type T is private;
        function Identity(X : T) return T;

        function Identity(X : T) return T is
        begin
            return X;
        end Identity;

        function Int_Identity is new Identity(Integer);
        X : Integer := 42;
    begin
        Ada.Integer_Text_IO.Put(Int_Identity(X));  -- 42
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_renaming_declaration():
    """Test renaming declarations."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        X : Integer := 100;
        Y : Integer renames X;
        procedure Put(N : Integer) renames Ada.Integer_Text_IO.Put;
    begin
        Y := 50;
        Put(X);  -- 50 (X and Y are same object)
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "50" in stdout


@skip_if_no_tools
def test_array_concatenation():
    """Test array concatenation operator."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array (Positive range <>) of Integer;
        A : Arr(1..2) := (1, 2);
        B : Arr(1..2) := (3, 4);
        C : Arr(1..4) := A & B;
    begin
        for I in C'Range loop
            Ada.Integer_Text_IO.Put(C(I));
            Ada.Text_IO.Put(" ");
        end loop;
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "1" in stdout
    assert "2" in stdout
    assert "3" in stdout
    assert "4" in stdout


@skip_if_no_tools
def test_record_assignment():
    """Test whole record assignment."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Point is record
            X, Y : Integer;
        end record;
        P1 : Point := (10, 20);
        P2 : Point;
    begin
        P2 := P1;  -- Copy whole record
        Ada.Integer_Text_IO.Put(P2.X);  -- 10
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(P2.Y);  -- 20
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "10" in stdout
    assert "20" in stdout


@skip_if_no_tools
def test_mod_attribute():
    """Test 'Mod attribute for modular types."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Byte is mod 256;
        X : Byte := 250;
    begin
        X := X + 10;  -- Wraps to 4
        Ada.Integer_Text_IO.Put(Integer(X));  -- 4
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "4" in stdout


@skip_if_no_tools
def test_array_aggregate_others():
    """Test array aggregate with others clause."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Arr is array (1..5) of Integer;
        A : Arr := (1 => 10, 3 => 30, others => 0);
        Sum : Integer := 0;
    begin
        for I in A'Range loop
            Sum := Sum + A(I);
        end loop;
        Ada.Integer_Text_IO.Put(Sum);  -- 10 + 0 + 30 + 0 + 0 = 40
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "40" in stdout


@skip_if_no_tools
def test_nested_package():
    """Test nested package declarations."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        package Inner is
            Value : Integer := 42;
            function Get return Integer;
        end Inner;

        package body Inner is
            function Get return Integer is
            begin
                return Value;
            end Get;
        end Inner;
    begin
        Ada.Integer_Text_IO.Put(Inner.Get);  -- 42
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "42" in stdout


@skip_if_no_tools
def test_for_loop_array():
    """Test for loop over array with 'Range."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        A : array (1..5) of Integer := (2, 4, 6, 8, 10);
        Sum : Integer := 0;
    begin
        for I in A'Range loop
            Sum := Sum + A(I);
        end loop;
        Ada.Integer_Text_IO.Put(Sum);  -- 30
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "30" in stdout


@skip_if_no_tools
def test_local_constant():
    """Test local constant declarations."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        Pi_Times_100 : constant Integer := 314;
        Radius : Integer := 10;
    begin
        Ada.Integer_Text_IO.Put(Pi_Times_100 * Radius);  -- 3140
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "3140" in stdout


@skip_if_no_tools
def test_array_element_assignment():
    """Test array element assignment."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        A : array (1..3) of Integer := (0, 0, 0);
    begin
        A(1) := 10;
        A(2) := 20;
        A(3) := 30;
        Ada.Integer_Text_IO.Put(A(1) + A(2) + A(3));  -- 60
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "60" in stdout


@skip_if_no_tools
def test_record_field_assignment():
    """Test individual record field assignment."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        type Point is record
            X, Y : Integer;
        end record;
        P : Point;
    begin
        P.X := 15;
        P.Y := 25;
        Ada.Integer_Text_IO.Put(P.X + P.Y);  -- 40
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "40" in stdout


@skip_if_no_tools
def test_negative_numbers():
    """Test negative number handling."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
        A : Integer := -50;
        B : Integer := 30;
    begin
        Ada.Integer_Text_IO.Put(A + B);  -- -20
        Ada.Text_IO.New_Line;
        Ada.Integer_Text_IO.Put(A * -1);  -- 50
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "-20" in stdout
    assert "50" in stdout


@skip_if_no_tools
def test_integer_division():
    """Test integer division and remainder."""
    source = """
    with Ada.Integer_Text_IO;
    with Ada.Text_IO;
    procedure Test is
    begin
        Ada.Integer_Text_IO.Put(17 / 5);   -- 3
        Ada.Text_IO.Put(" ");
        Ada.Integer_Text_IO.Put(17 mod 5); -- 2
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "3" in stdout
    assert "2" in stdout


# ============================================================================
# Float64 (Long_Float) Tests
# ============================================================================


@skip_if_no_tools
def test_long_float_addition():
    """Test Long_Float addition and conversion to Integer.

    This tests the Float64 runtime library (_f64_add, _f64_ftoi).
    """
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        X : Long_Float := 3.0;
        Y : Long_Float := 2.0;
        Z : Long_Float;
        I : Integer;
    begin
        Z := X + Y;
        I := Integer(Z);
        Ada.Integer_Text_IO.Put(I);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "5" in stdout, f"Expected 3.0+2.0=5, got: {stdout}"


@skip_if_no_tools
def test_long_float_subtraction():
    """Test Long_Float subtraction and conversion to Integer."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        X : Long_Float := 10.0;
        Y : Long_Float := 4.0;
        Z : Long_Float;
        I : Integer;
    begin
        Z := X - Y;
        I := Integer(Z);
        Ada.Integer_Text_IO.Put(I);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "6" in stdout, f"Expected 10.0-4.0=6, got: {stdout}"


@skip_if_no_tools
def test_long_float_multiplication():
    """Test Long_Float multiplication and conversion to Integer."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        X : Long_Float := 3.0;
        Y : Long_Float := 4.0;
        Z : Long_Float;
        I : Integer;
    begin
        Z := X * Y;
        I := Integer(Z);
        Ada.Integer_Text_IO.Put(I);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "12" in stdout, f"Expected 3.0*4.0=12, got: {stdout}"


@skip_if_no_tools
def test_long_float_division():
    """Test Long_Float division and conversion to Integer."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        X : Long_Float := 10.0;
        Y : Long_Float := 2.0;
        Z : Long_Float;
        I : Integer;
    begin
        Z := X / Y;
        I := Integer(Z);
        Ada.Integer_Text_IO.Put(I);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "5" in stdout, f"Expected 10.0/2.0=5, got: {stdout}"


@skip_if_no_tools
def test_long_float_itof():
    """Test Integer to Long_Float conversion."""
    source = """
    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    procedure Test is
        I : Integer := 7;
        X : Long_Float;
        R : Integer;
    begin
        X := Long_Float(I);
        R := Integer(X);
        Ada.Integer_Text_IO.Put(R);
        Ada.Text_IO.New_Line;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "7" in stdout, f"Expected itof(7)=7, got: {stdout}"


@skip_if_no_tools
def test_long_float_comparison():
    """Test Long_Float comparison operators."""
    source = """
    with Ada.Text_IO;
    procedure Test is
        X : Long_Float := 5.0;
        Y : Long_Float := 3.0;
        Z : Long_Float := 5.0;
    begin
        if X > Y then
            Ada.Text_IO.Put_Line("GT OK");
        end if;
        if Y < X then
            Ada.Text_IO.Put_Line("LT OK");
        end if;
        if X = Z then
            Ada.Text_IO.Put_Line("EQ OK");
        end if;
        if X /= Y then
            Ada.Text_IO.Put_Line("NE OK");
        end if;
        if X >= Z then
            Ada.Text_IO.Put_Line("GE OK");
        end if;
        if Y <= X then
            Ada.Text_IO.Put_Line("LE OK");
        end if;
    end Test;
    """

    success, stdout, stderr = compile_and_run(source)
    assert success, f"Program failed: {stderr}"
    assert "GT OK" in stdout, f"GT comparison failed: {stdout}"
    assert "LT OK" in stdout, f"LT comparison failed: {stdout}"
    assert "EQ OK" in stdout, f"EQ comparison failed: {stdout}"
    assert "NE OK" in stdout, f"NE comparison failed: {stdout}"
    assert "GE OK" in stdout, f"GE comparison failed: {stdout}"
    assert "LE OK" in stdout, f"LE comparison failed: {stdout}"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
