"""End-to-end tests for the uada80 compiler."""

import pytest
import tempfile
from pathlib import Path

from uada80.compiler import (
    Compiler,
    CompilationResult,
    OutputFormat,
    compile_source,
    compile_file,
)


# ============================================================================
# Basic Compilation Tests
# ============================================================================


def test_compile_empty_procedure():
    """Test compiling an empty procedure."""
    source = "procedure Main is begin null; end Main;"

    result = compile_source(source)

    assert result.success
    assert not result.has_errors
    assert "Main:" in result.output


def test_compile_hello_world():
    """Test compiling a simple program."""
    source = """
    procedure Hello is
    begin
        null;
    end Hello;
    """

    result = compile_source(source)

    assert result.success
    assert "Hello:" in result.output


def test_compile_function_returns_integer():
    """Test compiling a function that returns an integer."""
    source = """
    function Get_Answer return Integer is
    begin
        return 42;
    end Get_Answer;
    """

    result = compile_source(source)

    assert result.success
    assert "Get_Answer:" in result.output
    assert "ret" in result.output.lower()


def test_compile_with_local_variables():
    """Test compiling with local variable declarations."""
    source = """
    function Calculate return Integer is
        X : Integer := 10;
        Y : Integer := 20;
    begin
        return X + Y;
    end Calculate;
    """

    result = compile_source(source)

    assert result.success
    assert "Calculate:" in result.output


def test_compile_with_if_statement():
    """Test compiling with if statement."""
    source = """
    function Max(A, B : Integer) return Integer is
    begin
        if A > B then
            return A;
        else
            return B;
        end if;
    end Max;
    """

    result = compile_source(source)

    assert result.success
    assert "Max:" in result.output
    assert "jp" in result.output.lower()  # Conditional jump


def test_compile_with_loop():
    """Test compiling with a loop."""
    source = """
    function Sum_To_N(N : Integer) return Integer is
        Total : Integer := 0;
        I : Integer := 1;
    begin
        loop
            exit when I > N;
            Total := Total + I;
            I := I + 1;
        end loop;
        return Total;
    end Sum_To_N;
    """

    result = compile_source(source)

    assert result.success
    assert "Sum_To_N:" in result.output


def test_compile_with_while_loop():
    """Test compiling with while loop."""
    source = """
    function Count_Down(N : Integer) return Integer is
        Count : Integer := N;
    begin
        while Count > 0 loop
            Count := Count - 1;
        end loop;
        return Count;
    end Count_Down;
    """

    result = compile_source(source)

    assert result.success
    assert "Count_Down:" in result.output


def test_compile_with_for_loop():
    """Test compiling with for loop."""
    source = """
    function Sum_1_To_10 return Integer is
        Total : Integer := 0;
    begin
        for I in 1 .. 10 loop
            Total := Total + I;
        end loop;
        return Total;
    end Sum_1_To_10;
    """

    result = compile_source(source)

    assert result.success
    assert "Sum_1_To_10:" in result.output


# ============================================================================
# Output Format Tests
# ============================================================================


def test_dump_ast():
    """Test AST dump mode."""
    source = "procedure Test is begin null; end Test;"

    compiler = Compiler(output_format=OutputFormat.AST)
    result = compiler.compile(source)

    assert result.success
    assert "AST Dump" in result.output
    assert "CompilationUnit" in result.output


def test_dump_ir():
    """Test IR dump mode."""
    source = "procedure Test is begin null; end Test;"

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    assert "IR Module" in result.output
    assert "Function: Test" in result.output


# ============================================================================
# File Compilation Tests
# ============================================================================


def test_compile_file():
    """Test compiling from file."""
    source = "procedure From_File is begin null; end From_File;"

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".adb", delete=False
    ) as f:
        f.write(source)
        f.flush()
        filepath = Path(f.name)

    try:
        result = compile_file(filepath)
        assert result.success
        assert "From_File:" in result.output
    finally:
        filepath.unlink()


def test_compile_nonexistent_file():
    """Test error handling for nonexistent file."""
    result = compile_file("/nonexistent/path/file.adb")

    assert not result.success
    assert result.has_errors
    assert any("not found" in str(e).lower() for e in result.errors)


# ============================================================================
# Z80 Assembly Output Tests
# ============================================================================


def test_output_has_header():
    """Test that output includes header comments."""
    source = "procedure Test is begin null; end Test;"
    result = compile_source(source)

    assert result.success
    assert "uada80" in result.output
    assert "Z80" in result.output


def test_output_has_code_section():
    """Test that output has code section."""
    source = "procedure Test is begin null; end Test;"
    result = compile_source(source)

    assert result.success
    assert "CSEG" in result.output or ".code" in result.output  # MACRO-80 or generic syntax


def test_function_generates_ret():
    """Test that functions generate ret instruction."""
    source = """
    function Get_Value return Integer is
    begin
        return 1;
    end Get_Value;
    """
    result = compile_source(source)

    assert result.success
    assert "ret" in result.output.lower()


# ============================================================================
# Complex Program Tests
# ============================================================================


def test_multiple_functions():
    """Test compiling multiple functions."""
    source = """
    function Add(A, B : Integer) return Integer is
    begin
        return A + B;
    end Add;

    function Sub(A, B : Integer) return Integer is
    begin
        return A - B;
    end Sub;
    """

    result = compile_source(source)

    assert result.success
    assert "Add:" in result.output
    assert "Sub:" in result.output


def test_nested_if():
    """Test compiling nested if statements."""
    source = """
    function Classify(N : Integer) return Integer is
    begin
        if N > 0 then
            if N > 100 then
                return 2;
            else
                return 1;
            end if;
        else
            return 0;
        end if;
    end Classify;
    """

    result = compile_source(source)

    assert result.success
    assert "Classify:" in result.output


def test_arithmetic_expression():
    """Test compiling arithmetic expressions."""
    source = """
    function Calculate return Integer is
        A : Integer := 10;
        B : Integer := 20;
        C : Integer := 5;
    begin
        return (A + B) * C - A / 2;
    end Calculate;
    """

    result = compile_source(source)

    assert result.success
    assert "Calculate:" in result.output


def test_boolean_expression():
    """Test compiling boolean expressions."""
    source = """
    function Is_Valid(X, Y : Integer) return Boolean is
    begin
        return X > 0 and Y > 0;
    end Is_Valid;
    """

    result = compile_source(source)

    assert result.success
    assert "Is_Valid:" in result.output


def test_procedure_with_params():
    """Test procedure with parameters."""
    source = """
    procedure Process(X : Integer; Y : Integer) is
        Temp : Integer;
    begin
        Temp := X + Y;
    end Process;
    """

    result = compile_source(source)

    assert result.success
    assert "Process:" in result.output


# ============================================================================
# Compiler Instance Tests
# ============================================================================


def test_compiler_instance():
    """Test creating Compiler instance."""
    compiler = Compiler()

    assert compiler.output_format == OutputFormat.ASM
    assert not compiler.debug


def test_compiler_debug_mode():
    """Test compiler with debug mode."""
    compiler = Compiler(debug=True)

    assert compiler.debug


def test_compilation_result_properties():
    """Test CompilationResult properties."""
    source = "procedure Test is begin null; end Test;"
    result = compile_source(source)

    assert result.ast is not None
    assert result.ir is not None
    assert isinstance(result.output, str)
    assert isinstance(result.errors, list)
    assert isinstance(result.warnings, list)


# ============================================================================
# Edge Cases
# ============================================================================


def test_empty_function_body():
    """Test function with only return."""
    source = """
    function Empty return Integer is
    begin
        return 0;
    end Empty;
    """

    result = compile_source(source)

    assert result.success


def test_deeply_nested_expressions():
    """Test deeply nested arithmetic."""
    source = """
    function Deep return Integer is
    begin
        return ((1 + 2) * (3 + 4)) - ((5 - 6) / 2);
    end Deep;
    """

    result = compile_source(source)

    assert result.success


def test_many_local_variables():
    """Test function with many local variables."""
    source = """
    function Many_Locals return Integer is
        A : Integer := 1;
        B : Integer := 2;
        C : Integer := 3;
        D : Integer := 4;
        E : Integer := 5;
    begin
        return A + B + C + D + E;
    end Many_Locals;
    """

    result = compile_source(source)

    assert result.success


# ============================================================================
# Array Tests
# ============================================================================


def test_array_access():
    """Test array element access."""
    source = """
    procedure Test_Array is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        Sum : Integer;
    begin
        Data(1) := 42;
        Sum := Data(1);
    end Test_Array;
    """

    result = compile_source(source)

    assert result.success
    assert "ld (hl)" in result.output  # Store to computed address
    assert "ld e, (hl)" in result.output or "ld d, (hl)" in result.output  # Load from computed address


def test_array_ir_generation():
    """Test that array access generates correct IR."""
    from uada80.compiler import Compiler, OutputFormat

    source = """
    procedure Test_Array is
        type Arr is array (1 .. 5) of Integer;
        Data : Arr;
    begin
        Data(3) := 99;
    end Test_Array;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    assert "lea" in result.output.lower()  # Load effective address
    assert "store" in result.output.lower()  # Store to computed address


def test_multidimensional_array():
    """Test multidimensional array element access."""
    source = """
    procedure Test_Matrix is
        type Matrix is array (1 .. 3, 1 .. 4) of Integer;
        M : Matrix;
        Sum : Integer;
    begin
        M(2, 3) := 42;
        Sum := M(1, 2);
    end Test_Matrix;
    """

    result = compile_source(source)

    assert result.success
    assert "Test_Matrix:" in result.output


def test_multidimensional_array_ir():
    """Test that multidimensional array generates correct offset calculation."""
    from uada80.compiler import Compiler, OutputFormat

    source = """
    procedure Test_3D is
        type Cube is array (0 .. 2, 0 .. 3, 0 .. 4) of Integer;
        C : Cube;
    begin
        C(1, 2, 3) := 99;
    end Test_3D;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # Should have multiple offset calculations for 3D array
    assert "mul" in result.output.lower() or "add" in result.output.lower()


def test_array_aggregate_positional():
    """Test positional array aggregate."""
    source = """
    procedure Test_Agg is
        type Arr is array (1 .. 3) of Integer;
        Data : Arr := (10, 20, 30);
    begin
        null;
    end Test_Agg;
    """

    result = compile_source(source)
    assert result.success


def test_array_aggregate_with_others():
    """Test array aggregate with others clause."""
    source = """
    procedure Test_Others is
        type Arr is array (1 .. 5) of Integer;
        Data : Arr := (others => 0);
    begin
        null;
    end Test_Others;
    """

    result = compile_source(source)
    assert result.success


def test_record_aggregate():
    """Test record aggregate initialization."""
    source = """
    procedure Test_Rec is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
        P : Point := (X => 10, Y => 20);
    begin
        null;
    end Test_Rec;
    """

    result = compile_source(source)
    assert result.success


def test_string_literal():
    """Test string literal handling."""
    source = '''
    procedure Test_String is
        S : String(1..5) := "Hello";
    begin
        null;
    end Test_String;
    '''

    result = compile_source(source)
    assert result.success


def test_string_concatenation():
    """Test string concatenation."""
    source = '''
    procedure Test_Concat is
        A : String(1..5) := "Hello";
        B : String(1..5) := "World";
        C : String(1..10);
    begin
        C := A & B;
    end Test_Concat;
    '''

    result = compile_source(source)
    assert result.success


def test_record_field_access():
    """Test record field access (read and write)."""
    source = """
    procedure Test_Fields is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
        P : Point;
        Sum : Integer;
    begin
        P.X := 10;
        P.Y := 20;
        Sum := P.X + P.Y;
    end Test_Fields;
    """

    result = compile_source(source)
    assert result.success
    assert "Test_Fields:" in result.output


# ============================================================================
# Target Name (@) Tests - Ada 2022
# ============================================================================


def test_target_name_simple():
    """Test target name @ in simple increment."""
    source = """
    procedure Test_Increment is
        Counter : Integer := 10;
    begin
        Counter := @ + 1;
    end Test_Increment;
    """

    result = compile_source(source)
    assert result.success
    assert "Test_Increment:" in result.output


def test_target_name_multiply():
    """Test target name @ in multiplication."""
    source = """
    procedure Test_Double is
        Value : Integer := 5;
    begin
        Value := @ * 2;
    end Test_Double;
    """

    result = compile_source(source)
    assert result.success
    assert "Test_Double:" in result.output


def test_target_name_complex_expr():
    """Test target name @ in complex expression."""
    source = """
    procedure Test_Complex is
        X : Integer := 3;
    begin
        X := (@ + 1) * @;
    end Test_Complex;
    """

    result = compile_source(source)
    assert result.success
    assert "Test_Complex:" in result.output
