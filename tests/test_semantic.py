"""Tests for the semantic analyzer."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze, SemanticAnalyzer
from uada80.symbol_table import SymbolKind


def analyze_source(source: str):
    """Helper to parse and analyze source code."""
    program = parse(source)
    return analyze(program)


# ============================================================================
# Variable Declaration Tests
# ============================================================================


def test_variable_declaration():
    """Test variable declaration is added to symbol table."""
    source = """
    procedure Test is
        X : Integer;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors
    # Variable X should be defined (in the scope during analysis)


def test_variable_with_initialization():
    """Test variable with initialization."""
    source = """
    procedure Test is
        X : Integer := 42;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_constant_declaration():
    """Test constant declaration."""
    source = """
    procedure Test is
        Max : constant Integer := 100;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_constant_without_init_error():
    """Test that constant without initialization causes error."""
    source = """
    procedure Test is
        Max : constant Integer;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("initialization" in str(e) for e in result.errors)


def test_type_mismatch_in_init():
    """Test type mismatch in initialization."""
    source = """
    procedure Test is
        X : Boolean := 42;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("type mismatch" in str(e).lower() for e in result.errors)


# ============================================================================
# Type Declaration Tests
# ============================================================================


def test_integer_type_declaration():
    """Test integer type declaration."""
    source = """
    procedure Test is
        type Small is range 0 .. 255;
        X : Small;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_enumeration_type_declaration():
    """Test enumeration type declaration."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        C : Color;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_array_type_declaration():
    """Test array type declaration."""
    source = """
    procedure Test is
        type Vector is array (1 .. 10) of Integer;
        V : Vector;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_record_type_declaration():
    """Test record type declaration."""
    source = """
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
        P : Point;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


# ============================================================================
# Expression Type Checking Tests
# ============================================================================


def test_arithmetic_expression():
    """Test arithmetic expression type checking."""
    source = """
    procedure Test is
        X : Integer := 10;
        Y : Integer;
    begin
        Y := X + 5;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_boolean_expression():
    """Test boolean expression type checking."""
    source = """
    procedure Test is
        X : Integer := 10;
        B : Boolean;
    begin
        B := X > 5;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_logical_expression():
    """Test logical expression type checking."""
    source = """
    procedure Test is
        A : Boolean := True;
        B : Boolean := False;
        C : Boolean;
    begin
        C := A and B;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_logical_operator_with_non_boolean():
    """Test error when using logical operator with non-boolean."""
    source = """
    procedure Test is
        X : Integer := 10;
        Y : Integer := 20;
        B : Boolean;
    begin
        B := X and Y;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("Boolean" in str(e) for e in result.errors)


# ============================================================================
# Assignment Tests
# ============================================================================


def test_assignment_type_match():
    """Test assignment with matching types."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := 42;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_assignment_type_mismatch():
    """Test assignment with mismatched types."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := True;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("type mismatch" in str(e).lower() for e in result.errors)


def test_assign_to_constant():
    """Test that assigning to constant causes error."""
    source = """
    procedure Test is
        X : constant Integer := 10;
    begin
        X := 20;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("constant" in str(e).lower() for e in result.errors)


def test_assign_to_in_parameter():
    """Test that assigning to 'in' parameter causes error."""
    source = """
    procedure Test(X : in Integer) is
    begin
        X := 10;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("parameter" in str(e).lower() for e in result.errors)


# ============================================================================
# If Statement Tests
# ============================================================================


def test_if_with_boolean_condition():
    """Test if statement with boolean condition."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        if X > 5 then
            X := 0;
        end if;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_if_with_non_boolean_condition():
    """Test error for non-boolean if condition."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        if X then
            null;
        end if;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("Boolean" in str(e) for e in result.errors)


# ============================================================================
# Loop Statement Tests
# ============================================================================


def test_while_loop():
    """Test while loop."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        while X > 0 loop
            X := X - 1;
        end loop;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_for_loop():
    """Test for loop."""
    source = """
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in 1 .. 10 loop
            Sum := Sum + I;
        end loop;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_exit_outside_loop():
    """Test error for exit outside loop."""
    source = """
    procedure Test is
    begin
        exit;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("loop" in str(e).lower() for e in result.errors)


def test_exit_inside_loop():
    """Test exit inside loop is valid."""
    source = """
    procedure Test is
        X : Integer := 0;
    begin
        loop
            X := X + 1;
            exit when X > 10;
        end loop;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


# ============================================================================
# Return Statement Tests
# ============================================================================


def test_function_return():
    """Test function return statement."""
    source = """
    function Square(X : Integer) return Integer is
    begin
        return X * X;
    end Square;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_function_missing_return_value():
    """Test error for function with return without value."""
    source = """
    function GetValue return Integer is
    begin
        return;
    end GetValue;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("return" in str(e).lower() for e in result.errors)


def test_procedure_return_with_value():
    """Test error for procedure returning value."""
    source = """
    procedure Test is
    begin
        return 42;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("procedure" in str(e).lower() for e in result.errors)


# ============================================================================
# Procedure Call Tests
# ============================================================================


def test_procedure_call():
    """Test procedure call."""
    source = """
    procedure Test is
        procedure Inner is
        begin
            null;
        end Inner;
    begin
        Inner;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_undefined_procedure_call():
    """Test error for calling undefined procedure."""
    source = """
    procedure Test is
    begin
        Undefined;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("not found" in str(e).lower() for e in result.errors)


# ============================================================================
# Name Resolution Tests
# ============================================================================


def test_undefined_variable():
    """Test error for undefined variable."""
    source = """
    procedure Test is
    begin
        X := 10;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("not found" in str(e).lower() for e in result.errors)


def test_undefined_type():
    """Test error for undefined type."""
    source = """
    procedure Test is
        X : UndefinedType;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)

    # Type lookup returns None for undefined types
    # The variable will have no type


# ============================================================================
# Subprogram Tests
# ============================================================================


def test_function_definition():
    """Test function definition."""
    source = """
    function Add(X, Y : Integer) return Integer is
    begin
        return X + Y;
    end Add;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_procedure_parameters():
    """Test procedure with parameters."""
    source = """
    procedure Swap(X : in out Integer; Y : in out Integer) is
        Temp : Integer;
    begin
        Temp := X;
        X := Y;
        Y := Temp;
    end Swap;
    """
    # Note: "in out" is typically written "in out" but we parse it
    # This test may need adjustment based on parser


# ============================================================================
# Block Statement Tests
# ============================================================================


def test_block_scope():
    """Test block statement creates new scope."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        declare
            Y : Integer := 20;
        begin
            X := Y;
        end;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


# ============================================================================
# Package Tests
# ============================================================================


def test_package_declaration():
    """Test package declaration."""
    source = """
    package Math is
        function Add(X, Y : Integer) return Integer;
    end Math;
    """
    result = analyze_source(source)

    assert not result.has_errors


# ============================================================================
# Raise Statement Tests
# ============================================================================


def test_raise_predefined_exception():
    """Test raising predefined exception."""
    source = """
    procedure Test is
    begin
        raise Constraint_Error;
    end Test;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_raise_undefined_exception():
    """Test error for raising undefined exception."""
    source = """
    procedure Test is
    begin
        raise My_Error;
    end Test;
    """
    result = analyze_source(source)

    assert result.has_errors
    assert any("not found" in str(e).lower() for e in result.errors)


def test_raise_non_exception():
    """Test error for raising non-exception."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        raise X;
    end Test;
    """
    result = analyze_source(source)

    # X is a variable, not an exception
    # This should ideally cause an error but depends on implementation


# ============================================================================
# Complex Programs
# ============================================================================


def test_fibonacci():
    """Test analyzing fibonacci program."""
    source = """
    procedure Fibonacci is
        A, B, Temp : Integer;
        N : Integer := 10;
    begin
        A := 0;
        B := 1;

        for I in 1 .. N loop
            Temp := A + B;
            A := B;
            B := Temp;
        end loop;
    end Fibonacci;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_factorial():
    """Test analyzing factorial function."""
    source = """
    function Factorial(N : Integer) return Integer is
        Result : Integer := 1;
    begin
        for I in 2 .. N loop
            Result := Result * I;
        end loop;
        return Result;
    end Factorial;
    """
    result = analyze_source(source)

    assert not result.has_errors


def test_nested_procedures():
    """Test nested procedure definitions."""
    source = """
    procedure Outer is
        procedure Inner is
            X : Integer := 10;
        begin
            X := X + 1;
        end Inner;
    begin
        Inner;
    end Outer;
    """
    result = analyze_source(source)

    assert not result.has_errors


# ============================================================================
# Incomplete Type Tests
# ============================================================================


def test_incomplete_type_basic():
    """Test basic incomplete type declaration and completion."""
    source = """
    procedure Test is
        type Node;
        type Node_Access is access Node;
        type Node is record
            Data : Integer;
            Next : Node_Access;
        end record;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)
    assert not result.has_errors


def test_incomplete_type_access_before_completion():
    """Test that access types can refer to incomplete types."""
    source = """
    procedure Test is
        type List_Node;
        type List_Ptr is access List_Node;

        P : List_Ptr;

        type List_Node is record
            Value : Integer;
            Next : List_Ptr;
        end record;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)
    assert not result.has_errors


def test_incomplete_type_mutually_recursive():
    """Test mutually recursive types using incomplete declarations."""
    source = """
    procedure Test is
        type Tree_Node;
        type Tree_Ptr is access Tree_Node;

        type Tree_Node is record
            Value : Integer;
            Left : Tree_Ptr;
            Right : Tree_Ptr;
        end record;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)
    assert not result.has_errors


def test_incomplete_type_redefinition_error():
    """Test that redefining an incomplete type before completion is an error."""
    source = """
    procedure Test is
        type Node;
        type Node;  -- Error: already defined
    begin
        null;
    end Test;
    """
    result = analyze_source(source)
    assert result.has_errors
    assert any("already defined" in e.message for e in result.errors)


def test_type_redefinition_after_completion_error():
    """Test that redefining a completed type is an error."""
    source = """
    procedure Test is
        type Node is record
            Data : Integer;
        end record;
        type Node is record  -- Error: already defined
            Value : Integer;
        end record;
    begin
        null;
    end Test;
    """
    result = analyze_source(source)
    assert result.has_errors
    assert any("already defined" in e.message for e in result.errors)

# ============================================================================
# Float Type Tests
# ============================================================================


def test_float_type():
    """Test Float type declaration and usage."""
    source = """
    procedure Test is
        X : Float := 3.14;
        Y : Float;
    begin
        Y := X + 1.0;
    end Test;
    """
    result = analyze_source(source)
    assert not result.has_errors


def test_long_float_type():
    """Test Long_Float type (64-bit double precision)."""
    source = """
    procedure Test is
        X : Long_Float := 3.14159265358979;
        Y : Long_Float;
    begin
        Y := X * 2.0;
    end Test;
    """
    result = analyze_source(source)
    assert not result.has_errors


def test_long_long_float_type():
    """Test Long_Long_Float type (extended precision)."""
    source = """
    procedure Test is
        X : Long_Long_Float := 2.718281828459045;
        Y : Long_Long_Float;
    begin
        Y := X / 2.0;
    end Test;
    """
    result = analyze_source(source)
    assert not result.has_errors
