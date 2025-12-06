"""Tests for the Ada parser."""

import pytest
from uada80.parser import parse, Parser, ParseError
from uada80.lexer import lex
from uada80.ast_nodes import (
    Program, CompilationUnit, SubprogramBody, SubprogramDecl,
    PackageDecl, PackageBody,
    ObjectDecl, TypeDecl, SubtypeDecl,
    AssignmentStmt, IfStmt, CaseStmt, LoopStmt, ReturnStmt, NullStmt,
    ProcedureCallStmt, BlockStmt, ExitStmt, RaiseStmt,
    Identifier, IntegerLiteral, StringLiteral, CharacterLiteral,
    BinaryExpr, UnaryExpr, BinaryOp, UnaryOp,
    IndexedComponent, SelectedName, AttributeReference,
    IntegerTypeDef, EnumerationTypeDef, ArrayTypeDef, RecordTypeDef,
    RangeExpr, Aggregate,
)


# ============================================================================
# Simple Procedure Tests
# ============================================================================

def test_empty_procedure():
    """Test parsing an empty procedure."""
    source = """
    procedure Empty is
    begin
        null;
    end Empty;
    """
    program = parse(source)

    assert len(program.units) == 1
    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.name == "Empty"
    assert not unit.spec.is_function
    assert len(unit.statements) == 1
    assert isinstance(unit.statements[0], NullStmt)


def test_procedure_with_parameters():
    """Test parsing procedure with parameters."""
    source = """
    procedure Add(X : in Integer; Y : in Integer; Result : out Integer) is
    begin
        Result := X + Y;
    end Add;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.name == "Add"
    assert len(unit.spec.parameters) == 3

    # Check parameters
    assert unit.spec.parameters[0].names == ["X"]
    assert unit.spec.parameters[0].mode == "in"
    assert unit.spec.parameters[1].names == ["Y"]
    assert unit.spec.parameters[2].names == ["Result"]
    assert unit.spec.parameters[2].mode == "out"


def test_function():
    """Test parsing a function."""
    source = """
    function Square(N : Integer) return Integer is
    begin
        return N * N;
    end Square;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.is_function
    assert unit.spec.name == "Square"
    assert unit.spec.return_type is not None

    # Check return statement
    assert len(unit.statements) == 1
    assert isinstance(unit.statements[0], ReturnStmt)


# ============================================================================
# Variable Declaration Tests
# ============================================================================

def test_variable_declarations():
    """Test parsing variable declarations."""
    source = """
    procedure Test is
        X : Integer;
        Y : Integer := 42;
        Z : constant Integer := 100;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert len(unit.declarations) == 3

    # First declaration: X : Integer;
    decl1 = unit.declarations[0]
    assert isinstance(decl1, ObjectDecl)
    assert decl1.names == ["X"]
    assert not decl1.is_constant
    assert decl1.init_expr is None

    # Second declaration with initialization
    decl2 = unit.declarations[1]
    assert decl2.names == ["Y"]
    assert decl2.init_expr is not None

    # Third: constant
    decl3 = unit.declarations[2]
    assert decl3.is_constant


def test_multiple_variable_declaration():
    """Test parsing declaration with multiple names."""
    source = """
    procedure Test is
        A, B, C : Integer;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    decl = unit.declarations[0]
    assert isinstance(decl, ObjectDecl)
    assert decl.names == ["A", "B", "C"]


# ============================================================================
# Type Declaration Tests
# ============================================================================

def test_integer_type():
    """Test parsing integer type declaration."""
    source = """
    procedure Test is
        type Small is range 0 .. 255;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert type_decl.name == "Small"
    assert isinstance(type_decl.type_def, IntegerTypeDef)


def test_enumeration_type():
    """Test parsing enumeration type declaration."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert type_decl.name == "Color"
    assert isinstance(type_decl.type_def, EnumerationTypeDef)
    assert type_decl.type_def.literals == ["Red", "Green", "Blue"]


def test_array_type():
    """Test parsing array type declaration."""
    source = """
    procedure Test is
        type Vector is array (1 .. 10) of Integer;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert isinstance(type_decl.type_def, ArrayTypeDef)


def test_record_type():
    """Test parsing record type declaration."""
    source = """
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert isinstance(type_decl.type_def, RecordTypeDef)
    assert len(type_decl.type_def.components) == 2


def test_subtype_declaration():
    """Test parsing subtype declaration."""
    source = """
    procedure Test is
        subtype Positive is Integer range 1 .. Integer'Last;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    subtype_decl = unit.declarations[0]
    assert isinstance(subtype_decl, SubtypeDecl)
    assert subtype_decl.name == "Positive"


# ============================================================================
# Expression Tests
# ============================================================================

def test_arithmetic_expressions():
    """Test parsing arithmetic expressions."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := 1 + 2 * 3;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, AssignmentStmt)

    # Should be: 1 + (2 * 3) due to precedence
    expr = stmt.value
    assert isinstance(expr, BinaryExpr)
    assert expr.op == BinaryOp.ADD


def test_comparison_expressions():
    """Test parsing comparison expressions."""
    source = """
    procedure Test is
        X : Integer := 10;
        B : Boolean;
    begin
        B := X > 5;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, AssignmentStmt)
    assert isinstance(stmt.value, BinaryExpr)
    assert stmt.value.op == BinaryOp.GT


def test_logical_expressions():
    """Test parsing logical expressions."""
    source = """
    procedure Test is
        A, B : Boolean;
    begin
        A := B and then True;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    expr = stmt.value
    assert isinstance(expr, BinaryExpr)
    assert expr.op == BinaryOp.AND_THEN


def test_unary_expressions():
    """Test parsing unary expressions."""
    source = """
    procedure Test is
        X : Integer;
        B : Boolean;
    begin
        X := -42;
        X := abs X;
        B := not True;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit

    # -42
    stmt1 = unit.statements[0]
    assert isinstance(stmt1.value, UnaryExpr)
    assert stmt1.value.op == UnaryOp.MINUS

    # abs X
    stmt2 = unit.statements[1]
    assert isinstance(stmt2.value, UnaryExpr)
    assert stmt2.value.op == UnaryOp.ABS

    # not True
    stmt3 = unit.statements[2]
    assert isinstance(stmt3.value, UnaryExpr)
    assert stmt3.value.op == UnaryOp.NOT


def test_exponentiation():
    """Test parsing exponentiation."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := 2 ** 10;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt.value, BinaryExpr)
    assert stmt.value.op == BinaryOp.EXP


def test_array_indexing():
    """Test parsing array indexing."""
    source = """
    procedure Test is
        A : array (1 .. 10) of Integer;
        X : Integer;
    begin
        X := A(5);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt.value, IndexedComponent)
    assert len(stmt.value.indices) == 1


def test_selected_component():
    """Test parsing selected component (record field or package item)."""
    source = """
    procedure Test is
        P : Point;
    begin
        P.X := 10;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt.target, SelectedName)
    assert stmt.target.selector == "X"


def test_attribute_reference():
    """Test parsing attribute references."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := Integer'First;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt.value, AttributeReference)
    assert stmt.value.attribute == "First"


# ============================================================================
# Statement Tests
# ============================================================================

def test_if_statement():
    """Test parsing if statement."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        if X > 0 then
            X := X - 1;
        end if;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, IfStmt)
    assert len(stmt.then_stmts) == 1


def test_if_elsif_else():
    """Test parsing if with elsif and else."""
    source = """
    procedure Test is
        X : Integer := 0;
    begin
        if X > 0 then
            X := 1;
        elsif X < 0 then
            X := -1;
        else
            X := 0;
        end if;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, IfStmt)
    assert len(stmt.elsif_parts) == 1
    assert len(stmt.else_stmts) == 1


def test_case_statement():
    """Test parsing case statement."""
    source = """
    procedure Test is
        X : Integer := 1;
    begin
        case X is
            when 1 =>
                null;
            when 2 | 3 =>
                null;
            when others =>
                null;
        end case;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, CaseStmt)
    assert len(stmt.alternatives) == 3


def test_loop_statement():
    """Test parsing simple loop."""
    source = """
    procedure Test is
    begin
        loop
            exit;
        end loop;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, LoopStmt)
    assert stmt.iteration_scheme is None


def test_while_loop():
    """Test parsing while loop."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        while X > 0 loop
            X := X - 1;
        end loop;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, LoopStmt)
    assert stmt.iteration_scheme is not None


def test_for_loop():
    """Test parsing for loop."""
    source = """
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in 1 .. 10 loop
            Sum := Sum + I;
        end loop;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, LoopStmt)


def test_for_loop_reverse():
    """Test parsing for loop with reverse."""
    source = """
    procedure Test is
    begin
        for I in reverse 1 .. 10 loop
            null;
        end loop;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, LoopStmt)


def test_exit_statement():
    """Test parsing exit statement."""
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
    program = parse(source)

    unit = program.units[0].unit
    loop_stmt = unit.statements[0]
    exit_stmt = loop_stmt.statements[1]
    assert isinstance(exit_stmt, ExitStmt)
    assert exit_stmt.condition is not None


def test_block_statement():
    """Test parsing block statement."""
    source = """
    procedure Test is
    begin
        declare
            Temp : Integer;
        begin
            Temp := 42;
        end;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, BlockStmt)
    assert len(stmt.declarations) == 1


def test_procedure_call():
    """Test parsing procedure call."""
    source = """
    procedure Test is
    begin
        Put_Line("Hello");
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, ProcedureCallStmt)


# ============================================================================
# Package Tests
# ============================================================================

def test_package_specification():
    """Test parsing package specification."""
    source = """
    package Math is
        function Add(X, Y : Integer) return Integer;
        function Multiply(X, Y : Integer) return Integer;
    end Math;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, PackageDecl)
    assert unit.name == "Math"
    assert len(unit.declarations) == 2


def test_package_body():
    """Test parsing package body."""
    source = """
    package body Math is
        function Add(X, Y : Integer) return Integer is
        begin
            return X + Y;
        end Add;
    end Math;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, PackageBody)
    assert unit.name == "Math"


def test_with_clause():
    """Test parsing with clause."""
    source = """
    with Ada.Text_IO;

    procedure Hello is
    begin
        null;
    end Hello;
    """
    program = parse(source)

    assert len(program.units[0].context_clauses) == 1


def test_use_clause():
    """Test parsing use clause."""
    source = """
    with Ada.Text_IO;
    use Ada.Text_IO;

    procedure Hello is
    begin
        Put_Line("Hello");
    end Hello;
    """
    program = parse(source)

    assert len(program.units[0].context_clauses) == 2


# ============================================================================
# Aggregate Tests
# ============================================================================

def test_positional_aggregate():
    """Test parsing positional aggregate."""
    source = """
    procedure Test is
        P : Point := (10, 20);
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    decl = unit.declarations[0]
    assert isinstance(decl.init_expr, Aggregate)


# ============================================================================
# Error Handling Tests
# ============================================================================

def test_missing_semicolon():
    """Test error handling for missing semicolon."""
    source = """
    procedure Test is
    begin
        X := 10
    end Test;
    """
    # Should not crash, should recover
    program = parse(source)
    # Parser may have errors but should produce something


def test_unexpected_token():
    """Test error handling for unexpected token."""
    source = """
    procedure Test is
    begin
        @@@
    end Test;
    """
    # Lexer should catch this
    from uada80.lexer import LexerError
    with pytest.raises(LexerError):
        parse(source)


# ============================================================================
# Complex Program Tests
# ============================================================================

def test_fibonacci():
    """Test parsing fibonacci program."""
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
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.name == "Fibonacci"
    assert len(unit.declarations) == 2
    # 4 assignments + 1 for loop
    assert len(unit.statements) == 4


def test_exception_handler():
    """Test parsing exception handler."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := 10;
    exception
        when others =>
            null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert len(unit.handled_exception_handlers) == 1


def test_raise_statement():
    """Test parsing raise statement."""
    source = """
    procedure Test is
    begin
        raise Constraint_Error;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, RaiseStmt)
