"""Tests for the Ada parser."""

import pytest
from uada80.parser import parse, Parser, ParseError
from uada80.lexer import lex
from uada80.ast_nodes import (
    Program, CompilationUnit, SubprogramBody, SubprogramDecl,
    PackageDecl, PackageBody,
    ObjectDecl, TypeDecl, SubtypeDecl, NumberDecl, ExceptionDecl,
    AssignmentStmt, IfStmt, CaseStmt, LoopStmt, ReturnStmt, NullStmt,
    ProcedureCallStmt, BlockStmt, ExitStmt, RaiseStmt,
    Identifier, IntegerLiteral, RealLiteral, StringLiteral, CharacterLiteral,
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


def test_operator_function():
    """Test parsing operator symbol function (operator overloading)."""
    source = '''
    function "+" (Left, Right : Integer) return Integer is
    begin
        return Left + Right;
    end "+";
    '''
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.is_function
    assert unit.spec.name == "+"
    # Left, Right share the same type so they're grouped in one ParameterSpec
    assert len(unit.spec.parameters) == 1
    assert unit.spec.parameters[0].names == ["Left", "Right"]


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


def test_number_declaration():
    """Test parsing number declarations (compile-time constants without type)."""
    source = """
    procedure Test is
        Pi : constant := 3.14159;
        Max_Count : constant := 100;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert len(unit.declarations) == 2

    # First number declaration
    decl1 = unit.declarations[0]
    assert isinstance(decl1, NumberDecl)
    assert decl1.names == ["Pi"]
    assert isinstance(decl1.value, RealLiteral)

    # Second number declaration
    decl2 = unit.declarations[1]
    assert isinstance(decl2, NumberDecl)
    assert decl2.names == ["Max_Count"]
    assert isinstance(decl2.value, IntegerLiteral)


def test_number_declaration_expression():
    """Test parsing number declarations with expressions."""
    source = """
    procedure Test is
        Two_Pi : constant := 2 * 3.14159;
        One_Hundred : constant := 10 * 10;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert len(unit.declarations) == 2

    # Should be number declarations with BinaryExpr values
    decl1 = unit.declarations[0]
    assert isinstance(decl1, NumberDecl)
    assert isinstance(decl1.value, BinaryExpr)

    decl2 = unit.declarations[1]
    assert isinstance(decl2, NumberDecl)
    assert isinstance(decl2.value, BinaryExpr)


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


def test_discriminated_record():
    """Test parsing discriminated record type declaration."""
    source = """
    package Test is
        type Variant(Kind : Integer) is record
            Value : Integer;
        end record;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert len(type_decl.discriminants) == 1
    assert type_decl.discriminants[0].names == ["Kind"]
    assert type_decl.discriminants[0].type_mark.name == "Integer"
    assert isinstance(type_decl.type_def, RecordTypeDef)


def test_discriminated_record_multiple():
    """Test parsing discriminated record with multiple discriminants."""
    source = """
    package Test is
        type Buffer(Size : Positive; Mode : Integer) is record
            Data : Integer;
        end record;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert len(type_decl.discriminants) == 2
    assert type_decl.discriminants[0].names == ["Size"]
    assert type_decl.discriminants[1].names == ["Mode"]


def test_discriminated_record_default_value():
    """Test parsing discriminated record with default value."""
    source = """
    package Test is
        type Buffer(Size : Positive := 100) is record
            Data : Integer;
        end record;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert len(type_decl.discriminants) == 1
    assert type_decl.discriminants[0].names == ["Size"]
    assert isinstance(type_decl.discriminants[0].default_value, IntegerLiteral)
    assert type_decl.discriminants[0].default_value.value == 100


def test_variant_record():
    """Test parsing variant record with case part."""
    source = """
    package Test is
        type Figure(Kind : Integer) is record
            X : Integer;
            case Kind is
                when 0 =>
                    Radius : Integer;
                when 1 =>
                    Width : Integer;
                    Height : Integer;
                when others =>
                    null;
            end case;
        end record;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert len(type_decl.discriminants) == 1
    assert type_decl.discriminants[0].names == ["Kind"]
    assert isinstance(type_decl.type_def, RecordTypeDef)
    assert len(type_decl.type_def.components) == 1  # X
    assert type_decl.type_def.variant_part is not None
    vp = type_decl.type_def.variant_part
    assert vp.discriminant == "Kind"
    assert len(vp.variants) == 3
    # Variant 0 (when 0): Radius
    assert len(vp.variants[0].components) == 1
    # Variant 1 (when 1): Width, Height
    assert len(vp.variants[1].components) == 2
    # Variant 2 (when others): null
    assert len(vp.variants[2].components) == 0


def test_object_renames():
    """Test parsing object renaming declaration."""
    source = """
    package Test is
        X : Integer := 10;
        Y : Integer renames X;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    obj_decl = unit.declarations[1]
    assert isinstance(obj_decl, ObjectDecl)
    assert obj_decl.names == ["Y"]
    assert obj_decl.renames is not None
    assert obj_decl.renames.name == "X"


def test_procedure_renames():
    """Test parsing procedure renaming declaration."""
    source = """
    package Test is
        procedure Bar;
        procedure Foo renames Bar;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    proc_decl = unit.declarations[1]
    assert isinstance(proc_decl, SubprogramDecl)
    assert proc_decl.name == "Foo"
    assert proc_decl.renames is not None
    assert proc_decl.renames.name == "Bar"


def test_package_renames():
    """Test parsing package renaming declaration."""
    source = """
    with Ada.Text_IO;
    package Test is
        package TIO renames Ada.Text_IO;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    pkg_decl = unit.declarations[0]
    assert isinstance(pkg_decl, PackageDecl)
    assert pkg_decl.name == "TIO"
    assert pkg_decl.renames is not None


def test_fixed_point_type():
    """Test parsing fixed point type declaration."""
    source = """
    package Test is
        type Fixed_Point is delta 0.01 range 0.0 .. 100.0;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert not type_decl.type_def.is_floating
    assert type_decl.type_def.delta_expr is not None
    assert type_decl.type_def.range_constraint is not None


def test_floating_point_type():
    """Test parsing floating point type declaration."""
    source = """
    package Test is
        type My_Float is digits 6 range 0.0 .. 1.0;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert type_decl.type_def.is_floating
    assert type_decl.type_def.digits_expr is not None
    assert type_decl.type_def.range_constraint is not None


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


def test_named_parameter_association():
    """Test parsing named parameter associations."""
    source = """
    procedure Test is
    begin
        Put(Item => "Hello", Width => 10);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, ProcedureCallStmt)


def test_mixed_parameter_association():
    """Test parsing mixed positional and named parameters."""
    source = """
    procedure Test is
    begin
        Foo(42, X => 10, Y => 20);
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


def test_use_type_clause():
    """Test parsing use type clause."""
    source = """
    use type Ada.Text_IO.File_Type;

    procedure Test is
    begin
        null;
    end Test;
    """
    program = parse(source)

    use_clause = program.units[0].context_clauses[0]
    assert use_clause.is_type
    assert not use_clause.is_all


def test_use_all_type_clause():
    """Test parsing use all type clause (Ada 2012)."""
    source = """
    use all type Ada.Text_IO.File_Type;

    procedure Test is
    begin
        null;
    end Test;
    """
    program = parse(source)

    use_clause = program.units[0].context_clauses[0]
    assert use_clause.is_type
    assert use_clause.is_all


def test_use_all_type_multiple():
    """Test parsing multiple use all type clauses."""
    source = """
    use all type Pack.Type1, Pack.Type2;

    procedure Test is
    begin
        null;
    end Test;
    """
    program = parse(source)

    use_clause = program.units[0].context_clauses[0]
    assert use_clause.is_type
    assert use_clause.is_all
    assert len(use_clause.names) == 2


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
    # @ is a valid Ada token (AT_SIGN for 'Access), so parser handles this
    # The parser should recover and not crash
    program = parse(source)
    # Parser may have errors but should produce something or empty


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
    # 2 assignments + 1 for loop (loop body is separate)
    assert len(unit.statements) == 3


def test_exception_declaration():
    """Test parsing exception declarations."""
    source = """
    package Test is
        My_Error : exception;
        Error_A, Error_B : exception;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert len(unit.declarations) == 2

    decl1 = unit.declarations[0]
    assert isinstance(decl1, ExceptionDecl)
    assert decl1.names == ["My_Error"]

    decl2 = unit.declarations[1]
    assert isinstance(decl2, ExceptionDecl)
    assert decl2.names == ["Error_A", "Error_B"]


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


# ============================================================================
# Ada 2012 Conditional Expressions Tests
# ============================================================================

def test_conditional_expression_simple():
    """Test parsing simple Ada 2012 conditional expression."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := (if True then 1 else 0);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, AssignmentStmt)
    # The expression should be a conditional expression
    from uada80.ast_nodes import ConditionalExpr
    assert isinstance(stmt.value, ConditionalExpr)


def test_conditional_expression_elsif():
    """Test parsing conditional expression with elsif."""
    source = """
    procedure Test is
        X, Y : Integer;
    begin
        Y := (if X < 0 then -1 elsif X > 0 then 1 else 0);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, AssignmentStmt)
    from uada80.ast_nodes import ConditionalExpr
    cond_expr = stmt.value
    assert isinstance(cond_expr, ConditionalExpr)
    assert len(cond_expr.elsif_parts) == 1


def test_conditional_expression_nested():
    """Test parsing nested conditional expressions."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := (if (if True then False else True) then 1 else 2);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    from uada80.ast_nodes import ConditionalExpr
    assert isinstance(stmt.value, ConditionalExpr)
    assert isinstance(stmt.value.condition, ConditionalExpr)


# ============================================================================
# Ada 2012 Quantified Expressions Tests
# ============================================================================

def test_quantified_expression_for_all():
    """Test parsing 'for all' quantified expression."""
    source = """
    procedure Test is
        Result : Boolean;
    begin
        Result := (for all I in 1 .. 10 => I > 0);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    assert isinstance(stmt, AssignmentStmt)
    from uada80.ast_nodes import QuantifiedExpr
    quant = stmt.value
    assert isinstance(quant, QuantifiedExpr)
    assert quant.is_for_all is True
    assert quant.iterator.name == "I"


def test_quantified_expression_for_some():
    """Test parsing 'for some' quantified expression."""
    source = """
    procedure Test is
        Result : Boolean;
    begin
        Result := (for some X in 1 .. 100 => X = 42);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    stmt = unit.statements[0]
    from uada80.ast_nodes import QuantifiedExpr
    quant = stmt.value
    assert isinstance(quant, QuantifiedExpr)
    assert quant.is_for_all is False
    assert quant.iterator.name == "X"


# ============================================================================
# Operator Overloading Tests
# ============================================================================

def test_operator_overloading_plus():
    """Test parsing operator overloading for + operator."""
    source = '''
    function "+"(Left, Right : Integer) return Integer is
    begin
        return 0;
    end "+";
    '''
    program = parse(source)

    assert len(program.units) == 1
    from uada80.ast_nodes import SubprogramBody
    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.name == "+"
    assert unit.spec.is_function


def test_operator_overloading_equals():
    """Test parsing operator overloading for = operator."""
    source = '''
    function "="(Left, Right : My_Type) return Boolean is
    begin
        return True;
    end "=";
    '''
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import SubprogramBody
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.name == "="


def test_operator_overloading_declaration():
    """Test parsing operator overloading declaration (without body)."""
    source = '''
    package Test is
        type My_Int is range 0 .. 100;
        function "+"(Left, Right : My_Int) return My_Int;
        function "-"(Left, Right : My_Int) return My_Int;
    end Test;
    '''
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import PackageDecl, SubprogramDecl
    assert isinstance(unit, PackageDecl)
    # Find the operator declarations
    plus_found = False
    minus_found = False
    for decl in unit.declarations:
        if isinstance(decl, SubprogramDecl):
            if decl.name == "+":
                plus_found = True
            elif decl.name == "-":
                minus_found = True
    assert plus_found, "Expected '+' operator declaration"
    assert minus_found, "Expected '-' operator declaration"


# ============================================================================
# Goto Statement Tests
# ============================================================================

def test_goto_and_label():
    """Test parsing goto statement and label declaration."""
    source = """
    procedure Test is
        X : Integer;
    begin
        <<My_Label>>
        X := 1;
        goto My_Label;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import LabeledStmt, GotoStmt
    # First statement should be a labeled statement
    stmt1 = unit.statements[0]
    assert isinstance(stmt1, LabeledStmt)
    assert stmt1.label == "My_Label"
    # The labeled statement contains the assignment
    assert isinstance(stmt1.statement, AssignmentStmt)

    # Second statement is goto
    stmt2 = unit.statements[1]
    assert isinstance(stmt2, GotoStmt)
    assert stmt2.label == "My_Label"


def test_multiple_labels():
    """Test parsing multiple labels."""
    source = """
    procedure Test is
    begin
        <<First>>
        null;
        <<Second>>
        null;
        goto First;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import LabeledStmt
    assert isinstance(unit.statements[0], LabeledStmt)
    assert unit.statements[0].label == "First"
    assert isinstance(unit.statements[1], LabeledStmt)
    assert unit.statements[1].label == "Second"


# ============================================================================
# Ada 2012 Expression Function Tests
# ============================================================================

def test_expression_function():
    """Test parsing Ada 2012 expression functions."""
    source = """
    function Double(X : Integer) return Integer is (X * 2);
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.name == "Double"
    assert unit.spec.is_function
    assert len(unit.declarations) == 0
    assert len(unit.statements) == 1
    assert isinstance(unit.statements[0], ReturnStmt)
    assert isinstance(unit.statements[0].value, BinaryExpr)


def test_expression_function_with_condition():
    """Test parsing expression function with conditional expression."""
    source = """
    function Max(A, B : Integer) return Integer is
        (if A > B then A else B);
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.name == "Max"
    assert len(unit.statements) == 1
    assert isinstance(unit.statements[0], ReturnStmt)


def test_expression_function_simple():
    """Test parsing simple expression function."""
    source = """
    function Identity(X : Integer) return Integer is (X);
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    assert unit.spec.name == "Identity"
    assert len(unit.statements) == 1
    ret_stmt = unit.statements[0]
    assert isinstance(ret_stmt, ReturnStmt)
    assert isinstance(ret_stmt.value, Identifier)
    assert ret_stmt.value.name == "X"


# ============================================================================
# Ada 2012 Iterator Tests
# ============================================================================

def test_for_of_iterator():
    """Test parsing Ada 2012 'for X of Array' iterator."""
    source = """
    procedure Test is
        Arr : array (1 .. 5) of Integer;
        Sum : Integer := 0;
    begin
        for X of Arr loop
            Sum := Sum + X;
        end loop;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramBody)
    # Find the loop statement
    loop_stmt = unit.statements[0]
    assert isinstance(loop_stmt, LoopStmt)
    assert loop_stmt.iteration_scheme is not None
    from uada80.ast_nodes import ForScheme, IteratorSpec
    assert isinstance(loop_stmt.iteration_scheme, ForScheme)
    iterator = loop_stmt.iteration_scheme.iterator
    assert isinstance(iterator, IteratorSpec)
    assert iterator.name == "X"
    assert iterator.is_of_iterator == True
    assert isinstance(iterator.iterable, Identifier)
    assert iterator.iterable.name == "Arr"


def test_for_of_iterator_reverse():
    """Test parsing 'for X of reverse Array' iterator."""
    source = """
    procedure Test is
        Arr : array (1 .. 5) of Integer;
    begin
        for X of reverse Arr loop
            null;
        end loop;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    loop_stmt = unit.statements[0]
    from uada80.ast_nodes import ForScheme
    assert isinstance(loop_stmt.iteration_scheme, ForScheme)
    iterator = loop_stmt.iteration_scheme.iterator
    assert iterator.name == "X"
    assert iterator.is_of_iterator == True
    assert iterator.is_reverse == True


def test_case_expression_simple():
    """Test parsing simple case expression."""
    source = """
    procedure Test is
        X : Integer := 1;
        Y : Integer;
    begin
        Y := (case X is
              when 1 => 10,
              when 2 => 20,
              when others => 0);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assign = unit.statements[0]
    from uada80.ast_nodes import CaseExpr
    assert isinstance(assign.value, CaseExpr)
    case_expr = assign.value
    assert len(case_expr.alternatives) == 3


def test_case_expression_ranges():
    """Test parsing case expression with range choices."""
    source = """
    function Grade(Score : Integer) return Character is
    begin
        return (case Score is
                when 90 .. 100 => 'A',
                when 80 .. 89 => 'B',
                when 70 .. 79 => 'C',
                when others => 'F');
    end Grade;
    """
    program = parse(source)

    unit = program.units[0].unit
    ret_stmt = unit.statements[0]
    from uada80.ast_nodes import CaseExpr, RangeChoice
    assert isinstance(ret_stmt.value, CaseExpr)
    case_expr = ret_stmt.value
    assert len(case_expr.alternatives) == 4
    # First alternative should have a range choice
    first_alt = case_expr.alternatives[0]
    assert isinstance(first_alt.choices[0], RangeChoice)


def test_case_expression_in_expression_function():
    """Test parsing case expression inside expression function."""
    source = """
    function Sign(X : Integer) return Integer is
        (case X is
         when 0 => 0,
         when others => (if X > 0 then 1 else -1));
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import CaseExpr, ReturnStmt
    # Expression function body contains a return statement
    ret_stmt = unit.statements[0]
    assert isinstance(ret_stmt, ReturnStmt)
    assert isinstance(ret_stmt.value, CaseExpr)


def test_membership_test_simple():
    """Test parsing simple membership test (X in Type)."""
    source = """
    procedure Test is
        X : Integer := 5;
        B : Boolean;
    begin
        B := X in 1 .. 10;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assign = unit.statements[0]
    from uada80.ast_nodes import MembershipTest, RangeChoice
    assert isinstance(assign.value, MembershipTest)
    assert assign.value.is_not == False
    assert len(assign.value.choices) == 1
    assert isinstance(assign.value.choices[0], RangeChoice)


def test_membership_test_multiple_choices():
    """Test parsing membership test with multiple choices (X in A | B | C)."""
    source = """
    procedure Test is
        X : Integer := 5;
        B : Boolean;
    begin
        B := X in 1 | 2 | 3 | 10 .. 20;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assign = unit.statements[0]
    from uada80.ast_nodes import MembershipTest, ExprChoice, RangeChoice
    assert isinstance(assign.value, MembershipTest)
    assert assign.value.is_not == False
    assert len(assign.value.choices) == 4
    # First three are expression choices
    assert isinstance(assign.value.choices[0], ExprChoice)
    assert isinstance(assign.value.choices[1], ExprChoice)
    assert isinstance(assign.value.choices[2], ExprChoice)
    # Last one is a range choice
    assert isinstance(assign.value.choices[3], RangeChoice)


def test_membership_test_not_in():
    """Test parsing 'not in' membership test."""
    source = """
    procedure Test is
        Ch : Character := 'A';
        B : Boolean;
    begin
        B := Ch not in 'a' | 'e' | 'i' | 'o' | 'u';
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    assign = unit.statements[0]
    from uada80.ast_nodes import MembershipTest
    assert isinstance(assign.value, MembershipTest)
    assert assign.value.is_not == True
    assert len(assign.value.choices) == 5


def test_null_exclusion_access_type():
    """Test parsing 'not null access' type definition."""
    source = """
    procedure Test is
        type Ptr is not null access Integer;
        X : Ptr;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import TypeDecl, AccessTypeDef
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert isinstance(type_decl.type_def, AccessTypeDef)
    assert type_decl.type_def.is_not_null == True


def test_null_exclusion_access_all():
    """Test parsing 'not null access all' type definition."""
    source = """
    procedure Test is
        type Ptr is not null access all Integer;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import TypeDecl, AccessTypeDef
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert isinstance(type_decl.type_def, AccessTypeDef)
    assert type_decl.type_def.is_not_null == True
    assert type_decl.type_def.is_access_all == True


# ============================================================================
# Ada 2012 Aspect Specification Tests
# ============================================================================

def test_aspect_function_inline():
    """Test parsing function with Inline aspect."""
    source = """
    procedure Test is
        function Double(X : Integer) return Integer with Inline is
        begin
            return X * 2;
        end Double;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    func = unit.declarations[0]
    assert isinstance(func, SubprogramBody)
    assert func.spec.name == "Double"
    assert len(func.spec.aspects) == 1
    assert func.spec.aspects[0].name == "Inline"
    assert func.spec.aspects[0].value is None  # Boolean aspect


def test_aspect_function_with_value():
    """Test parsing function with aspect that has a value."""
    source = """
    procedure Test is
        function Sqrt(X : Float) return Float
            with Pre => X >= 0.0, Post => Sqrt'Result >= 0.0 is
        begin
            return X;
        end Sqrt;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    func = unit.declarations[0]
    assert isinstance(func, SubprogramBody)
    assert func.spec.name == "Sqrt"
    assert len(func.spec.aspects) == 2
    assert func.spec.aspects[0].name == "Pre"
    assert func.spec.aspects[0].value is not None
    assert func.spec.aspects[1].name == "Post"
    assert func.spec.aspects[1].value is not None


def test_aspect_type_declaration():
    """Test parsing type declaration with aspect."""
    source = """
    procedure Test is
        type My_Int is range 1 .. 100 with Size => 8;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import TypeDecl
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert len(type_decl.aspects) == 1
    assert type_decl.aspects[0].name == "Size"
    assert type_decl.aspects[0].value is not None


def test_aspect_object_declaration():
    """Test parsing object declaration with aspect."""
    source = """
    procedure Test is
        X : Integer := 0 with Volatile;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    obj_decl = unit.declarations[0]
    assert isinstance(obj_decl, ObjectDecl)
    assert len(obj_decl.aspects) == 1
    assert obj_decl.aspects[0].name == "Volatile"
    assert obj_decl.aspects[0].value is None  # Boolean aspect


# ============================================================================
# Generic Default Tests
# ============================================================================

def test_generic_object_with_default():
    """Test parsing generic object formal with default value."""
    source = """
    generic
        Size : Positive := 100;
    package Stack is
    end Stack;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import PackageDecl, GenericObjectDecl
    assert isinstance(unit, PackageDecl)
    assert len(unit.generic_formals) == 1
    obj = unit.generic_formals[0]
    assert isinstance(obj, GenericObjectDecl)
    assert obj.name == "Size"
    assert obj.default_value is not None


def test_generic_function_with_default_box():
    """Test parsing generic function formal with box default."""
    source = """
    generic
        type Element is private;
        with function "<" (L, R : Element) return Boolean is <>;
    package Sorting is
    end Sorting;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import PackageDecl, GenericSubprogramDecl
    assert isinstance(unit, PackageDecl)
    func = unit.generic_formals[1]
    assert isinstance(func, GenericSubprogramDecl)
    assert func.name == "<"
    assert func.is_box == True
    assert func.default_subprogram is None


def test_generic_function_with_default_name():
    """Test parsing generic function formal with specific default."""
    source = """
    generic
        with function Transform(X : Integer) return Integer is Identity;
    package Processing is
    end Processing;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import PackageDecl, GenericSubprogramDecl
    assert isinstance(unit, PackageDecl)
    func = unit.generic_formals[0]
    assert isinstance(func, GenericSubprogramDecl)
    assert func.name == "Transform"
    assert func.is_box == False
    assert func.default_subprogram is not None
    assert isinstance(func.default_subprogram, Identifier)
    assert func.default_subprogram.name == "Identity"


# ============================================================================
# Ada 2012 Type Invariants and Subtype Predicates
# ============================================================================

def test_type_invariant():
    """Test parsing type with Type_Invariant aspect."""
    source = """
    procedure Test is
        type Positive_Int is new Integer
            with Type_Invariant => Positive_Int > 0;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import TypeDecl
    type_decl = unit.declarations[0]
    assert isinstance(type_decl, TypeDecl)
    assert len(type_decl.aspects) == 1
    assert type_decl.aspects[0].name == "Type_Invariant"
    assert type_decl.aspects[0].value is not None


def test_subtype_static_predicate():
    """Test parsing subtype with Static_Predicate aspect."""
    source = """
    procedure Test is
        subtype Small_Int is Integer range 1 .. 10
            with Static_Predicate => Small_Int /= 5;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import SubtypeDecl
    subtype_decl = unit.declarations[0]
    assert isinstance(subtype_decl, SubtypeDecl)
    assert len(subtype_decl.aspects) == 1
    assert subtype_decl.aspects[0].name == "Static_Predicate"
    assert subtype_decl.aspects[0].value is not None


def test_subtype_dynamic_predicate():
    """Test parsing subtype with Dynamic_Predicate aspect."""
    source = """
    procedure Test is
        subtype Even is Integer
            with Dynamic_Predicate => Even mod 2 = 0;
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import SubtypeDecl
    subtype_decl = unit.declarations[0]
    assert isinstance(subtype_decl, SubtypeDecl)
    assert len(subtype_decl.aspects) == 1
    assert subtype_decl.aspects[0].name == "Dynamic_Predicate"
    assert subtype_decl.aspects[0].value is not None


# ============================================================================
# Extended Return Statement Tests (Ada 2005)
# ============================================================================

def test_extended_return_simple():
    """Test parsing simple extended return statement."""
    source = """
    function Create return Integer is
    begin
        return Result : Integer := 42;
    end Create;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ExtendedReturnStmt
    ret_stmt = unit.statements[0]
    assert isinstance(ret_stmt, ExtendedReturnStmt)
    assert ret_stmt.object_name == "Result"
    assert ret_stmt.type_mark is not None
    assert ret_stmt.init_expr is not None
    assert len(ret_stmt.statements) == 0


def test_extended_return_with_do():
    """Test parsing extended return with do block."""
    source = """
    function Create return Integer is
    begin
        return Result : Integer do
            Result := 42;
        end return;
    end Create;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ExtendedReturnStmt
    ret_stmt = unit.statements[0]
    assert isinstance(ret_stmt, ExtendedReturnStmt)
    assert ret_stmt.object_name == "Result"
    assert len(ret_stmt.statements) == 1


def test_extended_return_with_init_and_do():
    """Test parsing extended return with initialization and do block."""
    source = """
    function Create return Integer is
    begin
        return Result : Integer := 10 do
            Result := Result + 5;
        end return;
    end Create;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ExtendedReturnStmt
    ret_stmt = unit.statements[0]
    assert isinstance(ret_stmt, ExtendedReturnStmt)
    assert ret_stmt.object_name == "Result"
    assert ret_stmt.init_expr is not None
    assert len(ret_stmt.statements) == 1


# ============================================================================
# Null Procedure Tests (Ada 2005)
# ============================================================================

def test_null_procedure_simple():
    """Test parsing simple null procedure."""
    source = """
    procedure Do_Nothing is null;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramDecl)
    assert unit.name == "Do_Nothing"
    assert unit.is_null == True
    assert unit.is_function == False


def test_null_procedure_with_parameters():
    """Test parsing null procedure with parameters."""
    source = """
    procedure Ignore(X : Integer) is null;
    """
    program = parse(source)

    unit = program.units[0].unit
    assert isinstance(unit, SubprogramDecl)
    assert unit.name == "Ignore"
    assert unit.is_null == True
    assert len(unit.parameters) == 1


def test_null_procedure_in_package():
    """Test parsing null procedure inside a package."""
    source = """
    package Test is
        procedure Handler is null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import PackageDecl
    assert isinstance(unit, PackageDecl)
    proc = unit.declarations[0]
    assert isinstance(proc, SubprogramDecl)
    assert proc.is_null == True


# ============================================================================
# Target Name Tests (Ada 2022)
# ============================================================================

def test_target_name_simple_assignment():
    """Test parsing target name @ in simple assignment."""
    source = """
    procedure Test is
        X : Integer := 0;
    begin
        X := @ + 1;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import TargetName, BinaryExpr, AssignmentStmt
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    # RHS should be BinaryExpr with TargetName as left operand
    assert isinstance(assign.value, BinaryExpr)
    assert isinstance(assign.value.left, TargetName)


def test_target_name_complex_expression():
    """Test parsing target name @ in complex expression."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        X := @ * 2 + 1;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import TargetName, BinaryExpr, AssignmentStmt
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    # Expression uses @
    assert isinstance(assign.value, BinaryExpr)


def test_target_name_in_unary_expr():
    """Test parsing target name @ with unary operator."""
    source = """
    procedure Test is
        X : Integer := 5;
    begin
        X := -@;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import TargetName, AssignmentStmt, UnaryExpr
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    # RHS is unary expr with @ as operand
    assert isinstance(assign.value, UnaryExpr)
    assert isinstance(assign.value.operand, TargetName)


# ============================================================================
# Delta Aggregate Tests (Ada 2022)
# ============================================================================

def test_delta_aggregate_record():
    """Test parsing record delta aggregate."""
    source = """
    procedure Test is
        type Point is record
            X, Y : Integer;
        end record;
        P : Point := (X => 0, Y => 0);
    begin
        P := (P with delta X => 10);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import DeltaAggregate, AssignmentStmt, Identifier
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    assert isinstance(assign.value, DeltaAggregate)
    assert isinstance(assign.value.base_expression, Identifier)
    assert assign.value.base_expression.name == "P"
    assert len(assign.value.components) == 1


def test_delta_aggregate_multiple_fields():
    """Test parsing delta aggregate with multiple fields."""
    source = """
    procedure Test is
        type Rec is record
            A, B, C : Integer;
        end record;
        R : Rec := (A => 1, B => 2, C => 3);
    begin
        R := (R with delta A => 10, C => 30);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import DeltaAggregate, AssignmentStmt
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    assert isinstance(assign.value, DeltaAggregate)
    assert len(assign.value.components) == 2


def test_delta_aggregate_nested_expression():
    """Test parsing delta aggregate with function call as base."""
    source = """
    procedure Test is
        type Point is record
            X, Y : Integer;
        end record;
    begin
        Process((Get_Point with delta Y => 100));
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import DeltaAggregate, ProcedureCallStmt, FunctionCall
    call = unit.statements[0]
    assert isinstance(call, ProcedureCallStmt)
    delta_agg = call.args[0].value
    assert isinstance(delta_agg, DeltaAggregate)
    assert len(delta_agg.components) == 1


# ============================================================================
# Iterated Component Association Tests (Ada 2012)
# ============================================================================

def test_iterated_component_in_range():
    """Test parsing iterated component with 'in' and range."""
    source = """
    procedure Test is
        type Vector is array (1 .. 10) of Integer;
        A : Vector := (for I in 1 .. 10 => I * 2);
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import IteratedComponentAssociation, Aggregate
    decl = unit.declarations[1]
    agg = decl.init_expr
    assert isinstance(agg, Aggregate)
    assert len(agg.components) == 1
    iter_comp = agg.components[0]
    assert isinstance(iter_comp, IteratedComponentAssociation)
    assert iter_comp.loop_parameter == "I"
    assert iter_comp.is_of_form == False


def test_iterated_component_of_array():
    """Test parsing iterated component with 'of' and array."""
    source = """
    procedure Test is
        type Vector is array (1 .. 5) of Integer;
        Src : Vector := (1, 2, 3, 4, 5);
        Dst : Vector := (for X of Src => X + 1);
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import IteratedComponentAssociation, Aggregate
    decl = unit.declarations[2]
    agg = decl.init_expr
    assert isinstance(agg, Aggregate)
    iter_comp = agg.components[0]
    assert isinstance(iter_comp, IteratedComponentAssociation)
    assert iter_comp.loop_parameter == "X"
    assert iter_comp.is_of_form == True


def test_iterated_component_with_subtype():
    """Test parsing iterated component with subtype name."""
    source = """
    procedure Test is
        type Index is range 1 .. 100;
        type Vector is array (Index) of Integer;
        A : Vector := (for I in Index => 0);
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import IteratedComponentAssociation, Aggregate
    decl = unit.declarations[2]
    agg = decl.init_expr
    assert isinstance(agg, Aggregate)
    iter_comp = agg.components[0]
    assert isinstance(iter_comp, IteratedComponentAssociation)
    assert iter_comp.loop_parameter == "I"


# ============================================================================
# Declare Expression Tests (Ada 2022)
# ============================================================================

def test_declare_expr_simple():
    """Test parsing simple declare expression."""
    source = """
    procedure Test is
        X : Integer;
    begin
        X := (declare Y : Integer := 5; begin Y + 1);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import DeclareExpr, AssignmentStmt
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    assert isinstance(assign.value, DeclareExpr)
    assert len(assign.value.declarations) == 1


def test_declare_expr_multiple_decls():
    """Test parsing declare expression with multiple declarations."""
    source = """
    procedure Test is
        Result : Integer;
    begin
        Result := (declare
            A : Integer := 10;
            B : Integer := 20;
        begin
            A + B);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import DeclareExpr, AssignmentStmt
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    assert isinstance(assign.value, DeclareExpr)
    assert len(assign.value.declarations) == 2


def test_declare_expr_in_condition():
    """Test parsing declare expression as condition."""
    source = """
    procedure Test is
    begin
        if (declare X : Boolean := True; begin X) then
            null;
        end if;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import DeclareExpr, IfStmt
    if_stmt = unit.statements[0]
    assert isinstance(if_stmt, IfStmt)
    assert isinstance(if_stmt.condition, DeclareExpr)


# ============================================================================
# Parallel Constructs Tests (Ada 2022)
# ============================================================================

def test_parallel_for_loop():
    """Test parsing parallel for loop."""
    source = """
    procedure Test is
        type Vector is array (1 .. 100) of Integer;
        A : Vector;
    begin
        parallel for I in 1 .. 100 loop
            A(I) := I * 2;
        end loop;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import LoopStmt
    loop = unit.statements[0]
    assert isinstance(loop, LoopStmt)
    assert loop.is_parallel == True


def test_parallel_block_two_sequences():
    """Test parsing parallel block with two sequences."""
    source = """
    procedure Test is
        X, Y : Integer := 0;
    begin
        parallel
        do
            X := 1;
        and
        do
            Y := 2;
        end parallel;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ParallelBlockStmt
    parallel = unit.statements[0]
    assert isinstance(parallel, ParallelBlockStmt)
    assert len(parallel.sequences) == 2


def test_parallel_block_three_sequences():
    """Test parsing parallel block with three sequences."""
    source = """
    procedure Test is
        X, Y, Z : Integer := 0;
    begin
        parallel
        do
            X := Compute_X;
        and
        do
            Y := Compute_Y;
        and
        do
            Z := Compute_Z;
        end parallel;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ParallelBlockStmt
    parallel = unit.statements[0]
    assert isinstance(parallel, ParallelBlockStmt)
    assert len(parallel.sequences) == 3


# ============================================================================
# Reduction Expression Tests (Ada 2022)
# ============================================================================

def test_container_aggregate_reduce():
    """Test container aggregate with 'Reduce attribute."""
    source = """
    procedure Test is
        Sum : Integer;
    begin
        Sum := [for I in 1 .. 10 => I]'Reduce("+", 0);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import AssignmentStmt, AttributeReference, ContainerAggregate
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    assert isinstance(assign.value, AttributeReference)
    assert assign.value.attribute.lower() == "reduce"
    assert isinstance(assign.value.prefix, ContainerAggregate)
    assert len(assign.value.args) == 2


def test_array_variable_reduce():
    """Test array variable with 'Reduce attribute."""
    source = """
    procedure Test is
        type Vector is array (1 .. 10) of Integer;
        A : Vector;
        Sum : Integer;
    begin
        Sum := A'Reduce("+", 0);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import AssignmentStmt, AttributeReference, Identifier
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    assert isinstance(assign.value, AttributeReference)
    assert assign.value.attribute.lower() == "reduce"
    assert isinstance(assign.value.prefix, Identifier)
    assert len(assign.value.args) == 2


def test_positional_aggregate_reduce():
    """Test positional aggregate with 'Reduce attribute."""
    source = """
    procedure Test is
        Sum : Integer;
    begin
        Sum := (1, 2, 3, 4, 5)'Reduce("+", 0);
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import AssignmentStmt, AttributeReference, Aggregate
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    assert isinstance(assign.value, AttributeReference)
    assert assign.value.attribute.lower() == "reduce"
    assert isinstance(assign.value.prefix, Aggregate)
    assert len(assign.value.args) == 2


# ============================================================================
# Raise Expression Tests (Ada 2012)
# ============================================================================

def test_raise_expression_simple():
    """Test parsing simple raise expression."""
    source = """
    function Safe_Divide (X, Y : Integer) return Integer is
    begin
        return (if Y /= 0 then X / Y else raise Constraint_Error);
    end Safe_Divide;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ReturnStmt, ConditionalExpr, RaiseExpr
    ret_stmt = unit.statements[0]
    assert isinstance(ret_stmt, ReturnStmt)
    assert isinstance(ret_stmt.value, ConditionalExpr)
    assert isinstance(ret_stmt.value.else_expr, RaiseExpr)
    assert ret_stmt.value.else_expr.message is None


def test_raise_expression_with_message():
    """Test parsing raise expression with message."""
    source = """
    function Get_Value (X : Integer) return Integer is
    begin
        return (if X > 0 then X else raise Constraint_Error with "X must be positive");
    end Get_Value;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ReturnStmt, ConditionalExpr, RaiseExpr, StringLiteral
    ret_stmt = unit.statements[0]
    assert isinstance(ret_stmt, ReturnStmt)
    assert isinstance(ret_stmt.value, ConditionalExpr)
    assert isinstance(ret_stmt.value.else_expr, RaiseExpr)
    assert isinstance(ret_stmt.value.else_expr.message, StringLiteral)


def test_raise_expression_in_assignment():
    """Test parsing raise expression in assignment."""
    source = """
    procedure Test is
        X : Integer := 0;
        Y : Integer;
    begin
        Y := (if X > 0 then X else raise Program_Error with "Invalid X");
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import AssignmentStmt, ConditionalExpr, RaiseExpr
    assign = unit.statements[0]
    assert isinstance(assign, AssignmentStmt)
    assert isinstance(assign.value, ConditionalExpr)
    assert isinstance(assign.value.else_expr, RaiseExpr)


# ============================================================================
# Aggregate 'others' Tests
# ============================================================================

def test_aggregate_others_only():
    """Test aggregate with only 'others' clause."""
    source = """
    procedure Test is
        type Vector is array (1 .. 10) of Integer;
        V : Vector := (others => 0);
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ObjectDecl, Aggregate, OthersChoice
    v_decl = unit.declarations[1]
    assert isinstance(v_decl, ObjectDecl)
    assert isinstance(v_decl.init_expr, Aggregate)
    assert len(v_decl.init_expr.components) == 1
    assert isinstance(v_decl.init_expr.components[0].choices[0], OthersChoice)


def test_aggregate_mixed_with_others():
    """Test aggregate with named components and 'others'."""
    source = """
    procedure Test is
        type Vector is array (1 .. 5) of Integer;
        V : Vector := (1 => 100, 2 => 200, others => 0);
    begin
        null;
    end Test;
    """
    program = parse(source)

    unit = program.units[0].unit
    from uada80.ast_nodes import ObjectDecl, Aggregate, OthersChoice, ExprChoice
    v_decl = unit.declarations[1]
    assert isinstance(v_decl, ObjectDecl)
    assert isinstance(v_decl.init_expr, Aggregate)
    assert len(v_decl.init_expr.components) == 3
    assert isinstance(v_decl.init_expr.components[2].choices[0], OthersChoice)
