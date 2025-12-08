"""Tests for Ada renaming declarations."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestObjectRenamingParsing:
    """Tests for parsing object renaming declarations."""

    def test_simple_variable_renaming(self):
        """Test parsing simple variable renaming."""
        source = """
        procedure Test is
            X : Integer := 42;
            Y : Integer renames X;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_component_renaming(self):
        """Test renaming a record component."""
        source = """
        package Test is
            type Point is record
                X, Y : Integer;
            end record;

            P : Point := (X => 0, Y => 0);
            X_Coord : Integer renames P.X;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_array_element_renaming(self):
        """Test renaming an array element."""
        source = """
        procedure Test is
            type Arr is array (1 .. 10) of Integer;
            A : Arr := (others => 0);
            First : Integer renames A(1);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestSubprogramRenamingParsing:
    """Tests for parsing subprogram renaming declarations."""

    def test_procedure_renaming(self):
        """Test procedure renaming."""
        source = """
        package Test is
            procedure Original(X : Integer);
            procedure Alias(X : Integer) renames Original;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_function_renaming(self):
        """Test function renaming."""
        source = """
        package Test is
            function Compute(X : Integer) return Integer;
            function Calculate(X : Integer) return Integer renames Compute;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_operator_renaming(self):
        """Test operator renaming as function."""
        source = """
        package Test is
            type My_Int is new Integer;
            function "+"(Left, Right : My_Int) return My_Int;
            function Add(Left, Right : My_Int) return My_Int renames "+";
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestPackageRenamingParsing:
    """Tests for parsing package renaming declarations."""

    def test_package_renaming(self):
        """Test package renaming."""
        source = """
        package Original is
            X : Integer := 0;
        end Original;

        package Alias renames Original;
        """
        ast = parse(source)
        # Should parse without errors


class TestRenamingSemantic:
    """Tests for semantic analysis of renaming declarations."""

    def test_object_renaming_type(self):
        """Test that renamed object has same type."""
        source = """
        procedure Test is
            X : Integer := 42;
            Y : Integer renames X;
            Z : Integer;
        begin
            Z := Y + 1;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_procedure_renaming_callable(self):
        """Test that renamed procedure is callable."""
        source = """
        package Test is
            procedure Do_Something;

            procedure Process renames Do_Something;

            procedure Do_Something is
            begin
                null;
            end Do_Something;

            procedure Run is
            begin
                Process;
            end Run;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestRenamingUseCases:
    """Tests for common renaming use cases."""

    def test_shorthand_alias(self):
        """Test creating shorthand aliases."""
        source = """
        package Long_Package_Name is
            procedure Very_Long_Procedure_Name;
        end Long_Package_Name;

        procedure Short renames Long_Package_Name.Very_Long_Procedure_Name;
        """
        ast = parse(source)
        # Should parse without errors

    def test_use_type_alternative(self):
        """Test renaming as alternative to use type."""
        source = """
        package Math_Types is
            type Vector is record
                X, Y, Z : Integer;
            end record;

            function Add(A, B : Vector) return Vector;
        end Math_Types;

        package Test is
            subtype Vec is Math_Types.Vector;
            function "+"(A, B : Vec) return Vec renames Math_Types.Add;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_exception_renaming(self):
        """Test exception renaming."""
        source = """
        package Test is
            Original_Error : exception;
            Alias_Error : exception renames Original_Error;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors
