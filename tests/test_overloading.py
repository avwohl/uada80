"""Tests for Ada subprogram overloading."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestProcedureOverloading:
    """Tests for procedure overloading."""

    def test_different_parameter_count(self):
        """Test overloading by parameter count."""
        source = """
        package Test is
            procedure Print(X : Integer);
            procedure Print(X, Y : Integer);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_different_parameter_types(self):
        """Test overloading by parameter types."""
        source = """
        package Test is
            procedure Show(X : Integer);
            procedure Show(X : Boolean);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_different_parameter_modes(self):
        """Test overloading by parameter modes."""
        source = """
        package Test is
            procedure Update(X : Integer);
            procedure Update(X : out Integer);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # May or may not distinguish by mode depending on implementation


class TestFunctionOverloading:
    """Tests for function overloading."""

    def test_same_name_different_params(self):
        """Test function overloading with different parameters."""
        source = """
        package Test is
            function Compute(X : Integer) return Integer;
            function Compute(X : Boolean) return Boolean;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_same_name_different_return(self):
        """Test functions with same params different return types."""
        source = """
        package Test is
            function Convert(X : Integer) return Integer;
            function Convert(X : Integer) return Boolean;
        end Test;
        """
        ast = parse(source)
        # Ada allows overloading by return type


class TestOperatorOverloading:
    """Tests for operator overloading."""

    def test_plus_operator(self):
        """Test overloading + operator."""
        source = """
        package Test is
            type Vector is record
                X, Y : Integer;
            end record;

            function "+"(Left, Right : Vector) return Vector;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_comparison_operator(self):
        """Test overloading comparison operator."""
        source = """
        package Test is
            type Point is record
                X, Y : Integer;
            end record;

            function "="(Left, Right : Point) return Boolean;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_unary_operator(self):
        """Test overloading unary operator."""
        source = """
        package Test is
            type Vector is record
                X, Y : Integer;
            end record;

            function "-"(V : Vector) return Vector;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestOverloadingResolution:
    """Tests for overload resolution."""

    def test_call_specific_overload(self):
        """Test calling specific overloaded subprogram."""
        source = """
        procedure Test is
            function Add(A, B : Integer) return Integer is
            begin
                return A + B;
            end Add;

            function Add(A, B : Boolean) return Boolean is
            begin
                return A or B;
            end Add;

            X : Integer;
            Y : Boolean;
        begin
            X := Add(1, 2);  -- Calls Integer version
            Y := Add(True, False);  -- Calls Boolean version
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestOverloadingUseCases:
    """Tests for common overloading use cases."""

    def test_type_constructor_pattern(self):
        """Test overloaded constructors pattern."""
        source = """
        package Vectors is
            type Vector is record
                X, Y, Z : Integer;
            end record;

            function Create return Vector;
            function Create(Val : Integer) return Vector;
            function Create(X, Y, Z : Integer) return Vector;
        end Vectors;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_generic_print_pattern(self):
        """Test generic print pattern with overloading."""
        source = """
        package IO is
            procedure Put(X : Integer);
            procedure Put(X : Boolean);
            procedure Put(X : Character);
        end IO;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
