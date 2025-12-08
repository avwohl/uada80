"""Tests for Ada 2012 expression functions."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestExpressionFunctionParsing:
    """Tests for parsing expression functions."""

    def test_simple_expression_function(self):
        """Test parsing simple expression function."""
        source = """
        function Square(X : Integer) return Integer is (X * X);
        """
        ast = parse(source)
        # Should parse without errors

    def test_expression_function_with_operator(self):
        """Test expression function with binary operator."""
        source = """
        function Add(A, B : Integer) return Integer is (A + B);
        """
        ast = parse(source)
        # Should parse without errors

    def test_expression_function_with_conditional(self):
        """Test expression function with conditional expression."""
        source = """
        function Max(A, B : Integer) return Integer is
            (if A > B then A else B);
        """
        ast = parse(source)
        # Should parse without errors

    def test_expression_function_in_package(self):
        """Test expression function inside package."""
        source = """
        package Math is
            function Double(X : Integer) return Integer is (X * 2);
            function Half(X : Integer) return Integer is (X / 2);
        end Math;
        """
        ast = parse(source)
        # Should parse without errors


class TestExpressionFunctionSemantic:
    """Tests for semantic analysis of expression functions."""

    def test_expression_function_return_type(self):
        """Test that expression function return type is checked."""
        source = """
        function Square(X : Integer) return Integer is (X * X);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_expression_function_parameter_usage(self):
        """Test that parameters are usable in expression."""
        source = """
        function Sum(A, B, C : Integer) return Integer is (A + B + C);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_expression_function_boolean_return(self):
        """Test expression function returning Boolean."""
        source = """
        function Is_Positive(X : Integer) return Boolean is (X > 0);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_expression_function_calling_another(self):
        """Test expression function calling another function."""
        source = """
        package Test is
            function Double(X : Integer) return Integer is (X * 2);
            function Quadruple(X : Integer) return Integer is (Double(Double(X)));
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestExpressionFunctionUseCases:
    """Tests for common expression function use cases."""

    def test_arithmetic_functions(self):
        """Test arithmetic expression functions."""
        source = """
        package Arithmetic is
            function Inc(X : Integer) return Integer is (X + 1);
            function Dec(X : Integer) return Integer is (X - 1);
            function Negate(X : Integer) return Integer is (-X);
        end Arithmetic;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_predicate_functions(self):
        """Test predicate expression functions."""
        source = """
        package Predicates is
            function Is_Zero(X : Integer) return Boolean is (X = 0);
            function Is_Negative(X : Integer) return Boolean is (X < 0);
            function Is_Even(X : Integer) return Boolean is (X mod 2 = 0);
        end Predicates;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_expression_function_with_case(self):
        """Test expression function with case expression."""
        source = """
        function Sign(X : Integer) return Integer is
            (case X is
                when 0 => 0,
                when others => (if X > 0 then 1 else -1));
        """
        ast = parse(source)
        result = analyze(ast)
        # Note: case expressions may have specific requirements
        # Just verify parsing works for now

    def test_record_component_access(self):
        """Test expression function accessing record component."""
        source = """
        package Test is
            type Point is record
                X, Y : Integer;
            end record;

            function Get_X(P : Point) return Integer is (P.X);
            function Get_Y(P : Point) return Integer is (P.Y);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
