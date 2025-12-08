"""Tests for Ada 2012 expression functions."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import SubprogramBody, ReturnStmt


class TestExpressionFunctionParsing:
    """Tests for parsing expression functions."""

    def test_simple_expression_function(self):
        """Test simple expression function."""
        source = """
        function Square(X : Integer) return Integer is (X * X);
        """
        ast = parse(source)
        assert len(ast.units) == 1
        body = ast.units[0].unit
        assert isinstance(body, SubprogramBody)
        assert body.spec.name == "Square"
        assert len(body.statements) == 1
        assert isinstance(body.statements[0], ReturnStmt)

    def test_expression_function_with_comparison(self):
        """Test expression function returning Boolean."""
        source = """
        function Is_Positive(X : Integer) return Boolean is (X > 0);
        """
        ast = parse(source)
        body = ast.units[0].unit
        assert isinstance(body, SubprogramBody)
        assert len(body.statements) == 1

    def test_expression_function_with_conditional(self):
        """Test expression function with conditional expression."""
        source = """
        function Abs_Value(X : Integer) return Integer is
            (if X >= 0 then X else -X);
        """
        ast = parse(source)
        body = ast.units[0].unit
        assert isinstance(body, SubprogramBody)

    def test_expression_function_with_case(self):
        """Test expression function with case expression."""
        source = """
        type Color is (Red, Green, Blue);
        function To_Int(C : Color) return Integer is
            (case C is
                when Red => 1,
                when Green => 2,
                when Blue => 3);
        """
        ast = parse(source)

    def test_expression_function_complex(self):
        """Test expression function with complex expression."""
        source = """
        function Max(A, B : Integer) return Integer is
            (if A > B then A else B);
        """
        ast = parse(source)
        body = ast.units[0].unit
        assert isinstance(body, SubprogramBody)


class TestExpressionFunctionSemantic:
    """Tests for semantic analysis of expression functions."""

    def test_expression_function_type_check(self):
        """Test that expression function return type is checked."""
        source = """
        function Double(X : Integer) return Integer is (X * 2);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_expression_function_parameter_usage(self):
        """Test that parameters are usable in expression."""
        source = """
        function Add(X, Y : Integer) return Integer is (X + Y);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_expression_function_with_aspect(self):
        """Test expression function with aspects."""
        source = """
        function Is_Valid(X : Integer) return Boolean is (X >= 0)
            with Inline;
        """
        ast = parse(source)
        # May or may not work depending on aspect placement

    def test_expression_function_callable(self):
        """Test that expression function can be called."""
        source = """
        function Triple(X : Integer) return Integer is (X * 3);

        procedure Test is
            Y : Integer;
        begin
            Y := Triple(5);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestExpressionFunctionUseCases:
    """Tests for common expression function use cases."""

    def test_predicate_function(self):
        """Test expression function as predicate."""
        source = """
        function Is_Even(N : Integer) return Boolean is (N mod 2 = 0);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_accessor_function(self):
        """Test expression function as simple accessor."""
        source = """
        type Point is record
            X, Y : Integer;
        end record;

        function Get_X(P : Point) return Integer is (P.X);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_min_max_functions(self):
        """Test min/max expression functions."""
        source = """
        function Min(A, B : Integer) return Integer is
            (if A < B then A else B);

        function Max(A, B : Integer) return Integer is
            (if A > B then A else B);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_clamp_function(self):
        """Test clamp expression function."""
        source = """
        function Clamp(Value, Low, High : Integer) return Integer is
            (if Value < Low then Low
             elsif Value > High then High
             else Value);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
