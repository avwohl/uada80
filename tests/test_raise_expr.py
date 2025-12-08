"""Tests for Ada 2012 raise expressions."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import RaiseExpr


class TestRaiseExprParsing:
    """Tests for parsing raise expressions."""

    def test_simple_raise_expr(self):
        """Test parsing simple raise expression."""
        source = """
        function Safe_Div(A, B : Integer) return Integer is
        begin
            return (if B /= 0 then A / B else raise Constraint_Error);
        end Safe_Div;
        """
        ast = parse(source)
        # Should parse without errors

    def test_raise_expr_with_message(self):
        """Test raise expression with message."""
        source = """
        function Check(X : Integer) return Integer is
        begin
            return (if X > 0 then X else raise Constraint_Error with "X must be positive");
        end Check;
        """
        ast = parse(source)
        # Should parse without errors

    def test_raise_in_conditional(self):
        """Test raise expression in conditional expression."""
        source = """
        procedure Test is
            X : Integer := 0;
            Y : Integer;
        begin
            Y := (if X > 0 then X else raise Program_Error);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestRaiseExprSemantic:
    """Tests for semantic analysis of raise expressions."""

    def test_raise_expr_in_if(self):
        """Test raise expression in if expression."""
        source = """
        function Get_Value(Valid : Boolean; Value : Integer) return Integer is
        begin
            return (if Valid then Value else raise Constraint_Error);
        end Get_Value;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_raise_expr_with_string_message(self):
        """Test raise expression with string message."""
        source = """
        function Validate(X : Integer) return Integer is
        begin
            return (if X >= 0 then X else raise Constraint_Error with "Negative value");
        end Validate;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_raise_expr_in_case(self):
        """Test raise expression in case expression."""
        source = """
        function To_Digit(C : Integer) return Integer is
        begin
            return (case C is
                when 48 .. 57 => C - 48,
                when others => raise Constraint_Error);
        end To_Digit;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestRaiseExprUseCases:
    """Tests for common raise expression use cases."""

    def test_null_check_pattern(self):
        """Test raise expression for null check."""
        source = """
        function Require_Positive(X : Integer) return Integer is
        begin
            return (if X > 0 then X else raise Constraint_Error with "Must be positive");
        end Require_Positive;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_range_validation(self):
        """Test raise expression for range validation."""
        source = """
        function Validate_Percentage(P : Integer) return Integer is
        begin
            return (if P in 0 .. 100 then P else raise Constraint_Error);
        end Validate_Percentage;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_chained_conditions(self):
        """Test raise expression in chained conditional."""
        source = """
        function Safe_Operation(A, B : Integer) return Integer is
        begin
            return (if B = 0 then raise Constraint_Error
                    elsif A < 0 then raise Constraint_Error
                    else A / B);
        end Safe_Operation;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_raise_in_assignment(self):
        """Test raise expression in assignment."""
        source = """
        procedure Process(X : Integer) is
            Result : Integer;
        begin
            Result := (if X > 0 then X * 2 else raise Constraint_Error);
        end Process;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
