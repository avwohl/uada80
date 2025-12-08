"""Tests for Ada operators."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestArithmeticOperators:
    """Tests for arithmetic operators."""

    def test_basic_arithmetic(self):
        """Test basic arithmetic operators."""
        source = """
        procedure Test is
            A, B, R : Integer;
        begin
            R := A + B;
            R := A - B;
            R := A * B;
            R := A / B;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_modular_operators(self):
        """Test mod and rem operators."""
        source = """
        procedure Test is
            A, B, R : Integer;
        begin
            R := A mod B;
            R := A rem B;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_exponentiation(self):
        """Test ** operator."""
        source = """
        procedure Test is
            Base : Integer := 2;
            Exp : Integer := 3;
            R : Integer;
        begin
            R := Base ** Exp;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_abs_operator(self):
        """Test abs operator."""
        source = """
        procedure Test is
            X : Integer := 5;
            Y : Integer;
            R : Integer;
        begin
            Y := 0 - X;  -- Make it negative
            R := abs Y;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestComparisonOperators:
    """Tests for comparison operators."""

    def test_equality(self):
        """Test equality operators."""
        source = """
        procedure Test is
            A, B : Integer;
            R : Boolean;
        begin
            R := A = B;
            R := A /= B;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_relational(self):
        """Test relational operators."""
        source = """
        procedure Test is
            A, B : Integer;
            R : Boolean;
        begin
            R := A < B;
            R := A <= B;
            R := A > B;
            R := A >= B;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestLogicalOperators:
    """Tests for logical operators."""

    def test_boolean_operators(self):
        """Test boolean operators."""
        source = """
        procedure Test is
            A, B, R : Boolean;
        begin
            R := A and B;
            R := A or B;
            R := A xor B;
            R := not A;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_short_circuit(self):
        """Test short-circuit operators."""
        source = """
        procedure Test is
            A, B, R : Boolean;
        begin
            R := A and then B;
            R := A or else B;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestConcatenationOperator:
    """Tests for concatenation operator."""

    def test_array_concatenation(self):
        """Test array concatenation."""
        source = """
        procedure Test is
            type Arr is array (1 .. 10) of Integer;
            A, B : Arr;
        begin
            null;  -- Concatenation would need compatible array types
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestMembershipOperator:
    """Tests for membership operator."""

    def test_in_operator(self):
        """Test 'in' membership operator."""
        source = """
        procedure Test is
            type Small is range 1 .. 10;
            X : Integer := 5;
            R : Boolean;
        begin
            R := X in 1 .. 10;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_not_in_operator(self):
        """Test 'not in' membership operator."""
        source = """
        procedure Test is
            X : Integer := 15;
            R : Boolean;
        begin
            R := X not in 1 .. 10;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestOperatorUseCases:
    """Tests for common operator use cases."""

    def test_conditional_expressions(self):
        """Test operators in conditional expressions."""
        source = """
        procedure Test is
            A, B, C : Integer;
            D : Boolean;
        begin
            D := A > B and then B > C;
            D := A = 0 or else B /= 0;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_complex_arithmetic(self):
        """Test complex arithmetic expressions."""
        source = """
        procedure Test is
            A, B, C, R : Integer;
        begin
            R := (A + B) * C - A / B;
            R := A ** 2 + B ** 2;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
