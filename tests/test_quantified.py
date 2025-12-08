"""Tests for Ada 2012 quantified expressions."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import QuantifiedExpr


class TestQuantifiedExprParsing:
    """Tests for parsing quantified expressions."""

    def test_for_all_simple(self):
        """Test parsing simple for all expression."""
        source = """
        function All_Positive(Arr : array (1 .. 10) of Integer) return Boolean is
        begin
            return (for all I in 1 .. 10 => Arr(I) > 0);
        end All_Positive;
        """
        ast = parse(source)
        # Should parse without errors

    def test_for_some_simple(self):
        """Test parsing simple for some expression."""
        source = """
        function Any_Negative(Arr : array (1 .. 10) of Integer) return Boolean is
        begin
            return (for some I in 1 .. 10 => Arr(I) < 0);
        end Any_Negative;
        """
        ast = parse(source)
        # Should parse without errors

    def test_for_all_with_type(self):
        """Test for all with explicit type."""
        source = """
        function Check(N : Integer) return Boolean is
        begin
            return (for all I in Integer range 1 .. N => I > 0);
        end Check;
        """
        ast = parse(source)
        # Should parse without errors

    def test_quantified_in_declaration(self):
        """Test quantified expression in constant declaration."""
        source = """
        procedure Test is
            Is_Valid : constant Boolean := (for all I in 1 .. 5 => I > 0);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestQuantifiedExprSemantic:
    """Tests for semantic analysis of quantified expressions."""

    def test_for_all_type_check(self):
        """Test that for all expression returns Boolean."""
        source = """
        procedure Test is
            Result : Boolean;
        begin
            Result := (for all I in 1 .. 10 => I > 0);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_for_some_type_check(self):
        """Test that for some expression returns Boolean."""
        source = """
        procedure Test is
            Found : Boolean;
        begin
            Found := (for some I in 1 .. 5 => I = 3);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_quantified_in_if(self):
        """Test quantified expression in if condition."""
        source = """
        procedure Test is
        begin
            if (for all I in 1 .. 10 => I > 0) then
                null;
            end if;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestQuantifiedExprUseCases:
    """Tests for common quantified expression use cases."""

    def test_all_elements_condition(self):
        """Test checking all elements satisfy a condition."""
        source = """
        function All_Even(N : Integer) return Boolean is
        begin
            return (for all I in 1 .. N => I mod 2 = 0);
        end All_Even;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_exists_element(self):
        """Test checking if any element satisfies a condition."""
        source = """
        function Has_Zero(N : Integer) return Boolean is
        begin
            return (for some I in 0 .. N => I = 0);
        end Has_Zero;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_combined_with_and(self):
        """Test quantified expression combined with other expressions."""
        source = """
        function Valid_Range(N : Integer) return Boolean is
        begin
            return N > 0 and (for all I in 1 .. N => I <= N);
        end Valid_Range;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
