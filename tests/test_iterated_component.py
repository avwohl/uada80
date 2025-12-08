"""Tests for Ada 2012 iterated component associations."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import IteratedComponentAssociation


class TestIteratedComponentParsing:
    """Tests for parsing iterated component associations."""

    def test_simple_iterated_in(self):
        """Test parsing simple iterated component with 'in'."""
        source = """
        procedure Test is
            type Int_Array is array (1 .. 5) of Integer;
            Arr : Int_Array := (for I in 1 .. 5 => I);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_iterated_with_expression(self):
        """Test iterated component with expression value."""
        source = """
        procedure Test is
            type Int_Array is array (1 .. 10) of Integer;
            Squares : Int_Array := (for I in 1 .. 10 => I * I);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_iterated_container_aggregate(self):
        """Test iterated component in container aggregate."""
        source = """
        function Sum_Squares(N : Integer) return Integer is
        begin
            return [for I in 1 .. N => I * I]'Reduce("+", 0);
        end Sum_Squares;
        """
        ast = parse(source)
        # Should parse without errors


class TestIteratedComponentSemantic:
    """Tests for semantic analysis of iterated component associations."""

    def test_loop_variable_in_scope(self):
        """Test that loop variable is visible in the value expression."""
        source = """
        procedure Test is
            type Int_Array is array (1 .. 5) of Integer;
            Arr : Int_Array := (for I in 1 .. 5 => I + 1);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_loop_variable_out_of_scope(self):
        """Test that loop variable is not visible outside."""
        source = """
        procedure Test is
            type Int_Array is array (1 .. 5) of Integer;
            Arr : Int_Array := (for I in 1 .. 5 => I);
            X : Integer := I;  -- Error: I not in scope
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert result.has_errors
        assert any("I" in str(e) and "not found" in str(e).lower() for e in result.errors)

    def test_nested_iterated_expression(self):
        """Test using loop variable in complex expression."""
        source = """
        procedure Test is
            type Int_Array is array (1 .. 10) of Integer;
            Computed : Int_Array := (for I in 1 .. 10 => (I * I) + I);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestIteratedComponentUseCases:
    """Tests for common iterated component use cases."""

    def test_initialize_array(self):
        """Test using iterated component to initialize array."""
        source = """
        function Init_Array return Integer is
            type Arr_Type is array (1 .. 10) of Integer;
            Values : Arr_Type := (for I in 1 .. 10 => I * 2);
        begin
            return Values(5);
        end Init_Array;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_container_with_reduce(self):
        """Test container aggregate with iterated component and reduce."""
        source = """
        function Factorial(N : Integer) return Integer is
        begin
            return [for I in 1 .. N => I]'Reduce("*", 1);
        end Factorial;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_sum_of_squares(self):
        """Test computing sum of squares."""
        source = """
        function Sum_Squares(N : Integer) return Integer is
        begin
            return [for I in 1 .. N => I * I]'Reduce("+", 0);
        end Sum_Squares;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_iterated_in_function_return(self):
        """Test iterated component in return expression."""
        source = """
        function Make_Array return Integer is
        begin
            return [for I in 1 .. 5 => I]'Reduce("+", 0);
        end Make_Array;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
