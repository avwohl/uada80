"""Tests for Ada 2022 reduction expressions."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestReductionParsing:
    """Tests for parsing reduction expressions."""

    def test_array_reduce_sum(self):
        """Test parsing array reduction with sum."""
        source = """
        function Sum(Arr : array (1 .. 10) of Integer) return Integer is
        begin
            return Arr'Reduce("+", 0);
        end Sum;
        """
        ast = parse(source)
        # Should parse without errors

    def test_container_aggregate_reduce(self):
        """Test parsing container aggregate with reduce."""
        source = """
        function Sum_Squares(N : Integer) return Integer is
        begin
            return [for I in 1 .. N => I * I]'Reduce("+", 0);
        end Sum_Squares;
        """
        ast = parse(source)
        # Should parse without errors

    def test_reduce_with_function_name(self):
        """Test reduce with named function as combiner."""
        source = """
        package Test is
            function Add(A, B : Integer) return Integer;

            function Sum(Arr : array (1 .. 10) of Integer) return Integer is
            begin
                return Arr'Reduce(Add, 0);
            end Sum;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestReductionSemantic:
    """Tests for semantic analysis of reduction expressions."""

    def test_reduce_type(self):
        """Test that reduce returns correct type."""
        source = """
        procedure Test is
            type Arr_Type is array (1 .. 5) of Integer;
            Arr : Arr_Type := (1, 2, 3, 4, 5);
            Sum : Integer;
        begin
            Sum := Arr'Reduce("+", 0);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_reduce_in_expression(self):
        """Test reduce used in larger expression."""
        source = """
        procedure Test is
            type Arr_Type is array (1 .. 5) of Integer;
            Arr : Arr_Type := (1, 2, 3, 4, 5);
            Result : Integer;
        begin
            Result := Arr'Reduce("+", 0) + 10;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_reduce_in_return(self):
        """Test reduce in return statement."""
        source = """
        function Total(A, B, C : Integer) return Integer is
        begin
            return [A, B, C]'Reduce("+", 0);
        end Total;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestReductionUseCases:
    """Tests for common reduction expression use cases."""

    def test_sum_array(self):
        """Test summing array elements."""
        source = """
        function Array_Sum return Integer is
            type Int_Array is array (1 .. 4) of Integer;
            Arr : Int_Array := (10, 20, 30, 40);
        begin
            return Arr'Reduce("+", 0);
        end Array_Sum;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_product_of_values(self):
        """Test computing product with reduce."""
        source = """
        function Factorial(N : Integer) return Integer is
        begin
            return [for I in 1 .. N => I]'Reduce("*", 1);
        end Factorial;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_reduce_with_condition(self):
        """Test reduce used in conditional."""
        source = """
        function Check_Sum(Limit : Integer) return Boolean is
        begin
            return [for I in 1 .. 10 => I]'Reduce("+", 0) > Limit;
        end Check_Sum;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_nested_reduce(self):
        """Test using reduce result in another reduce."""
        source = """
        function Double_Sum return Integer is
        begin
            return [[1, 2, 3]'Reduce("+", 0), [4, 5, 6]'Reduce("+", 0)]'Reduce("+", 0);
        end Double_Sum;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
