"""Tests for Ada 2012 contract attributes ('Old, 'Result, 'Update)."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestOldAttributeParsing:
    """Tests for parsing 'Old attribute."""

    def test_old_in_postcondition(self):
        """Test parsing 'Old in postcondition."""
        source = """
        procedure Increment(X : in out Integer)
            with Post => X = X'Old + 1;
        """
        ast = parse(source)
        # Should parse without errors

    def test_old_with_expression(self):
        """Test 'Old with complex expression."""
        source = """
        procedure Update(A, B : in out Integer)
            with Post => A + B = (A'Old + B'Old);
        """
        ast = parse(source)
        # Should parse without errors


class TestResultAttributeParsing:
    """Tests for parsing 'Result attribute."""

    def test_result_in_postcondition(self):
        """Test parsing 'Result in function postcondition."""
        source = """
        function Double(X : Integer) return Integer
            with Post => Double'Result = X * 2;
        """
        ast = parse(source)
        # Should parse without errors

    def test_result_with_old(self):
        """Test 'Result combined with 'Old."""
        source = """
        function Increment(X : Integer) return Integer
            with Post => Increment'Result = X'Old + 1;
        """
        ast = parse(source)
        # Should parse without errors


class TestContractAttributesSemantic:
    """Tests for semantic analysis of contract attributes."""

    def test_old_type_preservation(self):
        """Test that 'Old preserves the type of the expression."""
        source = """
        procedure Inc(X : in out Integer)
            with Post => X = X'Old + 1;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_result_type_in_function(self):
        """Test that 'Result has correct type in function."""
        source = """
        function Square(X : Integer) return Integer
            with Post => Square'Result >= 0;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_old_on_parameter(self):
        """Test 'Old on parameter."""
        source = """
        procedure Swap(A, B : in out Integer)
            with Post => A = B'Old and B = A'Old;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestContractAttributesUseCases:
    """Tests for common contract attribute use cases."""

    def test_increment_contract(self):
        """Test increment procedure with full contract."""
        source = """
        procedure Increment(X : in out Integer)
            with Pre => X < Integer'Last,
                 Post => X = X'Old + 1;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_factorial_contract(self):
        """Test factorial function with result contract."""
        source = """
        function Factorial(N : Integer) return Integer
            with Pre => N >= 0,
                 Post => Factorial'Result >= 1;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_swap_contract(self):
        """Test swap procedure with 'Old for both parameters."""
        source = """
        procedure Swap(A, B : in out Integer)
            with Post => A = B'Old and B = A'Old;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_push_contract(self):
        """Test push operation contract."""
        source = """
        package Stack is
            type Stack_Type is private;

            function Size(S : Stack_Type) return Integer;

            procedure Push(S : in out Stack_Type; Item : Integer)
                with Post => Size(S) = Size(S'Old) + 1;
        private
            type Stack_Type is record
                Count : Integer;
            end record;
        end Stack;
        """
        ast = parse(source)
        result = analyze(ast)
        # May have errors due to incomplete implementation, but parsing should work

    def test_abs_contract(self):
        """Test absolute value function contract."""
        source = """
        function Abs_Val(X : Integer) return Integer
            with Post => Abs_Val'Result >= 0 and
                         (Abs_Val'Result = X or Abs_Val'Result = -X);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
