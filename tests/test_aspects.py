"""Tests for Ada aspect specifications (Ada 2012)."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestTypeAspects:
    """Tests for type aspect specifications."""

    def test_type_invariant(self):
        """Test Type_Invariant aspect."""
        source = """
        package Test is
            type Positive_Counter is private
                with Type_Invariant => Positive_Counter.Value > 0;
        private
            type Positive_Counter is record
                Value : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse type invariant

    def test_default_initial_condition(self):
        """Test Default_Initial_Condition aspect."""
        source = """
        package Test is
            type Counter is private
                with Default_Initial_Condition;
        private
            type Counter is new Integer;
        end Test;
        """
        ast = parse(source)
        # Should parse default initial condition


class TestSubprogramAspects:
    """Tests for subprogram aspect specifications."""

    def test_precondition(self):
        """Test Pre aspect."""
        source = """
        package Test is
            function Divide(A, B : Integer) return Integer
                with Pre => B /= 0;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_postcondition(self):
        """Test Post aspect."""
        source = """
        package Test is
            function Abs_Value(X : Integer) return Integer
                with Post => Abs_Value'Result >= 0;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_pre_and_post(self):
        """Test Pre and Post aspects together."""
        source = """
        package Test is
            function Square_Root(X : Integer) return Integer
                with Pre => X >= 0,
                     Post => Square_Root'Result >= 0;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_inline_aspect(self):
        """Test Inline aspect."""
        source = """
        package Test is
            function Fast_Add(A, B : Integer) return Integer
                with Inline;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestObjectAspects:
    """Tests for object aspect specifications."""

    def test_constant_aspect(self):
        """Test object with aspects."""
        source = """
        package Test is
            Max_Size : constant Integer := 100
                with Static;
        end Test;
        """
        ast = parse(source)
        # Should parse static aspect


class TestPackageAspects:
    """Tests for package aspect specifications."""

    def test_elaborate_body(self):
        """Test Elaborate_Body aspect."""
        source = """
        package Test
            with Elaborate_Body
        is
            procedure Init;
        end Test;
        """
        ast = parse(source)
        # Should parse elaborate body

    def test_preelaborate_aspect(self):
        """Test Preelaborate aspect."""
        source = """
        package Pure_Math
            with Pure
        is
            function Add(A, B : Integer) return Integer;
        end Pure_Math;
        """
        ast = parse(source)
        # Should parse pure aspect


class TestAspectUseCases:
    """Tests for common aspect use cases."""

    def test_contract_programming(self):
        """Test contract programming with aspects."""
        source = """
        package Stack is
            type Stack_Type is private;

            function Is_Empty(S : Stack_Type) return Boolean;
            function Is_Full(S : Stack_Type) return Boolean;

            procedure Push(S : in Out Stack_Type; X : Integer)
                with Pre => not Is_Full(S);

            function Pop(S : in Out Stack_Type) return Integer
                with Pre => not Is_Empty(S);
        private
            type Stack_Type is record
                Count : Integer;
            end record;
        end Stack;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should handle contract aspects

    def test_multiple_aspects(self):
        """Test multiple aspects on single declaration."""
        source = """
        package Test is
            function Safe_Divide(A, B : Integer) return Integer
                with Pre => B /= 0,
                     Post => Safe_Divide'Result * B <= A,
                     Inline;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
