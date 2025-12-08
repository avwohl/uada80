"""Tests for Ada 2022 delta aggregates."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import DeltaAggregate


class TestDeltaAggregateParsing:
    """Tests for parsing delta aggregates."""

    def test_simple_delta_aggregate(self):
        """Test parsing simple delta aggregate."""
        source = """
        package Test is
            type Point is record
                X, Y : Integer;
            end record;

            P1 : Point := (X => 0, Y => 0);
            P2 : Point := (P1 with delta X => 10);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_delta_aggregate_multiple_components(self):
        """Test delta aggregate with multiple component changes."""
        source = """
        package Test is
            type Rec is record
                A, B, C : Integer;
            end record;

            R1 : Rec := (A => 1, B => 2, C => 3);
            R2 : Rec := (R1 with delta A => 10, C => 30);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestDeltaAggregateSemantic:
    """Tests for semantic analysis of delta aggregates."""

    def test_delta_aggregate_type(self):
        """Test that delta aggregate has correct type."""
        source = """
        package Test is
            type Point is record
                X, Y : Integer;
            end record;

            function Shift_X(P : Point; Dx : Integer) return Point is
            begin
                return (P with delta X => P.X + Dx);
            end Shift_X;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_delta_aggregate_in_assignment(self):
        """Test delta aggregate in variable assignment."""
        source = """
        procedure Test is
            type Point is record
                X, Y : Integer;
            end record;
            P1 : Point := (X => 0, Y => 0);
            P2 : Point;
        begin
            P2 := (P1 with delta X => 5);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestDeltaAggregateUseCases:
    """Tests for common delta aggregate use cases."""

    def test_functional_update(self):
        """Test using delta aggregate for functional update."""
        source = """
        package Records is
            type Config is record
                Width : Integer;
                Height : Integer;
                Color : Integer;
            end record;

            function Set_Width(C : Config; W : Integer) return Config is
            begin
                return (C with delta Width => W);
            end Set_Width;

            function Set_Height(C : Config; H : Integer) return Config is
            begin
                return (C with delta Height => H);
            end Set_Height;
        end Records;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_delta_with_expression_value(self):
        """Test delta aggregate with expression as value."""
        source = """
        procedure Test is
            type Counter is record
                Value : Integer;
            end record;
            C : Counter := (Value => 0);
        begin
            C := (C with delta Value => C.Value + 1);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
