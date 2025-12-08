"""Tests for Ada aggregate expressions."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestRecordAggregateParsing:
    """Tests for parsing record aggregates."""

    def test_positional_aggregate(self):
        """Test positional record aggregate."""
        source = """
        procedure Test is
            type Point is record
                X, Y : Integer;
            end record;
            P : Point := (10, 20);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_named_aggregate(self):
        """Test named record aggregate."""
        source = """
        procedure Test is
            type Point is record
                X, Y : Integer;
            end record;
            P : Point := (X => 10, Y => 20);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_mixed_aggregate(self):
        """Test mixed positional/named aggregate."""
        source = """
        procedure Test is
            type RGB is record
                R, G, B : Integer;
            end record;
            C : RGB := (255, G => 128, B => 0);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse

    def test_others_aggregate(self):
        """Test aggregate with others clause."""
        source = """
        procedure Test is
            type Point3D is record
                X, Y, Z : Integer;
            end record;
            Origin : Point3D := (others => 0);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestArrayAggregateParsing:
    """Tests for parsing array aggregates."""

    def test_positional_array_aggregate(self):
        """Test positional array aggregate."""
        source = """
        procedure Test is
            type Arr is array (1 .. 3) of Integer;
            A : Arr := (10, 20, 30);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_named_array_aggregate(self):
        """Test named array aggregate."""
        source = """
        procedure Test is
            type Arr is array (1 .. 3) of Integer;
            A : Arr := (1 => 10, 2 => 20, 3 => 30);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_array_others(self):
        """Test array aggregate with others."""
        source = """
        procedure Test is
            type Arr is array (1 .. 100) of Integer;
            A : Arr := (others => 0);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_array_range_aggregate(self):
        """Test array aggregate with range."""
        source = """
        procedure Test is
            type Arr is array (1 .. 10) of Integer;
            A : Arr := (1 .. 5 => 1, 6 .. 10 => 2);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestNestedAggregates:
    """Tests for nested aggregate structures."""

    def test_nested_record_aggregate(self):
        """Test nested record aggregates."""
        source = """
        procedure Test is
            type Inner is record
                A, B : Integer;
            end record;
            type Outer is record
                X : Inner;
                Y : Integer;
            end record;
            O : Outer := (X => (A => 1, B => 2), Y => 3);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_array_of_records(self):
        """Test array of record aggregates."""
        source = """
        procedure Test is
            type Point is record
                X, Y : Integer;
            end record;
            type Points is array (1 .. 2) of Point;
            Pts : Points := ((1, 2), (3, 4));
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_2d_array_aggregate(self):
        """Test 2D array aggregate."""
        source = """
        procedure Test is
            type Matrix is array (1 .. 2, 1 .. 2) of Integer;
            M : Matrix := ((1, 2), (3, 4));
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestExtensionAggregates:
    """Tests for extension aggregates (tagged types)."""

    def test_extension_aggregate(self):
        """Test extension aggregate."""
        source = """
        package Test is
            type Base is tagged record
                X : Integer;
            end record;

            type Derived is new Base with record
                Y : Integer;
            end record;
        end Test;

        procedure Main is
            D : Test.Derived := (X => 1, Y => 2);
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should handle extension aggregate

    def test_ancestor_part(self):
        """Test aggregate with ancestor part."""
        source = """
        package Types is
            type Parent is tagged record
                A : Integer;
            end record;

            type Child is new Parent with record
                B : Integer;
            end record;
        end Types;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestQualifiedAggregates:
    """Tests for qualified expression aggregates."""

    def test_qualified_record_aggregate(self):
        """Test qualified record aggregate."""
        source = """
        procedure Test is
            type Point is record
                X, Y : Integer;
            end record;
            P : Point := Point'(10, 20);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_qualified_array_aggregate(self):
        """Test qualified array aggregate."""
        source = """
        procedure Test is
            type Arr is array (1 .. 3) of Integer;
            A : Arr := Arr'(1, 2, 3);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestAggregateUseCases:
    """Tests for common aggregate use cases."""

    def test_constant_initialization(self):
        """Test aggregate for constant initialization."""
        source = """
        package Constants is
            type Config is record
                Max_Size : Integer;
                Enabled : Boolean;
            end record;

            Default_Config : constant Config := (Max_Size => 100, Enabled => True);
        end Constants;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_inline_aggregate(self):
        """Test inline aggregate in expression."""
        source = """
        procedure Test is
            type Point is record
                X, Y : Integer;
            end record;

            function Distance(P : Point) return Integer is
            begin
                return P.X + P.Y;
            end Distance;

            Result : Integer;
        begin
            Result := Distance((X => 3, Y => 4));
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
