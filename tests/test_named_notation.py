"""Tests for Ada named notation in calls and aggregates."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestNamedParameterCalls:
    """Tests for named parameter notation in calls."""

    def test_simple_named_call(self):
        """Test simple named parameter call."""
        source = """
        procedure Test is
            procedure Set(X : Integer; Y : Integer) is
            begin
                null;
            end Set;
        begin
            Set(X => 1, Y => 2);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_mixed_positional_named(self):
        """Test mixed positional and named parameters."""
        source = """
        procedure Test is
            procedure Config(A, B, C : Integer) is
            begin
                null;
            end Config;
        begin
            Config(1, B => 2, C => 3);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_named_reordered(self):
        """Test named parameters in different order."""
        source = """
        procedure Test is
            procedure Init(First, Second, Third : Integer) is
            begin
                null;
            end Init;
        begin
            Init(Third => 3, First => 1, Second => 2);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestNamedAggregates:
    """Tests for named notation in aggregates."""

    def test_named_record_aggregate(self):
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

    def test_named_array_aggregate(self):
        """Test named array aggregate."""
        source = """
        procedure Test is
            type Days is array (1 .. 7) of Integer;
            D : Days := (1 => 10, 2 => 20, 3 => 30, others => 0);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_others_clause(self):
        """Test others clause in aggregate."""
        source = """
        procedure Test is
            type Vector is array (1 .. 100) of Integer;
            V : Vector := (1 => 1, 2 => 2, others => 0);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestNamedGenericActuals:
    """Tests for named notation in generic instantiation."""

    def test_named_generic_params(self):
        """Test named parameters in generic instantiation."""
        source = """
        generic
            type Key_Type is private;
            type Value_Type is private;
        package Map_Pkg is
        end Map_Pkg;

        package Int_String_Map is new Map_Pkg(
            Key_Type => Integer,
            Value_Type => Integer
        );
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestNamedNotationUseCases:
    """Tests for common named notation use cases."""

    def test_many_parameters(self):
        """Test named notation for clarity with many params."""
        source = """
        procedure Test is
            procedure Configure(
                Width : Integer;
                Height : Integer;
                Depth : Integer;
                Color : Integer;
                Visible : Boolean
            ) is
            begin
                null;
            end Configure;
        begin
            Configure(
                Width => 800,
                Height => 600,
                Depth => 32,
                Color => 0,
                Visible => True
            );
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_selective_named(self):
        """Test selective use of named notation."""
        source = """
        procedure Test is
            procedure Draw(X, Y : Integer; Color : Integer := 0) is
            begin
                null;
            end Draw;
        begin
            Draw(10, 20, Color => 255);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_discriminant_constraint_named(self):
        """Test named discriminant constraint."""
        source = """
        procedure Test is
            type Buffer(Max_Size : Integer) is record
                Count : Integer;
            end record;
            B : Buffer(Max_Size => 100);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
