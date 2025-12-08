"""Tests for Ada subtype declarations."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestSubtypeParsing:
    """Tests for parsing subtype declarations."""

    def test_simple_subtype(self):
        """Test simple subtype declaration."""
        source = """
        procedure Test is
            subtype Small_Int is Integer range 1 .. 100;
            X : Small_Int := 50;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_subtype_no_constraint(self):
        """Test subtype without constraint."""
        source = """
        procedure Test is
            subtype My_Int is Integer;
            X : My_Int := 42;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_predefined_subtypes(self):
        """Test predefined subtype Natural."""
        source = """
        procedure Test is
            N : Natural := 0;
            P : Positive := 1;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestSubtypeConstraints:
    """Tests for subtype constraints."""

    def test_range_constraint(self):
        """Test subtype with range constraint."""
        source = """
        procedure Test is
            subtype Percentage is Integer range 0 .. 100;
            P : Percentage := 75;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_index_constraint(self):
        """Test subtype with index constraint."""
        source = """
        procedure Test is
            type Arr is array (Integer range <>) of Integer;
            subtype Fixed_Arr is Arr(1 .. 10);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse index constraint

    def test_discriminant_constraint(self):
        """Test subtype with discriminant constraint."""
        source = """
        procedure Test is
            type Buffer(Size : Integer) is record
                Count : Integer;
            end record;
            subtype Small_Buffer is Buffer(100);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse discriminant constraint


class TestSubtypeUseCases:
    """Tests for common subtype use cases."""

    def test_type_alias(self):
        """Test subtype as type alias."""
        source = """
        package Types is
            subtype Counter is Natural;
            subtype Index is Positive;

            C : Counter := 0;
            I : Index := 1;
        end Types;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_constrained_subtype(self):
        """Test constrained subtype for validation."""
        source = """
        package Validation is
            subtype Month is Integer range 1 .. 12;
            subtype Day is Integer range 1 .. 31;
            subtype Year is Integer range 1900 .. 2100;

            M : Month := 6;
            D : Day := 15;
            Y : Year := 2024;
        end Validation;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_array_subtype(self):
        """Test array subtype."""
        source = """
        procedure Test is
            type Vector is array (Positive range <>) of Integer;
            subtype Vec3 is Vector(1 .. 3);
            V : Vec3 := (1, 2, 3);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestSubtypeCompatibility:
    """Tests for subtype compatibility."""

    def test_assignment_compatible(self):
        """Test assignment between compatible subtypes."""
        source = """
        procedure Test is
            subtype Small is Integer range 1 .. 10;
            subtype Medium is Integer range 1 .. 100;
            S : Small := 5;
            M : Medium;
        begin
            M := S;  -- Small is compatible with Medium
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_parameter_subtype(self):
        """Test subtype in parameter declarations."""
        source = """
        package Test is
            subtype Index is Positive;

            procedure Process(I : Index);
            function Get_Index return Index;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
