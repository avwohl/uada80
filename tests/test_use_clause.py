"""Tests for Ada use clauses."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestUseClauseParsing:
    """Tests for parsing use clauses."""

    def test_simple_use(self):
        """Test parsing simple use clause."""
        source = """
        with Ada.Text_IO;
        use Ada.Text_IO;

        procedure Test is
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_multiple_use(self):
        """Test multiple use clauses."""
        source = """
        with Ada.Text_IO;
        with Ada.Integer_Text_IO;
        use Ada.Text_IO;
        use Ada.Integer_Text_IO;

        procedure Test is
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_use_type(self):
        """Test use type clause."""
        source = """
        package Test is
            type My_Type is new Integer;
        end Test;

        with Test;
        use type Test.My_Type;

        procedure Main is
            A, B : Test.My_Type;
            C : Test.My_Type;
        begin
            C := A + B;  -- Operators visible via use type
        end Main;
        """
        ast = parse(source)
        # Should parse without errors

    def test_use_all_type(self):
        """Test use all type clause (Ada 2012)."""
        source = """
        package Types is
            type Vector is tagged record
                X, Y : Integer;
            end record;

            function Length(V : Vector) return Integer;
        end Types;

        with Types;
        use all type Types.Vector;

        procedure Test is
            V : Types.Vector;
            L : Integer;
        begin
            L := Length(V);  -- Primitive visible via use all type
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestUseClauseSemantic:
    """Tests for semantic analysis of use clauses."""

    def test_use_makes_visible(self):
        """Test that use clause makes names directly visible."""
        source = """
        package Pkg is
            X : constant Integer := 42;
        end Pkg;

        with Pkg;
        use Pkg;

        procedure Test is
            Y : Integer;
        begin
            Y := X;  -- X visible without Pkg prefix
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_use_in_package(self):
        """Test use clause inside package."""
        source = """
        package Helper is
            C : constant Integer := 42;
        end Helper;

        with Helper;
        use Helper;

        package Main is
            X : Integer := C;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestUseClauseUseCases:
    """Tests for common use clause use cases."""

    def test_standard_library_use(self):
        """Test using standard library packages."""
        source = """
        with Ada.Text_IO;
        use Ada.Text_IO;

        procedure Hello is
        begin
            null;
        end Hello;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_local_package_use(self):
        """Test use clause with local package."""
        source = """
        package Utils is
            C : constant Integer := 1;
        end Utils;

        with Utils;
        use Utils;

        procedure Test is
            X : Integer;
        begin
            X := C;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_multiple_with(self):
        """Test multiple with clauses."""
        source = """
        package A is
            X : constant Integer := 1;
        end A;

        package B is
            Y : constant Integer := 2;
        end B;

        with A;
        with B;
        use A;
        use B;

        procedure Test is
            Z : Integer;
        begin
            Z := X + Y;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
