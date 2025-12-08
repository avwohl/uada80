"""Tests for Ada child packages."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestChildPackageParsing:
    """Tests for parsing child packages."""

    def test_simple_child_package(self):
        """Test parsing simple child package."""
        source = """
        package Parent is
            X : Integer := 0;
        end Parent;

        package Parent.Child is
            Y : Integer := 1;
        end Parent.Child;
        """
        ast = parse(source)
        # Should parse multiple units without errors

    def test_nested_child_package(self):
        """Test parsing deeply nested child package."""
        source = """
        package Root is
            X : Integer := 0;
        end Root;

        package Root.Level1 is
            Y : Integer := 1;
        end Root.Level1;

        package Root.Level1.Level2 is
            Z : Integer := 2;
        end Root.Level1.Level2;
        """
        ast = parse(source)
        # Should parse nested child packages

    def test_child_package_with_types(self):
        """Test child package with types."""
        source = """
        package Types is
            type Base is tagged null record;
        end Types;

        package Types.Extended is
            type Derived is new Base with record
                Value : Integer;
            end record;
        end Types.Extended;
        """
        ast = parse(source)
        # Should parse without errors


class TestChildPackageSemantic:
    """Tests for semantic analysis of child packages."""

    def test_child_sees_parent_declarations(self):
        """Test that child can see parent's public declarations."""
        source = """
        package Parent is
            X : constant Integer := 42;
        end Parent;

        package Parent.Child is
            Y : Integer := X;
        end Parent.Child;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_child_with_use_parent(self):
        """Test child package using parent."""
        source = """
        package Math is
            type Vector is record
                X, Y : Integer;
            end record;
        end Math;

        package Math.Operations is
            function Add(A, B : Vector) return Vector;
        end Math.Operations;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestPrivateChildPackages:
    """Tests for private child packages."""

    def test_private_child_declaration(self):
        """Test private child package declaration."""
        source = """
        package Public_Parent is
            procedure External_API;
        end Public_Parent;

        private package Public_Parent.Private_Child is
            procedure Internal_Helper;
        end Public_Parent.Private_Child;
        """
        ast = parse(source)
        # Should parse without errors

    def test_private_child_sees_parent_private(self):
        """Test private child can see parent's private part."""
        source = """
        package Parent is
            type T is private;
        private
            type T is new Integer;
        end Parent;

        private package Parent.Helper is
            X : T;  -- Can access T because this is private child
        end Parent.Helper;
        """
        ast = parse(source)
        # Should parse without errors


class TestChildSubprograms:
    """Tests for child subprograms."""

    def test_child_procedure(self):
        """Test child procedure declaration."""
        source = """
        package Parent is
            X : Integer := 0;
        end Parent;

        procedure Parent.Child_Proc is
        begin
            X := X + 1;
        end Parent.Child_Proc;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_child_function(self):
        """Test child function declaration."""
        source = """
        package Parent is
            Value : Integer := 42;
        end Parent;

        function Parent.Get_Value return Integer is
        begin
            return Value;
        end Parent.Get_Value;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestChildPackageUseCases:
    """Tests for common child package use cases."""

    def test_library_organization(self):
        """Test library organization with child packages."""
        source = """
        package My_Library is
            Version : constant Integer := 1;
        end My_Library;

        package My_Library.Utilities is
            procedure Helper;
        end My_Library.Utilities;

        package My_Library.IO is
            procedure Read;
            procedure Write;
        end My_Library.IO;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_generic_child(self):
        """Test generic child package."""
        source = """
        package Containers is
            type Element is new Integer;
        end Containers;

        generic
            type Item is private;
        package Containers.Lists is
            procedure Add(X : Item);
        end Containers.Lists;
        """
        ast = parse(source)
        # Should parse without errors

    def test_child_package_body(self):
        """Test child package body."""
        source = """
        package Parent is
            procedure Proc;
        end Parent;

        package body Parent is
            procedure Proc is
            begin
                null;
            end Proc;
        end Parent;

        package Parent.Child is
            procedure Child_Proc;
        end Parent.Child;

        package body Parent.Child is
            procedure Child_Proc is
            begin
                null;
            end Child_Proc;
        end Parent.Child;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
