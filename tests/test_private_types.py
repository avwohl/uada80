"""Tests for Ada private types."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestPrivateTypeParsing:
    """Tests for parsing private types."""

    def test_simple_private_type(self):
        """Test simple private type declaration."""
        source = """
        package Test is
            type Secret is private;
        private
            type Secret is new Integer;
        end Test;
        """
        ast = parse(source)
        # Should parse private type declaration

    def test_limited_private(self):
        """Test limited private type."""
        source = """
        package Test is
            type Handle is limited private;
        private
            type Handle is new Integer;
        end Test;
        """
        ast = parse(source)
        # Should parse limited private

    def test_tagged_private(self):
        """Test tagged private type."""
        source = """
        package Test is
            type Object is tagged private;
        private
            type Object is tagged record
                Id : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse tagged private

    def test_abstract_tagged_private(self):
        """Test abstract tagged private type."""
        source = """
        package Test is
            type Abstract_Object is abstract tagged private;
        private
            type Abstract_Object is abstract tagged record
                Name : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse abstract tagged private


class TestPrivatePartParsing:
    """Tests for parsing private parts."""

    def test_private_section(self):
        """Test package with private section."""
        source = """
        package Test is
            type Public_Type is new Integer;
        private
            type Internal_Type is new Integer;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_mixed_declarations(self):
        """Test public and private declarations."""
        source = """
        package Test is
            Public_Const : constant Integer := 1;

            procedure Public_Proc;
        private
            Private_Const : constant Integer := 2;

            procedure Private_Proc;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestPrivateTypeOperations:
    """Tests for operations on private types."""

    def test_private_type_operations(self):
        """Test declaring operations on private type."""
        source = """
        package Stack is
            type Stack_Type is private;

            procedure Push(X : Integer);
            function Is_Empty return Boolean;
        private
            type Stack_Type is new Integer;
        end Stack;
        """
        ast = parse(source)
        # Should parse operations on private type


class TestPrivateTypeUseCases:
    """Tests for common private type patterns."""

    def test_opaque_type_pattern(self):
        """Test opaque type pattern."""
        source = """
        package Opaque is
            type Handle is limited private;
        private
            type Handle is new Integer;
        end Opaque;
        """
        ast = parse(source)
        # Should parse opaque type pattern

    def test_private_extension(self):
        """Test private type extension."""
        source = """
        package Test is
            type Base is tagged record
                X : Integer;
            end record;

            type Extended is new Base with private;
        private
            type Extended is new Base with record
                Y : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse private extension
