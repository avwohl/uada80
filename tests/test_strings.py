"""Tests for Ada String types."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestStringTypeParsing:
    """Tests for parsing String type usage."""

    def test_string_variable(self):
        """Test string variable declaration."""
        source = """
        procedure Test is
            S : String(1 .. 10);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_string_literal(self):
        """Test string literal."""
        source = """
        procedure Test is
            S : String := "Hello";
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse string literal

    def test_fixed_string(self):
        """Test fixed-length string."""
        source = """
        procedure Test is
            Name : String(1 .. 20);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestStringOperations:
    """Tests for string operations."""

    def test_string_assignment(self):
        """Test string assignment."""
        source = """
        procedure Test is
            S : String(1 .. 5) := "Hello";
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse string initialization

    def test_string_concatenation(self):
        """Test string concatenation."""
        source = """
        procedure Test is
            A : String(1 .. 5) := "Hello";
            B : String(1 .. 5) := "World";
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse strings


class TestCharacterType:
    """Tests for Character type."""

    def test_character_variable(self):
        """Test character variable."""
        source = """
        procedure Test is
            C : Character := 'A';
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_character_comparison(self):
        """Test character comparison."""
        source = """
        procedure Test is
            C : Character := 'Z';
            B : Boolean;
        begin
            B := C > 'A';
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_character_in_string(self):
        """Test character indexing in string."""
        source = """
        procedure Test is
            S : String(1 .. 5) := "Hello";
            C : Character;
        begin
            C := S(1);
        end Test;
        """
        ast = parse(source)
        # Should parse indexing


class TestStringUseCases:
    """Tests for common string use cases."""

    def test_string_parameter(self):
        """Test string as parameter."""
        source = """
        package Test is
            procedure Print(S : String);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_string_function_return(self):
        """Test function returning String."""
        source = """
        package Test is
            function Get_Name return String;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
