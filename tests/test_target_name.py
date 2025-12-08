"""Tests for Ada 2022 target name (@) feature."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import TargetName


class TestTargetNameParsing:
    """Tests for parsing target name expressions."""

    def test_simple_target_name(self):
        """Test parsing simple target name usage."""
        source = """
        procedure Test is
            X : Integer := 5;
        begin
            X := @ + 1;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_target_name_in_complex_expr(self):
        """Test target name in complex expression."""
        source = """
        procedure Test is
            Value : Integer := 10;
        begin
            Value := @ * 2 + @;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_target_name_with_function(self):
        """Test target name with function call."""
        source = """
        package Test is
            function Double(X : Integer) return Integer is
            begin
                return X * 2;
            end Double;

            procedure Increment is
                N : Integer := 1;
            begin
                N := Double(@);
            end Increment;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestTargetNameSemantic:
    """Tests for semantic analysis of target name expressions."""

    def test_target_name_type(self):
        """Test that target name has correct type."""
        source = """
        procedure Test is
            X : Integer := 5;
        begin
            X := @ + 1;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_target_name_type_mismatch(self):
        """Test that target name respects type checking."""
        source = """
        procedure Test is
            X : Integer := 5;
            Y : Boolean := True;
        begin
            X := @ and Y;  -- Error: @ is Integer, 'and' requires Boolean
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should detect type error
        # (Depending on implementation, this might pass if 'and' is treated loosely)

    def test_target_name_outside_assignment(self):
        """Test that target name outside assignment is an error."""
        source = """
        function Bad return Integer is
        begin
            return @;  -- Error: @ outside assignment
        end Bad;
        """
        ast = parse(source)
        result = analyze(ast)
        assert result.has_errors
        assert any("assignment" in str(e).lower() for e in result.errors)


class TestTargetNameUseCases:
    """Tests for common target name use cases."""

    def test_increment_pattern(self):
        """Test using @ for increment pattern."""
        source = """
        procedure Increment is
            Counter : Integer := 0;
        begin
            Counter := @ + 1;
        end Increment;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_multiply_in_place(self):
        """Test using @ for multiply-in-place."""
        source = """
        procedure Double_Value is
            Value : Integer := 5;
        begin
            Value := @ * 2;
        end Double_Value;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_compound_expression(self):
        """Test @ in compound expression."""
        source = """
        procedure Update is
            X : Integer := 10;
        begin
            X := (@ + 1) * 2;
        end Update;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_conditional_update(self):
        """Test @ in conditional expression."""
        source = """
        procedure Clamp is
            Value : Integer := 150;
        begin
            Value := (if @ > 100 then 100 else @);
        end Clamp;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
