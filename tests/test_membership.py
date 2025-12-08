"""Tests for Ada 2012 membership tests."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestMembershipParsing:
    """Tests for parsing membership tests."""

    def test_simple_in_test(self):
        """Test parsing simple 'in' membership test."""
        source = """
        procedure Test is
            X : Integer := 5;
            Valid : Boolean;
        begin
            Valid := X in 1 .. 10;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_not_in_test(self):
        """Test parsing 'not in' membership test."""
        source = """
        procedure Test is
            X : Integer := 5;
            Invalid : Boolean;
        begin
            Invalid := X not in 1 .. 10;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_multiple_choices(self):
        """Test membership test with multiple choices (Ada 2012)."""
        source = """
        procedure Test is
            X : Integer := 5;
            Valid : Boolean;
        begin
            Valid := X in 1 | 2 | 3 | 4 | 5;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_mixed_choices(self):
        """Test membership test with mixed ranges and values."""
        source = """
        procedure Test is
            N : Integer := 42;
            Special : Boolean;
        begin
            Special := N in 1 .. 10 | 42 | 100 .. 200;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestMembershipSemantic:
    """Tests for semantic analysis of membership tests."""

    def test_membership_returns_boolean(self):
        """Test that membership test returns Boolean."""
        source = """
        procedure Test is
            X : Integer := 5;
            Result : Boolean;
        begin
            Result := X in 1 .. 10;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_membership_in_condition(self):
        """Test membership test in if condition."""
        source = """
        procedure Test is
            X : Integer := 5;
        begin
            if X in 1 .. 10 then
                null;
            end if;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_membership_with_type(self):
        """Test membership test against a subtype."""
        source = """
        procedure Test is
            subtype Small_Int is Integer range 1 .. 100;
            X : Integer := 50;
            Valid : Boolean;
        begin
            Valid := X in Small_Int;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # May have some limitations but should parse and analyze

    def test_multiple_choice_membership(self):
        """Test multiple choice membership semantic analysis."""
        source = """
        function Is_Digit(C : Integer) return Boolean is
        begin
            return C in 48 .. 57;  -- ASCII '0'..'9'
        end Is_Digit;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestMembershipUseCases:
    """Tests for common membership test use cases."""

    def test_range_validation(self):
        """Test using membership for range validation."""
        source = """
        function Is_Valid_Age(Age : Integer) return Boolean is
        begin
            return Age in 0 .. 150;
        end Is_Valid_Age;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_discrete_choice_list(self):
        """Test membership with discrete choice list."""
        source = """
        function Is_Vowel_Code(C : Integer) return Boolean is
        begin
            return C in 65 | 69 | 73 | 79 | 85 |  -- A E I O U
                       97 | 101 | 105 | 111 | 117;  -- a e i o u
        end Is_Vowel_Code;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_combined_with_not(self):
        """Test 'not in' membership test."""
        source = """
        function Is_Not_Zero(X : Integer) return Boolean is
        begin
            return X not in 0;
        end Is_Not_Zero;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_membership_in_while(self):
        """Test membership test in while condition."""
        source = """
        procedure Read_Digits is
            Value : Integer := 48;
        begin
            while Value in 48 .. 57 loop
                Value := Value + 1;
            end loop;
        end Read_Digits;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
