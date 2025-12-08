"""Tests for Ada exception handling."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestExceptionDeclParsing:
    """Tests for parsing exception declarations."""

    def test_simple_exception(self):
        """Test parsing simple exception declaration."""
        source = """
        package Test is
            My_Error : exception;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_multiple_exceptions(self):
        """Test multiple exception declarations."""
        source = """
        package Errors is
            Input_Error : exception;
            Output_Error : exception;
            Format_Error : exception;
        end Errors;
        """
        ast = parse(source)
        # Should parse without errors


class TestRaiseStatementParsing:
    """Tests for parsing raise statements."""

    def test_simple_raise(self):
        """Test parsing simple raise statement."""
        source = """
        procedure Test is
            My_Error : exception;
        begin
            raise My_Error;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_raise_with_message(self):
        """Test raise with message."""
        source = """
        procedure Test is
            My_Error : exception;
        begin
            raise My_Error with "Something went wrong";
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_reraise(self):
        """Test re-raise (raise without exception name)."""
        source = """
        procedure Test is
            My_Error : exception;
        begin
            null;
        exception
            when others =>
                raise;  -- Re-raise current exception
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestExceptionHandlerParsing:
    """Tests for parsing exception handlers."""

    def test_simple_handler(self):
        """Test parsing simple exception handler."""
        source = """
        procedure Test is
            My_Error : exception;
        begin
            null;
        exception
            when My_Error =>
                null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_multiple_handlers(self):
        """Test multiple exception handlers."""
        source = """
        procedure Test is
            Error1 : exception;
            Error2 : exception;
        begin
            null;
        exception
            when Error1 =>
                null;
            when Error2 =>
                null;
            when others =>
                null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_handler_with_choice_list(self):
        """Test handler with multiple exception choices."""
        source = """
        procedure Test is
            Error1 : exception;
            Error2 : exception;
        begin
            null;
        exception
            when Error1 | Error2 =>
                null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_named_exception_occurrence(self):
        """Test exception handler with named occurrence."""
        source = """
        procedure Test is
            My_Error : exception;
        begin
            null;
        exception
            when E : My_Error =>
                null;  -- E can be used to get exception info
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestExceptionSemantic:
    """Tests for semantic analysis of exception handling."""

    def test_exception_in_scope(self):
        """Test that declared exception is in scope."""
        source = """
        procedure Test is
            My_Error : exception;
        begin
            raise My_Error;
        exception
            when My_Error =>
                null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_predefined_exceptions(self):
        """Test using predefined exceptions."""
        source = """
        procedure Test is
        begin
            null;
        exception
            when Constraint_Error =>
                null;
            when Program_Error =>
                null;
            when Storage_Error =>
                null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestExceptionUseCases:
    """Tests for common exception use cases."""

    def test_cleanup_on_error(self):
        """Test cleanup pattern with exception handler."""
        source = """
        procedure Process is
            Error : exception;
            Count : Integer := 0;
        begin
            Count := Count + 1;
        exception
            when Error =>
                Count := 0;  -- Cleanup
        end Process;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_nested_exception_handling(self):
        """Test nested exception handling."""
        source = """
        procedure Outer is
            Outer_Error : exception;
        begin
            declare
                Inner_Error : exception;
            begin
                raise Inner_Error;
            exception
                when Inner_Error =>
                    raise Outer_Error;
            end;
        exception
            when Outer_Error =>
                null;
        end Outer;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
