"""Tests for Ada null procedures (Ada 2005)."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestNullProcedureParsing:
    """Tests for parsing null procedures."""

    def test_simple_null_procedure(self):
        """Test simple null procedure declaration."""
        source = """
        package Test is
            procedure Do_Nothing is null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_null_procedure_with_params(self):
        """Test null procedure with parameters."""
        source = """
        package Test is
            procedure Ignore(X : Integer) is null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_multiple_null_procedures(self):
        """Test multiple null procedures."""
        source = """
        package Test is
            procedure First is null;
            procedure Second is null;
            procedure Third is null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestNullProcedureSemantics:
    """Tests for null procedure semantics."""

    def test_null_procedure_callable(self):
        """Test calling a null procedure."""
        source = """
        procedure Test is
            procedure Skip is null;
        begin
            Skip;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_null_procedure_in_block(self):
        """Test null procedure in declarative region."""
        source = """
        procedure Test is
            procedure Local_Null is null;
        begin
            Local_Null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestNullProcedureUseCases:
    """Tests for common null procedure use cases."""

    def test_interface_stub(self):
        """Test null procedure as interface stub."""
        source = """
        package Interface_Stubs is
            procedure On_Connect is null;
            procedure On_Disconnect is null;
            procedure On_Error(Code : Integer) is null;
        end Interface_Stubs;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_placeholder_implementation(self):
        """Test null procedure as placeholder."""
        source = """
        package Placeholder is
            procedure Initialize is null;
            procedure Finalize is null;
        end Placeholder;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_callback_default(self):
        """Test null procedure as callback default."""
        source = """
        package Callbacks is
            procedure Default_Handler is null;
            procedure Default_Logger(Msg : Integer) is null;
        end Callbacks;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_multiple_params_null(self):
        """Test null procedure with multiple parameters."""
        source = """
        package Test is
            procedure Multi(A, B, C : Integer) is null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
