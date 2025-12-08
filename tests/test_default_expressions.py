"""Tests for Ada default expressions."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestParameterDefaults:
    """Tests for default parameter values."""

    def test_procedure_default_parameter(self):
        """Test procedure with default parameter."""
        source = """
        package Test is
            procedure Print(Value : Integer := 0);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_function_default_parameter(self):
        """Test function with default parameter."""
        source = """
        package Test is
            function Add(A : Integer; B : Integer := 1) return Integer;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_multiple_defaults(self):
        """Test multiple default parameters."""
        source = """
        package Test is
            procedure Configure(
                Width  : Integer := 80;
                Height : Integer := 25;
                Color  : Boolean := True
            );
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestComponentDefaults:
    """Tests for record component defaults."""

    def test_simple_record_default(self):
        """Test record with default component values."""
        source = """
        package Test is
            type Config is record
                Max_Size : Integer := 100;
                Enabled : Boolean := True;
            end record;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_partial_defaults(self):
        """Test record with partial defaults."""
        source = """
        package Test is
            type Point is record
                X : Integer;  -- No default
                Y : Integer := 0;  -- Has default
            end record;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestDiscriminantDefaults:
    """Tests for discriminant defaults."""

    def test_discriminant_default(self):
        """Test discriminant with default value."""
        source = """
        package Test is
            type Buffer(Size : Integer := 100) is record
                Count : Integer := 0;
            end record;

            B1 : Buffer;  -- Uses default size
            B2 : Buffer(200);  -- Explicit size
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestExpressionDefaults:
    """Tests for various default expressions."""

    def test_constant_default(self):
        """Test constant as default."""
        source = """
        package Test is
            Default_Size : constant Integer := 80;
            procedure Set_Size(Size : Integer := Default_Size);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_function_call_default(self):
        """Test function call as default."""
        source = """
        package Test is
            function Get_Default return Integer;

            procedure Process(Value : Integer := Get_Default);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestDefaultUseCases:
    """Tests for common default expression use cases."""

    def test_api_versioning(self):
        """Test defaults for API versioning."""
        source = """
        package API is
            procedure Call(
                Endpoint : Integer;
                Timeout : Integer := 30;
                Retries : Integer := 3
            );
        end API;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_builder_pattern(self):
        """Test defaults in builder pattern."""
        source = """
        package Builder is
            type Config is record
                Host : Integer := 0;
                Port : Integer := 8080;
                Secure : Boolean := False;
            end record;

            function Create(
                C : Config := (Host => 0, Port => 8080, Secure => False)
            ) return Integer;
        end Builder;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
