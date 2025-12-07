"""Tests for modular type support."""

import pytest
from uada80.compiler import compile_source, Compiler, OutputFormat


# ============================================================================
# Basic Modular Type Tests
# ============================================================================


def test_simple_modular_declaration():
    """Test simple modular type declaration."""
    source = """
    procedure Test is
        type Byte is mod 256;
        B : Byte;
    begin
        null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_word_declaration():
    """Test 16-bit modular type declaration."""
    source = """
    procedure Test is
        type Word is mod 65536;
        W : Word;
    begin
        null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_assignment():
    """Test assigning values to modular types."""
    source = """
    procedure Test is
        type Byte is mod 256;
        B : Byte;
    begin
        B := 0;
        B := 255;
        B := 100;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_arithmetic():
    """Test arithmetic operations on modular types."""
    source = """
    procedure Test is
        type Byte is mod 256;
        A, B, C : Byte;
    begin
        A := 100;
        B := 50;
        C := A + B;
        C := A - B;
        C := A * B;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_comparison():
    """Test comparing modular values."""
    source = """
    procedure Test is
        type Byte is mod 256;
        A, B : Byte;
        Result : Boolean;
    begin
        A := 100;
        B := 100;
        Result := A = B;
        Result := A < B;
        Result := A > B;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_in_if_statement():
    """Test using modular types in conditionals."""
    source = """
    procedure Test is
        type Byte is mod 256;
        X : Byte;
    begin
        X := 128;
        if X > 100 then
            X := 0;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Wraparound Semantics Tests
# ============================================================================


def test_modular_wraparound():
    """Test that modular arithmetic wraps around."""
    source = """
    procedure Test is
        type Byte is mod 256;
        B : Byte;
    begin
        B := 255;
        B := B + 1;  -- Should wrap to 0
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_underflow():
    """Test that modular subtraction wraps around."""
    source = """
    procedure Test is
        type Byte is mod 256;
        B : Byte;
    begin
        B := 0;
        B := B - 1;  -- Should wrap to 255
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Bitwise Operations Tests
# ============================================================================


def test_modular_bitwise_and():
    """Test bitwise AND on modular types."""
    source = """
    procedure Test is
        type Byte is mod 256;
        A, B, C : Byte;
    begin
        A := 255;
        B := 15;
        C := A and B;  -- Should be 15
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_bitwise_or():
    """Test bitwise OR on modular types."""
    source = """
    procedure Test is
        type Byte is mod 256;
        A, B, C : Byte;
    begin
        A := 240;
        B := 15;
        C := A or B;  -- Should be 255
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_bitwise_xor():
    """Test bitwise XOR on modular types."""
    source = """
    procedure Test is
        type Byte is mod 256;
        A, B, C : Byte;
    begin
        A := 255;
        B := 15;
        C := A xor B;  -- Should be 240
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_bitwise_not():
    """Test bitwise NOT on modular types."""
    source = """
    procedure Test is
        type Byte is mod 256;
        A, B : Byte;
    begin
        A := 0;
        B := not A;  -- Should be 255
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Function Parameter Tests
# ============================================================================


def test_modular_as_parameter():
    """Test passing modular types as parameters."""
    source = """
    procedure Test is
        type Byte is mod 256;

        procedure Process(Value : Byte) is
        begin
            null;
        end Process;

        B : Byte;
    begin
        B := 42;
        Process(B);
        Process(100);
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_as_return_value():
    """Test returning modular types from functions."""
    source = """
    procedure Test is
        type Byte is mod 256;

        function Double(X : Byte) return Byte is
        begin
            return X * 2;
        end Double;

        B : Byte;
    begin
        B := Double(64);
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Multiple Modular Types Tests
# ============================================================================


def test_multiple_modular_types():
    """Test multiple distinct modular types."""
    source = """
    procedure Test is
        type Byte is mod 256;
        type Word is mod 65536;
        type Nibble is mod 16;

        B : Byte;
        W : Word;
        N : Nibble;
    begin
        B := 100;
        W := 1000;
        N := 10;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# IR Generation Tests
# ============================================================================


def test_modular_ir_generation():
    """Test IR generation for modular operations."""
    source = """
    procedure Test is
        type Byte is mod 256;
        B : Byte;
    begin
        B := 42;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    assert "mov" in result.output.lower() or "store" in result.output.lower()


def test_modular_arithmetic_ir():
    """Test IR generation for modular arithmetic."""
    source = """
    procedure Test is
        type Byte is mod 256;
        A, B, C : Byte;
    begin
        A := 100;
        B := 50;
        C := A + B;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    assert "add" in result.output.lower()


# ============================================================================
# Code Generation Tests
# ============================================================================


def test_modular_code_generation():
    """Test Z80 code generation for modular types."""
    source = """
    procedure Test is
        type Byte is mod 256;
        B : Byte;
    begin
        B := 42;
    end Test;
    """

    result = compile_source(source)

    assert result.success
    assert "ld" in result.output.lower()


# ============================================================================
# Error Handling Tests
# ============================================================================


def test_invalid_modulus_zero():
    """Test error on zero modulus."""
    source = """
    procedure Test is
        type Bad is mod 0;
        B : Bad;
    begin
        null;
    end Test;
    """

    result = compile_source(source)
    assert result.has_errors or not result.success


def test_invalid_modulus_negative():
    """Test error on negative modulus."""
    source = """
    procedure Test is
        type Bad is mod -1;
        B : Bad;
    begin
        null;
    end Test;
    """

    result = compile_source(source)
    assert result.has_errors or not result.success


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
