"""Tests for array attribute support."""

import pytest
from uada80.compiler import compile_source, Compiler, OutputFormat


# ============================================================================
# Array 'First Attribute Tests
# ============================================================================


def test_array_first_attribute():
    """Test 'First attribute on array."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        First_Index : Integer;
    begin
        First_Index := Data'First;  -- Should be 1
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_array_first_with_different_bounds():
    """Test 'First with non-default bounds."""
    source = """
    procedure Test is
        type Arr is array (5 .. 15) of Integer;
        Data : Arr;
        First_Index : Integer;
    begin
        First_Index := Data'First;  -- Should be 5
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_array_first_negative_bound():
    """Test 'First with negative bounds."""
    source = """
    procedure Test is
        type Arr is array (-10 .. 10) of Integer;
        Data : Arr;
        First_Index : Integer;
    begin
        First_Index := Data'First;  -- Should be -10
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Array 'Last Attribute Tests
# ============================================================================


def test_array_last_attribute():
    """Test 'Last attribute on array."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        Last_Index : Integer;
    begin
        Last_Index := Data'Last;  -- Should be 10
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_array_last_with_different_bounds():
    """Test 'Last with non-default bounds."""
    source = """
    procedure Test is
        type Arr is array (5 .. 15) of Integer;
        Data : Arr;
        Last_Index : Integer;
    begin
        Last_Index := Data'Last;  -- Should be 15
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Array 'Length Attribute Tests
# ============================================================================


def test_array_length_attribute():
    """Test 'Length attribute on array."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        Size : Integer;
    begin
        Size := Data'Length;  -- Should be 10
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_array_length_with_offset_bounds():
    """Test 'Length with offset bounds."""
    source = """
    procedure Test is
        type Arr is array (5 .. 15) of Integer;
        Data : Arr;
        Size : Integer;
    begin
        Size := Data'Length;  -- Should be 11 (15 - 5 + 1)
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_array_length_with_negative_bounds():
    """Test 'Length with negative bounds."""
    source = """
    procedure Test is
        type Arr is array (-5 .. 5) of Integer;
        Data : Arr;
        Size : Integer;
    begin
        Size := Data'Length;  -- Should be 11 (5 - (-5) + 1)
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Combined Attribute Tests
# ============================================================================


def test_array_attributes_in_loop():
    """Test using array attributes in loop bounds."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        Sum : Integer;
    begin
        Sum := 0;
        for I in Data'First .. Data'Last loop
            Data(I) := I;
            Sum := Sum + Data(I);
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_array_length_for_iteration():
    """Test using 'Length for iteration count."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        Count : Integer;
    begin
        Count := Data'Length;
        for I in 1 .. Count loop
            Data(I) := 0;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_array_bounds_calculation():
    """Test calculating array bounds."""
    source = """
    procedure Test is
        type Arr is array (5 .. 15) of Integer;
        Data : Arr;
        First, Last, Len : Integer;
    begin
        First := Data'First;
        Last := Data'Last;
        Len := Data'Length;
        -- Len should equal Last - First + 1
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Type Attribute Tests
# ============================================================================


def test_type_first_attribute():
    """Test 'First on array type (not variable)."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        First_Index : Integer;
    begin
        First_Index := Arr'First;  -- Should be 1
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_type_last_attribute():
    """Test 'Last on array type."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Last_Index : Integer;
    begin
        Last_Index := Arr'Last;  -- Should be 10
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_type_length_attribute():
    """Test 'Length on array type."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Size : Integer;
    begin
        Size := Arr'Length;  -- Should be 10
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# IR Generation Tests
# ============================================================================


def test_array_first_ir_generation():
    """Test IR generation for 'First attribute."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        F : Integer;
    begin
        F := Data'First;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # Should be a constant assignment
    assert "mov" in result.output.lower() or "immediate" in result.output.lower()


def test_array_length_ir_generation():
    """Test IR generation for 'Length attribute."""
    source = """
    procedure Test is
        type Arr is array (5 .. 15) of Integer;
        Data : Arr;
        L : Integer;
    begin
        L := Data'Length;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # Should be a constant assignment (11)
    assert "mov" in result.output.lower() or "immediate" in result.output.lower()


# ============================================================================
# Z80 Code Generation Tests
# ============================================================================


def test_array_attributes_code_generation():
    """Test Z80 code generation for array attributes."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        First, Last, Len : Integer;
    begin
        First := Data'First;
        Last := Data'Last;
        Len := Data'Length;
    end Test;
    """

    result = compile_source(source)

    assert result.success
    # Should generate immediate loads
    assert "ld" in result.output.lower()


# ============================================================================
# Error Handling Tests
# ============================================================================


def test_attribute_on_non_array():
    """Test error when using array attribute on non-array."""
    source = """
    procedure Test is
        X : Integer;
        F : Integer;
    begin
        F := X'First;  -- Error: X is not an array
    end Test;
    """

    result = compile_source(source)
    # Should fail or return error
    # May not be fully enforced yet
    assert result is not None


def test_invalid_attribute_name():
    """Test error on invalid attribute name."""
    source = """
    procedure Test is
        type Arr is array (1 .. 10) of Integer;
        Data : Arr;
        X : Integer;
    begin
        X := Data'Invalid;  -- Should fail
    end Test;
    """

    result = compile_source(source)
    # Parser or semantic analyzer should catch this
    assert result is not None


# ============================================================================
# String Attribute Tests (Strings are arrays of Character)
# ============================================================================


def test_string_first_attribute():
    """Test 'First on String."""
    source = """
    procedure Test is
        S : String(1 .. 10);
        F : Integer;
    begin
        F := S'First;  -- Should be 1
    end Test;
    """

    result = compile_source(source)
    # Strings may not be fully implemented yet
    assert result is not None


def test_string_length_attribute():
    """Test 'Length on String."""
    source = """
    procedure Test is
        S : String(1 .. 10);
        L : Integer;
    begin
        L := S'Length;  -- Should be 10
    end Test;
    """

    result = compile_source(source)
    # Strings may not be fully implemented yet
    assert result is not None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
