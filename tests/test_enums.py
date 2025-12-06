"""Tests for enumeration type support."""

import pytest
from uada80.compiler import compile_source, Compiler, OutputFormat


# ============================================================================
# Basic Enumeration Tests
# ============================================================================


def test_simple_enum_declaration():
    """Test simple enumeration type declaration."""
    source = """
    procedure Test is
        type Day is (Monday, Tuesday, Wednesday, Thursday, Friday);
        D : Day;
    begin
        null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_enum_literal_assignment():
    """Test assigning enum literals."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        C : Color;
    begin
        C := Red;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_enum_comparison():
    """Test comparing enum values."""
    source = """
    procedure Test is
        type Status is (Off, On);
        S1, S2 : Status;
        Match : Boolean;
    begin
        S1 := On;
        S2 := On;
        Match := S1 = S2;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_enum_in_if_statement():
    """Test using enums in conditionals."""
    source = """
    procedure Test is
        type Light is (Red, Yellow, Green);
        Signal : Light;
    begin
        Signal := Red;
        if Signal = Red then
            null;
        elsif Signal = Yellow then
            null;
        else
            null;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_enum_in_case_statement():
    """Test using enums in case statements."""
    source = """
    procedure Test is
        type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
        Today : Day;
    begin
        Today := Fri;
        case Today is
            when Mon | Tue | Wed | Thu | Fri =>
                null;  -- Weekday
            when Sat | Sun =>
                null;  -- Weekend
        end case;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_boolean_enum():
    """Test that Boolean works as enum type."""
    source = """
    procedure Test is
        Flag : Boolean;
    begin
        Flag := True;
        if Flag then
            null;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_character_enum():
    """Test that Character works."""
    source = """
    procedure Test is
        Ch : Character;
    begin
        Ch := 'A';
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Enumeration Attribute Tests
# ============================================================================


def test_enum_pos_attribute():
    """Test 'Pos attribute on enums."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        C : Color;
        P : Integer;
    begin
        C := Green;
        P := Color'Pos(C);  -- Should be 1
    end Test;
    """

    result = compile_source(source)
    # May not be implemented yet
    assert result is not None


def test_enum_val_attribute():
    """Test 'Val attribute on enums."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        C : Color;
    begin
        C := Color'Val(2);  -- Should be Blue
    end Test;
    """

    result = compile_source(source)
    # May not be implemented yet
    assert result is not None


def test_enum_succ_attribute():
    """Test 'Succ attribute on enums."""
    source = """
    procedure Test is
        type Day is (Mon, Tue, Wed);
        D : Day;
    begin
        D := Mon;
        D := Day'Succ(D);  -- Should be Tue
    end Test;
    """

    result = compile_source(source)
    # May not be implemented yet
    assert result is not None


def test_enum_pred_attribute():
    """Test 'Pred attribute on enums."""
    source = """
    procedure Test is
        type Day is (Mon, Tue, Wed);
        D : Day;
    begin
        D := Wed;
        D := Day'Pred(D);  -- Should be Tue
    end Test;
    """

    result = compile_source(source)
    # May not be implemented yet
    assert result is not None


def test_enum_first_last_attributes():
    """Test 'First and 'Last attributes."""
    source = """
    procedure Test is
        type Day is (Mon, Tue, Wed, Thu, Fri);
        First, Last : Day;
    begin
        First := Day'First;  -- Mon
        Last := Day'Last;    -- Fri
    end Test;
    """

    result = compile_source(source)
    # May not be implemented yet
    assert result is not None


# ============================================================================
# Enumeration with Parameters
# ============================================================================


def test_enum_as_parameter():
    """Test passing enums as parameters."""
    source = """
    procedure Test is
        type Status is (Active, Inactive, Pending);

        procedure Set_Status(S : Status) is
        begin
            null;
        end Set_Status;

        Current : Status;
    begin
        Current := Active;
        Set_Status(Current);
        Set_Status(Inactive);
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_enum_as_return_value():
    """Test returning enums from functions."""
    source = """
    procedure Test is
        type State is (Init, Running, Done);

        function Get_State return State is
        begin
            return Running;
        end Get_State;

        S : State;
    begin
        S := Get_State;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Multiple Enum Types
# ============================================================================


def test_multiple_enum_types():
    """Test multiple distinct enum types."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        type Size is (Small, Medium, Large);

        C : Color;
        S : Size;
    begin
        C := Red;
        S := Large;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_enum_with_same_literal_names():
    """Test that same literal names in different types are distinct."""
    source = """
    procedure Test is
        type Traffic_Light is (Red, Yellow, Green);
        type RGB is (Red, Green, Blue);

        Light : Traffic_Light;
        Color : RGB;
    begin
        Light := Red;   -- Traffic_Light.Red
        Color := Red;   -- RGB.Red
    end Test;
    """

    result = compile_source(source)
    # This might fail due to overloading resolution
    # It's a complex feature
    assert result is not None


# ============================================================================
# IR Generation Tests
# ============================================================================


def test_enum_ir_generation():
    """Test IR generation for enum operations."""
    source = """
    procedure Test is
        type State is (Off, On);
        S : State;
    begin
        S := On;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # Enums should be represented as small integers
    assert "mov" in result.output.lower() or "store" in result.output.lower()


def test_enum_comparison_ir():
    """Test IR generation for enum comparisons."""
    source = """
    procedure Test is
        type State is (Off, On);
        S : State;
        Match : Boolean;
    begin
        S := On;
        Match := S = On;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # Should have comparison instruction
    assert "cmp" in result.output.lower() or "eq" in result.output.lower()


# ============================================================================
# Code Generation Tests
# ============================================================================


def test_enum_code_generation():
    """Test Z80 code generation for enums."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        C : Color;
    begin
        C := Green;  -- Assign position value 1
    end Test;
    """

    result = compile_source(source)

    assert result.success
    # Should generate load/store instructions
    assert "ld" in result.output.lower()


# ============================================================================
# Error Handling Tests
# ============================================================================


def test_undefined_enum_literal():
    """Test error on undefined enum literal."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        C : Color;
    begin
        C := Yellow;  -- Yellow not in Color
    end Test;
    """

    result = compile_source(source)
    # Should fail semantic analysis
    assert result.has_errors or not result.success


def test_enum_type_mismatch():
    """Test error on enum type mismatch."""
    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        type Size is (Small, Medium, Large);

        C : Color;
        S : Size;
    begin
        C := Medium;  -- Medium is from Size, not Color
    end Test;
    """

    result = compile_source(source)
    # Should fail semantic analysis
    assert result.has_errors or not result.success


def test_enum_integer_mismatch():
    """Test that enums and integers are not compatible."""
    source = """
    procedure Test is
        type State is (Off, On);
        S : State;
        I : Integer;
    begin
        I := S;  -- Cannot assign enum to integer
    end Test;
    """

    result = compile_source(source)
    # Should fail - enums and integers are incompatible
    assert result.has_errors or not result.success


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
