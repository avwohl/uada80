"""Tests for control flow statements."""

import pytest
from uada80.compiler import compile_source, Compiler, OutputFormat


# ============================================================================
# If Statement Tests
# ============================================================================


def test_simple_if():
    """Test simple if statement."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        if X > 5 then
            X := 0;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_if_else():
    """Test if-else statement."""
    source = """
    procedure Test is
        X : Integer := 10;
        Y : Integer;
    begin
        if X > 5 then
            Y := 1;
        else
            Y := 0;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_if_elsif():
    """Test if-elsif statement."""
    source = """
    procedure Test is
        X : Integer := 10;
        Y : Integer;
    begin
        if X > 100 then
            Y := 3;
        elsif X > 50 then
            Y := 2;
        elsif X > 10 then
            Y := 1;
        else
            Y := 0;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_nested_if():
    """Test nested if statements."""
    source = """
    procedure Test is
        X : Integer := 10;
        Y : Integer := 20;
        Z : Integer;
    begin
        if X > 5 then
            if Y > 15 then
                Z := 1;
            else
                Z := 2;
            end if;
        else
            Z := 0;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_if_with_and():
    """Test if with boolean AND."""
    source = """
    procedure Test is
        X : Integer := 10;
        Y : Integer := 20;
        Z : Integer;
    begin
        if X > 5 and Y > 15 then
            Z := 1;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_if_with_or():
    """Test if with boolean OR."""
    source = """
    procedure Test is
        X : Integer := 10;
        Y : Integer := 20;
        Z : Integer;
    begin
        if X > 100 or Y > 15 then
            Z := 1;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_if_with_not():
    """Test if with boolean NOT."""
    source = """
    procedure Test is
        X : Integer := 10;
        Flag : Boolean := True;
        Y : Integer;
    begin
        if not Flag then
            Y := 0;
        else
            Y := 1;
        end if;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Loop Statement Tests
# ============================================================================


def test_simple_loop():
    """Test simple loop with exit."""
    source = """
    procedure Test is
        I : Integer := 0;
    begin
        loop
            I := I + 1;
            exit when I >= 10;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_while_loop():
    """Test while loop."""
    source = """
    procedure Test is
        I : Integer := 0;
    begin
        while I < 10 loop
            I := I + 1;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_for_loop_ascending():
    """Test for loop ascending."""
    source = """
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in 1 .. 10 loop
            Sum := Sum + I;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_for_loop_reverse():
    """Test for loop in reverse."""
    source = """
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in reverse 1 .. 10 loop
            Sum := Sum + I;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_nested_loops():
    """Test nested loops."""
    source = """
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in 1 .. 5 loop
            for J in 1 .. 5 loop
                Sum := Sum + I * J;
            end loop;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_loop_with_if():
    """Test loop with if inside."""
    source = """
    procedure Test is
        Sum : Integer := 0;
    begin
        for I in 1 .. 20 loop
            if I > 10 then
                Sum := Sum + I;
            end if;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_exit_without_when():
    """Test exit without when clause."""
    source = """
    procedure Test is
        I : Integer := 0;
    begin
        loop
            I := I + 1;
            if I >= 10 then
                exit;
            end if;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_while_loop_never_enters():
    """Test while loop that never enters."""
    source = """
    procedure Test is
        X : Integer := 0;
    begin
        while X > 0 loop
            X := X - 1;
        end loop;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Case Statement Tests
# ============================================================================


def test_simple_case():
    """Test simple case statement."""
    source = """
    procedure Test is
        X : Integer := 2;
        Y : Integer;
    begin
        case X is
            when 1 => Y := 10;
            when 2 => Y := 20;
            when 3 => Y := 30;
            when others => Y := 0;
        end case;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_case_with_ranges():
    """Test case with range choices."""
    source = """
    procedure Test is
        X : Integer := 5;
        Y : Integer;
    begin
        case X is
            when 1 .. 3 => Y := 1;
            when 4 .. 6 => Y := 2;
            when 7 .. 9 => Y := 3;
            when others => Y := 0;
        end case;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_case_with_multiple_choices():
    """Test case with multiple choices per branch."""
    source = """
    procedure Test is
        X : Integer := 5;
        Y : Integer;
    begin
        case X is
            when 1 | 3 | 5 | 7 | 9 => Y := 1;
            when 2 | 4 | 6 | 8 | 10 => Y := 2;
            when others => Y := 0;
        end case;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_case_on_boolean():
    """Test case on boolean."""
    source = """
    procedure Test is
        Flag : Boolean := True;
        X : Integer;
    begin
        case Flag is
            when True => X := 1;
            when False => X := 0;
        end case;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Block Statement Tests
# ============================================================================


def test_simple_block():
    """Test simple block statement."""
    source = """
    procedure Test is
    begin
        declare
            X : Integer := 10;
        begin
            X := X + 1;
        end;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_nested_blocks():
    """Test nested block statements."""
    source = """
    procedure Test is
    begin
        declare
            X : Integer := 10;
        begin
            declare
                Y : Integer := X + 5;
            begin
                X := Y;
            end;
        end;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Return Statement Tests
# ============================================================================


def test_early_return():
    """Test early return from function."""
    source = """
    function Test(X : Integer) return Integer is
    begin
        if X < 0 then
            return 0;
        end if;
        return X * 2;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_return_in_loop():
    """Test return inside loop."""
    source = """
    function Find_First_Positive(A, B, C : Integer) return Integer is
    begin
        if A > 0 then
            return A;
        end if;
        if B > 0 then
            return B;
        end if;
        if C > 0 then
            return C;
        end if;
        return 0;
    end Find_First_Positive;
    """

    result = compile_source(source)
    assert result.success


def test_procedure_return():
    """Test return from procedure."""
    source = """
    procedure Test(X : Integer) is
    begin
        if X < 0 then
            return;
        end if;
        null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Complex Control Flow Tests
# ============================================================================


def test_factorial_iterative():
    """Test iterative factorial."""
    source = """
    function Factorial(N : Integer) return Integer is
        Result : Integer := 1;
    begin
        for I in 2 .. N loop
            Result := Result * I;
        end loop;
        return Result;
    end Factorial;
    """

    result = compile_source(source)
    assert result.success


def test_fibonacci_iterative():
    """Test iterative fibonacci."""
    source = """
    function Fibonacci(N : Integer) return Integer is
        A : Integer := 0;
        B : Integer := 1;
        Temp : Integer;
    begin
        if N <= 0 then
            return 0;
        elsif N = 1 then
            return 1;
        end if;

        for I in 2 .. N loop
            Temp := A + B;
            A := B;
            B := Temp;
        end loop;

        return B;
    end Fibonacci;
    """

    result = compile_source(source)
    assert result.success


def test_bubble_sort_inner_loop():
    """Test bubble sort style nested loop."""
    source = """
    procedure Sort is
        type Arr is array (1 .. 5) of Integer;
        Data : Arr;
        Temp : Integer;
    begin
        for I in 1 .. 4 loop
            for J in 1 .. 4 loop
                if Data(J) > Data(J + 1) then
                    Temp := Data(J);
                    Data(J) := Data(J + 1);
                    Data(J + 1) := Temp;
                end if;
            end loop;
        end loop;
    end Sort;
    """

    result = compile_source(source)
    assert result.success


def test_complex_boolean_condition():
    """Test complex boolean expression in condition."""
    source = """
    function Check(A, B, C, D : Integer) return Boolean is
    begin
        return (A > 0 and B > 0) or (C < 0 and D < 0);
    end Check;
    """

    result = compile_source(source)
    assert result.success


def test_gcd_algorithm():
    """Test GCD algorithm implementation."""
    source = """
    function GCD(A, B : Integer) return Integer is
        X : Integer := A;
        Y : Integer := B;
        Temp : Integer;
    begin
        while Y /= 0 loop
            Temp := Y;
            Y := X - (X / Y) * Y;
            X := Temp;
        end loop;
        return X;
    end GCD;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# IR Generation Tests for Control Flow
# ============================================================================


def test_if_generates_jumps():
    """Test if statement generates jump instructions in IR."""
    source = """
    procedure Test is
        X : Integer := 10;
    begin
        if X > 5 then
            X := 0;
        end if;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # IR should contain conditional jump
    ir_lower = result.output.lower()
    assert "jz" in ir_lower or "jnz" in ir_lower or "jmp" in ir_lower


def test_loop_generates_jumps():
    """Test loop generates jump instructions in IR."""
    source = """
    procedure Test is
        I : Integer := 0;
    begin
        while I < 10 loop
            I := I + 1;
        end loop;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    assert "jmp" in result.output.lower()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
