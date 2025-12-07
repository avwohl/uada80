"""Integration tests for complete Ada programs."""

import pytest
from uada80.compiler import compile_source, Compiler, OutputFormat


# ============================================================================
# Complete Program Tests
# ============================================================================


def test_hello_world_procedure():
    """Test minimal procedure."""
    source = """
    procedure Hello is
    begin
        null;
    end Hello;
    """

    result = compile_source(source)
    assert result.success
    assert "ld" in result.output.lower() or "ret" in result.output.lower()


def test_simple_calculator():
    """Test simple arithmetic operations."""
    source = """
    procedure Calculator is
        A : Integer := 10;
        B : Integer := 5;
        Sum : Integer;
        Diff : Integer;
        Prod : Integer;
        Quot : Integer;
    begin
        Sum := A + B;
        Diff := A - B;
        Prod := A * B;
        Quot := A / B;
    end Calculator;
    """

    result = compile_source(source)
    assert result.success


def test_temperature_converter():
    """Test a temperature conversion function."""
    source = """
    procedure Temperature is
        function Celsius_To_Fahrenheit(C : Integer) return Integer is
        begin
            return (C * 9) / 5 + 32;
        end Celsius_To_Fahrenheit;

        Temp_C : Integer := 100;
        Temp_F : Integer;
    begin
        Temp_F := Celsius_To_Fahrenheit(Temp_C);
    end Temperature;
    """

    result = compile_source(source)
    assert result.success


def test_array_sum():
    """Test summing array elements."""
    source = """
    procedure Array_Sum is
        type Int_Array is array (1 .. 10) of Integer;
        Data : Int_Array;
        Sum : Integer := 0;
    begin
        for I in 1 .. 10 loop
            Data(I) := I;
        end loop;

        for I in 1 .. 10 loop
            Sum := Sum + Data(I);
        end loop;
    end Array_Sum;
    """

    result = compile_source(source)
    assert result.success


def test_factorial_program():
    """Test complete factorial program."""
    source = """
    procedure Factorial_Program is
        function Factorial(N : Integer) return Integer is
            Result : Integer := 1;
        begin
            for I in 2 .. N loop
                Result := Result * I;
            end loop;
            return Result;
        end Factorial;

        F5 : Integer;
        F10 : Integer;
    begin
        F5 := Factorial(5);
        F10 := Factorial(10);
    end Factorial_Program;
    """

    result = compile_source(source)
    assert result.success


def test_prime_checker():
    """Test prime number checker."""
    source = """
    procedure Prime_Checker is
        function Is_Prime(N : Integer) return Boolean is
            I : Integer := 2;
        begin
            if N < 2 then
                return False;
            end if;

            while I * I <= N loop
                if N / I * I = N then
                    return False;
                end if;
                I := I + 1;
            end loop;

            return True;
        end Is_Prime;

        Result : Boolean;
    begin
        Result := Is_Prime(17);
        Result := Is_Prime(20);
    end Prime_Checker;
    """

    result = compile_source(source)
    assert result.success


def test_gcd_program():
    """Test GCD calculation program."""
    source = """
    procedure GCD_Program is
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

        Result : Integer;
    begin
        Result := GCD(48, 18);
    end GCD_Program;
    """

    result = compile_source(source)
    assert result.success


def test_bubble_sort():
    """Test bubble sort implementation."""
    source = """
    procedure Bubble_Sort is
        type Arr is array (1 .. 5) of Integer;
        Data : Arr;
        Temp : Integer;
        Swapped : Boolean;
    begin
        Data(1) := 5;
        Data(2) := 3;
        Data(3) := 8;
        Data(4) := 1;
        Data(5) := 2;

        loop
            Swapped := False;
            for I in 1 .. 4 loop
                if Data(I) > Data(I + 1) then
                    Temp := Data(I);
                    Data(I) := Data(I + 1);
                    Data(I + 1) := Temp;
                    Swapped := True;
                end if;
            end loop;
            exit when not Swapped;
        end loop;
    end Bubble_Sort;
    """

    result = compile_source(source)
    assert result.success


def test_binary_search():
    """Test binary search implementation."""
    source = """
    procedure Binary_Search is
        type Arr is array (1 .. 10) of Integer;

        function Search(Data : Arr; Target : Integer) return Integer is
            Low : Integer := 1;
            High : Integer := 10;
            Mid : Integer;
        begin
            while Low <= High loop
                Mid := (Low + High) / 2;
                if Data(Mid) = Target then
                    return Mid;
                elsif Data(Mid) < Target then
                    Low := Mid + 1;
                else
                    High := Mid - 1;
                end if;
            end loop;
            return 0;
        end Search;

        Data : Arr;
        Index : Integer;
    begin
        for I in 1 .. 10 loop
            Data(I) := I * 10;
        end loop;
        Index := Search(Data, 50);
    end Binary_Search;
    """

    result = compile_source(source)
    assert result.success


def test_matrix_operations():
    """Test matrix operations."""
    source = """
    procedure Matrix_Ops is
        type Matrix is array (1 .. 3, 1 .. 3) of Integer;
        A : Matrix;
        B : Matrix;
        C : Matrix;
    begin
        for I in 1 .. 3 loop
            for J in 1 .. 3 loop
                A(I, J) := I + J;
                B(I, J) := I * J;
            end loop;
        end loop;

        for I in 1 .. 3 loop
            for J in 1 .. 3 loop
                C(I, J) := A(I, J) + B(I, J);
            end loop;
        end loop;
    end Matrix_Ops;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Record and Type Tests
# ============================================================================


def test_point_record():
    """Test record type for 2D point."""
    source = """
    procedure Point_Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;

        P1 : Point;
        P2 : Point;

        function Distance_Squared(A, B : Point) return Integer is
            DX : Integer;
            DY : Integer;
        begin
            DX := B.X - A.X;
            DY := B.Y - A.Y;
            return DX * DX + DY * DY;
        end Distance_Squared;

        D : Integer;
    begin
        P1.X := 0;
        P1.Y := 0;
        P2.X := 3;
        P2.Y := 4;
        D := Distance_Squared(P1, P2);
    end Point_Test;
    """

    result = compile_source(source)
    assert result.success


def test_complex_number():
    """Test complex number operations."""
    source = """
    procedure Complex_Test is
        type Complex is record
            Re : Integer;
            Im : Integer;
        end record;

        function Add(A, B : Complex) return Complex is
            Result : Complex;
        begin
            Result.Re := A.Re + B.Re;
            Result.Im := A.Im + B.Im;
            return Result;
        end Add;

        function Multiply(A, B : Complex) return Complex is
            Result : Complex;
        begin
            Result.Re := A.Re * B.Re - A.Im * B.Im;
            Result.Im := A.Re * B.Im + A.Im * B.Re;
            return Result;
        end Multiply;

        C1, C2, Sum, Product : Complex;
    begin
        C1.Re := 3;
        C1.Im := 4;
        C2.Re := 1;
        C2.Im := 2;
        Sum := Add(C1, C2);
        Product := Multiply(C1, C2);
    end Complex_Test;
    """

    result = compile_source(source)
    assert result.success


def test_enumeration_usage():
    """Test enumeration type usage."""
    source = """
    procedure Enum_Test is
        type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

        function Is_Weekend(D : Day) return Boolean is
        begin
            return D = Sat or D = Sun;
        end Is_Weekend;

        Today : Day;
        Weekend : Boolean;
    begin
        Today := Sat;
        Weekend := Is_Weekend(Today);
    end Enum_Test;
    """

    result = compile_source(source)
    assert result.success


def test_modular_byte_operations():
    """Test modular type for byte operations."""
    source = """
    procedure Byte_Ops is
        type Byte is mod 256;

        function High_Nibble(B : Byte) return Byte is
        begin
            return B / 16;
        end High_Nibble;

        function Low_Nibble(B : Byte) return Byte is
            Mask : Byte := 15;
        begin
            return B and Mask;
        end Low_Nibble;

        function Combine_Nibbles(High, Low : Byte) return Byte is
        begin
            return High * 16 + Low;
        end Combine_Nibbles;

        B : Byte := 171;
        H, L, Combined : Byte;
    begin
        H := High_Nibble(B);
        L := Low_Nibble(B);
        Combined := Combine_Nibbles(H, L);
    end Byte_Ops;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Control Flow Integration Tests
# ============================================================================


def test_state_machine():
    """Test state machine implementation."""
    source = """
    procedure State_Machine is
        type State is (Idle, Running, Paused, Stopped);
        Current : State := Idle;
        Event : Integer;
    begin
        Event := 1;

        case Current is
            when Idle =>
                if Event = 1 then
                    Current := Running;
                end if;
            when Running =>
                if Event = 2 then
                    Current := Paused;
                elsif Event = 3 then
                    Current := Stopped;
                end if;
            when Paused =>
                if Event = 1 then
                    Current := Running;
                elsif Event = 3 then
                    Current := Stopped;
                end if;
            when Stopped =>
                null;
        end case;
    end State_Machine;
    """

    result = compile_source(source)
    assert result.success


def test_menu_selection():
    """Test menu-style case statement."""
    source = """
    procedure Menu is
        Choice : Integer := 2;
        Result : Integer;
    begin
        case Choice is
            when 1 => Result := 100;
            when 2 => Result := 200;
            when 3 => Result := 300;
            when 4 | 5 => Result := 400;
            when 6 .. 10 => Result := 500;
            when others => Result := 0;
        end case;
    end Menu;
    """

    result = compile_source(source)
    assert result.success


def test_nested_control():
    """Test deeply nested control structures."""
    source = """
    procedure Nested_Control is
        A, B, C : Integer := 0;
    begin
        for I in 1 .. 3 loop
            if I > 1 then
                for J in 1 .. 3 loop
                    if J > 1 then
                        while C < 5 loop
                            C := C + 1;
                            if C = 3 then
                                exit;
                            end if;
                        end loop;
                        B := B + J;
                    end if;
                end loop;
            end if;
            A := A + I;
        end loop;
    end Nested_Control;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Multiple Subprogram Tests
# ============================================================================


def test_utility_library():
    """Test multiple utility functions."""
    source = """
    procedure Utility_Library is
        function Min(A, B : Integer) return Integer is
        begin
            if A < B then
                return A;
            else
                return B;
            end if;
        end Min;

        function Max(A, B : Integer) return Integer is
        begin
            if A > B then
                return A;
            else
                return B;
            end if;
        end Max;

        function Clamp(Value, Low, High : Integer) return Integer is
        begin
            return Max(Low, Min(Value, High));
        end Clamp;

        function Abs_Value(X : Integer) return Integer is
        begin
            if X < 0 then
                return 0 - X;
            else
                return X;
            end if;
        end Abs_Value;

        A, B, C, D : Integer;
    begin
        A := Min(10, 20);
        B := Max(10, 20);
        C := Clamp(50, 0, 100);
        D := Abs_Value(5);
    end Utility_Library;
    """

    result = compile_source(source)
    assert result.success


def test_math_library():
    """Test mathematical functions."""
    source = """
    procedure Math_Library is
        function Power(Base, Exp : Integer) return Integer is
            Result : Integer := 1;
        begin
            for I in 1 .. Exp loop
                Result := Result * Base;
            end loop;
            return Result;
        end Power;

        function Sum_Of_Squares(N : Integer) return Integer is
            Sum : Integer := 0;
        begin
            for I in 1 .. N loop
                Sum := Sum + I * I;
            end loop;
            return Sum;
        end Sum_Of_Squares;

        function Triangular(N : Integer) return Integer is
        begin
            return N * (N + 1) / 2;
        end Triangular;

        P, S, T : Integer;
    begin
        P := Power(2, 10);
        S := Sum_Of_Squares(10);
        T := Triangular(100);
    end Math_Library;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Code Generation Verification Tests
# ============================================================================


def test_generates_valid_z80():
    """Test that output contains valid Z80 instructions."""
    source = """
    procedure Test is
        X : Integer := 42;
    begin
        X := X + 1;
    end Test;
    """

    result = compile_source(source)
    assert result.success

    output = result.output.lower()
    # Check for common Z80 instructions
    has_z80 = any(instr in output for instr in [
        "ld", "add", "sub", "inc", "dec", "push", "pop",
        "call", "ret", "jp", "jr"
    ])
    assert has_z80


def test_ir_output_format():
    """Test IR output generation."""
    source = """
    procedure Test is
        A : Integer := 10;
        B : Integer := 20;
        C : Integer;
    begin
        C := A + B;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # IR should contain basic operations
    ir = result.output.lower()
    assert "mov" in ir or "store" in ir or "add" in ir


def test_assembly_has_labels():
    """Test that assembly output has proper labels."""
    source = """
    procedure Main is
        function Helper return Integer is
        begin
            return 42;
        end Helper;
        X : Integer;
    begin
        X := Helper;
    end Main;
    """

    result = compile_source(source)
    assert result.success
    # Should have labels for functions/procedures
    assert ":" in result.output


# ============================================================================
# Edge Case Tests
# ============================================================================


def test_empty_procedure():
    """Test procedure with only null statement."""
    source = """
    procedure Empty is
    begin
        null;
    end Empty;
    """

    result = compile_source(source)
    assert result.success


def test_many_local_variables():
    """Test procedure with many local variables."""
    source = """
    procedure Many_Vars is
        A, B, C, D, E : Integer := 0;
        F, G, H, I, J : Integer := 1;
        K, L, M, N, O : Integer := 2;
        P, Q, R, S, T : Integer := 3;
    begin
        A := B + C;
        D := E + F;
        G := H + I;
        J := K + L;
        M := N + O;
        P := Q + R;
        S := T + A;
    end Many_Vars;
    """

    result = compile_source(source)
    assert result.success


def test_large_array():
    """Test large array declaration."""
    source = """
    procedure Large_Array is
        type Big_Array is array (1 .. 100) of Integer;
        Data : Big_Array;
    begin
        for I in 1 .. 100 loop
            Data(I) := I * 2;
        end loop;
    end Large_Array;
    """

    result = compile_source(source)
    assert result.success


def test_deeply_nested_records():
    """Test nested record types."""
    source = """
    procedure Nested_Records is
        type Inner is record
            Value : Integer;
        end record;

        type Middle is record
            Data : Inner;
            Count : Integer;
        end record;

        type Outer is record
            M : Middle;
            Flag : Boolean;
        end record;

        O : Outer;
    begin
        O.M.Data.Value := 42;
        O.M.Count := 10;
        O.Flag := True;
    end Nested_Records;
    """

    result = compile_source(source)
    assert result.success


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
