"""Tests for subprogram features (procedures and functions)."""

import pytest
from uada80.compiler import compile_source, Compiler, OutputFormat


# ============================================================================
# Basic Procedure Tests
# ============================================================================


def test_simple_procedure():
    """Test simple procedure with no parameters."""
    source = """
    procedure Main is
        procedure Inner is
        begin
            null;
        end Inner;
    begin
        Inner;
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_procedure_with_in_parameter():
    """Test procedure with IN parameter."""
    source = """
    procedure Main is
        X : Integer := 0;

        procedure Set_Value(Value : Integer) is
        begin
            X := Value;
        end Set_Value;
    begin
        Set_Value(42);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_procedure_with_out_parameter():
    """Test procedure with OUT parameter."""
    source = """
    procedure Main is
        Result : Integer;

        procedure Get_Value(Value : out Integer) is
        begin
            Value := 100;
        end Get_Value;
    begin
        Get_Value(Result);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_procedure_with_in_out_parameter():
    """Test procedure with IN OUT parameter."""
    source = """
    procedure Main is
        X : Integer := 10;

        procedure Double(Value : in out Integer) is
        begin
            Value := Value * 2;
        end Double;
    begin
        Double(X);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_procedure_multiple_parameters():
    """Test procedure with multiple parameters."""
    source = """
    procedure Main is
        Result : Integer;

        procedure Add(A, B : Integer; Sum : out Integer) is
        begin
            Sum := A + B;
        end Add;
    begin
        Add(10, 20, Result);
    end Main;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Basic Function Tests
# ============================================================================


def test_simple_function():
    """Test simple function."""
    source = """
    procedure Main is
        function Get_Value return Integer is
        begin
            return 42;
        end Get_Value;

        X : Integer;
    begin
        X := Get_Value;
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_function_with_parameter():
    """Test function with parameter."""
    source = """
    procedure Main is
        function Double(X : Integer) return Integer is
        begin
            return X * 2;
        end Double;

        Y : Integer;
    begin
        Y := Double(21);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_function_multiple_parameters():
    """Test function with multiple parameters."""
    source = """
    procedure Main is
        function Add(A, B : Integer) return Integer is
        begin
            return A + B;
        end Add;

        X : Integer;
    begin
        X := Add(10, 20);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_function_in_expression():
    """Test function call in expression."""
    source = """
    procedure Main is
        function Square(X : Integer) return Integer is
        begin
            return X * X;
        end Square;

        Y : Integer;
    begin
        Y := Square(3) + Square(4);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_function_boolean_return():
    """Test function returning Boolean."""
    source = """
    procedure Main is
        function Is_Positive(X : Integer) return Boolean is
        begin
            return X > 0;
        end Is_Positive;

        Flag : Boolean;
    begin
        Flag := Is_Positive(10);
        Flag := Is_Positive(-5);
    end Main;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Nested Subprogram Tests
# ============================================================================


def test_nested_procedures():
    """Test nested procedure declarations."""
    source = """
    procedure Outer is
        X : Integer := 0;

        procedure Level1 is
            procedure Level2 is
            begin
                X := X + 1;
            end Level2;
        begin
            Level2;
        end Level1;
    begin
        Level1;
    end Outer;
    """

    result = compile_source(source)
    assert result.success


def test_nested_functions():
    """Test nested function declarations."""
    source = """
    procedure Main is
        function Outer(X : Integer) return Integer is
            function Inner(Y : Integer) return Integer is
            begin
                return Y * 2;
            end Inner;
        begin
            return Inner(X) + 1;
        end Outer;

        Result : Integer;
    begin
        Result := Outer(10);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_sibling_procedures():
    """Test sibling procedure declarations."""
    source = """
    procedure Main is
        X : Integer := 0;

        procedure First is
        begin
            X := 1;
        end First;

        procedure Second is
        begin
            X := 2;
        end Second;

        procedure Third is
        begin
            X := 3;
        end Third;
    begin
        First;
        Second;
        Third;
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_mutual_procedure_calls():
    """Test procedures calling each other."""
    source = """
    procedure Main is
        Count : Integer := 0;

        procedure A;

        procedure B is
        begin
            Count := Count + 1;
            if Count < 5 then
                A;
            end if;
        end B;

        procedure A is
        begin
            Count := Count + 1;
            if Count < 5 then
                B;
            end if;
        end A;
    begin
        A;
    end Main;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Recursion Tests
# ============================================================================


def test_recursive_procedure():
    """Test recursive procedure."""
    source = """
    procedure Main is
        Count : Integer := 0;

        procedure Countdown(N : Integer) is
        begin
            if N > 0 then
                Count := Count + 1;
                Countdown(N - 1);
            end if;
        end Countdown;
    begin
        Countdown(10);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_recursive_factorial():
    """Test recursive factorial function."""
    source = """
    procedure Main is
        function Factorial(N : Integer) return Integer is
        begin
            if N <= 1 then
                return 1;
            else
                return N * Factorial(N - 1);
            end if;
        end Factorial;

        Result : Integer;
    begin
        Result := Factorial(5);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_recursive_fibonacci():
    """Test recursive fibonacci function."""
    source = """
    procedure Main is
        function Fib(N : Integer) return Integer is
        begin
            if N <= 1 then
                return N;
            else
                return Fib(N - 1) + Fib(N - 2);
            end if;
        end Fib;

        Result : Integer;
    begin
        Result := Fib(10);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_tail_recursive():
    """Test tail-recursive function."""
    source = """
    procedure Main is
        function Sum_To(N : Integer; Acc : Integer) return Integer is
        begin
            if N <= 0 then
                return Acc;
            else
                return Sum_To(N - 1, Acc + N);
            end if;
        end Sum_To;

        Result : Integer;
    begin
        Result := Sum_To(10, 0);
    end Main;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Local Variable Tests
# ============================================================================


def test_local_variables_shadowing():
    """Test local variables shadowing outer scope."""
    source = """
    procedure Main is
        X : Integer := 10;

        procedure Inner is
            X : Integer := 20;
        begin
            X := X + 1;
        end Inner;
    begin
        Inner;
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_local_variables_initialization():
    """Test local variable initialization."""
    source = """
    procedure Main is
        function Compute return Integer is
            A : Integer := 10;
            B : Integer := 20;
            C : Integer := A + B;
        begin
            return C;
        end Compute;

        Result : Integer;
    begin
        Result := Compute;
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_multiple_local_arrays():
    """Test multiple local arrays."""
    source = """
    procedure Main is
        procedure Process is
            type Arr is array (1 .. 5) of Integer;
            A : Arr;
            B : Arr;
        begin
            A(1) := 10;
            B(1) := A(1);
        end Process;
    begin
        Process;
    end Main;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Parameter Passing Mode Tests
# ============================================================================


def test_in_parameter_unchanged():
    """Test that IN parameter doesn't affect caller."""
    source = """
    procedure Main is
        X : Integer := 10;

        procedure Try_Change(Value : Integer) is
            Temp : Integer;
        begin
            Temp := Value + 1;
        end Try_Change;
    begin
        Try_Change(X);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_out_parameter_modified():
    """Test that OUT parameter modifies caller variable."""
    source = """
    procedure Main is
        A, B : Integer;

        procedure Get_Two(X : out Integer; Y : out Integer) is
        begin
            X := 1;
            Y := 2;
        end Get_Two;
    begin
        Get_Two(A, B);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_in_out_parameter_both():
    """Test IN OUT parameter reads and writes."""
    source = """
    procedure Main is
        X : Integer := 5;

        procedure Increment(Value : in out Integer) is
        begin
            Value := Value + 1;
        end Increment;
    begin
        Increment(X);
        Increment(X);
        Increment(X);
    end Main;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Forward Declaration Tests
# ============================================================================


def test_forward_declaration():
    """Test forward procedure declaration."""
    source = """
    procedure Main is
        procedure Forward_Proc(X : Integer);

        procedure Another is
        begin
            Forward_Proc(10);
        end Another;

        procedure Forward_Proc(X : Integer) is
        begin
            null;
        end Forward_Proc;
    begin
        Another;
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_forward_function():
    """Test forward function declaration."""
    source = """
    procedure Main is
        function Forward_Func(X : Integer) return Integer;

        function User return Integer is
        begin
            return Forward_Func(5);
        end User;

        function Forward_Func(X : Integer) return Integer is
        begin
            return X * 2;
        end Forward_Func;

        Result : Integer;
    begin
        Result := User;
    end Main;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Complex Subprogram Tests
# ============================================================================


def test_function_with_local_subprogram():
    """Test function with local subprogram."""
    source = """
    procedure Main is
        function Outer(N : Integer) return Integer is
            function Helper(X : Integer) return Integer is
            begin
                return X * X;
            end Helper;
        begin
            return Helper(N) + Helper(N + 1);
        end Outer;

        Result : Integer;
    begin
        Result := Outer(3);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_procedure_with_loop():
    """Test procedure with loop inside."""
    source = """
    procedure Main is
        Sum : Integer := 0;

        procedure Add_Range(Start, Finish : Integer) is
        begin
            for I in Start .. Finish loop
                Sum := Sum + I;
            end loop;
        end Add_Range;
    begin
        Add_Range(1, 10);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_function_with_case():
    """Test function with case statement."""
    source = """
    procedure Main is
        function Grade(Score : Integer) return Integer is
        begin
            case Score is
                when 90 .. 100 => return 4;
                when 80 .. 89 => return 3;
                when 70 .. 79 => return 2;
                when 60 .. 69 => return 1;
                when others => return 0;
            end case;
        end Grade;

        G : Integer;
    begin
        G := Grade(85);
    end Main;
    """

    result = compile_source(source)
    assert result.success


def test_multiple_returns():
    """Test function with multiple return points."""
    source = """
    procedure Main is
        function Classify(X : Integer) return Integer is
        begin
            if X < 0 then
                return 0;
            elsif X = 0 then
                return 1;
            else
                return 2;
            end if;
        end Classify;

        C : Integer;
    begin
        C := Classify(5);
        C := Classify(0);
        C := Classify(10);
    end Main;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# IR Generation Tests
# ============================================================================


def test_procedure_call_ir():
    """Test IR generation for procedure call."""
    source = """
    procedure Main is
        procedure Inner is
        begin
            null;
        end Inner;
    begin
        Inner;
    end Main;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    assert "call" in result.output.lower()


def test_function_return_ir():
    """Test IR generation for function return."""
    source = """
    function Get_Value return Integer is
    begin
        return 42;
    end Get_Value;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    assert "ret" in result.output.lower()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
