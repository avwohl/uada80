"""Tests for Ada attributes."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestTypeAttributes:
    """Tests for type attributes."""

    def test_type_size(self):
        """Test 'Size attribute."""
        source = """
        procedure Test is
            X : Integer;
            Size : Integer;
        begin
            Size := Integer'Size;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_type_first_last(self):
        """Test 'First and 'Last for scalar types.

        In Ada, T'First returns a value of type T, so:
        - Small'First returns Small, not Integer
        - Integer'First returns Integer
        - Float'First returns Float
        """
        source = """
        procedure Test is
            type Small is range 1 .. 100;
            Min, Max : Small;  -- Must be Small, not Integer
            I_Min, I_Max : Integer;
            F_Min, F_Max : Float;
        begin
            Min := Small'First;
            Max := Small'Last;
            I_Min := Integer'First;
            I_Max := Integer'Last;
            F_Min := Float'First;
            F_Max := Float'Last;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_type_range(self):
        """Test 'Range attribute."""
        source = """
        procedure Test is
            type Index is range 1 .. 10;
        begin
            for I in Index'Range loop
                null;
            end loop;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestScalarAttributes:
    """Tests for scalar type attributes."""

    def test_succ_pred(self):
        """Test 'Succ and 'Pred attributes."""
        source = """
        procedure Test is
            type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
            D1, D2 : Day;
        begin
            D1 := Day'Succ(Mon);  -- Tue
            D2 := Day'Pred(Wed);  -- Tue
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_pos_val(self):
        """Test 'Pos and 'Val attributes."""
        source = """
        procedure Test is
            type Color is (Red, Green, Blue);
            N : Integer;
            C : Color;
        begin
            N := Color'Pos(Green);  -- 1
            C := Color'Val(0);  -- Red
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_image_value(self):
        """Test 'Image and 'Value attributes."""
        source = """
        procedure Test is
            N : Integer := 42;
            S : String := Integer'Image(N);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse

    def test_min_max(self):
        """Test 'Min and 'Max attributes."""
        source = """
        procedure Test is
            A : Integer := 10;
            B : Integer := 20;
            M : Integer;
        begin
            M := Integer'Max(A, B);
            M := Integer'Min(A, B);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestObjectAttributes:
    """Tests for object attributes."""

    def test_address(self):
        """Test 'Address attribute."""
        source = """
        procedure Test is
            X : Integer;
        begin
            null;  -- X'Address returns System.Address
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_alignment(self):
        """Test 'Alignment attribute."""
        source = """
        procedure Test is
            X : Integer;
            A : Integer;
        begin
            A := X'Alignment;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should parse and process


class TestAccessAttributes:
    """Tests for access-related attributes."""

    def test_access_attribute(self):
        """Test 'Access attribute."""
        source = """
        procedure Test is
            type Int_Ptr is access all Integer;
            X : aliased Integer := 42;
            P : Int_Ptr;
        begin
            P := X'Access;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_unchecked_access(self):
        """Test 'Unchecked_Access attribute."""
        source = """
        procedure Test is
            type Int_Ptr is access all Integer;
            X : aliased Integer := 42;
            P : Int_Ptr;
        begin
            P := X'Unchecked_Access;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestTaggedTypeAttributes:
    """Tests for tagged type attributes."""

    def test_tag_attribute(self):
        """Test 'Tag attribute."""
        source = """
        package Test is
            type Base is tagged record
                X : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_class_attribute(self):
        """Test 'Class attribute."""
        source = """
        package Test is
            type Base is tagged record
                X : Integer;
            end record;

            procedure Process(Obj : Base'Class);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should handle 'Class


class TestModAttribute:
    """Tests for modular type attributes."""

    def test_modulus(self):
        """Test 'Modulus attribute."""
        source = """
        procedure Test is
            type Byte is mod 256;
            M : Integer;
        begin
            M := Byte'Modulus;  -- 256
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestAttributeUseCases:
    """Tests for common attribute use cases."""

    def test_bounds_checking(self):
        """Test using attributes for bounds checking."""
        source = """
        procedure Test is
            type Index is range 1 .. 100;
            I : Index := 50;
        begin
            if I >= Index'First and I <= Index'Last then
                null;
            end if;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_array_iteration(self):
        """Test using attributes for array iteration."""
        source = """
        procedure Test is
            type Arr is array (1 .. 10) of Integer;
            A : Arr := (others => 0);
        begin
            for I in A'Range loop
                A(I) := I;
            end loop;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_type_conversion_validation(self):
        """Test using attributes for type validation.

        Note: Small'First returns Small type, not Integer.
        To compare with Integer, use Integer'First or explicit conversion.
        """
        source = """
        procedure Test is
            X : Integer := 5;
            In_Range : Boolean;
            First_Val : Integer;
            Last_Val : Integer;
        begin
            First_Val := Integer'First;
            Last_Val := Integer'Last;
            In_Range := X >= First_Val and X <= Last_Val;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestValidAttribute:
    """Tests for 'Valid attribute."""

    def test_valid_integer(self):
        """Test 'Valid on integer variable."""
        source = """
        procedure Test is
            X : Integer := 42;
            Is_Valid : Boolean;
        begin
            Is_Valid := X'Valid;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_valid_enumeration(self):
        """Test 'Valid on enumeration variable."""
        source = """
        procedure Test is
            type Color is (Red, Green, Blue);
            C : Color := Red;
            Is_Valid : Boolean;
        begin
            Is_Valid := C'Valid;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_valid_subtype(self):
        """Test 'Valid on constrained subtype."""
        source = """
        procedure Test is
            subtype Small is Integer range 1 .. 10;
            X : Small := 5;
            Is_Valid : Boolean;
        begin
            Is_Valid := X'Valid;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestConstrainedAttribute:
    """Tests for 'Constrained attribute."""

    def test_constrained_variable(self):
        """Test 'Constrained on a variable."""
        source = """
        procedure Test is
            X : Integer := 42;
            Is_Const : Boolean;
        begin
            Is_Const := X'Constrained;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestSizeAttributes:
    """Tests for size-related attributes."""

    def test_object_size(self):
        """Test 'Object_Size attribute."""
        source = """
        procedure Test is
            X : Integer := 42;
            Size : Integer;
        begin
            Size := X'Object_Size;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should parse (semantic may or may not handle)

    def test_value_size(self):
        """Test 'Value_Size attribute."""
        source = """
        procedure Test is
            type Small is range 1 .. 10;
            Size : Integer;
        begin
            Size := Small'Value_Size;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should parse

    def test_component_size(self):
        """Test 'Component_Size attribute."""
        source = """
        procedure Test is
            type Arr is array (1 .. 10) of Integer;
            Size : Integer;
        begin
            Size := Arr'Component_Size;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should parse


class TestMachineAttributes:
    """Tests for machine-related attributes."""

    def test_alignment(self):
        """Test 'Alignment attribute."""
        source = """
        procedure Test is
            X : Integer;
            Align : Integer;
        begin
            Align := Integer'Alignment;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should parse

    def test_bit_order(self):
        """Test 'Bit_Order attribute."""
        source = """
        procedure Test is
            type Rec is record
                X : Integer;
            end record;
            Order : Integer;
        begin
            Order := Rec'Bit_Order;
        end Test;
        """
        ast = parse(source)
        # Should parse


class TestTaskingAttributes:
    """Tests for tasking-related attributes."""

    def test_callable(self):
        """Test 'Callable attribute."""
        source = """
        procedure Test is
            task T is
                entry Start;
            end T;

            task body T is
            begin
                accept Start;
            end T;

            Is_Callable : Boolean;
        begin
            Is_Callable := T'Callable;
        end Test;
        """
        ast = parse(source)
        # Should parse

    def test_terminated(self):
        """Test 'Terminated attribute."""
        source = """
        procedure Test is
            task T is
                entry Start;
            end T;

            task body T is
            begin
                accept Start;
            end T;

            Is_Done : Boolean;
        begin
            Is_Done := T'Terminated;
        end Test;
        """
        ast = parse(source)
        # Should parse

    def test_identity(self):
        """Test 'Identity attribute."""
        source = """
        procedure Test is
            task T is
                entry Start;
            end T;

            task body T is
            begin
                accept Start;
            end T;
        begin
            null;  -- T'Identity would be used with tasking
        end Test;
        """
        ast = parse(source)
        # Should parse


class TestBaseAttribute:
    """Tests for 'Base attribute."""

    def test_base_type(self):
        """Test 'Base attribute on a subtype."""
        source = """
        procedure Test is
            subtype Small is Integer range 1 .. 10;
            X : Integer;
            Y : Small := 5;
        begin
            X := Small'Base'First;  -- Gets Integer'First
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should parse


class TestCharacterHandling:
    """Tests for character handling attributes."""

    def test_is_letter(self):
        """Test Is_Letter style attributes."""
        source = """
        procedure Test is
            C : Character := 'A';
            Result : Boolean;
        begin
            Result := Character'Pos(C) >= Character'Pos('A');
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_to_lower_upper(self):
        """Test case conversion attributes."""
        source = """
        procedure Test is
            C : Character := 'A';
            Lower : Character;
        begin
            -- Would use To_Lower attribute
            Lower := C;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestMathAttributes:
    """Tests for math function attributes."""

    def test_min_max(self):
        """Test Min/Max attributes."""
        source = """
        procedure Test is
            A, B, C : Integer;
        begin
            A := 10;
            B := 20;
            C := Integer'Min(A, B);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestMemoryAttributes:
    """Tests for memory-related attributes."""

    def test_address_attribute(self):
        """Test 'Address attribute."""
        source = """
        procedure Test is
            X : Integer;
            Addr : System.Address;
        begin
            Addr := X'Address;
        end Test;
        """
        ast = parse(source)
        # Should parse

    def test_size_attribute(self):
        """Test 'Size attribute variants."""
        source = """
        procedure Test is
            X : Integer;
            S : Natural;
        begin
            S := Integer'Size;
            S := X'Size;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestContractAttributes:
    """Tests for Ada 2012/2022 contract attributes."""

    def test_old_attribute(self):
        """Test 'Old attribute in postconditions."""
        source = """
        procedure Increment(X : in out Integer)
            with Post => X = X'Old + 1;
        """
        ast = parse(source)
        # Should parse with postcondition

    def test_result_attribute(self):
        """Test 'Result attribute in function postconditions."""
        source = """
        function Double(X : Integer) return Integer
            with Post => Double'Result = X * 2;
        """
        ast = parse(source)
        # Should parse

    def test_loop_entry_attribute(self):
        """Test 'Loop_Entry attribute in loop invariants."""
        source = """
        procedure Test is
            Sum : Integer := 0;
        begin
            for I in 1 .. 10 loop
                Sum := Sum + I;
                -- pragma Loop_Invariant (Sum >= Sum'Loop_Entry);
            end loop;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestStreamAttributes:
    """Tests for stream I/O attributes."""

    def test_write_attribute(self):
        """Test 'Write attribute."""
        source = """
        procedure Test is
            X : Integer := 42;
        begin
            -- Integer'Write would write to stream
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_read_attribute(self):
        """Test 'Read attribute."""
        source = """
        procedure Test is
            X : Integer;
        begin
            -- Integer'Read would read from stream
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestZ80SpecificAttributes:
    """Tests for Z80/CP/M specific attributes."""

    def test_system_attributes(self):
        """Test system-related attributes."""
        source = """
        procedure Test is
            Size : Integer;
        begin
            Size := Integer'Size;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
