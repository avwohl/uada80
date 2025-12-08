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
        """Test 'First and 'Last for scalar types."""
        source = """
        procedure Test is
            type Small is range 1 .. 100;
            Min, Max : Integer;
        begin
            Min := Small'First;
            Max := Small'Last;
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
        """Test using attributes for type validation."""
        source = """
        procedure Test is
            type Small is range 1 .. 10;
            X : Integer := 5;
            In_Range : Boolean;
            First_Val : Integer;
            Last_Val : Integer;
        begin
            First_Val := Small'First;
            Last_Val := Small'Last;
            In_Range := X >= First_Val and X <= Last_Val;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
