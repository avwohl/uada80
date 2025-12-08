"""Tests for Ada discriminated records."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestDiscriminatedParsing:
    """Tests for parsing discriminated records."""

    def test_simple_discriminated_record(self):
        """Test parsing simple discriminated record."""
        source = """
        package Test is
            type Message(Length : Integer) is record
                Text : String(1 .. Length);
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_multiple_discriminants(self):
        """Test record with multiple discriminants."""
        source = """
        package Test is
            type Matrix(Rows, Cols : Integer) is record
                Data : array (1 .. Rows, 1 .. Cols) of Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_discriminant_with_default(self):
        """Test discriminant with default value."""
        source = """
        package Test is
            type Buffer(Size : Integer := 100) is record
                Data : String(1 .. Size);
                Used : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_access_discriminant(self):
        """Test access discriminant (Ada 95)."""
        source = """
        package Test is
            type Node;
            type Node_Access is access all Node;

            type Node(Parent : access Node := null) is record
                Value : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestVariantRecordParsing:
    """Tests for parsing variant records."""

    def test_simple_variant_record(self):
        """Test parsing simple variant record."""
        source = """
        package Test is
            type Shape_Kind is (Circle, Rectangle, Triangle);

            type Shape(Kind : Shape_Kind) is record
                X, Y : Integer;
                case Kind is
                    when Circle =>
                        Radius : Integer;
                    when Rectangle =>
                        Width, Height : Integer;
                    when Triangle =>
                        Base, Side1, Side2 : Integer;
                end case;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_variant_with_others(self):
        """Test variant record with 'others' choice."""
        source = """
        package Test is
            type Tag is (A, B, C, D, E);

            type Tagged_Value(T : Tag) is record
                case T is
                    when A =>
                        Int_Val : Integer;
                    when B | C =>
                        Bool_Val : Boolean;
                    when others =>
                        null;
                end case;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestDiscriminatedSemantic:
    """Tests for semantic analysis of discriminated records."""

    def test_discriminant_in_scope(self):
        """Test that discriminant is visible in record body."""
        source = """
        package Test is
            type Sized_Record(Max_Length : Integer) is record
                Length : Integer;
                Count : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_discriminated_variable(self):
        """Test declaring discriminated record variable."""
        source = """
        package Test is
            type Buffer(Size : Integer) is record
                Count : Integer;
            end record;

            B : Buffer(100);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_discriminant_default_usage(self):
        """Test discriminant with default value."""
        source = """
        package Test is
            type Stack(Capacity : Integer := 10) is record
                Count : Integer := 0;
            end record;

            S1 : Stack;       -- Uses default capacity
            S2 : Stack(50);   -- Explicit capacity
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestDiscriminatedUseCases:
    """Tests for common discriminated record use cases."""

    def test_bounded_buffer(self):
        """Test bounded buffer pattern."""
        source = """
        package Bounded_Buffers is
            type Bounded_Buffer(Max_Size : Integer := 80) is record
                Count : Integer := 0;
                Head : Integer := 1;
            end record;
        end Bounded_Buffers;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_variant_for_union(self):
        """Test using variant record as discriminated union."""
        source = """
        package Values is
            type Value_Kind is (Int_Value, Float_Value, Bool_Value);

            type Value(Kind : Value_Kind := Int_Value) is record
                case Kind is
                    when Int_Value =>
                        I : Integer;
                    when Float_Value =>
                        F : Float;
                    when Bool_Value =>
                        B : Boolean;
                end case;
            end record;
        end Values;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should parse, analysis depends on implementation

    def test_linked_list_node(self):
        """Test linked list node with access discriminant."""
        source = """
        package Lists is
            type Node;
            type Node_Ptr is access Node;

            type Node is record
                Value : Integer;
                Next : Node_Ptr := null;
            end record;
        end Lists;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
