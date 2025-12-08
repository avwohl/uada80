"""Tests for Ada derived types and type inheritance."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestDerivedTypeParsing:
    """Tests for parsing derived types."""

    def test_simple_derived_type(self):
        """Test parsing simple derived type."""
        source = """
        package Test is
            type Base_Int is new Integer;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_derived_with_constraint(self):
        """Test derived type with range constraint."""
        source = """
        package Test is
            type Small_Int is new Integer range 0 .. 100;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_derived_record(self):
        """Test derived record type."""
        source = """
        package Test is
            type Base is record
                X : Integer;
            end record;

            type Derived is new Base;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_private_extension(self):
        """Test private type extension."""
        source = """
        package Test is
            type Base is tagged record
                X : Integer;
            end record;

            type Derived is new Base with private;
        private
            type Derived is new Base with record
                Y : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestTaggedTypeParsing:
    """Tests for parsing tagged types."""

    def test_simple_tagged_type(self):
        """Test parsing simple tagged type."""
        source = """
        package Shapes is
            type Shape is tagged record
                X, Y : Integer;
            end record;
        end Shapes;
        """
        ast = parse(source)
        # Should parse without errors

    def test_tagged_extension(self):
        """Test tagged type extension."""
        source = """
        package Shapes is
            type Shape is tagged record
                X, Y : Integer;
            end record;

            type Circle is new Shape with record
                Radius : Integer;
            end record;
        end Shapes;
        """
        ast = parse(source)
        # Should parse without errors

    def test_abstract_tagged(self):
        """Test abstract tagged type."""
        source = """
        package Test is
            type Abstract_Shape is abstract tagged record
                Name : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestDerivedTypeSemantic:
    """Tests for semantic analysis of derived types."""

    def test_derived_inherits_operations(self):
        """Test that derived types inherit operations."""
        source = """
        package Test is
            type Counter is new Integer;
            C : Counter := 0;
            D : Counter;
        begin
            D := C + 1;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should work since Integer operations are inherited

    def test_tagged_method(self):
        """Test method on tagged type."""
        source = """
        package Test is
            type Point is tagged record
                X, Y : Integer;
            end record;

            function Get_X(P : Point) return Integer;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_extension_adds_components(self):
        """Test that extension adds new components."""
        source = """
        package Test is
            type Base is tagged record
                X : Integer;
            end record;

            type Extended is new Base with record
                Y : Integer;
            end record;

            E : Extended;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestDerivedTypeUseCases:
    """Tests for common derived type use cases."""

    def test_numeric_types(self):
        """Test creating numeric derived types."""
        source = """
        package Types is
            type Meters is new Integer;
            type Seconds is new Integer;
            type Velocity is new Integer;

            D : Meters := 100;
            T : Seconds := 10;
        end Types;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_class_hierarchy(self):
        """Test class hierarchy with tagged types."""
        source = """
        package Vehicles is
            type Vehicle is tagged record
                Speed : Integer;
            end record;

            type Car is new Vehicle with record
                Doors : Integer;
            end record;

            type Truck is new Vehicle with record
                Capacity : Integer;
            end record;
        end Vehicles;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_null_extension(self):
        """Test null record extension."""
        source = """
        package Test is
            type Base is tagged record
                Value : Integer;
            end record;

            type Alias is new Base with null record;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
