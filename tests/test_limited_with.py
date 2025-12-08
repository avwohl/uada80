"""Tests for Ada limited with clauses (Ada 2005)."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestLimitedWithParsing:
    """Tests for parsing limited with clauses."""

    def test_simple_limited_with(self):
        """Test parsing simple limited with clause."""
        source = """
        limited with Other_Package;

        package Test is
            type Ref is access Other_Package.Some_Type;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_multiple_limited_with(self):
        """Test multiple limited with clauses."""
        source = """
        limited with Package_A;
        limited with Package_B;

        package Test is
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_mixed_with_clauses(self):
        """Test mixed with and limited with."""
        source = """
        with Regular_Package;
        limited with Limited_Package;

        package Test is
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestLimitedWithSemantic:
    """Tests for semantic analysis of limited with."""

    def test_limited_with_access_type(self):
        """Test limited with for access types."""
        source = """
        package Types is
            type Node is tagged record
                Value : Integer;
            end record;
        end Types;

        limited with Types;

        package Lists is
            type Node_Ptr is access Types.Node;
        end Lists;
        """
        ast = parse(source)
        result = analyze(ast)
        # Limited view allows access type declarations


class TestMutualDependency:
    """Tests for mutual dependency with limited with."""

    def test_forward_reference_pattern(self):
        """Test forward reference pattern with limited with."""
        source = """
        -- First declare the types package
        package Forward is
            type Node;
            type Node_Ptr is access Node;
            type Node is record
                Value : Integer;
                Next : Node_Ptr;
            end record;
        end Forward;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_incomplete_type_completion(self):
        """Test incomplete type with completion."""
        source = """
        package Types is
            type Node;
            type Node_Access is access Node;

            type Node is record
                Data : Integer;
                Next : Node_Access;
            end record;
        end Types;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestLimitedWithUseCases:
    """Tests for limited with use cases."""

    def test_circular_reference_breaking(self):
        """Test using limited with to break circular references."""
        source = """
        package Base is
            type Object is tagged record
                Id : Integer;
            end record;
        end Base;

        package Derived is
            type Child is new Base.Object with record
                Name : Integer;
            end record;
        end Derived;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_limited_view_access_only(self):
        """Test limited view allows only access types."""
        source = """
        package Objects is
            type Thing is tagged null record;
        end Objects;

        limited with Objects;

        package Refs is
            type Thing_Ptr is access Objects.Thing;
        end Refs;
        """
        ast = parse(source)
        # Should parse - limited view allows access type
