"""Tests for Ada 2005 interface types."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import InterfaceTypeDef, DerivedTypeDef


class TestInterfaceParsing:
    """Tests for parsing interface types."""

    def test_simple_interface(self):
        """Test parsing simple interface."""
        source = """
        package Test is
            type Printable is interface;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        type_decl = pkg.declarations[0]
        assert isinstance(type_decl.type_def, InterfaceTypeDef)

    def test_limited_interface(self):
        """Test parsing limited interface."""
        source = """
        package Test is
            type Limited_Iface is limited interface;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        type_decl = pkg.declarations[0]
        assert isinstance(type_decl.type_def, InterfaceTypeDef)
        assert type_decl.type_def.is_limited

    def test_task_interface(self):
        """Test parsing task interface."""
        source = """
        package Test is
            type Task_Iface is task interface;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        type_decl = pkg.declarations[0]
        assert isinstance(type_decl.type_def, InterfaceTypeDef)
        assert type_decl.type_def.is_task

    def test_protected_interface(self):
        """Test parsing protected interface."""
        source = """
        package Test is
            type Protected_Iface is protected interface;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        type_decl = pkg.declarations[0]
        assert isinstance(type_decl.type_def, InterfaceTypeDef)
        assert type_decl.type_def.is_protected

    def test_synchronized_interface(self):
        """Test parsing synchronized interface."""
        source = """
        package Test is
            type Sync_Iface is synchronized interface;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        type_decl = pkg.declarations[0]
        assert isinstance(type_decl.type_def, InterfaceTypeDef)
        assert type_decl.type_def.is_synchronized

    def test_interface_inheritance(self):
        """Test interface inheriting from other interfaces."""
        source = """
        package Test is
            type Base_Interface is interface;
            type Extended is interface and Base_Interface;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        extended_decl = pkg.declarations[1]
        assert isinstance(extended_decl.type_def, InterfaceTypeDef)
        assert len(extended_decl.type_def.parent_interfaces) == 1

    def test_multiple_interface_inheritance(self):
        """Test interface inheriting from multiple interfaces."""
        source = """
        package Test is
            type Printable is interface;
            type Comparable is interface;
            type Combined is interface and Printable and Comparable;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        combined_decl = pkg.declarations[2]
        assert isinstance(combined_decl.type_def, InterfaceTypeDef)
        assert len(combined_decl.type_def.parent_interfaces) == 2


class TestInterfaceImplementation:
    """Tests for implementing interfaces."""

    def test_type_implements_interface(self):
        """Test type implementing an interface."""
        source = """
        package Test is
            type Printable is interface;
            type My_Type is new Printable with record
                Value : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        my_type_decl = pkg.declarations[1]
        assert isinstance(my_type_decl.type_def, DerivedTypeDef)

    def test_type_implements_multiple_interfaces(self):
        """Test type implementing multiple interfaces."""
        source = """
        package Test is
            type Printable is interface;
            type Comparable is interface;
            type My_Type is new Printable and Comparable with record
                Value : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        my_type_decl = pkg.declarations[2]
        assert isinstance(my_type_decl.type_def, DerivedTypeDef)
        # First one is parent type (Printable), rest are additional interfaces
        assert len(my_type_decl.type_def.interfaces) == 1  # Comparable


class TestInterfaceSemantic:
    """Tests for semantic analysis of interfaces."""

    def test_interface_declaration(self):
        """Test semantic analysis of interface declaration."""
        source = """
        package Test is
            type Printable is interface;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_interface_with_abstract_procedure(self):
        """Test interface with abstract procedure declaration."""
        source = """
        package Test is
            type Printable is interface;
            procedure Print(Self : Printable) is abstract;
        end Test;
        """
        ast = parse(source)
        # Should parse correctly

    def test_multiple_interfaces_semantic(self):
        """Test semantic analysis of multiple interfaces."""
        source = """
        package Test is
            type Comparable is interface;
            type Hashable is interface;
            type Combined is interface and Comparable and Hashable;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestLimitedInterfaceSemantic:
    """Tests for limited interface semantics."""

    def test_limited_interface_declaration(self):
        """Test limited interface semantic analysis."""
        source = """
        package Test is
            type Resource is limited interface;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestTaskInterface:
    """Tests for task interface types."""

    def test_task_interface_declaration(self):
        """Test task interface declaration."""
        source = """
        package Test is
            type Worker is task interface;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestProtectedInterface:
    """Tests for protected interface types."""

    def test_protected_interface_declaration(self):
        """Test protected interface declaration."""
        source = """
        package Test is
            type Guard is protected interface;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
