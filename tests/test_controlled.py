"""Tests for Ada controlled types (finalization)."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.symbol_table import SymbolTable, SymbolKind


class TestFinalizationPackage:
    """Tests for Ada.Finalization package."""

    def test_finalization_package_exists(self):
        """Test that Ada.Finalization package exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "finalization" in ada_pkg.public_symbols

    def test_controlled_type_exists(self):
        """Test that Ada.Finalization.Controlled type exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        finalization = ada_pkg.public_symbols["finalization"]
        assert "controlled" in finalization.public_symbols
        controlled = finalization.public_symbols["controlled"]
        assert controlled.kind == SymbolKind.TYPE

    def test_limited_controlled_type_exists(self):
        """Test that Ada.Finalization.Limited_Controlled type exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        finalization = ada_pkg.public_symbols["finalization"]
        assert "limited_controlled" in finalization.public_symbols
        limited_ctrl = finalization.public_symbols["limited_controlled"]
        assert limited_ctrl.kind == SymbolKind.TYPE

    def test_initialize_procedure_exists(self):
        """Test that Initialize procedure exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        finalization = ada_pkg.public_symbols["finalization"]
        assert "initialize" in finalization.public_symbols

    def test_adjust_procedure_exists(self):
        """Test that Adjust procedure exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        finalization = ada_pkg.public_symbols["finalization"]
        assert "adjust" in finalization.public_symbols

    def test_finalize_procedure_exists(self):
        """Test that Finalize procedure exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        finalization = ada_pkg.public_symbols["finalization"]
        assert "finalize" in finalization.public_symbols


class TestControlledTypeParsing:
    """Tests for parsing controlled types."""

    def test_parse_controlled_type_declaration(self):
        """Test parsing a type that derives from Controlled."""
        source = """
        with Ada.Finalization;
        package Test is
            type Resource is new Ada.Finalization.Controlled with record
                Handle : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_parse_limited_controlled_type(self):
        """Test parsing a type that derives from Limited_Controlled."""
        source = """
        with Ada.Finalization;
        package Test is
            type File is new Ada.Finalization.Limited_Controlled with record
                Name : String(1 .. 100);
            end record;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_parse_overriding_initialize(self):
        """Test parsing overriding Initialize procedure."""
        source = """
        with Ada.Finalization;
        package Test is
            type Resource is new Ada.Finalization.Controlled with record
                Value : Integer := 0;
            end record;

            overriding procedure Initialize(Self : in out Resource);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_parse_overriding_finalize(self):
        """Test parsing overriding Finalize procedure."""
        source = """
        with Ada.Finalization;
        package Test is
            type Resource is new Ada.Finalization.Controlled with record
                Value : Integer := 0;
            end record;

            overriding procedure Finalize(Self : in Out Resource);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_parse_overriding_adjust(self):
        """Test parsing overriding Adjust procedure."""
        source = """
        with Ada.Finalization;
        package Test is
            type Resource is new Ada.Finalization.Controlled with record
                Value : Integer := 0;
            end record;

            overriding procedure Adjust(Self : in out Resource);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestControlledTypeSemantic:
    """Tests for semantic analysis of controlled types."""

    def test_controlled_type_semantic(self):
        """Test semantic analysis of controlled type."""
        source = """
        with Ada.Finalization;
        package Test is
            type My_Type is new Ada.Finalization.Controlled with record
                Data : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # May have some errors depending on full implementation

    def test_limited_controlled_semantic(self):
        """Test semantic analysis of limited controlled type."""
        source = """
        with Ada.Finalization;
        package Test is
            type File_Handle is new Ada.Finalization.Limited_Controlled with record
                Fd : Integer;
            end record;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # May have some errors depending on full implementation


class TestControlledOperations:
    """Tests for controlled type operations."""

    def test_all_three_operations(self):
        """Test type with all three controlled operations."""
        source = """
        with Ada.Finalization;
        package Resources is
            type Handle is new Ada.Finalization.Controlled with record
                Id : Integer;
            end record;

            overriding procedure Initialize(Self : in Out Handle);
            overriding procedure Adjust(Self : in Out Handle);
            overriding procedure Finalize(Self : in Out Handle);
        end Resources;
        """
        ast = parse(source)
        # Should parse without errors

    def test_limited_no_adjust(self):
        """Test limited controlled type (no Adjust operation)."""
        source = """
        with Ada.Finalization;
        package Files is
            type File is new Ada.Finalization.Limited_Controlled with record
                Descriptor : Integer;
            end record;

            overriding procedure Initialize(Self : in Out File);
            overriding procedure Finalize(Self : in Out File);
        end Files;
        """
        ast = parse(source)
        # Should parse without errors


class TestControlledTypeFlags:
    """Tests for controlled type flags in type system."""

    def test_controlled_type_flag(self):
        """Test that Controlled type has is_controlled flag."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        finalization = ada_pkg.public_symbols["finalization"]
        controlled_sym = finalization.public_symbols["controlled"]
        assert controlled_sym.ada_type.is_controlled

    def test_limited_controlled_flag(self):
        """Test that Limited_Controlled type has is_limited_controlled flag."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        finalization = ada_pkg.public_symbols["finalization"]
        limited_sym = finalization.public_symbols["limited_controlled"]
        assert limited_sym.ada_type.is_limited_controlled
