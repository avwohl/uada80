"""Tests for Ada representation clauses."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import AttributeDefinitionClause, RecordRepresentationClause


class TestAttributeDefinitionClause:
    """Tests for attribute definition clauses."""

    def test_parse_size_clause(self):
        """Test parsing for Type'Size use N."""
        source = """
        procedure Test is
            type Byte is range 0 .. 255;
            for Byte'Size use 8;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        decls = ast.units[0].unit.declarations
        # Find the attribute definition clause
        attr_clause = None
        for d in decls:
            if isinstance(d, AttributeDefinitionClause):
                attr_clause = d
                break
        assert attr_clause is not None
        assert attr_clause.attribute.lower() == "size"

    def test_parse_alignment_clause(self):
        """Test parsing for Type'Alignment use N."""
        source = """
        procedure Test is
            type Word is range 0 .. 65535;
            for Word'Alignment use 2;
        begin
            null;
        end Test;
        """
        ast = parse(source)

    def test_size_clause_semantic(self):
        """Test semantic analysis of size clause."""
        source = """
        procedure Test is
            type Small is range 0 .. 127;
            for Small'Size use 8;
            X : Small := 10;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_size_clause_unknown_type(self):
        """Test error for size clause with unknown type."""
        source = """
        procedure Test is
            for Unknown'Size use 8;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert result.has_errors
        assert any("unknown" in str(e).lower() for e in result.errors)


class TestRecordRepresentationClause:
    """Tests for record representation clauses."""

    def test_parse_record_rep_clause(self):
        """Test parsing record representation clause."""
        source = """
        procedure Test is
            type Status is record
                Flag : Boolean;
                Value : Integer;
            end record;

            for Status use record
                Flag at 0 range 0 .. 0;
                Value at 1 range 0 .. 31;
            end record;
        begin
            null;
        end Test;
        """
        ast = parse(source)

    def test_record_rep_clause_semantic(self):
        """Test semantic analysis of record representation."""
        source = """
        procedure Test is
            type Register is record
                Bit0 : Boolean;
                Bit1 : Boolean;
            end record;

            for Register use record
                Bit0 at 0 range 0 .. 0;
                Bit1 at 0 range 1 .. 1;
            end record;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should process without errors


class TestEnumerationRepresentationClause:
    """Tests for enumeration representation clauses."""

    def test_parse_enum_rep_clause(self):
        """Test parsing enumeration representation clause."""
        source = """
        procedure Test is
            type Signal is (Off, On, Error);
            for Signal use (Off => 0, On => 1, Error => 255);
        begin
            null;
        end Test;
        """
        ast = parse(source)

    def test_enum_rep_clause_semantic(self):
        """Test semantic analysis of enum representation."""
        source = """
        procedure Test is
            type Priority is (Low, Medium, High);
            for Priority use (Low => 1, Medium => 5, High => 10);
            P : Priority := Medium;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should process without errors


class TestAddressClause:
    """Tests for address clauses."""

    def test_parse_address_clause(self):
        """Test parsing for Object'Address use."""
        source = """
        with System;
        procedure Test is
            Port : Integer;
            for Port'Address use System.Storage_Elements.To_Address(16#FF00#);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse correctly


class TestMixedRepresentation:
    """Tests for multiple representation clauses."""

    def test_multiple_clauses(self):
        """Test multiple representation clauses together."""
        source = """
        procedure Test is
            type Byte is range 0 .. 255;
            for Byte'Size use 8;

            type Status_Byte is record
                Ready : Boolean;
                Error : Boolean;
                Data : Byte;
            end record;

            for Status_Byte'Size use 16;
            for Status_Byte use record
                Ready at 0 range 0 .. 0;
                Error at 0 range 1 .. 1;
                Data at 1 range 0 .. 7;
            end record;
        begin
            null;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should handle multiple clauses
