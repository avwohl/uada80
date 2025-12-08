"""Tests for Ada 2005 extended return statements."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import ExtendedReturnStmt


class TestExtendedReturnParsing:
    """Tests for parsing extended return statements."""

    def test_simple_extended_return(self):
        """Test parsing simple extended return."""
        source = """
        function Make_Zero return Integer is
        begin
            return Result : Integer := 0 do
                null;
            end return;
        end Make_Zero;
        """
        ast = parse(source)
        body = ast.units[0].unit
        # Find the extended return statement
        found_extended = False
        for stmt in body.statements:
            if isinstance(stmt, ExtendedReturnStmt):
                found_extended = True
                assert stmt.object_name == "Result"
        assert found_extended

    def test_extended_return_with_type(self):
        """Test extended return with explicit type."""
        source = """
        function Create return Integer is
        begin
            return Value : Integer do
                Value := 42;
            end return;
        end Create;
        """
        ast = parse(source)
        # Should parse without errors

    def test_extended_return_with_init(self):
        """Test extended return with initialization."""
        source = """
        function Init_Value return Integer is
        begin
            return X : Integer := 100 do
                null;
            end return;
        end Init_Value;
        """
        ast = parse(source)
        body = ast.units[0].unit
        found = False
        for stmt in body.statements:
            if isinstance(stmt, ExtendedReturnStmt):
                found = True
                assert stmt.init_expr is not None
        assert found

    def test_extended_return_no_do(self):
        """Test extended return without do block."""
        source = """
        function Simple_Return return Integer is
        begin
            return X : Integer := 5;
        end Simple_Return;
        """
        ast = parse(source)
        # Should parse - extended return without do block


class TestExtendedReturnSemantic:
    """Tests for semantic analysis of extended return statements."""

    def test_extended_return_type_check(self):
        """Test that extended return type is checked."""
        source = """
        function Make_Int return Integer is
        begin
            return Result : Integer := 0 do
                Result := 10;
            end return;
        end Make_Int;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_extended_return_object_usable(self):
        """Test that the return object is usable in the do block."""
        source = """
        function Build_Value return Integer is
        begin
            return X : Integer := 0 do
                X := X + 1;
                X := X * 2;
            end return;
        end Build_Value;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_extended_return_only_in_function(self):
        """Test that extended return is only in functions."""
        source = """
        procedure Bad is
        begin
            return X : Integer do
                null;
            end return;
        end Bad;
        """
        ast = parse(source)
        result = analyze(ast)
        assert result.has_errors
        assert any("function" in str(e).lower() for e in result.errors)


class TestExtendedReturnUseCases:
    """Tests for common extended return use cases."""

    def test_build_with_statements(self):
        """Test extended return with statements that modify return object."""
        source = """
        function Build_Value return Integer is
        begin
            return Result : Integer := 10 do
                Result := Result + 5;
                Result := Result * 2;
            end return;
        end Build_Value;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_conditional_building(self):
        """Test conditional initialization in extended return."""
        source = """
        function Make_Value(Flag : Boolean) return Integer is
        begin
            return Result : Integer := 0 do
                if Flag then
                    Result := 100;
                end if;
            end return;
        end Make_Value;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
