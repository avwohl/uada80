"""Tests for Ada 2022 declare expressions."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import DeclareExpr


class TestDeclareExprParsing:
    """Tests for parsing declare expressions."""

    def test_simple_declare_expr(self):
        """Test parsing simple declare expression."""
        source = """
        function Test return Integer is
        begin
            return (declare X : Integer := 5; begin X + 1);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_declare_expr_multiple_decls(self):
        """Test declare expression with multiple declarations."""
        source = """
        function Test return Integer is
        begin
            return (declare
                        X : Integer := 10;
                        Y : Integer := 20;
                    begin
                        X + Y);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_declare_expr_in_assignment(self):
        """Test declare expression in assignment."""
        source = """
        procedure Test is
            Result : Integer;
        begin
            Result := (declare X : Integer := 5; begin X * 2);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestDeclareExprSemantic:
    """Tests for semantic analysis of declare expressions."""

    def test_declare_expr_type(self):
        """Test that declare expression has correct type."""
        source = """
        procedure Test is
            Value : Integer;
        begin
            Value := (declare X : Integer := 10; begin X);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_declare_expr_variable_scope(self):
        """Test that declared variable is in scope for result."""
        source = """
        function Compute return Integer is
        begin
            return (declare
                        A : Integer := 5;
                        B : Integer := 3;
                    begin
                        A * B);
        end Compute;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_declare_expr_variable_not_visible_outside(self):
        """Test that declared variable is not visible outside."""
        source = """
        procedure Test is
            Y : Integer;
        begin
            Y := (declare X : Integer := 5; begin X);
            Y := X;  -- Error: X not visible here
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert result.has_errors
        assert any("X" in str(e) and "not found" in str(e).lower() for e in result.errors)


class TestDeclareExprUseCases:
    """Tests for common declare expression use cases."""

    def test_compute_intermediate(self):
        """Test using declare expression for intermediate computation."""
        source = """
        function Area_Of_Square(Side : Integer) return Integer is
        begin
            return (declare
                        S : Integer := Side;
                    begin
                        S * S);
        end Area_Of_Square;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_nested_declare_expr(self):
        """Test nested declare expressions."""
        source = """
        function Test return Integer is
        begin
            return (declare
                        X : Integer := 5;
                    begin
                        (declare
                            Y : Integer := X + 1;
                        begin
                            Y * 2));
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_declare_expr_with_condition(self):
        """Test declare expression in conditional."""
        source = """
        function Safe_Divide(A, B : Integer) return Integer is
        begin
            if B /= 0 then
                return (declare Q : Integer := A / B; begin Q);
            else
                return 0;
            end if;
        end Safe_Divide;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
