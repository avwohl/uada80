"""
Tests for the AST optimizer.

Tests constant folding, algebraic simplifications, dead code elimination,
and other optimization passes.
"""

import pytest
from uada80.parser import parse
from uada80.optimizer import (
    ASTOptimizer,
    OptimizerConfig,
    OptimizationLevel,
    OptimizationStats,
    optimize,
)
from uada80.ast_nodes import (
    IntegerLiteral,
    Identifier,
    BinaryExpr,
    AssignmentStmt,
    IfStmt,
    NullStmt,
    BlockStmt,
)


def get_first_stmt(code: str):
    """Parse code and return the first statement of the main procedure."""
    ast = parse(code)
    body = ast.units[0].unit
    return body.statements[0] if body.statements else None


def get_optimized_stmt(code: str, level: int = 2):
    """Parse, optimize, and return the first statement."""
    ast = parse(code)
    opt_ast, stats = optimize(ast, level=level)
    body = opt_ast.units[0].unit
    return body.statements[0] if body.statements else None, stats


def get_optimized_stmts(code: str, level: int = 2):
    """Parse, optimize, and return all statements."""
    ast = parse(code)
    opt_ast, stats = optimize(ast, level=level)
    body = opt_ast.units[0].unit
    return body.statements, stats


# =============================================================================
# Constant Folding Tests
# =============================================================================


class TestConstantFolding:
    """Tests for constant folding optimization."""

    def test_add_constants(self):
        """Test folding of constant addition."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 2 + 3;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 5
        assert stats.constants_folded >= 1

    def test_subtract_constants(self):
        """Test folding of constant subtraction."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 10 - 3;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 7

    def test_multiply_constants(self):
        """Test folding of constant multiplication."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 4 * 5;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 20

    def test_divide_constants(self):
        """Test folding of constant division."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 20 / 4;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 5

    def test_mod_constants(self):
        """Test folding of constant modulo."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 17 mod 5;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 2

    def test_exponent_constants(self):
        """Test folding of constant exponentiation."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 2 ** 3;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 8

    def test_nested_constant_expr(self):
        """Test folding of nested constant expressions."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := (2 + 3) * (4 - 1);
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 15

    def test_unary_minus_constant(self):
        """Test folding of unary minus on constant."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := -5;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == -5

    def test_abs_constant(self):
        """Test folding of abs on constant."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := abs(-7);
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 7

    def test_no_fold_division_by_zero(self):
        """Test that division by zero is not folded."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 10 / 0;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        # Should remain as binary expression
        assert isinstance(stmt.value, BinaryExpr)


# =============================================================================
# Algebraic Simplification Tests
# =============================================================================


class TestAlgebraicSimplifications:
    """Tests for algebraic simplification optimization."""

    def test_add_zero_right(self):
        """Test x + 0 = x."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := X + 0;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"
        assert stats.algebraic_simplifications >= 1

    def test_add_zero_left(self):
        """Test 0 + x = x."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := 0 + X;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"

    def test_subtract_zero(self):
        """Test x - 0 = x."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := X - 0;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"

    def test_subtract_self(self):
        """Test x - x = 0."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := X - X;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 0

    def test_multiply_one_right(self):
        """Test x * 1 = x."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := X * 1;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"

    def test_multiply_one_left(self):
        """Test 1 * x = x."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := 1 * X;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"

    def test_multiply_zero(self):
        """Test x * 0 = 0."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := X * 0;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 0

    def test_divide_one(self):
        """Test x / 1 = x."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := X / 1;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"

    def test_power_zero(self):
        """Test constant ** 0 = 1.

        Note: x ** 0 where x is a variable is NOT folded because the optimizer
        doesn't have type information. Folding it to IntegerLiteral(1) would be
        wrong for Float64 types like Long_Float. Only fold when base is also constant.
        """
        code = """
        procedure Test is
            Y : Integer;
        begin
            Y := 5 ** 0;  -- Constant base: will be folded to 1
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 1

    def test_power_one(self):
        """Test x ** 1 = x."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := X ** 1;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"

    def test_double_negation(self):
        """Test --x = x."""
        code = """
        procedure Test is
            X, Y : Integer;
        begin
            Y := - (- X);
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"


# =============================================================================
# Boolean Simplification Tests
# =============================================================================


class TestBooleanSimplifications:
    """Tests for boolean simplification optimization."""

    def test_and_true(self):
        """Test x and True = x."""
        code = """
        procedure Test is
            X, Y : Boolean;
        begin
            Y := X and True;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"

    def test_and_false(self):
        """Test x and False = False."""
        code = """
        procedure Test is
            X, Y : Boolean;
        begin
            Y := X and False;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name.upper() == "FALSE"

    def test_or_false(self):
        """Test x or False = x."""
        code = """
        procedure Test is
            X, Y : Boolean;
        begin
            Y := X or False;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"

    def test_or_true(self):
        """Test x or True = True."""
        code = """
        procedure Test is
            X, Y : Boolean;
        begin
            Y := X or True;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name.upper() == "TRUE"

    def test_equal_self(self):
        """Test x = x -> True."""
        code = """
        procedure Test is
            X : Integer;
            Y : Boolean;
        begin
            Y := X = X;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name.upper() == "TRUE"

    def test_not_equal_self(self):
        """Test x /= x -> False."""
        code = """
        procedure Test is
            X : Integer;
            Y : Boolean;
        begin
            Y := X /= X;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name.upper() == "FALSE"

    def test_less_than_self(self):
        """Test x < x -> False."""
        code = """
        procedure Test is
            X : Integer;
            Y : Boolean;
        begin
            Y := X < X;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name.upper() == "FALSE"

    def test_double_not(self):
        """Test not not x = x."""
        code = """
        procedure Test is
            X, Y : Boolean;
        begin
            Y := not (not X);
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, Identifier)
        assert stmt.value.name == "X"


# =============================================================================
# Dead Code Elimination Tests
# =============================================================================


class TestDeadCodeElimination:
    """Tests for dead code elimination optimization."""

    def test_if_true_eliminates_condition(self):
        """Test if True then ... end if; inlines then branch."""
        code = """
        procedure Test is
            X : Integer;
        begin
            if True then
                X := 1;
            end if;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        # Should be just the assignment, not an if
        assert isinstance(stmt, AssignmentStmt)
        assert stats.dead_code_eliminated >= 1

    def test_if_false_eliminates_then(self):
        """Test if False then ... end if; is removed."""
        code = """
        procedure Test is
            X : Integer;
        begin
            if False then
                X := 1;
            end if;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        # Should become null statement
        assert isinstance(stmt, NullStmt)

    def test_if_false_keeps_else(self):
        """Test if False then ... else ... end if; keeps else."""
        code = """
        procedure Test is
            X : Integer;
        begin
            if False then
                X := 1;
            else
                X := 2;
            end if;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt, AssignmentStmt)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 2

    def test_code_after_return_eliminated(self):
        """Test that code after return is eliminated."""
        code = """
        function Test return Integer is
            X : Integer;
        begin
            return 42;
            X := 1;
            X := 2;
        end Test;
        """
        stmts, stats = get_optimized_stmts(code)
        # Should only have the return statement
        assert len(stmts) == 1
        assert stats.dead_code_eliminated >= 1

    def test_while_false_eliminated(self):
        """Test while False loop is eliminated."""
        code = """
        procedure Test is
            X : Integer;
        begin
            while False loop
                X := X + 1;
            end loop;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt, NullStmt)


# =============================================================================
# Optimizer Configuration Tests
# =============================================================================


class TestOptimizerConfig:
    """Tests for optimizer configuration."""

    def test_level_0_no_optimization(self):
        """Test level 0 performs no optimizations."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 2 + 3;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code, level=0)
        # Should remain as binary expression
        assert isinstance(stmt.value, BinaryExpr)
        assert stats.total() == 0

    def test_level_1_basic_only(self):
        """Test level 1 does constant folding but not dead code elimination."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 2 + 3;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code, level=1)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 5

    def test_level_2_standard(self):
        """Test level 2 includes dead code elimination."""
        code = """
        procedure Test is
            X : Integer;
        begin
            if True then
                X := 1;
            end if;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code, level=2)
        assert isinstance(stmt, AssignmentStmt)

    def test_level_3_aggressive(self):
        """Test level 3 enables all optimizations."""
        config = OptimizerConfig.for_level(3)
        assert config.fold_constants is True
        assert config.eliminate_cse is True
        assert config.propagate_copies is True
        assert config.move_loop_invariants is True


# =============================================================================
# Ada-Specific Optimization Tests
# =============================================================================


class TestAdaSpecificOptimizations:
    """Tests for Ada-specific optimizations."""

    def test_range_first_attribute(self):
        """Test evaluation of 'First attribute on range."""
        # This would require attribute evaluation support
        # Currently a placeholder for future implementation
        pass

    def test_range_last_attribute(self):
        """Test evaluation of 'Last attribute on range."""
        pass

    def test_range_length_attribute(self):
        """Test evaluation of 'Length attribute on range."""
        pass


# =============================================================================
# Integration Tests
# =============================================================================


class TestOptimizerIntegration:
    """Integration tests for the optimizer."""

    def test_multiple_passes(self):
        """Test that optimizer runs multiple passes."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 1 + (2 + 0);
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        # First pass: 2 + 0 -> 2
        # Second pass: 1 + 2 -> 3
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 3

    def test_complex_expression_optimization(self):
        """Test optimization of complex expression."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := ((1 * 5) + (3 - 3)) * 2;
        end Test;
        """
        stmt, stats = get_optimized_stmt(code)
        assert isinstance(stmt.value, IntegerLiteral)
        assert stmt.value.value == 10

    def test_optimizer_preserves_side_effects(self):
        """Test that optimizer doesn't remove side effects."""
        code = """
        procedure Test is
            X : Integer;
        begin
            X := 1;
            X := 2;
            X := 3;
        end Test;
        """
        stmts, stats = get_optimized_stmts(code)
        # All assignments should remain (we don't do dead store elimination yet)
        assert len(stmts) == 3

    def test_optimize_in_declarations(self):
        """Test optimization of declaration initializers."""
        code = """
        procedure Test is
            X : constant Integer := 10 + 5;
            Y : Integer;
        begin
            Y := 0;
        end Test;
        """
        ast = parse(code)
        opt_ast, stats = optimize(ast, level=2)
        body = opt_ast.units[0].unit
        # Check the constant declaration was folded
        x_decl = body.declarations[0]
        assert isinstance(x_decl.init_expr, IntegerLiteral)
        assert x_decl.init_expr.value == 15
