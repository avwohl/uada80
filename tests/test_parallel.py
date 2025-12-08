"""Tests for Ada 2022 parallel constructs."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import ParallelBlockStmt, LoopStmt


class TestParallelLoopParsing:
    """Tests for parsing parallel loops."""

    def test_parallel_for_loop(self):
        """Test parsing parallel for loop."""
        source = """
        procedure Test is
            Sum : Integer := 0;
        begin
            parallel for I in 1 .. 10 loop
                Sum := Sum + I;
            end loop;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_parallel_loop_with_body(self):
        """Test parallel loop with multiple statements."""
        source = """
        procedure Process is
            A : array (1 .. 100) of Integer := (others => 0);
        begin
            parallel for I in 1 .. 100 loop
                A(I) := I * 2;
            end loop;
        end Process;
        """
        ast = parse(source)
        # Should parse without errors


class TestParallelBlockParsing:
    """Tests for parsing parallel blocks."""

    def test_simple_parallel_block(self):
        """Test parsing simple parallel block."""
        source = """
        procedure Test is
            A, B : Integer := 0;
        begin
            parallel do
                A := 1;
            and do
                B := 2;
            end parallel;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors
        body = ast.units[0].unit
        found = False
        for stmt in body.statements:
            if isinstance(stmt, ParallelBlockStmt):
                found = True
                assert len(stmt.sequences) == 2
        assert found

    def test_parallel_block_three_sequences(self):
        """Test parallel block with three sequences."""
        source = """
        procedure Test is
            A, B, C : Integer := 0;
        begin
            parallel do
                A := 1;
            and do
                B := 2;
            and do
                C := 3;
            end parallel;
        end Test;
        """
        ast = parse(source)
        body = ast.units[0].unit
        for stmt in body.statements:
            if isinstance(stmt, ParallelBlockStmt):
                assert len(stmt.sequences) == 3


class TestParallelSemantic:
    """Tests for semantic analysis of parallel constructs."""

    def test_parallel_loop_analysis(self):
        """Test semantic analysis of parallel loop."""
        source = """
        procedure Test is
            type Arr is array (1 .. 10) of Integer;
            Data : Arr := (others => 0);
        begin
            parallel for I in 1 .. 10 loop
                Data(I) := I * I;
            end loop;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_parallel_block_analysis(self):
        """Test semantic analysis of parallel block."""
        source = """
        procedure Test is
            X, Y : Integer := 0;
        begin
            parallel do
                X := 10;
            and do
                Y := 20;
            end parallel;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_parallel_loop_variable_scope(self):
        """Test loop variable scope in parallel loop."""
        source = """
        procedure Test is
            Sum : Integer := 0;
        begin
            parallel for I in 1 .. 10 loop
                Sum := Sum + I;
            end loop;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestParallelUseCases:
    """Tests for common parallel construct use cases."""

    def test_parallel_array_init(self):
        """Test parallel initialization of array."""
        source = """
        procedure Init_Array is
            type Int_Array is array (1 .. 1000) of Integer;
            Data : Int_Array;
        begin
            parallel for I in 1 .. 1000 loop
                Data(I) := I;
            end loop;
        end Init_Array;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_parallel_independent_tasks(self):
        """Test parallel block for independent tasks."""
        source = """
        procedure Do_Tasks is
            Result1 : Integer := 0;
            Result2 : Integer := 0;
        begin
            parallel do
                Result1 := 42;
            and do
                Result2 := 100;
            end parallel;
        end Do_Tasks;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_parallel_with_function_calls(self):
        """Test parallel block with function calls."""
        source = """
        package Test is
            function Compute_A return Integer is (42);
            function Compute_B return Integer is (100);

            procedure Run is
                A, B : Integer;
            begin
                parallel do
                    A := Compute_A;
                and do
                    B := Compute_B;
                end parallel;
            end Run;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
