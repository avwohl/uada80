"""Tests for Ada protected types and objects."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestProtectedTypeParsing:
    """Tests for parsing protected type declarations."""

    def test_simple_protected_type(self):
        """Test parsing simple protected type."""
        source = """
        package Test is
            protected type Counter is
                procedure Increment;
                function Value return Integer;
            private
                Count : Integer := 0;
            end Counter;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_protected_type_with_entry(self):
        """Test protected type with entry."""
        source = """
        package Test is
            protected type Barrier is
                entry Wait;
                procedure Release;
            private
                Open : Boolean := False;
            end Barrier;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_protected_type_entry_family(self):
        """Test protected type with entry family."""
        source = """
        package Test is
            protected type Channel is
                entry Read(1 .. 10)(Data : out Integer);
            private
                Buffer : array (1 .. 10) of Integer;
            end Channel;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestProtectedBodyParsing:
    """Tests for parsing protected bodies."""

    def test_simple_protected_body(self):
        """Test parsing protected body."""
        source = """
        procedure Main is
            protected type Counter is
                procedure Increment;
                function Value return Integer;
            private
                Count : Integer := 0;
            end Counter;

            protected body Counter is
                procedure Increment is
                begin
                    Count := Count + 1;
                end Increment;

                function Value return Integer is
                begin
                    return Count;
                end Value;
            end Counter;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        # Should parse without errors

    def test_protected_body_with_entry(self):
        """Test protected body with entry implementation."""
        source = """
        procedure Main is
            protected type Gate is
                entry Enter;
                procedure Open;
            private
                Is_Open : Boolean := False;
            end Gate;

            protected body Gate is
                entry Enter when Is_Open is
                begin
                    null;
                end Enter;

                procedure Open is
                begin
                    Is_Open := True;
                end Open;
            end Gate;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        # Should parse without errors


class TestSingleProtectedObjects:
    """Tests for single protected objects."""

    def test_single_protected_object(self):
        """Test single protected object declaration."""
        source = """
        procedure Main is
            protected Shared_Counter is
                procedure Increment;
                function Value return Integer;
            private
                Count : Integer := 0;
            end Shared_Counter;

            protected body Shared_Counter is
                procedure Increment is
                begin
                    Count := Count + 1;
                end Increment;

                function Value return Integer is
                begin
                    return Count;
                end Value;
            end Shared_Counter;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestProtectedSemantic:
    """Tests for semantic analysis of protected types."""

    def test_protected_type_creates_symbol(self):
        """Test that protected type creates symbol."""
        source = """
        procedure Main is
            protected type Lock is
                procedure Acquire;
                procedure Release;
            private
                Locked : Boolean := False;
            end Lock;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_protected_variable(self):
        """Test variable of protected type."""
        source = """
        procedure Main is
            protected type Mutex is
                procedure Lock;
            private
                Locked : Boolean := False;
            end Mutex;

            M : Mutex;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should process without errors


class TestProtectedUseCases:
    """Tests for common protected type use cases."""

    def test_semaphore_pattern(self):
        """Test semaphore pattern with protected type."""
        source = """
        package Sync is
            protected type Semaphore is
                entry Acquire;
                procedure Release;
            private
                Count : Integer := 1;
            end Semaphore;
        end Sync;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_buffer_pattern(self):
        """Test bounded buffer pattern."""
        source = """
        package Buffers is
            protected type Buffer is
                entry Put(X : Integer);
                entry Get(X : out Integer);
            private
                Value : Integer;
                Full : Boolean := False;
            end Buffer;
        end Buffers;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_read_write_lock(self):
        """Test readers-writer lock pattern."""
        source = """
        package RW_Lock is
            protected type Lock is
                entry Start_Read;
                procedure End_Read;
                entry Start_Write;
                procedure End_Write;
            private
                Readers : Integer := 0;
                Writing : Boolean := False;
            end Lock;
        end RW_Lock;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_protected_procedure_call(self):
        """Test calling protected procedure."""
        source = """
        procedure Main is
            protected Shared is
                procedure Set(X : Integer);
                function Get return Integer;
            private
                Value : Integer := 0;
            end Shared;

            protected body Shared is
                procedure Set(X : Integer) is
                begin
                    Value := X;
                end Set;

                function Get return Integer is
                begin
                    return Value;
                end Get;
            end Shared;
        begin
            Shared.Set(42);
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors
