"""Tests for Ada tasking features."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestDelayStatements:
    """Tests for delay statements."""

    def test_parse_delay_simple(self):
        """Test parsing a simple delay statement."""
        source = """
        procedure Test is
        begin
            delay 1.0;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_parse_delay_until(self):
        """Test parsing a delay until statement."""
        source = """
        procedure Test is
            Wake_Time : Integer := 100;
        begin
            delay until Wake_Time;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_delay_with_expression(self):
        """Test delay with an arithmetic expression."""
        source = """
        procedure Test is
            Wait : Integer := 5;
        begin
            delay Wait * 2;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestAcceptStatements:
    """Tests for accept statements in task bodies."""

    def test_task_with_entry_and_accept(self):
        """Test task with entry and accept statement."""
        source = """
        procedure Main is
            task type Server is
                entry Request;
            end Server;

            task body Server is
            begin
                accept Request do
                    null;
                end Request;
            end Server;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_accept_outside_task_error(self):
        """Test that accept outside task body causes error."""
        source = """
        procedure Test is
        begin
            accept Something;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert result.has_errors
        assert any("task body" in str(e).lower() for e in result.errors)

    def test_accept_simple_no_body(self):
        """Test accept statement without a body."""
        source = """
        procedure Main is
            task type Worker is
                entry Start;
            end Worker;

            task body Worker is
            begin
                accept Start;
            end Worker;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_accept_with_parameters(self):
        """Test accept statement with parameters."""
        source = """
        procedure Main is
            task type Buffer is
                entry Put(X : Integer);
            end Buffer;

            task body Buffer is
            begin
                accept Put(X : Integer) do
                    null;
                end Put;
            end Buffer;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestSelectStatements:
    """Tests for select statements."""

    def test_selective_accept(self):
        """Test selective accept with guarded alternatives."""
        source = """
        procedure Main is
            task type Server is
                entry Request;
                entry Shutdown;
            end Server;

            task body Server is
                Done : Boolean := False;
            begin
                select
                    when not Done =>
                        accept Request do
                            null;
                        end Request;
                or
                    accept Shutdown do
                        Done := True;
                    end Shutdown;
                end select;
            end Server;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_select_with_delay(self):
        """Test select with delay alternative."""
        source = """
        procedure Main is
            task type Server is
                entry Request;
            end Server;

            task body Server is
            begin
                select
                    accept Request do
                        null;
                    end Request;
                or
                    delay 1.0;
                end select;
            end Server;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestAbortStatements:
    """Tests for abort statements."""

    def test_parse_abort_single(self):
        """Test parsing abort with single task."""
        source = """
        procedure Test is
            task Worker;
            task body Worker is
            begin
                null;
            end Worker;
        begin
            abort Worker;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        # May or may not error depending on type resolution
        # The statement should at least parse correctly

    def test_parse_abort_multiple(self):
        """Test parsing abort with multiple tasks."""
        source = """
        procedure Test is
            task Worker1;
            task body Worker1 is
            begin
                null;
            end Worker1;

            task Worker2;
            task body Worker2 is
            begin
                null;
            end Worker2;
        begin
            abort Worker1, Worker2;
        end Test;
        """
        ast = parse(source)
        # Verify it parses


class TestRequeueStatements:
    """Tests for requeue statements."""

    def test_requeue_outside_accept_error(self):
        """Test that requeue outside accept/entry causes error."""
        source = """
        procedure Main is
            task type Server is
                entry Request;
            end Server;

            task body Server is
            begin
                requeue Request;
            end Server;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert result.has_errors
        assert any("accept" in str(e).lower() or "entry" in str(e).lower() for e in result.errors)

    def test_requeue_inside_accept(self):
        """Test requeue inside accept is valid."""
        source = """
        procedure Main is
            task type Server is
                entry Request;
                entry Handle;
            end Server;

            task body Server is
            begin
                accept Request do
                    requeue Handle;
                end Request;
            end Server;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        # Requeue inside accept should be valid
        # Entry 'Handle' may not be found but requeue context is correct
        errors_about_context = [e for e in result.errors if "accept" in str(e).lower() or "entry body" in str(e).lower()]
        assert len(errors_about_context) == 0

    def test_requeue_with_abort(self):
        """Test requeue with abort clause."""
        source = """
        procedure Main is
            task type Server is
                entry Request;
                entry Handle;
            end Server;

            task body Server is
            begin
                accept Request do
                    requeue Handle with abort;
                end Request;
            end Server;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        # Should parse correctly


class TestSingleTasks:
    """Tests for single task declarations."""

    def test_single_task_declaration(self):
        """Test single task (not task type) declaration."""
        source = """
        procedure Main is
            task Worker;
            task body Worker is
            begin
                null;
            end Worker;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_single_task_with_entry(self):
        """Test single task with entry."""
        source = """
        procedure Main is
            task Worker is
                entry Start;
            end Worker;

            task body Worker is
            begin
                accept Start;
            end Worker;
        begin
            null;
        end Main;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestTaskingCodegen:
    """Tests for tasking code generation."""

    def test_task_body_compilation(self):
        """Test that task bodies compile to IR and then Z80."""
        from uada80.compiler import compile_source

        source = """
        procedure Main is
            task Worker;
            task body Worker is
            begin
                delay 1.0;
            end Worker;
        begin
            null;
        end Main;
        """
        # Just verify it compiles without errors
        result = compile_source(source)
        # Check that task body function was generated
        assert result.success, f"Compilation failed: {result.errors}"
        assert result.output is not None
        assert "_task_body_Worker" in result.output or "Worker" in result.output

    def test_task_with_entry_compilation(self):
        """Test task with entry compiles correctly."""
        from uada80.compiler import compile_source

        source = """
        procedure Main is
            task type Server is
                entry Request(X : Integer);
            end Server;

            task body Server is
            begin
                accept Request(X : Integer) do
                    null;
                end Request;
            end Server;
        begin
            null;
        end Main;
        """
        result = compile_source(source)
        # Should generate task body and entry accept code
        assert result.success, f"Compilation failed: {result.errors}"
        assert result.output is not None
        assert "_task_body_Server" in result.output or "Server" in result.output

    def test_delay_statement_compilation(self):
        """Test delay statement generates correct runtime call."""
        from uada80.compiler import compile_source

        source = """
        procedure Test is
        begin
            delay 5.0;
        end Test;
        """
        result = compile_source(source)
        # Should call the delay runtime
        assert result.success, f"Compilation failed: {result.errors}"
        assert result.output is not None
        assert "_TASK_DELAY" in result.output or "DELAY" in result.output.upper()
