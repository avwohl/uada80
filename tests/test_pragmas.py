"""Tests for Ada pragmas."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze


class TestPragmaParsing:
    """Tests for parsing pragmas."""

    def test_pragma_no_args(self):
        """Test pragma with no arguments."""
        source = """
        pragma Pure;
        package Test is
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_with_identifier(self):
        """Test pragma with identifier argument."""
        source = """
        pragma Elaborate(Test);
        package Test is
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_elaborate_all(self):
        """Test pragma Elaborate_All."""
        source = """
        with Ada.Text_IO;
        pragma Elaborate_All(Ada.Text_IO);

        procedure Test is
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_preelaborate(self):
        """Test pragma Preelaborate."""
        source = """
        pragma Preelaborate;
        package Test is
            X : constant Integer := 0;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_inline(self):
        """Test pragma Inline."""
        source = """
        package Test is
            procedure Fast;
            pragma Inline(Fast);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestPackagePragmas:
    """Tests for package-level pragmas."""

    def test_pragma_pure(self):
        """Test pragma Pure for a package."""
        source = """
        pragma Pure;
        package Pure_Math is
            function Square(X : Integer) return Integer;
        end Pure_Math;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_pragma_remote_call_interface(self):
        """Test pragma Remote_Call_Interface."""
        source = """
        pragma Remote_Call_Interface;
        package RPC_Server is
            procedure Process;
        end RPC_Server;
        """
        ast = parse(source)
        # Should parse without errors


class TestImportExportPragmas:
    """Tests for Import/Export pragmas."""

    def test_pragma_import(self):
        """Test pragma Import."""
        source = """
        package C_Bindings is
            procedure printf;
            pragma Import(C, printf, "printf");
        end C_Bindings;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_export(self):
        """Test pragma Export."""
        source = """
        package Test is
            procedure Ada_Proc;
            pragma Export(C, Ada_Proc, "ada_proc");
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_convention(self):
        """Test pragma Convention."""
        source = """
        package Test is
            type C_Array is array (0 .. 9) of Integer;
            pragma Convention(C, C_Array);
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestControlPragmas:
    """Tests for control pragmas."""

    def test_pragma_suppress(self):
        """Test pragma Suppress."""
        source = """
        procedure Test is
            pragma Suppress(Range_Check);
            X : Integer range 1 .. 10;
        begin
            X := 5;
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_pragma_unsuppress(self):
        """Test pragma Unsuppress."""
        source = """
        procedure Test is
            pragma Unsuppress(Range_Check);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_assertion_policy(self):
        """Test pragma Assertion_Policy."""
        source = """
        pragma Assertion_Policy(Check);
        procedure Test is
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestOptimizationPragmas:
    """Tests for optimization pragmas."""

    def test_pragma_optimize(self):
        """Test pragma Optimize."""
        source = """
        procedure Test is
            pragma Optimize(Time);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_pack(self):
        """Test pragma Pack."""
        source = """
        package Test is
            type Byte_Array is array (1 .. 100) of Boolean;
            pragma Pack(Byte_Array);
        end Test;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors


class TestAda2012Pragmas:
    """Tests for Ada 2012 pragmas."""

    def test_pragma_default_initial_condition(self):
        """Test pragma Default_Initial_Condition."""
        source = """
        package Test is
            type T is private;
            pragma Default_Initial_Condition;
        private
            type T is new Integer;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_pragma_preelaborable_initialization(self):
        """Test pragma Preelaborable_Initialization."""
        source = """
        pragma Preelaborate;
        package Test is
            type T is private;
            pragma Preelaborable_Initialization(T);
        private
            type T is new Integer;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors


class TestPragmaUseCases:
    """Tests for common pragma use cases."""

    def test_c_interface_pattern(self):
        """Test C interface with pragmas."""
        source = """
        package C_Interface is
            type C_Int is new Integer;
            pragma Convention(C, C_Int);

            procedure C_Function(X : C_Int);
            pragma Import(C, C_Function, "c_function");
        end C_Interface;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

    def test_debug_pragma(self):
        """Test debug pragmas."""
        source = """
        procedure Test is
            pragma Debug(null);
        begin
            null;
        end Test;
        """
        ast = parse(source)
        # Should parse without errors

    def test_linker_options(self):
        """Test pragma Linker_Options."""
        source = """
        pragma Linker_Options("-lm");
        package Math_Binding is
        end Math_Binding;
        """
        ast = parse(source)
        # Should parse without errors
