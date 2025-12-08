"""Tests for Ada standard library packages."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.symbol_table import SymbolTable, SymbolKind


class TestAdaCalendar:
    """Tests for Ada.Calendar package."""

    def test_calendar_package_exists(self):
        """Test that Ada.Calendar package is available."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "calendar" in ada_pkg.public_symbols

    def test_calendar_types(self):
        """Test Ada.Calendar types."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        calendar = ada_pkg.public_symbols["calendar"]

        assert "time" in calendar.public_symbols
        assert "year_number" in calendar.public_symbols
        assert "month_number" in calendar.public_symbols
        assert "day_number" in calendar.public_symbols

    def test_calendar_clock_function(self):
        """Test Ada.Calendar.Clock function."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        calendar = ada_pkg.public_symbols["calendar"]

        clock = calendar.public_symbols["clock"]
        assert clock.kind == SymbolKind.FUNCTION
        assert clock.return_type is not None

    def test_calendar_time_error_exception(self):
        """Test Ada.Calendar.Time_Error exception."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        calendar = ada_pkg.public_symbols["calendar"]

        time_error = calendar.public_symbols["time_error"]
        assert time_error.kind == SymbolKind.EXCEPTION

    def test_calendar_usage_in_code(self):
        """Test using Ada.Calendar in code."""
        source = """
        with Ada.Calendar;
        procedure Test is
            Now : Ada.Calendar.Time;
        begin
            Now := Ada.Calendar.Clock;
        end Test;
        """
        ast = parse(source)
        # Should parse correctly


class TestAdaNumerics:
    """Tests for Ada.Numerics packages."""

    def test_numerics_package_exists(self):
        """Test that Ada.Numerics package is available."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "numerics" in ada_pkg.public_symbols

    def test_numerics_constants(self):
        """Test Ada.Numerics constants (Pi, e)."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        numerics = ada_pkg.public_symbols["numerics"]

        assert "pi" in numerics.public_symbols
        assert "e" in numerics.public_symbols

        pi = numerics.public_symbols["pi"]
        assert pi.is_constant

    def test_elementary_functions(self):
        """Test Ada.Numerics.Elementary_Functions."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        numerics = ada_pkg.public_symbols["numerics"]

        assert "elementary_functions" in numerics.public_symbols
        elem_funcs = numerics.public_symbols["elementary_functions"]

        # Check math functions exist
        assert "sqrt" in elem_funcs.public_symbols
        assert "sin" in elem_funcs.public_symbols
        assert "cos" in elem_funcs.public_symbols
        assert "tan" in elem_funcs.public_symbols
        assert "log" in elem_funcs.public_symbols
        assert "exp" in elem_funcs.public_symbols

    def test_float_random(self):
        """Test Ada.Numerics.Float_Random."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        numerics = ada_pkg.public_symbols["numerics"]

        assert "float_random" in numerics.public_symbols
        float_random = numerics.public_symbols["float_random"]

        assert "generator" in float_random.public_symbols
        assert "random" in float_random.public_symbols
        assert "reset" in float_random.public_symbols

    def test_discrete_random_generic(self):
        """Test Ada.Numerics.Discrete_Random is a generic."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        numerics = ada_pkg.public_symbols["numerics"]

        assert "discrete_random" in numerics.public_symbols
        discrete_random = numerics.public_symbols["discrete_random"]
        assert discrete_random.kind == SymbolKind.GENERIC_PACKAGE

    def test_numerics_usage_in_code(self):
        """Test using Ada.Numerics in code."""
        source = """
        with Ada.Numerics.Elementary_Functions;
        procedure Test is
            X : Float := 2.0;
            Y : Float;
        begin
            Y := Ada.Numerics.Elementary_Functions.Sqrt(X);
        end Test;
        """
        ast = parse(source)
        # Should parse correctly


class TestAdaTextIO:
    """Tests for Ada.Text_IO package."""

    def test_text_io_exists(self):
        """Test that Ada.Text_IO exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "text_io" in ada_pkg.public_symbols

    def test_text_io_put_line(self):
        """Test Ada.Text_IO.Put_Line exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        text_io = ada_pkg.public_symbols["text_io"]

        assert "put_line" in text_io.public_symbols
        put_line = text_io.public_symbols["put_line"]
        assert put_line.kind == SymbolKind.PROCEDURE


class TestAdaStrings:
    """Tests for Ada.Strings packages."""

    def test_strings_exists(self):
        """Test that Ada.Strings exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "strings" in ada_pkg.public_symbols

    def test_strings_fixed_exists(self):
        """Test that Ada.Strings.Fixed exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        strings = ada_pkg.public_symbols["strings"]
        assert "fixed" in strings.public_symbols


class TestAdaCommandLine:
    """Tests for Ada.Command_Line package."""

    def test_command_line_exists(self):
        """Test that Ada.Command_Line exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "command_line" in ada_pkg.public_symbols

    def test_command_line_argument_count(self):
        """Test Ada.Command_Line.Argument_Count exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        cmd_line = ada_pkg.public_symbols["command_line"]

        assert "argument_count" in cmd_line.public_symbols


class TestUncheckedOps:
    """Tests for Ada.Unchecked_* packages."""

    def test_unchecked_conversion_exists(self):
        """Test that Ada.Unchecked_Conversion exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "unchecked_conversion" in ada_pkg.public_symbols

        uc = ada_pkg.public_symbols["unchecked_conversion"]
        assert uc.kind == SymbolKind.GENERIC_FUNCTION

    def test_unchecked_deallocation_exists(self):
        """Test that Ada.Unchecked_Deallocation exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "unchecked_deallocation" in ada_pkg.public_symbols

        ud = ada_pkg.public_symbols["unchecked_deallocation"]
        assert ud.kind == SymbolKind.GENERIC_PROCEDURE


class TestAdaContainers:
    """Tests for Ada.Containers packages."""

    def test_containers_package_exists(self):
        """Test that Ada.Containers exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        assert ada_pkg is not None
        assert "containers" in ada_pkg.public_symbols

    def test_containers_types(self):
        """Test Ada.Containers types."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        containers = ada_pkg.public_symbols["containers"]

        assert "count_type" in containers.public_symbols
        assert "hash_type" in containers.public_symbols

    def test_containers_vectors(self):
        """Test Ada.Containers.Vectors exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        containers = ada_pkg.public_symbols["containers"]

        assert "vectors" in containers.public_symbols
        vectors = containers.public_symbols["vectors"]
        assert vectors.kind == SymbolKind.GENERIC_PACKAGE

    def test_containers_lists(self):
        """Test Ada.Containers.Doubly_Linked_Lists exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        containers = ada_pkg.public_symbols["containers"]

        assert "doubly_linked_lists" in containers.public_symbols
        lists = containers.public_symbols["doubly_linked_lists"]
        assert lists.kind == SymbolKind.GENERIC_PACKAGE

    def test_containers_hashed_maps(self):
        """Test Ada.Containers.Hashed_Maps exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        containers = ada_pkg.public_symbols["containers"]

        assert "hashed_maps" in containers.public_symbols
        hm = containers.public_symbols["hashed_maps"]
        assert hm.kind == SymbolKind.GENERIC_PACKAGE

    def test_containers_ordered_maps(self):
        """Test Ada.Containers.Ordered_Maps exists."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        containers = ada_pkg.public_symbols["containers"]

        assert "ordered_maps" in containers.public_symbols
        om = containers.public_symbols["ordered_maps"]
        assert om.kind == SymbolKind.GENERIC_PACKAGE

    def test_containers_sets(self):
        """Test Ada.Containers sets exist."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        containers = ada_pkg.public_symbols["containers"]

        assert "hashed_sets" in containers.public_symbols
        assert "ordered_sets" in containers.public_symbols

    def test_containers_indefinite(self):
        """Test Ada.Containers indefinite variants exist."""
        st = SymbolTable()
        ada_pkg = st.lookup("Ada")
        containers = ada_pkg.public_symbols["containers"]

        assert "indefinite_vectors" in containers.public_symbols
        assert "indefinite_doubly_linked_lists" in containers.public_symbols
        assert "indefinite_hashed_maps" in containers.public_symbols
        assert "indefinite_ordered_maps" in containers.public_symbols
