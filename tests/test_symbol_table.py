"""Tests for the symbol table."""

import pytest
from uada80.symbol_table import (
    SymbolTable,
    Symbol,
    SymbolKind,
    Scope,
)
from uada80.type_system import PREDEFINED_TYPES, IntegerType


# ============================================================================
# Basic Symbol Table Tests
# ============================================================================


def test_predefined_types_available():
    """Test that predefined types are in the symbol table."""
    symbols = SymbolTable()

    # Check predefined types exist
    int_sym = symbols.lookup("Integer")
    assert int_sym is not None
    assert int_sym.kind == SymbolKind.TYPE
    assert int_sym.ada_type == PREDEFINED_TYPES["Integer"]

    bool_sym = symbols.lookup("Boolean")
    assert bool_sym is not None
    assert bool_sym.kind == SymbolKind.TYPE

    char_sym = symbols.lookup("Character")
    assert char_sym is not None


def test_predefined_exceptions():
    """Test that predefined exceptions are available."""
    symbols = SymbolTable()

    ce = symbols.lookup("Constraint_Error")
    assert ce is not None
    assert ce.kind == SymbolKind.EXCEPTION


def test_case_insensitive_lookup():
    """Test that lookup is case-insensitive."""
    symbols = SymbolTable()

    # Ada is case-insensitive
    assert symbols.lookup("INTEGER") is not None
    assert symbols.lookup("integer") is not None
    assert symbols.lookup("Integer") is not None


def test_define_variable():
    """Test defining a variable."""
    symbols = SymbolTable()

    var = Symbol(
        name="X",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(var)

    result = symbols.lookup("X")
    assert result is not None
    assert result.name == "X"
    assert result.kind == SymbolKind.VARIABLE
    assert result.ada_type == PREDEFINED_TYPES["Integer"]


def test_define_constant():
    """Test defining a constant."""
    symbols = SymbolTable()

    const = Symbol(
        name="Pi",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
        is_constant=True,
    )
    symbols.define(const)

    result = symbols.lookup("Pi")
    assert result is not None
    assert result.is_constant


# ============================================================================
# Scope Tests
# ============================================================================


def test_enter_leave_scope():
    """Test entering and leaving scopes."""
    symbols = SymbolTable()
    initial_level = symbols.scope_level

    # Enter a scope
    symbols.enter_scope("Test")
    assert symbols.scope_level == initial_level + 1

    # Leave the scope
    symbols.leave_scope()
    assert symbols.scope_level == initial_level


def test_nested_scopes():
    """Test nested scope handling."""
    symbols = SymbolTable()

    # Define in outer scope
    outer_var = Symbol(
        name="Outer",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(outer_var)

    # Enter inner scope
    symbols.enter_scope("Inner")

    # Define in inner scope
    inner_var = Symbol(
        name="Inner",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(inner_var)

    # Both should be visible
    assert symbols.lookup("Outer") is not None
    assert symbols.lookup("Inner") is not None

    # Leave inner scope
    symbols.leave_scope()

    # Only outer should be visible
    assert symbols.lookup("Outer") is not None
    assert symbols.lookup("Inner") is None


def test_shadowing():
    """Test that inner scope shadows outer scope."""
    symbols = SymbolTable()

    # Define X in outer scope
    outer_x = Symbol(
        name="X",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(outer_x)

    # Enter inner scope and define X again
    symbols.enter_scope("Inner")

    inner_x = Symbol(
        name="X",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Boolean"],  # Different type
    )
    symbols.define(inner_x)

    # Should find inner X
    result = symbols.lookup("X")
    assert result is not None
    assert result.ada_type == PREDEFINED_TYPES["Boolean"]

    # Leave inner scope
    symbols.leave_scope()

    # Now should find outer X
    result = symbols.lookup("X")
    assert result is not None
    assert result.ada_type == PREDEFINED_TYPES["Integer"]


def test_local_lookup():
    """Test local-only lookup."""
    symbols = SymbolTable()

    outer_var = Symbol(
        name="Outer",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(outer_var)

    symbols.enter_scope("Inner")

    inner_var = Symbol(
        name="Inner",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(inner_var)

    # Local lookup should only find inner
    assert symbols.lookup_local("Inner") is not None
    assert symbols.lookup_local("Outer") is None

    # Full lookup finds both
    assert symbols.lookup("Inner") is not None
    assert symbols.lookup("Outer") is not None


def test_is_defined_locally():
    """Test checking if name is defined locally."""
    symbols = SymbolTable()

    symbols.define(
        Symbol(name="X", kind=SymbolKind.VARIABLE, ada_type=PREDEFINED_TYPES["Integer"])
    )

    assert symbols.is_defined_locally("X")
    assert not symbols.is_defined_locally("Y")

    # Enter a new scope - predefined types are no longer locally defined
    symbols.enter_scope("Inner")
    assert not symbols.is_defined_locally("Integer")
    assert not symbols.is_defined_locally("X")


# ============================================================================
# Type Lookup Tests
# ============================================================================


def test_lookup_type():
    """Test type lookup."""
    symbols = SymbolTable()

    # Lookup predefined type
    int_type = symbols.lookup_type("Integer")
    assert int_type is not None
    assert int_type == PREDEFINED_TYPES["Integer"]

    # Define a custom type
    my_type = IntegerType(name="MyInt", size_bits=8, low=0, high=100)
    symbols.define(Symbol(name="MyInt", kind=SymbolKind.TYPE, ada_type=my_type))

    result = symbols.lookup_type("MyInt")
    assert result is not None
    assert result == my_type


def test_lookup_type_not_found():
    """Test type lookup for non-existent type."""
    symbols = SymbolTable()

    result = symbols.lookup_type("NonExistent")
    assert result is None


def test_lookup_type_variable_not_type():
    """Test that lookup_type returns None for variables."""
    symbols = SymbolTable()

    symbols.define(
        Symbol(name="X", kind=SymbolKind.VARIABLE, ada_type=PREDEFINED_TYPES["Integer"])
    )

    result = symbols.lookup_type("X")
    assert result is None


# ============================================================================
# Overloading Tests
# ============================================================================


def test_function_overloading():
    """Test that functions can be overloaded."""
    symbols = SymbolTable()

    # Define two functions with same name
    func1 = Symbol(
        name="Process",
        kind=SymbolKind.FUNCTION,
        ada_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(func1)

    func2 = Symbol(
        name="Process",
        kind=SymbolKind.FUNCTION,
        ada_type=PREDEFINED_TYPES["Boolean"],
    )
    symbols.define(func2)

    # Both should be accessible
    overloads = symbols.all_overloads("Process")
    assert len(overloads) == 2


def test_procedure_overloading():
    """Test that procedures can be overloaded."""
    symbols = SymbolTable()

    proc1 = Symbol(name="Put", kind=SymbolKind.PROCEDURE)
    symbols.define(proc1)

    proc2 = Symbol(name="Put", kind=SymbolKind.PROCEDURE)
    symbols.define(proc2)

    overloads = symbols.all_overloads("Put")
    assert len(overloads) == 2


def test_no_overloading_for_variables():
    """Test that variables cannot be overloaded (they replace)."""
    symbols = SymbolTable()

    var1 = Symbol(
        name="X",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(var1)

    var2 = Symbol(
        name="X",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Boolean"],
    )
    symbols.define(var2)

    # Second definition replaces first
    overloads = symbols.all_overloads("X")
    assert len(overloads) == 1
    assert overloads[0].ada_type == PREDEFINED_TYPES["Boolean"]


# ============================================================================
# Package Tests
# ============================================================================


def test_package_scope():
    """Test package scope handling."""
    symbols = SymbolTable()

    # Create package symbol
    pkg = Symbol(name="MyPkg", kind=SymbolKind.PACKAGE)
    symbols.define(pkg)

    # Enter package scope
    scope = symbols.enter_scope("MyPkg", is_package=True)
    assert scope.is_package

    # Define something in package
    func = Symbol(
        name="Func",
        kind=SymbolKind.FUNCTION,
        return_type=PREDEFINED_TYPES["Integer"],
    )
    symbols.define(func)

    # Add to package's public symbols
    pkg.public_symbols["func"] = func

    symbols.leave_scope()

    # Look up via package prefix
    result = symbols.lookup_selected("MyPkg", "Func")
    assert result is not None
    assert result.name == "Func"


def test_use_clause():
    """Test use clause makes names directly visible."""
    symbols = SymbolTable()

    # Create package with a function
    pkg = Symbol(name="Utils", kind=SymbolKind.PACKAGE)
    func = Symbol(
        name="Helper",
        kind=SymbolKind.FUNCTION,
        return_type=PREDEFINED_TYPES["Integer"],
    )
    pkg.public_symbols["helper"] = func

    symbols.define(pkg)

    # Without use clause, Helper not visible
    assert symbols.lookup("Helper") is None

    # Add use clause
    symbols.add_use_clause(pkg)

    # Now Helper should be visible
    result = symbols.lookup("Helper")
    assert result is not None
    assert result.name == "Helper"


# ============================================================================
# Parameter Tests
# ============================================================================


def test_parameter_symbol():
    """Test parameter symbols."""
    symbols = SymbolTable()

    param = Symbol(
        name="X",
        kind=SymbolKind.PARAMETER,
        ada_type=PREDEFINED_TYPES["Integer"],
        mode="in",
    )
    symbols.define(param)

    result = symbols.lookup("X")
    assert result is not None
    assert result.kind == SymbolKind.PARAMETER
    assert result.mode == "in"


def test_out_parameter():
    """Test out parameter mode."""
    symbols = SymbolTable()

    param = Symbol(
        name="Result",
        kind=SymbolKind.PARAMETER,
        ada_type=PREDEFINED_TYPES["Integer"],
        mode="out",
    )
    symbols.define(param)

    result = symbols.lookup("Result")
    assert result is not None
    assert result.mode == "out"


def test_in_out_parameter():
    """Test in out parameter mode."""
    symbols = SymbolTable()

    param = Symbol(
        name="Buffer",
        kind=SymbolKind.PARAMETER,
        ada_type=PREDEFINED_TYPES["String"],
        mode="in out",
    )
    symbols.define(param)

    result = symbols.lookup("Buffer")
    assert result is not None
    assert result.mode == "in out"


# ============================================================================
# Error Cases
# ============================================================================


def test_leave_scope_at_root():
    """Test that leaving the root scope raises error."""
    symbols = SymbolTable()

    with pytest.raises(RuntimeError):
        symbols.leave_scope()


def test_add_use_clause_non_package():
    """Test that use clause on non-package raises error."""
    symbols = SymbolTable()

    var = Symbol(name="X", kind=SymbolKind.VARIABLE)
    symbols.define(var)

    with pytest.raises(ValueError):
        symbols.add_use_clause(var)
