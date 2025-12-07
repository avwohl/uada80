"""
Symbol table for Ada semantic analysis.

Implements Ada's scoping and visibility rules:
- Block-structured scoping with nested scopes
- Package scopes with public/private regions
- Subprogram overloading
- Use clauses for direct visibility
- Derived type visibility
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Optional

from uada80.type_system import AdaType, PREDEFINED_TYPES
from uada80.ast_nodes import ASTNode


class SymbolKind(Enum):
    """Classification of symbols."""

    VARIABLE = auto()  # Variable or constant
    TYPE = auto()  # Type declaration
    SUBTYPE = auto()  # Subtype declaration
    PROCEDURE = auto()  # Procedure
    FUNCTION = auto()  # Function
    PARAMETER = auto()  # Formal parameter
    PACKAGE = auto()  # Package
    GENERIC_PACKAGE = auto()  # Generic package (template)
    EXCEPTION = auto()  # Exception
    LABEL = auto()  # Statement label
    LOOP = auto()  # Loop identifier for exit statements
    COMPONENT = auto()  # Record component


@dataclass
class Symbol:
    """A symbol in the symbol table."""

    name: str
    kind: SymbolKind
    ada_type: Optional[AdaType] = None  # Type of this symbol
    is_constant: bool = False  # For variables
    is_aliased: bool = False  # For aliased objects
    mode: Optional[str] = None  # For parameters: "in", "out", "in out"
    definition: Optional[ASTNode] = None  # AST node where defined
    scope_level: int = 0  # Nesting level where defined

    # For subprograms: list of parameter symbols
    parameters: list["Symbol"] = field(default_factory=list)
    return_type: Optional[AdaType] = None  # For functions

    # For packages
    public_symbols: dict[str, "Symbol"] = field(default_factory=dict)
    private_symbols: dict[str, "Symbol"] = field(default_factory=dict)

    # For overloading: chain of overloaded symbols with same name
    overloaded_next: Optional["Symbol"] = None


@dataclass
class Scope:
    """A scope in the symbol table."""

    name: str  # Scope name (package name, subprogram name, or "")
    level: int  # Nesting level
    symbols: dict[str, Symbol] = field(default_factory=dict)
    parent: Optional["Scope"] = None

    # For packages: track if we're in private part
    is_package: bool = False
    in_private_part: bool = False

    # Use clauses: list of package symbols whose names are directly visible
    use_clauses: list[Symbol] = field(default_factory=list)

    def define(self, symbol: Symbol) -> None:
        """Define a symbol in this scope."""
        name_lower = symbol.name.lower()

        # Check for overloading (only allowed for subprograms)
        if name_lower in self.symbols:
            existing = self.symbols[name_lower]
            if symbol.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION):
                if existing.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION):
                    # Add to overload chain
                    symbol.overloaded_next = existing
                    self.symbols[name_lower] = symbol
                    return
            # Not overloadable - this is an error caught by semantic analyzer
            # For now, just replace (semantic analyzer will report the error)

        self.symbols[name_lower] = symbol

    def lookup_local(self, name: str) -> Optional[Symbol]:
        """Look up a symbol in this scope only."""
        return self.symbols.get(name.lower())

    def lookup_use_clause(self, name: str) -> Optional[Symbol]:
        """Look up a symbol through use clauses."""
        name_lower = name.lower()
        for pkg in self.use_clauses:
            if name_lower in pkg.public_symbols:
                return pkg.public_symbols[name_lower]
        return None


class SymbolTable:
    """
    Symbol table with nested scopes.

    Manages symbol definition and lookup with Ada visibility rules.
    """

    def __init__(self) -> None:
        """Initialize with predefined scope."""
        self.current_scope: Scope = Scope(name="Standard", level=0)
        self.scope_stack: list[Scope] = [self.current_scope]

        # Add predefined types to standard scope
        self._init_predefined()

    def _init_predefined(self) -> None:
        """Add predefined types and values to the symbol table."""
        for name, ada_type in PREDEFINED_TYPES.items():
            symbol = Symbol(
                name=name,
                kind=SymbolKind.TYPE,
                ada_type=ada_type,
                scope_level=0,
            )
            self.current_scope.define(symbol)

        # Add Boolean literals (True and False)
        bool_type = PREDEFINED_TYPES["Boolean"]
        self.current_scope.define(Symbol(
            name="True",
            kind=SymbolKind.VARIABLE,
            ada_type=bool_type,
            is_constant=True,
            scope_level=0,
        ))
        self.current_scope.define(Symbol(
            name="False",
            kind=SymbolKind.VARIABLE,
            ada_type=bool_type,
            is_constant=True,
            scope_level=0,
        ))

        # Add predefined exceptions
        for exc_name in ["Constraint_Error", "Program_Error", "Storage_Error"]:
            symbol = Symbol(
                name=exc_name,
                kind=SymbolKind.EXCEPTION,
                scope_level=0,
            )
            self.current_scope.define(symbol)

    def enter_scope(self, name: str = "", is_package: bool = False) -> Scope:
        """Enter a new nested scope."""
        new_scope = Scope(
            name=name,
            level=len(self.scope_stack),
            parent=self.current_scope,
            is_package=is_package,
        )
        self.scope_stack.append(new_scope)
        self.current_scope = new_scope
        return new_scope

    def leave_scope(self) -> Scope:
        """Leave the current scope and return to parent."""
        if len(self.scope_stack) <= 1:
            raise RuntimeError("Cannot leave the outermost scope")

        left_scope = self.scope_stack.pop()
        self.current_scope = self.scope_stack[-1]
        return left_scope

    def define(self, symbol: Symbol) -> None:
        """Define a symbol in the current scope."""
        symbol.scope_level = self.current_scope.level
        self.current_scope.define(symbol)

    def lookup(self, name: str) -> Optional[Symbol]:
        """
        Look up a symbol by name, searching outward through scopes.

        Returns the first matching symbol found, or None.
        """
        name_lower = name.lower()

        # Search from current scope outward
        scope: Optional[Scope] = self.current_scope
        while scope is not None:
            # First check direct symbols
            symbol = scope.lookup_local(name_lower)
            if symbol is not None:
                return symbol

            # Then check use clauses
            symbol = scope.lookup_use_clause(name_lower)
            if symbol is not None:
                return symbol

            scope = scope.parent

        return None

    def lookup_type(self, name: str) -> Optional[AdaType]:
        """Look up a type by name."""
        symbol = self.lookup(name)
        if symbol is not None and symbol.kind in (SymbolKind.TYPE, SymbolKind.SUBTYPE):
            return symbol.ada_type
        return None

    def lookup_local(self, name: str) -> Optional[Symbol]:
        """Look up a symbol in the current scope only."""
        return self.current_scope.lookup_local(name)

    def is_defined_locally(self, name: str) -> bool:
        """Check if a name is defined in the current scope."""
        return self.current_scope.lookup_local(name) is not None

    def add_use_clause(self, package_symbol: Symbol) -> None:
        """Add a use clause to the current scope."""
        if package_symbol.kind != SymbolKind.PACKAGE:
            raise ValueError(f"'{package_symbol.name}' is not a package")
        self.current_scope.use_clauses.append(package_symbol)

    def lookup_selected(self, prefix: str, selector: str) -> Optional[Symbol]:
        """
        Look up a selected component (Package.Name or Record.Field).

        Returns the symbol for the selector.
        """
        prefix_symbol = self.lookup(prefix)
        if prefix_symbol is None:
            return None

        selector_lower = selector.lower()

        # Package prefix
        if prefix_symbol.kind == SymbolKind.PACKAGE:
            # Check public symbols
            if selector_lower in prefix_symbol.public_symbols:
                return prefix_symbol.public_symbols[selector_lower]
            return None

        # Could also be record component access, but that's handled
        # by type checking, not symbol lookup

        return None

    def all_overloads(self, name: str) -> list[Symbol]:
        """Get all overloaded symbols with the given name."""
        symbol = self.lookup(name)
        if symbol is None:
            return []

        result = []
        current: Optional[Symbol] = symbol
        while current is not None:
            result.append(current)
            current = current.overloaded_next
        return result

    @property
    def scope_level(self) -> int:
        """Return the current scope nesting level."""
        return self.current_scope.level

    def __repr__(self) -> str:
        lines = [f"SymbolTable (level={self.scope_level}):"]
        for scope in reversed(self.scope_stack):
            lines.append(f"  Scope '{scope.name}' (level {scope.level}):")
            for name, sym in scope.symbols.items():
                lines.append(f"    {name}: {sym.kind.name}")
        return "\n".join(lines)
