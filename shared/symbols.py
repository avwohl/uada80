"""
Symbol table for compilers targeting Z80.

Provides a generic scope-based symbol table that can be extended
by language-specific compilers.
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any


class SymbolKind(Enum):
    """Kind of symbol."""

    VARIABLE = auto()
    CONSTANT = auto()
    TYPE = auto()
    PROCEDURE = auto()
    FUNCTION = auto()
    PACKAGE = auto()
    LABEL = auto()
    PARAMETER = auto()
    EXCEPTION = auto()
    BUILTIN = auto()


@dataclass
class Symbol:
    """A symbol in the symbol table."""

    name: str
    kind: SymbolKind
    # Type information (language-specific, stored as Any for flexibility)
    type_info: Any = None
    # For arrays/aggregate types
    dimension: int | None = None
    # For subprograms
    params: list["Symbol"] = field(default_factory=list)
    return_type: Any = None
    # Visibility and linkage
    is_public: bool = False
    is_external: bool = False
    # Memory location (assigned during code gen)
    address: int | None = None
    size: int = 0  # Size in bytes
    # Scope level where defined
    scope_level: int = 0
    # Assembly label name
    asm_name: str | None = None
    # Language-specific attributes
    attributes: dict[str, Any] = field(default_factory=dict)


@dataclass
class Scope:
    """A scope containing symbols."""

    symbols: dict[str, Symbol] = field(default_factory=dict)
    parent: "Scope | None" = None
    level: int = 0
    name: str = ""  # Package, procedure, or block name

    def define(self, symbol: Symbol) -> None:
        """Define a symbol in this scope."""
        symbol.scope_level = self.level
        self.symbols[symbol.name] = symbol

    def lookup_local(self, name: str) -> Symbol | None:
        """Look up a symbol in this scope only."""
        return self.symbols.get(name)

    def lookup(self, name: str) -> Symbol | None:
        """Look up a symbol, searching parent scopes."""
        sym = self.symbols.get(name)
        if sym is not None:
            return sym
        if self.parent is not None:
            return self.parent.lookup(name)
        return None

    def all_symbols(self) -> list[Symbol]:
        """Return all symbols defined in this scope."""
        return list(self.symbols.values())


class SymbolTable:
    """Symbol table with nested scopes."""

    def __init__(self, global_name: str = "<global>") -> None:
        self.global_scope = Scope(level=0, name=global_name)
        self.current_scope = self.global_scope

    def enter_scope(self, name: str = "") -> Scope:
        """Enter a new scope."""
        new_scope = Scope(
            parent=self.current_scope,
            level=self.current_scope.level + 1,
            name=name,
        )
        self.current_scope = new_scope
        return new_scope

    def leave_scope(self) -> Scope:
        """Leave the current scope, returning to parent."""
        old_scope = self.current_scope
        if self.current_scope.parent is not None:
            self.current_scope = self.current_scope.parent
        return old_scope

    def define(self, symbol: Symbol) -> None:
        """Define a symbol in the current scope."""
        self.current_scope.define(symbol)

    def lookup(self, name: str) -> Symbol | None:
        """Look up a symbol by name, searching all visible scopes."""
        return self.current_scope.lookup(name)

    def lookup_local(self, name: str) -> Symbol | None:
        """Look up a symbol in current scope only."""
        return self.current_scope.lookup_local(name)

    def is_defined_local(self, name: str) -> bool:
        """Check if a symbol is defined in the current scope."""
        return self.current_scope.lookup_local(name) is not None

    def is_defined(self, name: str) -> bool:
        """Check if a symbol is defined in any visible scope."""
        return self.lookup(name) is not None

    @property
    def scope_depth(self) -> int:
        """Return current scope depth (0 = global)."""
        return self.current_scope.level
