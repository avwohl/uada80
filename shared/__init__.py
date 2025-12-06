"""
Shared Z80 compiler components.

This module contains language-agnostic code for Z80/8080 compilers,
originally extracted from the uplm80 PL/M-80 compiler.

Components:
- target: Target processor enum (Z80, 8080)
- errors: Error handling and source location tracking
- symbols: Symbol table with nested scopes
- z80_peephole: Pattern-based assembly optimization
- z80_postopt: Post-assembly optimization (tail merging, JP->JR)
"""

from .target import Target
from .errors import (
    SourceLocation,
    CompilerError,
    LexerError,
    ParserError,
    SemanticError,
    CodeGenError,
    LinkerError,
    CompilerWarning,
    ErrorCollector,
)
from .symbols import Symbol, SymbolKind, Scope, SymbolTable
from .z80_peephole import PeepholeOptimizer, PeepholePattern
from .z80_postopt import optimize_asm, optimize, get_instr_size

__all__ = [
    # Target
    "Target",
    # Errors
    "SourceLocation",
    "CompilerError",
    "LexerError",
    "ParserError",
    "SemanticError",
    "CodeGenError",
    "LinkerError",
    "CompilerWarning",
    "ErrorCollector",
    # Symbols
    "Symbol",
    "SymbolKind",
    "Scope",
    "SymbolTable",
    # Peephole optimizer
    "PeepholeOptimizer",
    "PeepholePattern",
    # Post-assembly optimizer
    "optimize_asm",
    "optimize",
    "get_instr_size",
]
