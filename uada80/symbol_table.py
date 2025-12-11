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
from typing import Any, Optional

from uada80.type_system import AdaType, PREDEFINED_TYPES, IntegerType, RecordType
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
    GENERIC_PROCEDURE = auto()  # Generic procedure (template)
    GENERIC_FUNCTION = auto()  # Generic function (template)
    EXCEPTION = auto()  # Exception
    LABEL = auto()  # Statement label
    LOOP = auto()  # Loop identifier for exit statements
    COMPONENT = auto()  # Record component
    TASK_TYPE = auto()  # Task type
    TASK = auto()  # Task object (single task)
    ENTRY = auto()  # Task or protected entry
    PROTECTED_TYPE = auto()  # Protected type
    PROTECTED = auto()  # Protected object (single protected)


@dataclass
class Symbol:
    """A symbol in the symbol table."""

    name: str
    kind: SymbolKind
    ada_type: Optional[AdaType] = None  # Type of this symbol
    value: Optional[Any] = None  # Compile-time value for constants/named numbers
    is_constant: bool = False  # For variables
    is_aliased: bool = False  # For aliased objects
    alias_for: Optional[str] = None  # For renaming: name of the original entity
    mode: Optional[str] = None  # For parameters: "in", "out", "in out"
    definition: Optional[ASTNode] = None  # AST node where defined
    scope_level: int = 0  # Nesting level where defined
    default_value: Optional[ASTNode] = None  # Default value expression for parameters

    # For subprograms: list of parameter symbols
    parameters: list["Symbol"] = field(default_factory=list)
    return_type: Optional[AdaType] = None  # For functions

    # For packages
    public_symbols: dict[str, "Symbol"] = field(default_factory=dict)
    private_symbols: dict[str, "Symbol"] = field(default_factory=dict)

    # For overloading: chain of overloaded symbols with same name
    overloaded_next: Optional["Symbol"] = None

    # Pragma-related attributes
    is_imported: bool = False  # pragma Import
    is_exported: bool = False  # pragma Export
    external_name: Optional[str] = None  # External name from pragma Import/Export
    calling_convention: str = "ada"  # Calling convention: "ada", "c", "intrinsic", "asm"
    is_inline: bool = False  # pragma Inline
    is_volatile: bool = False  # pragma Volatile
    is_atomic: bool = False  # pragma Atomic (use DI/EI on Z80)
    is_no_return: bool = False  # pragma No_Return
    is_generic_formal: bool = False  # Generic formal parameter
    is_abstract: bool = False  # Abstract subprogram (is abstract)
    is_pure: bool = False  # pragma Pure (for packages)
    is_preelaborate: bool = False  # pragma Preelaborate (for packages)
    requires_body: bool = False  # pragma Elaborate_Body
    is_withed: bool = False  # Package from with clause (not fully loaded)

    # Representation clause attributes
    explicit_address: Optional[int] = None  # for Obj'Address use N; - fixed memory location
    explicit_size: Optional[int] = None  # for Type'Size use N; - explicit size in bits

    # For primitive operations of tagged types (OOP dispatching)
    primitive_of: Optional["RecordType"] = None  # Tagged type this is a primitive of
    vtable_slot: int = -1  # Slot index in vtable (-1 = not a primitive)

    # For generic instantiations
    generic_instance_of: Optional["Symbol"] = None  # The generic we're an instance of
    generic_actuals: list = field(default_factory=list)  # Actual parameters
    is_builtin_generic: bool = False  # Built-in generic (Unchecked_Deallocation, etc.)
    is_deallocation: bool = False  # Instance of Ada.Unchecked_Deallocation
    is_unchecked_conversion: bool = False  # Instance of Ada.Unchecked_Conversion

    # For built-in container operations
    runtime_name: Optional[str] = None  # Runtime function name (e.g., "_vec_append")
    is_container_op: bool = False  # True if this is a container operation
    container_kind: Optional[str] = None  # "vector", "list", "map", "set"


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

        # Add predefined exceptions (Ada RM 11.1)
        for exc_name in ["Constraint_Error", "Program_Error", "Storage_Error",
                         "Tasking_Error", "Assertion_Error"]:
            symbol = Symbol(
                name=exc_name,
                kind=SymbolKind.EXCEPTION,
                scope_level=0,
            )
            self.current_scope.define(symbol)

        # Add predefined packages
        self._init_text_io()
        self._init_finalization()
        self._init_strings()
        self._init_command_line()
        self._init_unchecked_ops()
        self._init_calendar()
        self._init_numerics()
        self._init_containers()

    def _init_text_io(self) -> None:
        """Add Ada.Text_IO package to the standard scope."""
        # Create the Ada package hierarchy
        ada_pkg = Symbol(
            name="Ada",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Create Text_IO subpackage
        text_io_pkg = Symbol(
            name="Text_IO",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Add procedure symbols to Text_IO
        char_type = PREDEFINED_TYPES["Character"]
        str_type = PREDEFINED_TYPES["String"]
        int_type = PREDEFINED_TYPES["Integer"]

        # Put(Item : Character)
        put_char = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Item", SymbolKind.PARAMETER, char_type, mode="in")],
        )

        # Put(Item : String)
        put_str = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Item", SymbolKind.PARAMETER, str_type, mode="in")],
            overloaded_next=put_char,
        )

        # Put_Line(Item : String)
        put_line = Symbol(
            name="Put_Line",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Item", SymbolKind.PARAMETER, str_type, mode="in")],
        )

        # New_Line
        new_line = Symbol(
            name="New_Line",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[],
        )

        # Get(Item : out Character)
        get_char = Symbol(
            name="Get",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Item", SymbolKind.PARAMETER, char_type, mode="out")],
        )

        # Get_Line(Item : out String; Last : out Natural)
        # Simplified: just reads a line
        get_line = Symbol(
            name="Get_Line",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, str_type, mode="out"),
                Symbol("Last", SymbolKind.PARAMETER, int_type, mode="out"),
            ],
        )

        # Put(Item : Integer) - for Integer'Image shorthand
        put_int = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Item", SymbolKind.PARAMETER, int_type, mode="in")],
            overloaded_next=put_str,
        )

        # Add all to Text_IO public symbols
        text_io_pkg.public_symbols = {
            "put": put_int,
            "put_line": put_line,
            "new_line": new_line,
            "get": get_char,
            "get_line": get_line,
        }

        # Add Text_IO to Ada package
        ada_pkg.public_symbols["text_io"] = text_io_pkg

        # Define Ada package at standard scope (will be updated by _init_finalization)
        self.current_scope.define(ada_pkg)

    def _init_finalization(self) -> None:
        """Add Ada.Finalization package to the standard scope."""
        from uada80.type_system import RecordType

        # Get the Ada package that was already defined
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        # Create Finalization subpackage
        finalization_pkg = Symbol(
            name="Finalization",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Create Controlled type - abstract tagged limited type
        controlled_type = RecordType(
            name="Controlled",
            is_tagged=True,
            is_controlled=True,
        )
        controlled_sym = Symbol(
            name="Controlled",
            kind=SymbolKind.TYPE,
            ada_type=controlled_type,
            scope_level=0,
        )

        # Create Limited_Controlled type - abstract tagged limited type
        limited_controlled_type = RecordType(
            name="Limited_Controlled",
            is_tagged=True,
            is_limited_controlled=True,
        )
        limited_controlled_sym = Symbol(
            name="Limited_Controlled",
            kind=SymbolKind.TYPE,
            ada_type=limited_controlled_type,
            scope_level=0,
        )

        # Create Initialize procedure (for Controlled type)
        initialize_sym = Symbol(
            name="Initialize",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            is_abstract=True,
        )

        # Create Adjust procedure (for Controlled type - not Limited_Controlled)
        adjust_sym = Symbol(
            name="Adjust",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            is_abstract=True,
        )

        # Create Finalize procedure (for both types)
        finalize_sym = Symbol(
            name="Finalize",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            is_abstract=True,
        )

        # Add types and procedures to Finalization package
        finalization_pkg.public_symbols = {
            "controlled": controlled_sym,
            "limited_controlled": limited_controlled_sym,
            "initialize": initialize_sym,
            "adjust": adjust_sym,
            "finalize": finalize_sym,
        }

        # Add Finalization to Ada package
        ada_pkg.public_symbols["finalization"] = finalization_pkg

    def _init_strings(self) -> None:
        """Add Ada.Strings and subpackages to the standard scope."""
        # Get the Ada package that was already defined
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        str_type = PREDEFINED_TYPES["String"]
        char_type = PREDEFINED_TYPES["Character"]
        int_type = PREDEFINED_TYPES["Integer"]
        nat_type = PREDEFINED_TYPES["Natural"]
        bool_type = PREDEFINED_TYPES["Boolean"]

        # =====================================================================
        # Ada.Strings - Base package with constants and types
        # =====================================================================
        strings_pkg = Symbol(
            name="Strings",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Direction type: Forward, Backward
        from uada80.type_system import EnumerationType
        direction_type = EnumerationType(
            name="Direction",
            literals=["Forward", "Backward"],
        )
        direction_sym = Symbol(
            name="Direction",
            kind=SymbolKind.TYPE,
            ada_type=direction_type,
            scope_level=0,
        )

        # Truncation type: Left, Right, Error
        truncation_type = EnumerationType(
            name="Truncation",
            literals=["Left", "Right", "Error"],
        )
        truncation_sym = Symbol(
            name="Truncation",
            kind=SymbolKind.TYPE,
            ada_type=truncation_type,
            scope_level=0,
        )

        # Membership type: Inside, Outside
        membership_type = EnumerationType(
            name="Membership",
            literals=["Inside", "Outside"],
        )
        membership_sym = Symbol(
            name="Membership",
            kind=SymbolKind.TYPE,
            ada_type=membership_type,
            scope_level=0,
        )

        # Alignment type: Left, Right, Center
        alignment_type = EnumerationType(
            name="Alignment",
            literals=["Left", "Right", "Center"],
        )
        alignment_sym = Symbol(
            name="Alignment",
            kind=SymbolKind.TYPE,
            ada_type=alignment_type,
            scope_level=0,
        )

        # Space constant
        space_sym = Symbol(
            name="Space",
            kind=SymbolKind.VARIABLE,
            ada_type=char_type,
            is_constant=True,
            scope_level=0,
        )

        strings_pkg.public_symbols = {
            "direction": direction_sym,
            "truncation": truncation_sym,
            "membership": membership_sym,
            "alignment": alignment_sym,
            "space": space_sym,
            "forward": Symbol("Forward", SymbolKind.VARIABLE, direction_type, is_constant=True),
            "backward": Symbol("Backward", SymbolKind.VARIABLE, direction_type, is_constant=True),
            "left": Symbol("Left", SymbolKind.VARIABLE, truncation_type, is_constant=True),
            "right": Symbol("Right", SymbolKind.VARIABLE, truncation_type, is_constant=True),
        }

        # =====================================================================
        # Ada.Strings.Fixed - Fixed-length string operations
        # =====================================================================
        fixed_pkg = Symbol(
            name="Fixed",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Move procedure: Move(Source, Target, ...)
        move_proc = Symbol(
            name="Move",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Target", SymbolKind.PARAMETER, str_type, mode="out"),
            ],
        )
        move_proc.runtime_name = "_str_move"

        # Index function: Index(Source, Pattern, Going) return Natural
        index_func = Symbol(
            name="Index",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Pattern", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        index_func.runtime_name = "_str_index"

        # Index function with character: Index(Source, Set, Test, Going)
        index_char_func = Symbol(
            name="Index",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Set", SymbolKind.PARAMETER, char_type, mode="in"),
            ],
            overloaded_next=index_func,
        )
        index_char_func.runtime_name = "_str_index_char"

        # Count function
        count_func = Symbol(
            name="Count",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Pattern", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        count_func.runtime_name = "_str_count"

        # Head function
        head_func = Symbol(
            name="Head",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Count", SymbolKind.PARAMETER, nat_type, mode="in"),
            ],
        )
        head_func.runtime_name = "_str_head"

        # Tail function
        tail_func = Symbol(
            name="Tail",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Count", SymbolKind.PARAMETER, nat_type, mode="in"),
            ],
        )
        tail_func.runtime_name = "_str_tail"

        # Trim function
        trim_func = Symbol(
            name="Trim",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        trim_func.runtime_name = "_str_trim"

        # Overwrite procedure
        overwrite_proc = Symbol(
            name="Overwrite",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in out"),
                Symbol("Position", SymbolKind.PARAMETER, int_type, mode="in"),
                Symbol("New_Item", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        overwrite_proc.runtime_name = "_str_overwrite"

        # Delete procedure
        delete_proc = Symbol(
            name="Delete",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in out"),
                Symbol("From", SymbolKind.PARAMETER, int_type, mode="in"),
                Symbol("Through", SymbolKind.PARAMETER, int_type, mode="in"),
            ],
        )
        delete_proc.runtime_name = "_str_delete"

        # Insert function
        insert_func = Symbol(
            name="Insert",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Before", SymbolKind.PARAMETER, int_type, mode="in"),
                Symbol("New_Item", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        insert_func.runtime_name = "_str_insert"

        # Replace_Slice function
        replace_slice_func = Symbol(
            name="Replace_Slice",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Low", SymbolKind.PARAMETER, int_type, mode="in"),
                Symbol("High", SymbolKind.PARAMETER, int_type, mode="in"),
                Symbol("By", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        replace_slice_func.runtime_name = "_str_replace_slice"

        # Translate function (with mapping)
        translate_func = Symbol(
            name="Translate",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        translate_func.runtime_name = "_str_translate"

        fixed_pkg.public_symbols = {
            "move": move_proc,
            "index": index_char_func,
            "count": count_func,
            "head": head_func,
            "tail": tail_func,
            "trim": trim_func,
            "overwrite": overwrite_proc,
            "delete": delete_proc,
            "insert": insert_func,
            "replace_slice": replace_slice_func,
            "translate": translate_func,
        }

        # Add Fixed to Strings package
        strings_pkg.public_symbols["fixed"] = fixed_pkg

        # =====================================================================
        # Ada.Strings.Maps - Character mappings (basic support)
        # =====================================================================
        maps_pkg = Symbol(
            name="Maps",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Character_Set type (simplified as range of characters)
        char_set_type = PREDEFINED_TYPES["String"]  # Simplified
        char_set_sym = Symbol(
            name="Character_Set",
            kind=SymbolKind.TYPE,
            ada_type=char_set_type,
            scope_level=0,
        )

        # Character_Mapping type
        char_mapping_sym = Symbol(
            name="Character_Mapping",
            kind=SymbolKind.TYPE,
            ada_type=char_set_type,  # Simplified
            scope_level=0,
        )

        # Is_In function
        is_in_func = Symbol(
            name="Is_In",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Element", SymbolKind.PARAMETER, char_type, mode="in"),
                Symbol("Set", SymbolKind.PARAMETER, char_set_type, mode="in"),
            ],
        )

        # To_Set function (String -> Character_Set)
        to_set_func = Symbol(
            name="To_Set",
            kind=SymbolKind.FUNCTION,
            return_type=char_set_type,
            scope_level=0,
            parameters=[
                Symbol("Sequence", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )

        maps_pkg.public_symbols = {
            "character_set": char_set_sym,
            "character_mapping": char_mapping_sym,
            "is_in": is_in_func,
            "to_set": to_set_func,
        }

        strings_pkg.public_symbols["maps"] = maps_pkg

        # =====================================================================
        # Ada.Strings.Bounded - Bounded-length strings
        # =====================================================================
        bounded_pkg = Symbol(
            name="Bounded",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Generic_Bounded_Length is a generic package that creates bounded strings
        # For our simplified implementation, we provide a fixed max length type
        # Bounded_String type (record with max length and current content)
        bounded_str_type = RecordType(
            name="Bounded_String",
            size_bits=258 * 8,  # 2 bytes for length + 256 max chars
        )
        bounded_str_sym = Symbol(
            name="Bounded_String",
            kind=SymbolKind.TYPE,
            ada_type=bounded_str_type,
            scope_level=0,
        )

        # Max_Length constant (default 256 for our implementation)
        max_length_sym = Symbol(
            name="Max_Length",
            kind=SymbolKind.VARIABLE,
            ada_type=nat_type,
            is_constant=True,
            scope_level=0,
        )
        max_length_sym.const_value = 256

        # Null_Bounded_String constant
        null_bounded_sym = Symbol(
            name="Null_Bounded_String",
            kind=SymbolKind.VARIABLE,
            ada_type=bounded_str_type,
            is_constant=True,
            scope_level=0,
        )

        # Length function
        bnd_length_func = Symbol(
            name="Length",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
            ],
        )
        bnd_length_func.runtime_name = "_bnd_length"

        # To_Bounded_String function (String -> Bounded_String)
        to_bounded_func = Symbol(
            name="To_Bounded_String",
            kind=SymbolKind.FUNCTION,
            return_type=bounded_str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        to_bounded_func.runtime_name = "_bnd_from_str"

        # To_String function (Bounded_String -> String)
        to_string_func = Symbol(
            name="To_String",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
            ],
        )
        to_string_func.runtime_name = "_bnd_to_str"

        # Append procedure (Bounded_String += String)
        bnd_append_proc = Symbol(
            name="Append",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in out"),
                Symbol("New_Item", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        bnd_append_proc.runtime_name = "_bnd_append"

        # Element function (get character at position)
        bnd_element_func = Symbol(
            name="Element",
            kind=SymbolKind.FUNCTION,
            return_type=char_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
                Symbol("Index", SymbolKind.PARAMETER, int_type, mode="in"),
            ],
        )
        bnd_element_func.runtime_name = "_bnd_element"

        # Replace_Element procedure
        bnd_replace_elem_proc = Symbol(
            name="Replace_Element",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in out"),
                Symbol("Index", SymbolKind.PARAMETER, int_type, mode="in"),
                Symbol("By", SymbolKind.PARAMETER, char_type, mode="in"),
            ],
        )
        bnd_replace_elem_proc.runtime_name = "_bnd_replace_element"

        # Slice function
        bnd_slice_func = Symbol(
            name="Slice",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
                Symbol("Low", SymbolKind.PARAMETER, int_type, mode="in"),
                Symbol("High", SymbolKind.PARAMETER, int_type, mode="in"),
            ],
        )
        bnd_slice_func.runtime_name = "_bnd_slice"

        # Index function (find pattern in bounded string)
        bnd_index_func = Symbol(
            name="Index",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
                Symbol("Pattern", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        bnd_index_func.runtime_name = "_bnd_index"

        # Head function
        bnd_head_func = Symbol(
            name="Head",
            kind=SymbolKind.FUNCTION,
            return_type=bounded_str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
                Symbol("Count", SymbolKind.PARAMETER, nat_type, mode="in"),
            ],
        )
        bnd_head_func.runtime_name = "_bnd_head"

        # Tail function
        bnd_tail_func = Symbol(
            name="Tail",
            kind=SymbolKind.FUNCTION,
            return_type=bounded_str_type,
            scope_level=0,
            parameters=[
                Symbol("Source", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
                Symbol("Count", SymbolKind.PARAMETER, nat_type, mode="in"),
            ],
        )
        bnd_tail_func.runtime_name = "_bnd_tail"

        # "&" operator (concatenation) - handled via Append
        bnd_concat_func = Symbol(
            name="&",
            kind=SymbolKind.FUNCTION,
            return_type=bounded_str_type,
            scope_level=0,
            parameters=[
                Symbol("Left", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
                Symbol("Right", SymbolKind.PARAMETER, bounded_str_type, mode="in"),
            ],
        )
        bnd_concat_func.runtime_name = "_bnd_concat"

        bounded_pkg.public_symbols = {
            "bounded_string": bounded_str_sym,
            "max_length": max_length_sym,
            "null_bounded_string": null_bounded_sym,
            "length": bnd_length_func,
            "to_bounded_string": to_bounded_func,
            "to_string": to_string_func,
            "append": bnd_append_proc,
            "element": bnd_element_func,
            "replace_element": bnd_replace_elem_proc,
            "slice": bnd_slice_func,
            "index": bnd_index_func,
            "head": bnd_head_func,
            "tail": bnd_tail_func,
            "&": bnd_concat_func,
        }

        strings_pkg.public_symbols["bounded"] = bounded_pkg

        # Add Strings to Ada package
        ada_pkg.public_symbols["strings"] = strings_pkg

    def _init_command_line(self) -> None:
        """Add Ada.Command_Line package to the standard scope."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        str_type = PREDEFINED_TYPES["String"]
        nat_type = PREDEFINED_TYPES["Natural"]
        int_type = PREDEFINED_TYPES["Integer"]

        # Create Command_Line package
        cmd_line_pkg = Symbol(
            name="Command_Line",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Argument_Count : function return Natural
        arg_count_func = Symbol(
            name="Argument_Count",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[],
        )

        # Argument : function (Number : Positive) return String
        argument_func = Symbol(
            name="Argument",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Number", SymbolKind.PARAMETER, int_type, mode="in"),
            ],
        )

        # Command_Name : function return String
        cmd_name_func = Symbol(
            name="Command_Name",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )

        # Set_Exit_Status : procedure (Code : Exit_Status)
        set_exit_proc = Symbol(
            name="Set_Exit_Status",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Code", SymbolKind.PARAMETER, int_type, mode="in"),
            ],
        )

        # Exit_Status type (Integer subtype)
        exit_status_sym = Symbol(
            name="Exit_Status",
            kind=SymbolKind.TYPE,
            ada_type=int_type,
            scope_level=0,
        )

        # Success and Failure constants
        success_sym = Symbol(
            name="Success",
            kind=SymbolKind.VARIABLE,
            ada_type=int_type,
            is_constant=True,
            scope_level=0,
        )
        failure_sym = Symbol(
            name="Failure",
            kind=SymbolKind.VARIABLE,
            ada_type=int_type,
            is_constant=True,
            scope_level=0,
        )

        cmd_line_pkg.public_symbols = {
            "argument_count": arg_count_func,
            "argument": argument_func,
            "command_name": cmd_name_func,
            "set_exit_status": set_exit_proc,
            "exit_status": exit_status_sym,
            "success": success_sym,
            "failure": failure_sym,
        }

        ada_pkg.public_symbols["command_line"] = cmd_line_pkg

    def _init_unchecked_ops(self) -> None:
        """Add Ada.Unchecked_Conversion and Ada.Unchecked_Deallocation."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        # Unchecked_Conversion is a generic function:
        # generic
        #    type Source(<>) is limited private;
        #    type Target(<>) is limited private;
        # function Ada.Unchecked_Conversion(S : Source) return Target;
        #
        # For simplicity, we represent it as a generic function symbol
        unchecked_conv = Symbol(
            name="Unchecked_Conversion",
            kind=SymbolKind.GENERIC_FUNCTION,
            scope_level=0,
        )
        # Mark as a built-in generic
        unchecked_conv.is_builtin_generic = True

        # Unchecked_Deallocation is a generic procedure:
        # generic
        #    type Object(<>) is limited private;
        #    type Name is access Object;
        # procedure Ada.Unchecked_Deallocation(X : in out Name);
        unchecked_dealloc = Symbol(
            name="Unchecked_Deallocation",
            kind=SymbolKind.GENERIC_PROCEDURE,
            scope_level=0,
        )
        unchecked_dealloc.is_builtin_generic = True

        ada_pkg.public_symbols["unchecked_conversion"] = unchecked_conv
        ada_pkg.public_symbols["unchecked_deallocation"] = unchecked_dealloc

    def _init_calendar(self) -> None:
        """Add Ada.Calendar package for time handling."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        # Create Calendar subpackage
        calendar_pkg = Symbol(
            name="Calendar",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Add Time type (private type representing calendar time)
        time_type = IntegerType(name="Time", low=0, high=2**63-1)

        # Add Day_Duration subtype of Duration
        day_duration_type = IntegerType(name="Day_Duration", low=0, high=86_400_000_000_000)

        # Add Year_Number, Month_Number, Day_Number subtypes
        year_type = IntegerType(name="Year_Number", low=1901, high=2399)
        month_type = IntegerType(name="Month_Number", low=1, high=12)
        day_type = IntegerType(name="Day_Number", low=1, high=31)

        calendar_pkg.public_symbols["time"] = Symbol(
            name="Time",
            kind=SymbolKind.TYPE,
            ada_type=time_type,
            scope_level=0,
        )
        calendar_pkg.public_symbols["day_duration"] = Symbol(
            name="Day_Duration",
            kind=SymbolKind.TYPE,
            ada_type=day_duration_type,
            scope_level=0,
        )
        calendar_pkg.public_symbols["year_number"] = Symbol(
            name="Year_Number",
            kind=SymbolKind.TYPE,
            ada_type=year_type,
            scope_level=0,
        )
        calendar_pkg.public_symbols["month_number"] = Symbol(
            name="Month_Number",
            kind=SymbolKind.TYPE,
            ada_type=month_type,
            scope_level=0,
        )
        calendar_pkg.public_symbols["day_number"] = Symbol(
            name="Day_Number",
            kind=SymbolKind.TYPE,
            ada_type=day_type,
            scope_level=0,
        )

        # Add Clock function: returns current time
        clock_func = Symbol(
            name="Clock",
            kind=SymbolKind.FUNCTION,
            return_type=time_type,
            scope_level=0,
        )
        calendar_pkg.public_symbols["clock"] = clock_func

        # Add Year, Month, Day, Seconds functions
        for func_name, ret_type in [
            ("Year", year_type),
            ("Month", month_type),
            ("Day", day_type),
        ]:
            func_sym = Symbol(
                name=func_name,
                kind=SymbolKind.FUNCTION,
                return_type=ret_type,
                scope_level=0,
            )
            func_sym.parameters = [
                Symbol(name="Date", kind=SymbolKind.PARAMETER, ada_type=time_type, mode="in")
            ]
            calendar_pkg.public_symbols[func_name.lower()] = func_sym

        # Add Seconds function returning Day_Duration
        seconds_func = Symbol(
            name="Seconds",
            kind=SymbolKind.FUNCTION,
            return_type=day_duration_type,
            scope_level=0,
        )
        seconds_func.parameters = [
            Symbol(name="Date", kind=SymbolKind.PARAMETER, ada_type=time_type, mode="in")
        ]
        calendar_pkg.public_symbols["seconds"] = seconds_func

        # Add Time_Of function: create Time from components
        time_of_func = Symbol(
            name="Time_Of",
            kind=SymbolKind.FUNCTION,
            return_type=time_type,
            scope_level=0,
        )
        calendar_pkg.public_symbols["time_of"] = time_of_func

        # Add Split procedure: split Time into components
        split_proc = Symbol(
            name="Split",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
        )
        calendar_pkg.public_symbols["split"] = split_proc

        # Add "+" and "-" operators for Time arithmetic
        for op_name in ["+", "-"]:
            op_sym = Symbol(
                name=op_name,
                kind=SymbolKind.FUNCTION,
                return_type=time_type,
                scope_level=0,
            )
            calendar_pkg.public_symbols[op_name] = op_sym

        # Add comparison operators
        bool_type = PREDEFINED_TYPES.get("Boolean")
        for op_name in ["<", "<=", ">", ">=", "="]:
            op_sym = Symbol(
                name=op_name,
                kind=SymbolKind.FUNCTION,
                return_type=bool_type,
                scope_level=0,
            )
            calendar_pkg.public_symbols[op_name] = op_sym

        # Add Time_Error exception
        time_error = Symbol(
            name="Time_Error",
            kind=SymbolKind.EXCEPTION,
            scope_level=0,
        )
        calendar_pkg.public_symbols["time_error"] = time_error

        ada_pkg.public_symbols["calendar"] = calendar_pkg

    def _init_numerics(self) -> None:
        """Add Ada.Numerics packages for math functions."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        # Create Numerics parent package
        numerics_pkg = Symbol(
            name="Numerics",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Add mathematical constants
        float_type = PREDEFINED_TYPES.get("Float")
        if float_type:
            # Pi constant
            pi_sym = Symbol(
                name="Pi",
                kind=SymbolKind.VARIABLE,
                ada_type=float_type,
                is_constant=True,
                scope_level=0,
            )
            numerics_pkg.public_symbols["pi"] = pi_sym

            # e constant
            e_sym = Symbol(
                name="e",
                kind=SymbolKind.VARIABLE,
                ada_type=float_type,
                is_constant=True,
                scope_level=0,
            )
            numerics_pkg.public_symbols["e"] = e_sym

        # Create Elementary_Functions subpackage (trigonometric, etc.)
        elem_funcs_pkg = Symbol(
            name="Elementary_Functions",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        if float_type:
            # Add elementary functions
            for func_name in [
                "Sqrt", "Log", "Log10", "Exp",
                "Sin", "Cos", "Tan",
                "Arcsin", "Arccos", "Arctan",
                "Sinh", "Cosh", "Tanh",
                "Arcsinh", "Arccosh", "Arctanh",
            ]:
                func_sym = Symbol(
                    name=func_name,
                    kind=SymbolKind.FUNCTION,
                    return_type=float_type,
                    scope_level=0,
                )
                func_sym.parameters = [
                    Symbol(name="X", kind=SymbolKind.PARAMETER, ada_type=float_type, mode="in")
                ]
                elem_funcs_pkg.public_symbols[func_name.lower()] = func_sym

            # Add power function (two arguments)
            power_func = Symbol(
                name="**",
                kind=SymbolKind.FUNCTION,
                return_type=float_type,
                scope_level=0,
            )
            elem_funcs_pkg.public_symbols["**"] = power_func

            # Add Arctan with two arguments (Y, X)
            arctan2_func = Symbol(
                name="Arctan",
                kind=SymbolKind.FUNCTION,
                return_type=float_type,
                scope_level=0,
            )
            arctan2_func.parameters = [
                Symbol(name="Y", kind=SymbolKind.PARAMETER, ada_type=float_type, mode="in"),
                Symbol(name="X", kind=SymbolKind.PARAMETER, ada_type=float_type, mode="in"),
            ]
            # Note: This overloads the single-argument Arctan

        numerics_pkg.public_symbols["elementary_functions"] = elem_funcs_pkg

        # Create Random subpackage
        random_pkg = Symbol(
            name="Discrete_Random",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        random_pkg.is_builtin_generic = True
        numerics_pkg.public_symbols["discrete_random"] = random_pkg

        float_random_pkg = Symbol(
            name="Float_Random",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )
        if float_type:
            # Generator type (opaque)
            gen_type = RecordType(name="Generator", components=[])
            float_random_pkg.public_symbols["generator"] = Symbol(
                name="Generator",
                kind=SymbolKind.TYPE,
                ada_type=gen_type,
                scope_level=0,
            )

            # Random function
            random_func = Symbol(
                name="Random",
                kind=SymbolKind.FUNCTION,
                return_type=float_type,
                scope_level=0,
            )
            float_random_pkg.public_symbols["random"] = random_func

            # Reset procedure
            reset_proc = Symbol(
                name="Reset",
                kind=SymbolKind.PROCEDURE,
                scope_level=0,
            )
            float_random_pkg.public_symbols["reset"] = reset_proc

        numerics_pkg.public_symbols["float_random"] = float_random_pkg

        ada_pkg.public_symbols["numerics"] = numerics_pkg

    def _init_containers(self) -> None:
        """Add Ada.Containers packages (Ada 2005+)."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        # Create Containers parent package
        containers_pkg = Symbol(
            name="Containers",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Add Count_Type (used throughout containers)
        count_type = IntegerType(name="Count_Type", low=0, high=2**31-1)
        containers_pkg.public_symbols["count_type"] = Symbol(
            name="Count_Type",
            kind=SymbolKind.TYPE,
            ada_type=count_type,
            scope_level=0,
        )

        # Add Hash_Type for hashed containers
        hash_type = IntegerType(name="Hash_Type", low=0, high=2**32-1)
        containers_pkg.public_symbols["hash_type"] = Symbol(
            name="Hash_Type",
            kind=SymbolKind.TYPE,
            ada_type=hash_type,
            scope_level=0,
        )

        # Add generic Vectors package
        vectors_pkg = Symbol(
            name="Vectors",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        vectors_pkg.is_builtin_generic = True
        vectors_pkg.container_kind = "vector"  # Mark for code generation

        # Generic formal: type Element_Type is private;
        # These will be substituted during instantiation

        # Vector type (opaque - pointer to runtime structure)
        from uada80.type_system import AccessType, RecordType
        vector_record = RecordType(name="Vector", components=[], is_tagged=False)
        vector_type = AccessType(name="Vector", designated_type=vector_record)

        # Cursor type (index into vector, 0xFFFF = No_Element)
        cursor_type = IntegerType(name="Cursor", low=0, high=0xFFFF)

        # No_Element constant
        no_element_sym = Symbol(
            name="No_Element",
            kind=SymbolKind.VARIABLE,
            ada_type=cursor_type,
            is_constant=True,
            value=0xFFFF,
            scope_level=0,
        )

        # Empty_Vector constant
        empty_vector_sym = Symbol(
            name="Empty_Vector",
            kind=SymbolKind.VARIABLE,
            ada_type=vector_type,
            is_constant=True,
            value=0,
            scope_level=0,
        )

        # Define operations
        nat_type = IntegerType(name="Natural", low=0, high=32767)
        bool_type = PREDEFINED_TYPES["Boolean"]

        # Length function
        length_func = Symbol(
            name="Length",
            kind=SymbolKind.FUNCTION,
            return_type=count_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
            ],
        )
        length_func.runtime_name = "_vec_length"

        # Is_Empty function
        is_empty_func = Symbol(
            name="Is_Empty",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
            ],
        )
        is_empty_func.runtime_name = "_vec_is_empty"

        # Clear procedure
        clear_proc = Symbol(
            name="Clear",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in out"),
            ],
        )
        clear_proc.runtime_name = "_vec_clear"

        # Append procedure (element)
        append_proc = Symbol(
            name="Append",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in out"),
                Symbol("New_Item", SymbolKind.PARAMETER, None, mode="in"),  # Element_Type
            ],
        )
        append_proc.runtime_name = "_vec_append"
        append_proc.is_container_op = True

        # Prepend procedure
        prepend_proc = Symbol(
            name="Prepend",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in out"),
                Symbol("New_Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        prepend_proc.runtime_name = "_vec_prepend"
        prepend_proc.is_container_op = True

        # First_Element function
        first_elem_func = Symbol(
            name="First_Element",
            kind=SymbolKind.FUNCTION,
            return_type=None,  # Element_Type
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
            ],
        )
        first_elem_func.runtime_name = "_vec_first_element"

        # Last_Element function
        last_elem_func = Symbol(
            name="Last_Element",
            kind=SymbolKind.FUNCTION,
            return_type=None,  # Element_Type
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
            ],
        )
        last_elem_func.runtime_name = "_vec_last_element"

        # Element function (by cursor)
        element_func = Symbol(
            name="Element",
            kind=SymbolKind.FUNCTION,
            return_type=None,  # Element_Type
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
                Symbol("Position", SymbolKind.PARAMETER, cursor_type, mode="in"),
            ],
        )
        element_func.runtime_name = "_vec_element"

        # Replace_Element procedure
        replace_elem_proc = Symbol(
            name="Replace_Element",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in out"),
                Symbol("Position", SymbolKind.PARAMETER, cursor_type, mode="in"),
                Symbol("New_Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        replace_elem_proc.runtime_name = "_vec_replace"
        replace_elem_proc.is_container_op = True

        # First function (returns cursor)
        first_func = Symbol(
            name="First",
            kind=SymbolKind.FUNCTION,
            return_type=cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
            ],
        )
        first_func.runtime_name = "_vec_first"

        # Last function (returns cursor)
        last_func = Symbol(
            name="Last",
            kind=SymbolKind.FUNCTION,
            return_type=cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
            ],
        )
        last_func.runtime_name = "_vec_last"

        # Next function
        next_func = Symbol(
            name="Next",
            kind=SymbolKind.FUNCTION,
            return_type=cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Position", SymbolKind.PARAMETER, cursor_type, mode="in"),
            ],
        )
        next_func.runtime_name = "_cursor_next"

        # Previous function
        prev_func = Symbol(
            name="Previous",
            kind=SymbolKind.FUNCTION,
            return_type=cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Position", SymbolKind.PARAMETER, cursor_type, mode="in"),
            ],
        )
        prev_func.runtime_name = "_cursor_previous"

        # Has_Element function
        has_elem_func = Symbol(
            name="Has_Element",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Position", SymbolKind.PARAMETER, cursor_type, mode="in"),
            ],
        )
        has_elem_func.runtime_name = "_cursor_has_element"

        # Delete procedure (by cursor)
        delete_proc = Symbol(
            name="Delete",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in out"),
                Symbol("Position", SymbolKind.PARAMETER, cursor_type, mode="in out"),
            ],
        )
        delete_proc.runtime_name = "_vec_delete"

        # Delete_First procedure
        delete_first_proc = Symbol(
            name="Delete_First",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in out"),
            ],
        )
        delete_first_proc.runtime_name = "_vec_delete_first"

        # Delete_Last procedure
        delete_last_proc = Symbol(
            name="Delete_Last",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in out"),
            ],
        )
        delete_last_proc.runtime_name = "_vec_delete_last"

        # Find function
        find_func = Symbol(
            name="Find",
            kind=SymbolKind.FUNCTION,
            return_type=cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        find_func.runtime_name = "_container_find"

        # Contains function
        contains_func = Symbol(
            name="Contains",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, vector_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        contains_func.runtime_name = "_container_contains"

        # Add types and operations to package
        vectors_pkg.public_symbols = {
            "vector": Symbol("Vector", SymbolKind.TYPE, vector_type, scope_level=0),
            "cursor": Symbol("Cursor", SymbolKind.TYPE, cursor_type, scope_level=0),
            "no_element": no_element_sym,
            "empty_vector": empty_vector_sym,
            "length": length_func,
            "is_empty": is_empty_func,
            "clear": clear_proc,
            "append": append_proc,
            "prepend": prepend_proc,
            "first_element": first_elem_func,
            "last_element": last_elem_func,
            "element": element_func,
            "replace_element": replace_elem_proc,
            "first": first_func,
            "last": last_func,
            "next": next_func,
            "previous": prev_func,
            "has_element": has_elem_func,
            "delete": delete_proc,
            "delete_first": delete_first_proc,
            "delete_last": delete_last_proc,
            "find": find_func,
            "contains": contains_func,
        }

        containers_pkg.public_symbols["vectors"] = vectors_pkg

        # Add generic Doubly_Linked_Lists package
        lists_pkg = Symbol(
            name="Doubly_Linked_Lists",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        lists_pkg.is_builtin_generic = True
        lists_pkg.container_kind = "list"

        # List type (opaque - pointer to runtime structure)
        list_record = RecordType(name="List", components=[], is_tagged=False)
        list_type = AccessType(name="List", designated_type=list_record)

        # Cursor type (pointer to list node, 0xFFFF = No_Element)
        list_cursor_type = IntegerType(name="Cursor", low=0, high=0xFFFF)

        # No_Element constant for lists
        list_no_element_sym = Symbol(
            name="No_Element",
            kind=SymbolKind.VARIABLE,
            ada_type=list_cursor_type,
            is_constant=True,
            value=0xFFFF,
            scope_level=0,
        )

        # Empty_List constant
        empty_list_sym = Symbol(
            name="Empty_List",
            kind=SymbolKind.VARIABLE,
            ada_type=list_type,
            is_constant=True,
            value=0,
            scope_level=0,
        )

        # List operations
        # Length function
        list_length_func = Symbol(
            name="Length",
            kind=SymbolKind.FUNCTION,
            return_type=count_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in"),
            ],
        )
        list_length_func.runtime_name = "_list_length"

        # Is_Empty function
        list_is_empty_func = Symbol(
            name="Is_Empty",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in"),
            ],
        )
        list_is_empty_func.runtime_name = "_list_is_empty"

        # Clear procedure
        list_clear_proc = Symbol(
            name="Clear",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in out"),
            ],
        )
        list_clear_proc.runtime_name = "_list_clear"

        # Append procedure
        list_append_proc = Symbol(
            name="Append",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in out"),
                Symbol("New_Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        list_append_proc.runtime_name = "_list_append"
        list_append_proc.is_container_op = True

        # Prepend procedure
        list_prepend_proc = Symbol(
            name="Prepend",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in out"),
                Symbol("New_Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        list_prepend_proc.runtime_name = "_list_prepend"
        list_prepend_proc.is_container_op = True

        # First function (returns cursor)
        list_first_func = Symbol(
            name="First",
            kind=SymbolKind.FUNCTION,
            return_type=list_cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in"),
            ],
        )
        list_first_func.runtime_name = "_list_first"

        # Last function (returns cursor)
        list_last_func = Symbol(
            name="Last",
            kind=SymbolKind.FUNCTION,
            return_type=list_cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in"),
            ],
        )
        list_last_func.runtime_name = "_list_last"

        # Next function
        list_next_func = Symbol(
            name="Next",
            kind=SymbolKind.FUNCTION,
            return_type=list_cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Position", SymbolKind.PARAMETER, list_cursor_type, mode="in"),
            ],
        )
        list_next_func.runtime_name = "_list_next"

        # Previous function
        list_prev_func = Symbol(
            name="Previous",
            kind=SymbolKind.FUNCTION,
            return_type=list_cursor_type,
            scope_level=0,
            parameters=[
                Symbol("Position", SymbolKind.PARAMETER, list_cursor_type, mode="in"),
            ],
        )
        list_prev_func.runtime_name = "_list_prev"

        # Element function (by cursor)
        list_element_func = Symbol(
            name="Element",
            kind=SymbolKind.FUNCTION,
            return_type=None,  # Element_Type
            scope_level=0,
            parameters=[
                Symbol("Position", SymbolKind.PARAMETER, list_cursor_type, mode="in"),
            ],
        )
        list_element_func.runtime_name = "_list_element"

        # Has_Element function
        list_has_elem_func = Symbol(
            name="Has_Element",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Position", SymbolKind.PARAMETER, list_cursor_type, mode="in"),
            ],
        )
        list_has_elem_func.runtime_name = "_cursor_has_element"

        # Delete procedure (by cursor)
        list_delete_proc = Symbol(
            name="Delete",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in out"),
                Symbol("Position", SymbolKind.PARAMETER, list_cursor_type, mode="in out"),
            ],
        )
        list_delete_proc.runtime_name = "_list_delete"

        # Replace_Element procedure
        list_replace_proc = Symbol(
            name="Replace_Element",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in out"),
                Symbol("Position", SymbolKind.PARAMETER, list_cursor_type, mode="in"),
                Symbol("New_Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        list_replace_proc.runtime_name = "_list_replace"
        list_replace_proc.is_container_op = True

        # Insert_Before procedure
        list_insert_proc = Symbol(
            name="Insert",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in out"),
                Symbol("Before", SymbolKind.PARAMETER, list_cursor_type, mode="in"),
                Symbol("New_Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        list_insert_proc.runtime_name = "_list_insert"
        list_insert_proc.is_container_op = True

        # Reverse_Elements procedure
        list_reverse_proc = Symbol(
            name="Reverse_Elements",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Container", SymbolKind.PARAMETER, list_type, mode="in out"),
            ],
        )
        list_reverse_proc.runtime_name = "_list_reverse"

        # Add types and operations to package
        lists_pkg.public_symbols = {
            "list": Symbol("List", SymbolKind.TYPE, list_type, scope_level=0),
            "cursor": Symbol("Cursor", SymbolKind.TYPE, list_cursor_type, scope_level=0),
            "no_element": list_no_element_sym,
            "empty_list": empty_list_sym,
            "length": list_length_func,
            "is_empty": list_is_empty_func,
            "clear": list_clear_proc,
            "append": list_append_proc,
            "prepend": list_prepend_proc,
            "first": list_first_func,
            "last": list_last_func,
            "next": list_next_func,
            "previous": list_prev_func,
            "element": list_element_func,
            "has_element": list_has_elem_func,
            "delete": list_delete_proc,
            "replace_element": list_replace_proc,
            "insert": list_insert_proc,
            "reverse_elements": list_reverse_proc,
        }

        containers_pkg.public_symbols["doubly_linked_lists"] = lists_pkg

        # Add generic Hashed_Maps package
        hashed_maps_pkg = Symbol(
            name="Hashed_Maps",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        hashed_maps_pkg.is_builtin_generic = True
        containers_pkg.public_symbols["hashed_maps"] = hashed_maps_pkg

        # Add generic Ordered_Maps package
        ordered_maps_pkg = Symbol(
            name="Ordered_Maps",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        ordered_maps_pkg.is_builtin_generic = True
        containers_pkg.public_symbols["ordered_maps"] = ordered_maps_pkg

        # Add generic Hashed_Sets package
        hashed_sets_pkg = Symbol(
            name="Hashed_Sets",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        hashed_sets_pkg.is_builtin_generic = True
        containers_pkg.public_symbols["hashed_sets"] = hashed_sets_pkg

        # Add generic Ordered_Sets package
        ordered_sets_pkg = Symbol(
            name="Ordered_Sets",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        ordered_sets_pkg.is_builtin_generic = True
        containers_pkg.public_symbols["ordered_sets"] = ordered_sets_pkg

        # Add generic Indefinite_Vectors (for unconstrained element types)
        indef_vectors_pkg = Symbol(
            name="Indefinite_Vectors",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        indef_vectors_pkg.is_builtin_generic = True
        containers_pkg.public_symbols["indefinite_vectors"] = indef_vectors_pkg

        # Add generic Indefinite_Doubly_Linked_Lists
        indef_lists_pkg = Symbol(
            name="Indefinite_Doubly_Linked_Lists",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        indef_lists_pkg.is_builtin_generic = True
        containers_pkg.public_symbols["indefinite_doubly_linked_lists"] = indef_lists_pkg

        # Add generic Indefinite_Hashed_Maps
        indef_hashed_maps_pkg = Symbol(
            name="Indefinite_Hashed_Maps",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        indef_hashed_maps_pkg.is_builtin_generic = True
        containers_pkg.public_symbols["indefinite_hashed_maps"] = indef_hashed_maps_pkg

        # Add generic Indefinite_Ordered_Maps
        indef_ordered_maps_pkg = Symbol(
            name="Indefinite_Ordered_Maps",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        indef_ordered_maps_pkg.is_builtin_generic = True
        containers_pkg.public_symbols["indefinite_ordered_maps"] = indef_ordered_maps_pkg

        ada_pkg.public_symbols["containers"] = containers_pkg

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

    def current_scope_symbols(self) -> list[Symbol]:
        """Return all symbols defined in the current scope."""
        return list(self.current_scope.symbols.values())

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
