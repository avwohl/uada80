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

from uada80.type_system import AdaType, PREDEFINED_TYPES, IntegerType, RecordType, TypeKind
from uada80.ast_nodes import ASTNode


class SymbolKind(Enum):
    """Classification of symbols."""

    VARIABLE = auto()  # Variable
    CONSTANT = auto()  # Named constant
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

    # For generic packages/subprograms: store the formal parameter symbols
    generic_formal_symbols: dict[str, "Symbol"] = field(default_factory=dict)

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

        # Helper to check if a symbol is an enumeration literal
        def is_enum_literal(sym: Symbol) -> bool:
            return (sym.is_constant and
                    sym.ada_type is not None and
                    sym.ada_type.kind == TypeKind.ENUMERATION)

        # Check for overloading (allowed for subprograms and enum literals)
        if name_lower in self.symbols:
            existing = self.symbols[name_lower]
            # Subprogram overloading
            if symbol.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION):
                if existing.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION):
                    # Add to overload chain
                    symbol.overloaded_next = existing
                    self.symbols[name_lower] = symbol
                    return
            # Enumeration literal overloading (Ada allows same literal in different enum types)
            elif is_enum_literal(symbol) and is_enum_literal(existing):
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
        self._init_exceptions()
        self._init_tags()
        self._init_characters()
        self._init_text_io_children()
        self._init_io_packages()
        self._init_streams()
        self._init_interfaces()
        self._init_system_packages()
        self._init_impdef()
        self._init_spprt13()
        self._init_assertions()
        self._init_synchronous_task_control()
        self._init_wide_characters()
        self._init_gnat_packages()

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

        # Also register at root level - Ada allows direct WITH of these generics
        # e.g., "with Unchecked_Deallocation;" without the Ada. prefix
        root_unchecked_conv = Symbol(
            name="Unchecked_Conversion",
            kind=SymbolKind.GENERIC_FUNCTION,
            scope_level=0,
        )
        root_unchecked_conv.is_builtin_generic = True
        self.current_scope.define(root_unchecked_conv)

        root_unchecked_dealloc = Symbol(
            name="Unchecked_Deallocation",
            kind=SymbolKind.GENERIC_PROCEDURE,
            scope_level=0,
        )
        root_unchecked_dealloc.is_builtin_generic = True
        self.current_scope.define(root_unchecked_dealloc)

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

    def _init_exceptions(self) -> None:
        """Add Ada.Exceptions package for exception handling utilities."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        str_type = PREDEFINED_TYPES["String"]

        # Create Exceptions subpackage
        exceptions_pkg = Symbol(
            name="Exceptions",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Exception_Id type (opaque - represents exception identity)
        exc_id_type = IntegerType(name="Exception_Id", low=0, high=0xFFFF)
        exc_id_sym = Symbol(
            name="Exception_Id",
            kind=SymbolKind.TYPE,
            ada_type=exc_id_type,
            scope_level=0,
        )

        # Null_Id constant
        null_id_sym = Symbol(
            name="Null_Id",
            kind=SymbolKind.VARIABLE,
            ada_type=exc_id_type,
            is_constant=True,
            value=0,
            scope_level=0,
        )

        # Exception_Occurrence type (record with exception info)
        exc_occ_type = RecordType(
            name="Exception_Occurrence",
            components=[],
        )
        exc_occ_sym = Symbol(
            name="Exception_Occurrence",
            kind=SymbolKind.TYPE,
            ada_type=exc_occ_type,
            scope_level=0,
        )

        # Exception_Occurrence_Access type
        from uada80.type_system import AccessType
        exc_occ_access_type = AccessType(
            name="Exception_Occurrence_Access",
            designated_type=exc_occ_type,
        )
        exc_occ_access_sym = Symbol(
            name="Exception_Occurrence_Access",
            kind=SymbolKind.TYPE,
            ada_type=exc_occ_access_type,
            scope_level=0,
        )

        # Exception_Name function (Exception_Id -> String)
        exc_name_func = Symbol(
            name="Exception_Name",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Id", SymbolKind.PARAMETER, exc_id_type, mode="in"),
            ],
        )
        exc_name_func.runtime_name = "_exc_name"

        # Exception_Name function (Exception_Occurrence -> String) - overloaded
        exc_name_occ_func = Symbol(
            name="Exception_Name",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("X", SymbolKind.PARAMETER, exc_occ_type, mode="in"),
            ],
            overloaded_next=exc_name_func,
        )
        exc_name_occ_func.runtime_name = "_exc_name_occ"

        # Exception_Message function
        exc_msg_func = Symbol(
            name="Exception_Message",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("X", SymbolKind.PARAMETER, exc_occ_type, mode="in"),
            ],
        )
        exc_msg_func.runtime_name = "_exc_message"

        # Exception_Information function
        exc_info_func = Symbol(
            name="Exception_Information",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("X", SymbolKind.PARAMETER, exc_occ_type, mode="in"),
            ],
        )
        exc_info_func.runtime_name = "_exc_info"

        # Raise_Exception procedure
        raise_exc_proc = Symbol(
            name="Raise_Exception",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("E", SymbolKind.PARAMETER, exc_id_type, mode="in"),
                Symbol("Message", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        raise_exc_proc.runtime_name = "_exc_raise"
        raise_exc_proc.is_no_return = True

        # Reraise_Occurrence procedure
        reraise_proc = Symbol(
            name="Reraise_Occurrence",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("X", SymbolKind.PARAMETER, exc_occ_type, mode="in"),
            ],
        )
        reraise_proc.runtime_name = "_exc_reraise"
        reraise_proc.is_no_return = True

        # Save_Occurrence procedure
        save_occ_proc = Symbol(
            name="Save_Occurrence",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Target", SymbolKind.PARAMETER, exc_occ_type, mode="out"),
                Symbol("Source", SymbolKind.PARAMETER, exc_occ_type, mode="in"),
            ],
        )
        save_occ_proc.runtime_name = "_exc_save"

        exceptions_pkg.public_symbols = {
            "exception_id": exc_id_sym,
            "null_id": null_id_sym,
            "exception_occurrence": exc_occ_sym,
            "exception_occurrence_access": exc_occ_access_sym,
            "exception_name": exc_name_occ_func,
            "exception_message": exc_msg_func,
            "exception_information": exc_info_func,
            "raise_exception": raise_exc_proc,
            "reraise_occurrence": reraise_proc,
            "save_occurrence": save_occ_proc,
        }

        ada_pkg.public_symbols["exceptions"] = exceptions_pkg

    def _init_tags(self) -> None:
        """Add Ada.Tags package for tagged type operations."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        str_type = PREDEFINED_TYPES["String"]
        bool_type = PREDEFINED_TYPES["Boolean"]

        # Create Tags subpackage
        tags_pkg = Symbol(
            name="Tags",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Tag type (opaque - pointer to type descriptor)
        tag_type = IntegerType(name="Tag", low=0, high=0xFFFF)
        tag_sym = Symbol(
            name="Tag",
            kind=SymbolKind.TYPE,
            ada_type=tag_type,
            scope_level=0,
        )

        # No_Tag constant
        no_tag_sym = Symbol(
            name="No_Tag",
            kind=SymbolKind.VARIABLE,
            ada_type=tag_type,
            is_constant=True,
            value=0,
            scope_level=0,
        )

        # Tag_Error exception
        tag_error_sym = Symbol(
            name="Tag_Error",
            kind=SymbolKind.EXCEPTION,
            scope_level=0,
        )

        # External_Tag function (Tag -> String)
        ext_tag_func = Symbol(
            name="External_Tag",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("T", SymbolKind.PARAMETER, tag_type, mode="in"),
            ],
        )
        ext_tag_func.runtime_name = "_tag_external"

        # Internal_Tag function (String -> Tag)
        int_tag_func = Symbol(
            name="Internal_Tag",
            kind=SymbolKind.FUNCTION,
            return_type=tag_type,
            scope_level=0,
            parameters=[
                Symbol("External", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        int_tag_func.runtime_name = "_tag_internal"

        # Expanded_Name function (Tag -> String) - full qualified name
        expanded_name_func = Symbol(
            name="Expanded_Name",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("T", SymbolKind.PARAMETER, tag_type, mode="in"),
            ],
        )
        expanded_name_func.runtime_name = "_tag_expanded_name"

        # Descendant_Tag function
        desc_tag_func = Symbol(
            name="Descendant_Tag",
            kind=SymbolKind.FUNCTION,
            return_type=tag_type,
            scope_level=0,
            parameters=[
                Symbol("External", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Ancestor", SymbolKind.PARAMETER, tag_type, mode="in"),
            ],
        )
        desc_tag_func.runtime_name = "_tag_descendant"

        # Is_Descendant_At_Same_Level function
        is_desc_func = Symbol(
            name="Is_Descendant_At_Same_Level",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Descendant", SymbolKind.PARAMETER, tag_type, mode="in"),
                Symbol("Ancestor", SymbolKind.PARAMETER, tag_type, mode="in"),
            ],
        )
        is_desc_func.runtime_name = "_tag_is_descendant"

        # Parent_Tag function
        parent_tag_func = Symbol(
            name="Parent_Tag",
            kind=SymbolKind.FUNCTION,
            return_type=tag_type,
            scope_level=0,
            parameters=[
                Symbol("T", SymbolKind.PARAMETER, tag_type, mode="in"),
            ],
        )
        parent_tag_func.runtime_name = "_tag_parent"

        # Interface_Ancestor_Tags function (returns array of tags)
        # Simplified: returns single tag for now
        iface_tags_func = Symbol(
            name="Interface_Ancestor_Tags",
            kind=SymbolKind.FUNCTION,
            return_type=tag_type,  # Simplified
            scope_level=0,
            parameters=[
                Symbol("T", SymbolKind.PARAMETER, tag_type, mode="in"),
            ],
        )
        iface_tags_func.runtime_name = "_tag_interfaces"

        # Type_Is_Abstract function
        type_abstract_func = Symbol(
            name="Type_Is_Abstract",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("T", SymbolKind.PARAMETER, tag_type, mode="in"),
            ],
        )
        type_abstract_func.runtime_name = "_tag_is_abstract"

        tags_pkg.public_symbols = {
            "tag": tag_sym,
            "no_tag": no_tag_sym,
            "tag_error": tag_error_sym,
            "external_tag": ext_tag_func,
            "internal_tag": int_tag_func,
            "expanded_name": expanded_name_func,
            "descendant_tag": desc_tag_func,
            "is_descendant_at_same_level": is_desc_func,
            "parent_tag": parent_tag_func,
            "interface_ancestor_tags": iface_tags_func,
            "type_is_abstract": type_abstract_func,
        }

        ada_pkg.public_symbols["tags"] = tags_pkg

    def _init_characters(self) -> None:
        """Add Ada.Characters and subpackages."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        char_type = PREDEFINED_TYPES["Character"]
        bool_type = PREDEFINED_TYPES["Boolean"]
        str_type = PREDEFINED_TYPES["String"]

        # Create Characters parent package
        chars_pkg = Symbol(
            name="Characters",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # =====================================================================
        # Ada.Characters.Handling - Character classification and conversion
        # =====================================================================
        handling_pkg = Symbol(
            name="Handling",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Character classification functions
        classification_funcs = [
            "Is_Control", "Is_Graphic", "Is_Letter", "Is_Lower", "Is_Upper",
            "Is_Basic", "Is_Digit", "Is_Decimal_Digit", "Is_Hexadecimal_Digit",
            "Is_Alphanumeric", "Is_Special", "Is_Line_Terminator",
            "Is_Mark", "Is_Other_Format",
        ]
        for func_name in classification_funcs:
            func_sym = Symbol(
                name=func_name,
                kind=SymbolKind.FUNCTION,
                return_type=bool_type,
                scope_level=0,
                parameters=[
                    Symbol("Item", SymbolKind.PARAMETER, char_type, mode="in"),
                ],
            )
            func_sym.runtime_name = f"_char_{func_name.lower()}"
            handling_pkg.public_symbols[func_name.lower()] = func_sym

        # Character conversion functions
        to_lower_func = Symbol(
            name="To_Lower",
            kind=SymbolKind.FUNCTION,
            return_type=char_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, char_type, mode="in"),
            ],
        )
        to_lower_func.runtime_name = "_char_to_lower"

        to_upper_func = Symbol(
            name="To_Upper",
            kind=SymbolKind.FUNCTION,
            return_type=char_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, char_type, mode="in"),
            ],
        )
        to_upper_func.runtime_name = "_char_to_upper"

        to_basic_func = Symbol(
            name="To_Basic",
            kind=SymbolKind.FUNCTION,
            return_type=char_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, char_type, mode="in"),
            ],
        )
        to_basic_func.runtime_name = "_char_to_basic"

        # String versions of conversion functions
        to_lower_str_func = Symbol(
            name="To_Lower",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
            overloaded_next=to_lower_func,
        )
        to_lower_str_func.runtime_name = "_str_to_lower"

        to_upper_str_func = Symbol(
            name="To_Upper",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
            overloaded_next=to_upper_func,
        )
        to_upper_str_func.runtime_name = "_str_to_upper"

        handling_pkg.public_symbols["to_lower"] = to_lower_str_func
        handling_pkg.public_symbols["to_upper"] = to_upper_str_func
        handling_pkg.public_symbols["to_basic"] = to_basic_func

        chars_pkg.public_symbols["handling"] = handling_pkg

        # =====================================================================
        # Ada.Characters.Latin_1 - Character constants
        # =====================================================================
        latin1_pkg = Symbol(
            name="Latin_1",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Control characters (C0)
        c0_chars = {
            "NUL": 0, "SOH": 1, "STX": 2, "ETX": 3, "EOT": 4, "ENQ": 5, "ACK": 6, "BEL": 7,
            "BS": 8, "HT": 9, "LF": 10, "VT": 11, "FF": 12, "CR": 13, "SO": 14, "SI": 15,
            "DLE": 16, "DC1": 17, "DC2": 18, "DC3": 19, "DC4": 20, "NAK": 21, "SYN": 22, "ETB": 23,
            "CAN": 24, "EM": 25, "SUB": 26, "ESC": 27, "FS": 28, "GS": 29, "RS": 30, "US": 31,
        }
        for name, code in c0_chars.items():
            sym = Symbol(
                name=name,
                kind=SymbolKind.VARIABLE,
                ada_type=char_type,
                is_constant=True,
                value=code,
                scope_level=0,
            )
            latin1_pkg.public_symbols[name.lower()] = sym

        # Common named characters
        named_chars = {
            "Space": 32, "Exclamation": 33, "Quotation": 34, "Number_Sign": 35,
            "Dollar_Sign": 36, "Percent_Sign": 37, "Ampersand": 38, "Apostrophe": 39,
            "Left_Parenthesis": 40, "Right_Parenthesis": 41, "Asterisk": 42, "Plus_Sign": 43,
            "Comma": 44, "Hyphen": 45, "Minus_Sign": 45, "Full_Stop": 46, "Solidus": 47,
            "Colon": 58, "Semicolon": 59, "Less_Than_Sign": 60, "Equals_Sign": 61,
            "Greater_Than_Sign": 62, "Question": 63, "Commercial_At": 64,
            "Left_Square_Bracket": 91, "Reverse_Solidus": 92, "Right_Square_Bracket": 93,
            "Circumflex": 94, "Low_Line": 95, "Grave": 96,
            "Left_Curly_Bracket": 123, "Vertical_Line": 124, "Right_Curly_Bracket": 125,
            "Tilde": 126, "DEL": 127,
        }
        for name, code in named_chars.items():
            sym = Symbol(
                name=name,
                kind=SymbolKind.VARIABLE,
                ada_type=char_type,
                is_constant=True,
                value=code,
                scope_level=0,
            )
            latin1_pkg.public_symbols[name.lower()] = sym

        # C1 control characters
        c1_chars = {
            "Reserved_128": 128, "Reserved_129": 129, "BPH": 130, "NBH": 131,
            "Reserved_132": 132, "NEL": 133, "SSA": 134, "ESA": 135,
            "HTS": 136, "HTJ": 137, "VTS": 138, "PLD": 139,
            "PLU": 140, "RI": 141, "SS2": 142, "SS3": 143,
            "DCS": 144, "PU1": 145, "PU2": 146, "STS": 147,
            "CCH": 148, "MW": 149, "SPA": 150, "EPA": 151,
            "SOS": 152, "Reserved_153": 153, "SCI": 154, "CSI": 155,
            "ST": 156, "OSC": 157, "PM": 158, "APC": 159,
        }
        for name, code in c1_chars.items():
            sym = Symbol(
                name=name,
                kind=SymbolKind.VARIABLE,
                ada_type=char_type,
                is_constant=True,
                value=code,
                scope_level=0,
            )
            latin1_pkg.public_symbols[name.lower()] = sym

        # Latin-1 supplement characters
        latin1_sup_chars = {
            "No_Break_Space": 160, "Inverted_Exclamation": 161, "Cent_Sign": 162,
            "Pound_Sign": 163, "Currency_Sign": 164, "Yen_Sign": 165, "Broken_Bar": 166,
            "Section_Sign": 167, "Diaeresis": 168, "Copyright_Sign": 169,
            "Feminine_Ordinal_Indicator": 170, "Left_Angle_Quotation": 171,
            "Not_Sign": 172, "Soft_Hyphen": 173, "Registered_Trade_Mark_Sign": 174,
            "Macron": 175, "Degree_Sign": 176, "Plus_Minus_Sign": 177,
            "Superscript_Two": 178, "Superscript_Three": 179, "Acute": 180,
            "Micro_Sign": 181, "Pilcrow_Sign": 182, "Middle_Dot": 183,
            "Cedilla": 184, "Superscript_One": 185, "Masculine_Ordinal_Indicator": 186,
            "Right_Angle_Quotation": 187, "Fraction_One_Quarter": 188,
            "Fraction_One_Half": 189, "Fraction_Three_Quarters": 190,
            "Inverted_Question": 191, "Multiplication_Sign": 215, "Division_Sign": 247,
        }
        for name, code in latin1_sup_chars.items():
            sym = Symbol(
                name=name,
                kind=SymbolKind.VARIABLE,
                ada_type=char_type,
                is_constant=True,
                value=code,
                scope_level=0,
            )
            latin1_pkg.public_symbols[name.lower()] = sym

        # LC (lowercase) and UC (uppercase) letter constants
        for i in range(26):
            # Uppercase A-Z
            uc_name = f"UC_{chr(65+i)}"
            uc_sym = Symbol(
                name=uc_name,
                kind=SymbolKind.VARIABLE,
                ada_type=char_type,
                is_constant=True,
                value=65 + i,
                scope_level=0,
            )
            latin1_pkg.public_symbols[uc_name.lower()] = uc_sym

            # Lowercase a-z
            lc_name = f"LC_{chr(65+i)}"
            lc_sym = Symbol(
                name=lc_name,
                kind=SymbolKind.VARIABLE,
                ada_type=char_type,
                is_constant=True,
                value=97 + i,
                scope_level=0,
            )
            latin1_pkg.public_symbols[lc_name.lower()] = lc_sym

        chars_pkg.public_symbols["latin_1"] = latin1_pkg

        # =====================================================================
        # Ada.Characters.Conversions - Wide_Character conversions
        # =====================================================================
        conv_pkg = Symbol(
            name="Conversions",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        wide_char_type = PREDEFINED_TYPES.get("Wide_Character", char_type)
        wide_str_type = PREDEFINED_TYPES.get("Wide_String", str_type)

        # Is_Character function
        is_char_func = Symbol(
            name="Is_Character",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in"),
            ],
        )

        # Is_String function
        is_str_func = Symbol(
            name="Is_String",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, wide_str_type, mode="in"),
            ],
        )

        # To_Character function
        to_char_func = Symbol(
            name="To_Character",
            kind=SymbolKind.FUNCTION,
            return_type=char_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in"),
            ],
        )

        # To_String function
        to_str_func = Symbol(
            name="To_String",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, wide_str_type, mode="in"),
            ],
        )

        # To_Wide_Character function
        to_wide_char_func = Symbol(
            name="To_Wide_Character",
            kind=SymbolKind.FUNCTION,
            return_type=wide_char_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, char_type, mode="in"),
            ],
        )

        # To_Wide_String function
        to_wide_str_func = Symbol(
            name="To_Wide_String",
            kind=SymbolKind.FUNCTION,
            return_type=wide_str_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )

        conv_pkg.public_symbols = {
            "is_character": is_char_func,
            "is_string": is_str_func,
            "to_character": to_char_func,
            "to_string": to_str_func,
            "to_wide_character": to_wide_char_func,
            "to_wide_string": to_wide_str_func,
        }

        chars_pkg.public_symbols["conversions"] = conv_pkg

        ada_pkg.public_symbols["characters"] = chars_pkg

    def _init_text_io_children(self) -> None:
        """Add Ada.Text_IO child packages (Integer_IO, Float_IO, etc.)."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        text_io_pkg = ada_pkg.public_symbols.get("text_io")
        if text_io_pkg is None:
            return

        int_type = PREDEFINED_TYPES["Integer"]
        nat_type = PREDEFINED_TYPES["Natural"]
        str_type = PREDEFINED_TYPES["String"]
        float_type = PREDEFINED_TYPES.get("Float")

        # =====================================================================
        # Ada.Text_IO.Integer_IO - Generic integer I/O
        # =====================================================================
        int_io_pkg = Symbol(
            name="Integer_IO",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        int_io_pkg.is_builtin_generic = True

        # Get procedure
        int_get_proc = Symbol(
            name="Get",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, int_type, mode="out"),
            ],
        )
        int_get_proc.runtime_name = "_int_io_get"

        # Put procedure
        int_put_proc = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, int_type, mode="in"),
                Symbol("Width", SymbolKind.PARAMETER, nat_type, mode="in"),
            ],
        )
        int_put_proc.runtime_name = "_int_io_put"

        # Get procedure (from string)
        int_get_str_proc = Symbol(
            name="Get",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("From", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, int_type, mode="out"),
                Symbol("Last", SymbolKind.PARAMETER, nat_type, mode="out"),
            ],
            overloaded_next=int_get_proc,
        )
        int_get_str_proc.runtime_name = "_int_io_get_str"

        # Put procedure (to string)
        int_put_str_proc = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("To", SymbolKind.PARAMETER, str_type, mode="out"),
                Symbol("Item", SymbolKind.PARAMETER, int_type, mode="in"),
            ],
            overloaded_next=int_put_proc,
        )
        int_put_str_proc.runtime_name = "_int_io_put_str"

        int_io_pkg.public_symbols = {
            "get": int_get_str_proc,
            "put": int_put_str_proc,
            "default_width": Symbol("Default_Width", SymbolKind.VARIABLE, nat_type, is_constant=True, value=11, scope_level=0),
            "default_base": Symbol("Default_Base", SymbolKind.VARIABLE, nat_type, is_constant=True, value=10, scope_level=0),
        }

        text_io_pkg.public_symbols["integer_io"] = int_io_pkg

        # =====================================================================
        # Ada.Text_IO.Float_IO - Generic float I/O
        # =====================================================================
        if float_type:
            float_io_pkg = Symbol(
                name="Float_IO",
                kind=SymbolKind.GENERIC_PACKAGE,
                scope_level=0,
            )
            float_io_pkg.is_builtin_generic = True

            # Get procedure
            float_get_proc = Symbol(
                name="Get",
                kind=SymbolKind.PROCEDURE,
                scope_level=0,
                parameters=[
                    Symbol("Item", SymbolKind.PARAMETER, float_type, mode="out"),
                ],
            )
            float_get_proc.runtime_name = "_float_io_get"

            # Put procedure
            float_put_proc = Symbol(
                name="Put",
                kind=SymbolKind.PROCEDURE,
                scope_level=0,
                parameters=[
                    Symbol("Item", SymbolKind.PARAMETER, float_type, mode="in"),
                    Symbol("Fore", SymbolKind.PARAMETER, nat_type, mode="in"),
                    Symbol("Aft", SymbolKind.PARAMETER, nat_type, mode="in"),
                    Symbol("Exp", SymbolKind.PARAMETER, nat_type, mode="in"),
                ],
            )
            float_put_proc.runtime_name = "_float_io_put"

            float_io_pkg.public_symbols = {
                "get": float_get_proc,
                "put": float_put_proc,
                "default_fore": Symbol("Default_Fore", SymbolKind.VARIABLE, nat_type, is_constant=True, value=2, scope_level=0),
                "default_aft": Symbol("Default_Aft", SymbolKind.VARIABLE, nat_type, is_constant=True, value=6, scope_level=0),
                "default_exp": Symbol("Default_Exp", SymbolKind.VARIABLE, nat_type, is_constant=True, value=3, scope_level=0),
            }

            text_io_pkg.public_symbols["float_io"] = float_io_pkg

        # =====================================================================
        # Ada.Text_IO.Enumeration_IO - Generic enumeration I/O
        # =====================================================================
        enum_io_pkg = Symbol(
            name="Enumeration_IO",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        enum_io_pkg.is_builtin_generic = True

        text_io_pkg.public_symbols["enumeration_io"] = enum_io_pkg

        # =====================================================================
        # Ada.Text_IO.Modular_IO - Generic modular type I/O
        # =====================================================================
        mod_io_pkg = Symbol(
            name="Modular_IO",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        mod_io_pkg.is_builtin_generic = True

        text_io_pkg.public_symbols["modular_io"] = mod_io_pkg

        # =====================================================================
        # Ada.Integer_Text_IO - Predefined Integer_IO instance
        # =====================================================================
        int_text_io_pkg = Symbol(
            name="Integer_Text_IO",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Copy operations from Integer_IO
        int_text_io_pkg.public_symbols = dict(int_io_pkg.public_symbols)

        ada_pkg.public_symbols["integer_text_io"] = int_text_io_pkg

        # =====================================================================
        # Ada.Float_Text_IO - Predefined Float_IO instance
        # =====================================================================
        if float_type:
            float_text_io_pkg = Symbol(
                name="Float_Text_IO",
                kind=SymbolKind.PACKAGE,
                scope_level=0,
            )
            float_text_io_pkg.public_symbols = dict(float_io_pkg.public_symbols)

            ada_pkg.public_symbols["float_text_io"] = float_text_io_pkg

    def _init_io_packages(self) -> None:
        """Add Ada.Sequential_IO and Ada.Direct_IO generic packages."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        int_type = PREDEFINED_TYPES["Integer"]
        nat_type = PREDEFINED_TYPES["Natural"]
        bool_type = PREDEFINED_TYPES["Boolean"]
        str_type = PREDEFINED_TYPES["String"]

        # =====================================================================
        # Ada.Sequential_IO - Generic sequential file I/O
        # =====================================================================
        seq_io_pkg = Symbol(
            name="Sequential_IO",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        seq_io_pkg.is_builtin_generic = True

        # File_Type (opaque - file descriptor)
        from uada80.type_system import AccessType
        file_record = RecordType(name="File_Type", components=[])
        file_type = AccessType(name="File_Type", designated_type=file_record)
        file_type_sym = Symbol(
            name="File_Type",
            kind=SymbolKind.TYPE,
            ada_type=file_type,
            scope_level=0,
        )

        # File_Mode type
        from uada80.type_system import EnumerationType
        file_mode_type = EnumerationType(
            name="File_Mode",
            literals=["In_File", "Out_File", "Append_File"],
        )
        file_mode_sym = Symbol(
            name="File_Mode",
            kind=SymbolKind.TYPE,
            ada_type=file_mode_type,
            scope_level=0,
        )

        # Create procedure
        create_proc = Symbol(
            name="Create",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in out"),
                Symbol("Mode", SymbolKind.PARAMETER, file_mode_type, mode="in"),
                Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        create_proc.runtime_name = "_seq_create"

        # Open procedure
        open_proc = Symbol(
            name="Open",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in out"),
                Symbol("Mode", SymbolKind.PARAMETER, file_mode_type, mode="in"),
                Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        open_proc.runtime_name = "_seq_open"

        # Close procedure
        close_proc = Symbol(
            name="Close",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in out"),
            ],
        )
        close_proc.runtime_name = "_seq_close"

        # Delete procedure
        delete_proc = Symbol(
            name="Delete",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in out"),
            ],
        )
        delete_proc.runtime_name = "_seq_delete"

        # Reset procedure
        reset_proc = Symbol(
            name="Reset",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in out"),
            ],
        )
        reset_proc.runtime_name = "_seq_reset"

        # Mode function
        mode_func = Symbol(
            name="Mode",
            kind=SymbolKind.FUNCTION,
            return_type=file_mode_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
            ],
        )
        mode_func.runtime_name = "_seq_mode"

        # Name function
        name_func = Symbol(
            name="Name",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
            ],
        )
        name_func.runtime_name = "_seq_name"

        # Is_Open function
        is_open_func = Symbol(
            name="Is_Open",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
            ],
        )
        is_open_func.runtime_name = "_seq_is_open"

        # End_Of_File function
        eof_func = Symbol(
            name="End_Of_File",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
            ],
        )
        eof_func.runtime_name = "_seq_eof"

        # Read procedure (Element_Type is generic formal)
        read_proc = Symbol(
            name="Read",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, None, mode="out"),
            ],
        )
        read_proc.runtime_name = "_seq_read"

        # Write procedure
        write_proc = Symbol(
            name="Write",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, None, mode="in"),
            ],
        )
        write_proc.runtime_name = "_seq_write"

        seq_io_pkg.public_symbols = {
            "file_type": file_type_sym,
            "file_mode": file_mode_sym,
            "in_file": Symbol("In_File", SymbolKind.VARIABLE, file_mode_type, is_constant=True, scope_level=0),
            "out_file": Symbol("Out_File", SymbolKind.VARIABLE, file_mode_type, is_constant=True, scope_level=0),
            "append_file": Symbol("Append_File", SymbolKind.VARIABLE, file_mode_type, is_constant=True, scope_level=0),
            "create": create_proc,
            "open": open_proc,
            "close": close_proc,
            "delete": delete_proc,
            "reset": reset_proc,
            "mode": mode_func,
            "name": name_func,
            "is_open": is_open_func,
            "end_of_file": eof_func,
            "read": read_proc,
            "write": write_proc,
        }

        ada_pkg.public_symbols["sequential_io"] = seq_io_pkg

        # =====================================================================
        # Ada.Direct_IO - Generic direct (random access) file I/O
        # =====================================================================
        dir_io_pkg = Symbol(
            name="Direct_IO",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        dir_io_pkg.is_builtin_generic = True

        # Count type (for file position)
        count_type = IntegerType(name="Count", low=0, high=2**31-1)
        count_sym = Symbol(
            name="Count",
            kind=SymbolKind.TYPE,
            ada_type=count_type,
            scope_level=0,
        )

        # Positive_Count subtype
        pos_count_type = IntegerType(name="Positive_Count", low=1, high=2**31-1)
        pos_count_sym = Symbol(
            name="Positive_Count",
            kind=SymbolKind.TYPE,
            ada_type=pos_count_type,
            scope_level=0,
        )

        # Direct_IO file mode includes Inout_File
        dir_file_mode_type = EnumerationType(
            name="File_Mode",
            literals=["In_File", "Inout_File", "Out_File"],
        )
        dir_file_mode_sym = Symbol(
            name="File_Mode",
            kind=SymbolKind.TYPE,
            ada_type=dir_file_mode_type,
            scope_level=0,
        )

        # Index function
        index_func = Symbol(
            name="Index",
            kind=SymbolKind.FUNCTION,
            return_type=pos_count_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
            ],
        )
        index_func.runtime_name = "_dir_index"

        # Set_Index procedure
        set_index_proc = Symbol(
            name="Set_Index",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
                Symbol("To", SymbolKind.PARAMETER, pos_count_type, mode="in"),
            ],
        )
        set_index_proc.runtime_name = "_dir_set_index"

        # Size function
        size_func = Symbol(
            name="Size",
            kind=SymbolKind.FUNCTION,
            return_type=count_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
            ],
        )
        size_func.runtime_name = "_dir_size"

        # Read with position
        dir_read_proc = Symbol(
            name="Read",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, None, mode="out"),
                Symbol("From", SymbolKind.PARAMETER, pos_count_type, mode="in"),
            ],
        )
        dir_read_proc.runtime_name = "_dir_read"

        # Write with position
        dir_write_proc = Symbol(
            name="Write",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, file_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, None, mode="in"),
                Symbol("To", SymbolKind.PARAMETER, pos_count_type, mode="in"),
            ],
        )
        dir_write_proc.runtime_name = "_dir_write"

        dir_io_pkg.public_symbols = {
            "file_type": file_type_sym,
            "file_mode": dir_file_mode_sym,
            "count": count_sym,
            "positive_count": pos_count_sym,
            "in_file": Symbol("In_File", SymbolKind.VARIABLE, dir_file_mode_type, is_constant=True, scope_level=0),
            "inout_file": Symbol("Inout_File", SymbolKind.VARIABLE, dir_file_mode_type, is_constant=True, scope_level=0),
            "out_file": Symbol("Out_File", SymbolKind.VARIABLE, dir_file_mode_type, is_constant=True, scope_level=0),
            "create": create_proc,
            "open": open_proc,
            "close": close_proc,
            "delete": delete_proc,
            "reset": reset_proc,
            "mode": mode_func,
            "name": name_func,
            "is_open": is_open_func,
            "end_of_file": eof_func,
            "index": index_func,
            "set_index": set_index_proc,
            "size": size_func,
            "read": dir_read_proc,
            "write": dir_write_proc,
        }

        ada_pkg.public_symbols["direct_io"] = dir_io_pkg

    def _init_streams(self) -> None:
        """Add Ada.Streams and Ada.Streams.Stream_IO packages."""
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        int_type = PREDEFINED_TYPES["Integer"]
        nat_type = PREDEFINED_TYPES["Natural"]
        bool_type = PREDEFINED_TYPES["Boolean"]
        str_type = PREDEFINED_TYPES["String"]

        # =====================================================================
        # Ada.Streams - Stream types and operations
        # =====================================================================
        streams_pkg = Symbol(
            name="Streams",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Stream_Element type (8-bit byte)
        stream_elem_type = IntegerType(name="Stream_Element", low=0, high=255)
        stream_elem_sym = Symbol(
            name="Stream_Element",
            kind=SymbolKind.TYPE,
            ada_type=stream_elem_type,
            scope_level=0,
        )

        # Stream_Element_Offset type
        stream_offset_type = IntegerType(name="Stream_Element_Offset", low=-2**31, high=2**31-1)
        stream_offset_sym = Symbol(
            name="Stream_Element_Offset",
            kind=SymbolKind.TYPE,
            ada_type=stream_offset_type,
            scope_level=0,
        )

        # Stream_Element_Count subtype
        stream_count_type = IntegerType(name="Stream_Element_Count", low=0, high=2**31-1)
        stream_count_sym = Symbol(
            name="Stream_Element_Count",
            kind=SymbolKind.TYPE,
            ada_type=stream_count_type,
            scope_level=0,
        )

        # Stream_Element_Array type (array of Stream_Element)
        from uada80.type_system import ArrayType
        stream_array_type = ArrayType(
            name="Stream_Element_Array",
            component_type=stream_elem_type,
            index_types=[stream_offset_type],
            is_constrained=False,
        )
        stream_array_sym = Symbol(
            name="Stream_Element_Array",
            kind=SymbolKind.TYPE,
            ada_type=stream_array_type,
            scope_level=0,
        )

        # Root_Stream_Type (abstract tagged limited type)
        root_stream_type = RecordType(
            name="Root_Stream_Type",
            is_tagged=True,
            is_limited=True,
        )
        root_stream_sym = Symbol(
            name="Root_Stream_Type",
            kind=SymbolKind.TYPE,
            ada_type=root_stream_type,
            scope_level=0,
        )

        # Read procedure (abstract)
        stream_read_proc = Symbol(
            name="Read",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            is_abstract=True,
            parameters=[
                Symbol("Stream", SymbolKind.PARAMETER, root_stream_type, mode="in out"),
                Symbol("Item", SymbolKind.PARAMETER, stream_array_type, mode="out"),
                Symbol("Last", SymbolKind.PARAMETER, stream_offset_type, mode="out"),
            ],
        )

        # Write procedure (abstract)
        stream_write_proc = Symbol(
            name="Write",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            is_abstract=True,
            parameters=[
                Symbol("Stream", SymbolKind.PARAMETER, root_stream_type, mode="in out"),
                Symbol("Item", SymbolKind.PARAMETER, stream_array_type, mode="in"),
            ],
        )

        streams_pkg.public_symbols = {
            "stream_element": stream_elem_sym,
            "stream_element_offset": stream_offset_sym,
            "stream_element_count": stream_count_sym,
            "stream_element_array": stream_array_sym,
            "root_stream_type": root_stream_sym,
            "read": stream_read_proc,
            "write": stream_write_proc,
        }

        # =====================================================================
        # Ada.Streams.Stream_IO - Stream-based file I/O
        # =====================================================================
        stream_io_pkg = Symbol(
            name="Stream_IO",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # File_Type
        from uada80.type_system import AccessType
        sio_file_record = RecordType(name="File_Type", components=[])
        sio_file_type = AccessType(name="File_Type", designated_type=sio_file_record)
        sio_file_sym = Symbol(
            name="File_Type",
            kind=SymbolKind.TYPE,
            ada_type=sio_file_type,
            scope_level=0,
        )

        # Stream_Access type
        stream_access_type = AccessType(
            name="Stream_Access",
            designated_type=root_stream_type,
        )
        stream_access_sym = Symbol(
            name="Stream_Access",
            kind=SymbolKind.TYPE,
            ada_type=stream_access_type,
            scope_level=0,
        )

        # File_Mode
        from uada80.type_system import EnumerationType
        sio_mode_type = EnumerationType(
            name="File_Mode",
            literals=["In_File", "Out_File", "Append_File"],
        )
        sio_mode_sym = Symbol(
            name="File_Mode",
            kind=SymbolKind.TYPE,
            ada_type=sio_mode_type,
            scope_level=0,
        )

        # Count type
        sio_count_type = IntegerType(name="Count", low=0, high=2**63-1)
        sio_count_sym = Symbol(
            name="Count",
            kind=SymbolKind.TYPE,
            ada_type=sio_count_type,
            scope_level=0,
        )

        # Positive_Count subtype
        sio_pos_count_type = IntegerType(name="Positive_Count", low=1, high=2**63-1)
        sio_pos_count_sym = Symbol(
            name="Positive_Count",
            kind=SymbolKind.TYPE,
            ada_type=sio_pos_count_type,
            scope_level=0,
        )

        # Create procedure
        sio_create_proc = Symbol(
            name="Create",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in out"),
                Symbol("Mode", SymbolKind.PARAMETER, sio_mode_type, mode="in"),
                Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        sio_create_proc.runtime_name = "_sio_create"

        # Open procedure
        sio_open_proc = Symbol(
            name="Open",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in out"),
                Symbol("Mode", SymbolKind.PARAMETER, sio_mode_type, mode="in"),
                Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        sio_open_proc.runtime_name = "_sio_open"

        # Close procedure
        sio_close_proc = Symbol(
            name="Close",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in out"),
            ],
        )
        sio_close_proc.runtime_name = "_sio_close"

        # Stream function
        sio_stream_func = Symbol(
            name="Stream",
            kind=SymbolKind.FUNCTION,
            return_type=stream_access_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in"),
            ],
        )
        sio_stream_func.runtime_name = "_sio_stream"

        # Read procedure
        sio_read_proc = Symbol(
            name="Read",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, stream_array_type, mode="out"),
                Symbol("Last", SymbolKind.PARAMETER, stream_offset_type, mode="out"),
            ],
        )
        sio_read_proc.runtime_name = "_sio_read"

        # Write procedure
        sio_write_proc = Symbol(
            name="Write",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in"),
                Symbol("Item", SymbolKind.PARAMETER, stream_array_type, mode="in"),
            ],
        )
        sio_write_proc.runtime_name = "_sio_write"

        # Index function
        sio_index_func = Symbol(
            name="Index",
            kind=SymbolKind.FUNCTION,
            return_type=sio_pos_count_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in"),
            ],
        )
        sio_index_func.runtime_name = "_sio_index"

        # Set_Index procedure
        sio_set_index_proc = Symbol(
            name="Set_Index",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in"),
                Symbol("To", SymbolKind.PARAMETER, sio_pos_count_type, mode="in"),
            ],
        )
        sio_set_index_proc.runtime_name = "_sio_set_index"

        # Size function
        sio_size_func = Symbol(
            name="Size",
            kind=SymbolKind.FUNCTION,
            return_type=sio_count_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in"),
            ],
        )
        sio_size_func.runtime_name = "_sio_size"

        # Set_Mode procedure
        sio_set_mode_proc = Symbol(
            name="Set_Mode",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in out"),
                Symbol("Mode", SymbolKind.PARAMETER, sio_mode_type, mode="in"),
            ],
        )
        sio_set_mode_proc.runtime_name = "_sio_set_mode"

        # Is_Open function
        sio_is_open_func = Symbol(
            name="Is_Open",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in"),
            ],
        )
        sio_is_open_func.runtime_name = "_sio_is_open"

        # End_Of_File function
        sio_eof_func = Symbol(
            name="End_Of_File",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("File", SymbolKind.PARAMETER, sio_file_type, mode="in"),
            ],
        )
        sio_eof_func.runtime_name = "_sio_eof"

        stream_io_pkg.public_symbols = {
            "file_type": sio_file_sym,
            "stream_access": stream_access_sym,
            "file_mode": sio_mode_sym,
            "count": sio_count_sym,
            "positive_count": sio_pos_count_sym,
            "in_file": Symbol("In_File", SymbolKind.VARIABLE, sio_mode_type, is_constant=True, scope_level=0),
            "out_file": Symbol("Out_File", SymbolKind.VARIABLE, sio_mode_type, is_constant=True, scope_level=0),
            "append_file": Symbol("Append_File", SymbolKind.VARIABLE, sio_mode_type, is_constant=True, scope_level=0),
            "create": sio_create_proc,
            "open": sio_open_proc,
            "close": sio_close_proc,
            "stream": sio_stream_func,
            "read": sio_read_proc,
            "write": sio_write_proc,
            "index": sio_index_func,
            "set_index": sio_set_index_proc,
            "size": sio_size_func,
            "set_mode": sio_set_mode_proc,
            "is_open": sio_is_open_func,
            "end_of_file": sio_eof_func,
        }

        streams_pkg.public_symbols["stream_io"] = stream_io_pkg

        ada_pkg.public_symbols["streams"] = streams_pkg

    def _init_interfaces(self) -> None:
        """Add Interfaces package with C types for interoperability."""
        int_type = PREDEFINED_TYPES["Integer"]
        bool_type = PREDEFINED_TYPES["Boolean"]

        # Create Interfaces package at root level (not under Ada)
        interfaces_pkg = Symbol(
            name="Interfaces",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # C-compatible integer types
        # Integer_8 (-128..127)
        int8_type = IntegerType(name="Integer_8", low=-128, high=127)
        interfaces_pkg.public_symbols["integer_8"] = Symbol(
            name="Integer_8",
            kind=SymbolKind.TYPE,
            ada_type=int8_type,
            scope_level=0,
        )

        # Integer_16 (-32768..32767)
        int16_type = IntegerType(name="Integer_16", low=-32768, high=32767)
        interfaces_pkg.public_symbols["integer_16"] = Symbol(
            name="Integer_16",
            kind=SymbolKind.TYPE,
            ada_type=int16_type,
            scope_level=0,
        )

        # Integer_32
        int32_type = IntegerType(name="Integer_32", low=-2**31, high=2**31-1)
        interfaces_pkg.public_symbols["integer_32"] = Symbol(
            name="Integer_32",
            kind=SymbolKind.TYPE,
            ada_type=int32_type,
            scope_level=0,
        )

        # Integer_64
        int64_type = IntegerType(name="Integer_64", low=-2**63, high=2**63-1)
        interfaces_pkg.public_symbols["integer_64"] = Symbol(
            name="Integer_64",
            kind=SymbolKind.TYPE,
            ada_type=int64_type,
            scope_level=0,
        )

        # Unsigned types (modular)
        from uada80.type_system import ModularType

        # Unsigned_8 (mod 256)
        uint8_type = ModularType(name="Unsigned_8", modulus=256)
        interfaces_pkg.public_symbols["unsigned_8"] = Symbol(
            name="Unsigned_8",
            kind=SymbolKind.TYPE,
            ada_type=uint8_type,
            scope_level=0,
        )

        # Unsigned_16 (mod 65536)
        uint16_type = ModularType(name="Unsigned_16", modulus=65536)
        interfaces_pkg.public_symbols["unsigned_16"] = Symbol(
            name="Unsigned_16",
            kind=SymbolKind.TYPE,
            ada_type=uint16_type,
            scope_level=0,
        )

        # Unsigned_32 (mod 2**32)
        uint32_type = ModularType(name="Unsigned_32", modulus=2**32)
        interfaces_pkg.public_symbols["unsigned_32"] = Symbol(
            name="Unsigned_32",
            kind=SymbolKind.TYPE,
            ada_type=uint32_type,
            scope_level=0,
        )

        # Unsigned_64 (mod 2**64)
        uint64_type = ModularType(name="Unsigned_64", modulus=2**64)
        interfaces_pkg.public_symbols["unsigned_64"] = Symbol(
            name="Unsigned_64",
            kind=SymbolKind.TYPE,
            ada_type=uint64_type,
            scope_level=0,
        )

        # Shift and rotate functions for each unsigned type
        for type_name, uint_type in [
            ("Unsigned_8", uint8_type),
            ("Unsigned_16", uint16_type),
            ("Unsigned_32", uint32_type),
            ("Unsigned_64", uint64_type),
        ]:
            # Shift_Left
            shift_left = Symbol(
                name="Shift_Left",
                kind=SymbolKind.FUNCTION,
                return_type=uint_type,
                scope_level=0,
                parameters=[
                    Symbol("Value", SymbolKind.PARAMETER, uint_type, mode="in"),
                    Symbol("Amount", SymbolKind.PARAMETER, int_type, mode="in"),
                ],
            )
            shift_left.runtime_name = f"_shift_left_{type_name.lower()}"

            # Shift_Right
            shift_right = Symbol(
                name="Shift_Right",
                kind=SymbolKind.FUNCTION,
                return_type=uint_type,
                scope_level=0,
                parameters=[
                    Symbol("Value", SymbolKind.PARAMETER, uint_type, mode="in"),
                    Symbol("Amount", SymbolKind.PARAMETER, int_type, mode="in"),
                ],
                overloaded_next=shift_left,
            )
            shift_right.runtime_name = f"_shift_right_{type_name.lower()}"

            # Shift_Right_Arithmetic
            shift_right_arith = Symbol(
                name="Shift_Right_Arithmetic",
                kind=SymbolKind.FUNCTION,
                return_type=uint_type,
                scope_level=0,
                parameters=[
                    Symbol("Value", SymbolKind.PARAMETER, uint_type, mode="in"),
                    Symbol("Amount", SymbolKind.PARAMETER, int_type, mode="in"),
                ],
            )
            shift_right_arith.runtime_name = f"_shift_right_arith_{type_name.lower()}"

            # Rotate_Left
            rotate_left = Symbol(
                name="Rotate_Left",
                kind=SymbolKind.FUNCTION,
                return_type=uint_type,
                scope_level=0,
                parameters=[
                    Symbol("Value", SymbolKind.PARAMETER, uint_type, mode="in"),
                    Symbol("Amount", SymbolKind.PARAMETER, int_type, mode="in"),
                ],
            )
            rotate_left.runtime_name = f"_rotate_left_{type_name.lower()}"

            # Rotate_Right
            rotate_right = Symbol(
                name="Rotate_Right",
                kind=SymbolKind.FUNCTION,
                return_type=uint_type,
                scope_level=0,
                parameters=[
                    Symbol("Value", SymbolKind.PARAMETER, uint_type, mode="in"),
                    Symbol("Amount", SymbolKind.PARAMETER, int_type, mode="in"),
                ],
            )
            rotate_right.runtime_name = f"_rotate_right_{type_name.lower()}"

        # Add shift/rotate functions (overloaded across all unsigned types)
        interfaces_pkg.public_symbols["shift_left"] = shift_left
        interfaces_pkg.public_symbols["shift_right"] = shift_right
        interfaces_pkg.public_symbols["shift_right_arithmetic"] = shift_right_arith
        interfaces_pkg.public_symbols["rotate_left"] = rotate_left
        interfaces_pkg.public_symbols["rotate_right"] = rotate_right

        # Register Interfaces at root scope
        self.current_scope.define(interfaces_pkg)

        # =====================================================================
        # Interfaces.C - C language types
        # =====================================================================
        c_pkg = Symbol(
            name="C",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # C.int (typically 16-bit on Z80)
        c_int_type = IntegerType(name="int", low=-32768, high=32767)
        c_pkg.public_symbols["int"] = Symbol(
            name="int",
            kind=SymbolKind.TYPE,
            ada_type=c_int_type,
            scope_level=0,
        )

        # C.short
        c_short_type = IntegerType(name="short", low=-32768, high=32767)
        c_pkg.public_symbols["short"] = Symbol(
            name="short",
            kind=SymbolKind.TYPE,
            ada_type=c_short_type,
            scope_level=0,
        )

        # C.long (32-bit on Z80)
        c_long_type = IntegerType(name="long", low=-2**31, high=2**31-1)
        c_pkg.public_symbols["long"] = Symbol(
            name="long",
            kind=SymbolKind.TYPE,
            ada_type=c_long_type,
            scope_level=0,
        )

        # C.unsigned (mod 65536 on Z80)
        c_unsigned_type = ModularType(name="unsigned", modulus=65536)
        c_pkg.public_symbols["unsigned"] = Symbol(
            name="unsigned",
            kind=SymbolKind.TYPE,
            ada_type=c_unsigned_type,
            scope_level=0,
        )

        # C.unsigned_short
        c_ushort_type = ModularType(name="unsigned_short", modulus=65536)
        c_pkg.public_symbols["unsigned_short"] = Symbol(
            name="unsigned_short",
            kind=SymbolKind.TYPE,
            ada_type=c_ushort_type,
            scope_level=0,
        )

        # C.unsigned_long
        c_ulong_type = ModularType(name="unsigned_long", modulus=2**32)
        c_pkg.public_symbols["unsigned_long"] = Symbol(
            name="unsigned_long",
            kind=SymbolKind.TYPE,
            ada_type=c_ulong_type,
            scope_level=0,
        )

        # C.char (8-bit)
        c_char_type = IntegerType(name="char", low=-128, high=127)
        c_pkg.public_symbols["char"] = Symbol(
            name="char",
            kind=SymbolKind.TYPE,
            ada_type=c_char_type,
            scope_level=0,
        )

        # C.unsigned_char
        c_uchar_type = ModularType(name="unsigned_char", modulus=256)
        c_pkg.public_symbols["unsigned_char"] = Symbol(
            name="unsigned_char",
            kind=SymbolKind.TYPE,
            ada_type=c_uchar_type,
            scope_level=0,
        )

        # C.signed_char
        c_schar_type = IntegerType(name="signed_char", low=-128, high=127)
        c_pkg.public_symbols["signed_char"] = Symbol(
            name="signed_char",
            kind=SymbolKind.TYPE,
            ada_type=c_schar_type,
            scope_level=0,
        )

        # C.size_t (16-bit on Z80)
        c_size_t_type = ModularType(name="size_t", modulus=65536)
        c_pkg.public_symbols["size_t"] = Symbol(
            name="size_t",
            kind=SymbolKind.TYPE,
            ada_type=c_size_t_type,
            scope_level=0,
        )

        # C.ptrdiff_t
        c_ptrdiff_type = IntegerType(name="ptrdiff_t", low=-32768, high=32767)
        c_pkg.public_symbols["ptrdiff_t"] = Symbol(
            name="ptrdiff_t",
            kind=SymbolKind.TYPE,
            ada_type=c_ptrdiff_type,
            scope_level=0,
        )

        # C.nul constant
        c_nul_sym = Symbol(
            name="nul",
            kind=SymbolKind.VARIABLE,
            ada_type=c_char_type,
            is_constant=True,
            value=0,
            scope_level=0,
        )
        c_pkg.public_symbols["nul"] = c_nul_sym

        interfaces_pkg.public_symbols["c"] = c_pkg

        # =====================================================================
        # Interfaces.C.Strings - C string handling
        # =====================================================================
        c_strings_pkg = Symbol(
            name="Strings",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        str_type = PREDEFINED_TYPES["String"]

        # chars_ptr type (pointer to C string)
        from uada80.type_system import AccessType, ArrayType
        char_array_type = ArrayType(
            name="char_array",
            component_type=c_char_type,
            index_types=[c_size_t_type],
            is_constrained=False,
        )
        chars_ptr_type = AccessType(name="chars_ptr", designated_type=char_array_type)
        c_strings_pkg.public_symbols["chars_ptr"] = Symbol(
            name="chars_ptr",
            kind=SymbolKind.TYPE,
            ada_type=chars_ptr_type,
            scope_level=0,
        )

        # Null_Ptr constant
        null_ptr_sym = Symbol(
            name="Null_Ptr",
            kind=SymbolKind.VARIABLE,
            ada_type=chars_ptr_type,
            is_constant=True,
            value=0,
            scope_level=0,
        )
        c_strings_pkg.public_symbols["null_ptr"] = null_ptr_sym

        # New_String function (Ada String -> chars_ptr)
        new_string_func = Symbol(
            name="New_String",
            kind=SymbolKind.FUNCTION,
            return_type=chars_ptr_type,
            scope_level=0,
            parameters=[
                Symbol("Str", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        new_string_func.runtime_name = "_c_new_string"
        c_strings_pkg.public_symbols["new_string"] = new_string_func

        # Free procedure
        free_proc = Symbol(
            name="Free",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, chars_ptr_type, mode="in out"),
            ],
        )
        free_proc.runtime_name = "_c_free_string"
        c_strings_pkg.public_symbols["free"] = free_proc

        # Value function (chars_ptr -> Ada String)
        value_func = Symbol(
            name="Value",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, chars_ptr_type, mode="in"),
            ],
        )
        value_func.runtime_name = "_c_value"
        c_strings_pkg.public_symbols["value"] = value_func

        # Strlen function
        strlen_func = Symbol(
            name="Strlen",
            kind=SymbolKind.FUNCTION,
            return_type=c_size_t_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, chars_ptr_type, mode="in"),
            ],
        )
        strlen_func.runtime_name = "_c_strlen"
        c_strings_pkg.public_symbols["strlen"] = strlen_func

        c_pkg.public_symbols["strings"] = c_strings_pkg

    def _init_system_packages(self) -> None:
        """Add System package and subpackages."""
        int_type = PREDEFINED_TYPES["Integer"]
        bool_type = PREDEFINED_TYPES["Boolean"]

        # System package should already exist with Address type
        system_pkg = self.lookup("System")
        if system_pkg is None:
            # Create System package if it doesn't exist
            system_pkg = Symbol(
                name="System",
                kind=SymbolKind.PACKAGE,
                scope_level=0,
            )
            self.current_scope.define(system_pkg)

            # Address type
            addr_type = IntegerType(name="Address", low=0, high=0xFFFF)
            system_pkg.public_symbols["address"] = Symbol(
                name="Address",
                kind=SymbolKind.TYPE,
                ada_type=addr_type,
                scope_level=0,
            )

            # Null_Address constant
            null_addr_sym = Symbol(
                name="Null_Address",
                kind=SymbolKind.VARIABLE,
                ada_type=addr_type,
                is_constant=True,
                value=0,
                scope_level=0,
            )
            system_pkg.public_symbols["null_address"] = null_addr_sym

        addr_type = system_pkg.public_symbols.get("address")
        if addr_type:
            addr_type = addr_type.ada_type
        else:
            addr_type = IntegerType(name="Address", low=0, high=0xFFFF)

        # =====================================================================
        # System.Storage_Elements - Address arithmetic
        # =====================================================================
        storage_elem_pkg = Symbol(
            name="Storage_Elements",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Storage_Offset type (signed address offset)
        storage_offset_type = IntegerType(name="Storage_Offset", low=-32768, high=32767)
        storage_elem_pkg.public_symbols["storage_offset"] = Symbol(
            name="Storage_Offset",
            kind=SymbolKind.TYPE,
            ada_type=storage_offset_type,
            scope_level=0,
        )

        # Storage_Count subtype (non-negative offset)
        storage_count_type = IntegerType(name="Storage_Count", low=0, high=32767)
        storage_elem_pkg.public_symbols["storage_count"] = Symbol(
            name="Storage_Count",
            kind=SymbolKind.TYPE,
            ada_type=storage_count_type,
            scope_level=0,
        )

        # Storage_Element type (8-bit byte)
        storage_elem_type = IntegerType(name="Storage_Element", low=0, high=255)
        storage_elem_pkg.public_symbols["storage_element"] = Symbol(
            name="Storage_Element",
            kind=SymbolKind.TYPE,
            ada_type=storage_elem_type,
            scope_level=0,
        )

        # Storage_Array type
        from uada80.type_system import ArrayType
        storage_array_type = ArrayType(
            name="Storage_Array",
            component_type=storage_elem_type,
            index_types=[storage_offset_type],
            is_constrained=False,
        )
        storage_elem_pkg.public_symbols["storage_array"] = Symbol(
            name="Storage_Array",
            kind=SymbolKind.TYPE,
            ada_type=storage_array_type,
            scope_level=0,
        )

        # "+" operator (Address + Storage_Offset -> Address)
        addr_plus_func = Symbol(
            name="+",
            kind=SymbolKind.FUNCTION,
            return_type=addr_type,
            scope_level=0,
            parameters=[
                Symbol("Left", SymbolKind.PARAMETER, addr_type, mode="in"),
                Symbol("Right", SymbolKind.PARAMETER, storage_offset_type, mode="in"),
            ],
        )
        addr_plus_func.runtime_name = "_addr_add"
        storage_elem_pkg.public_symbols["+"] = addr_plus_func

        # "-" operator (Address - Storage_Offset -> Address)
        addr_minus_func = Symbol(
            name="-",
            kind=SymbolKind.FUNCTION,
            return_type=addr_type,
            scope_level=0,
            parameters=[
                Symbol("Left", SymbolKind.PARAMETER, addr_type, mode="in"),
                Symbol("Right", SymbolKind.PARAMETER, storage_offset_type, mode="in"),
            ],
        )
        addr_minus_func.runtime_name = "_addr_sub"

        # "-" operator (Address - Address -> Storage_Offset)
        addr_diff_func = Symbol(
            name="-",
            kind=SymbolKind.FUNCTION,
            return_type=storage_offset_type,
            scope_level=0,
            parameters=[
                Symbol("Left", SymbolKind.PARAMETER, addr_type, mode="in"),
                Symbol("Right", SymbolKind.PARAMETER, addr_type, mode="in"),
            ],
            overloaded_next=addr_minus_func,
        )
        addr_diff_func.runtime_name = "_addr_diff"
        storage_elem_pkg.public_symbols["-"] = addr_diff_func

        # "mod" operator (Address mod Storage_Offset -> Storage_Offset)
        addr_mod_func = Symbol(
            name="mod",
            kind=SymbolKind.FUNCTION,
            return_type=storage_offset_type,
            scope_level=0,
            parameters=[
                Symbol("Left", SymbolKind.PARAMETER, addr_type, mode="in"),
                Symbol("Right", SymbolKind.PARAMETER, storage_offset_type, mode="in"),
            ],
        )
        addr_mod_func.runtime_name = "_addr_mod"
        storage_elem_pkg.public_symbols["mod"] = addr_mod_func

        # To_Address function (Storage_Offset -> Address)
        to_address_func = Symbol(
            name="To_Address",
            kind=SymbolKind.FUNCTION,
            return_type=addr_type,
            scope_level=0,
            parameters=[
                Symbol("Value", SymbolKind.PARAMETER, storage_offset_type, mode="in"),
            ],
        )
        to_address_func.runtime_name = "_to_address"
        storage_elem_pkg.public_symbols["to_address"] = to_address_func

        # To_Integer function (Address -> Storage_Offset)
        to_integer_func = Symbol(
            name="To_Integer",
            kind=SymbolKind.FUNCTION,
            return_type=storage_offset_type,
            scope_level=0,
            parameters=[
                Symbol("Value", SymbolKind.PARAMETER, addr_type, mode="in"),
            ],
        )
        to_integer_func.runtime_name = "_to_integer"
        storage_elem_pkg.public_symbols["to_integer"] = to_integer_func

        system_pkg.public_symbols["storage_elements"] = storage_elem_pkg

        # =====================================================================
        # System.Address_To_Access_Conversions - Generic address/access conversion
        # =====================================================================
        addr_conv_pkg = Symbol(
            name="Address_To_Access_Conversions",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        addr_conv_pkg.is_builtin_generic = True

        system_pkg.public_symbols["address_to_access_conversions"] = addr_conv_pkg

        # =====================================================================
        # System constants for Z80/CP/M target
        # =====================================================================
        # Storage_Unit (bits per storage element)
        system_pkg.public_symbols["storage_unit"] = Symbol(
            name="Storage_Unit",
            kind=SymbolKind.VARIABLE,
            ada_type=int_type,
            is_constant=True,
            value=8,
            scope_level=0,
        )

        # Word_Size (bits in a machine word)
        system_pkg.public_symbols["word_size"] = Symbol(
            name="Word_Size",
            kind=SymbolKind.VARIABLE,
            ada_type=int_type,
            is_constant=True,
            value=16,  # Z80 is 16-bit
            scope_level=0,
        )

        # Memory_Size (max address + 1)
        system_pkg.public_symbols["memory_size"] = Symbol(
            name="Memory_Size",
            kind=SymbolKind.VARIABLE,
            ada_type=int_type,
            is_constant=True,
            value=65536,  # 64KB address space
            scope_level=0,
        )

        # Max_Int
        system_pkg.public_symbols["max_int"] = Symbol(
            name="Max_Int",
            kind=SymbolKind.VARIABLE,
            ada_type=int_type,
            is_constant=True,
            value=32767,  # 16-bit signed max
            scope_level=0,
        )

        # Min_Int
        system_pkg.public_symbols["min_int"] = Symbol(
            name="Min_Int",
            kind=SymbolKind.VARIABLE,
            ada_type=int_type,
            is_constant=True,
            value=-32768,  # 16-bit signed min
            scope_level=0,
        )

        # Default_Bit_Order
        from uada80.type_system import EnumerationType
        bit_order_type = EnumerationType(
            name="Bit_Order",
            literals=["High_Order_First", "Low_Order_First"],
        )
        system_pkg.public_symbols["bit_order"] = Symbol(
            name="Bit_Order",
            kind=SymbolKind.TYPE,
            ada_type=bit_order_type,
            scope_level=0,
        )
        system_pkg.public_symbols["default_bit_order"] = Symbol(
            name="Default_Bit_Order",
            kind=SymbolKind.VARIABLE,
            ada_type=bit_order_type,
            is_constant=True,
            value=1,  # Low_Order_First (little-endian)
            scope_level=0,
        )

        # System_Name
        str_type = PREDEFINED_TYPES["String"]
        system_pkg.public_symbols["system_name"] = Symbol(
            name="System_Name",
            kind=SymbolKind.VARIABLE,
            ada_type=str_type,
            is_constant=True,
            scope_level=0,
        )

    def _init_impdef(self) -> None:
        """
        Initialize ImpDef package for ACATS test suite support.

        ImpDef provides implementation-defined constants and values
        required by ACATS conformance tests. Values are customized
        for the Z80/CP/M target.
        """
        int_type = PREDEFINED_TYPES["Integer"]
        float_type = PREDEFINED_TYPES["Float"]
        str_type = PREDEFINED_TYPES["String"]
        bool_type = PREDEFINED_TYPES["Boolean"]
        duration_type = PREDEFINED_TYPES["Duration"]

        # Create ImpDef package
        impdef_pkg = Symbol(
            name="ImpDef",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # =====================================================================
        # Annex Validation Flags
        # These indicate which Specialized Needs Annexes are supported
        # =====================================================================

        # Annex C - Systems Programming
        impdef_pkg.public_symbols["validating_annex_c"] = Symbol(
            name="Validating_Annex_C",
            kind=SymbolKind.CONSTANT,
            ada_type=bool_type,
            is_constant=True,
            value=True,  # We support Systems Programming features
            scope_level=0,
        )

        # Annex D - Real-Time Systems (limited on Z80)
        impdef_pkg.public_symbols["validating_annex_d"] = Symbol(
            name="Validating_Annex_D",
            kind=SymbolKind.CONSTANT,
            ada_type=bool_type,
            is_constant=True,
            value=False,  # Real-time not fully supported on Z80
            scope_level=0,
        )

        # Annex E - Distributed Systems
        impdef_pkg.public_symbols["validating_annex_e"] = Symbol(
            name="Validating_Annex_E",
            kind=SymbolKind.CONSTANT,
            ada_type=bool_type,
            is_constant=True,
            value=False,  # Not applicable to single-CPU Z80
            scope_level=0,
        )

        # Annex F - Information Systems (COBOL/Decimal)
        impdef_pkg.public_symbols["validating_annex_f"] = Symbol(
            name="Validating_Annex_F",
            kind=SymbolKind.CONSTANT,
            ada_type=bool_type,
            is_constant=True,
            value=False,  # Decimal types not implemented
            scope_level=0,
        )

        # Annex G - Numerics
        impdef_pkg.public_symbols["validating_annex_g"] = Symbol(
            name="Validating_Annex_G",
            kind=SymbolKind.CONSTANT,
            ada_type=bool_type,
            is_constant=True,
            value=True,  # Numerics supported via 48-bit float
            scope_level=0,
        )

        # Annex H - Safety and Security
        impdef_pkg.public_symbols["validating_annex_h"] = Symbol(
            name="Validating_Annex_H",
            kind=SymbolKind.CONSTANT,
            ada_type=bool_type,
            is_constant=True,
            value=False,  # Safety features not implemented
            scope_level=0,
        )

        # =====================================================================
        # Timing Constants for Task Tests
        # Values adjusted for Z80 clock speeds (typically 2-8 MHz)
        # =====================================================================

        # Minimum time needed for a task switch (in seconds)
        impdef_pkg.public_symbols["minimum_task_switch"] = Symbol(
            name="Minimum_Task_Switch",
            kind=SymbolKind.CONSTANT,
            ada_type=duration_type,
            is_constant=True,
            value=0.1,  # 100ms - Z80 is slow
            scope_level=0,
        )

        # Time to wait for a new task to start executing
        impdef_pkg.public_symbols["switch_to_new_task"] = Symbol(
            name="Switch_To_New_Task",
            kind=SymbolKind.CONSTANT,
            ada_type=duration_type,
            is_constant=True,
            value=0.1,  # 100ms
            scope_level=0,
        )

        # Time for the ready queue to empty
        impdef_pkg.public_symbols["clear_ready_queue"] = Symbol(
            name="Clear_Ready_Queue",
            kind=SymbolKind.CONSTANT,
            ada_type=duration_type,
            is_constant=True,
            value=1.0,  # 1 second
            scope_level=0,
        )

        # Time for a delay that should already have passed
        impdef_pkg.public_symbols["delay_for_time_past"] = Symbol(
            name="Delay_For_Time_Past",
            kind=SymbolKind.CONSTANT,
            ada_type=duration_type,
            is_constant=True,
            value=0.01,  # 10ms
            scope_level=0,
        )

        # Time for time-dependent resets
        impdef_pkg.public_symbols["time_dependent_reset"] = Symbol(
            name="Time_Dependent_Reset",
            kind=SymbolKind.CONSTANT,
            ada_type=duration_type,
            is_constant=True,
            value=0.01,  # 10ms
            scope_level=0,
        )

        # Delay for random number tests
        impdef_pkg.public_symbols["delay_per_random_test"] = Symbol(
            name="Delay_Per_Random_Test",
            kind=SymbolKind.CONSTANT,
            ada_type=duration_type,
            is_constant=True,
            value=0.01,  # 10ms
            scope_level=0,
        )

        # =====================================================================
        # String Constants
        # =====================================================================

        # Non-state string (for territory tests)
        impdef_pkg.public_symbols["non_state_string"] = Symbol(
            name="Non_State_String",
            kind=SymbolKind.CONSTANT,
            ada_type=str_type,
            is_constant=True,
            value="Not A State",
            scope_level=0,
        )

        # External tag value for tagged types
        impdef_pkg.public_symbols["external_tag_value"] = Symbol(
            name="External_Tag_Value",
            kind=SymbolKind.CONSTANT,
            ada_type=str_type,
            is_constant=True,
            value="uada80_tag",
            scope_level=0,
        )

        # =====================================================================
        # File System Constants (CP/M specific)
        # =====================================================================

        # Directory name to create for tests
        impdef_pkg.public_symbols["directory_to_create"] = Symbol(
            name="Directory_To_Create",
            kind=SymbolKind.CONSTANT,
            ada_type=str_type,
            is_constant=True,
            value="A:TESTDIR",  # CP/M style
            scope_level=0,
        )

        # Parent directory name
        impdef_pkg.public_symbols["parent_directory_name"] = Symbol(
            name="Parent_Directory_Name",
            kind=SymbolKind.CONSTANT,
            ada_type=str_type,
            is_constant=True,
            value="",  # CP/M doesn't have directories
            scope_level=0,
        )

        # Current directory name
        impdef_pkg.public_symbols["current_directory_name"] = Symbol(
            name="Current_Directory_Name",
            kind=SymbolKind.CONSTANT,
            ada_type=str_type,
            is_constant=True,
            value="",  # CP/M doesn't have directories
            scope_level=0,
        )

        # =====================================================================
        # Alignment and Storage Constants
        # =====================================================================

        # Maximum default alignment
        impdef_pkg.public_symbols["max_default_alignment"] = Symbol(
            name="Max_Default_Alignment",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=2,  # Word alignment on Z80
            scope_level=0,
        )

        # Maximum alignment achievable by linker
        impdef_pkg.public_symbols["max_linker_alignment"] = Symbol(
            name="Max_Linker_Alignment",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=256,  # Page alignment possible
            scope_level=0,
        )

        # Maximum adjustment to specified storage size
        impdef_pkg.public_symbols["maximum_adjustment_to_specified_storage_size"] = Symbol(
            name="Maximum_Adjustment_To_Specified_Storage_Size",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=256,  # Overhead for task control block
            scope_level=0,
        )

        # Bits per character
        impdef_pkg.public_symbols["char_bits"] = Symbol(
            name="Char_Bits",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=8,
            scope_level=0,
        )

        # Next storage slot after a component
        impdef_pkg.public_symbols["next_storage_slot"] = Symbol(
            name="Next_Storage_Slot",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=2,  # Word on Z80
            scope_level=0,
        )

        # =====================================================================
        # Type Support Flags
        # =====================================================================

        # Non-binary fixed-point support
        impdef_pkg.public_symbols["non_binary_fixed_supported"] = Symbol(
            name="Non_Binary_Fixed_Supported",
            kind=SymbolKind.CONSTANT,
            ada_type=bool_type,
            is_constant=True,
            value=True,  # We support arbitrary 'Small values
            scope_level=0,
        )

        # Long integer support (32-bit)
        impdef_pkg.public_symbols["long_integer_supported"] = Symbol(
            name="Long_Integer_Supported",
            kind=SymbolKind.CONSTANT,
            ada_type=bool_type,
            is_constant=True,
            value=True,  # 32-bit integers available
            scope_level=0,
        )

        # =====================================================================
        # Procedure Exceed_Time_Slice
        # This procedure should consume at least one time slice
        # =====================================================================
        exceed_time_slice = Symbol(
            name="Exceed_Time_Slice",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[],
        )
        exceed_time_slice.runtime_name = "_impdef_exceed_time_slice"
        impdef_pkg.public_symbols["exceed_time_slice"] = exceed_time_slice

        # =====================================================================
        # Function Equivalent_File_Names
        # Tests if two file names refer to the same file
        # =====================================================================
        equivalent_file_names = Symbol(
            name="Equivalent_File_Names",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Name_1", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Name_2", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        equivalent_file_names.runtime_name = "_impdef_equiv_files"
        impdef_pkg.public_symbols["equivalent_file_names"] = equivalent_file_names

        # Register the ImpDef package at global scope
        self.current_scope.define(impdef_pkg)

    def _init_spprt13(self) -> None:
        """
        Initialize SPPRT13 package for ACATS Chapter 13 test support.

        SPPRT13 provides address constants used by Chapter 13 tests
        (representation clauses). These are Z80/CP/M specific addresses.
        """
        # Get System.Address type
        system_pkg = self.lookup("System")
        if system_pkg is None:
            return

        addr_type = system_pkg.public_symbols.get("address")
        if addr_type is None:
            return
        addr_ada_type = addr_type.ada_type

        # Create SPPRT13 package
        spprt13_pkg = Symbol(
            name="SPPRT13",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # =====================================================================
        # Variable Address Constants
        # These are valid addresses for variables in the Z80 address space
        # We use addresses in the high TPA area (below BDOS)
        # =====================================================================

        # Variable_Address - primary variable address constant
        spprt13_pkg.public_symbols["variable_address"] = Symbol(
            name="Variable_Address",
            kind=SymbolKind.CONSTANT,
            ada_type=addr_ada_type,
            is_constant=True,
            value=0x8000,  # 32768 - middle of RAM
            scope_level=0,
        )

        # Variable_Address1 - second variable address
        spprt13_pkg.public_symbols["variable_address1"] = Symbol(
            name="Variable_Address1",
            kind=SymbolKind.CONSTANT,
            ada_type=addr_ada_type,
            is_constant=True,
            value=0x8100,  # 33024
            scope_level=0,
        )

        # Variable_Address2 - third variable address
        spprt13_pkg.public_symbols["variable_address2"] = Symbol(
            name="Variable_Address2",
            kind=SymbolKind.CONSTANT,
            ada_type=addr_ada_type,
            is_constant=True,
            value=0x8200,  # 33280
            scope_level=0,
        )

        # =====================================================================
        # Entry Address Constants (Interrupt Vectors)
        # On Z80, these would be interrupt mode 2 vectors
        # For CP/M, we use low memory addresses that would be valid
        # =====================================================================

        # Entry_Address - primary entry/interrupt address
        spprt13_pkg.public_symbols["entry_address"] = Symbol(
            name="Entry_Address",
            kind=SymbolKind.CONSTANT,
            ada_type=addr_ada_type,
            is_constant=True,
            value=0x0038,  # RST 38h - mode 1 interrupt
            scope_level=0,
        )

        # Entry_Address1 - second entry address
        spprt13_pkg.public_symbols["entry_address1"] = Symbol(
            name="Entry_Address1",
            kind=SymbolKind.CONSTANT,
            ada_type=addr_ada_type,
            is_constant=True,
            value=0x0066,  # NMI vector
            scope_level=0,
        )

        # Entry_Address2 - third entry address
        spprt13_pkg.public_symbols["entry_address2"] = Symbol(
            name="Entry_Address2",
            kind=SymbolKind.CONSTANT,
            ada_type=addr_ada_type,
            is_constant=True,
            value=0x0100,  # TPA start (after warm boot)
            scope_level=0,
        )

        # Register the SPPRT13 package at global scope
        self.current_scope.define(spprt13_pkg)

    def _init_assertions(self) -> None:
        """
        Initialize Ada.Assertions package (Ada 2005).

        Provides Assert procedures and Assertion_Error exception
        for runtime assertion checking.
        """
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        bool_type = PREDEFINED_TYPES["Boolean"]
        str_type = PREDEFINED_TYPES["String"]

        # Create Assertions subpackage
        assertions_pkg = Symbol(
            name="Assertions",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Assertion_Error exception
        assertions_pkg.public_symbols["assertion_error"] = Symbol(
            name="Assertion_Error",
            kind=SymbolKind.EXCEPTION,
            scope_level=0,
        )

        # Assert procedure (Check : Boolean)
        assert_proc1 = Symbol(
            name="Assert",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Check", SymbolKind.PARAMETER, bool_type, mode="in"),
            ],
        )
        assert_proc1.runtime_name = "_ada_assert"
        assertions_pkg.public_symbols["assert"] = assert_proc1

        # Assert procedure (Check : Boolean; Message : String)
        assert_proc2 = Symbol(
            name="Assert",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Check", SymbolKind.PARAMETER, bool_type, mode="in"),
                Symbol("Message", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        assert_proc2.runtime_name = "_ada_assert_msg"
        assert_proc1.overloaded_next = assert_proc2

        ada_pkg.public_symbols["assertions"] = assertions_pkg

    def _init_synchronous_task_control(self) -> None:
        """
        Initialize Ada.Synchronous_Task_Control package (Ada 95 Annex D).

        Provides Suspension_Object type and operations for task synchronization.
        """
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        bool_type = PREDEFINED_TYPES["Boolean"]

        # Create Synchronous_Task_Control subpackage
        stc_pkg = Symbol(
            name="Synchronous_Task_Control",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Suspension_Object type (limited private)
        from uada80.type_system import RecordType
        suspension_obj_type = RecordType(
            name="Suspension_Object",
            components=[],
            is_limited=True,
        )
        stc_pkg.public_symbols["suspension_object"] = Symbol(
            name="Suspension_Object",
            kind=SymbolKind.TYPE,
            ada_type=suspension_obj_type,
            scope_level=0,
        )

        # Set_True procedure
        set_true_proc = Symbol(
            name="Set_True",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("S", SymbolKind.PARAMETER, suspension_obj_type, mode="in out"),
            ],
        )
        set_true_proc.runtime_name = "_stc_set_true"
        stc_pkg.public_symbols["set_true"] = set_true_proc

        # Set_False procedure
        set_false_proc = Symbol(
            name="Set_False",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("S", SymbolKind.PARAMETER, suspension_obj_type, mode="in out"),
            ],
        )
        set_false_proc.runtime_name = "_stc_set_false"
        stc_pkg.public_symbols["set_false"] = set_false_proc

        # Current_State function
        current_state_func = Symbol(
            name="Current_State",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("S", SymbolKind.PARAMETER, suspension_obj_type, mode="in"),
            ],
        )
        current_state_func.runtime_name = "_stc_current_state"
        stc_pkg.public_symbols["current_state"] = current_state_func

        # Suspend_Until_True procedure
        suspend_proc = Symbol(
            name="Suspend_Until_True",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("S", SymbolKind.PARAMETER, suspension_obj_type, mode="in out"),
            ],
        )
        suspend_proc.runtime_name = "_stc_suspend"
        stc_pkg.public_symbols["suspend_until_true"] = suspend_proc

        ada_pkg.public_symbols["synchronous_task_control"] = stc_pkg

    def _init_wide_characters(self) -> None:
        """
        Initialize Ada.Wide_Characters packages (Ada 2005).

        Provides wide character handling analogous to Ada.Characters.
        """
        ada_pkg = self.lookup("Ada")
        if ada_pkg is None:
            return

        wide_char_type = PREDEFINED_TYPES["Wide_Character"]
        bool_type = PREDEFINED_TYPES["Boolean"]
        char_type = PREDEFINED_TYPES["Character"]

        # Create Wide_Characters subpackage
        wide_chars_pkg = Symbol(
            name="Wide_Characters",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Create Handling child package
        handling_pkg = Symbol(
            name="Handling",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Character classification functions
        classification_funcs = [
            ("Is_Control", "Is Wide_Character a control character"),
            ("Is_Graphic", "Is Wide_Character a graphic character"),
            ("Is_Letter", "Is Wide_Character a letter"),
            ("Is_Lower", "Is Wide_Character a lowercase letter"),
            ("Is_Upper", "Is Wide_Character an uppercase letter"),
            ("Is_Digit", "Is Wide_Character a decimal digit"),
            ("Is_Hexadecimal_Digit", "Is Wide_Character a hexadecimal digit"),
            ("Is_Alphanumeric", "Is Wide_Character a letter or digit"),
            ("Is_Special", "Is Wide_Character a special graphic character"),
            ("Is_Line_Terminator", "Is Wide_Character a line terminator"),
            ("Is_Mark", "Is Wide_Character a mark (diacritic)"),
            ("Is_Other_Format", "Is Wide_Character an other format character"),
            ("Is_Punctuation_Connector", "Is Wide_Character a punctuation connector"),
            ("Is_Space", "Is Wide_Character a space"),
        ]

        for func_name, _ in classification_funcs:
            func = Symbol(
                name=func_name,
                kind=SymbolKind.FUNCTION,
                return_type=bool_type,
                scope_level=0,
                parameters=[
                    Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in"),
                ],
            )
            func.runtime_name = f"_wide_{func_name.lower()}"
            handling_pkg.public_symbols[func_name.lower()] = func

        # Case conversion functions
        to_lower_func = Symbol(
            name="To_Lower",
            kind=SymbolKind.FUNCTION,
            return_type=wide_char_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in"),
            ],
        )
        to_lower_func.runtime_name = "_wide_to_lower"
        handling_pkg.public_symbols["to_lower"] = to_lower_func

        to_upper_func = Symbol(
            name="To_Upper",
            kind=SymbolKind.FUNCTION,
            return_type=wide_char_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in"),
            ],
        )
        to_upper_func.runtime_name = "_wide_to_upper"
        handling_pkg.public_symbols["to_upper"] = to_upper_func

        # Is_Basic function
        is_basic_func = Symbol(
            name="Is_Basic",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in"),
            ],
        )
        is_basic_func.runtime_name = "_wide_is_basic"
        handling_pkg.public_symbols["is_basic"] = is_basic_func

        # To_Basic function
        to_basic_func = Symbol(
            name="To_Basic",
            kind=SymbolKind.FUNCTION,
            return_type=wide_char_type,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in"),
            ],
        )
        to_basic_func.runtime_name = "_wide_to_basic"
        handling_pkg.public_symbols["to_basic"] = to_basic_func

        wide_chars_pkg.public_symbols["handling"] = handling_pkg
        ada_pkg.public_symbols["wide_characters"] = wide_chars_pkg

    def _init_gnat_packages(self) -> None:
        """
        Initialize GNAT-specific packages.

        GNAT provides a rich set of utility packages beyond the standard
        Ada library. These are commonly used in real-world Ada programs.
        """
        int_type = PREDEFINED_TYPES["Integer"]
        nat_type = PREDEFINED_TYPES["Natural"]
        pos_type = PREDEFINED_TYPES["Positive"]
        bool_type = PREDEFINED_TYPES["Boolean"]
        char_type = PREDEFINED_TYPES["Character"]
        str_type = PREDEFINED_TYPES["String"]
        float_type = PREDEFINED_TYPES["Float"]

        # Create GNAT root package
        gnat_pkg = Symbol(
            name="GNAT",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # =====================================================================
        # GNAT.IO - Simple I/O package (preelaborated, lighter than Text_IO)
        # =====================================================================
        io_pkg = Symbol(
            name="IO",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )
        io_pkg.is_preelaborate = True

        # Put procedures
        put_char = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("C", SymbolKind.PARAMETER, char_type, mode="in")],
        )
        put_char.runtime_name = "_gnat_io_put_char"
        io_pkg.public_symbols["put"] = put_char

        put_str = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("S", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        put_str.runtime_name = "_gnat_io_put_str"
        put_char.overloaded_next = put_str

        put_int = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("I", SymbolKind.PARAMETER, int_type, mode="in")],
        )
        put_int.runtime_name = "_gnat_io_put_int"
        put_str.overloaded_next = put_int

        # Put_Line procedures
        put_line_str = Symbol(
            name="Put_Line",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("S", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        put_line_str.runtime_name = "_gnat_io_put_line"
        io_pkg.public_symbols["put_line"] = put_line_str

        # New_Line procedure
        new_line = Symbol(
            name="New_Line",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[],
        )
        new_line.runtime_name = "_gnat_io_new_line"
        io_pkg.public_symbols["new_line"] = new_line

        # Get procedures
        get_char = Symbol(
            name="Get",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("C", SymbolKind.PARAMETER, char_type, mode="out")],
        )
        get_char.runtime_name = "_gnat_io_get_char"
        io_pkg.public_symbols["get"] = get_char

        get_int = Symbol(
            name="Get",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("I", SymbolKind.PARAMETER, int_type, mode="out")],
        )
        get_int.runtime_name = "_gnat_io_get_int"
        get_char.overloaded_next = get_int

        # Get_Line function
        get_line = Symbol(
            name="Get_Line",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        get_line.runtime_name = "_gnat_io_get_line"
        io_pkg.public_symbols["get_line"] = get_line

        gnat_pkg.public_symbols["io"] = io_pkg

        # =====================================================================
        # GNAT.Source_Info - Compile-time source information
        # =====================================================================
        source_info_pkg = Symbol(
            name="Source_Info",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # File function - returns current source file name
        file_func = Symbol(
            name="File",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        file_func.runtime_name = "_gnat_source_file"
        source_info_pkg.public_symbols["file"] = file_func

        # Line function - returns current line number
        line_func = Symbol(
            name="Line",
            kind=SymbolKind.FUNCTION,
            return_type=pos_type,
            scope_level=0,
            parameters=[],
        )
        line_func.runtime_name = "_gnat_source_line"
        source_info_pkg.public_symbols["line"] = line_func

        # Source_Location function - returns "file:line"
        source_loc_func = Symbol(
            name="Source_Location",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        source_loc_func.runtime_name = "_gnat_source_location"
        source_info_pkg.public_symbols["source_location"] = source_loc_func

        # Enclosing_Entity function - returns enclosing subprogram/package name
        enclosing_func = Symbol(
            name="Enclosing_Entity",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        enclosing_func.runtime_name = "_gnat_enclosing_entity"
        source_info_pkg.public_symbols["enclosing_entity"] = enclosing_func

        # Compilation_Date function
        comp_date_func = Symbol(
            name="Compilation_Date",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        comp_date_func.runtime_name = "_gnat_compilation_date"
        source_info_pkg.public_symbols["compilation_date"] = comp_date_func

        # Compilation_Time function
        comp_time_func = Symbol(
            name="Compilation_Time",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        comp_time_func.runtime_name = "_gnat_compilation_time"
        source_info_pkg.public_symbols["compilation_time"] = comp_time_func

        gnat_pkg.public_symbols["source_info"] = source_info_pkg

        # =====================================================================
        # GNAT.Case_Util - Case conversion utilities
        # =====================================================================
        case_util_pkg = Symbol(
            name="Case_Util",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # To_Upper for Character
        to_upper_char = Symbol(
            name="To_Upper",
            kind=SymbolKind.FUNCTION,
            return_type=char_type,
            scope_level=0,
            parameters=[Symbol("A", SymbolKind.PARAMETER, char_type, mode="in")],
        )
        to_upper_char.runtime_name = "_gnat_to_upper_char"
        case_util_pkg.public_symbols["to_upper"] = to_upper_char

        # To_Lower for Character
        to_lower_char = Symbol(
            name="To_Lower",
            kind=SymbolKind.FUNCTION,
            return_type=char_type,
            scope_level=0,
            parameters=[Symbol("A", SymbolKind.PARAMETER, char_type, mode="in")],
        )
        to_lower_char.runtime_name = "_gnat_to_lower_char"
        case_util_pkg.public_symbols["to_lower"] = to_lower_char

        # To_Mixed for String (in out parameter)
        to_mixed_str = Symbol(
            name="To_Mixed",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("A", SymbolKind.PARAMETER, str_type, mode="in out")],
        )
        to_mixed_str.runtime_name = "_gnat_to_mixed"
        case_util_pkg.public_symbols["to_mixed"] = to_mixed_str

        gnat_pkg.public_symbols["case_util"] = case_util_pkg

        # =====================================================================
        # GNAT.CRC32 - CRC-32 checksum computation
        # =====================================================================
        from uada80.type_system import ModularType
        crc32_type = ModularType(name="CRC32", modulus=2**32)

        crc32_pkg = Symbol(
            name="CRC32",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # CRC32 type
        crc32_pkg.public_symbols["crc32"] = Symbol(
            name="CRC32",
            kind=SymbolKind.TYPE,
            ada_type=crc32_type,
            scope_level=0,
        )

        # Initialize procedure
        init_crc = Symbol(
            name="Initialize",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("C", SymbolKind.PARAMETER, crc32_type, mode="out")],
        )
        init_crc.runtime_name = "_gnat_crc32_init"
        crc32_pkg.public_symbols["initialize"] = init_crc

        # Update procedure (single character)
        update_crc_char = Symbol(
            name="Update",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("C", SymbolKind.PARAMETER, crc32_type, mode="in out"),
                Symbol("Value", SymbolKind.PARAMETER, char_type, mode="in"),
            ],
        )
        update_crc_char.runtime_name = "_gnat_crc32_update_char"
        crc32_pkg.public_symbols["update"] = update_crc_char

        # Update procedure (string)
        update_crc_str = Symbol(
            name="Update",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("C", SymbolKind.PARAMETER, crc32_type, mode="in out"),
                Symbol("Value", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        update_crc_str.runtime_name = "_gnat_crc32_update_str"
        update_crc_char.overloaded_next = update_crc_str

        # Get_Value function
        get_value_crc = Symbol(
            name="Get_Value",
            kind=SymbolKind.FUNCTION,
            return_type=crc32_type,
            scope_level=0,
            parameters=[Symbol("C", SymbolKind.PARAMETER, crc32_type, mode="in")],
        )
        get_value_crc.runtime_name = "_gnat_crc32_get_value"
        crc32_pkg.public_symbols["get_value"] = get_value_crc

        gnat_pkg.public_symbols["crc32"] = crc32_pkg

        # =====================================================================
        # GNAT.Heap_Sort - Generic heap sort algorithm
        # =====================================================================
        heap_sort_pkg = Symbol(
            name="Heap_Sort",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        heap_sort_pkg.is_builtin_generic = True
        gnat_pkg.public_symbols["heap_sort"] = heap_sort_pkg

        # GNAT.Heap_Sort_G - Simpler generic heap sort
        heap_sort_g_pkg = Symbol(
            name="Heap_Sort_G",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        heap_sort_g_pkg.is_builtin_generic = True
        gnat_pkg.public_symbols["heap_sort_g"] = heap_sort_g_pkg

        # =====================================================================
        # GNAT.Bubble_Sort - Generic bubble sort algorithm
        # =====================================================================
        bubble_sort_pkg = Symbol(
            name="Bubble_Sort",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        bubble_sort_pkg.is_builtin_generic = True
        gnat_pkg.public_symbols["bubble_sort"] = bubble_sort_pkg

        # GNAT.Bubble_Sort_G - Simpler generic bubble sort
        bubble_sort_g_pkg = Symbol(
            name="Bubble_Sort_G",
            kind=SymbolKind.GENERIC_PACKAGE,
            scope_level=0,
        )
        bubble_sort_g_pkg.is_builtin_generic = True
        gnat_pkg.public_symbols["bubble_sort_g"] = bubble_sort_g_pkg

        # =====================================================================
        # GNAT.String_Split - String tokenization
        # =====================================================================
        string_split_pkg = Symbol(
            name="String_Split",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Slice_Set type (limited private)
        from uada80.type_system import RecordType
        slice_set_type = RecordType(
            name="Slice_Set",
            components=[],
            is_limited=True,
        )
        string_split_pkg.public_symbols["slice_set"] = Symbol(
            name="Slice_Set",
            kind=SymbolKind.TYPE,
            ada_type=slice_set_type,
            scope_level=0,
        )

        # Slice_Number subtype
        string_split_pkg.public_symbols["slice_number"] = Symbol(
            name="Slice_Number",
            kind=SymbolKind.SUBTYPE,
            ada_type=nat_type,
            scope_level=0,
        )

        # Separators_Mode enumeration
        from uada80.type_system import EnumerationType
        sep_mode_type = EnumerationType(
            name="Separators_Mode",
            literals=["Single", "Multiple"],
        )
        string_split_pkg.public_symbols["separators_mode"] = Symbol(
            name="Separators_Mode",
            kind=SymbolKind.TYPE,
            ada_type=sep_mode_type,
            scope_level=0,
        )

        # Create procedure
        create_proc = Symbol(
            name="Create",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("S", SymbolKind.PARAMETER, slice_set_type, mode="out"),
                Symbol("From", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Separators", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Mode", SymbolKind.PARAMETER, sep_mode_type, mode="in"),
            ],
        )
        create_proc.runtime_name = "_gnat_string_split_create"
        string_split_pkg.public_symbols["create"] = create_proc

        # Slice_Count function
        slice_count_func = Symbol(
            name="Slice_Count",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[Symbol("S", SymbolKind.PARAMETER, slice_set_type, mode="in")],
        )
        slice_count_func.runtime_name = "_gnat_string_split_count"
        string_split_pkg.public_symbols["slice_count"] = slice_count_func

        # Slice function
        slice_func = Symbol(
            name="Slice",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[
                Symbol("S", SymbolKind.PARAMETER, slice_set_type, mode="in"),
                Symbol("Index", SymbolKind.PARAMETER, nat_type, mode="in"),
            ],
        )
        slice_func.runtime_name = "_gnat_string_split_slice"
        string_split_pkg.public_symbols["slice"] = slice_func

        gnat_pkg.public_symbols["string_split"] = string_split_pkg

        # =====================================================================
        # GNAT.OS_Lib - Operating system interface
        # =====================================================================
        os_lib_pkg = Symbol(
            name="OS_Lib",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # File_Descriptor type (Integer)
        os_lib_pkg.public_symbols["file_descriptor"] = Symbol(
            name="File_Descriptor",
            kind=SymbolKind.SUBTYPE,
            ada_type=int_type,
            scope_level=0,
        )

        # Standard file descriptors
        os_lib_pkg.public_symbols["standin"] = Symbol(
            name="Standin",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=0,
            scope_level=0,
        )
        os_lib_pkg.public_symbols["standout"] = Symbol(
            name="Standout",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=1,
            scope_level=0,
        )
        os_lib_pkg.public_symbols["standerr"] = Symbol(
            name="Standerr",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=2,
            scope_level=0,
        )

        # Invalid_FD constant
        os_lib_pkg.public_symbols["invalid_fd"] = Symbol(
            name="Invalid_FD",
            kind=SymbolKind.CONSTANT,
            ada_type=int_type,
            is_constant=True,
            value=-1,
            scope_level=0,
        )

        # Getenv function
        getenv_func = Symbol(
            name="Getenv",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        getenv_func.runtime_name = "_gnat_getenv"
        os_lib_pkg.public_symbols["getenv"] = getenv_func

        # Setenv procedure
        setenv_proc = Symbol(
            name="Setenv",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Value", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        setenv_proc.runtime_name = "_gnat_setenv"
        os_lib_pkg.public_symbols["setenv"] = setenv_proc

        # Is_Regular_File function
        is_regular_file_func = Symbol(
            name="Is_Regular_File",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        is_regular_file_func.runtime_name = "_gnat_is_regular_file"
        os_lib_pkg.public_symbols["is_regular_file"] = is_regular_file_func

        # Is_Directory function
        is_directory_func = Symbol(
            name="Is_Directory",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        is_directory_func.runtime_name = "_gnat_is_directory"
        os_lib_pkg.public_symbols["is_directory"] = is_directory_func

        # File_Time_Stamp function
        file_time_func = Symbol(
            name="File_Time_Stamp",
            kind=SymbolKind.FUNCTION,
            return_type=int_type,  # OS_Time
            scope_level=0,
            parameters=[Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        file_time_func.runtime_name = "_gnat_file_time_stamp"
        os_lib_pkg.public_symbols["file_time_stamp"] = file_time_func

        # Delete_File procedure
        delete_file_proc = Symbol(
            name="Delete_File",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Success", SymbolKind.PARAMETER, bool_type, mode="out"),
            ],
        )
        delete_file_proc.runtime_name = "_gnat_delete_file"
        os_lib_pkg.public_symbols["delete_file"] = delete_file_proc

        # Rename_File procedure
        rename_file_proc = Symbol(
            name="Rename_File",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Old_Name", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("New_Name", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Success", SymbolKind.PARAMETER, bool_type, mode="out"),
            ],
        )
        rename_file_proc.runtime_name = "_gnat_rename_file"
        os_lib_pkg.public_symbols["rename_file"] = rename_file_proc

        # OS_Exit procedure
        os_exit_proc = Symbol(
            name="OS_Exit",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Status", SymbolKind.PARAMETER, int_type, mode="in")],
        )
        os_exit_proc.runtime_name = "_gnat_os_exit"
        os_exit_proc.is_no_return = True
        os_lib_pkg.public_symbols["os_exit"] = os_exit_proc

        gnat_pkg.public_symbols["os_lib"] = os_lib_pkg

        # =====================================================================
        # GNAT.Calendar - Extended calendar operations
        # =====================================================================
        gnat_calendar_pkg = Symbol(
            name="Calendar",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Day_Of_Week type
        day_of_week_type = EnumerationType(
            name="Day_Of_Week_Type",
            literals=["Sunday", "Monday", "Tuesday", "Wednesday",
                     "Thursday", "Friday", "Saturday"],
        )
        gnat_calendar_pkg.public_symbols["day_of_week_type"] = Symbol(
            name="Day_Of_Week_Type",
            kind=SymbolKind.TYPE,
            ada_type=day_of_week_type,
            scope_level=0,
        )

        # Get Ada.Calendar.Time type
        ada_pkg = self.lookup("Ada")
        calendar_pkg = ada_pkg.public_symbols.get("calendar") if ada_pkg else None
        if calendar_pkg:
            time_sym = calendar_pkg.public_symbols.get("time")
            time_type = time_sym.ada_type if time_sym else None
        else:
            time_type = float_type  # fallback

        # Day_Of_Week function
        day_of_week_func = Symbol(
            name="Day_Of_Week",
            kind=SymbolKind.FUNCTION,
            return_type=day_of_week_type,
            scope_level=0,
            parameters=[Symbol("Date", SymbolKind.PARAMETER, time_type, mode="in")],
        )
        day_of_week_func.runtime_name = "_gnat_day_of_week"
        gnat_calendar_pkg.public_symbols["day_of_week"] = day_of_week_func

        # Hour function
        hour_func = Symbol(
            name="Hour",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[Symbol("Date", SymbolKind.PARAMETER, time_type, mode="in")],
        )
        hour_func.runtime_name = "_gnat_hour"
        gnat_calendar_pkg.public_symbols["hour"] = hour_func

        # Minute function
        minute_func = Symbol(
            name="Minute",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[Symbol("Date", SymbolKind.PARAMETER, time_type, mode="in")],
        )
        minute_func.runtime_name = "_gnat_minute"
        gnat_calendar_pkg.public_symbols["minute"] = minute_func

        # Second function
        second_func = Symbol(
            name="Second",
            kind=SymbolKind.FUNCTION,
            return_type=nat_type,
            scope_level=0,
            parameters=[Symbol("Date", SymbolKind.PARAMETER, time_type, mode="in")],
        )
        second_func.runtime_name = "_gnat_second"
        gnat_calendar_pkg.public_symbols["second"] = second_func

        gnat_pkg.public_symbols["calendar"] = gnat_calendar_pkg

        # =====================================================================
        # GNAT.MD5 - MD5 message digest
        # =====================================================================
        md5_pkg = Symbol(
            name="MD5",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Message_Digest type (String of 32 hex characters)
        md5_pkg.public_symbols["message_digest"] = Symbol(
            name="Message_Digest",
            kind=SymbolKind.SUBTYPE,
            ada_type=str_type,
            scope_level=0,
        )

        # Context type (limited private)
        md5_context_type = RecordType(
            name="Context",
            components=[],
            is_limited=True,
        )
        md5_pkg.public_symbols["context"] = Symbol(
            name="Context",
            kind=SymbolKind.TYPE,
            ada_type=md5_context_type,
            scope_level=0,
        )

        # Digest function (simple - hash a string)
        md5_digest_func = Symbol(
            name="Digest",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[Symbol("S", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        md5_digest_func.runtime_name = "_gnat_md5_digest"
        md5_pkg.public_symbols["digest"] = md5_digest_func

        gnat_pkg.public_symbols["md5"] = md5_pkg

        # =====================================================================
        # GNAT.SHA1 - SHA-1 secure hash
        # =====================================================================
        sha1_pkg = Symbol(
            name="SHA1",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Context type
        sha1_context_type = RecordType(
            name="Context",
            components=[],
            is_limited=True,
        )
        sha1_pkg.public_symbols["context"] = Symbol(
            name="Context",
            kind=SymbolKind.TYPE,
            ada_type=sha1_context_type,
            scope_level=0,
        )

        # Digest function
        sha1_digest_func = Symbol(
            name="Digest",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[Symbol("S", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        sha1_digest_func.runtime_name = "_gnat_sha1_digest"
        sha1_pkg.public_symbols["digest"] = sha1_digest_func

        gnat_pkg.public_symbols["sha1"] = sha1_pkg

        # =====================================================================
        # GNAT.SHA256 - SHA-256 secure hash
        # =====================================================================
        sha256_pkg = Symbol(
            name="SHA256",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        sha256_context_type = RecordType(
            name="Context",
            components=[],
            is_limited=True,
        )
        sha256_pkg.public_symbols["context"] = Symbol(
            name="Context",
            kind=SymbolKind.TYPE,
            ada_type=sha256_context_type,
            scope_level=0,
        )

        sha256_digest_func = Symbol(
            name="Digest",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[Symbol("S", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        sha256_digest_func.runtime_name = "_gnat_sha256_digest"
        sha256_pkg.public_symbols["digest"] = sha256_digest_func

        gnat_pkg.public_symbols["sha256"] = sha256_pkg

        # =====================================================================
        # GNAT.Regpat - Regular expression pattern matching
        # =====================================================================
        regpat_pkg = Symbol(
            name="Regpat",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Pattern_Matcher type (limited private)
        pattern_matcher_type = RecordType(
            name="Pattern_Matcher",
            components=[],
            is_limited=True,
        )
        regpat_pkg.public_symbols["pattern_matcher"] = Symbol(
            name="Pattern_Matcher",
            kind=SymbolKind.TYPE,
            ada_type=pattern_matcher_type,
            scope_level=0,
        )

        # Match_Location record
        from uada80.type_system import RecordComponent
        match_loc_type = RecordType(
            name="Match_Location",
            components=[
                RecordComponent(name="First", component_type=nat_type),
                RecordComponent(name="Last", component_type=nat_type),
            ],
        )
        regpat_pkg.public_symbols["match_location"] = Symbol(
            name="Match_Location",
            kind=SymbolKind.TYPE,
            ada_type=match_loc_type,
            scope_level=0,
        )

        # No_Match constant
        regpat_pkg.public_symbols["no_match"] = Symbol(
            name="No_Match",
            kind=SymbolKind.CONSTANT,
            ada_type=match_loc_type,
            is_constant=True,
            scope_level=0,
        )

        # Compile function
        compile_func = Symbol(
            name="Compile",
            kind=SymbolKind.FUNCTION,
            return_type=pattern_matcher_type,
            scope_level=0,
            parameters=[Symbol("Expression", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        compile_func.runtime_name = "_gnat_regpat_compile"
        regpat_pkg.public_symbols["compile"] = compile_func

        # Match function
        match_func = Symbol(
            name="Match",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[
                Symbol("Self", SymbolKind.PARAMETER, pattern_matcher_type, mode="in"),
                Symbol("Data", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        match_func.runtime_name = "_gnat_regpat_match"
        regpat_pkg.public_symbols["match"] = match_func

        gnat_pkg.public_symbols["regpat"] = regpat_pkg

        # =====================================================================
        # GNAT.Command_Line - Advanced command line parsing
        # =====================================================================
        cmd_line_pkg = Symbol(
            name="Command_Line",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Define_Switch procedure
        define_switch_proc = Symbol(
            name="Define_Switch",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Switch", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Help", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        define_switch_proc.runtime_name = "_gnat_define_switch"
        cmd_line_pkg.public_symbols["define_switch"] = define_switch_proc

        # Getopt function
        getopt_func = Symbol(
            name="Getopt",
            kind=SymbolKind.FUNCTION,
            return_type=char_type,
            scope_level=0,
            parameters=[Symbol("Switches", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        getopt_func.runtime_name = "_gnat_getopt"
        cmd_line_pkg.public_symbols["getopt"] = getopt_func

        # Full_Switch function
        full_switch_func = Symbol(
            name="Full_Switch",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        full_switch_func.runtime_name = "_gnat_full_switch"
        cmd_line_pkg.public_symbols["full_switch"] = full_switch_func

        # Parameter function
        parameter_func = Symbol(
            name="Parameter",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        parameter_func.runtime_name = "_gnat_parameter"
        cmd_line_pkg.public_symbols["parameter"] = parameter_func

        gnat_pkg.public_symbols["command_line"] = cmd_line_pkg

        # =====================================================================
        # GNAT.Directory_Operations - Directory handling
        # =====================================================================
        dir_ops_pkg = Symbol(
            name="Directory_Operations",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Dir_Type (limited private)
        dir_type = RecordType(
            name="Dir_Type",
            components=[],
            is_limited=True,
        )
        dir_ops_pkg.public_symbols["dir_type"] = Symbol(
            name="Dir_Type",
            kind=SymbolKind.TYPE,
            ada_type=dir_type,
            scope_level=0,
        )

        # Dir_Separator constant
        dir_ops_pkg.public_symbols["dir_separator"] = Symbol(
            name="Dir_Separator",
            kind=SymbolKind.CONSTANT,
            ada_type=char_type,
            is_constant=True,
            value=ord('/'),  # Unix-style, even though CP/M is target
            scope_level=0,
        )

        # Get_Current_Dir function
        get_current_dir_func = Symbol(
            name="Get_Current_Dir",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        get_current_dir_func.runtime_name = "_gnat_get_current_dir"
        dir_ops_pkg.public_symbols["get_current_dir"] = get_current_dir_func

        # Change_Dir procedure
        change_dir_proc = Symbol(
            name="Change_Dir",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Dir_Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        change_dir_proc.runtime_name = "_gnat_change_dir"
        dir_ops_pkg.public_symbols["change_dir"] = change_dir_proc

        # Make_Dir procedure
        make_dir_proc = Symbol(
            name="Make_Dir",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Dir_Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        make_dir_proc.runtime_name = "_gnat_make_dir"
        dir_ops_pkg.public_symbols["make_dir"] = make_dir_proc

        # Remove_Dir procedure
        remove_dir_proc = Symbol(
            name="Remove_Dir",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Dir_Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        remove_dir_proc.runtime_name = "_gnat_remove_dir"
        dir_ops_pkg.public_symbols["remove_dir"] = remove_dir_proc

        # Open procedure
        open_dir_proc = Symbol(
            name="Open",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Dir", SymbolKind.PARAMETER, dir_type, mode="out"),
                Symbol("Dir_Name", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        open_dir_proc.runtime_name = "_gnat_dir_open"
        dir_ops_pkg.public_symbols["open"] = open_dir_proc

        # Read procedure
        read_dir_proc = Symbol(
            name="Read",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Dir", SymbolKind.PARAMETER, dir_type, mode="in out"),
                Symbol("Str", SymbolKind.PARAMETER, str_type, mode="out"),
                Symbol("Last", SymbolKind.PARAMETER, nat_type, mode="out"),
            ],
        )
        read_dir_proc.runtime_name = "_gnat_dir_read"
        dir_ops_pkg.public_symbols["read"] = read_dir_proc

        # Close procedure
        close_dir_proc = Symbol(
            name="Close",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Dir", SymbolKind.PARAMETER, dir_type, mode="in out")],
        )
        close_dir_proc.runtime_name = "_gnat_dir_close"
        dir_ops_pkg.public_symbols["close"] = close_dir_proc

        # Base_Name function
        base_name_func = Symbol(
            name="Base_Name",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[Symbol("Path", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        base_name_func.runtime_name = "_gnat_base_name"
        dir_ops_pkg.public_symbols["base_name"] = base_name_func

        # Dir_Name function
        dir_name_func = Symbol(
            name="Dir_Name",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[Symbol("Path", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        dir_name_func.runtime_name = "_gnat_dir_name"
        dir_ops_pkg.public_symbols["dir_name"] = dir_name_func

        # File_Extension function
        file_ext_func = Symbol(
            name="File_Extension",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[Symbol("Path", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        file_ext_func.runtime_name = "_gnat_file_extension"
        dir_ops_pkg.public_symbols["file_extension"] = file_ext_func

        gnat_pkg.public_symbols["directory_operations"] = dir_ops_pkg

        # =====================================================================
        # GNAT.Traceback - Stack traceback
        # =====================================================================
        traceback_pkg = Symbol(
            name="Traceback",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Tracebacks_Array type (access to array of addresses)
        system_pkg = self.lookup("System")
        if system_pkg:
            addr_sym = system_pkg.public_symbols.get("address")
            addr_type = addr_sym.ada_type if addr_sym else int_type
        else:
            addr_type = int_type

        # Call_Chain procedure
        call_chain_proc = Symbol(
            name="Call_Chain",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Len", SymbolKind.PARAMETER, nat_type, mode="out"),
            ],
        )
        call_chain_proc.runtime_name = "_gnat_call_chain"
        traceback_pkg.public_symbols["call_chain"] = call_chain_proc

        gnat_pkg.public_symbols["traceback"] = traceback_pkg

        # =====================================================================
        # GNAT.Traceback.Symbolic - Symbolic traceback
        # =====================================================================
        symbolic_traceback_pkg = Symbol(
            name="Symbolic",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Symbolic_Traceback function (from Exception_Occurrence)
        sym_traceback_func = Symbol(
            name="Symbolic_Traceback",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],  # Takes Exception_Occurrence, simplified here
        )
        sym_traceback_func.runtime_name = "_gnat_symbolic_traceback"
        symbolic_traceback_pkg.public_symbols["symbolic_traceback"] = sym_traceback_func

        traceback_pkg.public_symbols["symbolic"] = symbolic_traceback_pkg

        # Register the GNAT package at global scope
        self.current_scope.define(gnat_pkg)

        # Initialize additional standard library packages
        self._init_additional_stdlib()

    def _init_additional_stdlib(self) -> None:
        """Initialize additional standard library packages for completeness."""
        int_type = PREDEFINED_TYPES["Integer"]
        nat_type = PREDEFINED_TYPES["Natural"]
        bool_type = PREDEFINED_TYPES["Boolean"]
        char_type = PREDEFINED_TYPES["Character"]
        str_type = PREDEFINED_TYPES["String"]
        wide_char_type = PREDEFINED_TYPES["Wide_Character"]
        wide_str_type = PREDEFINED_TYPES["Wide_String"]

        # Get Ada package from current scope
        ada_pkg = self.current_scope.lookup_local("ada")
        if ada_pkg is None:
            return  # Ada package should exist

        # =====================================================================
        # Ada.Locales - Locale support (stub for Z80/CP/M)
        # =====================================================================
        locales_pkg = Symbol(
            name="Locales",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )
        locales_pkg.is_pure = True

        # Language_Code subtype (3-character ISO 639 code)
        locales_pkg.public_symbols["language_code"] = Symbol(
            name="Language_Code",
            kind=SymbolKind.TYPE,
            ada_type=str_type,
            scope_level=0,
        )

        # Country_Code subtype (2-character ISO 3166 code)
        locales_pkg.public_symbols["country_code"] = Symbol(
            name="Country_Code",
            kind=SymbolKind.TYPE,
            ada_type=str_type,
            scope_level=0,
        )

        # Language function
        language_func = Symbol(
            name="Language",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        language_func.runtime_name = "_ada_locales_language"
        locales_pkg.public_symbols["language"] = language_func

        # Country function
        country_func = Symbol(
            name="Country",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[],
        )
        country_func.runtime_name = "_ada_locales_country"
        locales_pkg.public_symbols["country"] = country_func

        ada_pkg.public_symbols["locales"] = locales_pkg

        # =====================================================================
        # Ada.Environment_Variables - Environment variable access
        # (Stub for CP/M - typically not available on Z80/CP/M)
        # =====================================================================
        env_vars_pkg = Symbol(
            name="Environment_Variables",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Value function
        value_func = Symbol(
            name="Value",
            kind=SymbolKind.FUNCTION,
            return_type=str_type,
            scope_level=0,
            parameters=[Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        value_func.runtime_name = "_ada_env_value"
        env_vars_pkg.public_symbols["value"] = value_func

        # Exists function
        exists_func = Symbol(
            name="Exists",
            kind=SymbolKind.FUNCTION,
            return_type=bool_type,
            scope_level=0,
            parameters=[Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        exists_func.runtime_name = "_ada_env_exists"
        env_vars_pkg.public_symbols["exists"] = exists_func

        # Set procedure
        set_proc = Symbol(
            name="Set",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in"),
                Symbol("Value", SymbolKind.PARAMETER, str_type, mode="in"),
            ],
        )
        set_proc.runtime_name = "_ada_env_set"
        env_vars_pkg.public_symbols["set"] = set_proc

        # Clear procedure
        clear_proc = Symbol(
            name="Clear",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[Symbol("Name", SymbolKind.PARAMETER, str_type, mode="in")],
        )
        clear_proc.runtime_name = "_ada_env_clear"
        env_vars_pkg.public_symbols["clear"] = clear_proc

        ada_pkg.public_symbols["environment_variables"] = env_vars_pkg

        # =====================================================================
        # Ada.Wide_Wide_Characters - Wide Wide character support
        # =====================================================================
        wide_wide_chars_pkg = Symbol(
            name="Wide_Wide_Characters",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        # Wide_Wide_Characters.Handling
        www_handling_pkg = Symbol(
            name="Handling",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )
        www_handling_pkg.is_pure = True

        # Character classification functions
        for func_name in ["Is_Control", "Is_Letter", "Is_Lower", "Is_Upper",
                          "Is_Digit", "Is_Alphanumeric", "Is_Special",
                          "Is_Line_Terminator", "Is_Mark"]:
            func = Symbol(
                name=func_name,
                kind=SymbolKind.FUNCTION,
                return_type=bool_type,
                scope_level=0,
                parameters=[Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in")],
            )
            func.runtime_name = f"_www_{func_name.lower()}"
            www_handling_pkg.public_symbols[func_name.lower()] = func

        # Case conversion functions
        to_lower_func = Symbol(
            name="To_Lower",
            kind=SymbolKind.FUNCTION,
            return_type=wide_char_type,
            scope_level=0,
            parameters=[Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in")],
        )
        to_lower_func.runtime_name = "_www_to_lower"
        www_handling_pkg.public_symbols["to_lower"] = to_lower_func

        to_upper_func = Symbol(
            name="To_Upper",
            kind=SymbolKind.FUNCTION,
            return_type=wide_char_type,
            scope_level=0,
            parameters=[Symbol("Item", SymbolKind.PARAMETER, wide_char_type, mode="in")],
        )
        to_upper_func.runtime_name = "_www_to_upper"
        www_handling_pkg.public_symbols["to_upper"] = to_upper_func

        wide_wide_chars_pkg.public_symbols["handling"] = www_handling_pkg

        ada_pkg.public_symbols["wide_wide_characters"] = wide_wide_chars_pkg

        # =====================================================================
        # Ada.Long_Long_Integer_Text_IO - I/O for Long_Long_Integer
        # =====================================================================
        lli_text_io_pkg = Symbol(
            name="Long_Long_Integer_Text_IO",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )

        lli_type = PREDEFINED_TYPES.get("Long_Long_Integer", int_type)

        lli_get_proc = Symbol(
            name="Get",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, lli_type, mode="out"),
            ],
        )
        lli_get_proc.runtime_name = "_lli_text_io_get"
        lli_text_io_pkg.public_symbols["get"] = lli_get_proc

        lli_put_proc = Symbol(
            name="Put",
            kind=SymbolKind.PROCEDURE,
            scope_level=0,
            parameters=[
                Symbol("Item", SymbolKind.PARAMETER, lli_type, mode="in"),
            ],
        )
        lli_put_proc.runtime_name = "_lli_text_io_put"
        lli_text_io_pkg.public_symbols["put"] = lli_put_proc

        ada_pkg.public_symbols["long_long_integer_text_io"] = lli_text_io_pkg

        # =====================================================================
        # Ada.Iterator_Interfaces - Ada 2012 iterator interfaces
        # =====================================================================
        from uada80.type_system import InterfaceType

        iterator_pkg = Symbol(
            name="Iterator_Interfaces",
            kind=SymbolKind.PACKAGE,
            scope_level=0,
        )
        iterator_pkg.is_pure = True

        # Forward_Iterator interface
        forward_iterator = InterfaceType(
            name="Forward_Iterator",
            primitive_ops=[],
        )
        iterator_pkg.public_symbols["forward_iterator"] = Symbol(
            name="Forward_Iterator",
            kind=SymbolKind.TYPE,
            ada_type=forward_iterator,
            scope_level=0,
        )

        # Reversible_Iterator interface (extends Forward_Iterator)
        reversible_iterator = InterfaceType(
            name="Reversible_Iterator",
            primitive_ops=[],
        )
        iterator_pkg.public_symbols["reversible_iterator"] = Symbol(
            name="Reversible_Iterator",
            kind=SymbolKind.TYPE,
            ada_type=reversible_iterator,
            scope_level=0,
        )

        ada_pkg.public_symbols["iterator_interfaces"] = iterator_pkg

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
        if package_symbol.kind not in (SymbolKind.PACKAGE, SymbolKind.GENERIC_PACKAGE):
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
        """Get all overloaded symbols with the given name from all visible scopes.

        In Ada, overloading resolution considers all visible declarations of a name,
        not just the first one found. This method collects overloads from:
        1. All scopes in the scope chain (current scope outward)
        2. All USE'd packages in each scope
        """
        name_lower = name.lower()
        result: list[Symbol] = []
        seen_ids: set[int] = set()  # Track by id to avoid duplicates

        def add_symbol_chain(sym: Optional[Symbol]) -> None:
            """Add a symbol and its overload chain to results."""
            current = sym
            while current is not None:
                if id(current) not in seen_ids:
                    seen_ids.add(id(current))
                    result.append(current)
                current = current.overloaded_next

        # Search from current scope outward
        scope: Optional[Scope] = self.current_scope
        while scope is not None:
            # Check direct symbols in this scope
            symbol = scope.lookup_local(name_lower)
            add_symbol_chain(symbol)

            # Check USE clauses in this scope
            symbol = scope.lookup_use_clause(name_lower)
            add_symbol_chain(symbol)

            scope = scope.parent

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
