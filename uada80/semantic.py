"""
Semantic analyzer for Ada.

Performs:
- Name resolution (builds symbol table)
- Type checking
- Overload resolution
- Static expression evaluation
- Semantic error reporting
"""

import os
from dataclasses import dataclass, field
from typing import Optional

from uada80.ast_nodes import (
    ASTNode,
    Program,
    CompilationUnit,
    SubprogramBody,
    SubprogramDecl,
    PackageDecl,
    PackageBody,
    ObjectDecl,
    NumberDecl,
    TypeDecl,
    SubtypeDecl,
    ParameterSpec,
    UseClause,
    WithClause,
    ExceptionDecl,
    GenericSubprogramUnit,
    TaskTypeDecl,
    TaskBody,
    EntryDecl,
    EntryBody,
    BodyStub,
    ProtectedTypeDecl,
    ProtectedBody,
    # Statements
    Stmt,
    NullStmt,
    AssignmentStmt,
    IfStmt,
    CaseStmt,
    LoopStmt,
    WhileScheme,
    ForScheme,
    BlockStmt,
    ExitStmt,
    ReturnStmt,
    ExtendedReturnStmt,
    RaiseStmt,
    ProcedureCallStmt,
    PragmaStmt,
    DelayStmt,
    AcceptStmt,
    SelectStmt,
    RequeueStmt,
    AbortStmt,
    ParallelBlockStmt,
    # Expressions
    Expr,
    Identifier,
    SelectedName,
    AttributeReference,
    IndexedComponent,
    IntegerLiteral,
    RealLiteral,
    StringLiteral,
    CharacterLiteral,
    NullLiteral,
    BinaryExpr,
    UnaryExpr,
    BinaryOp,
    UnaryOp,
    RangeExpr,
    Parenthesized,
    Aggregate,
    DeltaAggregate,
    ContainerAggregate,
    IteratedComponentAssociation,
    ComponentAssociation,
    ActualParameter,
    FunctionCall,
    TypeConversion,
    QualifiedExpr,
    Allocator,
    ConditionalExpr,
    QuantifiedExpr,
    DeclareExpr,
    CaseExpr,
    MembershipTest,
    ExprChoice,
    RangeChoice,
    Slice,
    Dereference,
    TargetName,
    RaiseExpr,
    # Type definitions
    TypeDef,
    IntegerTypeDef,
    ModularTypeDef,
    EnumerationTypeDef,
    ArrayTypeDef,
    RecordTypeDef,
    AccessTypeDef,
    AccessSubprogramTypeDef,
    DerivedTypeDef,
    InterfaceTypeDef,
    PrivateTypeDef,
    RealTypeDef,
    SubtypeIndication,
    GenericInstantiation,
    GenericTypeDecl,
    # Representation clauses
    RepresentationClause,
    AttributeDefinitionClause,
    RecordRepresentationClause,
    EnumerationRepresentationClause,
)
from uada80.symbol_table import SymbolTable, Symbol, SymbolKind
from uada80.type_system import (
    AdaType,
    TypeKind,
    IntegerType,
    ModularType,
    FloatType,
    EnumerationType,
    ArrayType,
    RecordType,
    RecordComponent,
    AccessType,
    AccessSubprogramType,
    InterfaceType,
    TaskType,
    EntryInfo,
    ProtectedType,
    ProtectedOperation,
    VariantPartInfo,
    VariantInfo,
    PREDEFINED_TYPES,
    types_compatible,
    common_type,
    can_convert,
    same_type,
)


@dataclass
class SemanticError:
    """A semantic error."""

    message: str
    node: Optional[ASTNode] = None
    line: int = 0
    column: int = 0

    def __str__(self) -> str:
        if self.node and self.node.span:
            return f"{self.node.span}: error: {self.message}"
        if self.line > 0:
            return f"line {self.line}: error: {self.message}"
        return f"error: {self.message}"


@dataclass
class SemanticResult:
    """Result of semantic analysis."""

    symbols: SymbolTable
    errors: list[SemanticError] = field(default_factory=list)

    @property
    def has_errors(self) -> bool:
        return len(self.errors) > 0


class SemanticAnalyzer:
    """
    Semantic analyzer for Ada programs.

    Performs two passes:
    1. Declaration pass: collects all declarations into symbol table
    2. Checking pass: performs type checking and validation
    """

    def __init__(self, search_paths: Optional[list[str]] = None) -> None:
        self.symbols = SymbolTable()
        self.errors: list[SemanticError] = []
        self.current_subprogram: Optional[Symbol] = None  # For return type checking
        self.current_package: Optional[Symbol] = None  # For pragma Pure/Preelaborate
        self.in_loop: bool = False  # For exit statement validation
        self.loop_labels: list[Optional[str]] = []  # Stack of loop labels (None = unlabeled)
        # Task-related state
        self.in_task_body: bool = False  # For accept statement validation
        self.current_task: Optional[Symbol] = None  # Current task being analyzed
        self.in_accept_or_entry: bool = False  # For requeue statement validation
        # Assignment target tracking for @ (target name) support
        self.current_assignment_target_type: Optional[AdaType] = None
        # Multi-file package loading support
        # Auto-include adalib directory for standard library stubs
        adalib_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), "adalib")
        self.search_paths: list[str] = search_paths or []
        if os.path.isdir(adalib_dir) and adalib_dir not in self.search_paths:
            self.search_paths.insert(0, adalib_dir)
        self._loaded_packages: dict[str, Symbol] = {}  # Cache of loaded packages
        self._loading_packages: set[str] = set()  # Packages currently being loaded (cycle detection)
        # Set up standard prelude (ASCII package, etc.)
        self._setup_standard_prelude()

    def _setup_standard_prelude(self) -> None:
        """Set up the standard Ada prelude (implicitly visible packages like ASCII)."""
        # ASCII package - obsolescent but still used in Ada 83 code
        ascii_pkg = Symbol(name="ASCII", kind=SymbolKind.PACKAGE)
        ascii_pkg.package_symbols = {}
        char_type = PREDEFINED_TYPES.get("Character")

        # Define ASCII character constants
        ascii_chars = {
            "NUL": 0, "SOH": 1, "STX": 2, "ETX": 3, "EOT": 4, "ENQ": 5, "ACK": 6, "BEL": 7,
            "BS": 8, "HT": 9, "LF": 10, "VT": 11, "FF": 12, "CR": 13, "SO": 14, "SI": 15,
            "DLE": 16, "DC1": 17, "DC2": 18, "DC3": 19, "DC4": 20, "NAK": 21, "SYN": 22,
            "ETB": 23, "CAN": 24, "EM": 25, "SUB": 26, "ESC": 27, "FS": 28, "GS": 29,
            "RS": 30, "US": 31, "DEL": 127,
        }
        for name, val in ascii_chars.items():
            sym = Symbol(name=name, kind=SymbolKind.CONSTANT, ada_type=char_type)
            sym.is_constant = True
            ascii_pkg.package_symbols[name.lower()] = sym

        self.symbols.define(ascii_pkg)

    def analyze(self, program: Program) -> SemanticResult:
        """Analyze a complete program."""
        for unit in program.units:
            self._analyze_compilation_unit(unit)

        return SemanticResult(symbols=self.symbols, errors=self.errors)

    def error(self, message: str, node: Optional[ASTNode] = None) -> None:
        """Report a semantic error."""
        self.errors.append(SemanticError(message=message, node=node))

    # =========================================================================
    # Compilation Units
    # =========================================================================

    def _analyze_compilation_unit(self, unit: CompilationUnit) -> None:
        """Analyze a compilation unit."""
        # Process context clauses (with, use)
        for clause in unit.context_clauses:
            if isinstance(clause, WithClause):
                self._analyze_with_clause(clause)
            elif isinstance(clause, UseClause):
                self._analyze_use_clause(clause)

        # Process the main unit
        if isinstance(unit.unit, SubprogramBody):
            self._analyze_subprogram_body(unit.unit)
        elif isinstance(unit.unit, PackageDecl):
            self._analyze_package_decl(unit.unit)
        elif isinstance(unit.unit, PackageBody):
            self._analyze_package_body(unit.unit)
        elif isinstance(unit.unit, GenericInstantiation):
            self._analyze_generic_instantiation(unit.unit)
        elif isinstance(unit.unit, GenericSubprogramUnit):
            self._analyze_generic_subprogram(unit.unit)

    def _analyze_with_clause(self, clause: WithClause) -> None:
        """Analyze a with clause.

        A with clause makes the specified packages visible in the current
        compilation unit. The package names become directly usable for
        qualified references (Package.Entity).

        This now supports multi-file package loading:
        1. Load the package specification from the file system
        2. Parse and analyze it
        3. Add its public declarations to the visible scope
        """
        for name in clause.names:
            if isinstance(name, Identifier):
                pkg_name = name.name
                # Check if already defined (e.g., from a previous with)
                existing = self.symbols.lookup(pkg_name)
                if existing is None:
                    # Try to load the package from file system first
                    loaded_pkg = self._load_external_package(pkg_name)
                    if loaded_pkg:
                        self.symbols.define(loaded_pkg)
                    else:
                        # Create a placeholder package symbol
                        pkg_symbol = Symbol(
                            name=pkg_name,
                            kind=SymbolKind.PACKAGE,
                        )
                        pkg_symbol.is_withed = True  # Mark as from with clause
                        # For standard library packages, we could predefine their contents
                        if pkg_name.upper() in ("ADA", "SYSTEM", "INTERFACES"):
                            self._setup_standard_package(pkg_symbol, pkg_name.upper())
                        self.symbols.define(pkg_symbol)
            elif hasattr(name, 'prefix') and hasattr(name, 'selector'):
                # Handle hierarchical package names like Ada.Text_IO
                # Register both the root package and the full hierarchical name
                full_name = self._get_hierarchical_name(name)
                root_pkg = self._get_root_name(name)

                # First register the root package if not already defined
                existing = self.symbols.lookup(root_pkg)
                if existing is None:
                    # Try to load root package from file system
                    loaded_root = self._load_external_package(root_pkg)
                    if loaded_root:
                        self.symbols.define(loaded_root)
                    else:
                        pkg_symbol = Symbol(
                            name=root_pkg,
                            kind=SymbolKind.PACKAGE,
                        )
                        pkg_symbol.is_withed = True
                        if root_pkg.upper() in ("ADA", "SYSTEM", "INTERFACES"):
                            self._setup_standard_package(pkg_symbol, root_pkg.upper())
                        self.symbols.define(pkg_symbol)

                # Also register the full hierarchical name for direct lookup
                # This allows "Ada.Text_IO" to be found when used as a prefix
                if full_name != root_pkg:
                    existing_full = self.symbols.lookup(full_name)
                    if existing_full is None:
                        # Try to load the full package from file system first
                        loaded_full = self._load_external_package(full_name)
                        if loaded_full:
                            self.symbols.define(loaded_full)
                        else:
                            # Try to resolve the child package from the root's public_symbols
                            child_sym = self._resolve_hierarchical_package(name)
                            if child_sym:
                                # Register the full name pointing to the child package
                                full_pkg = Symbol(
                                    name=full_name,
                                    kind=child_sym.kind,
                                )
                                full_pkg.is_withed = True
                                full_pkg.public_symbols = child_sym.public_symbols
                                full_pkg.private_symbols = child_sym.private_symbols
                                self.symbols.define(full_pkg)

    def _get_hierarchical_name(self, name) -> str:
        """Get the full dotted name from a hierarchical package reference.

        E.g., SelectedName(prefix=Identifier("Ada"), selector="Text_IO") -> "Ada.Text_IO"
        """
        if isinstance(name, Identifier):
            return name.name
        elif hasattr(name, 'prefix') and hasattr(name, 'selector'):
            prefix_name = self._get_hierarchical_name(name.prefix)
            selector = name.selector if isinstance(name.selector, str) else name.selector
            return f"{prefix_name}.{selector}"
        return str(name)

    def _get_root_name(self, name) -> str:
        """Get the root package name from a hierarchical reference.

        E.g., SelectedName(prefix=Identifier("Ada"), selector="Text_IO") -> "Ada"
        """
        if isinstance(name, Identifier):
            return name.name
        elif hasattr(name, 'prefix'):
            return self._get_root_name(name.prefix)
        return str(name)

    def _resolve_hierarchical_package(self, name) -> Optional[Symbol]:
        """Resolve a hierarchical package name to its symbol.

        E.g., Ada.Text_IO -> look up "Ada", then find "Text_IO" in Ada.public_symbols
        """
        if isinstance(name, Identifier):
            return self.symbols.lookup(name.name)
        elif hasattr(name, 'prefix') and hasattr(name, 'selector'):
            # Recursively resolve the prefix
            prefix_sym = self._resolve_hierarchical_package(name.prefix)
            if prefix_sym is None:
                return None
            # Look up the selector in the prefix's public symbols
            selector = name.selector.lower() if isinstance(name.selector, str) else name.selector.lower()
            if prefix_sym.public_symbols and selector in prefix_sym.public_symbols:
                return prefix_sym.public_symbols[selector]
        return None

    def _find_package_file(self, pkg_name: str) -> Optional[str]:
        """Find the file containing a package specification.

        Converts Ada package names to file paths following GNAT naming conventions:
        - Ada.Text_IO -> ada-text_io.ads
        - My_Package -> my_package.ads

        Returns the full path if found, None otherwise.
        """
        # Convert package name to file name (GNAT convention)
        file_base = pkg_name.lower().replace(".", "-")
        file_name = f"{file_base}.ads"

        # Search in all configured paths
        for search_path in self.search_paths:
            file_path = os.path.join(search_path, file_name)
            if os.path.isfile(file_path):
                return file_path

        # Also search current directory
        if os.path.isfile(file_name):
            return file_name

        return None

    def _load_external_package(self, pkg_name: str) -> Optional[Symbol]:
        """Load and analyze an external package specification.

        Parses the package specification file and extracts public symbols.
        Returns a Symbol with populated public_symbols, or None if not found.
        """
        # Check cache first
        pkg_key = pkg_name.lower()
        if pkg_key in self._loaded_packages:
            return self._loaded_packages[pkg_key]

        # Detect circular dependencies
        if pkg_key in self._loading_packages:
            return None
        self._loading_packages.add(pkg_key)

        try:
            # Find the package file
            file_path = self._find_package_file(pkg_name)
            if not file_path:
                return None

            # Parse the file
            try:
                with open(file_path, "r") as f:
                    source = f.read()
                from uada80.parser import parse
                program = parse(source, file_path)
            except Exception:
                return None

            # Find the package declaration in the parsed AST
            pkg_decl = None
            for unit in program.units:
                if isinstance(unit.unit, PackageDecl):
                    # Match by name (case-insensitive)
                    if unit.unit.name.lower() == pkg_name.lower():
                        pkg_decl = unit.unit
                        break
                    # Also check for child package match (Ada.Text_IO in file)
                    if "." in pkg_name:
                        if unit.unit.name.lower().endswith(pkg_name.lower().split(".")[-1]):
                            pkg_decl = unit.unit
                            break

            if not pkg_decl:
                return None

            # Handle package renaming (e.g., package Text_IO renames Ada.Text_IO)
            if pkg_decl.renames:
                renamed_name = self._get_hierarchical_name(pkg_decl.renames)
                renamed_pkg = self._load_external_package(renamed_name)
                if renamed_pkg:
                    pkg_symbol = Symbol(
                        name=pkg_name,
                        kind=SymbolKind.PACKAGE,
                    )
                    pkg_symbol.is_withed = True
                    pkg_symbol.public_symbols = renamed_pkg.public_symbols
                    pkg_symbol.private_symbols = renamed_pkg.private_symbols
                    self._loaded_packages[pkg_key] = pkg_symbol
                    return pkg_symbol
                return None

            # Create a symbol for this package and extract its public declarations
            pkg_symbol = Symbol(
                name=pkg_name,
                kind=SymbolKind.PACKAGE,
            )
            pkg_symbol.is_withed = True

            # Save current state
            saved_errors = self.errors
            saved_symbols = self.symbols
            saved_package = self.current_package

            # Create fresh state for analyzing the external package
            self.errors = []
            self.symbols = SymbolTable()
            self.current_package = pkg_symbol

            # Enter package scope for analysis
            self.symbols.enter_scope(pkg_name, is_package=True)

            # Process WITH clauses from the package (recursive loading)
            for unit in program.units:
                if isinstance(unit, CompilationUnit):
                    for clause in unit.context_clauses:
                        if isinstance(clause, WithClause):
                            self._analyze_with_clause(clause)
                        elif isinstance(clause, UseClause):
                            self._analyze_use_clause(clause)

            # Analyze public declarations
            for decl in pkg_decl.declarations:
                try:
                    self._analyze_declaration(decl)
                    self._add_to_package(pkg_symbol, decl, is_private=False)
                except Exception:
                    pass  # Skip declarations that fail analysis

            self.symbols.leave_scope()

            # Restore state
            self.errors = saved_errors
            self.symbols = saved_symbols
            self.current_package = saved_package

            # Cache the loaded package
            self._loaded_packages[pkg_key] = pkg_symbol

            return pkg_symbol

        finally:
            self._loading_packages.discard(pkg_key)

    def _setup_standard_package(self, pkg_symbol: Symbol, name: str) -> None:
        """Set up standard library package contents.

        This provides minimal type/subprogram definitions for standard
        packages so that code referencing them can be analyzed.
        """
        if name == "SYSTEM":
            # System package provides Address, Storage_Elements, etc.
            # Add common types (keys are lowercase for case-insensitive lookup)
            addr_type = Symbol(name="Address", kind=SymbolKind.TYPE)
            addr_type.ada_type = AdaType(kind=TypeKind.ACCESS, name="Address")
            pkg_symbol.public_symbols["address"] = addr_type

            storage_type = Symbol(name="Storage_Offset", kind=SymbolKind.TYPE)
            storage_type.ada_type = AdaType(kind=TypeKind.INTEGER, name="Storage_Offset")
            pkg_symbol.public_symbols["storage_offset"] = storage_type

            # Standard Ada integer range constants (implementation-defined)
            # These are Universal_Integer type for implicit conversion to any integer
            int_type = AdaType(kind=TypeKind.UNIVERSAL_INTEGER, name="Universal_Integer")
            min_int = Symbol(
                name="Min_Int",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=-2147483648
            )
            pkg_symbol.public_symbols["min_int"] = min_int

            max_int = Symbol(
                name="Max_Int",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=2147483647
            )
            pkg_symbol.public_symbols["max_int"] = max_int

            # Storage_Unit - typically 8 bits per storage unit
            storage_unit = Symbol(
                name="Storage_Unit",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=8
            )
            pkg_symbol.public_symbols["storage_unit"] = storage_unit

            # Word_Size - typically 32 bits
            word_size = Symbol(
                name="Word_Size",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=32
            )
            pkg_symbol.public_symbols["word_size"] = word_size

            # Max_Binary_Modulus - largest power of 2 for modular types
            max_binary = Symbol(
                name="Max_Binary_Modulus",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=2**32  # 4294967296
            )
            pkg_symbol.public_symbols["max_binary_modulus"] = max_binary

            # Max_Nonbinary_Modulus - largest non-power-of-2 for modular types
            max_nonbinary = Symbol(
                name="Max_Nonbinary_Modulus",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=2**32  # Same as binary for this implementation
            )
            pkg_symbol.public_symbols["max_nonbinary_modulus"] = max_nonbinary

            # Max_Digits - maximum digits for floating-point types
            max_digits = Symbol(
                name="Max_Digits",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=15  # IEEE double precision
            )
            pkg_symbol.public_symbols["max_digits"] = max_digits

            # Max_Mantissa - maximum mantissa for fixed-point types
            max_mantissa = Symbol(
                name="Max_Mantissa",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=31  # 31-bit mantissa
            )
            pkg_symbol.public_symbols["max_mantissa"] = max_mantissa

            # Max_Base_Digits - maximum base digits
            max_base_digits = Symbol(
                name="Max_Base_Digits",
                kind=SymbolKind.VARIABLE,
                ada_type=int_type,
                is_constant=True,
                value=18  # Long double precision
            )
            pkg_symbol.public_symbols["max_base_digits"] = max_base_digits

            # Real-valued constants use Universal_Real type
            real_type = AdaType(kind=TypeKind.UNIVERSAL_REAL, name="Universal_Real")

            # Fine_Delta - smallest delta for fixed-point types (Universal_Real)
            fine_delta = Symbol(
                name="Fine_Delta",
                kind=SymbolKind.VARIABLE,
                ada_type=real_type,
                is_constant=True,
                value=2**(-31)  # Smallest fixed-point delta
            )
            pkg_symbol.public_symbols["fine_delta"] = fine_delta

            # Tick - clock tick duration (Universal_Real)
            tick = Symbol(
                name="Tick",
                kind=SymbolKind.VARIABLE,
                ada_type=real_type,
                is_constant=True,
                value=0.0001  # 100 microseconds
            )
            pkg_symbol.public_symbols["tick"] = tick

        elif name == "INTERFACES":
            # Interfaces package provides C types (keys are lowercase)
            for c_type in ["Integer_8", "Integer_16", "Integer_32",
                          "Unsigned_8", "Unsigned_16", "Unsigned_32"]:
                type_sym = Symbol(name=c_type, kind=SymbolKind.TYPE)
                if "Unsigned" in c_type:
                    type_sym.ada_type = AdaType(kind=TypeKind.MODULAR, name=c_type)
                else:
                    type_sym.ada_type = AdaType(kind=TypeKind.INTEGER, name=c_type)
                pkg_symbol.public_symbols[c_type.lower()] = type_sym

    def _analyze_use_clause(self, clause: UseClause) -> None:
        """Analyze a use clause."""
        if clause.is_type or clause.is_all:
            # use type T; or use all type T;
            # Makes operators (and for is_all, all primitives) directly visible
            for name in clause.names:
                type_sym = None
                pkg_symbol = None

                if isinstance(name, SelectedName):
                    # Qualified name like P.T
                    pkg_name = self._get_hierarchical_name(name.prefix)
                    pkg_symbol = self.symbols.lookup(pkg_name)
                    if pkg_symbol and pkg_symbol.kind == SymbolKind.PACKAGE:
                        type_name = name.selector.lower() if isinstance(name.selector, str) else name.selector
                        if type_name in pkg_symbol.public_symbols:
                            type_sym = pkg_symbol.public_symbols[type_name]
                elif isinstance(name, Identifier):
                    type_sym = self.symbols.lookup(name.name)

                if type_sym is None or type_sym.kind not in (SymbolKind.TYPE, SymbolKind.SUBTYPE):
                    type_name = self._get_hierarchical_name(name)
                    self.error(f"type '{type_name}' not found for use type clause", name)
                    continue

                # For use all type, make all primitive operations visible
                if clause.is_all and pkg_symbol and pkg_symbol.kind == SymbolKind.PACKAGE:
                    # Find primitive operations - operations with parameters of this type
                    target_type = type_sym.ada_type
                    for sym_name, sym in pkg_symbol.public_symbols.items():
                        if sym.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION):
                            # Check if any parameter is of the target type
                            is_primitive = False
                            if hasattr(sym, 'parameters') and sym.parameters:
                                for param in sym.parameters:
                                    if param.ada_type and target_type:
                                        if (param.ada_type.name == target_type.name or
                                            param.ada_type == target_type):
                                            is_primitive = True
                                            break
                            if is_primitive:
                                # Make this operation directly visible
                                self.symbols.define(sym)
        else:
            # Regular use clause (use Package;)
            for name in clause.names:
                if isinstance(name, Identifier):
                    pkg_symbol = self.symbols.lookup(name.name)
                    if pkg_symbol is None:
                        self.error(f"package '{name.name}' not found", name)
                    elif pkg_symbol.kind != SymbolKind.PACKAGE:
                        self.error(f"'{name.name}' is not a package", name)
                    else:
                        self.symbols.add_use_clause(pkg_symbol)

    # =========================================================================
    # Subprograms
    # =========================================================================

    def _analyze_subprogram_body(self, body: SubprogramBody) -> None:
        """Analyze a subprogram body."""
        spec = body.spec

        # Check if this is completing a generic subprogram spec
        existing = self.symbols.lookup_local(spec.name)
        is_completing_generic = (existing and
            existing.kind in (SymbolKind.GENERIC_PROCEDURE, SymbolKind.GENERIC_FUNCTION))

        if is_completing_generic:
            # This body completes the generic spec - store body reference
            existing.generic_body = body
            # Don't create a new symbol, use the existing generic one
            subprog_symbol = existing
            kind = (SymbolKind.GENERIC_FUNCTION
                    if existing.kind == SymbolKind.GENERIC_FUNCTION
                    else SymbolKind.GENERIC_PROCEDURE)
        else:
            # Create symbol for subprogram
            kind = SymbolKind.FUNCTION if spec.is_function else SymbolKind.PROCEDURE
            return_type = None
            if spec.is_function and spec.return_type:
                return_type = self._resolve_type(spec.return_type)

            subprog_symbol = Symbol(
                name=spec.name,
                kind=kind,
                return_type=return_type,
            )

            # Define in current scope
            self.symbols.define(subprog_symbol)

        # Collect parameter types for primitive operation check
        param_types = []
        for param_spec in spec.parameters:
            param_type = self._resolve_type(param_spec.type_mark)
            param_types.append(param_type)

        # Check if this is a primitive operation of a tagged type
        # (not for generic subprograms)
        if not is_completing_generic:
            return_type = subprog_symbol.return_type
            self._check_primitive_operation(subprog_symbol, kind == SymbolKind.FUNCTION,
                                            param_types, return_type)

        # Enter subprogram scope
        self.symbols.enter_scope(spec.name)
        old_subprogram = self.current_subprogram
        self.current_subprogram = subprog_symbol

        # If completing a generic, make the generic formal symbols visible
        if is_completing_generic:
            generic_decl = getattr(existing, 'generic_decl', None)
            if generic_decl:
                # Re-analyze generic formals to add them to current scope
                for formal in generic_decl.formals:
                    self._analyze_generic_formal(formal)

        # Process parameters (but don't add to symbol if completing generic - already done)
        for param_spec in spec.parameters:
            self._analyze_parameter_spec(param_spec, subprog_symbol,
                                        add_to_symbol=not is_completing_generic)

        # Analyze Pre/Post aspects
        self._analyze_subprogram_aspects(spec, subprog_symbol)

        # Process declarations
        for decl in body.declarations:
            self._analyze_declaration(decl)

        # Process statements
        for stmt in body.statements:
            self._analyze_statement(stmt)

        # Leave scope
        self.current_subprogram = old_subprogram
        self.symbols.leave_scope()

    def _analyze_parameter_spec(
        self, param: ParameterSpec, subprog: Symbol, add_to_symbol: bool = True
    ) -> None:
        """Analyze a parameter specification."""
        param_type = self._resolve_type(param.type_mark)

        # Access parameters have access mode - wrap type in AccessType
        if param.mode == "access" and param_type:
            param_type = AccessType(
                name=f"access_{param_type.name}",
                designated_type=param_type,
                is_access_all=True,  # Access parameters can access aliased objects
            )

        for name in param.names:
            param_symbol = Symbol(
                name=name,
                kind=SymbolKind.PARAMETER,
                ada_type=param_type,
                mode=param.mode,
                default_value=param.default_value,
            )
            self.symbols.define(param_symbol)
            if add_to_symbol:
                subprog.parameters.append(param_symbol)

    def _analyze_subprogram_aspects(
        self, spec: SubprogramDecl, subprog: Symbol
    ) -> None:
        """Analyze aspects on a subprogram (Pre, Post, etc.)."""
        for aspect in spec.aspects:
            aspect_name = aspect.name.lower()

            if aspect_name == "pre":
                # Precondition - must be Boolean expression
                if aspect.value:
                    expr_type = self._analyze_expr(aspect.value)
                    if expr_type and expr_type.name.lower() != "boolean":
                        self.error(
                            f"Pre aspect expression must be Boolean, got '{expr_type.name}'",
                            aspect.value,
                        )
                else:
                    self.error("Pre aspect requires an expression", spec)

            elif aspect_name == "post":
                # Postcondition - must be Boolean expression
                # For functions, Name'Result can be used to refer to return value
                if aspect.value:
                    # Temporarily add 'Result attribute support for functions
                    if spec.is_function:
                        subprog.analyzing_post = True
                    expr_type = self._analyze_expr(aspect.value)
                    if spec.is_function:
                        subprog.analyzing_post = False
                    if expr_type and expr_type.name.lower() != "boolean":
                        self.error(
                            f"Post aspect expression must be Boolean, got '{expr_type.name}'",
                            aspect.value,
                        )
                else:
                    self.error("Post aspect requires an expression", spec)

            elif aspect_name == "inline":
                # Boolean aspect, no value needed
                subprog.is_inline = True

            elif aspect_name == "import":
                # Mark as imported (external)
                subprog.is_imported = True

            elif aspect_name == "export":
                # Mark as exported
                subprog.is_exported = True

            elif aspect_name in ("convention", "external_name", "link_name"):
                # These affect linkage - store the values
                if aspect.value:
                    if isinstance(aspect.value, StringLiteral):
                        setattr(subprog, aspect_name, aspect.value.value)
                    elif isinstance(aspect.value, Identifier):
                        setattr(subprog, aspect_name, aspect.value.name)

            # Other aspects (Pure, Spark_Mode, etc.) are silently accepted

    # =========================================================================
    # Packages
    # =========================================================================

    def _analyze_package_decl(self, pkg: PackageDecl) -> None:
        """Analyze a package declaration."""
        # Handle package renaming: package X renames Y;
        if pkg.renames:
            renamed_name = self._get_hierarchical_name(pkg.renames)
            renamed_pkg = self.symbols.lookup(renamed_name)
            if renamed_pkg is None:
                # Try to load from file
                renamed_pkg = self._load_external_package(renamed_name)
            if renamed_pkg and renamed_pkg.kind == SymbolKind.PACKAGE:
                # Create renaming symbol that points to the renamed package
                pkg_symbol = Symbol(
                    name=pkg.name,
                    kind=SymbolKind.PACKAGE,
                )
                pkg_symbol.public_symbols = renamed_pkg.public_symbols
                pkg_symbol.private_symbols = renamed_pkg.private_symbols
                self.symbols.define(pkg_symbol)
            else:
                self.error(f"'{renamed_name}' is not a package", pkg)
            return

        is_generic = getattr(pkg, 'is_generic', False) or bool(pkg.generic_formals)

        pkg_symbol = Symbol(
            name=pkg.name,
            kind=SymbolKind.GENERIC_PACKAGE if is_generic else SymbolKind.PACKAGE,
        )
        # Store the AST node for instantiation
        if is_generic:
            pkg_symbol.generic_decl = pkg
        self.symbols.define(pkg_symbol)

        # Track current package for pragma Pure/Preelaborate
        old_package = self.current_package
        self.current_package = pkg_symbol

        # Enter package scope
        self.symbols.enter_scope(pkg.name, is_package=True)

        # For child packages (names with dots), make parent's declarations visible
        # Ada RM 10.1.1: A child package has implicit visibility to its parent
        if "." in pkg.name:
            self._import_parent_package_symbols(pkg.name)

        # Process generic formal parameters first
        for formal in pkg.generic_formals:
            self._analyze_generic_formal(formal, pkg_symbol)

        # Process public declarations
        for decl in pkg.declarations:
            self._analyze_declaration(decl)
            # Add to package's public symbols
            self._add_to_package(pkg_symbol, decl, is_private=False)

        # Process private declarations
        self.symbols.current_scope.in_private_part = True
        for decl in pkg.private_declarations:
            self._analyze_declaration(decl)
            self._add_to_package(pkg_symbol, decl, is_private=True)

        self.symbols.leave_scope()
        self.current_package = old_package

    def _import_parent_package_symbols(self, child_name: str) -> None:
        """Import parent package symbols for a child package.

        For a child package like Parent.Child, this makes all public
        declarations from Parent visible in Child's scope.
        Ada RM 10.1.1: The declarative region of a child package includes
        the visible part of its parent.
        """
        # Get parent name (e.g., "Parent" from "Parent.Child")
        parts = child_name.rsplit(".", 1)
        if len(parts) < 2:
            return

        parent_name = parts[0]

        # Look up the parent package symbol
        parent_sym = self.symbols.lookup(parent_name)
        if parent_sym is None or parent_sym.kind not in (SymbolKind.PACKAGE, SymbolKind.GENERIC_PACKAGE):
            # Parent not found - might be analyzed later or not present
            return

        # Add parent as an implicit "use" clause so its symbols are visible
        # This uses the existing use clause mechanism for symbol lookup
        self.symbols.add_use_clause(parent_sym)

        # Recursively import grandparent symbols if parent is also a child
        if "." in parent_name:
            self._import_parent_package_symbols(parent_name)

    def _analyze_generic_formal(self, formal, owner_symbol: Optional[Symbol] = None) -> None:
        """Analyze a generic formal parameter.

        Args:
            formal: The generic formal AST node
            owner_symbol: Optional symbol of the generic package/subprogram that owns this formal.
                         If provided, the formal symbol is stored on this symbol for later
                         retrieval when analyzing the generic body.
        """
        from uada80.ast_nodes import GenericObjectDecl

        sym = None  # The symbol we'll create for this formal

        if isinstance(formal, GenericTypeDecl):
            # Create a placeholder type for the generic type formal
            type_sym = Symbol(
                name=formal.name,
                kind=SymbolKind.TYPE,
            )
            # Mark it as a generic formal type
            type_sym.is_generic_formal = True

            # Determine the appropriate type kind based on the constraint
            constraint = getattr(formal, 'constraint', None) or 'private'
            if constraint == 'range':
                # Signed integer type (type T is range <>)
                type_kind = TypeKind.INTEGER
            elif constraint == 'mod':
                # Modular integer type (type T is mod <>)
                type_kind = TypeKind.MODULAR
            elif constraint == 'digits':
                # Floating point type (type T is digits <>)
                type_kind = TypeKind.FLOAT
            elif constraint in ('delta', 'delta_digits'):
                # Fixed point type (type T is delta <>)
                type_kind = TypeKind.FIXED
            elif constraint == 'discrete':
                # Discrete type (type T is (<>))
                type_kind = TypeKind.ENUMERATION
            else:
                # Private, tagged private, derived, etc.
                type_kind = TypeKind.PRIVATE

            type_sym.ada_type = AdaType(
                kind=type_kind,
                name=formal.name,
            )
            self.symbols.define(type_sym)
            sym = type_sym

        elif isinstance(formal, GenericObjectDecl):
            # Generic formal object: X : in Integer := 0
            # Or multiple names: F, L : E;
            # Get list of names, falling back to single name
            names = getattr(formal, 'names', None) or [formal.name]

            # Resolve the type reference
            resolved_type = None
            if isinstance(formal.type_ref, Identifier):
                type_sym = self.symbols.lookup(formal.type_ref.name)
                if type_sym and type_sym.ada_type:
                    resolved_type = type_sym.ada_type

            # Create symbol for each name
            for obj_name in names:
                obj_sym = Symbol(
                    name=obj_name,
                    kind=SymbolKind.VARIABLE,
                )
                obj_sym.is_generic_formal = True
                obj_sym.is_constant = (formal.mode == "in")  # "in" mode = read-only
                obj_sym.ada_type = resolved_type
                self.symbols.define(obj_sym)
                sym = obj_sym  # Last one becomes the representative

        elif hasattr(formal, '__class__') and formal.__class__.__name__ == 'GenericSubprogramDecl':
            # Generic formal subprogram
            # The formal subprogram declares a subprogram name that will be
            # substituted with an actual subprogram at instantiation
            # GenericSubprogramDecl has: name, kind (function/procedure), params, return_type
            subp_name = getattr(formal, 'name', None)
            if subp_name:
                is_function = getattr(formal, 'kind', 'procedure') == 'function'
                subp_sym = Symbol(
                    name=subp_name,
                    kind=SymbolKind.FUNCTION if is_function else SymbolKind.PROCEDURE,
                )
                subp_sym.is_generic_formal = True
                # Store return type for functions
                if is_function and hasattr(formal, 'return_type'):
                    subp_sym.return_type = self._resolve_type(formal.return_type)
                self.symbols.define(subp_sym)
                sym = subp_sym

        # Store the formal symbol on the owner for later retrieval in body analysis
        if sym is not None and owner_symbol is not None:
            owner_symbol.generic_formal_symbols[sym.name.lower()] = sym

    def _count_generic_parameters(self, formals: list) -> int:
        """Count actual number of generic parameters (accounts for multi-name declarations)."""
        from uada80.ast_nodes import GenericObjectDecl
        count = 0
        for formal in formals:
            if isinstance(formal, GenericObjectDecl):
                # Multi-name declaration like "F, L : E" counts as len(names) parameters
                names = getattr(formal, 'names', None)
                if names:
                    count += len(names)
                else:
                    count += 1
            else:
                count += 1
        return count

    def _analyze_generic_instantiation(self, inst: GenericInstantiation) -> None:
        """Analyze a generic instantiation."""
        # Look up the generic
        if isinstance(inst.generic_name, Identifier):
            generic_name = inst.generic_name.name
            generic_sym = self.symbols.lookup(generic_name)
        elif isinstance(inst.generic_name, SelectedName):
            # Handle qualified names like Ada.Unchecked_Deallocation
            if isinstance(inst.generic_name.prefix, Identifier):
                generic_sym = self.symbols.lookup_selected(
                    inst.generic_name.prefix.name,
                    inst.generic_name.selector
                )
                generic_name = f"{inst.generic_name.prefix.name}.{inst.generic_name.selector}"
            else:
                generic_name = str(inst.generic_name)
                generic_sym = self.symbols.lookup(generic_name)
        else:
            generic_name = str(inst.generic_name)
            generic_sym = self.symbols.lookup(generic_name)

        if generic_sym is None:
            self.error(f"generic '{generic_name}' not found", inst.generic_name)
            return

        # Handle generic subprogram instantiation
        if generic_sym.kind in (SymbolKind.GENERIC_PROCEDURE, SymbolKind.GENERIC_FUNCTION):
            self._analyze_generic_subprogram_instantiation(inst)
            return

        if generic_sym.kind != SymbolKind.GENERIC_PACKAGE:
            self.error(f"'{generic_name}' is not a generic", inst.generic_name)
            return

        # Get the generic declaration
        generic_decl = getattr(generic_sym, 'generic_decl', None)
        if generic_decl is None:
            self.error(f"generic '{generic_name}' has no declaration", inst.generic_name)
            return

        # Check number of actual parameters (accounting for defaults and multi-name formals)
        num_formals = self._count_generic_parameters(generic_decl.generic_formals)
        num_actuals = len(inst.actual_parameters)
        # Count formals with default values (including 'is <>' box defaults)
        num_with_defaults = sum(
            1 for f in generic_decl.generic_formals
            if (hasattr(f, 'default_value') and f.default_value is not None) or
               (hasattr(f, 'is_box') and f.is_box)
        )
        min_required = num_formals - num_with_defaults

        if num_actuals < min_required or num_actuals > num_formals:
            self.error(
                f"wrong number of generic parameters for '{generic_name}': "
                f"expected {min_required if min_required == num_formals else f'{min_required} to {num_formals}'}, got {num_actuals}",
                inst
            )

        # Create the instantiated package
        inst_symbol = Symbol(
            name=inst.name,
            kind=SymbolKind.PACKAGE,
        )
        # Store mapping from formals to actuals for code generation
        inst_symbol.generic_instance_of = generic_sym
        inst_symbol.generic_actuals = inst.actual_parameters
        self.symbols.define(inst_symbol)

        # Build mapping from generic formal names to actual values/types
        formal_to_actual: dict[str, any] = {}
        for i, formal in enumerate(generic_decl.generic_formals):
            if i < len(inst.actual_parameters):
                actual = inst.actual_parameters[i]
                if hasattr(formal, 'name'):
                    # Object formal (like X : Integer)
                    formal_to_actual[formal.name.lower()] = actual
                elif hasattr(formal, 'type_name'):
                    # Type formal (like type T is private)
                    formal_to_actual[formal.type_name.lower()] = actual

        # Enter the package scope to define its contents
        self.symbols.enter_scope(inst.name)

        # Save generic context for type resolution
        old_generic_formals = getattr(self, '_generic_formals', {})
        self._generic_formals = formal_to_actual

        # Process the generic package's declarations
        for decl in generic_decl.declarations:
            self._analyze_declaration(decl)

        # Restore generic context
        self._generic_formals = old_generic_formals

        # Export public symbols to the package
        for name, sym in self.symbols.current_scope.symbols.items():
            inst_symbol.public_symbols[name] = sym

        self.symbols.leave_scope()

    def _analyze_generic_subprogram(self, gen_subprog: GenericSubprogramUnit) -> None:
        """Analyze a generic subprogram declaration."""
        name = gen_subprog.name
        is_function = gen_subprog.is_function

        # Create symbol for the generic subprogram (template)
        kind = SymbolKind.GENERIC_FUNCTION if is_function else SymbolKind.GENERIC_PROCEDURE

        gen_symbol = Symbol(
            name=name,
            kind=kind,
        )
        # Store the AST node for instantiation
        gen_symbol.generic_decl = gen_subprog
        self.symbols.define(gen_symbol)

        # Enter scope for analyzing the generic formals
        self.symbols.enter_scope(name)

        # Process generic formal parameters
        for formal in gen_subprog.formals:
            self._analyze_generic_formal(formal)

        # Analyze the subprogram spec/body (but don't generate code - it's a template)
        if isinstance(gen_subprog.subprogram, SubprogramBody):
            spec = gen_subprog.subprogram.spec
        else:
            spec = gen_subprog.subprogram

        # Record parameter info
        return_type = None
        if is_function and spec.return_type:
            return_type = self._resolve_type(spec.return_type)

        gen_symbol.return_type = return_type

        # Process parameters to record their types and add them to scope
        for param_spec in spec.parameters:
            param_type = self._resolve_type(param_spec.type_mark)
            for param_name in param_spec.names:
                param_symbol = Symbol(
                    name=param_name,
                    kind=SymbolKind.PARAMETER,
                    ada_type=param_type,
                    mode=param_spec.mode,
                )
                gen_symbol.parameters.append(param_symbol)
                # Also define parameter in current scope for body analysis
                self.symbols.define(param_symbol)

        # If this is a generic subprogram body, analyze it (for error checking)
        # The generic formals and parameters are visible in this scope
        if isinstance(gen_subprog.subprogram, SubprogramBody):
            body = gen_subprog.subprogram
            # Set current_subprogram so return statements are valid
            old_subprogram = self.current_subprogram
            self.current_subprogram = gen_symbol
            # Analyze local declarations
            for decl in body.declarations:
                self._analyze_declaration(decl)
            # Analyze statements (for symbol resolution checks)
            for stmt in body.statements:
                self._analyze_statement(stmt)
            # Restore previous context
            self.current_subprogram = old_subprogram

        self.symbols.leave_scope()

    def _analyze_generic_subprogram_instantiation(
        self, inst: GenericInstantiation
    ) -> None:
        """Analyze a generic subprogram instantiation."""
        # Look up the generic
        if isinstance(inst.generic_name, Identifier):
            generic_name = inst.generic_name.name
            generic_sym = self.symbols.lookup(generic_name)
        elif isinstance(inst.generic_name, SelectedName):
            # Handle qualified names like Ada.Unchecked_Deallocation
            if isinstance(inst.generic_name.prefix, Identifier):
                generic_sym = self.symbols.lookup_selected(
                    inst.generic_name.prefix.name,
                    inst.generic_name.selector
                )
                generic_name = f"{inst.generic_name.prefix.name}.{inst.generic_name.selector}"
            else:
                generic_name = str(inst.generic_name)
                generic_sym = self.symbols.lookup(generic_name)
        else:
            generic_name = str(inst.generic_name)
            generic_sym = self.symbols.lookup(generic_name)

        if generic_sym is None:
            self.error(f"generic '{generic_name}' not found", inst.generic_name)
            return

        if generic_sym.kind not in (SymbolKind.GENERIC_PROCEDURE, SymbolKind.GENERIC_FUNCTION):
            self.error(f"'{generic_name}' is not a generic subprogram", inst.generic_name)
            return

        # Check if this is a built-in generic (like Ada.Unchecked_Deallocation)
        is_builtin = getattr(generic_sym, 'is_builtin_generic', False)

        # For non-builtin generics, check declaration and formal parameters
        if not is_builtin:
            # Get the generic declaration
            generic_decl = getattr(generic_sym, 'generic_decl', None)
            if generic_decl is None:
                self.error(f"generic '{generic_name}' has no declaration", inst.generic_name)
                return

            # Check number of actual parameters (accounting for defaults and multi-name formals)
            num_formals = self._count_generic_parameters(generic_decl.formals)
            num_actuals = len(inst.actual_parameters)
            # Count formals with default values (including 'is <>' box defaults)
            num_with_defaults = sum(
                1 for f in generic_decl.formals
                if (hasattr(f, 'default_value') and f.default_value is not None) or
                   (hasattr(f, 'is_box') and f.is_box)
            )
            min_required = num_formals - num_with_defaults

            if num_actuals < min_required or num_actuals > num_formals:
                self.error(
                    f"wrong number of generic parameters for '{generic_name}': "
                    f"expected {min_required if min_required == num_formals else f'{min_required} to {num_formals}'}, got {num_actuals}",
                    inst
                )

        # Create the instantiated subprogram
        is_function = generic_sym.kind == SymbolKind.GENERIC_FUNCTION
        inst_symbol = Symbol(
            name=inst.name,
            kind=SymbolKind.FUNCTION if is_function else SymbolKind.PROCEDURE,
        )
        # Store mapping from formals to actuals for code generation
        inst_symbol.generic_instance_of = generic_sym
        inst_symbol.generic_actuals = inst.actual_parameters
        inst_symbol.return_type = generic_sym.return_type
        # Copy parameters from the original generic subprogram
        inst_symbol.parameters = generic_sym.parameters.copy() if generic_sym.parameters else []

        # Check if this is Ada.Unchecked_Deallocation instantiation
        generic_name_lower = generic_name.lower()
        if generic_name_lower in ("ada.unchecked_deallocation", "unchecked_deallocation"):
            inst_symbol.is_deallocation = True

        # Check if this is Ada.Unchecked_Conversion instantiation
        if generic_name_lower in ("ada.unchecked_conversion", "unchecked_conversion"):
            inst_symbol.is_unchecked_conversion = True

        self.symbols.define(inst_symbol)

    def _analyze_package_body(self, body: PackageBody) -> None:
        """Analyze a package body."""
        # Look up the package declaration
        pkg_symbol = self.symbols.lookup(body.name)
        if pkg_symbol is None:
            self.error(f"package specification for '{body.name}' not found")
            return
        if pkg_symbol.kind not in (SymbolKind.PACKAGE, SymbolKind.GENERIC_PACKAGE):
            self.error(f"'{body.name}' is not a package")
            return

        # Enter package scope
        self.symbols.enter_scope(body.name)

        # Make generic formal symbols visible in the body (for generic packages)
        for sym in pkg_symbol.generic_formal_symbols.values():
            self.symbols.define(sym)

        # Make package specification symbols visible in the body
        # This includes both public and private declarations from the spec
        for sym in pkg_symbol.public_symbols.values():
            self.symbols.define(sym)
        for sym in pkg_symbol.private_symbols.values():
            self.symbols.define(sym)

        # Process declarations
        for decl in body.declarations:
            self._analyze_declaration(decl)

        # Process initialization statements
        for stmt in body.statements:
            self._analyze_statement(stmt)

        self.symbols.leave_scope()

    def _add_to_package(
        self, pkg: Symbol, decl: ASTNode, is_private: bool
    ) -> None:
        """Add a declaration to a package's symbol table."""
        if hasattr(decl, "name"):
            # Handle both string names and Identifier objects
            decl_name = decl.name
            name = decl_name.name.lower() if isinstance(decl_name, Identifier) else str(decl_name).lower()
            symbol = self.symbols.lookup_local(name)
            if symbol:
                if is_private:
                    pkg.private_symbols[name] = symbol
                else:
                    pkg.public_symbols[name] = symbol
        elif hasattr(decl, "names"):
            for name in decl.names:
                # Handle both string names and Identifier objects
                name_str = name.name if isinstance(name, Identifier) else str(name)
                name_lower = name_str.lower()
                symbol = self.symbols.lookup_local(name_lower)
                if symbol:
                    if is_private:
                        pkg.private_symbols[name_lower] = symbol
                    else:
                        pkg.public_symbols[name_lower] = symbol

    # =========================================================================
    # Declarations
    # =========================================================================

    def _analyze_declaration(self, decl: ASTNode) -> None:
        """Analyze a declaration."""
        if isinstance(decl, ObjectDecl):
            self._analyze_object_decl(decl)
        elif isinstance(decl, NumberDecl):
            self._analyze_number_decl(decl)
        elif isinstance(decl, TypeDecl):
            self._analyze_type_decl(decl)
        elif isinstance(decl, SubtypeDecl):
            self._analyze_subtype_decl(decl)
        elif isinstance(decl, SubprogramBody):
            self._analyze_subprogram_body(decl)
        elif isinstance(decl, SubprogramDecl):
            self._analyze_subprogram_decl(decl)
        elif isinstance(decl, ExceptionDecl):
            self._analyze_exception_decl(decl)
        elif isinstance(decl, UseClause):
            self._analyze_use_clause(decl)
        elif isinstance(decl, RepresentationClause):
            self._analyze_representation_clause(decl)
        elif isinstance(decl, GenericSubprogramUnit):
            self._analyze_generic_subprogram(decl)
        elif isinstance(decl, GenericInstantiation):
            self._analyze_generic_instantiation(decl)
        elif isinstance(decl, TaskTypeDecl):
            self._analyze_task_type_decl(decl)
        elif isinstance(decl, TaskBody):
            self._analyze_task_body(decl)
        elif isinstance(decl, EntryDecl):
            self._analyze_entry_decl(decl)
        elif isinstance(decl, ProtectedTypeDecl):
            self._analyze_protected_type_decl(decl)
        elif isinstance(decl, ProtectedBody):
            self._analyze_protected_body(decl)
        elif isinstance(decl, BodyStub):
            self._analyze_body_stub(decl)
        elif isinstance(decl, PackageDecl):
            self._analyze_package_decl(decl)
        elif isinstance(decl, PackageBody):
            self._analyze_package_body(decl)
        elif isinstance(decl, PragmaStmt):
            # Handle pragmas in declarative part (e.g., pragma Atomic)
            self._analyze_pragma(decl)

    def _analyze_object_decl(self, decl: ObjectDecl) -> None:
        """Analyze an object (variable/constant) declaration."""
        # Handle renaming declarations
        if decl.renames:
            self._analyze_renaming_decl(decl)
            return

        # Resolve type
        obj_type: Optional[AdaType] = None
        if decl.type_mark:
            # Handle anonymous array types (e.g., X : array (1..10) of Integer)
            if isinstance(decl.type_mark, ArrayTypeDef):
                obj_type = self._build_array_type(decl.names[0] if decl.names else "_anon", decl.type_mark)
            elif isinstance(decl.type_mark, SubtypeIndication):
                obj_type = self._resolve_subtype_indication(decl.type_mark)
            else:
                # Assume it's a type name (Identifier or SelectedName)
                obj_type = self._resolve_type(decl.type_mark)

        # Check initialization expression
        if decl.init_expr:
            # Pass expected type for overload resolution (e.g., enum literals)
            init_type = self._analyze_expr(decl.init_expr, expected_type=obj_type)
            if obj_type and init_type:
                if not types_compatible(obj_type, init_type):
                    self.error(
                        f"type mismatch in initialization: expected "
                        f"'{obj_type.name}', got '{init_type.name}'",
                        decl.init_expr,
                    )
            elif init_type and not obj_type:
                # Type inference from initializer (not strictly Ada, but useful)
                obj_type = init_type

        # Try to evaluate static value for constants
        static_value = None
        if decl.is_constant and decl.init_expr:
            static_value = self._try_eval_static(decl.init_expr)

        # Create symbols
        for name in decl.names:
            existing = self.symbols.lookup_local(name)

            # Check for deferred constant completion
            if existing is not None:
                if (decl.is_constant and decl.init_expr and
                    existing.is_constant and existing.value is None and
                    existing.definition and
                    not getattr(existing.definition, 'init_expr', None)):
                    # This is completing a deferred constant - update existing symbol
                    existing.value = static_value
                    existing.definition = decl
                    if obj_type:
                        existing.ada_type = obj_type
                    continue
                else:
                    self.error(f"'{name}' is already defined in this scope", decl)
                    continue

            # Constants without initialization in package specs are deferred constants
            # They'll be completed in the private part - don't error here
            is_deferred_constant = (decl.is_constant and not decl.init_expr and
                                    self.symbols.current_scope.is_package)

            if decl.is_constant and not decl.init_expr and not is_deferred_constant:
                self.error("constant declaration must have initialization", decl)

            symbol = Symbol(
                name=name,
                kind=SymbolKind.VARIABLE,
                ada_type=obj_type,
                is_constant=decl.is_constant,
                is_aliased=decl.is_aliased,
                definition=decl,
                value=static_value,  # Store static value for constants (None for deferred)
            )
            self.symbols.define(symbol)

    def _analyze_number_decl(self, decl: NumberDecl) -> None:
        """Analyze a number declaration (named number).

        Ada allows named numbers like:
            X : constant := 10;
            Pi : constant := 3.14159;

        These are compile-time constants with universal types.
        """
        # Evaluate the static expression
        static_value = self._try_eval_static(decl.value)

        # Determine type based on the expression
        if isinstance(decl.value, IntegerLiteral):
            num_type = self.symbols.lookup_type("universal_integer")
        elif isinstance(decl.value, RealLiteral):
            num_type = self.symbols.lookup_type("universal_real")
        else:
            # For other expressions, try to infer type
            num_type = self._analyze_expr(decl.value)
            if num_type is None:
                num_type = self.symbols.lookup_type("universal_integer")

        # Create symbols for each name
        for name in decl.names:
            if self.symbols.is_defined_locally(name):
                self.error(f"'{name}' is already defined in this scope", decl)
                continue

            symbol = Symbol(
                name=name,
                kind=SymbolKind.VARIABLE,
                ada_type=num_type,
                is_constant=True,
                definition=decl,
                value=static_value,
            )
            self.symbols.define(symbol)

    def _analyze_renaming_decl(self, decl: ObjectDecl) -> None:
        """Analyze a renaming declaration (X : T renames Y)."""
        # Analyze the renamed object
        renamed_type = self._analyze_expr(decl.renames)

        # Resolve declared type if provided
        obj_type = renamed_type
        if decl.type_mark:
            declared_type = self._resolve_subtype_indication(decl.type_mark)
            if declared_type and renamed_type:
                if not types_compatible(declared_type, renamed_type):
                    self.error(
                        f"type mismatch in renaming: declared type "
                        f"'{declared_type.name}' does not match renamed "
                        f"object type '{renamed_type.name}'",
                        decl,
                    )
            if declared_type:
                obj_type = declared_type

        # Create symbol for the new name that aliases the renamed object
        for name in decl.names:
            if self.symbols.is_defined_locally(name):
                self.error(f"'{name}' is already defined in this scope", decl)
                continue

            symbol = Symbol(
                name=name,
                kind=SymbolKind.VARIABLE,
                ada_type=obj_type,
                is_constant=decl.is_constant,
                is_aliased=True,  # Renamings are effectively aliases
                definition=decl,
            )
            self.symbols.define(symbol)

    def _analyze_type_decl(self, decl: TypeDecl) -> None:
        """Analyze a type declaration."""
        existing = self.symbols.lookup_local(decl.name)

        # Check for incomplete type declaration (type T;)
        if decl.type_def is None:
            if existing is not None:
                self.error(f"type '{decl.name}' is already defined", decl)
                return

            # Create an incomplete type placeholder
            ada_type = AdaType(
                name=decl.name,
                kind=TypeKind.INCOMPLETE,
            )
            symbol = Symbol(
                name=decl.name,
                kind=SymbolKind.TYPE,
                ada_type=ada_type,
                definition=decl,
            )
            self.symbols.define(symbol)
            return

        # Check if we're completing an incomplete or private type
        if existing is not None:
            if (existing.kind == SymbolKind.TYPE and
                existing.ada_type and
                existing.ada_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE)):
                # This is completing an incomplete or private type - update the existing symbol
                is_tagged = getattr(decl, 'is_tagged', False)
                ada_type = self._build_type(decl.name, decl.type_def, is_tagged)

                # Add discriminants to record types
                if isinstance(ada_type, RecordType) and decl.discriminants:
                    for disc_spec in decl.discriminants:
                        disc_type = self._resolve_type(disc_spec.type_mark)
                        if disc_type is None:
                            disc_type = IntegerType(name="_unknown", size_bits=16, low=0, high=0)
                        for disc_name in disc_spec.names:
                            ada_type.discriminants.append(
                                RecordComponent(
                                    name=disc_name,
                                    component_type=disc_type,
                                    is_discriminant=True,
                                )
                            )

                existing.ada_type = ada_type
                existing.definition = decl
                decl.ada_type = ada_type  # Store on AST for lowering
                # Fall through to handle enum literals if applicable
            else:
                self.error(f"type '{decl.name}' is already defined", decl)
                return
        else:
            # Build the type
            is_tagged = getattr(decl, 'is_tagged', False)
            ada_type = self._build_type(decl.name, decl.type_def, is_tagged)

            # Add discriminants to record types
            if isinstance(ada_type, RecordType) and decl.discriminants:
                for disc_spec in decl.discriminants:
                    disc_type = self._resolve_type(disc_spec.type_mark)
                    if disc_type is None:
                        disc_type = IntegerType(name="_unknown", size_bits=16, low=0, high=0)
                    for disc_name in disc_spec.names:
                        ada_type.discriminants.append(
                            RecordComponent(
                                name=disc_name,
                                component_type=disc_type,
                                is_discriminant=True,
                            )
                        )

            # Store the analyzed type on the AST node for lowering to access
            decl.ada_type = ada_type

            symbol = Symbol(
                name=decl.name,
                kind=SymbolKind.TYPE,
                ada_type=ada_type,
                definition=decl,
            )
            self.symbols.define(symbol)

        # For derived types, inherit primitive operations from parent type
        if isinstance(decl.type_def, DerivedTypeDef):
            parent_type = self._resolve_type(decl.type_def.parent_type)
            if parent_type:
                self._inherit_primitive_operations(ada_type, parent_type, decl.type_def.parent_type)

        # For enumeration types, add literals to symbol table
        # Ada allows the same literal name in different enumeration types (overloading)
        # BUT only for new enumeration type definitions, NOT for derived types.
        # Derived types inherit literals which are already in scope from the parent.
        if isinstance(ada_type, EnumerationType) and isinstance(decl.type_def, EnumerationTypeDef):
            for literal in ada_type.literals:
                existing = self.symbols.lookup_local(literal)
                # Allow if it's a new literal OR if existing is also an enum literal
                # (enum literals can be overloaded like subprograms)
                if existing is not None:
                    # Check if existing is an enumeration literal
                    if not (existing.is_constant and
                            existing.ada_type and
                            existing.ada_type.kind == TypeKind.ENUMERATION):
                        self.error(
                            f"enumeration literal '{literal}' conflicts with existing declaration",
                            decl,
                        )
                        continue
                    # Same literal in different enum type - this is fine in Ada
                    # The literal becomes overloaded

                literal_symbol = Symbol(
                    name=literal,
                    kind=SymbolKind.VARIABLE,
                    ada_type=ada_type,
                    is_constant=True,
                    definition=decl,
                )
                # Use define which handles overloading
                self.symbols.define(literal_symbol)

    def _analyze_subtype_decl(self, decl: SubtypeDecl) -> None:
        """Analyze a subtype declaration."""
        if self.symbols.is_defined_locally(decl.name):
            self.error(f"subtype '{decl.name}' is already defined", decl)
            return

        base_type = self._resolve_subtype_indication(decl.subtype_indication)
        if base_type is None:
            return

        # For now, just use the base type with a different name
        # Full implementation would apply constraints
        symbol = Symbol(
            name=decl.name,
            kind=SymbolKind.SUBTYPE,
            ada_type=base_type,
            definition=decl,
        )
        self.symbols.define(symbol)

    def _analyze_subprogram_decl(self, decl: SubprogramDecl) -> None:
        """Analyze a subprogram declaration (spec only, no body)."""
        kind = SymbolKind.FUNCTION if decl.is_function else SymbolKind.PROCEDURE
        return_type = None
        if decl.is_function and decl.return_type:
            return_type = self._resolve_type(decl.return_type)

        symbol = Symbol(
            name=decl.name,
            kind=kind,
            return_type=return_type,
            is_abstract=decl.is_abstract,
        )

        # Process parameters to record their types
        param_types = []
        for param_spec in decl.parameters:
            param_type = self._resolve_type(param_spec.type_mark)
            param_types.append(param_type)
            for name in param_spec.names:
                param_symbol = Symbol(
                    name=name,
                    kind=SymbolKind.PARAMETER,
                    ada_type=param_type,
                    mode=param_spec.mode,
                )
                symbol.parameters.append(param_symbol)

        # Abstract subprograms can only be declared for tagged types
        if decl.is_abstract:
            if not param_types or not self._is_tagged_type(param_types[0]):
                # In full Ada, this would be an error
                pass  # Allow for now, just track the flag

        self.symbols.define(symbol)

        # Check if this is a primitive operation of a tagged type
        self._check_primitive_operation(symbol, kind == SymbolKind.FUNCTION,
                                        param_types, return_type)

    def _is_tagged_type(self, ada_type) -> bool:
        """Check if a type is a tagged type or interface."""
        from uada80.type_system import RecordType, InterfaceType
        if isinstance(ada_type, InterfaceType):
            return True
        return isinstance(ada_type, RecordType) and ada_type.is_tagged

    def _check_primitive_operation(self, symbol: Symbol, is_function: bool,
                                   param_types: list, return_type) -> None:
        """Check if a subprogram is a primitive operation of a tagged type or interface.

        Updates the symbol with primitive_of and vtable_slot if it's a primitive.
        """
        from uada80.type_system import RecordType, InterfaceType, PrimitiveOperation

        # Check first parameter for controlling type
        controlling_type = None
        if param_types:
            first_type = param_types[0]
            if isinstance(first_type, RecordType) and first_type.is_tagged:
                controlling_type = first_type
            elif isinstance(first_type, InterfaceType):
                controlling_type = first_type

        # Check return type for tagged type or interface
        if not controlling_type and is_function:
            if isinstance(return_type, RecordType) and return_type.is_tagged:
                controlling_type = return_type
            elif isinstance(return_type, InterfaceType):
                controlling_type = return_type

        if controlling_type:
            # This is a primitive operation
            op = PrimitiveOperation(
                name=symbol.name,
                is_function=is_function,
                parameter_types=param_types,
                return_type=return_type,
            )
            controlling_type.add_primitive(op)

            # Update the symbol with primitive information for dispatching
            if isinstance(controlling_type, RecordType):
                symbol.primitive_of = controlling_type
                symbol.vtable_slot = op.slot_index

    def _analyze_exception_decl(self, decl: ExceptionDecl) -> None:
        """Analyze an exception declaration."""
        for name in decl.names:
            if self.symbols.is_defined_locally(name):
                self.error(f"'{name}' is already defined", decl)
                continue

            symbol = Symbol(
                name=name,
                kind=SymbolKind.EXCEPTION,
                definition=decl,
            )
            self.symbols.define(symbol)

    def _analyze_exception_handler(self, handler) -> None:
        """Analyze an exception handler."""
        # Check that exception names are valid
        for exc_name in handler.exception_names:
            if isinstance(exc_name, Identifier):
                # Verify it's a declared exception
                symbol = self.symbols.lookup(exc_name.name)
                if symbol is None:
                    self.error(f"unknown exception '{exc_name.name}'", exc_name)
                elif symbol.kind != SymbolKind.EXCEPTION:
                    self.error(f"'{exc_name.name}' is not an exception", exc_name)

        # Analyze handler statements
        for stmt in handler.statements:
            self._analyze_statement(stmt)

    def _analyze_representation_clause(self, decl: RepresentationClause) -> None:
        """Analyze a representation clause.

        Representation clauses specify:
        - Type'Size use N;  (attribute definition)
        - for Type use record ... end record; (record rep)
        - for Type use (...); (enumeration rep)
        """
        if isinstance(decl, AttributeDefinitionClause):
            self._analyze_attribute_definition_clause(decl)
        elif isinstance(decl, RecordRepresentationClause):
            self._analyze_record_representation_clause(decl)
        elif isinstance(decl, EnumerationRepresentationClause):
            self._analyze_enumeration_representation_clause(decl)

    def _analyze_attribute_definition_clause(
        self, decl: AttributeDefinitionClause
    ) -> None:
        """Analyze an attribute definition clause.

        Handles:
        - for Type'Size use N;
        - for Type'Alignment use N;
        - for Object'Address use N;
        - for Type'Component_Size use N;
        """
        # Get the name being modified
        obj_name = ""
        if isinstance(decl.name, Identifier):
            obj_name = decl.name.name
        elif hasattr(decl.name, "name"):
            obj_name = decl.name.name

        sym = self.symbols.lookup(obj_name)
        if sym is None:
            self.error(f"unknown identifier '{obj_name}'", decl)
            return

        # Evaluate the value expression
        value = self._eval_static_expr(decl.value)

        # Apply the attribute based on what it is and what kind of entity
        attr = decl.attribute.lower()

        if attr == "size":
            if sym.kind == SymbolKind.TYPE and sym.ada_type:
                sym.ada_type.size_bits = value
            elif sym.kind == SymbolKind.VARIABLE:
                sym.explicit_size = value
        elif attr == "alignment":
            if sym.kind == SymbolKind.TYPE and sym.ada_type:
                sym.ada_type.alignment = value
        elif attr == "address":
            # for Object'Address use N; - place object at specific address
            if sym.kind == SymbolKind.VARIABLE:
                sym.explicit_address = value
            else:
                self.error(f"Address clause only applies to variables", decl)
        elif attr == "component_size":
            # for Array_Type'Component_Size use N;
            if sym.kind == SymbolKind.TYPE and sym.ada_type:
                from uada80.type_system import ArrayType
                if isinstance(sym.ada_type, ArrayType):
                    # Store component size (would need to add field)
                    sym.ada_type.component_type.size_bits = value
        elif attr == "storage_size":
            # for Access_Type'Storage_Size use N;
            # for Task_Type'Storage_Size use N;
            pass  # Handled in access type or task type declaration
        elif attr == "":
            # Direct value clause (for Type use value)
            pass

    def _analyze_record_representation_clause(
        self, decl: RecordRepresentationClause
    ) -> None:
        """Analyze a record representation clause."""
        # Get the record type
        type_name = ""
        if isinstance(decl.type_name, Identifier):
            type_name = decl.type_name.name
        elif hasattr(decl.type_name, "name"):
            type_name = decl.type_name.name

        sym = self.symbols.lookup(type_name)
        if sym is None:
            self.error(f"unknown type '{type_name}'", decl)
            return

        if sym.kind != SymbolKind.TYPE:
            self.error(f"'{type_name}' is not a type", decl)
            return

        if not isinstance(sym.ada_type, RecordType):
            self.error(f"'{type_name}' is not a record type", decl)
            return

        # Process each component clause
        for comp_clause in decl.component_clauses:
            position = self._eval_static_expr(comp_clause.position)
            first_bit = self._eval_static_expr(comp_clause.first_bit)
            last_bit = self._eval_static_expr(comp_clause.last_bit)

            # Find the component in the record type
            found = False
            for comp in sym.ada_type.components:
                if comp.name.lower() == comp_clause.name.lower():
                    # Update the component's bit layout
                    comp.offset_bits = position * 8 + first_bit
                    # Store representation size on component, not on shared type
                    comp.size_bits = last_bit - first_bit + 1
                    found = True
                    break

            if not found:
                self.error(
                    f"'{comp_clause.name}' is not a component of '{type_name}'",
                    decl
                )

    def _analyze_enumeration_representation_clause(
        self, decl: EnumerationRepresentationClause
    ) -> None:
        """Analyze an enumeration representation clause."""
        # Get the enumeration type
        type_name = ""
        if isinstance(decl.type_name, Identifier):
            type_name = decl.type_name.name
        elif hasattr(decl.type_name, "name"):
            type_name = decl.type_name.name

        sym = self.symbols.lookup(type_name)
        if sym is None:
            self.error(f"unknown type '{type_name}'", decl)
            return

        if sym.kind != SymbolKind.TYPE:
            self.error(f"'{type_name}' is not a type", decl)
            return

        if not isinstance(sym.ada_type, EnumerationType):
            self.error(f"'{type_name}' is not an enumeration type", decl)
            return

        # Process each value assignment
        for lit_name, lit_value in decl.values:
            value = self._eval_static_expr(lit_value)

            # Update the position value for this literal
            # EnumerationType.positions is a dict mapping literal name to value
            if sym.ada_type.positions is not None:
                # Find the literal (case-insensitive)
                for lit in sym.ada_type.literals:
                    if lit.lower() == lit_name.lower():
                        sym.ada_type.positions[lit] = value
                        break

    # =========================================================================
    # Task and Protected Types
    # =========================================================================

    def _analyze_task_type_decl(self, decl: TaskTypeDecl) -> None:
        """Analyze a task type declaration."""
        # Check if we're completing an incomplete type
        existing = self.symbols.lookup_local(decl.name)
        if existing is not None:
            # Allow completing an incomplete or private type with a task type
            if (existing.kind == SymbolKind.TYPE and
                existing.ada_type and
                existing.ada_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE)):
                # This is completing an incomplete/private type - will update below
                pass
            else:
                self.error(f"task type '{decl.name}' is already defined", decl)
                return

        # Build entry information
        entries = []
        for entry_decl in decl.entries:
            param_types = []
            for param in entry_decl.parameters:
                param_type = self._resolve_type(param.type_mark)
                # Always count the parameter even if type can't be resolved
                # (e.g., for generic formal types)
                param_types.append(param_type)

            family_type = None
            if entry_decl.family_index:
                family_type = self._resolve_type(entry_decl.family_index)

            entries.append(EntryInfo(
                name=entry_decl.name,
                parameter_types=param_types,
                family_index_type=family_type,
            ))

        # Create the task type
        task_type = TaskType(
            name=decl.name,
            entries=entries,
        )

        if existing is not None and existing.ada_type and existing.ada_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
            # Completing an incomplete/private type - update the existing symbol
            existing.kind = SymbolKind.TASK_TYPE
            existing.ada_type = task_type
            existing.definition = decl
        else:
            symbol = Symbol(
                name=decl.name,
                kind=SymbolKind.TASK_TYPE,
                ada_type=task_type,
                definition=decl,
            )
            self.symbols.define(symbol)

        # Enter task scope to analyze entries and declarations
        self.symbols.enter_scope(decl.name)

        # Add entries to scope
        for entry_decl in decl.entries:
            self._analyze_entry_decl(entry_decl)

        # Analyze other declarations
        for inner_decl in decl.declarations:
            self._analyze_declaration(inner_decl)

        self.symbols.leave_scope()

    def _analyze_task_body(self, body: TaskBody) -> None:
        """Analyze a task body."""
        # Look up the task type
        task_sym = self.symbols.lookup(body.name)

        if task_sym is None:
            # Single task (no separate type declaration)
            task_type = TaskType(name=body.name, is_single_task=True)
            symbol = Symbol(
                name=body.name,
                kind=SymbolKind.TASK,
                ada_type=task_type,
                definition=body,
            )
            self.symbols.define(symbol)
            task_sym = symbol
        elif task_sym.kind != SymbolKind.TASK_TYPE:
            self.error(f"'{body.name}' is not a task type", body)
            return

        # Enter task body scope
        self.symbols.enter_scope(body.name)

        # Set task context flags
        old_in_task_body = self.in_task_body
        old_current_task = self.current_task
        self.in_task_body = True
        self.current_task = task_sym

        # Analyze declarations
        for decl in body.declarations:
            self._analyze_declaration(decl)

        # Analyze statements
        for stmt in body.statements:
            self._analyze_statement(stmt)

        # Analyze exception handlers
        for handler in body.handled_exception_handlers:
            self._analyze_exception_handler(handler)

        # Restore task context
        self.in_task_body = old_in_task_body
        self.current_task = old_current_task

        self.symbols.leave_scope()

    def _analyze_entry_decl(self, decl: EntryDecl) -> None:
        """Analyze an entry declaration."""
        if self.symbols.is_defined_locally(decl.name):
            self.error(f"entry '{decl.name}' is already defined", decl)
            return

        # Build parameter list
        params = []
        for param in decl.parameters:
            param_type = self._resolve_type(param.type_mark)
            for name in param.names:
                param_sym = Symbol(
                    name=name,
                    kind=SymbolKind.PARAMETER,
                    ada_type=param_type,
                    mode=param.mode,
                )
                params.append(param_sym)

        entry_sym = Symbol(
            name=decl.name,
            kind=SymbolKind.ENTRY,
            definition=decl,
            parameters=params,
        )
        self.symbols.define(entry_sym)

    def _analyze_protected_type_decl(self, decl: ProtectedTypeDecl) -> None:
        """Analyze a protected type declaration."""
        # Check if we're completing an incomplete type
        existing = self.symbols.lookup_local(decl.name)
        if existing is not None:
            # Allow completing an incomplete or private type with a protected type
            if (existing.kind == SymbolKind.TYPE and
                existing.ada_type and
                existing.ada_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE)):
                # This is completing an incomplete/private type - will update below
                pass
            else:
                self.error(f"protected type '{decl.name}' is already defined", decl)
                return

        # Build entry and operation information
        entries = []
        operations = []
        components = []

        for item in decl.items:
            if isinstance(item, EntryDecl):
                param_types = []
                for param in item.parameters:
                    param_type = self._resolve_type(param.type_mark)
                    # Always count the parameter even if type can't be resolved
                    param_types.append(param_type)
                entries.append(EntryInfo(
                    name=item.name,
                    parameter_types=param_types,
                ))
            elif isinstance(item, SubprogramDecl):
                param_types = []
                for param in item.parameters:
                    param_type = self._resolve_type(param.type_mark)
                    if param_type:
                        param_types.append(param_type)
                ret_type = None
                if item.is_function and item.return_type:
                    ret_type = self._resolve_type(item.return_type)
                operations.append(ProtectedOperation(
                    name=item.name,
                    kind="function" if item.is_function else "procedure",
                    parameter_types=param_types,
                    return_type=ret_type,
                ))
            elif isinstance(item, ObjectDecl):
                # Private component
                for name in item.names:
                    if isinstance(item.type_mark, SubtypeIndication):
                        comp_type = self._resolve_subtype_indication(item.type_mark)
                    else:
                        comp_type = self._resolve_type(item.type_mark)
                    if comp_type:
                        components.append(RecordComponent(
                            name=name,
                            component_type=comp_type,
                        ))

        # Create the protected type
        prot_type = ProtectedType(
            name=decl.name,
            entries=entries,
            operations=operations,
            components=components,
        )

        if existing is not None and existing.ada_type and existing.ada_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
            # Completing an incomplete/private type - update the existing symbol
            existing.kind = SymbolKind.PROTECTED_TYPE
            existing.ada_type = prot_type
            existing.definition = decl
        else:
            symbol = Symbol(
                name=decl.name,
                kind=SymbolKind.PROTECTED_TYPE,
                ada_type=prot_type,
                definition=decl,
            )
            self.symbols.define(symbol)

        # Enter scope for protected type
        self.symbols.enter_scope(decl.name)

        # Add entries and operations to scope
        for item in decl.items:
            if isinstance(item, EntryDecl):
                self._analyze_entry_decl(item)
            elif isinstance(item, SubprogramDecl):
                self._analyze_subprogram_decl(item)

        self.symbols.leave_scope()

    def _analyze_protected_body(self, body: ProtectedBody) -> None:
        """Analyze a protected body."""
        # Look up the protected type
        prot_sym = self.symbols.lookup(body.name)
        prot_type = None

        if prot_sym is None:
            # Single protected (no separate type declaration)
            prot_type = ProtectedType(name=body.name, is_single_protected=True)
            symbol = Symbol(
                name=body.name,
                kind=SymbolKind.PROTECTED,
                ada_type=prot_type,
                definition=body,
            )
            self.symbols.define(symbol)
        elif prot_sym.kind != SymbolKind.PROTECTED_TYPE:
            self.error(f"'{body.name}' is not a protected type", body)
            return
        else:
            prot_type = prot_sym.ada_type

        # Enter protected body scope
        self.symbols.enter_scope(body.name)

        # Add private components to scope (they're accessible in the body)
        if prot_type and hasattr(prot_type, 'components'):
            for comp in prot_type.components:
                comp_sym = Symbol(
                    name=comp.name,
                    kind=SymbolKind.VARIABLE,
                    ada_type=comp.component_type,
                )
                self.symbols.define(comp_sym)

        # Add entries to scope (for requeue targets)
        if prot_type and hasattr(prot_type, 'entries'):
            for entry in prot_type.entries:
                entry_sym = Symbol(
                    name=entry.name,
                    kind=SymbolKind.ENTRY,
                )
                self.symbols.define(entry_sym)

        # Get entry names from the protected type to identify entry bodies
        entry_names = set()
        if prot_type and hasattr(prot_type, 'entries'):
            for entry in prot_type.entries:
                entry_names.add(entry.name.lower())

        # Analyze each item in the body
        for item in body.items:
            if isinstance(item, EntryBody):
                self._analyze_entry_body(item)
            elif isinstance(item, SubprogramBody):
                # Check if this is an entry body (matched by name)
                subprog_name = item.spec.name.lower() if item.spec else ""
                if subprog_name in entry_names:
                    # Entry body - set flag for requeue
                    old_in_accept_or_entry = self.in_accept_or_entry
                    self.in_accept_or_entry = True
                    self._analyze_subprogram_body(item)
                    self.in_accept_or_entry = old_in_accept_or_entry
                else:
                    self._analyze_subprogram_body(item)
            else:
                self._analyze_declaration(item)

        self.symbols.leave_scope()

    def _analyze_entry_body(self, body: EntryBody) -> None:
        """Analyze an entry body in a protected type."""
        # Analyze the barrier condition if present
        if body.barrier:
            barrier_type = self._analyze_expr(body.barrier)
            if barrier_type:
                bool_type = PREDEFINED_TYPES.get("Boolean")
                if bool_type and not types_compatible(barrier_type, bool_type):
                    self.error(
                        f"entry barrier must be Boolean, got '{barrier_type.name}'",
                        body.barrier,
                    )

        # Enter entry body scope
        self.symbols.enter_scope(body.name)

        # Add parameters to scope
        for param in body.parameters:
            param_type = self._resolve_type(param.type_mark)
            for name in param.names:
                self.symbols.define(Symbol(
                    name=name,
                    kind=SymbolKind.VARIABLE,
                    ada_type=param_type,
                ))

        # Analyze declarations
        for decl in body.decls:
            self._analyze_declaration(decl)

        # Analyze statements with in_accept_or_entry set (for requeue)
        old_in_accept_or_entry = self.in_accept_or_entry
        self.in_accept_or_entry = True
        for stmt in body.stmts:
            self._analyze_statement(stmt)
        self.in_accept_or_entry = old_in_accept_or_entry

        self.symbols.leave_scope()

    def _analyze_body_stub(self, stub: BodyStub) -> None:
        """Analyze a body stub declaration (is separate).

        A body stub declares a subprogram, package, task, or protected unit
        whose body will be provided in a separate compilation unit.
        We define the symbol here so it can be referenced before the body is seen.
        """
        if stub.kind == "procedure":
            # Define a procedure symbol
            symbol = Symbol(
                name=stub.name,
                kind=SymbolKind.PROCEDURE,
                parameters=[],  # No parameters available from stub
                definition=stub,
            )
            self.symbols.define(symbol)
        elif stub.kind == "function":
            # Define a function symbol
            symbol = Symbol(
                name=stub.name,
                kind=SymbolKind.FUNCTION,
                parameters=[],  # No parameters available from stub
                return_type=None,
                definition=stub,
            )
            self.symbols.define(symbol)
        elif stub.kind == "package":
            # Define a package symbol
            symbol = Symbol(
                name=stub.name,
                kind=SymbolKind.PACKAGE,
                definition=stub,
            )
            self.symbols.define(symbol)
        elif stub.kind == "task":
            # Define a task symbol
            task_type = TaskType(name=stub.name, is_single_task=True)
            symbol = Symbol(
                name=stub.name,
                kind=SymbolKind.TASK,
                ada_type=task_type,
                definition=stub,
            )
            self.symbols.define(symbol)
        elif stub.kind == "protected":
            # Define a protected symbol
            prot_type = ProtectedType(name=stub.name, is_single_protected=True)
            symbol = Symbol(
                name=stub.name,
                kind=SymbolKind.PROTECTED,
                ada_type=prot_type,
                definition=stub,
            )
            self.symbols.define(symbol)

    # =========================================================================
    # Type Building
    # =========================================================================

    def _build_type(self, name: str, type_def: Optional[TypeDef], is_tagged: bool = False) -> Optional[AdaType]:
        """Build an AdaType from a type definition."""
        if type_def is None:
            # Incomplete type
            return None

        if isinstance(type_def, IntegerTypeDef):
            return self._build_integer_type(name, type_def)
        elif isinstance(type_def, ModularTypeDef):
            return self._build_modular_type(name, type_def)
        elif isinstance(type_def, EnumerationTypeDef):
            return self._build_enumeration_type(name, type_def)
        elif isinstance(type_def, ArrayTypeDef):
            return self._build_array_type(name, type_def)
        elif isinstance(type_def, RecordTypeDef):
            return self._build_record_type(name, type_def, is_tagged)
        elif isinstance(type_def, AccessTypeDef):
            return self._build_access_type(name, type_def)
        elif isinstance(type_def, AccessSubprogramTypeDef):
            return self._build_access_subprogram_type(name, type_def)
        elif isinstance(type_def, DerivedTypeDef):
            return self._build_derived_type(name, type_def)
        elif isinstance(type_def, InterfaceTypeDef):
            return self._build_interface_type(name, type_def)
        elif isinstance(type_def, PrivateTypeDef):
            return self._build_private_type(name, type_def)
        elif isinstance(type_def, RealTypeDef):
            return self._build_real_type(name, type_def)

        return None

    def _build_integer_type(
        self, name: str, type_def: IntegerTypeDef
    ) -> IntegerType:
        """Build an integer type."""
        low = 0
        high = 0
        if type_def.range_constraint:
            low = self._eval_static_expr(type_def.range_constraint.low)
            high = self._eval_static_expr(type_def.range_constraint.high)

        return IntegerType(name=name, size_bits=0, low=low, high=high)

    def _build_modular_type(
        self, name: str, type_def: ModularTypeDef
    ) -> ModularType:
        """Build a modular (unsigned wraparound) type."""
        modulus = self._eval_static_expr(type_def.modulus)
        if modulus <= 0:
            self.error(f"modulus must be positive, got {modulus}", type_def.modulus)
            modulus = 256  # Default to byte
        return ModularType(name=name, size_bits=0, modulus=modulus)

    def _build_real_type(
        self, name: str, type_def: RealTypeDef
    ) -> FloatType:
        """Build a floating-point or fixed-point type."""
        digits = 6  # Default precision
        range_first = None
        range_last = None

        if type_def.is_floating and type_def.digits_expr:
            digits = self._eval_static_expr(type_def.digits_expr)

        if type_def.range_constraint:
            # Try to evaluate bounds as floats
            try:
                range_first = float(self._eval_static_expr(type_def.range_constraint.low))
                range_last = float(self._eval_static_expr(type_def.range_constraint.high))
            except (TypeError, ValueError):
                pass

        return FloatType(
            name=name,
            kind=TypeKind.FLOAT,
            size_bits=32 if digits <= 6 else 64,
            digits=digits,
            range_first=range_first,
            range_last=range_last,
        )

    def _build_enumeration_type(
        self, name: str, type_def: EnumerationTypeDef
    ) -> EnumerationType:
        """Build an enumeration type."""
        return EnumerationType(
            name=name,
            size_bits=0,
            literals=type_def.literals,
        )

    def _build_array_type(
        self, name: str, type_def: ArrayTypeDef
    ) -> ArrayType:
        """Build an array type."""
        # Resolve component type
        component_type = self._resolve_type(type_def.component_type)

        # Resolve index types and bounds
        index_types: list[AdaType] = []
        bounds: list[tuple[int, int]] = []

        for idx_subtype in type_def.index_subtypes:
            if isinstance(idx_subtype, RangeExpr):
                # Constrained with explicit range - may be dynamic for local variables
                low = self._try_eval_static(idx_subtype.low)
                high = self._try_eval_static(idx_subtype.high)
                # Analyze expressions even if not static
                self._analyze_expr(idx_subtype.low)
                self._analyze_expr(idx_subtype.high)
                if low is not None and high is not None:
                    bounds.append((low, high))
                else:
                    # Dynamic bounds - mark as unconstrained at compile time
                    bounds.append((0, 0))  # Placeholder for dynamic bounds
                index_types.append(PREDEFINED_TYPES["Integer"])
            else:
                # Type or subtype mark
                idx_type = self._resolve_type(idx_subtype)
                if idx_type:
                    index_types.append(idx_type)

        return ArrayType(
            name=name,
            size_bits=0,
            index_types=index_types,
            component_type=component_type,
            is_constrained=type_def.is_constrained,
            bounds=bounds if type_def.is_constrained else [],
        )

    def _build_record_type(
        self, name: str, type_def: RecordTypeDef, is_tagged: bool = False
    ) -> RecordType:
        """Build a record type."""
        components: list[RecordComponent] = []

        for comp_decl in type_def.components:
            comp_type = self._resolve_type(comp_decl.type_mark)
            # If type couldn't be resolved, use a placeholder type
            if comp_type is None:
                comp_type = IntegerType(name="_unknown", size_bits=16, low=0, high=0)
            for comp_name in comp_decl.names:
                components.append(
                    RecordComponent(name=comp_name, component_type=comp_type)
                )

        # Build variant part if present
        variant_part = None
        if type_def.variant_part is not None:
            variants: list[VariantInfo] = []
            for variant in type_def.variant_part.variants:
                var_components: list[RecordComponent] = []
                for comp_decl in variant.components:
                    comp_type = self._resolve_type(comp_decl.type_mark)
                    if comp_type is None:
                        comp_type = IntegerType(name="_unknown", size_bits=16, low=0, high=0)
                    for comp_name in comp_decl.names:
                        var_components.append(
                            RecordComponent(name=comp_name, component_type=comp_type)
                        )
                # Extract choice values (simplified - stores the choice AST nodes)
                variants.append(VariantInfo(choices=variant.choices, components=var_components))
            variant_part = VariantPartInfo(
                discriminant_name=type_def.variant_part.discriminant,
                variants=variants,
            )

        # Check if record is limited
        is_limited = getattr(type_def, 'is_limited', False)

        return RecordType(name=name, size_bits=0, components=components,
                          variant_part=variant_part, is_limited=is_limited,
                          is_tagged=is_tagged)

    def _build_access_type(
        self, name: str, type_def: AccessTypeDef
    ) -> AccessType:
        """Build an access (pointer) type."""
        designated = self._resolve_type(type_def.designated_type)

        return AccessType(
            name=name,
            size_bits=16,  # Z80 address
            designated_type=designated,
            is_access_all=type_def.is_access_all,
            is_access_constant=type_def.is_access_constant,
        )

    def _build_access_subprogram_type(
        self, name: str, type_def: AccessSubprogramTypeDef
    ) -> AccessSubprogramType:
        """Build an access-to-subprogram (function pointer) type."""
        # Resolve parameter types
        param_types: list[AdaType] = []
        for param_spec in type_def.parameters:
            param_type = self._resolve_type(param_spec.type_mark)
            if param_type:
                # Add one entry per parameter name (for multiple params of same type)
                for _ in param_spec.names:
                    param_types.append(param_type)

        # Resolve return type
        return_type = None
        if type_def.is_function and type_def.return_type:
            return_type = self._resolve_type(type_def.return_type)

        return AccessSubprogramType(
            name=name,
            is_function=type_def.is_function,
            parameter_types=param_types,
            return_type=return_type,
            is_not_null=type_def.is_not_null,
            is_access_protected=type_def.is_access_protected,
        )

    def _build_derived_type(
        self, name: str, type_def: DerivedTypeDef
    ) -> Optional[AdaType]:
        """Build a derived type."""
        parent = self._resolve_type(type_def.parent_type)
        if parent is None:
            return None

        # For now, just copy the parent type with new name
        # Full implementation would handle record extensions
        if isinstance(parent, IntegerType):
            return IntegerType(
                name=name,
                size_bits=parent.size_bits,
                low=parent.low,
                high=parent.high,
                base_type=parent,  # Link to parent for derived type compatibility
            )

        # Handle derivation from enumeration type (e.g., type MyBool is new Boolean)
        # In Ada, the derived type has the same literals but is a distinct type.
        # The literals are overloaded to work with both parent and derived types.
        if isinstance(parent, EnumerationType):
            return EnumerationType(
                name=name,
                size_bits=parent.size_bits,
                literals=parent.literals.copy(),
                positions=parent.positions.copy(),
                base_type=parent,  # Link to parent for derived type compatibility
            )

        # Handle tagged type derivation with record extension and interfaces
        if isinstance(parent, RecordType) and parent.is_tagged:
            # Build components from record extension
            components: list[RecordComponent] = []
            if type_def.record_extension:
                for comp_decl in type_def.record_extension.components:
                    comp_type = self._resolve_type(comp_decl.type_mark)
                    # If type couldn't be resolved, use a placeholder type
                    if comp_type is None:
                        comp_type = IntegerType(name="_unknown", size_bits=16, low=0, high=0)
                    for comp_name in comp_decl.names:
                        components.append(
                            RecordComponent(name=comp_name, component_type=comp_type)
                        )

            # Resolve interfaces
            interfaces: list[InterfaceType] = []
            for iface_expr in type_def.interfaces:
                iface_type = self._resolve_type(iface_expr)
                if isinstance(iface_type, InterfaceType):
                    interfaces.append(iface_type)

            # Propagate controlled type status from parent
            is_controlled = parent.is_controlled or parent.needs_finalization()
            is_limited_controlled = parent.is_limited_controlled

            # Propagate limited status from parent or from explicit declaration
            is_limited = getattr(type_def, 'is_limited', False) or parent.is_limited or parent.is_limited_type()

            return RecordType(
                name=name,
                is_tagged=True,
                parent_type=parent,
                components=components,
                interfaces=interfaces,
                is_controlled=is_controlled,
                is_limited_controlled=is_limited_controlled,
                is_limited=is_limited,
            )

        # Handle derivation from interface type with record extension
        # e.g., type Circle is new Shape with record Radius : Float; end record;
        if isinstance(parent, InterfaceType) and type_def.record_extension:
            # Build components from record extension
            components: list[RecordComponent] = []
            for comp_decl in type_def.record_extension.components:
                comp_type = self._resolve_type(comp_decl.type_mark)
                if comp_type is None:
                    comp_type = IntegerType(name="_unknown", size_bits=16, low=0, high=0)
                for comp_name in comp_decl.names:
                    components.append(
                        RecordComponent(name=comp_name, component_type=comp_type)
                    )

            # Resolve additional interfaces
            interfaces: list[InterfaceType] = [parent]  # The parent interface
            for iface_expr in type_def.interfaces:
                iface_type = self._resolve_type(iface_expr)
                if isinstance(iface_type, InterfaceType):
                    interfaces.append(iface_type)

            is_limited = getattr(type_def, 'is_limited', False) or parent.is_limited

            return RecordType(
                name=name,
                is_tagged=True,
                parent_type=None,  # No record parent, only interfaces
                components=components,
                interfaces=interfaces,
                is_limited=is_limited,
            )

        # Handle derivation from array type
        if isinstance(parent, ArrayType):
            return ArrayType(
                name=name,
                size_bits=parent.size_bits,
                index_types=parent.index_types,
                component_type=parent.component_type,
                is_constrained=parent.is_constrained,
                bounds=parent.bounds,
                base_type=parent,  # Link to parent for derived type compatibility
            )

        return parent

    def _build_interface_type(
        self, name: str, type_def: InterfaceTypeDef
    ) -> InterfaceType:
        """Build an interface type."""
        # Resolve parent interfaces
        parent_interfaces: list[InterfaceType] = []
        for parent_expr in type_def.parent_interfaces:
            parent_type = self._resolve_type(parent_expr)
            if isinstance(parent_type, InterfaceType):
                parent_interfaces.append(parent_type)

        return InterfaceType(
            name=name,
            is_limited=type_def.is_limited,
            is_synchronized=type_def.is_synchronized,
            is_task=type_def.is_task,
            is_protected=type_def.is_protected,
            parent_interfaces=parent_interfaces,
        )

    def _build_private_type(
        self, name: str, type_def: PrivateTypeDef
    ) -> AdaType:
        """Build a private type placeholder.

        A private type declaration (type T is private;) creates an opaque
        type that will be completed with a full type definition in the
        private part of the package.
        """
        return AdaType(
            name=name,
            kind=TypeKind.PRIVATE,
        )

    # =========================================================================
    # Primitive Operation Inheritance
    # =========================================================================

    def _inherit_primitive_operations(
        self, derived_type: AdaType, parent_type: AdaType, parent_type_expr: Expr
    ) -> None:
        """Inherit primitive operations from parent type to derived type.

        When TYPE NEW_T IS NEW A.T is declared, NEW_T inherits the primitive
        operations of A.T. A primitive operation is a subprogram declared in
        the same package as the type that has that type as parameter or return type.
        """
        # Find the package containing the parent type
        parent_package: Optional[Symbol] = None

        if isinstance(parent_type_expr, SelectedName):
            # A.T - look up package A
            prefix_name = self._get_identifier_name(parent_type_expr.prefix)
            if prefix_name:
                parent_package = self.symbols.lookup(prefix_name)
        else:
            # Simple name - parent type is in current or enclosing scope
            # Look for a package in scope that contains this type
            # For now, skip inheritance for types not from explicit packages
            return

        if parent_package is None or parent_package.kind != SymbolKind.PACKAGE:
            return

        # Find primitive operations in the parent package
        # A primitive operation has the parent type as parameter or return type
        for sym_name, sym in parent_package.public_symbols.items():
            if sym.kind not in (SymbolKind.FUNCTION, SymbolKind.PROCEDURE):
                continue

            # Check if this is a primitive operation of parent_type
            is_primitive = False

            # Check return type (for functions)
            if sym.return_type and same_type(sym.return_type, parent_type):
                is_primitive = True

            # Check parameter types
            for param in sym.parameters:
                if param.ada_type and same_type(param.ada_type, parent_type):
                    is_primitive = True
                    break

            if not is_primitive:
                continue

            # Create an inherited version of this primitive
            # The inherited primitive has the same signature but with
            # derived_type substituted for parent_type
            inherited_sym = Symbol(
                name=sym.name,
                kind=sym.kind,
                # Return type: substitute parent_type with derived_type
                return_type=derived_type if sym.return_type and same_type(sym.return_type, parent_type) else sym.return_type,
                parameters=[],
                definition=sym.definition,
            )

            # Copy parameters, substituting types as needed
            for param in sym.parameters:
                param_type = derived_type if param.ada_type and same_type(param.ada_type, parent_type) else param.ada_type
                inherited_param = Symbol(
                    name=param.name,
                    kind=SymbolKind.PARAMETER,
                    ada_type=param_type,
                    mode=param.mode,
                    default_value=param.default_value,
                )
                inherited_sym.parameters.append(inherited_param)

            # Define the inherited primitive in current scope
            self.symbols.define(inherited_sym)

            # Also add to current package's public_symbols if we're in a package
            # This allows further derivation to find the inherited primitives
            if self.current_package:
                self.current_package.public_symbols[sym.name.lower()] = inherited_sym

    # =========================================================================
    # Type Resolution
    # =========================================================================

    def _resolve_type(self, type_expr: Expr) -> Optional[AdaType]:
        """Resolve a type expression to an AdaType."""
        if isinstance(type_expr, Identifier):
            type_name = type_expr.name
            # Check for generic formal type mapping (during instantiation)
            generic_formals = getattr(self, '_generic_formals', {})
            if type_name.lower() in generic_formals:
                actual = generic_formals[type_name.lower()]
                # The actual might be ActualParameter (wrapping value) or Identifier
                if hasattr(actual, 'value'):
                    actual = actual.value
                if isinstance(actual, Identifier):
                    return self.symbols.lookup_type(actual.name)
            return self.symbols.lookup_type(type_name)
        elif isinstance(type_expr, SelectedName):
            # Package.Type
            prefix_name = self._get_identifier_name(type_expr.prefix)
            if prefix_name:
                symbol = self.symbols.lookup_selected(
                    prefix_name, type_expr.selector
                )
                if symbol and symbol.ada_type:
                    return symbol.ada_type
        elif isinstance(type_expr, SubtypeIndication):
            # Delegate to subtype indication resolver
            return self._resolve_subtype_indication(type_expr)
        elif isinstance(type_expr, IndexedComponent):
            # Constrained type: REC(2) parses as IndexedComponent
            # Extract the base type from the prefix
            return self._resolve_type(type_expr.prefix)
        elif isinstance(type_expr, Slice):
            # Constrained array type: ARR(1..10) parses as Slice
            # Extract the base type from the prefix
            return self._resolve_type(type_expr.prefix)
        return None

    def _resolve_subtype_indication(
        self, subtype_ind: SubtypeIndication
    ) -> Optional[AdaType]:
        """Resolve a subtype indication.

        The type_mark can be:
        - Identifier: simple type name (e.g., Integer)
        - SelectedName: qualified name (e.g., Ada.Integer_Text_IO)
        - Slice: constrained array (e.g., ARRT(1..10)) - prefix is the type
        - IndexedComponent: constrained type (e.g., Vector(1, 10)) - prefix is the type
        """
        type_mark = subtype_ind.type_mark

        # Handle constrained array/type syntax: Type(Constraint)
        # The parser produces a Slice or IndexedComponent for this
        if isinstance(type_mark, Slice):
            base_type = self._resolve_type(type_mark.prefix)
            # If base is unconstrained array, create constrained version with bounds
            if isinstance(base_type, ArrayType) and not base_type.is_constrained:
                # Extract bounds from slice range
                slice_range = type_mark.range_expr
                if isinstance(slice_range, RangeExpr):
                    low = self._try_eval_static(slice_range.low)
                    high = self._try_eval_static(slice_range.high)
                    # Ensure low and high are actual integers
                    if isinstance(low, int) and isinstance(high, int):
                        # Create constrained array type with bounds
                        return ArrayType(
                            name=f"{base_type.name}({low}..{high})",
                            kind=base_type.kind,
                            size_bits=(high - low + 1) * (base_type.component_type.size_bits if base_type.component_type else 8),
                            component_type=base_type.component_type,
                            index_types=base_type.index_types,
                            bounds=[(low, high)],
                            is_constrained=True,
                            base_type=base_type,
                        )
            return base_type
        elif isinstance(type_mark, IndexedComponent):
            base_type = self._resolve_type(type_mark.prefix)
            # If base is unconstrained array, create constrained version
            if isinstance(base_type, ArrayType) and not base_type.is_constrained:
                # Extract bounds from indices (could be range or discrete values)
                if type_mark.indices and len(type_mark.indices) == 1:
                    idx = type_mark.indices[0]
                    if isinstance(idx, RangeExpr):
                        low = self._try_eval_static(idx.low)
                        high = self._try_eval_static(idx.high)
                        # Ensure low and high are actual integers
                        if isinstance(low, int) and isinstance(high, int):
                            return ArrayType(
                                name=f"{base_type.name}({low}..{high})",
                                kind=base_type.kind,
                                size_bits=(high - low + 1) * (base_type.component_type.size_bits if base_type.component_type else 8),
                                component_type=base_type.component_type,
                                index_types=base_type.index_types,
                                bounds=[(low, high)],
                                is_constrained=True,
                                base_type=base_type,
                            )
            return base_type

        return self._resolve_type(type_mark)

    def _get_identifier_name(self, expr: Expr) -> Optional[str]:
        """Get the name from an identifier expression."""
        if isinstance(expr, Identifier):
            return expr.name
        return None

    # =========================================================================
    # Statements
    # =========================================================================

    def _analyze_statement(self, stmt: Stmt) -> None:
        """Analyze a statement."""
        if isinstance(stmt, NullStmt):
            pass  # Nothing to check
        elif isinstance(stmt, AssignmentStmt):
            self._analyze_assignment(stmt)
        elif isinstance(stmt, IfStmt):
            self._analyze_if_stmt(stmt)
        elif isinstance(stmt, CaseStmt):
            self._analyze_case_stmt(stmt)
        elif isinstance(stmt, LoopStmt):
            self._analyze_loop_stmt(stmt)
        elif isinstance(stmt, BlockStmt):
            self._analyze_block_stmt(stmt)
        elif isinstance(stmt, ExitStmt):
            self._analyze_exit_stmt(stmt)
        elif isinstance(stmt, ReturnStmt):
            self._analyze_return_stmt(stmt)
        elif isinstance(stmt, ExtendedReturnStmt):
            self._analyze_extended_return_stmt(stmt)
        elif isinstance(stmt, RaiseStmt):
            self._analyze_raise_stmt(stmt)
        elif isinstance(stmt, ProcedureCallStmt):
            self._analyze_procedure_call(stmt)
        elif isinstance(stmt, PragmaStmt):
            self._analyze_pragma(stmt)
        elif isinstance(stmt, DelayStmt):
            self._analyze_delay_stmt(stmt)
        elif isinstance(stmt, AcceptStmt):
            self._analyze_accept_stmt(stmt)
        elif isinstance(stmt, SelectStmt):
            self._analyze_select_stmt(stmt)
        elif isinstance(stmt, RequeueStmt):
            self._analyze_requeue_stmt(stmt)
        elif isinstance(stmt, AbortStmt):
            self._analyze_abort_stmt(stmt)
        elif isinstance(stmt, ParallelBlockStmt):
            self._analyze_parallel_block(stmt)

    def _analyze_parallel_block(self, stmt: ParallelBlockStmt) -> None:
        """Analyze an Ada 2022 parallel block statement."""
        # Analyze each parallel sequence
        for sequence in stmt.sequences:
            for s in sequence:
                self._analyze_statement(s)

    def _analyze_pragma(self, stmt: PragmaStmt) -> None:
        """Analyze a pragma statement."""
        pragma_name = stmt.name.lower()

        if pragma_name == "import":
            # pragma Import(Convention, Entity, External_Name);
            # Used to import external (assembly) routines
            if len(stmt.args) >= 2:
                # Get entity name
                entity = stmt.args[1]
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_imported = True
                        # External name is optional
                        if len(stmt.args) >= 3:
                            ext_name = stmt.args[2]
                            if isinstance(ext_name, StringLiteral):
                                sym.external_name = ext_name.value
                            elif isinstance(ext_name, Identifier):
                                sym.external_name = ext_name.name

        elif pragma_name == "inline":
            # pragma Inline(subprogram);
            if stmt.args:
                entity = stmt.args[0]
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_inline = True

        elif pragma_name == "volatile":
            # pragma Volatile(variable);
            if stmt.args:
                entity = stmt.args[0]
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_volatile = True

        elif pragma_name == "atomic":
            # pragma Atomic(variable);
            # Atomic implies volatile behavior plus indivisible access
            if stmt.args:
                entity = stmt.args[0]
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_atomic = True
                        sym.is_volatile = True  # Atomic implies volatile

        elif pragma_name == "no_return":
            # pragma No_Return(procedure);
            if stmt.args:
                entity = stmt.args[0]
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_no_return = True

        elif pragma_name == "pack":
            # pragma Pack(type);
            if stmt.args:
                entity = stmt.args[0]
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym and sym.ada_type:
                        sym.ada_type.is_packed = True
                        # Recalculate record layout with packing
                        if isinstance(sym.ada_type, RecordType):
                            sym.ada_type.size_bits = sym.ada_type._compute_size()

        elif pragma_name == "pure":
            # pragma Pure [(package_name)];
            # Package has no state, can be preelaborated
            if stmt.args:
                entity = stmt.args[0]
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_pure = True
            elif self.current_package:
                # If no argument, applies to enclosing package
                self.current_package.is_pure = True

        elif pragma_name == "preelaborate":
            # pragma Preelaborate [(package_name)];
            # Package can be elaborated before execution
            if stmt.args:
                entity = stmt.args[0]
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_preelaborate = True
            elif self.current_package:
                self.current_package.is_preelaborate = True

        elif pragma_name == "elaborate_body":
            # pragma Elaborate_Body;
            # Package body must be elaborated immediately after spec
            if self.current_package:
                self.current_package.requires_body = True

        elif pragma_name == "suppress":
            # pragma Suppress(Check_Name [, On => Entity]);
            # Disable specified checks - note: we don't fully implement this
            pass  # Silently accept

        elif pragma_name == "unsuppress":
            # pragma Unsuppress(Check_Name [, On => Entity]);
            # Re-enable specified checks
            pass  # Silently accept

        # Other pragmas are silently ignored for now

    def _analyze_assignment(self, stmt: AssignmentStmt) -> None:
        """Analyze an assignment statement."""
        target_type = self._analyze_expr(stmt.target)
        # Set target type for @ (target name) support in Ada 2022
        old_target_type = self.current_assignment_target_type
        self.current_assignment_target_type = target_type
        # Pass target_type as expected type for overload resolution (enum literals)
        value_type = self._analyze_expr(stmt.value, expected_type=target_type)
        self.current_assignment_target_type = old_target_type

        # Check that target is assignable (variable, not constant)
        if isinstance(stmt.target, Identifier):
            symbol = self.symbols.lookup(stmt.target.name)
            if symbol:
                if symbol.is_constant:
                    self.error(
                        f"cannot assign to constant '{symbol.name}'", stmt
                    )
                if symbol.kind == SymbolKind.PARAMETER:
                    if symbol.mode == "in":
                        self.error(
                            f"cannot assign to 'in' parameter '{symbol.name}'",
                            stmt,
                        )

        # Check for limited type - cannot assign limited types
        if target_type:
            if hasattr(target_type, 'is_limited_type') and target_type.is_limited_type():
                self.error(
                    f"cannot assign to variable of limited type '{target_type.name}'",
                    stmt,
                )
            elif hasattr(target_type, 'is_limited') and target_type.is_limited:
                self.error(
                    f"cannot assign to variable of limited type '{target_type.name}'",
                    stmt,
                )
            elif hasattr(target_type, 'is_limited_controlled') and target_type.is_limited_controlled:
                self.error(
                    f"cannot assign to variable of limited controlled type '{target_type.name}'",
                    stmt,
                )

        # Type check
        if target_type and value_type:
            if not types_compatible(target_type, value_type):
                self.error(
                    f"type mismatch in assignment: cannot assign "
                    f"'{value_type.name}' to '{target_type.name}'",
                    stmt,
                )

    def _analyze_if_stmt(self, stmt: IfStmt) -> None:
        """Analyze an if statement."""
        cond_type = self._analyze_expr(stmt.condition)
        self._check_boolean(cond_type, stmt.condition)

        for s in stmt.then_stmts:
            self._analyze_statement(s)

        for cond, stmts in stmt.elsif_parts:
            cond_type = self._analyze_expr(cond)
            self._check_boolean(cond_type, cond)
            for s in stmts:
                self._analyze_statement(s)

        for s in stmt.else_stmts:
            self._analyze_statement(s)

    def _analyze_case_stmt(self, stmt: CaseStmt) -> None:
        """Analyze a case statement."""
        expr_type = self._analyze_expr(stmt.expr)

        # Case expression must be discrete
        if expr_type and not expr_type.is_discrete():
            self.error("case expression must be discrete type", stmt.expr)

        for alt in stmt.alternatives:
            for s in alt.statements:
                self._analyze_statement(s)

    def _analyze_loop_stmt(self, stmt: LoopStmt) -> None:
        """Analyze a loop statement."""
        old_in_loop = self.in_loop
        self.in_loop = True

        # Track loop label for exit validation
        self.loop_labels.append(stmt.label.lower() if stmt.label else None)

        if stmt.iteration_scheme:
            if isinstance(stmt.iteration_scheme, WhileScheme):
                cond_type = self._analyze_expr(stmt.iteration_scheme.condition)
                self._check_boolean(cond_type, stmt.iteration_scheme.condition)
            elif isinstance(stmt.iteration_scheme, ForScheme):
                # Enter scope for loop variable
                self.symbols.enter_scope()
                iterator = stmt.iteration_scheme.iterator
                iter_type = self._analyze_expr(iterator.iterable)

                # For "for X of Array" loops, get the element type
                loop_var_type = iter_type
                is_constant = True  # Loop variable is normally constant

                if iterator.is_of_iterator and iter_type:
                    # "for Element of Container" - get element type
                    if isinstance(iter_type, ArrayType) and iter_type.component_type:
                        loop_var_type = iter_type.component_type
                    elif hasattr(iter_type, 'component_type') and iter_type.component_type:
                        loop_var_type = iter_type.component_type
                    else:
                        # Fall back to Integer for unknown container types
                        loop_var_type = PREDEFINED_TYPES.get("Integer")

                    # For-of loop variable is mutable if container is mutable
                    # Check if iterable is a non-constant variable
                    if isinstance(iterator.iterable, Identifier):
                        container_sym = self.symbols.lookup(iterator.iterable.name)
                        if container_sym and not getattr(container_sym, 'is_constant', False):
                            is_constant = False

                # Define loop variable
                loop_var = Symbol(
                    name=iterator.name,
                    kind=SymbolKind.VARIABLE,
                    ada_type=loop_var_type if loop_var_type else PREDEFINED_TYPES["Integer"],
                    is_constant=is_constant,
                )
                self.symbols.define(loop_var)

        for s in stmt.statements:
            self._analyze_statement(s)

        if isinstance(stmt.iteration_scheme, ForScheme):
            self.symbols.leave_scope()

        self.loop_labels.pop()
        self.in_loop = old_in_loop

    def _analyze_block_stmt(self, stmt: BlockStmt) -> None:
        """Analyze a block statement."""
        self.symbols.enter_scope()

        for decl in stmt.declarations:
            self._analyze_declaration(decl)

        for s in stmt.statements:
            self._analyze_statement(s)

        self.symbols.leave_scope()

    def _analyze_exit_stmt(self, stmt: ExitStmt) -> None:
        """Analyze an exit statement."""
        if not self.in_loop:
            self.error("exit statement must be inside a loop", stmt)

        # Validate loop label if specified
        if stmt.loop_label:
            label_lower = stmt.loop_label.lower()
            if label_lower not in self.loop_labels:
                self.error(f"exit references unknown loop label '{stmt.loop_label}'", stmt)

        if stmt.condition:
            cond_type = self._analyze_expr(stmt.condition)
            self._check_boolean(cond_type, stmt.condition)

    def _analyze_return_stmt(self, stmt: ReturnStmt) -> None:
        """Analyze a return statement."""
        if self.current_subprogram is None:
            self.error("return statement outside subprogram", stmt)
            return

        is_function = self.current_subprogram.kind in (
            SymbolKind.FUNCTION, SymbolKind.GENERIC_FUNCTION
        )
        if is_function:
            if stmt.value is None:
                self.error("function must return a value", stmt)
            else:
                value_type = self._analyze_expr(stmt.value)
                if value_type and self.current_subprogram.return_type:
                    if not types_compatible(
                        self.current_subprogram.return_type, value_type
                    ):
                        self.error(
                            f"return type mismatch: expected "
                            f"'{self.current_subprogram.return_type.name}', "
                            f"got '{value_type.name}'",
                            stmt,
                        )
        else:
            # Procedure
            if stmt.value is not None:
                self.error("procedure cannot return a value", stmt)

    def _analyze_extended_return_stmt(self, stmt: ExtendedReturnStmt) -> None:
        """Analyze an extended return statement (Ada 2005)."""
        if self.current_subprogram is None:
            self.error("extended return statement outside subprogram", stmt)
            return

        is_function = self.current_subprogram.kind in (
            SymbolKind.FUNCTION, SymbolKind.GENERIC_FUNCTION
        )
        if not is_function:
            self.error("extended return statement only allowed in functions", stmt)
            return

        # Enter a new scope for the return object
        self.symbols.enter_scope("extended_return")

        # Resolve the return type
        return_type: Optional[AdaType] = None
        if stmt.type_mark:
            if isinstance(stmt.type_mark, SubtypeIndication):
                return_type = self._resolve_subtype_indication(stmt.type_mark)
            else:
                return_type = self._resolve_type(stmt.type_mark)
        elif self.current_subprogram.return_type:
            return_type = self.current_subprogram.return_type

        # Define the return object even if type resolution failed
        # (allows the body to be analyzed for other errors)
        if return_type or stmt.object_name:
            self.symbols.define(
                Symbol(
                    name=stmt.object_name,
                    kind=SymbolKind.VARIABLE,
                    ada_type=return_type,
                )
            )

        # Check type compatibility with function return type
        if return_type and self.current_subprogram.return_type:
            if not types_compatible(self.current_subprogram.return_type, return_type):
                self.error(
                    f"extended return type mismatch: expected "
                    f"'{self.current_subprogram.return_type.name}', "
                    f"got '{return_type.name}'",
                    stmt,
                )

        # Analyze initialization expression if present
        if stmt.init_expr:
            init_type = self._analyze_expr(stmt.init_expr)
            if init_type and return_type:
                if not types_compatible(return_type, init_type):
                    self.error(
                        f"initialization type mismatch: expected "
                        f"'{return_type.name}', got '{init_type.name}'",
                        stmt.init_expr,
                    )

        # Analyze the statements in the do block
        for inner_stmt in stmt.statements:
            self._analyze_statement(inner_stmt)

        # Leave the scope
        self.symbols.leave_scope()

    def _analyze_raise_stmt(self, stmt: RaiseStmt) -> None:
        """Analyze a raise statement."""
        if stmt.exception_name:
            if isinstance(stmt.exception_name, Identifier):
                symbol = self.symbols.lookup(stmt.exception_name.name)
                if symbol is None:
                    self.error(
                        f"exception '{stmt.exception_name.name}' not found",
                        stmt,
                    )
                elif symbol.kind != SymbolKind.EXCEPTION:
                    self.error(
                        f"'{stmt.exception_name.name}' is not an exception",
                        stmt,
                    )

    def _analyze_delay_stmt(self, stmt: DelayStmt) -> None:
        """Analyze a delay statement."""
        # Analyze the delay expression
        expr_type = self._analyze_expr(stmt.expression)
        if expr_type:
            # For delay, expect a Duration (numeric type)
            # For delay until, expect a Time type from Ada.Calendar
            # For now, accept any numeric type
            type_name = expr_type.name.lower()
            if stmt.is_until:
                # delay until expects a Time type (or similar)
                # Allow numeric types for now (until we have Ada.Calendar fully)
                pass
            else:
                # delay expects a Duration (numeric type)
                if type_name not in ("duration", "integer", "float", "universal_integer", "universal_real"):
                    self.error(
                        f"delay expression must be of numeric type, got '{expr_type.name}'",
                        stmt.expression,
                    )

    def _analyze_accept_stmt(self, stmt: AcceptStmt) -> None:
        """Analyze an accept statement for task rendezvous."""
        # Check we're inside a task body
        if not self.in_task_body:
            self.error("accept statement must be inside a task body", stmt)
            return

        # Look up the entry being accepted
        if self.current_task:
            entry_sym = None
            # Look for the entry in the task type's entries
            if self.current_task.ada_type and hasattr(self.current_task.ada_type, 'entries'):
                for entry_info in self.current_task.ada_type.entries:
                    if entry_info.name.lower() == stmt.entry_name.lower():
                        # Found entry - parameter count check based on entry_info
                        if len(stmt.parameters) != len(entry_info.parameter_types):
                            self.error(
                                f"wrong number of parameters in accept: expected {len(entry_info.parameter_types)}, "
                                f"got {len(stmt.parameters)}",
                                stmt,
                            )
                        entry_sym = entry_info
                        break

            # Also check current scope for entries (for single tasks defined inline)
            if entry_sym is None:
                for sym in self.symbols.current_scope_symbols():
                    if sym.name.lower() == stmt.entry_name.lower() and sym.kind == SymbolKind.ENTRY:
                        entry_sym = sym
                        break

            # Also check parent scope (entries might be in task type scope)
            if entry_sym is None:
                entry_sym = self.symbols.lookup(stmt.entry_name)
                if entry_sym and entry_sym.kind != SymbolKind.ENTRY:
                    entry_sym = None

            if entry_sym is None:
                self.error(f"entry '{stmt.entry_name}' not found in current task", stmt)

        # Analyze the statements in the accept body (requeue is valid here)
        old_in_accept = self.in_accept_or_entry
        self.in_accept_or_entry = True
        for s in stmt.statements:
            self._analyze_statement(s)
        self.in_accept_or_entry = old_in_accept

    def _analyze_select_stmt(self, stmt: SelectStmt) -> None:
        """Analyze a select statement."""
        # Check we're inside a task body for selective accept
        # (though select can also be used for timed entry calls outside tasks)

        for alt in stmt.alternatives:
            # Analyze guard if present
            if alt.guard:
                guard_type = self._analyze_expr(alt.guard)
                if guard_type and guard_type.name.lower() != "boolean":
                    self.error(
                        f"select guard must be Boolean, got '{guard_type.name}'",
                        alt.guard,
                    )

            # Analyze statements in alternative
            for s in alt.statements:
                self._analyze_statement(s)

    def _analyze_requeue_stmt(self, stmt: RequeueStmt) -> None:
        """Analyze a requeue statement."""
        # Requeue can only appear in accept statement or entry body
        if not self.in_accept_or_entry:
            self.error("requeue must be inside an accept statement or entry body", stmt)
            return

        # Analyze the entry name expression
        if isinstance(stmt.entry_name, Identifier):
            sym = self.symbols.lookup(stmt.entry_name.name)
            if sym is None:
                self.error(f"entry '{stmt.entry_name.name}' not found", stmt)
            elif sym.kind != SymbolKind.ENTRY:
                self.error(f"'{stmt.entry_name.name}' is not an entry", stmt)
        else:
            # Could be a selected component (task.entry)
            self._analyze_expr(stmt.entry_name)

    def _analyze_abort_stmt(self, stmt: AbortStmt) -> None:
        """Analyze an abort statement."""
        # Analyze each task name being aborted
        for task_expr in stmt.task_names:
            task_type = self._analyze_expr(task_expr)
            if task_type:
                if task_type.kind != TypeKind.TASK:
                    self.error(
                        f"abort requires a task object, got '{task_type.name}'",
                        task_expr,
                    )

    def _analyze_procedure_call(self, stmt: ProcedureCallStmt) -> None:
        """Analyze a procedure call statement."""
        # Resolve procedure name
        if isinstance(stmt.name, Identifier):
            symbol = self.symbols.lookup(stmt.name.name)
            if symbol is None:
                self.error(f"procedure '{stmt.name.name}' not found", stmt)
                return

            # Handle access-to-subprogram (function pointer) calls
            if symbol.kind in (SymbolKind.VARIABLE, SymbolKind.CONSTANT):
                if isinstance(symbol.ada_type, AccessSubprogramType):
                    if symbol.ada_type.is_function:
                        self.error(
                            f"'{stmt.name.name}' is an access-to-function, "
                            "cannot be called as a procedure",
                            stmt,
                        )
                        return
                    # Check arguments against access subprogram type
                    self._check_access_subprogram_call(
                        symbol.ada_type, stmt.args, stmt
                    )
                    return
                else:
                    self.error(f"'{stmt.name.name}' is not a procedure", stmt)
                    return

            if symbol.kind not in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION):
                self.error(f"'{stmt.name.name}' is not a procedure", stmt)
                return

            # Try to resolve overloaded call
            overloads = self.symbols.all_overloads(stmt.name.name)
            overloads = [o for o in overloads if o.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION)]

            if len(overloads) > 1:
                # Multiple overloads - find the best match
                best_match = self._resolve_overloaded_call(overloads, stmt.args, stmt)
                if best_match:
                    symbol = best_match

            # Check arguments
            self._check_call_arguments(symbol, stmt.args, stmt)

    def _resolve_overloaded_call(
        self, overloads: list[Symbol], args: list, node: ASTNode
    ) -> Optional[Symbol]:
        """Resolve an overloaded call to the best matching subprogram.

        Returns the best matching symbol, or None if no match found.
        """
        # Analyze argument types first
        arg_types = []
        for arg in args:
            if arg.value:
                arg_type = self._analyze_expr(arg.value)
                arg_types.append(arg_type)
            else:
                arg_types.append(None)

        # Find matching overloads
        matches = []
        for candidate in overloads:
            num_params = len(candidate.parameters)
            num_args = len(args)

            # Count default parameters
            num_with_defaults = sum(
                1 for p in candidate.parameters if p.default_value is not None
            )
            min_required = num_params - num_with_defaults

            if num_args < min_required or num_args > num_params:
                continue  # Wrong number of arguments

            # Check type compatibility for each argument
            all_match = True
            exact_matches = 0
            for i, (arg_type, param) in enumerate(zip(arg_types, candidate.parameters)):
                if arg_type is None or param.ada_type is None:
                    continue
                if not types_compatible(param.ada_type, arg_type):
                    all_match = False
                    break
                # Count exact type matches for preference
                if arg_type.name == param.ada_type.name:
                    exact_matches += 1

            if all_match:
                matches.append((candidate, exact_matches))

        if not matches:
            return None  # No match found, will report error later

        if len(matches) == 1:
            return matches[0][0]

        # Prefer the one with most exact matches
        matches.sort(key=lambda x: x[1], reverse=True)
        return matches[0][0]

    def _check_call_arguments(
        self, subprog: Symbol, args: list, node: ASTNode
    ) -> None:
        """Check that call arguments match parameters."""
        num_params = len(subprog.parameters)
        num_args = len(args)

        # Count parameters with default values
        num_with_defaults = sum(
            1 for p in subprog.parameters if p.default_value is not None
        )
        min_required = num_params - num_with_defaults

        if num_args < min_required or num_args > num_params:
            expected = str(num_params) if min_required == num_params else f"{min_required} to {num_params}"
            self.error(
                f"wrong number of arguments: expected {expected}, "
                f"got {num_args}",
                node,
            )
            return

        # Check if this is a generic instance - if so, skip strict type checking
        # because parameter types are generic formals, not substituted actuals
        is_generic_instance = getattr(subprog, 'generic_instance_of', None) is not None

        for arg, param in zip(args, subprog.parameters):
            if arg.value:
                # Pass expected type for context-dependent expressions (aggregates)
                arg_type = self._analyze_expr(arg.value, expected_type=param.ada_type)
                if arg_type and param.ada_type:
                    # For generic instances, accept any type for generic formal parameters
                    if is_generic_instance:
                        # Just analyze the argument, don't check type compatibility
                        continue
                    if not types_compatible(param.ada_type, arg_type):
                        self.error(
                            f"type mismatch for parameter '{param.name}': "
                            f"expected '{param.ada_type.name}', got '{arg_type.name}'",
                            arg.value,
                        )

    def _check_access_subprogram_call(
        self, access_type: AccessSubprogramType, args: list, node: ASTNode
    ) -> None:
        """Check arguments for a call through an access-to-subprogram type."""
        num_params = len(access_type.parameter_types)
        num_args = len(args)

        if num_args != num_params:
            self.error(
                f"wrong number of arguments: expected {num_params}, got {num_args}",
                node,
            )
            return

        for i, (arg, param_type) in enumerate(zip(args, access_type.parameter_types)):
            if arg.value:
                arg_type = self._analyze_expr(arg.value)
                if arg_type and param_type:
                    if not types_compatible(param_type, arg_type):
                        self.error(
                            f"type mismatch for parameter {i + 1}: "
                            f"expected '{param_type.name}', got '{arg_type.name}'",
                            arg.value,
                        )

    def _check_access_subprogram_call_expr(
        self, access_type: AccessSubprogramType, args: list, node: ASTNode
    ) -> None:
        """Check arguments for a function call through access type in expression context.

        Unlike _check_access_subprogram_call, the args here are raw expressions
        (from IndexedComponent.indices), not ArgumentAssociation objects.
        """
        num_params = len(access_type.parameter_types)
        num_args = len(args)

        if num_args != num_params:
            self.error(
                f"wrong number of arguments: expected {num_params}, got {num_args}",
                node,
            )
            return

        for i, (arg, param_type) in enumerate(zip(args, access_type.parameter_types)):
            arg_type = self._analyze_expr(arg)
            if arg_type and param_type:
                if not types_compatible(param_type, arg_type):
                    self.error(
                        f"type mismatch for parameter {i + 1}: "
                        f"expected '{param_type.name}', got '{arg_type.name}'",
                        arg,
                    )

    def _check_boolean(self, t: Optional[AdaType], node: ASTNode) -> None:
        """Check that a type is Boolean."""
        if t is None:
            return
        bool_type = PREDEFINED_TYPES.get("Boolean")
        if bool_type and not types_compatible(t, bool_type):
            self.error(f"expected Boolean, got '{t.name}'", node)

    # =========================================================================
    # Expressions
    # =========================================================================

    def _analyze_expr(self, expr: Expr, expected_type: Optional[AdaType] = None) -> Optional[AdaType]:
        """Analyze an expression and return its type.

        Args:
            expr: The expression to analyze
            expected_type: Optional expected type for overload resolution
        """
        if isinstance(expr, Identifier):
            return self._analyze_identifier(expr, expected_type)
        elif isinstance(expr, IntegerLiteral):
            return PREDEFINED_TYPES["Universal_Integer"]
        elif isinstance(expr, RealLiteral):
            return PREDEFINED_TYPES["Universal_Real"]
        elif isinstance(expr, StringLiteral):
            return PREDEFINED_TYPES["String"]
        elif isinstance(expr, CharacterLiteral):
            return PREDEFINED_TYPES["Character"]
        elif isinstance(expr, NullLiteral):
            return None  # Type determined by context
        elif isinstance(expr, BinaryExpr):
            return self._analyze_binary_expr(expr)
        elif isinstance(expr, UnaryExpr):
            return self._analyze_unary_expr(expr)
        elif isinstance(expr, RangeExpr):
            return self._analyze_range_expr(expr)
        elif isinstance(expr, IndexedComponent):
            return self._analyze_indexed_component(expr)
        elif isinstance(expr, SelectedName):
            return self._analyze_selected_name(expr)
        elif isinstance(expr, AttributeReference):
            return self._analyze_attribute_ref(expr)
        elif isinstance(expr, FunctionCall):
            return self._analyze_function_call(expr)
        elif isinstance(expr, TypeConversion):
            return self._analyze_type_conversion(expr)
        elif isinstance(expr, QualifiedExpr):
            return self._analyze_qualified_expr(expr)
        elif isinstance(expr, Parenthesized):
            # Parenthesized expression - just analyze the inner expression
            return self._analyze_expr(expr.expr, expected_type)
        elif isinstance(expr, Aggregate):
            return self._analyze_aggregate(expr, expected_type)
        elif isinstance(expr, DeltaAggregate):
            return self._analyze_delta_aggregate(expr)
        elif isinstance(expr, ContainerAggregate):
            return self._analyze_container_aggregate(expr)
        elif isinstance(expr, Allocator):
            return self._analyze_allocator(expr)
        elif isinstance(expr, ConditionalExpr):
            return self._analyze_conditional_expr(expr)
        elif isinstance(expr, QuantifiedExpr):
            return self._analyze_quantified_expr(expr)
        elif isinstance(expr, DeclareExpr):
            return self._analyze_declare_expr(expr)
        elif isinstance(expr, CaseExpr):
            return self._analyze_case_expr(expr)
        elif isinstance(expr, MembershipTest):
            return self._analyze_membership_test(expr)
        elif isinstance(expr, Slice):
            return self._analyze_slice(expr)
        elif isinstance(expr, Dereference):
            return self._analyze_dereference(expr)
        elif isinstance(expr, TargetName):
            return self._analyze_target_name(expr)
        elif isinstance(expr, RaiseExpr):
            return self._analyze_raise_expr(expr)

        return None

    def _analyze_allocator(self, expr: Allocator) -> Optional[AdaType]:
        """Analyze an allocator expression (new Type)."""
        # Resolve the type mark
        designated_type = self._resolve_type(expr.type_mark)
        if designated_type is None:
            return None

        # If there's an initial value, check it's compatible
        if expr.initial_value:
            init_type = self._analyze_expr(expr.initial_value)
            if init_type and not types_compatible(designated_type, init_type):
                self.error(
                    f"initial value type '{init_type.name}' not compatible with "
                    f"designated type '{designated_type.name}'",
                    expr.initial_value,
                )

        # Return an anonymous access type for the allocator
        return AccessType(
            name=f"access_{designated_type.name}",
            designated_type=designated_type,
        )

    def _analyze_conditional_expr(self, expr: ConditionalExpr) -> Optional[AdaType]:
        """Analyze an Ada 2012 conditional expression: (if Cond then E1 else E2)."""
        # Condition must be Boolean
        cond_type = self._analyze_expr(expr.condition)
        if cond_type and cond_type.name.lower() != "boolean":
            self.error(
                f"condition must be Boolean, got '{cond_type.name}'",
                expr.condition,
            )

        # Analyze then expression
        then_type = self._analyze_expr(expr.then_expr)

        # Analyze elsif parts (if any)
        result_type = then_type
        for elsif_cond, elsif_expr in expr.elsif_parts:
            elsif_cond_type = self._analyze_expr(elsif_cond)
            if elsif_cond_type and elsif_cond_type.name.lower() != "boolean":
                self.error(
                    f"elsif condition must be Boolean, got '{elsif_cond_type.name}'",
                    elsif_cond,
                )
            elsif_type = self._analyze_expr(elsif_expr)
            if result_type and elsif_type and not types_compatible(result_type, elsif_type):
                self.error(
                    f"elsif branch type '{elsif_type.name}' not compatible with "
                    f"then branch type '{result_type.name}'",
                    elsif_expr,
                )

        # Analyze else expression (if any)
        if expr.else_expr:
            else_type = self._analyze_expr(expr.else_expr)
            if result_type and else_type and not types_compatible(result_type, else_type):
                self.error(
                    f"else branch type '{else_type.name}' not compatible with "
                    f"then branch type '{result_type.name}'",
                    expr.else_expr,
                )

        return result_type

    def _analyze_quantified_expr(self, expr: QuantifiedExpr) -> Optional[AdaType]:
        """Analyze an Ada 2012 quantified expression: (for all/some X in Range => Pred)."""
        # Push a new scope for the loop variable
        self.symbols.enter_scope("quantified_expr")

        # Analyze the iterator and define the loop variable
        if expr.iterator:
            # Get the iterable type
            iter_type: Optional[AdaType] = None
            if expr.iterator.iterable:
                iter_type = self._analyze_expr(expr.iterator.iterable)

            # Determine the element type for the loop variable
            element_type = iter_type if iter_type else PREDEFINED_TYPES["Integer"]
            if isinstance(iter_type, ArrayType):
                element_type = iter_type.component_type

            # Define the loop variable
            self.symbols.define(
                Symbol(
                    name=expr.iterator.name,
                    kind=SymbolKind.VARIABLE,
                    ada_type=element_type,
                    is_constant=True,  # Loop variable is constant
                ),
            )

        # Analyze the predicate - must be Boolean
        pred_type = self._analyze_expr(expr.predicate)
        if pred_type and pred_type.name.lower() != "boolean":
            self.error(
                f"quantified expression predicate must be Boolean, got '{pred_type.name}'",
                expr.predicate,
            )

        # Pop the scope
        self.symbols.leave_scope()

        # Quantified expressions always return Boolean
        return PREDEFINED_TYPES["Boolean"]

    def _analyze_declare_expr(self, expr: DeclareExpr) -> Optional[AdaType]:
        """Analyze an Ada 2022 declare expression: (declare ... begin Expr)."""
        # Enter a new scope for the declarations
        self.symbols.enter_scope("declare_expr")

        # Analyze each declaration
        for decl in expr.declarations:
            self._analyze_declaration(decl)

        # Analyze the result expression
        result_type = self._analyze_expr(expr.result_expr)

        # Leave the scope
        self.symbols.leave_scope()

        return result_type

    def _analyze_delta_aggregate(self, expr: DeltaAggregate) -> Optional[AdaType]:
        """Analyze an Ada 2022 delta aggregate: (base with delta ...)."""
        # Analyze the base expression to get its type
        base_type = self._analyze_expr(expr.base_expression)
        if base_type is None:
            return None

        # Base must be a record or array type
        if not isinstance(base_type, (RecordType, ArrayType)):
            self.error(
                f"delta aggregate base must be record or array, got '{base_type.name}'",
                expr.base_expression,
            )
            return base_type

        # Analyze each component association
        for component in expr.components:
            # For record delta aggregates, verify the component exists
            if isinstance(base_type, RecordType):
                for choice in component.choices:
                    if isinstance(choice, Identifier):
                        found = False
                        for comp in base_type.components:
                            if comp.name.lower() == choice.name.lower():
                                found = True
                                break
                        if not found:
                            self.error(
                                f"component '{choice.name}' not in record type '{base_type.name}'",
                                choice,
                            )

            # Analyze the component value
            if component.value:
                self._analyze_expr(component.value)

        # Delta aggregate has the same type as the base
        return base_type

    def _analyze_aggregate(self, expr: Aggregate, expected_type: Optional[AdaType] = None) -> Optional[AdaType]:
        """Analyze an aggregate expression.

        Args:
            expr: The aggregate expression to analyze
            expected_type: Optional expected type from context (e.g., array type for array aggregate)
        """
        # Analyze all components, including iterated ones
        element_type = None
        for component in expr.components:
            if isinstance(component, IteratedComponentAssociation):
                element_type = self._analyze_iterated_component(component)
            elif isinstance(component, ComponentAssociation):
                # Analyze the value expression
                if component.value:
                    comp_type = self._analyze_expr(component.value)
                    if element_type is None:
                        element_type = comp_type
        # If we have an expected type, return it (aggregate takes type from context)
        if expected_type:
            return expected_type
        # Type is determined by context, but we analyze components
        return None

    def _analyze_container_aggregate(self, expr: ContainerAggregate) -> Optional[AdaType]:
        """Analyze a container aggregate [...]."""
        # Analyze all components
        element_type = None
        for component in expr.components:
            if isinstance(component, IteratedComponentAssociation):
                elem = self._analyze_iterated_component(component)
                if element_type is None:
                    element_type = elem
            elif isinstance(component, ComponentAssociation):
                if component.value:
                    comp_type = self._analyze_expr(component.value)
                    if element_type is None:
                        element_type = comp_type
        # Return an anonymous array type if we can determine element type
        if element_type:
            return ArrayType(
                name="anonymous_array",
                component_type=element_type,
                index_types=[PREDEFINED_TYPES["Integer"]],
            )
        return None

    def _analyze_iterated_component(
        self, comp: IteratedComponentAssociation
    ) -> Optional[AdaType]:
        """Analyze an iterated component association (Ada 2012)."""
        # Enter scope for loop variable
        self.symbols.enter_scope("iterated_component")

        # Analyze the iterator specification (range or iterable)
        iter_type = self._analyze_expr(comp.iterator_spec)

        # Define the loop parameter
        if iter_type:
            loop_var_type = iter_type
            # For discrete ranges, the variable type is the range type
            if isinstance(comp.iterator_spec, RangeExpr):
                loop_var_type = PREDEFINED_TYPES["Integer"]
        else:
            loop_var_type = PREDEFINED_TYPES["Integer"]

        self.symbols.define(
            Symbol(
                name=comp.loop_parameter,
                kind=SymbolKind.VARIABLE,
                ada_type=loop_var_type,
            )
        )

        # Analyze the value expression
        element_type = self._analyze_expr(comp.value)

        self.symbols.leave_scope()
        return element_type

    def _analyze_target_name(self, expr: TargetName) -> Optional[AdaType]:
        """Analyze an Ada 2022 target name (@) expression.

        The @ symbol refers to the target of the enclosing assignment statement.
        Example: X := @ + 1;  -- Equivalent to X := X + 1;
        """
        if self.current_assignment_target_type is None:
            self.error(
                "target name (@) can only be used in an assignment statement",
                expr,
            )
            return None
        return self.current_assignment_target_type

    def _analyze_raise_expr(self, expr: RaiseExpr) -> Optional[AdaType]:
        """Analyze an Ada 2012 raise expression.

        Raise expressions can appear where any type is expected since
        they never return normally.
        Example: X := (if Y > 0 then Y else raise Constraint_Error);
        """
        # Verify exception name is valid if provided
        if expr.exception_name:
            # For now, allow any identifier as exception name
            # In a full implementation, we'd verify it's a declared exception
            pass

        # Analyze the message expression if present
        if expr.message:
            msg_type = self._analyze_expr(expr.message)
            if msg_type and msg_type.name.lower() != "string":
                self.error(
                    f"raise expression message must be String, got '{msg_type.name}'",
                    expr.message,
                )

        # Raise expressions are "polymorphic" - they can have any type
        # since they never return. Return None to allow type inference
        # from context.
        return None

    def _analyze_case_expr(self, expr: CaseExpr) -> Optional[AdaType]:
        """Analyze an Ada 2012 case expression: (case Selector is when ...)."""
        # Analyze the selector expression
        selector_type = self._analyze_expr(expr.selector)
        if selector_type is None:
            return None

        # Selector must be a discrete type (integer, enumeration, or modular)
        if selector_type and not selector_type.is_discrete():
            self.error(
                f"case expression selector must be a discrete type, got '{selector_type.name}'",
                expr.selector,
            )

        # Analyze all alternatives and find common type
        result_type: Optional[AdaType] = None
        for alt in expr.alternatives:
            # Analyze choice expressions (simple analysis)
            for choice in alt.choices:
                if isinstance(choice, ExprChoice):
                    self._analyze_expr(choice.expr)
                elif isinstance(choice, RangeChoice):
                    if choice.range_expr:
                        self._analyze_expr(choice.range_expr)
                # OthersChoice needs no analysis

            # Analyze the result expression
            alt_type = self._analyze_expr(alt.result_expr)
            if alt_type:
                if result_type is None:
                    result_type = alt_type
                # Type compatibility check - just warn if different types
                elif result_type.name.lower() != alt_type.name.lower():
                    self.error(
                        f"case expression alternatives must have compatible types, "
                        f"got '{result_type.name}' and '{alt_type.name}'",
                        alt.result_expr,
                    )

        return result_type

    def _analyze_membership_test(self, expr: MembershipTest) -> Optional[AdaType]:
        """Analyze a membership test (X in A | B | C)."""
        # Analyze the tested expression
        expr_type = self._analyze_expr(expr.expr)

        # Analyze each choice
        for choice in expr.choices:
            if isinstance(choice, ExprChoice):
                self._analyze_expr(choice.expr)
            elif isinstance(choice, RangeChoice):
                if choice.range_expr:
                    self._analyze_expr(choice.range_expr)
            # OthersChoice doesn't need analysis

        # Membership tests always return Boolean
        return PREDEFINED_TYPES["Boolean"]

    def _analyze_slice(self, expr: Slice) -> Optional[AdaType]:
        """Analyze a slice expression (A(1 .. 10))."""
        # Get the prefix type
        prefix_type = self._analyze_expr(expr.prefix)
        if prefix_type is None:
            return None

        # Ada allows implicit dereference for access-to-array types
        # X(Low..High) where X is access-to-array implicitly dereferences X
        if isinstance(prefix_type, AccessType):
            designated = prefix_type.designated_type
            if isinstance(designated, ArrayType):
                prefix_type = designated

        # Prefix must be an array type
        if not isinstance(prefix_type, ArrayType):
            self.error(
                f"slice prefix must be an array type, got '{prefix_type.name}'",
                expr.prefix,
            )
            return None

        # Analyze the range expression
        self._analyze_expr(expr.range_expr)

        # Slice of an array returns same array type (unconstrained)
        # Use the same type name so slices are compatible with the base type
        return ArrayType(
            name=prefix_type.name,  # Same type name - slice of String is String
            kind=prefix_type.kind,
            size_bits=0,  # Size depends on range at runtime
            index_types=prefix_type.index_types,
            component_type=prefix_type.component_type,
            is_constrained=False,
        )

    def _analyze_dereference(self, expr: Dereference) -> Optional[AdaType]:
        """Analyze a dereference expression (P.all)."""
        # Get the prefix type
        prefix_type = self._analyze_expr(expr.prefix)
        if prefix_type is None:
            return None

        # Prefix must be an access type
        if not isinstance(prefix_type, AccessType):
            self.error(
                f"dereference prefix must be an access type, got '{prefix_type.name}'",
                expr.prefix,
            )
            return None

        # Return the designated type, resolving incomplete types
        designated = prefix_type.designated_type
        if designated and designated.kind == TypeKind.INCOMPLETE:
            # Try to find the completed type
            completed = self.symbols.lookup_type(designated.name)
            if completed:
                designated = completed
        return designated

    def _analyze_identifier(self, expr: Identifier, expected_type: Optional[AdaType] = None) -> Optional[AdaType]:
        """Analyze an identifier expression.

        Args:
            expr: The identifier to analyze
            expected_type: Optional expected type for overload resolution
        """
        # Check for generic formal type mapping (during instantiation)
        generic_formals = getattr(self, '_generic_formals', {})
        if expr.name.lower() in generic_formals:
            actual = generic_formals[expr.name.lower()]
            # The actual might be ActualParameter (wrapping value) or Identifier
            if hasattr(actual, 'value'):
                actual = actual.value
            if isinstance(actual, Identifier):
                actual_type = self.symbols.lookup_type(actual.name)
                if actual_type:
                    return actual_type

        symbol = self.symbols.lookup(expr.name)
        if symbol is None:
            self.error(f"'{expr.name}' not found", expr)
            return None

        # Check if this is an enum literal (VARIABLE with is_constant and ENUMERATION type)
        def is_enum_literal(sym: Symbol) -> bool:
            return (sym.is_constant and
                    sym.ada_type is not None and
                    sym.ada_type.kind == TypeKind.ENUMERATION)

        # If there's an expected type and the symbol is an enum literal,
        # try to find a matching overload
        if expected_type is not None and is_enum_literal(symbol):
            # Get all overloads for this name
            overloads = self.symbols.all_overloads(expr.name)
            for candidate in overloads:
                if is_enum_literal(candidate):
                    if candidate.ada_type and types_compatible(expected_type, candidate.ada_type):
                        return candidate.ada_type
            # No match found - fall through to return default

        return symbol.ada_type

    def _analyze_binary_expr(self, expr: BinaryExpr) -> Optional[AdaType]:
        """Analyze a binary expression."""
        left_type = self._analyze_expr(expr.left)
        right_type = self._analyze_expr(expr.right)

        # Relational operators return Boolean
        if expr.op in (
            BinaryOp.EQ,
            BinaryOp.NE,
            BinaryOp.LT,
            BinaryOp.LE,
            BinaryOp.GT,
            BinaryOp.GE,
        ):
            return PREDEFINED_TYPES["Boolean"]

        # Logical/bitwise operators
        if expr.op in (
            BinaryOp.AND,
            BinaryOp.OR,
            BinaryOp.XOR,
            BinaryOp.AND_THEN,
            BinaryOp.OR_ELSE,
        ):
            # For modular types, these are bitwise operators
            if left_type and left_type.kind == TypeKind.MODULAR:
                if right_type and right_type.kind == TypeKind.MODULAR:
                    result = common_type(left_type, right_type)
                    if result is None:
                        self.error(
                            f"incompatible modular types: "
                            f"'{left_type.name}' and '{right_type.name}'",
                            expr,
                        )
                    return result
                elif right_type and right_type.kind == TypeKind.UNIVERSAL_INTEGER:
                    # Universal_Integer is implicitly convertible to modular
                    return left_type
                else:
                    self.error(
                        f"expected modular type, got '{right_type.name if right_type else 'unknown'}'",
                        expr.right,
                    )
                    return left_type
            # Also handle reverse: Universal_Integer on left, modular on right
            if left_type and left_type.kind == TypeKind.UNIVERSAL_INTEGER:
                if right_type and right_type.kind == TypeKind.MODULAR:
                    return right_type
            # For arrays of Boolean, these are element-wise logical operators (Ada RM 4.5.1)
            if (left_type and left_type.kind == TypeKind.ARRAY and
                isinstance(left_type, ArrayType) and left_type.component_type):
                comp_type = left_type.component_type
                # Check if component is Boolean or derived from Boolean
                is_boolean_component = False
                if comp_type.name.lower() == 'boolean':
                    is_boolean_component = True
                else:
                    # Walk the base_type chain to check for Boolean
                    current = comp_type
                    while hasattr(current, 'base_type') and current.base_type:
                        current = current.base_type
                        if current.name.lower() == 'boolean':
                            is_boolean_component = True
                            break
                if is_boolean_component:
                    # If right type is None (e.g., aggregate without context), re-analyze with expected type
                    if right_type is None:
                        right_type = self._analyze_expr(expr.right, expected_type=left_type)
                    # Both operands must be compatible array types
                    if right_type and types_compatible(left_type, right_type):
                        return left_type  # Result is same array type
            # For Boolean, these are logical operators
            self._check_boolean(left_type, expr.left)
            self._check_boolean(right_type, expr.right)
            return PREDEFINED_TYPES["Boolean"]

        # Exponentiation is special: X ** N where N must be integer, result is type of X
        if expr.op == BinaryOp.EXP:
            if left_type and right_type:
                # Right operand must be integer type
                if right_type.kind not in (TypeKind.INTEGER, TypeKind.MODULAR,
                                           TypeKind.UNIVERSAL_INTEGER):
                    self.error(
                        f"exponent must be integer type, got '{right_type.name}'",
                        expr,
                    )
                # Result type is the left operand type
                return left_type
            return left_type

        # Other arithmetic operators
        if expr.op in (
            BinaryOp.ADD,
            BinaryOp.SUB,
            BinaryOp.MUL,
            BinaryOp.DIV,
            BinaryOp.MOD,
            BinaryOp.REM,
        ):
            if left_type and right_type:
                result = common_type(left_type, right_type)
                if result is None:
                    self.error(
                        f"incompatible types for arithmetic: "
                        f"'{left_type.name}' and '{right_type.name}'",
                        expr,
                    )
                return result

        # Concatenation
        if expr.op == BinaryOp.CONCAT:
            # For arrays, concatenation returns the array type
            if left_type and left_type.kind == TypeKind.ARRAY:
                return left_type
            if right_type and right_type.kind == TypeKind.ARRAY:
                return right_type
            # Default to String for string literals
            return PREDEFINED_TYPES["String"]

        return left_type

    def _analyze_unary_expr(self, expr: UnaryExpr) -> Optional[AdaType]:
        """Analyze a unary expression."""
        operand_type = self._analyze_expr(expr.operand)

        if expr.op == UnaryOp.NOT:
            # For modular types, NOT is bitwise complement
            if operand_type and operand_type.kind == TypeKind.MODULAR:
                return operand_type
            # For Universal_Integer, NOT is also bitwise complement
            if operand_type and operand_type.kind == TypeKind.UNIVERSAL_INTEGER:
                return operand_type
            # For arrays of Boolean, NOT is element-wise negation (Ada RM 4.5.6)
            if operand_type and operand_type.kind == TypeKind.ARRAY:
                if isinstance(operand_type, ArrayType) and operand_type.component_type:
                    comp_type = operand_type.component_type
                    # Check if component is Boolean or derived from Boolean
                    is_boolean_component = False
                    if comp_type.name.lower() == 'boolean':
                        is_boolean_component = True
                    else:
                        # Walk the base_type chain to check for Boolean
                        current = comp_type
                        while hasattr(current, 'base_type') and current.base_type:
                            current = current.base_type
                            if current.name.lower() == 'boolean':
                                is_boolean_component = True
                                break
                    if is_boolean_component:
                        return operand_type  # Returns array of same type
            # For Boolean, NOT is logical negation
            self._check_boolean(operand_type, expr.operand)
            return PREDEFINED_TYPES["Boolean"]

        if expr.op in (UnaryOp.PLUS, UnaryOp.MINUS, UnaryOp.ABS):
            if operand_type and not operand_type.is_numeric():
                # Check for user-defined operator
                op_name = {
                    UnaryOp.PLUS: '"++"',  # Ada uses "+" for unary plus
                    UnaryOp.MINUS: '"-"',
                    UnaryOp.ABS: '"abs"',
                }.get(expr.op, '"-"')
                # Ada convention: unary operators use the same name as binary ones
                op_name_lookup = {
                    UnaryOp.PLUS: '+',
                    UnaryOp.MINUS: '-',
                    UnaryOp.ABS: 'abs',
                }.get(expr.op, '-')
                overloads = self.symbols.all_overloads(op_name_lookup)
                found_match = False
                for candidate in overloads:
                    if candidate.kind == SymbolKind.FUNCTION and len(candidate.parameters) == 1:
                        param_type = candidate.parameters[0].ada_type
                        if param_type and types_compatible(param_type, operand_type):
                            found_match = True
                            return candidate.return_type
                if not found_match:
                    self.error(
                        f"numeric type required, got '{operand_type.name}'",
                        expr.operand,
                    )
            return operand_type

        return operand_type

    def _analyze_range_expr(self, expr: RangeExpr) -> Optional[AdaType]:
        """Analyze a range expression."""
        low_type = self._analyze_expr(expr.low)
        high_type = self._analyze_expr(expr.high)

        if low_type and high_type:
            result = common_type(low_type, high_type)
            if result is None:
                self.error(
                    f"incompatible types in range: "
                    f"'{low_type.name}' and '{high_type.name}'",
                    expr,
                )
            return result
        return low_type or high_type

    def _analyze_indexed_component(self, expr: IndexedComponent) -> Optional[AdaType]:
        """Analyze an indexed component (array access) or type conversion.

        In Ada, T(X) can be either:
        - Array indexing if T is an array
        - Type conversion if T is a type name

        The parser cannot distinguish these, so we resolve it here.
        """
        # Check if prefix is a type name (type conversion)
        if isinstance(expr.prefix, Identifier):
            symbol = self.symbols.lookup(expr.prefix.name)
            if symbol and symbol.kind in (SymbolKind.TYPE, SymbolKind.SUBTYPE):
                # This is a type conversion: Type(Expr)
                if len(expr.indices) != 1:
                    self.error("type conversion takes exactly one argument", expr)
                    return None
                # Analyze the argument
                arg_type = self._analyze_expr(expr.indices[0])
                target_type = symbol.ada_type
                # Check if conversion is valid
                if arg_type and target_type:
                    if not can_convert(arg_type, target_type):
                        self.error(
                            f"cannot convert from '{arg_type.name}' to '{target_type.name}'",
                            expr
                        )
                return target_type

            # Check if prefix is a function with a single aggregate argument
            # This handles cases like IDENT((TRUE, FALSE, TRUE)) where the parser
            # creates IndexedComponent instead of FunctionCall
            if (symbol and symbol.kind == SymbolKind.FUNCTION and
                len(expr.indices) == 1 and
                isinstance(expr.indices[0], Aggregate)):
                # This is a function call with an aggregate argument
                func_params = symbol.parameters if symbol.parameters else []
                if len(func_params) == 1:
                    args = [ActualParameter(span=None, name=None, value=expr.indices[0])]
                    self._check_call_arguments(symbol, args, expr)
                    return symbol.return_type

            # Check if prefix is an access-to-function variable (function pointer call)
            if symbol and symbol.kind in (SymbolKind.VARIABLE, SymbolKind.CONSTANT):
                if isinstance(symbol.ada_type, AccessSubprogramType):
                    if symbol.ada_type.is_function:
                        # This is a function call through access type: Func_Ptr(args)
                        self._check_access_subprogram_call_expr(
                            symbol.ada_type, expr.indices, expr
                        )
                        return symbol.ada_type.return_type
                    else:
                        self.error(
                            f"'{expr.prefix.name}' is an access-to-procedure, "
                            "cannot be used in an expression",
                            expr,
                        )
                        return None

        # Otherwise, it's array indexing
        prefix_type = self._analyze_expr(expr.prefix)

        if prefix_type is None:
            return None

        # Handle implicit dereference: access-to-array types can be indexed directly
        if isinstance(prefix_type, AccessType):
            if isinstance(prefix_type.designated_type, ArrayType):
                prefix_type = prefix_type.designated_type
            else:
                self.error(
                    f"'{prefix_type.name}' is not an access-to-array type",
                    expr.prefix,
                )
                return None

        if not isinstance(prefix_type, ArrayType):
            self.error(f"'{prefix_type.name}' is not an array", expr.prefix)
            return None

        # Check indices
        for idx in expr.indices:
            self._analyze_expr(idx)

        return prefix_type.component_type

    def _analyze_selected_name(self, expr: SelectedName) -> Optional[AdaType]:
        """Analyze a selected name (record.field, package.item, or pointer.all)."""
        prefix_type = self._analyze_expr(expr.prefix)

        if prefix_type is None:
            # Might be a package prefix - handle both simple and hierarchical names
            if isinstance(expr.prefix, Identifier):
                symbol = self.symbols.lookup_selected(
                    expr.prefix.name, expr.selector
                )
                if symbol:
                    return symbol.ada_type
            elif isinstance(expr.prefix, SelectedName):
                # Handle recursive SelectedName prefix (e.g., Ada.Text_IO.Put)
                # First try to look up the full prefix as a registered package
                full_prefix = self._get_hierarchical_name(expr.prefix)
                prefix_sym = self.symbols.lookup(full_prefix)
                if prefix_sym and prefix_sym.kind == SymbolKind.PACKAGE:
                    selector = expr.selector.lower() if isinstance(expr.selector, str) else expr.selector.lower()
                    if selector in prefix_sym.public_symbols:
                        return prefix_sym.public_symbols[selector].ada_type
                # Try resolving through the package hierarchy
                prefix_pkg = self._resolve_hierarchical_package(expr.prefix)
                if prefix_pkg and prefix_pkg.kind == SymbolKind.PACKAGE:
                    selector = expr.selector.lower() if isinstance(expr.selector, str) else expr.selector.lower()
                    if selector in prefix_pkg.public_symbols:
                        return prefix_pkg.public_symbols[selector].ada_type
            return None

        # Access type dereference (Ptr.all)
        if expr.selector.lower() == "all":
            if isinstance(prefix_type, AccessType):
                designated = prefix_type.designated_type
                # If designated type is incomplete, try to get the completed type
                if designated and designated.kind == TypeKind.INCOMPLETE:
                    completed = self.symbols.lookup_type(designated.name)
                    if completed:
                        designated = completed
                return designated
            self.error(
                f"'.all' can only be applied to access types, not '{prefix_type.name}'",
                expr,
            )
            return None

        # Record component access
        if isinstance(prefix_type, RecordType):
            comp = prefix_type.get_component(expr.selector)
            if comp is None:
                self.error(
                    f"record '{prefix_type.name}' has no component '{expr.selector}'",
                    expr,
                )
                return None
            return comp.component_type

        # Access to record - implicit dereference
        if isinstance(prefix_type, AccessType):
            designated = prefix_type.designated_type
            # If designated type is incomplete, try to get the completed type
            if designated and designated.kind == TypeKind.INCOMPLETE:
                completed = self.symbols.lookup_type(designated.name)
                if completed:
                    designated = completed
            if isinstance(designated, RecordType):
                comp = designated.get_component(expr.selector)
                if comp is None:
                    self.error(
                        f"record '{designated.name}' has no component '{expr.selector}'",
                        expr,
                    )
                    return None
                return comp.component_type

        self.error(f"'{prefix_type.name}' is not a record", expr.prefix)
        return None

    def _analyze_attribute_ref(self, expr: AttributeReference) -> Optional[AdaType]:
        """Analyze an attribute reference."""
        # Analyze prefix and get its type
        prefix_type = self._analyze_expr(expr.prefix)

        # Handle implicit dereference for access-to-array types
        # e.g., if V is access-to-array, V'Last implicitly dereferences
        if isinstance(prefix_type, AccessType):
            if isinstance(prefix_type.designated_type, ArrayType):
                prefix_type = prefix_type.designated_type

        # Handle attributes based on their name
        attr_lower = expr.attribute.lower()

        # First/Last return the same type as the prefix (for scalar types)
        if attr_lower in ("first", "last", "min", "max"):
            # For scalar types, First/Last return that type
            if prefix_type and prefix_type.kind in (
                TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.ENUMERATION,
                TypeKind.FLOAT, TypeKind.FIXED, TypeKind.UNIVERSAL_INTEGER,
                TypeKind.UNIVERSAL_REAL
            ):
                return prefix_type
            # For arrays, First/Last return the index type
            if isinstance(prefix_type, ArrayType) and prefix_type.index_types:
                return prefix_type.index_types[0]
            return PREDEFINED_TYPES["Integer"]

        # Integer-valued attributes
        # 'Length and 'Size return Universal_Integer (implicitly convertible to any integer)
        if attr_lower in ("length", "size"):
            return PREDEFINED_TYPES["Universal_Integer"]
        # 'Pos returns Universal_Integer
        if attr_lower == "pos":
            return PREDEFINED_TYPES["Universal_Integer"]

        # Val returns the enumeration type
        if attr_lower == "val":
            return prefix_type

        # Image returns String
        if attr_lower == "image":
            return PREDEFINED_TYPES["String"]

        # Value returns the type (inverse of Image)
        if attr_lower == "value":
            return prefix_type

        # Address returns System.Address type
        if attr_lower == "address":
            # Try to get the actual System.Address type from the symbol table
            system_pkg = self.symbols.lookup("System")
            if system_pkg and system_pkg.public_symbols.get("address"):
                return system_pkg.public_symbols["address"].ada_type
            # Fallback if System package not available
            return AdaType(name="Address", kind=TypeKind.ACCESS)

        # Access returns an access type to the prefix
        if attr_lower == "access":
            if prefix_type is None:
                return None
            return AccessType(
                name=f"access_{prefix_type.name}",
                kind=TypeKind.ACCESS,
                size_bits=16,  # Z80 has 16-bit pointers
                designated_type=prefix_type,
            )

        # Unchecked_Access is like Access but without accessibility checks
        if attr_lower == "unchecked_access":
            if prefix_type is None:
                return None
            return AccessType(
                name=f"access_{prefix_type.name}",
                kind=TypeKind.ACCESS,
                size_bits=16,
                designated_type=prefix_type,
            )

        # Range attribute on arrays returns the index range
        if attr_lower == "range":
            if isinstance(prefix_type, ArrayType) and prefix_type.index_types:
                return prefix_type.index_types[0]
            return PREDEFINED_TYPES["Integer"]

        # Succ and Pred return the same discrete type
        if attr_lower in ("succ", "pred"):
            return prefix_type

        # Modulus for modular types
        if attr_lower == "modulus":
            return PREDEFINED_TYPES["Integer"]

        # Boolean attributes
        if attr_lower in ("valid", "constrained", "terminated", "callable"):
            return PREDEFINED_TYPES["Boolean"]

        # Reduce attribute (Ada 2022)
        # Syntax: Prefix'Reduce(Combiner, Initial_Value)
        if attr_lower == "reduce":
            # Analyze the combiner and initial value arguments
            if len(expr.args) >= 2:
                self._analyze_expr(expr.args[0])  # Combiner (function or operator)
                init_type = self._analyze_expr(expr.args[1])  # Initial value
                # The result type is the type of the initial value
                if init_type:
                    return init_type
            # If prefix is an array, the result type is component type
            if isinstance(prefix_type, ArrayType):
                return prefix_type.component_type
            return PREDEFINED_TYPES["Integer"]

        # Parallel_Reduce attribute (Ada 2022)
        if attr_lower == "parallel_reduce":
            if len(expr.args) >= 2:
                self._analyze_expr(expr.args[0])
                init_type = self._analyze_expr(expr.args[1])
                if init_type:
                    return init_type
            if isinstance(prefix_type, ArrayType):
                return prefix_type.component_type
            return PREDEFINED_TYPES["Integer"]

        # 'Old attribute (Ada 2012) - used in postconditions
        # Returns the value of the expression at subprogram entry
        if attr_lower == "old":
            # 'Old has the same type as the prefix
            return prefix_type

        # 'Result attribute (Ada 2012) - used in postconditions
        # Refers to the return value of the enclosing function
        if attr_lower == "result":
            # Should be used only in postconditions of functions
            if self.current_subprogram is not None:
                if self.current_subprogram.kind in (
                    SymbolKind.FUNCTION, SymbolKind.GENERIC_FUNCTION
                ):
                    return self.current_subprogram.return_type
            self.error("'Result can only be used in function postconditions", expr)
            return None

        # 'Update attribute (Ada 2012 AI12-0001) - for record/array update
        if attr_lower == "update":
            # Returns same type as prefix
            return prefix_type

        # Floating-point rounding/truncation attributes
        # These return the same floating-point type as the prefix
        if attr_lower in ("floor", "ceiling", "truncation", "rounding",
                          "machine_rounding", "unbiased_rounding", "machine"):
            # Analyze the argument if present
            if expr.args:
                self._analyze_expr(expr.args[0])
            # Return the floating-point type (from prefix)
            return prefix_type

        # Default: return Integer for unknown attributes
        return PREDEFINED_TYPES["Integer"]

    def _analyze_function_call(self, expr: FunctionCall) -> Optional[AdaType]:
        """Analyze a function call."""
        if isinstance(expr.name, Identifier):
            symbol = self.symbols.lookup(expr.name.name)
            if symbol is None:
                self.error(f"function '{expr.name.name}' not found", expr)
                return None
            if symbol.kind != SymbolKind.FUNCTION:
                self.error(f"'{expr.name.name}' is not a function", expr)
                return None

            self._check_call_arguments(symbol, expr.args, expr)
            return symbol.return_type

        return None

    def _analyze_type_conversion(self, expr: TypeConversion) -> Optional[AdaType]:
        """Analyze a type conversion."""
        target_type = self._resolve_type(expr.type_mark)
        operand_type = self._analyze_expr(expr.operand)

        if target_type and operand_type:
            if not can_convert(operand_type, target_type):
                self.error(
                    f"cannot convert '{operand_type.name}' to '{target_type.name}'",
                    expr,
                )

        return target_type

    def _analyze_qualified_expr(self, expr: QualifiedExpr) -> Optional[AdaType]:
        """Analyze a qualified expression."""
        target_type = self._resolve_type(expr.type_mark)
        self._analyze_expr(expr.expr)
        return target_type

    # =========================================================================
    # Static Expression Evaluation
    # =========================================================================

    def _try_eval_static(self, expr: Expr) -> Optional[int]:
        """Try to evaluate a static expression. Returns None if not static."""
        try:
            return self._eval_static_impl(expr, report_errors=False)
        except Exception:
            return None

    def _eval_static_expr(self, expr: Expr) -> int:
        """Evaluate a static expression to an integer value."""
        result = self._eval_static_impl(expr, report_errors=True)
        return result if result is not None else 0

    def _eval_static_impl(self, expr: Expr, report_errors: bool = True) -> Optional[int]:
        """Implementation of static expression evaluation."""
        if isinstance(expr, IntegerLiteral):
            return expr.value

        if isinstance(expr, RealLiteral):
            # Return truncated integer value for real literals in integer contexts
            return int(expr.value)

        if isinstance(expr, CharacterLiteral):
            # Character literals are static - return their position value
            return ord(expr.value)

        if isinstance(expr, StringLiteral):
            # String literals are static for 'Length - return length
            return len(expr.value)

        if isinstance(expr, Identifier):
            # Look up constant value
            sym = self.symbols.lookup(expr.name)
            if sym and sym.is_constant and sym.value is not None:
                return sym.value
            # Check if it's an enumeration literal (constant with enumeration type)
            if sym and sym.is_constant and sym.ada_type:
                if hasattr(sym.ada_type, 'kind') and sym.ada_type.kind == TypeKind.ENUMERATION:
                    # Get position from positions dict or literals list
                    if hasattr(sym.ada_type, 'positions') and expr.name in sym.ada_type.positions:
                        return sym.ada_type.positions[expr.name]
                    if hasattr(sym.ada_type, 'literals') and sym.ada_type.literals:
                        try:
                            pos = sym.ada_type.literals.index(expr.name)
                            return pos
                        except (ValueError, AttributeError):
                            pass
            # Check if it's a variable with an enumeration type (enum value lookup)
            if sym and sym.kind == SymbolKind.VARIABLE and sym.ada_type:
                if hasattr(sym.ada_type, 'literals') and sym.ada_type.literals:
                    # This is an enum value - find its position
                    try:
                        pos = sym.ada_type.literals.index(expr.name)
                        return pos
                    except (ValueError, AttributeError):
                        pass
            if report_errors:
                self.error("expression is not static", expr)
            return None

        if isinstance(expr, SelectedName):
            # Handle Package.Name for constants like SYSTEM.MIN_INT
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup_selected(expr.prefix.name, expr.selector)
                if sym and sym.is_constant and sym.value is not None:
                    return sym.value
            if report_errors:
                self.error("expression is not static", expr)
            return None

        if isinstance(expr, UnaryExpr):
            operand = self._eval_static_impl(expr.operand, report_errors)
            if operand is None:
                return None
            if expr.op == UnaryOp.MINUS:
                return -operand
            if expr.op == UnaryOp.PLUS:
                return operand
            if expr.op == UnaryOp.ABS:
                return abs(operand)
            return operand

        if isinstance(expr, BinaryExpr):
            left = self._eval_static_impl(expr.left, report_errors)
            right = self._eval_static_impl(expr.right, report_errors)
            if left is None or right is None:
                return None
            if expr.op == BinaryOp.ADD:
                return left + right
            if expr.op == BinaryOp.SUB:
                return left - right
            if expr.op == BinaryOp.MUL:
                return left * right
            if expr.op == BinaryOp.DIV:
                return left // right if right != 0 else 0
            if expr.op == BinaryOp.MOD:
                return left % right if right != 0 else 0
            if expr.op == BinaryOp.REM:
                # Ada rem has sign of dividend
                if right == 0:
                    return 0
                result = abs(left) % abs(right)
                return result if left >= 0 else -result
            if expr.op == BinaryOp.EXP:
                return left ** right

        if isinstance(expr, TypeConversion):
            # Type conversion of static expression is static
            return self._eval_static_impl(expr.expr, report_errors)

        if isinstance(expr, QualifiedExpr):
            # Qualified expression - evaluate the expression part
            return self._eval_static_impl(expr.expr, report_errors)

        if isinstance(expr, Parenthesized):
            # Parenthesized expression - evaluate the inner expression
            return self._eval_static_impl(expr.expr, report_errors)

        if isinstance(expr, AttributeReference):
            attr = expr.attribute.lower()
            type_obj = None

            # Get the type object from the prefix
            if isinstance(expr.prefix, Identifier):
                type_obj = self.symbols.lookup_type(expr.prefix.name)
                if not type_obj:
                    # Might be an object, check its type
                    sym = self.symbols.lookup(expr.prefix.name)
                    if sym and sym.ada_type:
                        type_obj = sym.ada_type
            elif isinstance(expr.prefix, SelectedName):
                # Handle Package.Type'Attr
                if isinstance(expr.prefix.prefix, Identifier):
                    sym = self.symbols.lookup_selected(expr.prefix.prefix.name, expr.prefix.selector)
                    if sym and sym.ada_type:
                        type_obj = sym.ada_type

            if type_obj:
                # 'First and 'Last for scalar types
                if attr == "first":
                    if hasattr(type_obj, "low"):
                        return type_obj.low
                    if hasattr(type_obj, "range_first") and type_obj.range_first is not None:
                        return int(type_obj.range_first)
                    # Enumeration types: 'First is 0
                    if hasattr(type_obj, "literals") and type_obj.literals:
                        return 0
                    # Array types: 'First is first dimension's low bound
                    if hasattr(type_obj, "index_types") and type_obj.index_types:
                        idx_type = type_obj.index_types[0]
                        if hasattr(idx_type, "low"):
                            return idx_type.low

                if attr == "last":
                    if hasattr(type_obj, "high"):
                        return type_obj.high
                    if hasattr(type_obj, "range_last") and type_obj.range_last is not None:
                        return int(type_obj.range_last)
                    # Enumeration types: 'Last is len(literals) - 1
                    if hasattr(type_obj, "literals") and type_obj.literals:
                        return len(type_obj.literals) - 1
                    # Array types: 'Last is first dimension's high bound
                    if hasattr(type_obj, "index_types") and type_obj.index_types:
                        idx_type = type_obj.index_types[0]
                        if hasattr(idx_type, "high"):
                            return idx_type.high

                # 'Size
                if attr == "size" and hasattr(type_obj, "size_bits"):
                    return type_obj.size_bits

                # 'Length for arrays
                if attr == "length":
                    if hasattr(type_obj, "length"):
                        return type_obj.length
                    # Calculate from bounds
                    if hasattr(type_obj, "low") and hasattr(type_obj, "high"):
                        return type_obj.high - type_obj.low + 1
                    # For array types, get first dimension
                    if hasattr(type_obj, "index_types") and type_obj.index_types:
                        idx_type = type_obj.index_types[0]
                        if hasattr(idx_type, "low") and hasattr(idx_type, "high"):
                            return idx_type.high - idx_type.low + 1

                # 'Modulus for modular types
                if attr == "modulus" and hasattr(type_obj, "modulus"):
                    return type_obj.modulus

                # 'Component_Size for arrays
                if attr == "component_size" and hasattr(type_obj, "element_type"):
                    elem_type = type_obj.element_type
                    if hasattr(elem_type, "size_bits"):
                        return elem_type.size_bits

            # Handle 'Pos and 'Val for enumeration types
            if attr == "pos" and expr.args:
                # 'Pos(X) returns position of X
                # If X is a character literal, return its ord value
                arg = expr.args[0]
                if isinstance(arg, CharacterLiteral):
                    return ord(arg.value)
                arg_val = self._eval_static_impl(arg, report_errors)
                return arg_val
            if attr == "val" and expr.args:
                arg_val = self._eval_static_impl(expr.args[0], report_errors)
                return arg_val

        # Default/fallback
        if report_errors:
            self.error("expression is not static", expr)
        return None


def analyze(program: Program, search_paths: Optional[list[str]] = None) -> SemanticResult:
    """Analyze a program and return the result."""
    analyzer = SemanticAnalyzer(search_paths=search_paths)
    return analyzer.analyze(program)
