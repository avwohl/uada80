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
    Subunit,
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
    IfExpr,
    QuantifiedExpr,
    DeclareExpr,
    CaseExpr,
    MembershipTest,
    ExprChoice,
    RangeChoice,
    Slice,
    Dereference,
    TargetName,
    BoxExpr,
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
    AccessTypeIndication,
    AccessSubprogramTypeIndication,
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
    AddressClause,
    RangeConstraint,
)
from uada80.symbol_table import SymbolTable, Symbol, SymbolKind
from uada80.type_system import (
    AdaType,
    TypeKind,
    IntegerType,
    ModularType,
    FloatType,
    FixedType,
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
    is_derived_from,
    get_root_type,
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

        # Standard package - the implicit root package containing predefined entities
        # This allows explicit qualification like STANDARD."*"
        standard_pkg = Symbol(name="Standard", kind=SymbolKind.PACKAGE)
        standard_pkg.public_symbols = {}

        # Add ASCII package as a child of Standard
        standard_pkg.public_symbols["ascii"] = ascii_pkg

        # Add predefined operators as functions
        # The operator symbols are stored as operator names
        for op in ("*", "/", "+", "-", "mod", "rem", "**", "abs", "not",
                   "=", "/=", "<", "<=", ">", ">=", "and", "or", "xor"):
            op_sym = Symbol(name=op, kind=SymbolKind.FUNCTION)
            op_sym.is_intrinsic = True  # Mark as built-in
            standard_pkg.public_symbols[op.lower()] = op_sym

        self.symbols.define(standard_pkg)

    def analyze(self, program: Program) -> SemanticResult:
        """Analyze a complete program."""
        # Store all units for later lookup of child package specs
        self._all_units = program.units

        for unit in program.units:
            # Skip units already analyzed on-demand (e.g., child package specs)
            analyzed = getattr(self, '_analyzed_units', set())
            if id(unit) in analyzed:
                continue
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
        # Track which package is being analyzed (for child package deferral)
        # Must be set BEFORE processing with clauses, since with clauses
        # may reference child packages that depend on parent's public symbols
        analyzing = getattr(self, '_analyzing_units', None)
        if analyzing is None:
            self._analyzing_units = analyzing = set()
        pkg_name_lower = None
        if isinstance(unit.unit, PackageDecl):
            pkg_name_lower = unit.unit.name.lower() if isinstance(unit.unit.name, str) else str(unit.unit.name).lower()
            analyzing.add(pkg_name_lower)

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
            if pkg_name_lower:
                analyzing.discard(pkg_name_lower)
        elif isinstance(unit.unit, PackageBody):
            self._analyze_package_body(unit.unit)
        elif isinstance(unit.unit, GenericInstantiation):
            self._analyze_generic_instantiation(unit.unit)
        elif isinstance(unit.unit, GenericSubprogramUnit):
            self._analyze_generic_subprogram(unit.unit)
        elif isinstance(unit.unit, Subunit):
            self._analyze_subunit(unit.unit)

    def _analyze_subunit(self, subunit: Subunit) -> None:
        """Analyze a separate subunit (SEPARATE (parent) body)."""
        # Extract parent unit name and resolve parent symbol
        if isinstance(subunit.parent_unit, Identifier):
            parent_name = subunit.parent_unit.name
            parent_sym = self.symbols.lookup(parent_name)
        elif isinstance(subunit.parent_unit, SelectedName):
            # Dotted name like CA2004A0M.CA2004A1 — use hierarchical resolution
            parent_sym = self._resolve_hierarchical_package(subunit.parent_unit)
            # For scope entry, use the final selector name
            parent_name = subunit.parent_unit.selector if isinstance(subunit.parent_unit.selector, str) else str(subunit.parent_unit.selector)
        else:
            parent_name = str(subunit.parent_unit)
            parent_sym = self.symbols.lookup(parent_name)

        # Enter the parent scope so the body can see parent declarations
        entered_scopes = []
        if parent_sym and parent_sym.kind == SymbolKind.PACKAGE:
            self.symbols.enter_scope(parent_name)
            entered_scopes.append(parent_name)
            # Import parent's symbols into this scope
            for sym_name, sym_val in parent_sym.public_symbols.items():
                try:
                    self.symbols.define(sym_val)
                except Exception:
                    pass
        elif parent_sym and parent_sym.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION,
                                                 SymbolKind.GENERIC_PROCEDURE, SymbolKind.GENERIC_FUNCTION):
            # Parent is a subprogram - find nested declarations from its AST
            self.symbols.enter_scope(parent_name)
            entered_scopes.append(parent_name)
            # Search the parent's AST for nested package specs and other declarations
            self._import_subprogram_locals(parent_name)

        # Analyze the body
        body = subunit.body
        if isinstance(body, SubprogramBody):
            self._analyze_subprogram_body(body)
        elif isinstance(body, PackageBody):
            self._analyze_package_body(body)
        elif isinstance(body, TaskBody):
            self._analyze_task_body(body)
        elif isinstance(body, ProtectedBody):
            self._analyze_protected_body(body)

        # Leave the parent scope(s)
        for _ in entered_scopes:
            self.symbols.leave_scope()

    def _import_subprogram_locals(self, parent_name: str) -> None:
        """Import local declarations from a subprogram parent for separate subunit analysis.

        When a subunit is separated from a subprogram parent, we need to find
        the parent's declarations from the AST and analyze them so the subunit
        can see them (Ada LRM 10.1.3: subunits have visibility of declarations
        prior to the body stub in the parent).
        """
        all_units = getattr(self, '_all_units', None)
        if not all_units:
            return
        # Extract simple name for matching (subprogram bodies use simple names)
        parent_lower = parent_name.split('.')[-1].lower()
        # Find the parent subprogram body in the AST
        for cu in all_units:
            if isinstance(cu.unit, SubprogramBody):
                body_name = cu.unit.spec.name.lower() if isinstance(cu.unit.spec.name, str) else str(cu.unit.spec.name).lower()
                if body_name == parent_lower:
                    # Analyze context clauses of the parent unit
                    for clause in cu.context_clauses:
                        if isinstance(clause, WithClause):
                            self._analyze_with_clause(clause)
                        elif isinstance(clause, UseClause):
                            self._analyze_use_clause(clause)
                    # Analyze parameters
                    for param_spec in cu.unit.spec.parameters:
                        self._analyze_parameter_spec(param_spec, None, add_to_symbol=False)
                    # Analyze all declarations from the parent body
                    for decl in cu.unit.declarations:
                        self._analyze_declaration(decl)
                    return

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
                    elif not self._find_package_in_ast(pkg_name):
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

                # Load all intermediate packages (e.g., for A.B.C, also load A.B)
                # Ada RM 10.1.2: with A.B.C makes A, A.B, and A.B.C all visible
                parts_list = full_name.split(".")
                for i in range(2, len(parts_list)):
                    intermediate = ".".join(parts_list[:i])
                    if self.symbols.lookup(intermediate) is None:
                        loaded_inter = self._load_external_package(intermediate)
                        if loaded_inter:
                            self.symbols.define(loaded_inter)
                            # Update parent's child reference
                            inter_parts = intermediate.rsplit(".", 1)
                            parent_sym = self.symbols.lookup(inter_parts[0])
                            if parent_sym:
                                parent_sym.public_symbols[inter_parts[1].lower()] = loaded_inter

                # Also register the full hierarchical name for direct lookup
                # This allows "Ada.Text_IO" to be found when used as a prefix
                if full_name != root_pkg:
                    existing_full = self.symbols.lookup(full_name)
                    if existing_full is None:
                        # Try to load the full package from file system first
                        loaded_full = self._load_external_package(full_name)
                        if loaded_full:
                            self.symbols.define(loaded_full)
                            # Update parent's child reference to point to fully-loaded symbol
                            parts = full_name.rsplit(".", 1)
                            if len(parts) == 2:
                                parent_sym = self.symbols.lookup(parts[0])
                                if parent_sym:
                                    parent_sym.public_symbols[parts[1].lower()] = loaded_full
                        elif not self._find_package_in_ast(full_name):
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
                    # Ensure child is registered in parent's public_symbols
                    # (may have been defined earlier without parent linkage)
                    if existing_full is None:
                        existing_full = self.symbols.lookup(full_name)
                    if existing_full is not None:
                        parts = full_name.rsplit(".", 1)
                        if len(parts) == 2:
                            parent_sym = self.symbols.lookup(parts[0])
                            if parent_sym and parts[1].lower() not in parent_sym.public_symbols:
                                parent_sym.public_symbols[parts[1].lower()] = existing_full

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
        Also handles block labels as prefixes (e.g., DD.P1 where DD is a block label).
        """
        if isinstance(name, Identifier):
            return self.symbols.lookup(name.name)
        elif hasattr(name, 'prefix') and hasattr(name, 'selector'):
            # First try flat name lookup — loaded packages (e.g., "Ada.Text_IO")
            # may be registered under their full dotted name and should take
            # precedence over builtin placeholders in parent public_symbols
            flat_name = self._get_hierarchical_name(name)
            flat_sym = self.symbols.lookup(flat_name)
            if flat_sym is not None:
                return flat_sym
            # Fall back to step-by-step resolution
            prefix_sym = self._resolve_hierarchical_package(name.prefix)
            if prefix_sym is None:
                return None
            selector = name.selector.lower() if isinstance(name.selector, str) else name.selector.lower()
            # For block labels, look up in the named scope directly
            # This handles the case where we're still inside the block
            if prefix_sym.kind == SymbolKind.LABEL:
                result = self.symbols.lookup_in_named_scope(prefix_sym.name, selector)
                if result is not None:
                    return result
            # Look up the selector in the prefix's public symbols
            # Works for both packages and block labels (both have public_symbols)
            if prefix_sym.public_symbols and selector in prefix_sym.public_symbols:
                return prefix_sym.public_symbols[selector]
        return None

    def _find_package_in_ast(self, pkg_name: str) -> Optional[Symbol]:
        """Search the combined AST for a package or subprogram declaration by name.

        When compiling multiple files together, child package specs and child
        subprograms may live in other files in the same compilation.  Scan the
        units list and analyze the matching declaration on demand.
        """
        all_units = getattr(self, '_all_units', None)
        if not all_units:
            return None
        analyzed = getattr(self, '_analyzed_units', None)
        if analyzed is None:
            self._analyzed_units = analyzed = set()
        # Track packages currently being analyzed to prevent premature child analysis
        analyzing = getattr(self, '_analyzing_units', None)
        if analyzing is None:
            self._analyzing_units = analyzing = set()
        pkg_name_lower = pkg_name.lower()
        # If this is a child package (has dot), check if parent is currently being analyzed.
        # If so, defer - the parent's public symbols aren't complete yet.
        if '.' in pkg_name_lower:
            parent_name = pkg_name_lower.rsplit('.', 1)[0]
            if parent_name in analyzing:
                return None
        # Also match by child name for hierarchical names like FA11D00.CA11D011
        child_name = pkg_name_lower.split(".")[-1] if "." in pkg_name_lower else None
        for cu in all_units:
            if isinstance(cu.unit, PackageDecl):
                decl_name = cu.unit.name.lower() if isinstance(cu.unit.name, str) else str(cu.unit.name).lower()
                if decl_name == pkg_name_lower and id(cu) not in analyzed:
                    # Mark as analyzed so the main loop skips it
                    analyzed.add(id(cu))
                    # Analyze context clauses and declaration
                    for clause in cu.context_clauses:
                        if isinstance(clause, WithClause):
                            self._analyze_with_clause(clause)
                        elif isinstance(clause, UseClause):
                            self._analyze_use_clause(clause)
                    self._analyze_package_decl(cu.unit)
                    return self.symbols.lookup(pkg_name)
            elif isinstance(cu.unit, (SubprogramBody, SubprogramDecl)):
                # Handle child subprograms (procedure/function as compilation unit)
                spec = cu.unit.spec if isinstance(cu.unit, SubprogramBody) else cu.unit
                decl_name = spec.name.lower() if isinstance(spec.name, str) else str(spec.name).lower()
                # Match full name or child name
                if (decl_name == pkg_name_lower or (child_name and decl_name == child_name)) and id(cu) not in analyzed:
                    analyzed.add(id(cu))
                    # Analyze context clauses
                    for clause in cu.context_clauses:
                        if isinstance(clause, WithClause):
                            self._analyze_with_clause(clause)
                        elif isinstance(clause, UseClause):
                            self._analyze_use_clause(clause)
                    # Create a subprogram symbol
                    is_function = spec.is_function
                    kind = SymbolKind.FUNCTION if is_function else SymbolKind.PROCEDURE
                    subprog_symbol = Symbol(
                        name=pkg_name,
                        kind=kind,
                    )
                    subprog_symbol.is_withed = True
                    # Extract parameters
                    for param_spec in spec.parameters:
                        param_type = self._resolve_type(param_spec.type_mark)
                        for param_name in param_spec.names:
                            param_symbol = Symbol(
                                name=param_name,
                                kind=SymbolKind.PARAMETER,
                                ada_type=param_type,
                                mode=param_spec.mode,
                            )
                            param_symbol.default_value = param_spec.default_value
                            subprog_symbol.parameters.append(param_symbol)
                    # Extract return type for functions
                    if is_function and spec.return_type:
                        subprog_symbol.return_type = self._resolve_type(spec.return_type)
                    self.symbols.define(subprog_symbol)
                    # Also register in parent package's public_symbols
                    if "." in pkg_name:
                        parts = pkg_name.rsplit(".", 1)
                        parent_sym = self.symbols.lookup(parts[0])
                        if parent_sym:
                            parent_sym.public_symbols[parts[1].lower()] = subprog_symbol
                    return subprog_symbol
        return None

    def _find_package_file(self, pkg_name: str) -> Optional[str]:
        """Find the file containing a package specification.

        Converts Ada package names to file paths following GNAT naming conventions:
        - Ada.Text_IO -> ada-text_io.ads
        - My_Package -> my_package.ads

        Also searches for ACATS-style naming (e.g., lencheck.ada for LENGTH_CHECK).

        Returns the full path if found, None otherwise.
        """
        # Convert package name to file name (GNAT convention)
        file_base = pkg_name.lower().replace(".", "-")

        # File name patterns to try:
        # 1. Standard GNAT: name.ads, name.ada
        # 2. ACATS style: 8-char truncation with variations
        candidates = [
            f"{file_base}.ads",
            f"{file_base}.ada",
        ]

        # ACATS naming pattern: remove underscores, truncate to 8 chars
        acats_base = pkg_name.lower().replace("_", "")[:8]
        if acats_base != file_base:
            candidates.append(f"{acats_base}.ads")
            candidates.append(f"{acats_base}.ada")

        # Search in all configured paths
        for search_path in self.search_paths:
            for file_name in candidates:
                file_path = os.path.join(search_path, file_name)
                if os.path.isfile(file_path):
                    return file_path

            # Also try to find any file in the search path containing the
            # lowercase name (ACATS support files have unusual naming)
            # E.g., LENGTH_CHECK -> lencheck.ada, ENUM_CHECK -> enumchek.ada
            try:
                for entry in os.listdir(search_path):
                    if entry.endswith(('.ada', '.ads', '.a')):
                        entry_base = os.path.splitext(entry)[0].lower()
                        # Try to match ACATS abbreviated names:
                        # Build a regex-like check: entry should match significant
                        # parts of the package name
                        pkg_parts = pkg_name.lower().split("_")
                        # Check if all significant parts appear in filename
                        all_match = True
                        for part in pkg_parts:
                            if len(part) >= 3 and part[:3] not in entry_base:
                                all_match = False
                                break
                        if all_match and len(pkg_parts) >= 2:
                            file_path = os.path.join(search_path, entry)
                            return file_path
            except OSError:
                pass

        # Also search current directory
        for file_name in candidates:
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

            # Find the package declaration or generic subprogram in the parsed AST
            pkg_decl = None
            gen_subprog = None
            gen_body = None
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
                elif isinstance(unit.unit, GenericSubprogramUnit):
                    # Match generic procedure/function by name (case-insensitive)
                    if unit.unit.name.lower() == pkg_name.lower():
                        gen_subprog = unit.unit
                        # Don't break - continue looking for a separate body
                elif isinstance(unit.unit, SubprogramBody):
                    # Check if this is the body for a previously found generic spec
                    if gen_subprog is not None and unit.unit.spec.name.lower() == pkg_name.lower():
                        gen_body = unit.unit

            # Handle generic subprogram (e.g., LENGTH_CHECK, ENUM_CHECK)
            if gen_subprog is not None:
                is_function = gen_subprog.is_function
                kind = SymbolKind.GENERIC_FUNCTION if is_function else SymbolKind.GENERIC_PROCEDURE
                gen_symbol = Symbol(
                    name=pkg_name,
                    kind=kind,
                )
                gen_symbol.is_withed = True
                gen_symbol.generic_decl = gen_subprog
                # Store the separate body if found (for instantiation)
                if gen_body is not None:
                    gen_symbol.generic_body = gen_body

                # Extract parameters from the subprogram declaration
                # This is needed for instantiation to copy parameters
                subprog = gen_subprog.subprogram
                if isinstance(subprog, SubprogramBody):
                    spec = subprog.spec
                else:
                    spec = subprog
                for param_spec in spec.parameters:
                    param_type = self._resolve_type(param_spec.type_mark)
                    for param_name in param_spec.names:
                        param_symbol = Symbol(
                            name=param_name,
                            kind=SymbolKind.PARAMETER,
                            ada_type=param_type,
                            mode=param_spec.mode,
                        )
                        param_symbol.default_value = param_spec.default_value
                        gen_symbol.parameters.append(param_symbol)

                # Extract return type for functions
                if is_function and spec.return_type:
                    gen_symbol.return_type = self._resolve_type(spec.return_type)

                # Cache and return the generic subprogram symbol
                self._loaded_packages[pkg_key] = gen_symbol
                return gen_symbol

            # Handle non-generic child subprograms (procedure/function as compilation unit)
            # E.g., "with Parent.Child_Proc;" where Child_Proc is a procedure
            if not pkg_decl:
                child_subprog = None
                for unit in program.units:
                    if isinstance(unit.unit, SubprogramBody):
                        if unit.unit.spec.name.lower() == pkg_name.lower():
                            child_subprog = unit.unit
                            break
                        # Check child name: FA11D00.CA11D011 -> match CA11D011
                        if "." in pkg_name:
                            child_name = pkg_name.split(".")[-1].lower()
                            if unit.unit.spec.name.lower() == child_name:
                                child_subprog = unit.unit
                                break
                    elif isinstance(unit.unit, SubprogramDecl):
                        if unit.unit.name.lower() == pkg_name.lower():
                            child_subprog = unit.unit
                            break
                        if "." in pkg_name:
                            child_name = pkg_name.split(".")[-1].lower()
                            if unit.unit.name.lower() == child_name:
                                child_subprog = unit.unit
                                break

                if child_subprog is not None:
                    spec = child_subprog.spec if isinstance(child_subprog, SubprogramBody) else child_subprog
                    is_function = spec.is_function
                    kind = SymbolKind.FUNCTION if is_function else SymbolKind.PROCEDURE
                    subprog_symbol = Symbol(
                        name=pkg_name,
                        kind=kind,
                    )
                    subprog_symbol.is_withed = True
                    # Extract parameters
                    for param_spec in spec.parameters:
                        param_type = self._resolve_type(param_spec.type_mark)
                        for param_name in param_spec.names:
                            param_symbol = Symbol(
                                name=param_name,
                                kind=SymbolKind.PARAMETER,
                                ada_type=param_type,
                                mode=param_spec.mode,
                            )
                            param_symbol.default_value = param_spec.default_value
                            subprog_symbol.parameters.append(param_symbol)
                    # Extract return type for functions
                    if is_function and spec.return_type:
                        subprog_symbol.return_type = self._resolve_type(spec.return_type)

                    self._loaded_packages[pkg_key] = subprog_symbol
                    return subprog_symbol

                return None

            # Handle package renaming (e.g., package Text_IO renames Ada.Text_IO)
            if pkg_decl.renames:
                renamed_name = self._get_hierarchical_name(pkg_decl.renames)
                renamed_pkg = self._load_external_package(renamed_name)
                if renamed_pkg:
                    # Preserve the generic nature if renaming a generic package
                    pkg_symbol = Symbol(
                        name=pkg_name,
                        kind=renamed_pkg.kind,
                    )
                    pkg_symbol.is_withed = True
                    pkg_symbol.public_symbols = renamed_pkg.public_symbols
                    pkg_symbol.private_symbols = renamed_pkg.private_symbols
                    # Copy generic-related attributes for generic package renamings
                    if renamed_pkg.kind == SymbolKind.GENERIC_PACKAGE:
                        pkg_symbol.generic_decl = renamed_pkg.generic_decl
                        pkg_symbol.generic_formal_symbols = renamed_pkg.generic_formal_symbols
                    self._loaded_packages[pkg_key] = pkg_symbol
                    return pkg_symbol
                return None

            # Create a symbol for this package and extract its public declarations
            # Check if the package is a generic package
            is_generic = getattr(pkg_decl, 'is_generic', False) or bool(pkg_decl.generic_formals)
            pkg_symbol = Symbol(
                name=pkg_name,
                kind=SymbolKind.GENERIC_PACKAGE if is_generic else SymbolKind.PACKAGE,
            )
            pkg_symbol.is_withed = True
            # Store the AST node for generic instantiation
            if is_generic:
                pkg_symbol.generic_decl = pkg_decl

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

            # Analyze private declarations (completes private types with full definitions)
            # E.g., "type Controlled is abstract tagged private;" in public part
            # is completed by "type Controlled is abstract tagged record...end record;"
            # in the private part. Without this, private tagged types remain as
            # plain AdaType(kind=PRIVATE) instead of becoming RecordType.
            for decl in pkg_decl.private_declarations:
                try:
                    self._analyze_declaration(decl)
                    self._add_to_package(pkg_symbol, decl, is_private=True)
                except Exception:
                    pass  # Skip declarations that fail analysis

            # After private completion, fix up access types whose designated_type
            # was None or incomplete when first built
            self._fixup_access_types(pkg_symbol, pkg_decl)

            # For standard packages, merge in fallback values for constants
            # that weren't properly evaluated during analysis
            if pkg_name.upper() in ("SYSTEM", "ADA", "INTERFACES"):
                self._merge_standard_values(pkg_symbol, pkg_name.upper())

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

    def _merge_standard_values(self, pkg_symbol: Symbol, name: str) -> None:
        """Merge fallback values for standard package constants that weren't evaluated.

        When loading standard packages from files, some constant expressions may not
        be evaluated properly. This merges in known values from _setup_standard_package.
        """
        fallback = Symbol(name=name, kind=SymbolKind.PACKAGE)
        self._setup_standard_package(fallback, name)
        for sym_name, fallback_sym in fallback.public_symbols.items():
            if sym_name in pkg_symbol.public_symbols:
                existing = pkg_symbol.public_symbols[sym_name]
                # If the loaded symbol is a constant without a value, use the fallback
                if existing.is_constant and existing.value is None and fallback_sym.value is not None:
                    existing.value = fallback_sym.value

    def _analyze_use_clause(self, clause: UseClause) -> None:
        """Analyze a use clause."""
        if clause.is_type or clause.is_all:
            # use type T; or use all type T;
            # Makes operators (and for is_all, all primitives) directly visible
            for name in clause.names:
                type_sym = None
                pkg_symbol = None

                # Handle T'Class attribute references
                actual_name = name
                is_class_wide = False
                if isinstance(name, AttributeReference) and name.attribute.lower() == 'class':
                    actual_name = name.prefix
                    is_class_wide = True

                if isinstance(actual_name, SelectedName):
                    # Qualified name like P.T
                    pkg_name = self._get_hierarchical_name(actual_name.prefix)
                    pkg_symbol = self._resolve_hierarchical_package(actual_name.prefix)
                    if pkg_symbol and pkg_symbol.kind == SymbolKind.PACKAGE:
                        type_name = actual_name.selector.lower() if isinstance(actual_name.selector, str) else actual_name.selector
                        if type_name in pkg_symbol.public_symbols:
                            type_sym = pkg_symbol.public_symbols[type_name]
                elif isinstance(actual_name, Identifier):
                    type_sym = self.symbols.lookup(actual_name.name)

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
                pkg_name = self._get_hierarchical_name(name)
                pkg_symbol = self._resolve_hierarchical_package(name)
                if pkg_symbol is None:
                    self.error(f"package '{pkg_name}' not found", name)
                elif pkg_symbol.kind != SymbolKind.PACKAGE:
                    self.error(f"'{pkg_name}' is not a package", name)
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

        # For child subprograms (dotted names), import parent package symbols
        if "." in spec.name:
            self._import_parent_package_symbols(spec.name)

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
            # First try to resolve as hierarchical name (handles DD.P1, PQ2.PK2, etc.)
            renamed_pkg = self._resolve_hierarchical_package(pkg.renames)
            if renamed_pkg is None:
                # Try simple lookup for non-hierarchical names
                renamed_pkg = self.symbols.lookup(renamed_name)
            if renamed_pkg is None:
                # Try to load from file
                renamed_pkg = self._load_external_package(renamed_name)
            if renamed_pkg and renamed_pkg.kind in (SymbolKind.PACKAGE, SymbolKind.GENERIC_PACKAGE):
                # Create renaming symbol that points to the renamed package
                # Preserve the generic nature if renaming a generic package
                pkg_symbol = Symbol(
                    name=pkg.name,
                    kind=renamed_pkg.kind,
                    alias_for=renamed_name,  # Track the renaming chain
                )
                pkg_symbol.public_symbols = renamed_pkg.public_symbols
                pkg_symbol.private_symbols = renamed_pkg.private_symbols
                # Copy generic-related attributes for generic package renamings
                if renamed_pkg.kind == SymbolKind.GENERIC_PACKAGE:
                    pkg_symbol.generic_decl = renamed_pkg.generic_decl
                    pkg_symbol.generic_formal_symbols = renamed_pkg.generic_formal_symbols
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

        # For child packages (dotted names like Parent.Child), also register
        # in the parent's public_symbols for hierarchical lookup
        if "." in pkg.name:
            parts = pkg.name.rsplit(".", 1)
            parent_sym = self.symbols.lookup(parts[0])
            if parent_sym and parent_sym.kind in (SymbolKind.PACKAGE, SymbolKind.GENERIC_PACKAGE):
                parent_sym.public_symbols[parts[1].lower()] = pkg_symbol

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

        # After private completion, fix up access types whose designated_type
        # was None or incomplete/private when first built from public declarations
        self._fixup_access_types(pkg_symbol, pkg)

        self.symbols.leave_scope()
        self.current_package = old_package

    def _fixup_access_designated(self, old_type: 'AdaType', new_type: 'AdaType') -> None:
        """Update access types in current scope that reference old_type as designated_type."""
        scope = self.symbols.current_scope
        while scope:
            for sym in scope.symbols.values():
                if isinstance(sym, list):
                    syms = sym
                else:
                    syms = [sym]
                for s in syms:
                    if s.kind == SymbolKind.TYPE and isinstance(s.ada_type, AccessType):
                        if s.ada_type.designated_type is old_type:
                            s.ada_type.designated_type = new_type
            scope = scope.parent

    def _fixup_access_types(self, pkg_symbol: Symbol, pkg: PackageDecl) -> None:
        """Fix up access types whose designated_type was None or incomplete.

        After analyzing private declarations, types that were private stubs
        during public declaration analysis are now complete. Re-resolve the
        designated types of any access types that were built with incomplete info.
        """
        # Build map: access type name -> TypeDecl for re-resolution
        access_decls: dict[str, TypeDecl] = {}
        for decl in pkg.declarations:
            if isinstance(decl, TypeDecl) and isinstance(decl.type_def, AccessTypeDef):
                access_decls[decl.name.lower()] = decl

        for sym_name, sym in list(pkg_symbol.public_symbols.items()):
            if sym.kind == SymbolKind.TYPE and isinstance(sym.ada_type, AccessType):
                acc = sym.ada_type
                needs_fixup = (
                    acc.designated_type is None or
                    (hasattr(acc.designated_type, 'kind') and
                     acc.designated_type.kind in (TypeKind.PRIVATE, TypeKind.INCOMPLETE))
                )
                if needs_fixup and sym_name in access_decls:
                    td = access_decls[sym_name]
                    resolved = self._resolve_type(td.type_def.designated_type)
                    if resolved and resolved.kind not in (TypeKind.PRIVATE, TypeKind.INCOMPLETE):
                        acc.designated_type = resolved

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

            # Check if this is an array type formal (has definition with ArrayTypeDef)
            if hasattr(formal, 'definition') and formal.definition is not None:
                if isinstance(formal.definition, ArrayTypeDef):
                    # Create an ArrayType for the formal
                    # Resolve index types and component type
                    index_types = []
                    for idx_subtype in formal.definition.index_subtypes:
                        idx_type = self._resolve_type(idx_subtype)
                        if idx_type:
                            index_types.append(idx_type)
                        else:
                            # Fallback for unresolvable index types
                            index_types.append(PREDEFINED_TYPES["Integer"])
                    component_type = self._resolve_type(formal.definition.component_type)
                    type_sym.ada_type = ArrayType(
                        name=formal.name,
                        kind=TypeKind.ARRAY,
                        size_bits=0,
                        index_types=index_types,
                        component_type=component_type,
                        is_constrained=formal.definition.is_constrained,
                    )
                    type_sym.ada_type.is_generic_formal = True
                    self.symbols.define(type_sym)
                    sym = type_sym
                elif isinstance(formal.definition, AccessTypeDef):
                    # Create an AccessType for the formal
                    type_sym.ada_type = self._build_access_type(formal.name, formal.definition)
                    type_sym.ada_type.is_generic_formal = True
                    self.symbols.define(type_sym)
                    sym = type_sym
                else:
                    # Other type definitions - fall through to constraint handling
                    pass

            if not hasattr(type_sym, 'ada_type') or type_sym.ada_type is None:
                # Determine the appropriate type kind based on the constraint
                constraint = getattr(formal, 'constraint', None) or 'private'
                if constraint == 'derived' and hasattr(formal, 'parent_type') and formal.parent_type:
                    # Derived type formal (type T is new Parent [with private])
                    parent = self._resolve_type(formal.parent_type)
                    if parent and isinstance(parent, RecordType):
                        # Create a record type with parent's components
                        type_sym.ada_type = RecordType(
                            name=formal.name,
                            components=list(parent.components),
                            discriminants=list(parent.discriminants) if parent.discriminants else [],
                            parent_type=parent,
                            is_tagged=parent.is_tagged,
                        )
                    elif parent:
                        type_sym.ada_type = AdaType(kind=parent.kind, name=formal.name)
                        type_sym.ada_type.base_type = parent
                    else:
                        type_sym.ada_type = AdaType(kind=TypeKind.PRIVATE, name=formal.name)
                elif constraint == 'range':
                    # Signed integer type (type T is range <>)
                    type_kind = TypeKind.INTEGER
                    type_sym.ada_type = AdaType(kind=type_kind, name=formal.name)
                elif constraint == 'mod':
                    # Modular integer type (type T is mod <>)
                    type_kind = TypeKind.MODULAR
                    type_sym.ada_type = AdaType(kind=type_kind, name=formal.name)
                elif constraint == 'digits':
                    # Floating point type (type T is digits <>)
                    type_kind = TypeKind.FLOAT
                    type_sym.ada_type = AdaType(kind=type_kind, name=formal.name)
                elif constraint in ('delta', 'delta_digits'):
                    # Fixed point type (type T is delta <>)
                    type_kind = TypeKind.FIXED
                    type_sym.ada_type = AdaType(kind=type_kind, name=formal.name)
                elif constraint == 'discrete':
                    # Discrete type (type T is (<>))
                    type_kind = TypeKind.ENUMERATION
                    type_sym.ada_type = AdaType(kind=type_kind, name=formal.name)
                else:
                    # Private, tagged private, etc.
                    type_kind = TypeKind.PRIVATE
                    type_sym.ada_type = AdaType(kind=type_kind, name=formal.name)
                    # Propagate is_tagged from generic formal declaration
                    if getattr(formal, 'is_tagged', False):
                        type_sym.ada_type.is_tagged = True

                type_sym.ada_type.is_generic_formal = True
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
                # Store each name in generic_formal_symbols for body visibility
                if owner_symbol is not None:
                    owner_symbol.generic_formal_symbols[obj_sym.name.lower()] = obj_sym

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
                if is_function and hasattr(formal, 'return_type') and formal.return_type:
                    subp_sym.return_type = self._resolve_type(formal.return_type)
                # Process parameters - this is critical for overload resolution
                params = getattr(formal, 'params', [])
                for param_spec in params:
                    param_type = self._resolve_type(param_spec.type_mark)
                    # Handle access parameters
                    if param_spec.mode == "access" and param_type:
                        param_type = AccessType(
                            name=f"access_{param_type.name}",
                            designated_type=param_type,
                            is_access_all=True,
                        )
                    for name in param_spec.names:
                        param_symbol = Symbol(
                            name=name,
                            kind=SymbolKind.PARAMETER,
                            ada_type=param_type,
                            mode=param_spec.mode,
                            default_value=param_spec.default_value,
                        )
                        subp_sym.parameters.append(param_symbol)
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

    def _count_generic_defaults(self, formals: list) -> int:
        """Count generic formals with default values.

        Handles: default_value (object formals), is_box (subprogram '<>'),
        default_subprogram (specific subprogram default), and multi-name
        object formals where one default covers all names.
        """
        from uada80.ast_nodes import GenericObjectDecl
        count = 0
        for f in formals:
            has_default = (
                (hasattr(f, 'default_value') and f.default_value is not None) or
                (hasattr(f, 'is_box') and f.is_box) or
                (hasattr(f, 'default_subprogram') and f.default_subprogram is not None)
            )
            if has_default:
                # Multi-name object formals: "P, Q : T := val" — all names have the default
                if isinstance(f, GenericObjectDecl):
                    names = getattr(f, 'names', None)
                    count += len(names) if names else 1
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
                generic_name = self._get_hierarchical_name(inst.generic_name)
                generic_sym = self._resolve_hierarchical_package(inst.generic_name)
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

        # For builtin generics (e.g. Ada.Strings.Bounded.Generic_Bounded_Length),
        # copy public_symbols directly — no AST-based re-analysis needed since
        # the symbols already carry runtime_name linkage.
        if getattr(generic_sym, 'is_builtin_generic', False):
            inst_symbol = Symbol(
                name=inst.name,
                kind=SymbolKind.PACKAGE,
            )
            inst_symbol.generic_instance_of = generic_sym
            inst_symbol.generic_actuals = inst.actual_parameters
            for sym_name, sym in generic_sym.public_symbols.items():
                inst_symbol.public_symbols[sym_name] = sym
            self.symbols.define(inst_symbol)
            return

        # Get the generic declaration
        generic_decl = getattr(generic_sym, 'generic_decl', None)
        if generic_decl is None:
            self.error(f"generic '{generic_name}' has no declaration", inst.generic_name)
            return

        # Check number of actual parameters (accounting for defaults and multi-name formals)
        num_formals = self._count_generic_parameters(generic_decl.generic_formals)
        num_actuals = len(inst.actual_parameters)
        num_with_defaults = self._count_generic_defaults(generic_decl.generic_formals)
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
        # Handle both positional and named actual parameters
        formal_to_actual: dict[str, any] = {}
        positional_actuals = []
        named_actuals = {}
        for ap in inst.actual_parameters:
            if isinstance(ap, ActualParameter) and ap.name:
                named_actuals[ap.name.lower()] = ap
            else:
                positional_actuals.append(ap)
        pos_idx = 0
        for formal in generic_decl.generic_formals:
            formal_name = getattr(formal, 'name', None) or getattr(formal, 'type_name', None)
            if formal_name and formal_name.lower() in named_actuals:
                formal_to_actual[formal_name.lower()] = named_actuals[formal_name.lower()]
            elif pos_idx < len(positional_actuals):
                if formal_name:
                    formal_to_actual[formal_name.lower()] = positional_actuals[pos_idx]
                pos_idx += 1

        # Enter the package scope to define its contents
        self.symbols.enter_scope(inst.name, is_package=True)

        # Save generic context for type resolution
        old_generic_formals = getattr(self, '_generic_formals', {})
        self._generic_formals = formal_to_actual

        # Add generic formal type and object symbols to the instance scope
        # This ensures that references to formal names resolve correctly
        from uada80.ast_nodes import GenericObjectDecl
        for formal in generic_decl.generic_formals:
            formal_name = getattr(formal, 'name', None) or getattr(formal, 'type_name', None)
            if not formal_name:
                continue
            if formal_name.lower() in formal_to_actual:
                actual = formal_to_actual[formal_name.lower()]
                # Unwrap ActualParameter to get the raw expression
                actual_expr = getattr(actual, 'value', actual)
            elif isinstance(formal, GenericObjectDecl) and getattr(formal, 'default_value', None):
                # Use the default value when no actual is provided
                actual_expr = formal.default_value
            else:
                continue
            if isinstance(formal, GenericTypeDecl):
                # Type formal: create a type symbol with the actual type
                type_name = formal.name
                actual_type = self._resolve_type(actual_expr)
                if actual_type:
                    type_sym = Symbol(
                        name=type_name,
                        kind=SymbolKind.TYPE,
                        ada_type=actual_type,
                    )
                    self.symbols.define(type_sym)
            elif isinstance(formal, GenericObjectDecl):
                # Object formal: create a variable symbol with the actual's type
                # Handle multi-name formals (e.g., "X, Y : Integer")
                names = getattr(formal, 'names', None) or [formal.name]
                for obj_name in names:
                    actual_type = self._analyze_expr(actual_expr)
                    # Evaluate static value of the actual for constants
                    static_value = None
                    if formal.mode == "in":
                        static_value = self._try_eval_static(actual_expr)
                    obj_sym = Symbol(
                        name=obj_name,
                        kind=SymbolKind.VARIABLE,
                        ada_type=actual_type,
                        is_constant=(formal.mode == "in"),
                        value=static_value,
                    )
                    self.symbols.define(obj_sym)

        # Process the generic package's declarations
        for decl in generic_decl.declarations:
            self._analyze_declaration(decl)

        # Restore generic context
        self._generic_formals = old_generic_formals

        # Export public symbols to the package
        for name, sym in self.symbols.current_scope.symbols.items():
            inst_symbol.public_symbols[name] = sym

        # Fix up access types with incomplete designated types
        # (forward-declared types are now complete after all declarations processed)
        access_decls: dict[str, TypeDecl] = {}
        for decl in generic_decl.declarations:
            if isinstance(decl, TypeDecl) and isinstance(decl.type_def, AccessTypeDef):
                access_decls[decl.name.lower()] = decl
        for sym_name, sym in list(inst_symbol.public_symbols.items()):
            if sym.kind == SymbolKind.TYPE and isinstance(sym.ada_type, AccessType):
                acc = sym.ada_type
                needs_fixup = (
                    acc.designated_type is None or
                    (hasattr(acc.designated_type, 'kind') and
                     acc.designated_type.kind in (TypeKind.PRIVATE, TypeKind.INCOMPLETE))
                )
                if needs_fixup:
                    if sym_name in access_decls:
                        resolved = self._resolve_type(access_decls[sym_name].type_def.designated_type)
                        if resolved and resolved.kind not in (TypeKind.PRIVATE, TypeKind.INCOMPLETE):
                            acc.designated_type = resolved
                    else:
                        # Try resolving by name of designated type
                        dt_name = getattr(acc.designated_type, 'name', None)
                        if dt_name:
                            resolved = self.symbols.lookup_type(dt_name)
                            if resolved and resolved.kind not in (TypeKind.PRIVATE, TypeKind.INCOMPLETE):
                                acc.designated_type = resolved

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
                param_symbol.default_value = param_spec.default_value
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
                generic_name = self._get_hierarchical_name(inst.generic_name)
                generic_sym = self._resolve_hierarchical_package(inst.generic_name)
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
            num_with_defaults = self._count_generic_defaults(generic_decl.formals)
            min_required = num_formals - num_with_defaults

            if num_actuals < min_required or num_actuals > num_formals:
                self.error(
                    f"wrong number of generic parameters for '{generic_name}': "
                    f"expected {min_required if min_required == num_formals else f'{min_required} to {num_formals}'}, got {num_actuals}",
                    inst
                )

        # Build type mapping from formal type parameters to actual types
        type_map: dict[str, AdaType] = {}
        if not is_builtin and generic_decl:
            from uada80.ast_nodes import GenericTypeDecl
            formal_idx = 0
            for formal in generic_decl.formals:
                if isinstance(formal, GenericTypeDecl) and formal_idx < len(inst.actual_parameters):
                    actual = inst.actual_parameters[formal_idx]
                    # Get the actual type name
                    actual_name = None
                    if hasattr(actual, 'value') and hasattr(actual.value, 'name'):
                        actual_name = actual.value.name
                    elif hasattr(actual, 'name'):
                        actual_name = actual.name
                    elif isinstance(actual, Identifier):
                        actual_name = actual.name
                    if actual_name:
                        actual_type = self._resolve_type(actual if isinstance(actual, (Identifier, SelectedName)) else
                                                         (actual.value if hasattr(actual, 'value') else actual))
                        if actual_type:
                            type_map[formal.name.lower()] = actual_type
                formal_idx += 1

        # Create the instantiated subprogram
        is_function = generic_sym.kind == SymbolKind.GENERIC_FUNCTION
        inst_symbol = Symbol(
            name=inst.name,
            kind=SymbolKind.FUNCTION if is_function else SymbolKind.PROCEDURE,
        )
        # Store mapping from formals to actuals for code generation
        inst_symbol.generic_instance_of = generic_sym
        inst_symbol.generic_actuals = inst.actual_parameters

        # Substitute return type: replace formal type names with actual types
        ret_type = generic_sym.return_type
        if ret_type and hasattr(ret_type, 'name') and ret_type.name.lower() in type_map:
            ret_type = type_map[ret_type.name.lower()]
        inst_symbol.return_type = ret_type

        # Copy parameters with type substitution
        inst_symbol.parameters = []
        for param in (generic_sym.parameters or []):
            new_param = Symbol(
                name=param.name,
                kind=param.kind,
                ada_type=param.ada_type,
                mode=param.mode,
                default_value=param.default_value,
            )
            if new_param.ada_type and hasattr(new_param.ada_type, 'name'):
                if new_param.ada_type.name.lower() in type_map:
                    new_param.ada_type = type_map[new_param.ada_type.name.lower()]
            inst_symbol.parameters.append(new_param)

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
            # Try to auto-load the package spec from the file system
            # In Ada, a package body implicitly depends on its own spec
            loaded_pkg = self._load_external_package(body.name)
            if loaded_pkg:
                self.symbols.define(loaded_pkg)
                pkg_symbol = loaded_pkg
            else:
                # Try to find the spec in the combined AST (multi-file compilation)
                ast_pkg = self._find_package_in_ast(body.name)
                if ast_pkg:
                    pkg_symbol = ast_pkg
                else:
                    self.error(f"package specification for '{body.name}' not found")
                    return
        if pkg_symbol.kind not in (SymbolKind.PACKAGE, SymbolKind.GENERIC_PACKAGE):
            self.error(f"'{body.name}' is not a package")
            return

        # Enter package scope
        self.symbols.enter_scope(body.name)

        # For child packages, make parent's declarations visible
        # Ada RM 10.1.1: A child package body has visibility to its parent
        if "." in body.name:
            self._import_parent_package_symbols(body.name)

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

            # For enumeration types, also add the literals to the package
            if isinstance(decl, TypeDecl) and isinstance(decl.type_def, EnumerationTypeDef):
                for literal in decl.type_def.literals:
                    lit_name = literal.lower()
                    lit_sym = self.symbols.lookup_local(lit_name)
                    if lit_sym:
                        if is_private:
                            pkg.private_symbols[lit_name] = lit_sym
                        else:
                            pkg.public_symbols[lit_name] = lit_sym
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
            elif isinstance(decl.type_mark, AccessTypeIndication):
                # Handle anonymous access types (e.g., X : access Integer)
                designated_type = self._resolve_type(decl.type_mark.subtype)
                obj_type = AccessType(
                    name="_anonymous_access",
                    designated_type=designated_type,
                    is_access_constant=decl.type_mark.is_constant,
                    is_not_null=decl.type_mark.not_null,
                    is_access_all=decl.type_mark.is_all,
                )
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

            # Character literals don't conflict with identifiers
            if existing is not None and self._is_character_literal_symbol(existing):
                existing = None

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
            existing = self.symbols.lookup_local(name)
            if existing is not None:
                # Allow shadowing enumeration literals (Ada allows this)
                if (existing.kind == SymbolKind.VARIABLE and
                    existing.is_constant and
                    existing.ada_type and
                    existing.ada_type.kind == TypeKind.ENUMERATION):
                    # This number declaration shadows an enumeration literal - allow it
                    pass
                else:
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
            # Handle different type mark forms
            if isinstance(decl.type_mark, AccessTypeIndication):
                designated_type = self._resolve_type(decl.type_mark.subtype)
                declared_type = AccessType(
                    name="_anonymous_access",
                    designated_type=designated_type,
                    is_access_constant=decl.type_mark.is_constant,
                    is_not_null=decl.type_mark.not_null,
                    is_access_all=decl.type_mark.is_all,
                )
            elif isinstance(decl.type_mark, AccessSubprogramTypeIndication):
                # For access-to-subprogram types, just use the renamed type
                declared_type = renamed_type
            else:
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

        # Character literals (single character names) don't conflict with identifiers
        # in Ada, because 'T' and T are syntactically distinct
        if existing is not None and self._is_character_literal_symbol(existing):
            existing = None

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
            is_completing_private = (
                existing.kind == SymbolKind.TYPE and
                existing.ada_type and (
                    existing.ada_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE) or
                    getattr(existing.ada_type, 'is_private_extension', False)
                )
            )
            if is_completing_private:
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

                old_type = existing.ada_type
                existing.ada_type = ada_type
                existing.definition = decl
                decl.ada_type = ada_type  # Store on AST for lowering
                # Fix up access types that referenced the old incomplete/private type
                if old_type is not ada_type:
                    self._fixup_access_designated(old_type, ada_type)
                # Fall through to handle enum literals if applicable
            else:
                self.error(f"type '{decl.name}' is already defined", decl)
                return
        else:
            # Build the type
            is_tagged = getattr(decl, 'is_tagged', False)
            ada_type = self._build_type(decl.name, decl.type_def, is_tagged)

            # If type build failed for a private extension, create a stub so that
            # the completion in the private part can replace it later
            if ada_type is None and isinstance(decl.type_def, DerivedTypeDef):
                if getattr(decl.type_def, 'is_private_extension', False):
                    ada_type = RecordType(name=decl.name, is_tagged=True)
                    ada_type.is_private_extension = True

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

        # For derived enumeration types, add overloaded literals for the derived type
        # In Ada, when TYPE T IS NEW Parent_Enum, the literals work with both types
        # through overload resolution. We need to register the literals with the
        # derived type so that they can be used in contexts expecting the derived type.
        # Skip character literals (single chars) — they would shadow identifiers
        # from outer scopes. Character literals are resolved through type context.
        if isinstance(ada_type, EnumerationType) and isinstance(decl.type_def, DerivedTypeDef):
            for literal in ada_type.literals:
                # Skip character literals — only register identifier-form literals
                if len(literal) == 1:
                    continue
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

        # Store resolved type on AST node for use during lowering
        decl.ada_type = base_type

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

        # Get alias name for renaming declarations
        alias_for = None
        if decl.renames:
            alias_for = self._get_hierarchical_name(decl.renames)

        symbol = Symbol(
            name=decl.name,
            kind=kind,
            return_type=return_type,
            is_abstract=decl.is_abstract,
            alias_for=alias_for,
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
                    default_value=param_spec.default_value,
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
            elif isinstance(exc_name, SelectedName):
                # Handle Package.Exception_Name
                symbol = self._resolve_hierarchical_package(exc_name)
                if symbol is None:
                    name = self._get_hierarchical_name(exc_name)
                    self.error(f"unknown exception '{name}'", exc_name)
                elif symbol.kind != SymbolKind.EXCEPTION:
                    name = self._get_hierarchical_name(exc_name)
                    self.error(f"'{name}' is not an exception", exc_name)

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
        if isinstance(decl, AddressClause):
            pass  # Address clauses are accepted but don't affect semantic analysis
        elif isinstance(decl, AttributeDefinitionClause):
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

        # Apply the attribute based on what it is and what kind of entity
        attr = decl.attribute.lower()

        if attr == "size":
            value = self._eval_static_expr(decl.value)
            if sym.kind == SymbolKind.TYPE and sym.ada_type:
                sym.ada_type.size_bits = value
            elif sym.kind == SymbolKind.VARIABLE:
                sym.explicit_size = value
        elif attr == "alignment":
            value = self._eval_static_expr(decl.value)
            if sym.kind == SymbolKind.TYPE and sym.ada_type:
                sym.ada_type.alignment = value
        elif attr == "address":
            # Address clauses don't require static expressions (Ada RM 13.3)
            value = self._try_eval_static(decl.value)
            # for Object'Address use N; - place object at specific address
            if sym.kind in (SymbolKind.VARIABLE, SymbolKind.CONSTANT,
                            SymbolKind.FUNCTION, SymbolKind.PROCEDURE,
                            SymbolKind.TASK, SymbolKind.PROTECTED):
                if isinstance(value, int):
                    sym.explicit_address = value
            else:
                self.error(f"Address clause only applies to objects and subprograms", decl)
        elif attr == "component_size":
            value = self._eval_static_expr(decl.value)
            # for Array_Type'Component_Size use N;
            if sym.kind == SymbolKind.TYPE and sym.ada_type:
                from uada80.type_system import ArrayType
                if isinstance(sym.ada_type, ArrayType):
                    # Store component size (would need to add field)
                    sym.ada_type.component_type.size_bits = value
        elif attr == "storage_size":
            # for Access_Type'Storage_Size use N;
            # for Task_Type'Storage_Size use N;
            # Expression need not be static per Ada RM 13.11(3)
            pass
        elif attr == "machine_radix":
            # for Decimal_Type'Machine_Radix use N;
            # Accept non-static expressions
            pass
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

            # Search discriminants
            if not found and hasattr(sym.ada_type, 'discriminants') and sym.ada_type.discriminants:
                for disc in sym.ada_type.discriminants:
                    if disc.name.lower() == comp_clause.name.lower():
                        disc.offset_bits = position * 8 + first_bit
                        disc.size_bits = last_bit - first_bit + 1
                        found = True
                        break

            # Search variant part components
            if not found and hasattr(sym.ada_type, 'variant_part') and sym.ada_type.variant_part:
                for variant in sym.ada_type.variant_part.variants:
                    if found:
                        break
                    for comp in variant.components:
                        if comp.name.lower() == comp_clause.name.lower():
                            comp.offset_bits = position * 8 + first_bit
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
        for idx, (lit_name, lit_value) in enumerate(decl.values):
            value = self._eval_static_expr(lit_value)

            # Update the position value for this literal
            # EnumerationType.positions is a dict mapping literal name to value
            if sym.ada_type.positions is not None:
                if lit_name is not None:
                    # Named form: match by literal name (case-insensitive)
                    for lit in sym.ada_type.literals:
                        if lit.lower() == lit_name.lower():
                            sym.ada_type.positions[lit] = value
                            break
                else:
                    # Positional form: use index to match against literals
                    if idx < len(sym.ada_type.literals):
                        lit = sym.ada_type.literals[idx]
                        sym.ada_type.positions[lit] = value

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
            # Allow shadowing enumeration literals (Ada allows this)
            elif (existing.kind == SymbolKind.VARIABLE and
                  existing.is_constant and
                  existing.ada_type and
                  existing.ada_type.kind == TypeKind.ENUMERATION):
                # This task type shadows an enumeration literal - allow it
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
                # Add one entry per parameter name (for multiple params of same type)
                # e.g., "PIN1, PIN2 : in Square" has 2 names, so add 2 entries
                for _ in param.names:
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
        is_single = getattr(decl, 'is_single', False)
        task_type = TaskType(
            name=decl.name,
            entries=entries,
            is_single_task=is_single,
        )

        if existing is not None and existing.ada_type and existing.ada_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
            # Completing an incomplete/private type - update the existing symbol
            old_type = existing.ada_type
            existing.kind = SymbolKind.TASK_TYPE
            existing.ada_type = task_type
            existing.definition = decl
            # Fix up access types that referenced the old incomplete type
            self._fixup_access_designated(old_type, task_type)
        else:
            sym_kind = SymbolKind.TASK if is_single else SymbolKind.TASK_TYPE
            symbol = Symbol(
                name=decl.name,
                kind=sym_kind,
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
        elif task_sym.kind not in (SymbolKind.TASK_TYPE, SymbolKind.TASK):
            self.error(f"'{body.name}' is not a task type", body)
            return

        # Enter task body scope
        self.symbols.enter_scope(body.name)

        # Re-register entries from task spec so they're visible in the body
        if task_sym.ada_type and hasattr(task_sym.ada_type, 'entries'):
            for entry_info in task_sym.ada_type.entries:
                entry_sym = Symbol(
                    name=entry_info.name,
                    kind=SymbolKind.ENTRY,
                    parameters=getattr(entry_info, 'parameters', []),
                )
                self.symbols.define(entry_sym)

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
        """Analyze an entry declaration.

        Note: Entries can be overloaded like subprograms, so we don't check
        for duplicate names here. The symbol table handles overloading.
        """
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
                    # Add one entry per parameter name (for multiple params of same type)
                    for _ in param.names:
                        param_types.append(param_type)
                entries.append(EntryInfo(
                    name=item.name,
                    parameter_types=param_types,
                ))
            elif isinstance(item, SubprogramDecl):
                param_types = []
                for param in item.parameters:
                    param_type = self._resolve_type(param.type_mark)
                    # Add one entry per parameter name
                    for _ in param.names:
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
            old_type = existing.ada_type
            existing.kind = SymbolKind.PROTECTED_TYPE
            existing.ada_type = prot_type
            existing.definition = decl
            # Fix up access types that referenced the old incomplete type
            self._fixup_access_designated(old_type, prot_type)
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
        if stub.kind in ("procedure", "function"):
            # Don't overwrite an existing generic spec, but DO allow overloading
            # of procedure/function stubs (the symbol table handles overload chains)
            existing = self.symbols.lookup(stub.name)
            if existing is None or existing.kind in (
                SymbolKind.PROCEDURE, SymbolKind.FUNCTION
            ):
                # Analyze parameters from the stub's subprogram spec
                params = []
                for param_spec in (stub.parameters or []):
                    param_type = self._resolve_type(param_spec.type_mark)
                    for param_name in param_spec.names:
                        param_sym = Symbol(
                            name=param_name,
                            kind=SymbolKind.PARAMETER,
                            ada_type=param_type,
                            mode=param_spec.mode,
                            default_value=param_spec.default_value,
                        )
                        params.append(param_sym)
                ret_type = self._resolve_type(stub.return_type) if stub.return_type else None
                is_func = stub.kind == "function"
                symbol = Symbol(
                    name=stub.name,
                    kind=SymbolKind.FUNCTION if is_func else SymbolKind.PROCEDURE,
                    parameters=params,
                    return_type=ret_type,
                    definition=stub,
                )
                self.symbols.define(symbol)
        elif stub.kind == "package":
            # Check if the package spec is already defined (e.g., generic package)
            # Don't overwrite a GENERIC_PACKAGE with a plain PACKAGE
            existing = self.symbols.lookup(stub.name)
            if existing is None:
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
            return self._build_private_type(name, type_def, is_tagged)
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
    ) -> "AdaType":
        """Build a floating-point or fixed-point type."""
        digits = 6  # Default precision
        range_first = None
        range_last = None
        delta_value = None

        if type_def.is_floating and type_def.digits_expr:
            digits = self._eval_static_expr(type_def.digits_expr)

        # Handle fixed-point delta expression
        if not type_def.is_floating and type_def.delta_expr:
            try:
                delta_value = float(self._eval_static_expr(type_def.delta_expr))
            except (TypeError, ValueError):
                pass

        if type_def.range_constraint:
            # Try to evaluate bounds as floats
            try:
                range_first = float(self._eval_static_expr(type_def.range_constraint.low))
                range_last = float(self._eval_static_expr(type_def.range_constraint.high))
            except (TypeError, ValueError):
                pass

        # Create FixedType for fixed-point, FloatType for floating-point
        if not type_def.is_floating:
            return FixedType(
                name=name,
                size_bits=32,
                delta=delta_value if delta_value is not None else 0.0,
                range_first=range_first if range_first is not None else 0.0,
                range_last=range_last if range_last is not None else 0.0,
                digits=digits if type_def.digits_expr else None,
            )

        return FloatType(
            name=name,
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
            if isinstance(comp_decl.type_mark, AccessTypeIndication):
                # Anonymous access type in record component (e.g., Id : access String)
                designated = self._resolve_type(comp_decl.type_mark.subtype)
                comp_type = AccessType(
                    name=f"access_{designated.name}" if designated else "_anonymous_access",
                    size_bits=16,
                    designated_type=designated,
                    is_access_all=getattr(comp_decl.type_mark, 'is_all', False),
                    is_access_constant=getattr(comp_decl.type_mark, 'is_constant', False),
                )
            else:
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
                    if isinstance(comp_decl.type_mark, AccessTypeIndication):
                        designated = self._resolve_type(comp_decl.type_mark.subtype)
                        comp_type = AccessType(
                            name=f"access_{designated.name}" if designated else "_anonymous_access",
                            size_bits=16,
                            designated_type=designated,
                            is_access_all=getattr(comp_decl.type_mark, 'is_all', False),
                            is_access_constant=getattr(comp_decl.type_mark, 'is_constant', False),
                        )
                    else:
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
        # Check for array index constraint in parent expression
        # e.g., type T is new String(1..10) parses parent_type as Slice
        array_constraint = None
        if isinstance(type_def.parent_type, Slice):
            array_constraint = type_def.parent_type.range_expr

        parent = self._resolve_type(type_def.parent_type)
        if parent is None:
            return None

        # Handle derivation from integer type
        if isinstance(parent, IntegerType):
            low, high = parent.low, parent.high
            # Apply explicit RANGE constraint if present
            if type_def.constraint and isinstance(type_def.constraint, RangeExpr):
                clow = self._try_eval_static(type_def.constraint.low)
                chigh = self._try_eval_static(type_def.constraint.high)
                if isinstance(clow, int) and isinstance(chigh, int):
                    low, high = clow, chigh
            return IntegerType(
                name=name,
                size_bits=parent.size_bits,
                low=low,
                high=high,
                base_type=parent,
                is_derived=True,
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
                base_type=parent,  # Link to parent for type info (e.g., operations)
                is_derived=True,  # Mark as derived type - distinct from parent
            )

        # Handle tagged type derivation with record extension and interfaces
        # Also handle when parent is a generic formal tagged private type
        # (not a RecordType instance but has a record extension)
        parent_is_tagged_record = isinstance(parent, RecordType) and parent.is_tagged
        parent_is_tagged_private = (
            not isinstance(parent, RecordType) and
            getattr(parent, 'is_tagged', False) and
            type_def.record_extension is not None
        )
        if parent_is_tagged_record or parent_is_tagged_private:
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
            is_controlled = getattr(parent, 'is_controlled', False) or (hasattr(parent, 'needs_finalization') and parent.needs_finalization())
            is_limited_controlled = getattr(parent, 'is_limited_controlled', False)

            # Propagate limited status from parent or from explicit declaration
            is_limited = getattr(type_def, 'is_limited', False) or getattr(parent, 'is_limited', False) or (hasattr(parent, 'is_limited_type') and parent.is_limited_type())

            result = RecordType(
                name=name,
                is_tagged=True,
                parent_type=parent,
                components=components,
                interfaces=interfaces,
                is_controlled=is_controlled,
                is_limited_controlled=is_limited_controlled,
                is_limited=is_limited,
            )
            # Mark private extensions so completion in the private part works
            if getattr(type_def, 'is_private_extension', False):
                result.is_private_extension = True
            return result

        # Handle private extension from interface or tagged private type
        # e.g., type Object is new Point with private;
        # Also handles generic formal tagged private parent with private extension
        if getattr(type_def, 'is_private_extension', False) and (
            isinstance(parent, InterfaceType) or getattr(parent, 'is_tagged', False)
        ):
            result = RecordType(
                name=name,
                is_tagged=True,
                parent_type=None,
                components=[],
                interfaces=[parent],
            )
            result.is_private_extension = True
            return result

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

        # Handle derivation from modular type
        if isinstance(parent, ModularType):
            return ModularType(
                name=name,
                size_bits=parent.size_bits,
                modulus=parent.modulus,
            )

        # Handle derivation from fixed type (check before FloatType)
        if isinstance(parent, FixedType) or (
            isinstance(parent, FloatType) and parent.kind == TypeKind.FIXED
        ):
            delta = getattr(parent, "delta", 0.0)
            return FixedType(
                name=name,
                size_bits=parent.size_bits,
                delta=delta,
                range_first=parent.range_first,
                range_last=parent.range_last,
                digits=parent.digits,
                base_type=parent if isinstance(parent, FixedType) else None,
            )

        # Handle derivation from float type
        if isinstance(parent, FloatType):
            return FloatType(
                name=name,
                size_bits=parent.size_bits,
                digits=parent.digits,
                range_first=parent.range_first,
                range_last=parent.range_last,
                base_type=parent,
            )

        # Handle derivation from non-tagged record type
        if isinstance(parent, RecordType) and not parent.is_tagged:
            return RecordType(
                name=name,
                components=list(parent.components),
                discriminants=list(parent.discriminants) if parent.discriminants else [],
                parent_type=parent,
            )

        # Handle derivation from array type
        if isinstance(parent, ArrayType):
            # Apply index constraint if present (e.g., type T is new String(1..10))
            if array_constraint and isinstance(array_constraint, RangeExpr) and not parent.is_constrained:
                low = self._try_eval_static(array_constraint.low)
                high = self._try_eval_static(array_constraint.high)
                if isinstance(low, int) and isinstance(high, int):
                    comp_size = parent.component_type.size_bits if parent.component_type else 8
                    return ArrayType(
                        name=name,
                        size_bits=(high - low + 1) * comp_size,
                        index_types=parent.index_types,
                        component_type=parent.component_type,
                        is_constrained=True,
                        bounds=[(low, high)],
                        base_type=parent,
                    )
                else:
                    # Bounds not statically known but constraint was given —
                    # mark as constrained with placeholder bounds (0, 0)
                    return ArrayType(
                        name=name,
                        size_bits=parent.size_bits,
                        index_types=parent.index_types,
                        component_type=parent.component_type,
                        is_constrained=True,
                        bounds=[(0, 0)],
                        base_type=parent,
                    )
            return ArrayType(
                name=name,
                size_bits=parent.size_bits,
                index_types=parent.index_types,
                component_type=parent.component_type,
                is_constrained=parent.is_constrained,
                bounds=parent.bounds,
                base_type=parent,
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
        self, name: str, type_def: PrivateTypeDef,
        is_tagged: bool = False,
    ) -> AdaType:
        """Build a private type placeholder.

        A private type declaration (type T is private;) creates an opaque
        type that will be completed with a full type definition in the
        private part of the package.
        """
        ada_type = AdaType(
            name=name,
            kind=TypeKind.PRIVATE,
        )
        if is_tagged or getattr(type_def, 'is_tagged', False):
            ada_type.is_tagged = True
        return ada_type

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
            # Try to find the package that contains the parent type
            parent_type_name = None
            if isinstance(parent_type_expr, Identifier):
                parent_type_name = parent_type_expr.name.lower()
            elif isinstance(parent_type_expr, Slice):
                # TYPE T IS NEW PARENT(5..7) parses as Slice with prefix=PARENT
                if isinstance(parent_type_expr.prefix, Identifier):
                    parent_type_name = parent_type_expr.prefix.name.lower()
                elif isinstance(parent_type_expr.prefix, SelectedName):
                    parent_type_name = parent_type_expr.prefix.selector.lower()
            elif isinstance(parent_type_expr, (FunctionCall, IndexedComponent)):
                # TYPE T IS NEW PARENT(5) parses as FunctionCall/IndexedComponent
                fc_name = parent_type_expr.name if isinstance(parent_type_expr, FunctionCall) else parent_type_expr.prefix
                if isinstance(fc_name, Identifier):
                    parent_type_name = fc_name.name.lower()
                elif isinstance(fc_name, SelectedName):
                    parent_type_name = fc_name.selector.lower()
            elif hasattr(parent_type_expr, 'name'):
                parent_type_name = str(parent_type_expr.name).lower()

            if parent_type_name:
                # Check current package first
                if self.current_package:
                    if parent_type_name in self.current_package.public_symbols:
                        parent_package = self.current_package
                    elif parent_type_name in self.current_package.private_symbols:
                        parent_package = self.current_package

                # If not found, search visible packages for this type
                if parent_package is None:
                    # Look up the parent type symbol and find its defining package
                    parent_sym = self.symbols.lookup(parent_type_name)
                    if parent_sym and hasattr(parent_sym, 'defining_package'):
                        parent_package = parent_sym.defining_package
                    else:
                        # Search all visible packages in the scope stack
                        for scope in self.symbols.scope_stack:
                            for sym_name, sym in scope.symbols.items():
                                if sym.kind == SymbolKind.PACKAGE and sym.public_symbols:
                                    if parent_type_name in sym.public_symbols:
                                        parent_package = sym
                                        break
                            if parent_package:
                                break

        # If we couldn't find a package for the direct parent type name,
        # follow the base_type chain (e.g., SUBPARENT -> PARENT -> in PKG)
        # Track the base type we found in the package for primitive matching,
        # but keep parent_type as the original for type substitution.
        primitive_match_type = parent_type
        if parent_package is None or parent_package.kind != SymbolKind.PACKAGE:
            base = parent_type
            while base is not None:
                base_name = base.name.lower() if hasattr(base, 'name') else None
                if base_name:
                    for scope in self.symbols.scope_stack:
                        for sym_name, sym in scope.symbols.items():
                            if sym.kind == SymbolKind.PACKAGE and sym.public_symbols:
                                if base_name in sym.public_symbols:
                                    parent_package = sym
                                    primitive_match_type = base
                                    break
                        if parent_package:
                            break
                if parent_package:
                    break
                if hasattr(base, 'base_type') and base.base_type:
                    base = base.base_type
                elif isinstance(base, RecordType) and base.parent_type:
                    base = base.parent_type
                else:
                    break

        if parent_package is None or parent_package.kind != SymbolKind.PACKAGE:
            return

        # Find primitive operations in the parent package
        # A primitive operation has the parent type as parameter or return type.
        # When deriving from a subtype (e.g., TYPE S IS NEW SUBPARENT where
        # SUBPARENT IS PARENT(TRUE, 3)), primitive_match_type is the base type
        # (PARENT) whose operations we inherit.
        for sym_name, sym in parent_package.public_symbols.items():
            if sym.kind not in (SymbolKind.FUNCTION, SymbolKind.PROCEDURE):
                continue

            # Check if this is a primitive operation of the match type
            is_primitive = False

            # Check return type (for functions)
            if sym.return_type and same_type(sym.return_type, primitive_match_type):
                is_primitive = True

            # Check parameter types
            for param in sym.parameters:
                if param.ada_type and same_type(param.ada_type, primitive_match_type):
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
                # Return type: substitute primitive_match_type with derived_type
                return_type=derived_type if sym.return_type and same_type(sym.return_type, primitive_match_type) else sym.return_type,
                parameters=[],
                definition=sym.definition,
            )
            # Preserve intrinsic flag so inherited operators use inline code
            if getattr(sym, 'is_intrinsic', False):
                inherited_sym.is_intrinsic = True

            # Copy parameters, substituting types as needed
            for param in sym.parameters:
                param_type = derived_type if param.ada_type and same_type(param.ada_type, primitive_match_type) else param.ada_type
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
            # Package.Type or Parent.Child.Type
            prefix_name = self._get_identifier_name(type_expr.prefix)
            if prefix_name:
                symbol = self.symbols.lookup_selected(
                    prefix_name, type_expr.selector
                )
                if symbol and symbol.ada_type:
                    return symbol.ada_type
            # Handle hierarchical names like System.Storage_Elements.Integer_Address
            prefix_sym = self._resolve_hierarchical_package(type_expr.prefix)
            if prefix_sym and prefix_sym.public_symbols:
                selector = type_expr.selector.lower()
                type_sym = prefix_sym.public_symbols.get(selector)
                if type_sym and type_sym.ada_type:
                    return type_sym.ada_type
        elif isinstance(type_expr, AttributeReference):
            # Handle type attributes like Type'Class, Type'Base
            if type_expr.attribute.lower() == 'class':
                # Resolve the prefix type
                prefix_type = self._resolve_type(type_expr.prefix)
                if prefix_type and isinstance(prefix_type, RecordType) and prefix_type.is_tagged:
                    # Return the class-wide type
                    return prefix_type.get_class_wide_type()
            elif type_expr.attribute.lower() == 'base':
                # Base attribute returns the base type
                prefix_type = self._resolve_type(type_expr.prefix)
                if prefix_type:
                    return get_root_type(prefix_type)
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

        base_type = self._resolve_type(type_mark)
        if base_type is None:
            return None

        # Apply range constraint if present (e.g., Integer range -100 .. 100)
        if subtype_ind.constraint and isinstance(subtype_ind.constraint, RangeConstraint):
            range_expr = subtype_ind.constraint.range_expr
            if isinstance(range_expr, RangeExpr):
                low = self._try_eval_static(range_expr.low)
                high = self._try_eval_static(range_expr.high)
                if isinstance(low, int) and isinstance(high, int):
                    if isinstance(base_type, IntegerType):
                        return IntegerType(
                            name=base_type.name,
                            size_bits=base_type.size_bits,
                            low=low,
                            high=high,
                            base_type=base_type,
                        )
                    elif isinstance(base_type, EnumerationType):
                        return EnumerationType(
                            name=base_type.name,
                            size_bits=base_type.size_bits,
                            literals=base_type.literals.copy(),
                            positions=base_type.positions.copy(),
                            base_type=base_type,
                        )

        return base_type

    def _get_identifier_name(self, expr: Expr) -> Optional[str]:
        """Get the name from an identifier expression."""
        if isinstance(expr, Identifier):
            return expr.name
        return None

    def _is_character_literal_symbol(self, sym: Symbol) -> bool:
        """Check if a symbol is a character literal (from Character or derived type).

        Character literals don't conflict with identifiers in Ada because
        'T' (character literal) and T (identifier) are syntactically distinct.
        """
        if sym is None:
            return False
        # Character literals are stored as single-character names
        if len(sym.name) != 1:
            return False
        # Must be a constant (literals are constants)
        if not sym.is_constant:
            return False
        # Must be from an enumeration type (Character is an enumeration)
        if sym.ada_type is None or sym.ada_type.kind != TypeKind.ENUMERATION:
            return False
        return True

    # Sentinel to distinguish "found procedure (return type None)" from "not found"
    _PROCEDURE_FOUND = AdaType(name="_procedure_found", kind=TypeKind.PRIVATE)

    def _find_prefix_notation_primitive(
        self, tagged_type, selector: str
    ) -> Optional[AdaType]:
        """Find a primitive operation for prefix notation calls.

        In Ada 2005+, you can call X.Method(Args) where Method is a primitive
        operation that takes X (or access to X, or X'Class) as the first parameter.

        Returns the return type for functions, _PROCEDURE_FOUND sentinel for
        procedures, or None if not found.
        """
        selector_lower = selector.lower()

        def check_symbol(sym: Symbol) -> Optional[AdaType]:
            """Check if symbol is a matching primitive operation."""
            while sym is not None:
                if sym.kind in (SymbolKind.FUNCTION, SymbolKind.PROCEDURE):
                    if sym.parameters:
                        first_param = sym.parameters[0]
                        first_type = first_param.ada_type

                        # Check if first param matches the tagged type
                        is_match = False
                        if first_type:
                            tagged_name = tagged_type.name if hasattr(tagged_type, 'name') else str(tagged_type)
                            # Direct match
                            if same_type(first_type, tagged_type):
                                is_match = True
                            # Derived type match: tagged_type derives from first_type
                            # e.g., TQ.Base_Func where Base_Func takes TP param
                            elif (isinstance(first_type, RecordType) and
                                  first_type.is_tagged and
                                  is_derived_from(tagged_type, first_type.name)):
                                is_match = True
                            # Class-wide type match: param is T'Class, obj is T or derived
                            elif (hasattr(first_type, 'is_class_wide') and first_type.is_class_wide):
                                specific = getattr(first_type, 'specific_type', None)
                                if specific:
                                    if same_type(specific, tagged_type) or is_derived_from(tagged_type, specific.name):
                                        is_match = True
                                elif hasattr(first_type, 'name') and first_type.name.endswith("'Class"):
                                    base = first_type.name[:-6]
                                    if tagged_name == base or is_derived_from(tagged_type, base):
                                        is_match = True
                            # Access to tagged type match
                            elif isinstance(first_type, AccessType):
                                designated = first_type.designated_type
                                if designated:
                                    if same_type(designated, tagged_type):
                                        is_match = True
                                    elif is_derived_from(tagged_type, designated.name):
                                        is_match = True
                                    # Access to class-wide
                                    elif hasattr(designated, 'is_class_wide') and designated.is_class_wide:
                                        sp = getattr(designated, 'specific_type', None)
                                        if sp and (same_type(sp, tagged_type) or is_derived_from(tagged_type, sp.name)):
                                            is_match = True

                        if is_match:
                            # Return return_type for functions, sentinel for procedures
                            if sym.return_type is not None:
                                return sym.return_type
                            return SemanticAnalyzer._PROCEDURE_FOUND

                # Check for overloaded version
                sym = sym.overloaded_next
            return None

        # Look in current and all enclosing scopes
        sym = self.symbols.lookup(selector_lower)
        result = check_symbol(sym)
        if result is not None:
            return result

        # For prefix notation, Ada requires searching the package where the
        # tagged type (or its ancestors) is declared (Ada RM 4.1.3).
        # Search through all visible package symbols' public_symbols.
        def search_package_symbols(pkg_sym: Symbol) -> Optional[AdaType]:
            if not pkg_sym.public_symbols:
                return None
            sel_sym = pkg_sym.public_symbols.get(selector_lower)
            if sel_sym:
                res = check_symbol(sel_sym)
                if res is not None:
                    return res
            # Search nested child packages
            for child_name, child_sym in pkg_sym.public_symbols.items():
                if child_sym.kind in (SymbolKind.PACKAGE, SymbolKind.GENERIC_PACKAGE):
                    res = search_package_symbols(child_sym)
                    if res is not None:
                        return res
            return None

        scope = self.symbols.current_scope
        while scope is not None:
            for sym_name, sym_val in scope.symbols.items():
                if sym_val.kind in (SymbolKind.PACKAGE, SymbolKind.GENERIC_PACKAGE):
                    res = search_package_symbols(sym_val)
                    if res is not None:
                        return res
            scope = scope.parent

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
                # Get entity name - handle ActualParameter wrapper
                entity = stmt.args[1]
                if isinstance(entity, ActualParameter):
                    entity = entity.value
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_imported = True
                        # External name is optional
                        if len(stmt.args) >= 3:
                            ext_name = stmt.args[2]
                            # Handle ActualParameter wrapper
                            if isinstance(ext_name, ActualParameter):
                                ext_name = ext_name.value
                            if isinstance(ext_name, StringLiteral):
                                sym.external_name = ext_name.value
                            elif isinstance(ext_name, Identifier):
                                sym.external_name = ext_name.name

        elif pragma_name == "inline":
            # pragma Inline(subprogram);
            if stmt.args:
                entity = stmt.args[0]
                # Handle ActualParameter wrapper
                if isinstance(entity, ActualParameter):
                    entity = entity.value
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_inline = True

        elif pragma_name == "volatile":
            # pragma Volatile(variable);
            if stmt.args:
                entity = stmt.args[0]
                # Handle ActualParameter wrapper
                if isinstance(entity, ActualParameter):
                    entity = entity.value
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_volatile = True

        elif pragma_name == "atomic":
            # pragma Atomic(variable);
            # Atomic implies volatile behavior plus indivisible access
            if stmt.args:
                entity = stmt.args[0]
                # Handle ActualParameter wrapper
                if isinstance(entity, ActualParameter):
                    entity = entity.value
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_atomic = True
                        sym.is_volatile = True  # Atomic implies volatile

        elif pragma_name == "no_return":
            # pragma No_Return(procedure);
            if stmt.args:
                entity = stmt.args[0]
                # Handle ActualParameter wrapper
                if isinstance(entity, ActualParameter):
                    entity = entity.value
                if isinstance(entity, Identifier):
                    sym = self.symbols.lookup(entity.name)
                    if sym:
                        sym.is_no_return = True

        elif pragma_name == "pack":
            # pragma Pack(type);
            if stmt.args:
                entity = stmt.args[0]
                # Handle ActualParameter wrapper
                if isinstance(entity, ActualParameter):
                    entity = entity.value
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
                # Handle ActualParameter wrapper
                if isinstance(entity, ActualParameter):
                    entity = entity.value
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
                # Handle ActualParameter wrapper
                if isinstance(entity, ActualParameter):
                    entity = entity.value
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
        cond_type = self._analyze_expr(stmt.condition, expected_type=PREDEFINED_TYPES["Boolean"])
        self._check_boolean(cond_type, stmt.condition)

        for s in stmt.then_stmts:
            self._analyze_statement(s)

        for cond, stmts in stmt.elsif_parts:
            cond_type = self._analyze_expr(cond, expected_type=PREDEFINED_TYPES["Boolean"])
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
                cond_type = self._analyze_expr(stmt.iteration_scheme.condition, expected_type=PREDEFINED_TYPES["Boolean"])
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
        """Analyze a block statement.

        If the block has a label, the label can be used as a prefix to access
        declarations inside the block (e.g., DD.P1 where DD is a block label).
        """
        # Create a symbol for the block label if present
        block_symbol = None
        if stmt.label:
            block_symbol = Symbol(
                name=stmt.label,
                kind=SymbolKind.LABEL,
            )
            # Define the block label in the current scope (before entering block scope)
            self.symbols.define(block_symbol)

        self.symbols.enter_scope(stmt.label if stmt.label else None)

        for decl in stmt.declarations:
            self._analyze_declaration(decl)

        for s in stmt.statements:
            self._analyze_statement(s)

        # If block has a label, collect all declarations for prefix access
        if block_symbol:
            block_symbol.public_symbols = {}
            for sym in self.symbols.current_scope_symbols():
                block_symbol.public_symbols[sym.name.lower()] = sym

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
            cond_type = self._analyze_expr(stmt.condition, expected_type=PREDEFINED_TYPES["Boolean"])
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
                # Pass expected return type for proper overload resolution of literals
                expected_return = self.current_subprogram.return_type
                value_type = self._analyze_expr(stmt.value, expected_type=expected_return)
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
            elif isinstance(stmt.exception_name, SelectedName):
                symbol = self._resolve_hierarchical_package(stmt.exception_name)
                if symbol is None:
                    name = self._get_hierarchical_name(stmt.exception_name)
                    self.error(f"exception '{name}' not found", stmt)
                elif symbol.kind != SymbolKind.EXCEPTION:
                    name = self._get_hierarchical_name(stmt.exception_name)
                    self.error(f"'{name}' is not an exception", stmt)

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
        entry_sym = None
        if self.current_task:
            # Count parameters in accept statement
            accept_param_count = sum(len(p.names) for p in stmt.parameters)

            # Look for the entry in the task type's entries
            if self.current_task.ada_type and hasattr(self.current_task.ada_type, 'entries'):
                for entry_info in self.current_task.ada_type.entries:
                    if entry_info.name.lower() == stmt.entry_name.lower():
                        # Check if parameter count matches (for overload resolution)
                        if len(entry_info.parameter_types) == accept_param_count:
                            entry_sym = entry_info
                            break

            # Also check current scope for entries (for single tasks defined inline)
            # Need to check the full overload chain
            if entry_sym is None:
                sym = self.symbols.lookup(stmt.entry_name)
                while sym is not None:
                    if sym.kind == SymbolKind.ENTRY:
                        # Check if parameter count matches
                        if len(sym.parameters) == accept_param_count:
                            entry_sym = sym
                            break
                    sym = sym.overloaded_next

            if entry_sym is None:
                # Check if any entry exists with that name (for error message)
                any_entry = self.symbols.lookup(stmt.entry_name)
                if any_entry and any_entry.kind == SymbolKind.ENTRY:
                    self.error(
                        f"no matching entry '{stmt.entry_name}' for accept with {accept_param_count} parameters",
                        stmt,
                    )
                else:
                    self.error(f"entry '{stmt.entry_name}' not found in current task", stmt)

        # Enter a scope for the accept body
        self.symbols.enter_scope(f"accept_{stmt.entry_name}")

        # Add accept parameters to scope
        for param_spec in stmt.parameters:
            param_type = self._resolve_type(param_spec.type_mark)
            for param_name in param_spec.names:
                param_sym = Symbol(
                    name=param_name,
                    kind=SymbolKind.PARAMETER,
                    ada_type=param_type,
                    mode=param_spec.mode or "in",
                )
                self.symbols.define(param_sym)

        # Make entry name visible for attributes like E'COUNT
        if entry_sym:
            # Create a symbol that allows entry attribute access
            entry_ref = Symbol(
                name=stmt.entry_name,
                kind=SymbolKind.ENTRY,
                ada_type=entry_sym.ada_type if hasattr(entry_sym, 'ada_type') else None,
            )
            self.symbols.define(entry_ref)

        # Analyze the statements in the accept body (requeue is valid here)
        old_in_accept = self.in_accept_or_entry
        self.in_accept_or_entry = True
        for s in stmt.statements:
            self._analyze_statement(s)
        self.in_accept_or_entry = old_in_accept

        self.symbols.leave_scope()

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
            if symbol.kind in (SymbolKind.VARIABLE, SymbolKind.CONSTANT, SymbolKind.PARAMETER):
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

            if symbol.kind not in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION,
                                    SymbolKind.GENERIC_PROCEDURE, SymbolKind.GENERIC_FUNCTION,
                                    SymbolKind.ENTRY):
                self.error(f"'{stmt.name.name}' is not a procedure", stmt)
                return

            # Try to resolve overloaded call
            overloads = self.symbols.all_overloads(stmt.name.name)
            overloads = [o for o in overloads if o.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION,
                                                             SymbolKind.GENERIC_PROCEDURE, SymbolKind.GENERIC_FUNCTION,
                                                             SymbolKind.ENTRY)]

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

            # Check type compatibility for each argument (handle named args)
            all_match = True
            exact_matches = 0
            for i, arg in enumerate(args):
                if i >= len(arg_types):
                    break
                arg_type = arg_types[i]
                # Match argument to parameter (named or positional)
                if hasattr(arg, 'name') and arg.name:
                    param = None
                    for p in candidate.parameters:
                        if p.name.lower() == arg.name.lower():
                            param = p
                            break
                    if param is None:
                        all_match = False
                        break
                elif i < len(candidate.parameters):
                    param = candidate.parameters[i]
                else:
                    all_match = False
                    break
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

    def _resolve_overload(self, symbol: Symbol, args: list, node) -> Optional[Symbol]:
        """Try to find the best overload match among all visible overloads.

        Returns the best matching overload, or None to use the default.
        """
        # Analyze argument types without reporting errors
        arg_types = []
        for arg in args:
            arg_expr = arg.value if hasattr(arg, 'value') else arg
            # Don't double-analyze - just get the type
            t = self._analyze_expr(arg_expr)
            arg_types.append(t)

        # Collect all overloads (follow overloaded_next chain + check outer scopes)
        candidates = []
        sym = symbol
        while sym is not None:
            if sym.kind == SymbolKind.FUNCTION and len(sym.parameters) == len(args):
                candidates.append(sym)
            sym = getattr(sym, 'overloaded_next', None)

        # Also search outer scopes for same-named functions
        for scope in self.symbols.scope_stack:
            s = scope.lookup_local(symbol.name.lower())
            while s is not None:
                if s.kind == SymbolKind.FUNCTION and len(s.parameters) == len(args):
                    if s not in candidates:
                        candidates.append(s)
                s = getattr(s, 'overloaded_next', None)

        if len(candidates) <= 1:
            return None  # No overloads to resolve

        # Score each candidate
        best_score = -1
        best_sym = None
        for cand in candidates:
            score = 0
            match = True
            for i, (param, arg_type) in enumerate(zip(cand.parameters, arg_types)):
                if arg_type is None or param.ada_type is None:
                    continue
                if types_compatible(param.ada_type, arg_type):
                    score += 1
                else:
                    match = False
                    break
            if match and score > best_score:
                best_score = score
                best_sym = cand

        return best_sym

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

        # Match arguments to parameters (handle named arguments)
        matched_params = []
        for i, arg in enumerate(args):
            if hasattr(arg, 'name') and arg.name:
                # Named argument: find matching parameter
                param = None
                for p in subprog.parameters:
                    if p.name.lower() == arg.name.lower():
                        param = p
                        break
                if param is None:
                    self.error(
                        f"no parameter named '{arg.name}'",
                        arg.value if arg.value else node,
                    )
                    continue
            elif i < len(subprog.parameters):
                param = subprog.parameters[i]
            else:
                continue
            matched_params.append((arg, param))

        for arg, param in matched_params:
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
        """Check that a type is Boolean or derived from Boolean.

        In Ada, any type derived from Boolean can be used in Boolean contexts
        (if conditions, while conditions, etc.).
        """
        if t is None:
            return
        # Accept Boolean or any type derived from Boolean
        if is_derived_from(t, "Boolean"):
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
            # Ada RM 4.2(9): String literals can match any array type with character component
            # Ada RM 3.5.2(3): A "character type" is any enumeration type containing
            # at least one character literal (like 'A', 'B', etc.)
            if expected_type and isinstance(expected_type, ArrayType):
                comp_type = expected_type.component_type
                if comp_type:
                    is_char_type = False
                    # Check if component is Character or derived from Character
                    if hasattr(comp_type, 'name') and comp_type.name == 'Character':
                        is_char_type = True
                    elif hasattr(comp_type, 'base_type'):
                        base = comp_type.base_type
                        while base:
                            if hasattr(base, 'name') and base.name == 'Character':
                                is_char_type = True
                                break
                            base = getattr(base, 'base_type', None)
                    # Check if it's an enumeration type with character literals
                    # Ada RM 3.5.2(3): enumeration types containing character literals
                    # are considered "character types"
                    if not is_char_type and hasattr(comp_type, 'kind'):
                        if comp_type.kind == TypeKind.ENUMERATION:
                            # Check if literals contain character literals (single chars)
                            if hasattr(comp_type, 'literals') and comp_type.literals:
                                for lit in comp_type.literals:
                                    if isinstance(lit, str) and len(lit) == 1:
                                        is_char_type = True
                                        break
                    if is_char_type:
                        return expected_type
            return PREDEFINED_TYPES["String"]
        elif isinstance(expr, CharacterLiteral):
            # Check if expected_type is a character enumeration type containing this literal
            if expected_type and hasattr(expected_type, 'literals'):
                char_val = expr.value
                if char_val in expected_type.literals:
                    return expected_type
            # Also check for derived character types
            if expected_type and hasattr(expected_type, 'base_type'):
                base = expected_type.base_type
                if hasattr(base, 'literals') and expr.value in base.literals:
                    return expected_type
            return PREDEFINED_TYPES["Character"]
        elif isinstance(expr, NullLiteral):
            return None  # Type determined by context
        elif isinstance(expr, BinaryExpr):
            return self._analyze_binary_expr(expr, expected_type)
        elif isinstance(expr, UnaryExpr):
            return self._analyze_unary_expr(expr)
        elif isinstance(expr, RangeExpr):
            return self._analyze_range_expr(expr)
        elif isinstance(expr, IndexedComponent):
            return self._analyze_indexed_component(expr)
        elif isinstance(expr, SelectedName):
            return self._analyze_selected_name(expr, expected_type)
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
            return self._analyze_allocator(expr, expected_type=expected_type)
        elif isinstance(expr, (ConditionalExpr, IfExpr)):
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
        elif isinstance(expr, BoxExpr):
            # Box (<>) in generic instantiation context - just a placeholder
            return None
        elif isinstance(expr, SubtypeIndication):
            return self._resolve_subtype_indication(expr)

        return None

    def _analyze_allocator(self, expr: Allocator, expected_type: Optional[AdaType] = None) -> Optional[AdaType]:
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

        # If context expects a named access type, use it (Ada context-determined resolution)
        if expected_type and isinstance(expected_type, AccessType):
            return expected_type

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
            # Resolve incomplete/private types
            if designated and designated.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
                completed = self.symbols.lookup_type(designated.name)
                if completed:
                    designated = completed
            if isinstance(designated, ArrayType):
                prefix_type = designated

        # Resolve private types for direct array access (e.g., X2(1) where type is private array)
        if prefix_type and prefix_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
            completed = self.symbols.lookup_type(prefix_type.name)
            if completed:
                prefix_type = completed

        # Prefix must be an array type
        if not isinstance(prefix_type, ArrayType):
            self.error(
                f"slice prefix must be an array type, got '{prefix_type.name}'",
                expr.prefix,
            )
            return None

        # Analyze the range expression
        self._analyze_expr(expr.range_expr)

        # Slice of an array returns the unconstrained base type
        # Follow base_type chain to get the root unconstrained array type
        result_type = prefix_type
        while isinstance(result_type, ArrayType) and result_type.base_type and not result_type.is_derived:
            result_type = result_type.base_type
        return ArrayType(
            name=result_type.name,
            kind=result_type.kind,
            size_bits=0,  # Size depends on range at runtime
            index_types=result_type.index_types,
            component_type=result_type.component_type,
            is_constrained=False,
        )

    def _analyze_dereference(self, expr: Dereference) -> Optional[AdaType]:
        """Analyze a dereference expression (P.all)."""
        # Get the prefix type
        prefix_type = self._analyze_expr(expr.prefix)
        if prefix_type is None:
            return None

        # Prefix must be an access type (AccessType or AccessSubprogramType)
        from uada80.type_system import AccessSubprogramType
        if not isinstance(prefix_type, (AccessType, AccessSubprogramType)):
            self.error(
                f"dereference prefix must be an access type, got '{prefix_type.name}'",
                expr.prefix,
            )
            return None

        # Return the designated type, resolving incomplete/private types
        if isinstance(prefix_type, AccessSubprogramType):
            # For access-to-subprogram, .all returns the subprogram type itself
            return prefix_type
        designated = prefix_type.designated_type
        if designated and designated.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
            # Try to find the completed type
            completed = self.symbols.lookup_type(designated.name)
            if completed:
                designated = completed
        return designated

    def _resolve_private_type(self, ada_type: Optional[AdaType]) -> Optional[AdaType]:
        """Resolve incomplete/private types to their completed definitions.

        When inside a package body, private types declared in the spec
        should be resolved to their full definitions.
        """
        if ada_type is None:
            return None
        if ada_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
            completed = self.symbols.lookup_type(ada_type.name)
            if completed:
                return completed
        return ada_type

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

    def _analyze_binary_expr(self, expr: BinaryExpr, expected_type: Optional[AdaType] = None) -> Optional[AdaType]:
        """Analyze a binary expression."""
        # For relational operators, don't propagate expected_type (e.g. Boolean)
        # to operands - operand types are independent of the result type
        is_relational = expr.op in (
            BinaryOp.EQ, BinaryOp.NE, BinaryOp.LT,
            BinaryOp.LE, BinaryOp.GT, BinaryOp.GE,
        )
        operand_expected = None if is_relational else expected_type
        left_type = self._analyze_expr(expr.left, expected_type=operand_expected)
        right_type = self._analyze_expr(expr.right, expected_type=operand_expected)

        # Relational operators return Boolean (unless user-defined to return something else)
        if expr.op in (
            BinaryOp.EQ,
            BinaryOp.NE,
            BinaryOp.LT,
            BinaryOp.LE,
            BinaryOp.GT,
            BinaryOp.GE,
        ):
            # First check for user-defined relational operator
            if left_type and right_type:
                op_name = {
                    BinaryOp.EQ: "=", BinaryOp.NE: "/=", BinaryOp.LT: "<",
                    BinaryOp.LE: "<=", BinaryOp.GT: ">", BinaryOp.GE: ">="
                }.get(expr.op)
                if op_name:
                    overloads = self.symbols.all_overloads(op_name)
                    matches = []
                    for candidate in overloads:
                        if candidate.kind == SymbolKind.FUNCTION and len(candidate.parameters) == 2:
                            param1_type = candidate.parameters[0].ada_type
                            param2_type = candidate.parameters[1].ada_type
                            if (param1_type and param2_type and
                                types_compatible(param1_type, left_type) and
                                types_compatible(param2_type, right_type)):
                                matches.append(candidate)
                    if matches:
                        # When expected_type is available (e.g. Boolean from IF condition),
                        # prefer the operator whose return type matches it
                        if expected_type:
                            for m in matches:
                                if m.return_type and types_compatible(m.return_type, expected_type):
                                    return m.return_type
                            # No match for expected_type - for relational ops, fall through
                            # to predefined Boolean if that's what's expected
                            if types_compatible(PREDEFINED_TYPES["Boolean"], expected_type):
                                pass  # fall through to return Boolean below
                            else:
                                return matches[0].return_type
                        else:
                            return matches[0].return_type
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
            # Check for user-defined AND/OR/XOR operator before requiring Boolean
            op_name = {BinaryOp.AND: "and", BinaryOp.OR: "or", BinaryOp.XOR: "xor"}.get(expr.op)
            if op_name and left_type and right_type:
                overloads = self.symbols.all_overloads(op_name)
                for candidate in overloads:
                    if candidate.kind == SymbolKind.FUNCTION and len(candidate.parameters) == 2:
                        param1_type = candidate.parameters[0].ada_type
                        param2_type = candidate.parameters[1].ada_type
                        if (param1_type and param2_type and
                            types_compatible(param1_type, left_type) and
                            types_compatible(param2_type, right_type)):
                            return candidate.return_type
            # For Boolean, these are logical operators
            self._check_boolean(left_type, expr.left)
            self._check_boolean(right_type, expr.right)
            return PREDEFINED_TYPES["Boolean"]

        # Exponentiation is special: X ** N where N must be integer, result is type of X
        if expr.op == BinaryOp.EXP:
            if left_type and right_type:
                # First check for user-defined "**" operator
                overloads = self.symbols.all_overloads("**")
                for candidate in overloads:
                    if candidate.kind == SymbolKind.FUNCTION and len(candidate.parameters) == 2:
                        param1_type = candidate.parameters[0].ada_type
                        param2_type = candidate.parameters[1].ada_type
                        if (param1_type and param2_type and
                            types_compatible(param1_type, left_type) and
                            types_compatible(param2_type, right_type)):
                            return candidate.return_type
                # No matching user-defined operator - apply built-in rule
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
                # First check for user-defined arithmetic operator
                op_name = {
                    BinaryOp.ADD: "+", BinaryOp.SUB: "-", BinaryOp.MUL: "*",
                    BinaryOp.DIV: "/", BinaryOp.MOD: "mod", BinaryOp.REM: "rem"
                }.get(expr.op)
                if op_name:
                    overloads = self.symbols.all_overloads(op_name)
                    # Collect all matching candidates
                    matches = []
                    for candidate in overloads:
                        if candidate.kind == SymbolKind.FUNCTION and len(candidate.parameters) == 2:
                            param1_type = candidate.parameters[0].ada_type
                            param2_type = candidate.parameters[1].ada_type
                            if (param1_type and param2_type and
                                types_compatible(param1_type, left_type) and
                                types_compatible(param2_type, right_type)):
                                matches.append(candidate)
                    if matches:
                        # When expected_type is available, prefer the
                        # operator whose return type matches it (Ada RM
                        # 8.6 context-based overload resolution).
                        if expected_type and len(matches) > 1:
                            for m in matches:
                                if m.return_type and types_compatible(m.return_type, expected_type):
                                    return m.return_type
                        return matches[0].return_type
                # No user-defined operator - use common_type
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
            # If expected_type is an array and operands couldn't resolve, use it
            if expected_type and isinstance(expected_type, ArrayType):
                return expected_type
            # For character/string literals, default to String
            if (left_type and left_type.kind in (TypeKind.ENUMERATION, TypeKind.UNIVERSAL_INTEGER) and
                getattr(left_type, 'name', '') == 'Character'):
                return PREDEFINED_TYPES["String"]
            if (right_type and right_type.kind in (TypeKind.ENUMERATION, TypeKind.UNIVERSAL_INTEGER) and
                getattr(right_type, 'name', '') == 'Character'):
                return PREDEFINED_TYPES["String"]
            # For element & element (e.g., record concatenation), build anonymous array
            if left_type and right_type and types_compatible(left_type, right_type):
                return ArrayType(
                    name=f"<anonymous_array_of_{left_type.name}>",
                    component_type=left_type,
                    is_constrained=False,
                )
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
            # Check for user-defined NOT operator before requiring Boolean
            if operand_type:
                overloads = self.symbols.all_overloads('not')
                for candidate in overloads:
                    if candidate.kind == SymbolKind.FUNCTION and len(candidate.parameters) == 1:
                        param_type = candidate.parameters[0].ada_type
                        if param_type and types_compatible(param_type, operand_type):
                            return candidate.return_type
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
                # Re-resolve overloaded literals with context from the other
                # bound. E.g., 'A' .. ENUM'(Z) — 'A' defaulted to Character
                # but should be ENUM since ENUM has 'A' as a literal.
                # Also handles identifier enum literals: FALSE .. B3'(TRUE).
                def _is_resolvable_literal(e):
                    return isinstance(e, CharacterLiteral) or isinstance(e, Identifier)

                reresolved = False
                if (_is_resolvable_literal(expr.low) and
                        high_type.kind == TypeKind.ENUMERATION and
                        low_type != high_type):
                    new_low = self._analyze_expr(expr.low, expected_type=high_type)
                    if new_low and common_type(new_low, high_type) is not None:
                        low_type = new_low
                        reresolved = True
                if not reresolved and (_is_resolvable_literal(expr.high) and
                        low_type.kind == TypeKind.ENUMERATION and
                        low_type != high_type):
                    new_high = self._analyze_expr(expr.high, expected_type=low_type)
                    if new_high and common_type(new_high, low_type) is not None:
                        high_type = new_high
                        reresolved = True
                if reresolved:
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
            # First check if this is an operator call like "ABS"(X) or "+"(A, B)
            # The parser converts quoted operator names to identifiers
            op_name = expr.prefix.name.upper()
            UNARY_OPS = {'ABS', 'NOT', '+', '-'}  # + and - can be unary
            BINARY_OPS = {'+', '-', '*', '/', 'MOD', 'REM', '**', '&',
                          'AND', 'OR', 'XOR', '=', '/=', '<', '>', '<=', '>='}

            # Handle unary operators (including unary + and -)
            if op_name in UNARY_OPS and len(expr.indices) == 1:
                # Unary operator call: "ABS"(X) -> abs X, "+"(X) -> +X, "-"(X) -> -X
                arg_type = self._analyze_expr(expr.indices[0])
                if arg_type is None:
                    return None
                # Check that the type supports this operator
                if op_name == 'ABS':
                    if arg_type.kind in (TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.FLOAT, TypeKind.FIXED,
                                         TypeKind.UNIVERSAL_INTEGER, TypeKind.UNIVERSAL_REAL):
                        return arg_type
                    self.error(f"operator 'abs' not defined for type '{arg_type.name}'", expr)
                    return None
                elif op_name in ('+', '-'):
                    # Unary + and - are valid for numeric types
                    if arg_type.is_numeric():
                        return arg_type
                    self.error(f"operator '{op_name.lower()}' not defined for type '{arg_type.name}'", expr)
                    return None
                elif op_name == 'NOT':
                    if arg_type.name and arg_type.name.lower() == 'boolean':
                        return arg_type
                    if arg_type.kind == TypeKind.MODULAR:
                        return arg_type  # Bitwise not for modular types
                    if arg_type.kind == TypeKind.ARRAY:
                        # Array of Boolean - element-wise not
                        return arg_type
                    self.error(f"operator 'not' not defined for type '{arg_type.name}'", expr)
                    return None

            if op_name in BINARY_OPS and len(expr.indices) == 2:
                # Binary operator call: "+"(A, B) -> A + B
                left_type = self._analyze_expr(expr.indices[0])
                right_type = self._analyze_expr(expr.indices[1])
                if left_type is None or right_type is None:
                    return None
                # For arithmetic operators, return common numeric type
                if op_name in {'+', '-', '*', '/', 'MOD', 'REM', '**'}:
                    if left_type.kind in (TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.FLOAT):
                        return left_type
                # For comparison operators, return Boolean
                if op_name in {'=', '/=', '<', '>', '<=', '>='}:
                    return self.symbols.lookup_type('Boolean')
                # For logical operators
                if op_name in {'AND', 'OR', 'XOR'}:
                    if left_type.name and left_type.name.lower() == 'boolean':
                        return left_type
                    if left_type.kind == TypeKind.MODULAR:
                        return left_type  # Bitwise for modular
                # For concatenation
                if op_name == '&':
                    return left_type
                # For non-built-in types, fall through to user-defined operator lookup
                # (don't return left_type blindly - let the function overload check handle it)

            # Also handle unary + and - with single argument
            if op_name in {'+', '-'} and len(expr.indices) == 1:
                arg_type = self._analyze_expr(expr.indices[0])
                if arg_type and arg_type.kind in (TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.FLOAT):
                    return arg_type

            symbol = self.symbols.lookup(expr.prefix.name)
            if symbol and symbol.kind in (
                SymbolKind.TYPE, SymbolKind.SUBTYPE,
                SymbolKind.TASK_TYPE, SymbolKind.PROTECTED_TYPE,
            ):
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

            # Check if prefix is a function call
            # The parser creates IndexedComponent for F(X) which could be
            # a function call rather than array indexing
            if symbol and symbol.kind == SymbolKind.FUNCTION:
                # Collect all overloads and find a matching one
                args = [ActualParameter(span=None, name=None, value=idx) for idx in expr.indices]
                arg_types = [self._analyze_expr(idx) for idx in expr.indices]
                overloads = self.symbols.all_overloads(expr.prefix.name)
                best_match = None
                derived_match = None
                for candidate in overloads:
                    if candidate.kind != SymbolKind.FUNCTION:
                        continue
                    cand_params = candidate.parameters if candidate.parameters else []
                    if len(cand_params) != len(args):
                        continue
                    # Check if argument types are compatible
                    all_match = True
                    all_derived = True
                    for i, param in enumerate(cand_params):
                        if arg_types[i] and param.ada_type:
                            if not types_compatible(param.ada_type, arg_types[i]):
                                all_match = False
                                # Check derived type relationship as fallback
                                if not (is_derived_from(arg_types[i], param.ada_type.name) or
                                        is_derived_from(param.ada_type, arg_types[i].name)):
                                    all_derived = False
                    if all_match:
                        best_match = candidate
                        break
                    elif all_derived and derived_match is None:
                        derived_match = candidate
                if best_match:
                    return best_match.return_type
                if derived_match:
                    return derived_match.return_type
                # No matching overload found - try with first overload anyway
                # (could be an array result being indexed)
                func_params = symbol.parameters if symbol.parameters else []
                if len(func_params) == len(args):
                    self._check_call_arguments(symbol, args, expr)
                    return symbol.return_type

            # Check if prefix is an access-to-function variable (function pointer call)
            if symbol and symbol.kind in (SymbolKind.VARIABLE, SymbolKind.CONSTANT, SymbolKind.PARAMETER):
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

        # Check if prefix is a qualified type name (type conversion via SelectedName)
        # e.g., Ada.Text_IO.Count (I+1)
        if isinstance(expr.prefix, SelectedName):
            # Try to look up the type via its package
            # Get the package part (prefix) and type name (selector)
            prefix_name = self._get_hierarchical_name(expr.prefix.prefix)
            type_name = expr.prefix.selector
            pkg_symbol = self.symbols.lookup(prefix_name)
            if pkg_symbol and pkg_symbol.kind == SymbolKind.PACKAGE:
                # Look for the type in the package's public symbols
                type_key = type_name.lower() if isinstance(type_name, str) else type_name
                if type_key in pkg_symbol.public_symbols:
                    symbol = pkg_symbol.public_symbols[type_key]
                    if symbol.kind in (
                        SymbolKind.TYPE, SymbolKind.SUBTYPE,
                        SymbolKind.TASK_TYPE, SymbolKind.PROTECTED_TYPE,
                    ):
                        # This is a type conversion: Package.Type(Expr)
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

        # Otherwise, it's array indexing
        prefix_type = self._analyze_expr(expr.prefix)

        if prefix_type is None:
            return None

        # Handle implicit dereference: access-to-array types can be indexed directly
        if isinstance(prefix_type, AccessType):
            designated = prefix_type.designated_type
            # If designated type is incomplete/private, try to get the completed type
            if designated and designated.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
                completed = self.symbols.lookup_type(designated.name)
                if completed:
                    designated = completed
            if isinstance(designated, ArrayType):
                prefix_type = designated
            else:
                self.error(
                    f"'{prefix_type.name}' is not an access-to-array type",
                    expr.prefix,
                )
                return None

        # Resolve private types for direct array access
        if prefix_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
            completed = self.symbols.lookup_type(prefix_type.name)
            if completed:
                prefix_type = completed

        if not isinstance(prefix_type, ArrayType):
            self.error(f"'{prefix_type.name}' is not an array", expr.prefix)
            return None

        # Check if this is a slice (single index that's a range)
        # Slices return the same array type, not the component type
        if len(expr.indices) == 1:
            idx = expr.indices[0]
            # Check for RangeExpr, SubtypeIndication, or Range attribute
            is_slice = False
            if isinstance(idx, (RangeExpr, SubtypeIndication)):
                is_slice = True
            elif isinstance(idx, AttributeReference):
                # A'Range is a slice
                if idx.attribute.upper() == 'RANGE':
                    is_slice = True
            if is_slice:
                # This is a slice - analyze the range bounds
                self._analyze_expr(idx)
                # Return the array type (slice of array has same type)
                return prefix_type

        # Check indices
        for idx in expr.indices:
            self._analyze_expr(idx)

        # Resolve private types to their full definitions
        return self._resolve_private_type(prefix_type.component_type)

    def _analyze_selected_name(self, expr: SelectedName, expected_type: Optional[AdaType] = None) -> Optional[AdaType]:
        """Analyze a selected name (record.field, package.item, or pointer.all)."""
        prefix_type = self._analyze_expr(expr.prefix)

        if prefix_type is None:
            # Helper to select best match from overload chain based on expected_type
            def select_from_overloads(symbol: Symbol) -> Optional[AdaType]:
                """Select the best matching symbol from an overload chain."""
                if symbol is None:
                    return None
                # If no expected type, return first (most recent) symbol
                if expected_type is None:
                    return symbol.ada_type
                # Traverse overload chain looking for matching type
                current = symbol
                while current is not None:
                    if current.ada_type and same_type(current.ada_type, expected_type):
                        return current.ada_type
                    current = current.overloaded_next
                # No match found, return first
                return symbol.ada_type

            # Might be a package prefix - handle both simple and hierarchical names
            if isinstance(expr.prefix, Identifier):
                symbol = self.symbols.lookup_selected(
                    expr.prefix.name, expr.selector
                )
                if symbol:
                    return select_from_overloads(symbol)
            elif isinstance(expr.prefix, SelectedName):
                # Handle recursive SelectedName prefix (e.g., Ada.Text_IO.Put)
                # First try to look up the full prefix as a registered package
                full_prefix = self._get_hierarchical_name(expr.prefix)
                prefix_sym = self.symbols.lookup(full_prefix)
                if prefix_sym and prefix_sym.kind == SymbolKind.PACKAGE:
                    selector = expr.selector.lower() if isinstance(expr.selector, str) else expr.selector.lower()
                    if selector in prefix_sym.public_symbols:
                        return select_from_overloads(prefix_sym.public_symbols[selector])
                # Try resolving through the package hierarchy
                prefix_pkg = self._resolve_hierarchical_package(expr.prefix)
                if prefix_pkg and prefix_pkg.kind == SymbolKind.PACKAGE:
                    selector = expr.selector.lower() if isinstance(expr.selector, str) else expr.selector.lower()
                    if selector in prefix_pkg.public_symbols:
                        return select_from_overloads(prefix_pkg.public_symbols[selector])
            return None

        # Access type dereference (Ptr.all)
        if expr.selector.lower() == "all":
            if isinstance(prefix_type, AccessType):
                designated = prefix_type.designated_type
                # If designated type is incomplete/private, try to get the completed type
                if designated and designated.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
                    completed = self.symbols.lookup_type(designated.name)
                    if completed:
                        designated = completed
                return designated
            self.error(
                f"'.all' can only be applied to access types, not '{prefix_type.name}'",
                expr,
            )
            return None

        # Resolve private types for direct record access
        if prefix_type.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
            completed = self.symbols.lookup_type(prefix_type.name)
            if completed:
                prefix_type = completed

        # Record component access
        if isinstance(prefix_type, RecordType):
            comp = prefix_type.get_component(expr.selector)
            if comp is not None:
                return self._resolve_private_type(comp.component_type)

            # For tagged types, check for prefix notation calls (Ada 2005+)
            # X.Method where Method is a primitive of X's type
            if prefix_type.is_tagged:
                prim_type = self._find_prefix_notation_primitive(prefix_type, expr.selector)
                if prim_type is not None:
                    return None if prim_type is self._PROCEDURE_FOUND else prim_type

            self.error(
                f"record '{prefix_type.name}' has no component '{expr.selector}'",
                expr,
            )
            return None

        # Access to record - implicit dereference
        if isinstance(prefix_type, AccessType):
            designated = prefix_type.designated_type
            # If designated type is incomplete/private, try to get the completed type
            if designated and (
                designated.kind in (TypeKind.INCOMPLETE, TypeKind.PRIVATE)
                or not isinstance(designated, RecordType)
            ):
                completed = self.symbols.lookup_type(designated.name)
                if completed and isinstance(completed, RecordType):
                    designated = completed
                elif completed and completed.kind not in (TypeKind.INCOMPLETE, TypeKind.PRIVATE):
                    designated = completed
            if isinstance(designated, RecordType):
                comp = designated.get_component(expr.selector)
                if comp is not None:
                    return self._resolve_private_type(comp.component_type)

                # For tagged types, check for prefix notation calls (Ada 2005+)
                if designated.is_tagged:
                    prim_type = self._find_prefix_notation_primitive(designated, expr.selector)
                    if prim_type is not None:
                        return None if prim_type is self._PROCEDURE_FOUND else prim_type

                self.error(
                    f"record '{designated.name}' has no component '{expr.selector}'",
                    expr,
                )
                return None
            # Even if designated type isn't a RecordType, if it has a base_type
            # chain that leads to a record, follow it (e.g., derived types)
            if designated and hasattr(designated, 'base_type') and designated.base_type:
                base = designated.base_type
                if isinstance(base, RecordType):
                    comp = base.get_component(expr.selector)
                    if comp is not None:
                        return self._resolve_private_type(comp.component_type)
            # For tagged non-record designated types, try prefix notation
            if designated and getattr(designated, 'is_tagged', False):
                prim_type = self._find_prefix_notation_primitive(designated, expr.selector)
                if prim_type is not None:
                    return None if prim_type is self._PROCEDURE_FOUND else prim_type
            # Access to protected type - implicit dereference for operations
            if isinstance(designated, ProtectedType):
                selector_lower = expr.selector.lower() if isinstance(expr.selector, str) else expr.selector.lower()
                for op in designated.operations:
                    if op.name.lower() == selector_lower:
                        if op.kind == "function" and op.return_type:
                            return op.return_type
                        return None
                for entry in designated.entries:
                    if entry.name.lower() == selector_lower:
                        return None
            # Access to task type - implicit dereference for entries
            if isinstance(designated, TaskType):
                selector_lower = expr.selector.lower() if isinstance(expr.selector, str) else expr.selector.lower()
                for entry in designated.entries:
                    if entry.name.lower() == selector_lower:
                        return None

        # Protected type operation access (Counter.Increment, Counter.Value)
        if isinstance(prefix_type, ProtectedType):
            selector_lower = expr.selector.lower() if isinstance(expr.selector, str) else expr.selector.lower()
            # Look up the operation in the protected type
            for op in prefix_type.operations:
                if op.name.lower() == selector_lower:
                    # For functions, return the return type
                    if op.kind == "function" and op.return_type:
                        return op.return_type
                    # For procedures and entries, return None (statement context)
                    return None
            # Also check entries (protected entries are like operations)
            for entry in prefix_type.entries:
                if entry.name.lower() == selector_lower:
                    return None  # Entry calls are statements
            # Check if it's a component access (shouldn't be allowed from outside)
            self.error(
                f"protected type '{prefix_type.name}' has no visible operation '{expr.selector}'",
                expr,
            )
            return None

        # Task type entry access (Task_Obj.Entry_Name)
        if isinstance(prefix_type, TaskType):
            selector_lower = expr.selector.lower() if isinstance(expr.selector, str) else expr.selector.lower()
            for entry in prefix_type.entries:
                if entry.name.lower() == selector_lower:
                    return None  # Entry calls are statements
            # Before rejecting, check if this is an expanded name for a local
            # declaration within the current task body (e.g., L.R where R is a
            # local variable in task L's body)
            prefix_name = None
            if isinstance(expr.prefix, Identifier):
                prefix_name = expr.prefix.name.lower()
            if prefix_name:
                local_sym = self.symbols.lookup(expr.selector)
                if local_sym and local_sym.kind in (
                    SymbolKind.VARIABLE, SymbolKind.CONSTANT,
                    SymbolKind.TYPE, SymbolKind.FUNCTION, SymbolKind.PROCEDURE,
                ):
                    return local_sym.ada_type
            self.error(
                f"task type '{prefix_type.name}' has no entry '{expr.selector}'",
                expr,
            )
            return None

        # For tagged types that aren't records (e.g., tagged private types from
        # generic instantiation), still try prefix notation
        if getattr(prefix_type, 'is_tagged', False):
            prim_type = self._find_prefix_notation_primitive(prefix_type, expr.selector)
            if prim_type is not None:
                return None if prim_type is self._PROCEDURE_FOUND else prim_type

        self.error(f"'{prefix_type.name}' is not a record", expr.prefix)
        return None

    def _analyze_attribute_ref(self, expr: AttributeReference) -> Optional[AdaType]:
        """Analyze an attribute reference."""
        # Analyze prefix and get its type
        # Special handling: prefix might be a type name (e.g., Integer'First)
        prefix_type = None
        if isinstance(expr.prefix, Identifier):
            # Try type lookup first (for Type'Attribute)
            prefix_type = self.symbols.lookup_type(expr.prefix.name)
        elif isinstance(expr.prefix, SelectedName):
            # Handle Package.Type'Attribute
            if isinstance(expr.prefix.prefix, Identifier):
                sym = self.symbols.lookup_selected(expr.prefix.prefix.name, expr.prefix.selector)
                if sym and sym.ada_type:
                    prefix_type = sym.ada_type

        # If not a type, analyze as expression to get object's type
        if prefix_type is None:
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
            # The dimension argument (if present) specifies which dimension (1-based)
            if isinstance(prefix_type, ArrayType) and prefix_type.index_types:
                dim = 0  # Default to first dimension (0-indexed)
                if expr.args:
                    # Get dimension from argument (e.g., A'FIRST(2) for second dimension)
                    dim_val = self._try_eval_static(expr.args[0])
                    if dim_val is not None and isinstance(dim_val, int):
                        dim = dim_val - 1  # Convert to 0-indexed
                    self._analyze_expr(expr.args[0])  # Analyze the argument
                if 0 <= dim < len(prefix_type.index_types):
                    return prefix_type.index_types[dim]
                return prefix_type.index_types[0]  # Fallback to first
            return PREDEFINED_TYPES["Integer"]

        # BASE attribute - returns the base type (Ada RM 3.5(14))
        # T'BASE is the unconstrained base type of T
        if attr_lower == "base":
            if prefix_type:
                # If there's a base_type, return it; otherwise return the type itself
                if hasattr(prefix_type, 'base_type') and prefix_type.base_type:
                    return prefix_type.base_type
                return prefix_type
            return None

        # Integer-valued attributes that return Universal_Integer
        # (implicitly convertible to any integer type per Ada RM)
        if attr_lower in ("length", "size", "pos", "storage_size",
                          "alignment", "width", "count", "component_size",
                          "modulus", "fore", "aft", "max_size_in_storage_elements",
                          "digits", "machine_emax", "machine_emin",
                          "machine_mantissa", "machine_radix",
                          "model_emin", "model_mantissa",
                          "safe_emax", "mantissa",
                          "bit_order", "word_size", "max_alignment_for_allocation"):
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

        # (Modulus already handled above as Universal_Integer)

        # Boolean attributes
        if attr_lower in ("valid", "constrained", "terminated", "callable",
                          "machine_overflows", "machine_rounds", "denorm",
                          "signed_zeros", "has_discriminants", "has_access_values",
                          "has_tagged_values", "definite", "preelaborable_initialization"):
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
                return self._resolve_private_type(prefix_type.component_type)
            return PREDEFINED_TYPES["Integer"]

        # Parallel_Reduce attribute (Ada 2022)
        if attr_lower == "parallel_reduce":
            if len(expr.args) >= 2:
                self._analyze_expr(expr.args[0])
                init_type = self._analyze_expr(expr.args[1])
                if init_type:
                    return init_type
            if isinstance(prefix_type, ArrayType):
                return self._resolve_private_type(prefix_type.component_type)
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

        # Floating-point attributes that return the same type as the prefix
        # Model_Epsilon, Model_Small, Safe_Last, Safe_First, Safe_Large, Epsilon
        # are real-valued attributes per Ada RM
        if attr_lower in ("model_epsilon", "model_small", "safe_last", "safe_first",
                          "safe_large", "epsilon"):
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

        # Fixed-point attributes: per Ada RM 3.5.10, S'Delta and S'Small
        # yield a value of type universal_real (not the fixed-point type)
        if attr_lower in ("small", "delta"):
            return PREDEFINED_TYPES.get("Universal_Real", PREDEFINED_TYPES["Float"])

        # (fore, aft already handled above as Universal_Integer)

        # Default: return Integer for unknown attributes
        return PREDEFINED_TYPES["Integer"]

    def _analyze_function_call(self, expr: FunctionCall) -> Optional[AdaType]:
        """Analyze a function call."""
        if isinstance(expr.name, Identifier):
            func_name = expr.name.name

            # Check if this is a predefined operator call like "+"(RIGHT => X)
            UNARY_OPS = {'+', '-', 'ABS', 'NOT', 'abs', 'not'}
            BINARY_OPS = {'+', '-', '*', '/', 'MOD', 'REM', '**', '&',
                          'AND', 'OR', 'XOR', '=', '/=', '<', '>', '<=', '>=',
                          'mod', 'rem', 'and', 'or', 'xor'}

            op_name = func_name.upper() if func_name else ''

            # Handle unary operator calls with named parameter
            if op_name in {'ABS', 'NOT', '+', '-'} and len(expr.args) == 1:
                # Get the actual argument value
                arg = expr.args[0]
                arg_expr = arg.value if hasattr(arg, 'value') else arg
                arg_type = self._analyze_expr(arg_expr)
                if arg_type is None:
                    return None

                if op_name == 'ABS':
                    if arg_type.kind in (TypeKind.INTEGER, TypeKind.MODULAR,
                                         TypeKind.FLOAT, TypeKind.FIXED,
                                         TypeKind.UNIVERSAL_INTEGER, TypeKind.UNIVERSAL_REAL):
                        return arg_type
                elif op_name in ('+', '-'):
                    if arg_type.is_numeric():
                        return arg_type
                elif op_name == 'NOT':
                    if arg_type.name and arg_type.name.lower() == 'boolean':
                        return arg_type
                    if arg_type.kind == TypeKind.MODULAR:
                        return arg_type
                # Return the type if it's valid
                return arg_type

            # Handle binary operator calls with named/positional parameters
            if op_name in BINARY_OPS and len(expr.args) == 2:
                arg1 = expr.args[0]
                arg2 = expr.args[1]
                left_expr = arg1.value if hasattr(arg1, 'value') else arg1
                right_expr = arg2.value if hasattr(arg2, 'value') else arg2
                left_type = self._analyze_expr(left_expr)
                right_type = self._analyze_expr(right_expr)

                if left_type and right_type:
                    result = common_type(left_type, right_type)
                    if result:
                        return result
                    # For comparison operators, return Boolean
                    if op_name in ('=', '/=', '<', '>', '<=', '>='):
                        return PREDEFINED_TYPES["Boolean"]
                    return left_type
                return left_type or right_type

            # Regular function call - collect all visible overloads
            symbol = self.symbols.lookup(func_name)
            if symbol is None:
                self.error(f"'{func_name}' not found", expr)
                return None
            if symbol.kind != SymbolKind.FUNCTION:
                self.error(f"'{func_name}' is not a function", expr)
                return None

            # Try overload resolution: find the overload whose params match
            best = self._resolve_overload(symbol, expr.args, expr)
            if best is not None:
                symbol = best

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
        except (TypeError, ValueError, AttributeError, KeyError):
            # Expected exceptions for non-static expressions
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
            if sym and sym.is_constant:
                if sym.value is not None:
                    return sym.value
                # Try to evaluate the constant's definition if value wasn't set
                if sym.definition and hasattr(sym.definition, 'value'):
                    val = self._eval_static_impl(sym.definition.value, report_errors=False)
                    if val is not None:
                        sym.value = val  # Cache for future use
                        return val
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
                if sym and sym.is_constant:
                    if sym.value is not None:
                        return sym.value
                    # Try to evaluate the constant's definition if value wasn't set
                    if sym.definition and hasattr(sym.definition, 'value'):
                        val = self._eval_static_impl(sym.definition.value, report_errors=False)
                        if val is not None:
                            sym.value = val  # Cache for future use
                            return val
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

        if isinstance(expr, Aggregate):
            # Aggregate with a single positional component (used in qualified expressions)
            if len(expr.components) == 1:
                comp = expr.components[0]
                if hasattr(comp, 'value') and not comp.choices:
                    return self._eval_static_impl(comp.value, report_errors)
            return None

        if isinstance(expr, FunctionCall):
            # Handle prefix operator notation like "+"(a, b), "-"(a), "ABS"(a)
            func_name = None
            if isinstance(expr.func, StringLiteral):
                func_name = expr.func.value
            elif isinstance(expr.func, Identifier):
                func_name = expr.func.name
            elif isinstance(expr.func, SelectedName):
                # Handle P."+"(a, b)
                func_name = expr.func.selector

            if func_name and func_name.startswith('"') and func_name.endswith('"'):
                op = func_name[1:-1].upper()
                args = [a.value if hasattr(a, 'value') else a for a in (expr.args or [])]

                if len(args) == 1:
                    # Unary operators
                    operand = self._eval_static_impl(args[0], report_errors)
                    if operand is None:
                        return None
                    if op == '+':
                        return operand
                    if op == '-':
                        return -operand
                    if op == 'ABS':
                        return abs(operand)
                    if op == 'NOT':
                        return ~operand
                elif len(args) == 2:
                    # Binary operators
                    left = self._eval_static_impl(args[0], report_errors)
                    right = self._eval_static_impl(args[1], report_errors)
                    if left is None or right is None:
                        return None
                    if op == '+':
                        return left + right
                    if op == '-':
                        return left - right
                    if op == '*':
                        return left * right
                    if op == '/':
                        return left // right if isinstance(left, int) else left / right
                    if op == 'MOD':
                        return left % right
                    if op == 'REM':
                        return left % right
                    if op == '**':
                        return left ** right
            return None

        if isinstance(expr, IndexedComponent):
            # Handle prefix operator notation like P."+"(a, b) parsed as IndexedComponent
            op = None
            if isinstance(expr.prefix, SelectedName):
                op = expr.prefix.selector
            elif isinstance(expr.prefix, Identifier):
                name = expr.prefix.name
                # Parser may strip quotes from operator names
                if name in ('+', '-', '*', '/', 'mod', 'rem', 'abs', 'not', '**',
                            'MOD', 'REM', 'ABS', 'NOT'):
                    op = name

            if op:
                op_upper = op.upper()
                # Get arguments from actual_params or indices
                args = []
                if expr.actual_params:
                    for ap in expr.actual_params:
                        arg = getattr(ap, 'value', ap)
                        args.append(arg)
                elif expr.indices:
                    args = list(expr.indices)

                if len(args) == 1:
                    # Unary operators
                    operand = self._eval_static_impl(args[0], report_errors)
                    if operand is None:
                        return None
                    if op_upper == '+':
                        return operand
                    if op_upper == '-':
                        return -operand
                    if op_upper == 'ABS':
                        return abs(operand)
                    if op_upper == 'NOT':
                        return ~operand
                elif len(args) == 2:
                    # Binary operators
                    left = self._eval_static_impl(args[0], report_errors)
                    right = self._eval_static_impl(args[1], report_errors)
                    if left is None or right is None:
                        return None
                    if op_upper == '+':
                        return left + right
                    if op_upper == '-':
                        return left - right
                    if op_upper == '*':
                        return left * right
                    if op_upper == '/':
                        return left // right if isinstance(left, int) else left / right
                    if op_upper == 'MOD':
                        return left % right
                    if op_upper == 'REM':
                        return left % right
                    if op_upper == '**':
                        return left ** right

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

                # 'Digits for floating-point types
                if attr == "digits" and hasattr(type_obj, "digits"):
                    return type_obj.digits

                # Fixed-point type attributes
                if attr == "delta" and isinstance(type_obj, FixedType):
                    return type_obj.delta
                if attr == "delta" and hasattr(type_obj, "delta_value"):
                    return type_obj.delta_value
                if attr == "fore" and hasattr(type_obj, "kind"):
                    return 2  # Typical default fore value
                if attr == "aft" and hasattr(type_obj, "kind"):
                    return 3  # Typical default aft value
                if attr == "small" and hasattr(type_obj, "kind"):
                    return 1  # Placeholder

                # Machine representation attributes (implementation-defined)
                if attr == "machine_radix":
                    return 2  # Binary machine
                if attr == "machine_mantissa":
                    return 24  # IEEE single precision mantissa bits
                if attr == "machine_emax":
                    return 127  # IEEE single precision max exponent
                if attr == "machine_emin":
                    return -126  # IEEE single precision min exponent

                # 'Width for enumeration types
                if attr == "width":
                    if hasattr(type_obj, "literals") and type_obj.literals:
                        # Width is the maximum length of any literal image
                        return max(len(lit) for lit in type_obj.literals)
                    return 0

            # Handle 'Pos and 'Val for enumeration types
            if attr == "pos" and expr.args:
                # 'Pos(X) returns position of X
                arg = expr.args[0]
                if isinstance(arg, CharacterLiteral):
                    char_val = arg.value
                    # Check if this character is in a user-defined enum type (not Character)
                    if type_obj and hasattr(type_obj, "literals") and type_obj.literals:
                        type_name = getattr(type_obj, 'name', '').lower()
                        # Skip standard Character types - check user-defined enums
                        if type_name not in ('character', 'wide_character', 'wide_wide_character'):
                            for i, lit in enumerate(type_obj.literals):
                                if lit == char_val:
                                    return i
                    # Fall back to ASCII value for Character type
                    return ord(arg.value)
                arg_val = self._eval_static_impl(arg, report_errors)
                return arg_val
            if attr == "val" and expr.args:
                arg_val = self._eval_static_impl(expr.args[0], report_errors)
                return arg_val

            # 'Min and 'Max attribute functions: Type'Min(X, Y)
            if attr == "min" and len(expr.args) == 2:
                left = self._eval_static_impl(expr.args[0], report_errors)
                right = self._eval_static_impl(expr.args[1], report_errors)
                if left is not None and right is not None:
                    return min(left, right)
                return None
            if attr == "max" and len(expr.args) == 2:
                left = self._eval_static_impl(expr.args[0], report_errors)
                right = self._eval_static_impl(expr.args[1], report_errors)
                if left is not None and right is not None:
                    return max(left, right)
                return None

        # Default/fallback
        if report_errors:
            self.error("expression is not static", expr)
        return None


def analyze(program: Program, search_paths: Optional[list[str]] = None) -> SemanticResult:
    """Analyze a program and return the result."""
    analyzer = SemanticAnalyzer(search_paths=search_paths)
    return analyzer.analyze(program)
