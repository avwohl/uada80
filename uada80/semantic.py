"""
Semantic analyzer for Ada.

Performs:
- Name resolution (builds symbol table)
- Type checking
- Overload resolution
- Static expression evaluation
- Semantic error reporting
"""

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
    Aggregate,
    DeltaAggregate,
    ContainerAggregate,
    IteratedComponentAssociation,
    ComponentAssociation,
    FunctionCall,
    TypeConversion,
    QualifiedExpr,
    Allocator,
    ConditionalExpr,
    QuantifiedExpr,
    DeclareExpr,
    CaseExpr,
    CaseExprAlternative,
    MembershipTest,
    ExprChoice,
    RangeChoice,
    OthersChoice,
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
    SubtypeIndication,
    ComponentDecl,
    GenericInstantiation,
    GenericTypeDecl,
    # Representation clauses
    RepresentationClause,
    AttributeDefinitionClause,
    RecordRepresentationClause,
    EnumerationRepresentationClause,
    ComponentClause,
)
from uada80.symbol_table import SymbolTable, Symbol, SymbolKind
from uada80.type_system import (
    AdaType,
    TypeKind,
    IntegerType,
    ModularType,
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
    PREDEFINED_TYPES,
    types_compatible,
    common_type,
    can_convert,
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

    def __init__(self) -> None:
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

        For a full implementation, this would:
        1. Load the package specification from the file system
        2. Parse and analyze it
        3. Add its public declarations to the visible scope

        Currently we register a placeholder symbol for the package name
        so qualified references can be resolved during semantic analysis.
        """
        for name in clause.names:
            if isinstance(name, Identifier):
                pkg_name = name.name
                # Check if already defined (e.g., from a previous with)
                existing = self.symbols.lookup(pkg_name)
                if existing is None:
                    # Create a placeholder package symbol
                    # In a full implementation, we would load the actual package
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
                # For now, just register the root package
                if isinstance(name.prefix, Identifier):
                    root_pkg = name.prefix.name
                    existing = self.symbols.lookup(root_pkg)
                    if existing is None:
                        pkg_symbol = Symbol(
                            name=root_pkg,
                            kind=SymbolKind.PACKAGE,
                        )
                        pkg_symbol.is_withed = True
                        if root_pkg.upper() in ("ADA", "SYSTEM", "INTERFACES"):
                            self._setup_standard_package(pkg_symbol, root_pkg.upper())
                        self.symbols.define(pkg_symbol)

    def _setup_standard_package(self, pkg_symbol: Symbol, name: str) -> None:
        """Set up standard library package contents.

        This provides minimal type/subprogram definitions for standard
        packages so that code referencing them can be analyzed.
        """
        if name == "SYSTEM":
            # System package provides Address, Storage_Elements, etc.
            # Add common types
            addr_type = Symbol(name="Address", kind=SymbolKind.TYPE)
            addr_type.ada_type = AdaType(kind=TypeKind.ACCESS, name="Address")
            pkg_symbol.public_symbols["Address"] = addr_type

            storage_type = Symbol(name="Storage_Offset", kind=SymbolKind.TYPE)
            storage_type.ada_type = AdaType(kind=TypeKind.INTEGER, name="Storage_Offset")
            pkg_symbol.public_symbols["Storage_Offset"] = storage_type

        elif name == "INTERFACES":
            # Interfaces package provides C types
            for c_type in ["Integer_8", "Integer_16", "Integer_32",
                          "Unsigned_8", "Unsigned_16", "Unsigned_32"]:
                type_sym = Symbol(name=c_type, kind=SymbolKind.TYPE)
                if "Unsigned" in c_type:
                    type_sym.ada_type = AdaType(kind=TypeKind.MODULAR, name=c_type)
                else:
                    type_sym.ada_type = AdaType(kind=TypeKind.INTEGER, name=c_type)
                pkg_symbol.public_symbols[c_type] = type_sym

    def _analyze_use_clause(self, clause: UseClause) -> None:
        """Analyze a use clause."""
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
        self._check_primitive_operation(subprog_symbol, kind == SymbolKind.FUNCTION,
                                        param_types, return_type)

        # Enter subprogram scope
        self.symbols.enter_scope(spec.name)
        old_subprogram = self.current_subprogram
        self.current_subprogram = subprog_symbol

        # Process parameters
        for param_spec in spec.parameters:
            self._analyze_parameter_spec(param_spec, subprog_symbol)

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
        self, param: ParameterSpec, subprog: Symbol
    ) -> None:
        """Analyze a parameter specification."""
        param_type = self._resolve_type(param.type_mark)

        for name in param.names:
            param_symbol = Symbol(
                name=name,
                kind=SymbolKind.PARAMETER,
                ada_type=param_type,
                mode=param.mode,
            )
            self.symbols.define(param_symbol)
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
        is_generic = bool(pkg.generic_formals)

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

        # Process generic formal parameters first
        for formal in pkg.generic_formals:
            self._analyze_generic_formal(formal)

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

    def _analyze_generic_formal(self, formal) -> None:
        """Analyze a generic formal parameter."""
        from uada80.ast_nodes import GenericObjectDecl, GenericSubprogramDecl

        if isinstance(formal, GenericTypeDecl):
            # Create a placeholder type for the generic type formal
            type_sym = Symbol(
                name=formal.name,
                kind=SymbolKind.TYPE,
            )
            # Mark it as a generic formal type
            type_sym.is_generic_formal = True
            type_sym.ada_type = AdaType(
                kind=TypeKind.PRIVATE,
                name=formal.name,
            )
            self.symbols.define(type_sym)

        elif isinstance(formal, GenericObjectDecl):
            # Generic formal object: X : in Integer := 0
            # Create a variable symbol (VARIABLE is used for both vars and consts)
            obj_sym = Symbol(
                name=formal.name,
                kind=SymbolKind.VARIABLE,
            )
            obj_sym.is_generic_formal = True
            obj_sym.is_constant = (formal.mode == "in")  # "in" mode = read-only

            # Resolve the type reference
            if isinstance(formal.type_ref, Identifier):
                type_sym = self.symbols.lookup(formal.type_ref.name)
                if type_sym and type_sym.ada_type:
                    obj_sym.ada_type = type_sym.ada_type
            self.symbols.define(obj_sym)

        elif hasattr(formal, '__class__') and formal.__class__.__name__ == 'GenericSubprogramDecl':
            # Generic formal subprogram
            # The formal subprogram declares a subprogram name that will be
            # substituted with an actual subprogram at instantiation
            subp_spec = getattr(formal, 'spec', None) or getattr(formal, 'subprogram', None)
            if subp_spec:
                subp_name = getattr(subp_spec, 'name', 'unknown')
                is_function = getattr(subp_spec, 'is_function', False)
                subp_sym = Symbol(
                    name=subp_name,
                    kind=SymbolKind.FUNCTION if is_function else SymbolKind.PROCEDURE,
                )
                subp_sym.is_generic_formal = True
                self.symbols.define(subp_sym)

    def _analyze_generic_instantiation(self, inst: GenericInstantiation) -> None:
        """Analyze a generic instantiation."""
        # Look up the generic
        if isinstance(inst.generic_name, Identifier):
            generic_name = inst.generic_name.name
        else:
            # Handle qualified names like Pkg.Generic_Unit
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

        # Check number of actual parameters
        num_formals = len(generic_decl.generic_formals)
        num_actuals = len(inst.actual_parameters)

        if num_actuals != num_formals:
            self.error(
                f"wrong number of generic parameters for '{generic_name}': "
                f"expected {num_formals}, got {num_actuals}",
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

        # Process parameters to record their types
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

        self.symbols.leave_scope()

    def _analyze_generic_subprogram_instantiation(
        self, inst: GenericInstantiation
    ) -> None:
        """Analyze a generic subprogram instantiation."""
        # Look up the generic
        if isinstance(inst.generic_name, Identifier):
            generic_name = inst.generic_name.name
        else:
            generic_name = str(inst.generic_name)

        generic_sym = self.symbols.lookup(generic_name)

        if generic_sym is None:
            self.error(f"generic '{generic_name}' not found", inst.generic_name)
            return

        if generic_sym.kind not in (SymbolKind.GENERIC_PROCEDURE, SymbolKind.GENERIC_FUNCTION):
            self.error(f"'{generic_name}' is not a generic subprogram", inst.generic_name)
            return

        # Get the generic declaration
        generic_decl = getattr(generic_sym, 'generic_decl', None)
        if generic_decl is None:
            self.error(f"generic '{generic_name}' has no declaration", inst.generic_name)
            return

        # Check number of actual parameters
        num_formals = len(generic_decl.formals)
        num_actuals = len(inst.actual_parameters)

        if num_actuals != num_formals:
            self.error(
                f"wrong number of generic parameters for '{generic_name}': "
                f"expected {num_formals}, got {num_actuals}",
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
        self.symbols.define(inst_symbol)

    def _analyze_package_body(self, body: PackageBody) -> None:
        """Analyze a package body."""
        # Look up the package declaration
        pkg_symbol = self.symbols.lookup(body.name)
        if pkg_symbol is None:
            self.error(f"package specification for '{body.name}' not found")
            return
        if pkg_symbol.kind != SymbolKind.PACKAGE:
            self.error(f"'{body.name}' is not a package")
            return

        # Enter package scope
        self.symbols.enter_scope(body.name)

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
            name = decl.name.lower()
            symbol = self.symbols.lookup_local(name)
            if symbol:
                if is_private:
                    pkg.private_symbols[name] = symbol
                else:
                    pkg.public_symbols[name] = symbol
        elif hasattr(decl, "names"):
            for name in decl.names:
                name_lower = name.lower()
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

    def _analyze_object_decl(self, decl: ObjectDecl) -> None:
        """Analyze an object (variable/constant) declaration."""
        # Handle renaming declarations
        if decl.renames:
            self._analyze_renaming_decl(decl)
            return

        # Resolve type
        obj_type: Optional[AdaType] = None
        if decl.type_mark:
            obj_type = self._resolve_subtype_indication(decl.type_mark)

        # Check initialization expression
        if decl.init_expr:
            init_type = self._analyze_expr(decl.init_expr)
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

        # Constants must have initialization
        if decl.is_constant and not decl.init_expr:
            self.error("constant declaration must have initialization", decl)

        # Create symbols
        for name in decl.names:
            if self.symbols.is_defined_locally(name):
                self.error(f"'{name}' is already defined in this scope", decl)
                continue

            symbol = Symbol(
                name=name,
                kind=SymbolKind.VARIABLE,
                ada_type=obj_type,
                is_constant=decl.is_constant,
                is_aliased=decl.is_aliased,
                definition=decl,
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

        # Check if we're completing an incomplete type
        if existing is not None:
            if (existing.kind == SymbolKind.TYPE and
                existing.ada_type and
                existing.ada_type.kind == TypeKind.INCOMPLETE):
                # This is completing an incomplete type - update the existing symbol
                ada_type = self._build_type(decl.name, decl.type_def)
                existing.ada_type = ada_type
                existing.definition = decl
                # Fall through to handle enum literals if applicable
            else:
                self.error(f"type '{decl.name}' is already defined", decl)
                return
        else:
            # Build the type
            ada_type = self._build_type(decl.name, decl.type_def)

            symbol = Symbol(
                name=decl.name,
                kind=SymbolKind.TYPE,
                ada_type=ada_type,
                definition=decl,
            )
            self.symbols.define(symbol)

        # For enumeration types, add literals to symbol table
        if isinstance(ada_type, EnumerationType):
            for literal in ada_type.literals:
                # Check if literal already exists (could be from another enum)
                if self.symbols.is_defined_locally(literal):
                    self.error(
                        f"enumeration literal '{literal}' conflicts with existing declaration",
                        decl,
                    )
                    continue

                literal_symbol = Symbol(
                    name=literal,
                    kind=SymbolKind.VARIABLE,
                    ada_type=ada_type,
                    is_constant=True,
                    definition=decl,
                )
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
        """Analyze an attribute definition clause (Type'Size use N)."""
        # Get the type being modified
        type_name = ""
        if isinstance(decl.name, Identifier):
            type_name = decl.name.name
        elif hasattr(decl.name, "name"):
            type_name = decl.name.name

        sym = self.symbols.lookup(type_name)
        if sym is None:
            self.error(f"unknown type '{type_name}'", decl)
            return

        if sym.kind != SymbolKind.TYPE:
            self.error(f"'{type_name}' is not a type", decl)
            return

        # Evaluate the value expression
        value = self._eval_static_expr(decl.value)

        # Apply the attribute
        attr = decl.attribute.lower()
        if attr == "size":
            if sym.ada_type:
                sym.ada_type.size_bits = value
        elif attr == "alignment":
            if sym.ada_type:
                # Store alignment (would need to add field to AdaType)
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
        if self.symbols.is_defined_locally(decl.name):
            self.error(f"task type '{decl.name}' is already defined", decl)
            return

        # Build entry information
        entries = []
        for entry_decl in decl.entries:
            param_types = []
            for param in entry_decl.parameters:
                param_type = self._resolve_type(param.type_mark)
                if param_type:
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
        if self.symbols.is_defined_locally(decl.name):
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
                    if param_type:
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

        # Analyze each item in the body
        for item in body.items:
            if isinstance(item, SubprogramBody):
                self._analyze_subprogram_body(item)
            else:
                self._analyze_declaration(item)

        self.symbols.leave_scope()

    # =========================================================================
    # Type Building
    # =========================================================================

    def _build_type(self, name: str, type_def: Optional[TypeDef]) -> Optional[AdaType]:
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
            return self._build_record_type(name, type_def)
        elif isinstance(type_def, AccessTypeDef):
            return self._build_access_type(name, type_def)
        elif isinstance(type_def, AccessSubprogramTypeDef):
            return self._build_access_subprogram_type(name, type_def)
        elif isinstance(type_def, DerivedTypeDef):
            return self._build_derived_type(name, type_def)
        elif isinstance(type_def, InterfaceTypeDef):
            return self._build_interface_type(name, type_def)

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
                # Constrained with explicit range
                low = self._eval_static_expr(idx_subtype.low)
                high = self._eval_static_expr(idx_subtype.high)
                bounds.append((low, high))
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
        self, name: str, type_def: RecordTypeDef
    ) -> RecordType:
        """Build a record type."""
        components: list[RecordComponent] = []

        for comp_decl in type_def.components:
            comp_type = self._resolve_type(comp_decl.type_mark)
            for comp_name in comp_decl.names:
                components.append(
                    RecordComponent(name=comp_name, component_type=comp_type)
                )

        return RecordType(name=name, size_bits=0, components=components)

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
            )

        # Handle tagged type derivation with record extension and interfaces
        if isinstance(parent, RecordType) and parent.is_tagged:
            # Build components from record extension
            components: list[RecordComponent] = []
            if type_def.record_extension:
                for comp_decl in type_def.record_extension.components:
                    comp_type = self._resolve_type(comp_decl.type_mark)
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

            return RecordType(
                name=name,
                is_tagged=True,
                parent_type=parent,
                components=components,
                interfaces=interfaces,
                is_controlled=is_controlled,
                is_limited_controlled=is_limited_controlled,
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

    # =========================================================================
    # Type Resolution
    # =========================================================================

    def _resolve_type(self, type_expr: Expr) -> Optional[AdaType]:
        """Resolve a type expression to an AdaType."""
        if isinstance(type_expr, Identifier):
            return self.symbols.lookup_type(type_expr.name)
        elif isinstance(type_expr, SelectedName):
            # Package.Type
            prefix_name = self._get_identifier_name(type_expr.prefix)
            if prefix_name:
                symbol = self.symbols.lookup_selected(
                    prefix_name, type_expr.selector
                )
                if symbol and symbol.ada_type:
                    return symbol.ada_type
        return None

    def _resolve_subtype_indication(
        self, subtype_ind: SubtypeIndication
    ) -> Optional[AdaType]:
        """Resolve a subtype indication."""
        return self._resolve_type(subtype_ind.type_mark)

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
        value_type = self._analyze_expr(stmt.value)
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

                # Define loop variable
                loop_var = Symbol(
                    name=iterator.name,
                    kind=SymbolKind.VARIABLE,
                    ada_type=iter_type if iter_type else PREDEFINED_TYPES["Integer"],
                    is_constant=True,  # Loop variable is implicitly constant
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

        if self.current_subprogram.kind == SymbolKind.FUNCTION:
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

        if self.current_subprogram.kind != SymbolKind.FUNCTION:
            self.error("extended return statement only allowed in functions", stmt)
            return

        # Enter a new scope for the return object
        self.symbols.enter_scope("extended_return")

        # Resolve the return type
        return_type: Optional[AdaType] = None
        if stmt.type_mark:
            return_type = self._resolve_type(stmt.type_mark)
        elif self.current_subprogram.return_type:
            return_type = self.current_subprogram.return_type

        # Define the return object
        if return_type:
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
            if symbol.kind not in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION):
                self.error(f"'{stmt.name.name}' is not a procedure", stmt)
                return

            # Check arguments
            self._check_call_arguments(symbol, stmt.args, stmt)

    def _check_call_arguments(
        self, subprog: Symbol, args: list, node: ASTNode
    ) -> None:
        """Check that call arguments match parameters."""
        if len(args) != len(subprog.parameters):
            self.error(
                f"wrong number of arguments: expected {len(subprog.parameters)}, "
                f"got {len(args)}",
                node,
            )
            return

        for arg, param in zip(args, subprog.parameters):
            if arg.value:
                arg_type = self._analyze_expr(arg.value)
                if arg_type and param.ada_type:
                    if not types_compatible(param.ada_type, arg_type):
                        self.error(
                            f"type mismatch for parameter '{param.name}': "
                            f"expected '{param.ada_type.name}', got '{arg_type.name}'",
                            arg.value,
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

    def _analyze_expr(self, expr: Expr) -> Optional[AdaType]:
        """Analyze an expression and return its type."""
        if isinstance(expr, Identifier):
            return self._analyze_identifier(expr)
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
        elif isinstance(expr, Aggregate):
            return self._analyze_aggregate(expr)
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
                element_type = iter_type.element_type

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

    def _analyze_aggregate(self, expr: Aggregate) -> Optional[AdaType]:
        """Analyze an aggregate expression."""
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
        if selector_type and not isinstance(
            selector_type, (IntegerType, EnumerationType, ModularType)
        ):
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
        return ArrayType(
            name=f"{prefix_type.name}_slice",
            kind=prefix_type.kind,
            size_bits=0,  # Size depends on range
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

        # Return the designated type
        return prefix_type.designated_type

    def _analyze_identifier(self, expr: Identifier) -> Optional[AdaType]:
        """Analyze an identifier expression."""
        symbol = self.symbols.lookup(expr.name)
        if symbol is None:
            self.error(f"'{expr.name}' not found", expr)
            return None

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
                else:
                    self.error(
                        f"expected modular type, got '{right_type.name if right_type else 'unknown'}'",
                        expr.right,
                    )
                    return left_type
            # For Boolean, these are logical operators
            self._check_boolean(left_type, expr.left)
            self._check_boolean(right_type, expr.right)
            return PREDEFINED_TYPES["Boolean"]

        # Arithmetic operators
        if expr.op in (
            BinaryOp.ADD,
            BinaryOp.SUB,
            BinaryOp.MUL,
            BinaryOp.DIV,
            BinaryOp.MOD,
            BinaryOp.REM,
            BinaryOp.EXP,
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
            return PREDEFINED_TYPES["String"]

        return left_type

    def _analyze_unary_expr(self, expr: UnaryExpr) -> Optional[AdaType]:
        """Analyze a unary expression."""
        operand_type = self._analyze_expr(expr.operand)

        if expr.op == UnaryOp.NOT:
            # For modular types, NOT is bitwise complement
            if operand_type and operand_type.kind == TypeKind.MODULAR:
                return operand_type
            # For Boolean, NOT is logical negation
            self._check_boolean(operand_type, expr.operand)
            return PREDEFINED_TYPES["Boolean"]

        if expr.op in (UnaryOp.PLUS, UnaryOp.MINUS, UnaryOp.ABS):
            if operand_type and not operand_type.is_numeric():
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
        """Analyze an indexed component (array access)."""
        prefix_type = self._analyze_expr(expr.prefix)

        if prefix_type is None:
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
            # Might be a package prefix
            if isinstance(expr.prefix, Identifier):
                symbol = self.symbols.lookup_selected(
                    expr.prefix.name, expr.selector
                )
                if symbol:
                    return symbol.ada_type
            return None

        # Access type dereference (Ptr.all)
        if expr.selector.lower() == "all":
            if isinstance(prefix_type, AccessType):
                return prefix_type.designated_type
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

        # Handle attributes based on their name
        attr_lower = expr.attribute.lower()

        # Integer-valued attributes
        if attr_lower in ("first", "last", "length", "size", "pos", "min", "max"):
            return PREDEFINED_TYPES["Integer"]

        # Val returns the enumeration type
        if attr_lower == "val":
            return prefix_type

        # Image returns String
        if attr_lower == "image":
            return PREDEFINED_TYPES["String"]

        # Value returns the type (inverse of Image)
        if attr_lower == "value":
            return prefix_type

        # Address returns an access type (System.Address)
        if attr_lower == "address":
            return AccessType(
                name="System.Address",
                kind=TypeKind.ACCESS,
                size_bits=16,  # Z80 has 16-bit addresses
                designated_type=prefix_type if prefix_type else PREDEFINED_TYPES["Integer"],
            )

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
        if attr_lower in ("valid", "constrained"):
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
                if self.current_subprogram.kind == SymbolKind.FUNCTION:
                    return self.current_subprogram.return_type
            self.error("'Result can only be used in function postconditions", expr)
            return None

        # 'Update attribute (Ada 2012 AI12-0001) - for record/array update
        if attr_lower == "update":
            # Returns same type as prefix
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

    def _eval_static_expr(self, expr: Expr) -> int:
        """Evaluate a static expression to an integer value."""
        if isinstance(expr, IntegerLiteral):
            return expr.value

        if isinstance(expr, UnaryExpr):
            operand = self._eval_static_expr(expr.operand)
            if expr.op == UnaryOp.MINUS:
                return -operand
            if expr.op == UnaryOp.PLUS:
                return operand
            if expr.op == UnaryOp.ABS:
                return abs(operand)

        if isinstance(expr, BinaryExpr):
            left = self._eval_static_expr(expr.left)
            right = self._eval_static_expr(expr.right)
            if expr.op == BinaryOp.ADD:
                return left + right
            if expr.op == BinaryOp.SUB:
                return left - right
            if expr.op == BinaryOp.MUL:
                return left * right
            if expr.op == BinaryOp.DIV:
                return left // right if right != 0 else 0
            if expr.op == BinaryOp.EXP:
                return left**right

        if isinstance(expr, AttributeReference):
            if isinstance(expr.prefix, Identifier):
                type_obj = self.symbols.lookup_type(expr.prefix.name)
                if type_obj:
                    attr = expr.attribute.lower()
                    if attr == "first" and hasattr(type_obj, "low"):
                        return type_obj.low
                    if attr == "last" and hasattr(type_obj, "high"):
                        return type_obj.high

        # Default/fallback
        self.error("expression is not static", expr)
        return 0


def analyze(program: Program) -> SemanticResult:
    """Analyze a program and return the result."""
    analyzer = SemanticAnalyzer()
    return analyzer.analyze(program)
