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
    RaiseStmt,
    ProcedureCallStmt,
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
    FunctionCall,
    TypeConversion,
    QualifiedExpr,
    Allocator,
    # Type definitions
    TypeDef,
    IntegerTypeDef,
    ModularTypeDef,
    EnumerationTypeDef,
    ArrayTypeDef,
    RecordTypeDef,
    AccessTypeDef,
    DerivedTypeDef,
    SubtypeIndication,
    ComponentDecl,
    GenericInstantiation,
    GenericTypeDecl,
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
        self.in_loop: bool = False  # For exit statement validation

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

    def _analyze_with_clause(self, clause: WithClause) -> None:
        """Analyze a with clause."""
        # For now, just note that packages are imported
        # Full implementation would load package specifications
        pass

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

        # Enter subprogram scope
        self.symbols.enter_scope(spec.name)
        old_subprogram = self.current_subprogram
        self.current_subprogram = subprog_symbol

        # Process parameters
        for param_spec in spec.parameters:
            self._analyze_parameter_spec(param_spec, subprog_symbol)

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

    def _analyze_generic_formal(self, formal) -> None:
        """Analyze a generic formal parameter."""
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
        # TODO: Handle GenericObjectDecl, GenericSubprogramDecl, GenericPackageDecl

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

        if generic_sym.kind != SymbolKind.GENERIC_PACKAGE:
            self.error(f"'{generic_name}' is not a generic package", inst.generic_name)
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

    def _analyze_object_decl(self, decl: ObjectDecl) -> None:
        """Analyze an object (variable/constant) declaration."""
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

    def _analyze_type_decl(self, decl: TypeDecl) -> None:
        """Analyze a type declaration."""
        if self.symbols.is_defined_locally(decl.name):
            self.error(f"type '{decl.name}' is already defined", decl)
            return

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
        )

        # Process parameters to record their types
        for param_spec in decl.parameters:
            param_type = self._resolve_type(param_spec.type_mark)
            for name in param_spec.names:
                param_symbol = Symbol(
                    name=name,
                    kind=SymbolKind.PARAMETER,
                    ada_type=param_type,
                    mode=param_spec.mode,
                )
                symbol.parameters.append(param_symbol)

        self.symbols.define(symbol)

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
        elif isinstance(type_def, DerivedTypeDef):
            return self._build_derived_type(name, type_def)

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

        return parent

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
        elif isinstance(stmt, RaiseStmt):
            self._analyze_raise_stmt(stmt)
        elif isinstance(stmt, ProcedureCallStmt):
            self._analyze_procedure_call(stmt)

    def _analyze_assignment(self, stmt: AssignmentStmt) -> None:
        """Analyze an assignment statement."""
        target_type = self._analyze_expr(stmt.target)
        value_type = self._analyze_expr(stmt.value)

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
            return None  # Type determined by context
        elif isinstance(expr, Allocator):
            return self._analyze_allocator(expr)

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
        # Analyze prefix
        self._analyze_expr(expr.prefix)

        # Most attributes return Integer or the type's range
        attr_lower = expr.attribute.lower()
        if attr_lower in ("first", "last", "length", "size", "pos"):
            return PREDEFINED_TYPES["Integer"]
        if attr_lower == "val":
            # Returns the enumeration type
            prefix_type = self._analyze_expr(expr.prefix)
            return prefix_type
        if attr_lower == "image":
            return PREDEFINED_TYPES["String"]

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
