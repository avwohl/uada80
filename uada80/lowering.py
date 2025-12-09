"""
AST to IR Lowering.

Translates type-checked AST to IR for code generation.
"""

from dataclasses import dataclass, field
from typing import Any, Optional

from uada80.ast_nodes import (
    Program,
    CompilationUnit,
    SubprogramBody,
    SubprogramDecl,
    PackageDecl,
    PackageBody,
    ObjectDecl,
    NumberDecl,
    ExceptionDecl,
    RenamingDecl,
    TypeDecl,
    SubtypeDecl,
    ParameterSpec,
    GenericInstantiation,
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
    LabeledStmt,
    GotoStmt,
    ProcedureCallStmt,
    ExceptionHandler,
    # Tasking statements
    AcceptStmt,
    SelectStmt,
    DelayStmt,
    AbortStmt,
    RequeueStmt,
    ParallelBlockStmt,
    PragmaStmt,
    ExtendedReturnStmt,
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
    ComponentAssociation,
    ExprChoice,
    RangeChoice,
    OthersChoice,
    Slice,
    FunctionCall,
    TypeConversion,
    QualifiedExpr,
    Allocator,
    Dereference,
    ConditionalExpr,
    QuantifiedExpr,
    MembershipTest,
    CaseExpr,
    RaiseExpr,
    DeclareExpr,
    DeltaAggregate,
    ContainerAggregate,
    Parenthesized,
    TargetName,
    ProtectedTypeDecl,
    ProtectedBody,
)
from uada80.ir import (
    IRType,
    IRModule,
    IRFunction,
    BasicBlock,
    IRBuilder,
    VReg,
    Immediate,
    Label,
    MemoryLocation,
    OpCode,
    IRInstr,
    ir_type_from_bits,
)
from uada80.symbol_table import SymbolTable, Symbol, SymbolKind
from uada80.type_system import (
    AdaType,
    TypeKind,
    PREDEFINED_TYPES,
    AccessType,
    RecordType,
    ArrayType,
    EnumerationType,
    ProtectedType,
)
from uada80.semantic import SemanticResult


@dataclass
class LocalVariable:
    """Information about a local variable."""

    name: str
    vreg: VReg
    stack_offset: int
    size: int
    ada_type: Optional["AdaType"] = None


@dataclass
class LoweringContext:
    """Context for lowering a function."""

    function: IRFunction
    locals: dict[str, LocalVariable] = field(default_factory=dict)
    params: dict[str, VReg] = field(default_factory=dict)
    loop_exit_label: Optional[str] = None  # For exit statements (innermost loop)
    loop_continue_label: Optional[str] = None  # For continue
    # Named loop labels: maps loop label (lowercase) -> exit label
    loop_exit_labels: dict[str, str] = field(default_factory=dict)
    # Exception handling: stack of (handler_count, exit_label) for nested handlers
    exception_handler_stack: list[tuple[int, str]] = field(default_factory=list)
    # Current assignment target for @ (TargetName) - the LHS of enclosing assignment
    assignment_target: Optional[Any] = None  # AST node (Identifier, IndexedComponent, etc.)
    # Named numbers (compile-time constants) in scope
    named_numbers: dict[str, int] = field(default_factory=dict)


class ASTLowering:
    """Lowers AST to IR."""

    # Fixed IDs for predefined exceptions (must match runtime)
    PREDEFINED_EXCEPTIONS = {
        "constraint_error": 1,
        "program_error": 2,
        "storage_error": 3,
        "tasking_error": 4,
        "assertion_error": 5,
    }

    def __init__(self, symbols: SymbolTable) -> None:
        self.symbols = symbols
        self.builder = IRBuilder()
        self.ctx: Optional[LoweringContext] = None
        self._label_counter = 0
        # For generic instantiation: type mappings and instance prefix
        self._generic_type_map: dict[str, str] = {}
        self._generic_prefix: Optional[str] = None
        # Exception handling: map exception names to IDs
        # Initialize with predefined exceptions
        self._exception_ids: dict[str, int] = dict(self.PREDEFINED_EXCEPTIONS)
        self._next_exception_id = 5  # Start user exceptions after predefined

    def lower(self, program: Program) -> IRModule:
        """Lower an entire program to IR."""
        module = self.builder.new_module("main")

        for unit in program.units:
            self._lower_compilation_unit(unit)

        # Generate vtables for tagged types
        self._generate_vtables()

        return module

    def _generate_vtables(self) -> None:
        """Generate vtables for all tagged types in the program."""
        # Collect all tagged types from the symbol table
        tagged_types: list[RecordType] = []

        def collect_tagged(scope):
            for sym in scope.symbols.values():
                if sym.kind == SymbolKind.TYPE:
                    if isinstance(sym.ada_type, RecordType) and sym.ada_type.is_tagged:
                        if not sym.ada_type.is_class_wide:
                            tagged_types.append(sym.ada_type)

        # Walk all scopes
        scope = self.symbols.current_scope
        while scope:
            collect_tagged(scope)
            scope = scope.parent

        # Generate a vtable for each tagged type
        for tagged_type in tagged_types:
            self._generate_vtable(tagged_type)

    def _generate_vtable(self, tagged_type: RecordType) -> None:
        """Generate a vtable for a tagged type."""
        vtable_name = f"_vtable_{tagged_type.name}"
        primitives = tagged_type.all_primitives()

        if not primitives:
            return

        # Add vtable as a global with procedure addresses
        # Each entry is 2 bytes (16-bit address on Z80)
        vtable_data: list[str] = []
        for op in primitives:
            # The procedure name is just the operation name
            # In a full implementation, we'd need the mangled name
            vtable_data.append(op.name)

        # Store vtable info in the module for codegen
        self.builder.module.vtables[vtable_name] = vtable_data

    def _new_label(self, prefix: str = "L") -> str:
        """Generate a unique label."""
        name = f"{prefix}{self._label_counter}"
        self._label_counter += 1
        return name

    def _emit_constraint_check(
        self, value, low: int, high: int, comment: str = ""
    ) -> None:
        """Emit runtime constraint check that raises Constraint_Error if out of bounds.

        value: the value to check (vreg or immediate)
        low: minimum allowed value
        high: maximum allowed value
        """
        if self.ctx is None:
            return

        # Generate labels
        ok_label = self._new_label("check_ok")

        # Check value >= low
        cond1 = self.builder.new_vreg(IRType.BOOL, "_check_lo")
        self.builder.cmp_ge(cond1, value, Immediate(low, IRType.WORD))
        self.builder.jz(cond1, Label("_raise_constraint_error"))

        # Check value <= high
        cond2 = self.builder.new_vreg(IRType.BOOL, "_check_hi")
        self.builder.cmp_le(cond2, value, Immediate(high, IRType.WORD))
        self.builder.jz(cond2, Label("_raise_constraint_error"))

        # Continue to ok label
        ok_block = self.builder.new_block(ok_label)
        self.builder.set_block(ok_block)

    def _emit_array_bounds_check(
        self, index, low: int, high: int, comment: str = ""
    ) -> None:
        """Emit runtime array bounds check."""
        self._emit_constraint_check(index, low, high, comment)

    def _emit_succ_check(self, value, last: int) -> None:
        """Emit check that value < last (can compute successor)."""
        if self.ctx is None:
            return

        ok_label = self._new_label("succ_ok")
        cond = self.builder.new_vreg(IRType.BOOL, "_succ_chk")
        self.builder.cmp_lt(cond, value, Immediate(last, IRType.WORD))
        self.builder.jz(cond, Label("_raise_constraint_error"))

        ok_block = self.builder.new_block(ok_label)
        self.builder.set_block(ok_block)

    def _emit_pred_check(self, value, first: int) -> None:
        """Emit check that value > first (can compute predecessor)."""
        if self.ctx is None:
            return

        ok_label = self._new_label("pred_ok")
        cond = self.builder.new_vreg(IRType.BOOL, "_pred_chk")
        self.builder.cmp_gt(cond, value, Immediate(first, IRType.WORD))
        self.builder.jz(cond, Label("_raise_constraint_error"))

        ok_block = self.builder.new_block(ok_label)
        self.builder.set_block(ok_block)

    def _ada_type_to_ir(self, ada_type: Optional[AdaType]) -> IRType:
        """Convert Ada type to IR type."""
        if ada_type is None:
            return IRType.WORD

        if ada_type.kind == TypeKind.ENUMERATION:
            if ada_type.name == "Boolean":
                return IRType.BOOL
            return IRType.BYTE if ada_type.size_bits <= 8 else IRType.WORD

        if ada_type.kind in (TypeKind.INTEGER, TypeKind.MODULAR):
            return ir_type_from_bits(ada_type.size_bits)

        if ada_type.kind == TypeKind.ACCESS:
            return IRType.PTR

        # Default to WORD for composite types
        return IRType.WORD

    def _get_exception_id(self, name: str) -> int:
        """Get or create an exception ID for the given exception name."""
        name_lower = name.lower()
        if name_lower not in self._exception_ids:
            self._exception_ids[name_lower] = self._next_exception_id
            self._next_exception_id += 1
        return self._exception_ids[name_lower]

    # =========================================================================
    # Compilation Units
    # =========================================================================

    def _lower_compilation_unit(self, unit: CompilationUnit) -> None:
        """Lower a compilation unit."""
        if isinstance(unit.unit, SubprogramBody):
            self._lower_subprogram_body(unit.unit)
        elif isinstance(unit.unit, PackageDecl):
            self._lower_package_decl(unit.unit)
        elif isinstance(unit.unit, PackageBody):
            self._lower_package_body(unit.unit)
        elif isinstance(unit.unit, GenericInstantiation):
            self._lower_generic_instantiation(unit.unit)

    def _lower_subprogram_body(self, body: SubprogramBody) -> None:
        """Lower a subprogram body."""
        spec = body.spec

        # Check if this subprogram is imported (pragma Import)
        # Imported subprograms don't have bodies - they're external
        sym = self.symbols.lookup(spec.name)
        if sym and sym.is_imported:
            # No body to generate for imported subprograms
            return

        # Determine return type
        return_type = IRType.VOID
        if spec.is_function and spec.return_type:
            if sym and sym.return_type:
                return_type = self._ada_type_to_ir(sym.return_type)

        # Create function
        func = self.builder.new_function(spec.name, return_type)

        # Create context
        self.ctx = LoweringContext(function=func)

        # Process parameters
        for param_spec in spec.parameters:
            self._lower_parameter(param_spec)

        # Calculate local variable sizes
        stack_offset = 0
        for decl in body.declarations:
            if isinstance(decl, ObjectDecl):
                for name in decl.names:
                    size = 2  # Default to word size
                    vreg = self.builder.new_vreg(IRType.WORD, name)
                    self.ctx.locals[name.lower()] = LocalVariable(
                        name=name,
                        vreg=vreg,
                        stack_offset=stack_offset,
                        size=size,
                    )
                    stack_offset += size

        func.locals_size = stack_offset

        # Create entry block
        entry = self.builder.new_block(f"{spec.name}_entry")
        self.builder.set_block(entry)

        # Generate precondition checks
        self._generate_preconditions(spec)

        # Process declarations (initializations)
        for decl in body.declarations:
            self._lower_declaration(decl)

        # Process statements (with exception handlers if present)
        if body.handled_exception_handlers:
            self._lower_block_with_handlers(
                body.statements, body.handled_exception_handlers
            )
        else:
            for stmt in body.statements:
                self._lower_statement(stmt)

        # Generate postcondition checks before returns
        # Note: In a full implementation, postconditions would be checked
        # before each return statement. For simplicity, we check at the end.
        self._generate_postconditions(spec)

        # Add implicit return if needed
        if not self._block_has_return(self.builder.block):
            if return_type == IRType.VOID:
                self.builder.ret()

        self.ctx = None

    def _generate_preconditions(self, spec: SubprogramDecl) -> None:
        """Generate precondition checks from Pre aspect."""
        for aspect in spec.aspects:
            if aspect.name.lower() == "pre" and aspect.value:
                # Evaluate the precondition expression
                cond_value = self._lower_expr(aspect.value)
                # If false, raise Assertion_Error
                fail_label = self._new_label("pre_fail")
                ok_label = self._new_label("pre_ok")
                self.builder.jnz(cond_value, Label(ok_label))
                # Pre failed - raise Assertion_Error
                fail_block = self.builder.new_block(fail_label)
                self.builder.set_block(fail_block)
                self.builder.emit(IRInstr(OpCode.EXC_RAISE,
                                          src1=Immediate(5, IRType.WORD),  # Assertion_Error ID
                                          comment="Pre condition failed"))
                # Continue on success
                ok_block = self.builder.new_block(ok_label)
                self.builder.set_block(ok_block)

    def _generate_postconditions(self, spec: SubprogramDecl) -> None:
        """Generate postcondition checks from Post aspect."""
        for aspect in spec.aspects:
            if aspect.name.lower() == "post" and aspect.value:
                # Evaluate the postcondition expression
                cond_value = self._lower_expr(aspect.value)
                # If false, raise Assertion_Error
                fail_label = self._new_label("post_fail")
                ok_label = self._new_label("post_ok")
                self.builder.jnz(cond_value, Label(ok_label))
                # Post failed - raise Assertion_Error
                fail_block = self.builder.new_block(fail_label)
                self.builder.set_block(fail_block)
                self.builder.emit(IRInstr(OpCode.EXC_RAISE,
                                          src1=Immediate(5, IRType.WORD),  # Assertion_Error ID
                                          comment="Post condition failed"))
                # Continue on success
                ok_block = self.builder.new_block(ok_label)
                self.builder.set_block(ok_block)

    def _lower_parameter(self, param: ParameterSpec) -> None:
        """Lower a parameter specification."""
        if self.ctx is None:
            return

        for name in param.names:
            vreg = self.builder.new_vreg(IRType.WORD, name)
            self.ctx.params[name.lower()] = vreg
            self.ctx.function.params.append(vreg)

    def _lower_package_decl(self, pkg: PackageDecl) -> None:
        """Lower a package declaration."""
        # Skip generic packages - they are templates, not concrete code
        if pkg.generic_formals:
            return

        pkg_prefix = pkg.name + "." if pkg.name else ""

        # Process public declarations
        for decl in pkg.declarations:
            if isinstance(decl, SubprogramBody):
                self._lower_subprogram_body(decl)
            elif isinstance(decl, ObjectDecl):
                self._lower_package_object_decl(decl, pkg_prefix)
            elif isinstance(decl, PackageDecl):
                # Nested package
                self._lower_package_decl(decl)

        # Process private declarations
        for decl in pkg.private_declarations:
            if isinstance(decl, SubprogramBody):
                self._lower_subprogram_body(decl)
            elif isinstance(decl, ObjectDecl):
                self._lower_package_object_decl(decl, pkg_prefix)
            elif isinstance(decl, PackageDecl):
                self._lower_package_decl(decl)

    def _lower_package_body(self, body: PackageBody) -> None:
        """Lower a package body."""
        pkg_prefix = body.name + "." if body.name else ""

        # First pass: process declarations (variables become globals, subprograms get lowered)
        for decl in body.declarations:
            if isinstance(decl, SubprogramBody):
                self._lower_subprogram_body(decl)
            elif isinstance(decl, ObjectDecl):
                self._lower_package_object_decl(decl, pkg_prefix)
            elif isinstance(decl, PackageBody):
                # Nested package body
                self._lower_package_body(decl)

        # Generate package initialization function if there are init statements
        if body.statements:
            self._lower_package_init(body)

    def _lower_package_object_decl(self, decl: ObjectDecl, prefix: str = "") -> None:
        """Lower a package-level object declaration as a global variable."""
        if self.builder.module is None:
            return

        for name in decl.names:
            global_name = f"{prefix}{name}".replace(".", "_")

            # Determine size from type
            size = 2  # Default to word
            if decl.type_expr:
                ada_type = self._resolve_type(decl.type_expr)
                if ada_type:
                    size = (ada_type.size_bits + 7) // 8

            # Add as global variable
            self.builder.module.add_global(global_name, IRType.WORD, size)

            # If there's an initializer, we need to handle it in package init
            # For now, store it for later processing
            if decl.init_expr:
                if not hasattr(self, '_pending_pkg_inits'):
                    self._pending_pkg_inits = []
                self._pending_pkg_inits.append((global_name, decl.init_expr))

    def _lower_package_init(self, body: PackageBody) -> None:
        """Generate initialization function for a package body."""
        init_func_name = f"_{body.name}_init".replace(".", "_")

        # Create init function
        func = self.builder.new_function(init_func_name, IRType.VOID)
        entry = self.builder.new_block(f"{init_func_name}_entry")
        self.builder.set_block(entry)

        # Create a temporary context for lowering statements
        self.ctx = LoweringContext(function=func)

        # Process pending package-level variable initializations
        if hasattr(self, '_pending_pkg_inits') and self._pending_pkg_inits:
            for global_name, init_expr in self._pending_pkg_inits:
                # Evaluate the initializer
                value = self._lower_expr(init_expr)
                # Store to global variable
                global_mem = MemoryLocation(
                    is_global=True,
                    symbol_name=global_name,
                    ir_type=IRType.WORD
                )
                self.builder.emit(IRInstr(
                    OpCode.STORE, global_mem, value,
                    comment=f"init {global_name}"
                ))
            # Clear the list after processing
            self._pending_pkg_inits = []

        # Lower initialization statements
        for stmt in body.statements:
            self._lower_statement(stmt)

        # Add return
        self.builder.ret()
        self.ctx = None

    def _lower_generic_instantiation(self, inst: GenericInstantiation) -> None:
        """Lower a generic instantiation.

        This creates specialized code for the generic package/subprogram with
        actual type parameters substituted for formal parameters.
        """
        # Look up the generic unit
        generic_name = (
            inst.generic_name.name
            if hasattr(inst.generic_name, "name")
            else str(inst.generic_name)
        )
        generic_sym = self.symbols.lookup(generic_name)

        if inst.kind == "package":
            self._lower_generic_package_instantiation(inst, generic_sym)
        elif inst.kind in ("procedure", "function"):
            self._lower_generic_subprogram_instantiation(inst, generic_sym)

    def _lower_generic_package_instantiation(self, inst: GenericInstantiation,
                                              generic_sym: Optional[Symbol]) -> None:
        """Lower a generic package instantiation."""
        if generic_sym is None or generic_sym.kind != SymbolKind.GENERIC_PACKAGE:
            return

        # Get the generic package's AST definition
        generic_pkg = generic_sym.definition
        if not isinstance(generic_pkg, PackageDecl):
            return

        # Build type mapping from formal to actual parameters
        type_map = self._build_generic_type_map(generic_pkg.generic_formals,
                                                 inst.actual_parameters)

        # Store the type map for later use during expression lowering
        self._generic_type_map = type_map
        self._generic_prefix = inst.name

        # Lower each subprogram in the generic package
        for decl in generic_pkg.declarations:
            if isinstance(decl, SubprogramBody):
                # Create prefixed name for instantiated subprogram
                original_name = decl.spec.name
                decl.spec.name = f"{inst.name}.{original_name}"
                self._lower_subprogram_body(decl)
                decl.spec.name = original_name  # Restore original name

        # Clear the type map
        self._generic_type_map = {}
        self._generic_prefix = None

    def _lower_generic_subprogram_instantiation(self, inst: GenericInstantiation,
                                                  generic_sym: Optional[Symbol]) -> None:
        """Lower a generic procedure or function instantiation."""
        if generic_sym is None:
            return

        if generic_sym.kind not in (SymbolKind.GENERIC_PROCEDURE, SymbolKind.GENERIC_FUNCTION):
            return

        # Get the generic subprogram's AST definition
        generic_decl = getattr(generic_sym, 'generic_decl', None)
        if generic_decl is None:
            return

        # Handle GenericSubprogramUnit
        from uada80.ast_nodes import GenericSubprogramUnit
        if isinstance(generic_decl, GenericSubprogramUnit):
            formals = generic_decl.formals
            subprogram = generic_decl.subprogram
        else:
            # Legacy format
            formals = getattr(generic_decl, "generic_formals", [])
            subprogram = generic_decl

        # Build type mapping from formal to actual parameters
        type_map: dict[str, str] = {}
        if formals:
            type_map = self._build_generic_type_map(formals, inst.actual_parameters)

        # Store the type map for later use during expression lowering
        self._generic_type_map = type_map
        self._generic_prefix = inst.name

        # Get the body to lower
        if isinstance(subprogram, SubprogramBody):
            body = subprogram
        elif hasattr(subprogram, 'body'):
            body = subprogram.body
        else:
            # Spec only - nothing to instantiate
            self._generic_type_map = {}
            self._generic_prefix = None
            return

        # Create instantiated subprogram with the new name
        original_name = body.spec.name
        body.spec.name = inst.name
        self._lower_subprogram_body(body)
        body.spec.name = original_name  # Restore original name

        # Clear the type map
        self._generic_type_map = {}
        self._generic_prefix = None

    def _build_generic_type_map(self, formals: list, actuals: list) -> dict[str, str]:
        """Build a mapping from formal generic parameters to actual parameters."""
        type_map: dict[str, str] = {}
        for i, formal in enumerate(formals):
            if i < len(actuals):
                formal_name = formal.name if hasattr(formal, "name") else str(formal)
                actual = actuals[i]
                actual_name = (
                    actual.value.name
                    if hasattr(actual.value, "name")
                    else str(actual.value)
                )
                type_map[formal_name.lower()] = actual_name
        return type_map

    def _block_has_return(self, block: Optional[BasicBlock]) -> bool:
        """Check if a block ends with a return."""
        if block is None or not block.instructions:
            return False
        return block.instructions[-1].opcode == OpCode.RET

    # =========================================================================
    # Declarations
    # =========================================================================

    def _lower_declaration(self, decl) -> None:
        """Lower a declaration."""
        if isinstance(decl, ObjectDecl):
            self._lower_object_decl(decl)
        elif isinstance(decl, NumberDecl):
            self._lower_number_decl(decl)
        elif isinstance(decl, ExceptionDecl):
            self._lower_exception_decl(decl)
        elif isinstance(decl, RenamingDecl):
            self._lower_generic_renaming(decl)
        elif isinstance(decl, SubprogramBody):
            # Nested subprogram - lower separately
            self._lower_subprogram_body(decl)
        elif isinstance(decl, TypeDecl):
            self._lower_type_decl(decl)
        elif isinstance(decl, SubtypeDecl):
            self._lower_subtype_decl(decl)
        elif isinstance(decl, ProtectedTypeDecl):
            self._lower_protected_type_decl(decl)
        elif isinstance(decl, ProtectedBody):
            self._lower_protected_body(decl)

    def _lower_object_decl(self, decl: ObjectDecl) -> None:
        """Lower an object declaration."""
        if self.ctx is None:
            return

        # Handle renaming declarations
        if decl.renames:
            self._lower_renaming_decl(decl)
            return

        # Get the type for controlled type checks
        ada_type = None
        if decl.type_mark and decl.type_mark.type_mark:
            if isinstance(decl.type_mark.type_mark, Identifier):
                type_sym = self.symbols.lookup(decl.type_mark.type_mark.name)
                if type_sym:
                    ada_type = type_sym.ada_type

        # Process initialization
        if decl.init_expr:
            init_value = self._lower_expr(decl.init_expr)

            for name in decl.names:
                local = self.ctx.locals.get(name.lower())
                if local:
                    self.builder.mov(local.vreg, init_value,
                                    comment=f"init {name}")

                    # For controlled types with initializer, call Adjust
                    # (Initialize is for default initialization only)
                    if ada_type and self._type_needs_adjustment(ada_type):
                        self._call_adjust(local.vreg, ada_type)

                    # Check type invariant after initialization
                    if decl.type_mark and decl.type_mark.type_mark:
                        self._check_type_invariant(local.vreg, decl.type_mark.type_mark)
        else:
            # Default initialization - call Initialize for controlled types
            for name in decl.names:
                local = self.ctx.locals.get(name.lower())
                if local and ada_type and self._type_needs_finalization(ada_type):
                    self._call_initialize(local.vreg, ada_type)

        # Track controlled objects for finalization at scope exit
        if ada_type and self._type_needs_finalization(ada_type):
            for name in decl.names:
                local = self.ctx.locals.get(name.lower())
                if local:
                    self._register_for_finalization(name, local, ada_type)

    def _lower_number_decl(self, decl: NumberDecl) -> None:
        """Lower a named number declaration.

        Named numbers are compile-time constants like:
            PI : constant := 3.14159;
            Max_Size : constant := 100;

        These don't allocate storage - the value is substituted at compile time.
        We register the constant value in the symbol table for use in expressions.
        """
        # Evaluate the constant expression at compile time
        const_value = self._evaluate_static_expr(decl.value)

        # Register each name as a compile-time constant
        for name in decl.names:
            # Store in context for local use
            if self.ctx:
                self.ctx.named_numbers[name.lower()] = const_value

            # Also register in symbol table as a constant
            sym = Symbol(
                name=name,
                kind=SymbolKind.CONSTANT,
                value=const_value,
            )
            self.symbols.define(sym)

    def _evaluate_static_expr(self, expr: Expr) -> int:
        """Evaluate a static expression at compile time.

        Returns the constant integer value of the expression.
        Used for named numbers, array bounds, etc.
        """
        from uada80.ast_nodes import IntegerLiteral, RealLiteral, Identifier, BinaryExpr, UnaryExpr

        if isinstance(expr, IntegerLiteral):
            return expr.value
        elif isinstance(expr, RealLiteral):
            # Convert to fixed-point representation for Z80
            return int(expr.value * 65536)  # 16.16 fixed point
        elif isinstance(expr, Identifier):
            # Look up named number or constant
            name = expr.name.lower()
            if self.ctx and name in self.ctx.named_numbers:
                return self.ctx.named_numbers[name]
            sym = self.symbols.lookup(expr.name)
            if sym and sym.kind == SymbolKind.CONSTANT and sym.value is not None:
                return sym.value
            return 0
        elif isinstance(expr, BinaryExpr):
            left = self._evaluate_static_expr(expr.left)
            right = self._evaluate_static_expr(expr.right)
            op = expr.op
            if op == BinaryOp.ADD:
                return left + right
            elif op == BinaryOp.SUB:
                return left - right
            elif op == BinaryOp.MUL:
                return left * right
            elif op == BinaryOp.DIV:
                return left // right if right != 0 else 0
            elif op == BinaryOp.MOD:
                return left % right if right != 0 else 0
            elif op == BinaryOp.EXP:
                return left ** right
            return 0
        elif isinstance(expr, UnaryExpr):
            operand = self._evaluate_static_expr(expr.operand)
            op = expr.op
            if op == UnaryOp.MINUS:
                return -operand
            elif op == UnaryOp.PLUS:
                return operand
            elif op == UnaryOp.ABS:
                return abs(operand)
            return operand
        return 0

    def _lower_exception_decl(self, decl: ExceptionDecl) -> None:
        """Lower an exception declaration.

        User-defined exceptions like:
            My_Error : exception;

        Each exception gets a unique ID for runtime identification.
        The exception info is stored in a global table.
        """
        if not hasattr(self, '_exception_counter'):
            self._exception_counter = 100  # Start user exceptions at 100

        for name in decl.names:
            # Assign unique exception ID
            exc_id = self._exception_counter
            self._exception_counter += 1

            # Register in symbol table
            sym = Symbol(
                name=name,
                kind=SymbolKind.EXCEPTION,
                value=exc_id,
            )
            self.symbols.define(sym)

            # Generate exception info in data section
            if self.builder.module:
                exc_label = f"_exc_{name.lower()}"
                # Exception info: ID (2 bytes) + name pointer
                self.builder.module.add_global(exc_label, exc_id)

    def _lower_generic_renaming(self, decl: RenamingDecl) -> None:
        """Lower a general renaming declaration.

        Ada supports several kinds of renaming:
        - Object renaming: X : T renames Y.Field;
        - Exception renaming: My_Error : exception renames Pkg.Error;
        - Package renaming: package P renames Q;
        - Subprogram renaming: procedure X renames Y;

        For most renamings, we create a symbol alias that points to
        the renamed entity. The renamed expression is evaluated once
        at elaboration time.
        """
        if self.ctx is None:
            return

        # Get the renamed entity's symbol/address
        renamed_expr = decl.renames

        # Try to find the renamed entity in symbol table
        renamed_sym = None
        if isinstance(renamed_expr, Identifier):
            renamed_sym = self.symbols.lookup(renamed_expr.name)
        elif isinstance(renamed_expr, SelectedName):
            # Handle Pkg.Name style renaming
            full_name = self._get_selected_name_str(renamed_expr)
            renamed_sym = self.symbols.lookup(full_name)

        # Create aliases for each declared name
        for name in decl.names:
            if renamed_sym:
                # Create an alias symbol that points to the renamed entity
                alias_sym = Symbol(
                    name=name,
                    kind=renamed_sym.kind,
                    value=renamed_sym.value,
                    ada_type=renamed_sym.ada_type,
                    alias_for=renamed_sym.name,
                )
                self.symbols.define(alias_sym)
            else:
                # No symbol found - compute address at runtime
                renamed_addr = self._lower_expr(renamed_expr)
                local = self.ctx.locals.get(name.lower())
                if local:
                    self.builder.mov(local.vreg, renamed_addr,
                                    comment=f"rename {name}")

    def _get_selected_name_str(self, expr: SelectedName) -> str:
        """Get the full dotted name string from a SelectedName."""
        parts = []
        current = expr
        while isinstance(current, SelectedName):
            parts.append(current.selector)
            current = current.prefix
        if isinstance(current, Identifier):
            parts.append(current.name)
        return ".".join(reversed(parts))

    def _type_needs_finalization(self, ada_type) -> bool:
        """Check if a type needs finalization."""
        from uada80.type_system import RecordType
        if isinstance(ada_type, RecordType):
            return ada_type.needs_finalization()
        return False

    def _type_needs_adjustment(self, ada_type) -> bool:
        """Check if a type needs adjustment after assignment."""
        from uada80.type_system import RecordType
        if isinstance(ada_type, RecordType):
            return ada_type.needs_adjustment()
        return False

    def _call_initialize(self, obj_ptr, ada_type) -> None:
        """Generate a call to Initialize for a controlled object."""
        # Push object pointer as argument
        self.builder.push(obj_ptr)
        # Call Initialize (mangled name based on type)
        init_name = f"{ada_type.name}_Initialize"
        self.builder.call(Label(init_name))
        # Clean up stack
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

    def _call_adjust(self, obj_ptr, ada_type) -> None:
        """Generate a call to Adjust for a controlled object after assignment."""
        # Push object pointer as argument
        self.builder.push(obj_ptr)
        # Call Adjust (mangled name based on type)
        adj_name = f"{ada_type.name}_Adjust"
        self.builder.call(Label(adj_name))
        # Clean up stack
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

    def _call_finalize(self, obj_ptr, ada_type) -> None:
        """Generate a call to Finalize for a controlled object going out of scope."""
        # Push object pointer as argument
        self.builder.push(obj_ptr)
        # Call Finalize (mangled name based on type)
        fin_name = f"{ada_type.name}_Finalize"
        self.builder.call(Label(fin_name))
        # Clean up stack
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

    def _register_for_finalization(self, name: str, local, ada_type) -> None:
        """Register a controlled object for finalization at scope exit."""
        if self.ctx is None:
            return
        # Store info for finalization when scope exits
        if not hasattr(self.ctx, 'finalization_list'):
            self.ctx.finalization_list = []
        self.ctx.finalization_list.append((name, local, ada_type))

    def _generate_finalizations(self) -> None:
        """Generate finalization calls for all registered controlled objects."""
        if self.ctx is None:
            return
        if not hasattr(self.ctx, 'finalization_list'):
            return

        # Finalize in reverse order of declaration (LIFO)
        for name, local, ada_type in reversed(self.ctx.finalization_list):
            self._call_finalize(local.vreg, ada_type)

    def _check_discriminant_constraints(self, obj_ptr, ada_type, disc_values: dict) -> None:
        """Check discriminant constraints for a record object.

        obj_ptr: pointer to the record object
        ada_type: the record type
        disc_values: dict mapping discriminant name -> value (vreg or immediate)
        """
        from uada80.type_system import RecordType, DiscriminantConstraint

        if not isinstance(ada_type, RecordType) or not ada_type.has_discriminants():
            return

        for disc in ada_type.discriminants:
            if disc.discriminant_constraint:
                constraint = disc.discriminant_constraint

                # Get the actual discriminant value
                disc_value = disc_values.get(disc.name.lower())
                if disc_value is None:
                    continue

                # Check against constraint
                if constraint.constraint_value is not None:
                    # Exact value constraint: disc = value
                    ne_result = self.builder.new_vreg(IRType.BOOL, "_disc_ne")
                    self.builder.cmp_ne(ne_result, disc_value,
                                       Immediate(constraint.constraint_value, IRType.WORD))
                    self.builder.jnz(ne_result, Label("_raise_constraint_error"))

                elif constraint.constraint_low is not None and constraint.constraint_high is not None:
                    # Range constraint: low <= disc <= high
                    lt_result = self.builder.new_vreg(IRType.BOOL, "_disc_lt")
                    self.builder.cmp_lt(lt_result, disc_value,
                                       Immediate(constraint.constraint_low, IRType.WORD))
                    self.builder.jnz(lt_result, Label("_raise_constraint_error"))

                    gt_result = self.builder.new_vreg(IRType.BOOL, "_disc_gt")
                    self.builder.cmp_gt(gt_result, disc_value,
                                       Immediate(constraint.constraint_high, IRType.WORD))
                    self.builder.jnz(gt_result, Label("_raise_constraint_error"))

    def _check_type_invariant(self, value, type_expr) -> None:
        """Check type invariant for a value if the type has one."""
        # Look up the type and check for Type_Invariant aspect
        if isinstance(type_expr, Identifier):
            type_sym = self.symbols.lookup(type_expr.name)
            if type_sym and type_sym.definition:
                type_decl = type_sym.definition
                if hasattr(type_decl, 'aspects'):
                    for aspect in type_decl.aspects:
                        if aspect.name.lower() == "type_invariant" and aspect.value:
                            # Evaluate invariant with the value
                            # For simplicity, we assume the invariant references
                            # the type name to mean the current value
                            cond_value = self._lower_expr(aspect.value)
                            fail_label = self._new_label("inv_fail")
                            ok_label = self._new_label("inv_ok")
                            self.builder.jnz(cond_value, Label(ok_label))
                            # Invariant failed - raise Assertion_Error
                            fail_block = self.builder.new_block(fail_label)
                            self.builder.set_block(fail_block)
                            self.builder.emit(IRInstr(OpCode.EXC_RAISE,
                                                      src1=Immediate(5, IRType.WORD),
                                                      comment="Type_Invariant failed"))
                            ok_block = self.builder.new_block(ok_label)
                            self.builder.set_block(ok_block)

    def _check_subtype_predicate(self, value, var_name: str) -> None:
        """Check subtype predicate for a value if its subtype has one."""
        # Look up the variable's type
        sym = self.symbols.lookup(var_name)
        if not sym or not sym.ada_type:
            return

        # Check if the type has a predicate aspect
        type_sym = self.symbols.lookup(sym.ada_type.name) if sym.ada_type.name else None
        if type_sym and type_sym.definition:
            type_decl = type_sym.definition
            if hasattr(type_decl, 'aspects'):
                for aspect in type_decl.aspects:
                    aspect_name = aspect.name.lower()
                    if aspect_name in ("static_predicate", "dynamic_predicate") and aspect.value:
                        # Evaluate predicate with the value
                        cond_value = self._lower_expr(aspect.value)
                        fail_label = self._new_label("pred_fail")
                        ok_label = self._new_label("pred_ok")
                        self.builder.jnz(cond_value, Label(ok_label))
                        # Predicate failed - raise Assertion_Error
                        fail_block = self.builder.new_block(fail_label)
                        self.builder.set_block(fail_block)
                        self.builder.emit(IRInstr(OpCode.EXC_RAISE,
                                                  src1=Immediate(5, IRType.WORD),
                                                  comment=f"{aspect_name} failed"))
                        ok_block = self.builder.new_block(ok_label)
                        self.builder.set_block(ok_block)

    def _lower_type_decl(self, decl: TypeDecl) -> None:
        """Lower a type declaration.

        Type declarations don't generate code directly, but we process:
        - Default_Value/Default_Component_Value aspects for array/record types
        - Type_Invariant aspects (registered for checking)
        - Discriminant defaults
        """
        if self.ctx is None:
            return

        # Type declarations don't generate runtime code in most cases
        # The type information is recorded in the symbol table during semantic analysis
        # Code is generated when objects of the type are declared

        # However, we may need to process aspects that affect code generation
        if hasattr(decl, 'aspects') and decl.aspects:
            for aspect in decl.aspects:
                aspect_name = aspect.name.lower()
                if aspect_name == "default_value" and aspect.value:
                    # Store default value expression for use in object declarations
                    # This is handled at object declaration time
                    pass
                elif aspect_name == "default_component_value" and aspect.value:
                    # For array types - default value for each component
                    pass
                # Type_Invariant is checked at assignment/call boundaries
                # Static/Dynamic_Predicate is checked at subtype constraints

    def _lower_subtype_decl(self, decl: SubtypeDecl) -> None:
        """Lower a subtype declaration.

        Subtype declarations don't generate code directly, but we may need to:
        - Register predicate checks for use at assignment/call boundaries
        - Process range constraints for bounds checking
        """
        if self.ctx is None:
            return

        # Subtype declarations are primarily handled during semantic analysis
        # The subtype constraints are checked at appropriate points during
        # object declaration and assignment

        # Process aspects like Static_Predicate, Dynamic_Predicate
        if hasattr(decl, 'aspects') and decl.aspects:
            for aspect in decl.aspects:
                aspect_name = aspect.name.lower()
                if aspect_name in ("static_predicate", "dynamic_predicate"):
                    # Predicates are checked at assignment and call boundaries
                    # The actual checking is done by _check_subtype_predicate
                    pass

    def _lower_protected_type_decl(self, decl: ProtectedTypeDecl) -> None:
        """Lower a protected type declaration.

        Protected types provide mutual exclusion for their operations.
        The type includes a lock byte at offset 0 for synchronization.
        """
        # Protected type declarations are handled during semantic analysis.
        # The type layout is computed there (lock byte + components).
        # We just need to ensure the protected operations are registered.
        pass

    def _lower_protected_body(self, decl: ProtectedBody) -> None:
        """Lower a protected body (implementation of protected operations).

        Each protected procedure/function is wrapped with lock/unlock calls.
        """
        if self.ctx is None:
            return

        # Get the protected type information
        prot_sym = self.symbols.lookup(decl.name)
        if not prot_sym or not isinstance(prot_sym.ada_type, ProtectedType):
            return

        # Lower each operation body with lock wrapper
        for item in decl.items:
            if isinstance(item, SubprogramBody):
                self._lower_protected_operation(decl.name, item, prot_sym.ada_type)

    def _lower_protected_operation(self, prot_name: str, body: SubprogramBody, prot_type: ProtectedType) -> None:
        """Lower a protected operation (procedure/function) body.

        Generates wrapper code:
        1. Acquire lock
        2. Execute operation body
        3. Release lock
        """
        # Save current context
        old_ctx = self.ctx

        # Create the operation name (Protected_Name.Operation_Name)
        op_name = f"{prot_name}_{body.name}"

        # Create IR function for this operation
        ir_func = IRFunction(name=op_name)
        if self.builder.module:
            self.builder.module.functions.append(ir_func)

        # Create entry block
        entry_block = BasicBlock(name=f"L_{op_name}_entry")
        ir_func.blocks.append(entry_block)
        self.builder.set_function(ir_func)
        self.builder.set_block(entry_block)

        # Create new context
        self.ctx = FunctionContext(name=op_name)

        # Emit lock acquisition
        # The protected object address is passed as first implicit parameter
        prot_obj = self.builder.new_vreg(IRType.PTR, "_protected_obj")
        self.builder.pop(prot_obj)  # Get protected object address from stack

        # Call lock acquire
        self.builder.push(prot_obj)
        self.builder.call(Label("_protected_lock"), comment=f"acquire lock for {prot_name}")
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

        # Lower the parameters (after the implicit protected object param)
        self._setup_parameters(body.params)

        # Lower the declarations
        for d in body.decls:
            self._lower_declaration(d)

        # Lower the statements
        for stmt in body.stmts:
            self._lower_statement(stmt)

        # Emit lock release before return
        self.builder.push(prot_obj)
        self.builder.call(Label("_protected_unlock"), comment=f"release lock for {prot_name}")
        self.builder.pop(temp)

        # Return
        if body.subprogram_kind == "function":
            # Function: result was set in local "_result"
            result_local = self.ctx.locals.get("_result")
            if result_local:
                self.builder.push(result_local.vreg)
        self.builder.ret()

        # Restore context
        self.ctx = old_ctx

    def _lower_renaming_decl(self, decl: ObjectDecl) -> None:
        """Lower a renaming declaration (X : T renames Y).

        A renaming doesn't allocate new storage - X becomes an alias for Y.
        We store the address of the renamed object.
        """
        if self.ctx is None:
            return

        # Get the address of the renamed object
        renamed_addr = self._lower_expr(decl.renames)

        # For each name, store the alias in locals
        for name in decl.names:
            local = self.ctx.locals.get(name.lower())
            if local:
                # Store the address/reference of the renamed object
                self.builder.mov(local.vreg, renamed_addr,
                                comment=f"rename {name}")

    # =========================================================================
    # Statements
    # =========================================================================

    def _lower_statement(self, stmt: Stmt) -> None:
        """Lower a statement."""
        if isinstance(stmt, NullStmt):
            pass  # Nothing to generate
        elif isinstance(stmt, AssignmentStmt):
            self._lower_assignment(stmt)
        elif isinstance(stmt, IfStmt):
            self._lower_if(stmt)
        elif isinstance(stmt, LoopStmt):
            self._lower_loop(stmt)
        elif isinstance(stmt, BlockStmt):
            self._lower_block(stmt)
        elif isinstance(stmt, ExitStmt):
            self._lower_exit(stmt)
        elif isinstance(stmt, ReturnStmt):
            self._lower_return(stmt)
        elif isinstance(stmt, ProcedureCallStmt):
            self._lower_procedure_call(stmt)
        elif isinstance(stmt, CaseStmt):
            self._lower_case(stmt)
        elif isinstance(stmt, RaiseStmt):
            self._lower_raise(stmt)
        elif isinstance(stmt, LabeledStmt):
            self._lower_labeled(stmt)
        elif isinstance(stmt, GotoStmt):
            self._lower_goto(stmt)
        # Tasking statements
        elif isinstance(stmt, AcceptStmt):
            self._lower_accept(stmt)
        elif isinstance(stmt, SelectStmt):
            self._lower_select(stmt)
        elif isinstance(stmt, DelayStmt):
            self._lower_delay(stmt)
        elif isinstance(stmt, AbortStmt):
            self._lower_abort(stmt)
        elif isinstance(stmt, RequeueStmt):
            self._lower_requeue(stmt)
        elif isinstance(stmt, ParallelBlockStmt):
            self._lower_parallel_block(stmt)
        elif isinstance(stmt, PragmaStmt):
            self._lower_pragma(stmt)
        elif isinstance(stmt, ExtendedReturnStmt):
            self._lower_extended_return(stmt)

    def _lower_labeled(self, stmt: LabeledStmt) -> None:
        """Lower a labeled statement (<<Label>> stmt)."""
        if self.ctx is None:
            return

        # Emit the label
        label_name = f"_usr_{stmt.label.lower()}"
        self.builder.label(label_name)

        # Lower the inner statement
        self._lower_statement(stmt.statement)

    def _lower_goto(self, stmt: GotoStmt) -> None:
        """Lower a goto statement."""
        if self.ctx is None:
            return

        # Jump to the user-defined label
        label_name = f"_usr_{stmt.label.lower()}"
        self.builder.jmp(Label(label_name))

    # =========================================================================
    # Tasking Statements
    # =========================================================================

    def _lower_accept(self, stmt: AcceptStmt) -> None:
        """Lower an accept statement (task entry accept).

        Syntax: accept Entry_Name (params) do ... end;

        For Z80 (single-threaded), we emit a runtime call to handle
        the rendezvous protocol. In a full implementation, this would
        involve task scheduling and synchronization.
        """
        if self.ctx is None:
            return

        # Push entry name for the runtime
        entry_label = self.builder.new_string_label()
        if self.builder.module:
            self.builder.module.add_string(entry_label, stmt.entry_name)
        entry_reg = self.builder.new_vreg(IRType.PTR, "_entry_name")
        self.builder.mov(entry_reg, Label(entry_label))
        self.builder.push(entry_reg)

        # Call runtime to wait for entry call
        self.builder.call(Label("_task_accept_start"), comment=f"accept {stmt.entry_name}")

        # Clean up stack
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

        # Lower the accept body statements
        for s in stmt.statements:
            self._lower_statement(s)

        # Signal accept completion
        self.builder.call(Label("_task_accept_end"))

    def _lower_select(self, stmt: SelectStmt) -> None:
        """Lower a select statement (selective accept, timed entry, etc.).

        Supports various forms:
        - Selective accept: select accept E1; or accept E2; end select;
        - Timed entry call: select call or delay D; end select;
        - Conditional entry: select call else stmts; end select;
        - Asynchronous select: select triggering_stmt then abort seq; end select;

        For Z80 (single-threaded), we emit runtime calls for the select protocol.
        """
        if self.ctx is None:
            return

        # Generate labels for each alternative
        end_label = self._new_label("select_end")
        alt_labels = []

        # Emit select start
        self.builder.call(Label("_task_select_start"))

        # Process each alternative
        for i, alt in enumerate(stmt.alternatives):
            alt_label = self._new_label(f"select_alt_{i}")
            alt_labels.append(alt_label)

            # Register this alternative with the runtime
            self.builder.push(Immediate(i, IRType.WORD))
            self.builder.call(Label("_task_select_register"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)

        # Wait for one alternative to be ready
        self.builder.call(Label("_task_select_wait"))

        # The runtime returns the index of the selected alternative in HL
        selected = self.builder.new_vreg(IRType.WORD, "_selected")
        self.builder.emit(IRInstr(
            OpCode.MOV, selected,
            MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
            comment="selected alternative index"
        ))

        # Generate code for each alternative (jump table style)
        for i, alt in enumerate(stmt.alternatives):
            next_label = alt_labels[i + 1] if i + 1 < len(alt_labels) else end_label
            self.builder.cmp(selected, Immediate(i, IRType.WORD))
            self.builder.jnz(Label(next_label))

            self.builder.label(alt_labels[i])
            # Lower the statements for this alternative
            if hasattr(alt, 'statements'):
                for s in alt.statements:
                    self._lower_statement(s)
            self.builder.jmp(Label(end_label))

        # Handle else clause if present
        if stmt.else_statements:
            else_label = self._new_label("select_else")
            self.builder.label(else_label)
            for s in stmt.else_statements:
                self._lower_statement(s)

        self.builder.label(end_label)
        self.builder.call(Label("_task_select_end"))

    def _lower_delay(self, stmt: DelayStmt) -> None:
        """Lower a delay statement.

        delay D;         -- relative delay
        delay until T;   -- absolute delay

        For Z80, calls the runtime delay function.
        """
        if self.ctx is None:
            return

        # Evaluate the delay expression
        delay_val = self._lower_expr(stmt.expression)
        self.builder.push(delay_val)

        if stmt.is_until:
            # delay until: wait until absolute time
            self.builder.call(Label("_task_delay_until"), comment="delay until")
        else:
            # delay: relative delay
            self.builder.call(Label("_task_delay"), comment="delay")

        # Clean up stack
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

    def _lower_abort(self, stmt: AbortStmt) -> None:
        """Lower an abort statement.

        abort Task1, Task2, ...;

        Aborts the specified tasks. For Z80, calls runtime for each task.
        """
        if self.ctx is None:
            return

        for task_name in stmt.task_names:
            task_val = self._lower_expr(task_name)
            self.builder.push(task_val)
            self.builder.call(Label("_task_abort"), comment="abort task")
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)

    def _lower_requeue(self, stmt: RequeueStmt) -> None:
        """Lower a requeue statement.

        requeue Entry_Name;
        requeue Entry_Name with abort;

        Requeues the current entry call to another entry.
        """
        if self.ctx is None:
            return

        # Evaluate the target entry
        entry_val = self._lower_expr(stmt.entry_name)
        self.builder.push(entry_val)

        # Push with_abort flag
        abort_flag = 1 if stmt.is_with_abort else 0
        self.builder.push(Immediate(abort_flag, IRType.WORD))

        self.builder.call(Label("_task_requeue"), comment="requeue")

        # Clean up stack
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)
        self.builder.pop(temp)

    def _lower_parallel_block(self, stmt: ParallelBlockStmt) -> None:
        """Lower a parallel block statement (Ada 2022).

        parallel do seq1; and do seq2; end parallel;

        For Z80 (single-threaded), we execute sequences sequentially
        but emit markers for a potential future parallel runtime.
        """
        if self.ctx is None:
            return

        # Emit parallel start marker
        num_sequences = len(stmt.sequences)
        self.builder.push(Immediate(num_sequences, IRType.WORD))
        self.builder.call(Label("_parallel_start"), comment=f"parallel ({num_sequences} sequences)")
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

        # Execute each sequence (sequentially for now)
        for i, seq in enumerate(stmt.sequences):
            # Notify runtime of sequence start
            self.builder.push(Immediate(i, IRType.WORD))
            self.builder.call(Label("_parallel_seq_start"))
            self.builder.pop(temp)

            # Lower the statements in this sequence
            for s in seq:
                self._lower_statement(s)

            # Notify runtime of sequence end
            self.builder.push(Immediate(i, IRType.WORD))
            self.builder.call(Label("_parallel_seq_end"))
            self.builder.pop(temp)

        # Emit parallel end marker (wait for all sequences)
        self.builder.call(Label("_parallel_end"))

    def _lower_pragma(self, stmt: PragmaStmt) -> None:
        """Lower a pragma statement.

        pragma Pragma_Name(arguments);

        Most pragmas are handled at compile-time or by the semantic analyzer.
        Some pragmas (like pragma Assert) need runtime code.
        """
        if self.ctx is None:
            return

        pragma_name = stmt.name.lower() if hasattr(stmt, 'name') else ""

        if pragma_name == "assert":
            # pragma Assert(Condition [, Message]);
            if stmt.arguments:
                cond = self._lower_expr(stmt.arguments[0])
                self.builder.cmp(cond, Immediate(0, IRType.WORD))
                ok_label = self._new_label("assert_ok")
                self.builder.jnz(Label(ok_label))

                # Assertion failed - raise Assertion_Error
                exc_id = self._get_exception_id("Assertion_Error")
                self.builder.emit(IRInstr(
                    OpCode.EXC_RAISE,
                    src1=Immediate(exc_id, IRType.WORD),
                    comment="assertion failed",
                ))

                self.builder.label(ok_label)

        elif pragma_name == "debug":
            # pragma Debug(procedure_call);
            # Execute the call only in debug mode (for now, always execute)
            if stmt.arguments:
                arg = stmt.arguments[0]
                if isinstance(arg, ProcedureCallStmt):
                    self._lower_procedure_call(arg)

        # Other pragmas: Optimize, Inline, Suppress, etc. are compile-time only
        # No runtime code needed

    def _lower_extended_return(self, stmt: ExtendedReturnStmt) -> None:
        """Lower an extended return statement (Ada 2005).

        return Obj : Type [:= Init] do
            statements
        end return;

        Creates a return object, executes statements, then returns.
        """
        if self.ctx is None:
            return

        # Create the return object as a local
        # The object name and type are in stmt.object_name and stmt.type_mark
        obj_name = stmt.object_name if hasattr(stmt, 'object_name') else "_ret_obj"
        obj_type_name = ""
        if hasattr(stmt, 'type_mark') and stmt.type_mark:
            if isinstance(stmt.type_mark, Identifier):
                obj_type_name = stmt.type_mark.name

        # Allocate space for the return object
        ret_obj = self.builder.new_vreg(IRType.WORD, f"_{obj_name}")

        # Initialize if there's an initializer
        if hasattr(stmt, 'init_expr') and stmt.init_expr:
            init_val = self._lower_expr(stmt.init_expr)
            self.builder.mov(ret_obj, init_val)
        else:
            self.builder.mov(ret_obj, Immediate(0, IRType.WORD))

        # Add to locals so statements can reference it
        if self.ctx:
            self.ctx.locals[obj_name.lower()] = LocalVariable(
                name=obj_name,
                vreg=ret_obj,
                stack_offset=0,
                size=2,
            )

        # Execute the statements
        if hasattr(stmt, 'statements'):
            for s in stmt.statements:
                self._lower_statement(s)

        # Return the object
        self.builder.ret(ret_obj)

    def _lower_raise(self, stmt: RaiseStmt) -> None:
        """Lower a raise statement."""
        if self.ctx is None:
            return

        if stmt.exception_name is None:
            # Re-raise: raise;
            self.builder.emit(IRInstr(OpCode.EXC_RERAISE))
        else:
            # Get exception name
            exc_name = ""
            if isinstance(stmt.exception_name, Identifier):
                exc_name = stmt.exception_name.name
            elif hasattr(stmt.exception_name, "name"):
                exc_name = stmt.exception_name.name

            exc_id = self._get_exception_id(exc_name)

            # Handle message if present
            msg_vreg = None
            if stmt.message:
                msg_vreg = self._lower_expr(stmt.message)

            # Emit raise instruction
            self.builder.emit(IRInstr(
                OpCode.EXC_RAISE,
                src1=Immediate(exc_id, IRType.WORD),
                src2=msg_vreg,
                comment=f"raise {exc_name}",
            ))

    def _lower_assignment(self, stmt: AssignmentStmt) -> None:
        """Lower an assignment statement."""
        if self.ctx is None:
            return

        # Set assignment target for @ (TargetName) support
        old_target = self.ctx.assignment_target
        self.ctx.assignment_target = stmt.target

        try:
            value = self._lower_expr(stmt.value)

            # Get target type for controlled type handling and range checking
            target_type = self._get_target_type(stmt.target)

            # Emit range check for constrained types
            if target_type:
                self._emit_range_check(value, target_type, "assignment range check")

            # Get target
            if isinstance(stmt.target, Identifier):
                name = stmt.target.name.lower()

                # Check locals
                if name in self.ctx.locals:
                    local = self.ctx.locals[name]

                    # For controlled types: Finalize old, copy, Adjust new
                    if target_type and self._type_needs_adjustment(target_type):
                        self._call_finalize(local.vreg, target_type)
                        self.builder.mov(local.vreg, value, comment=f"{name} := ...")
                        self._call_adjust(local.vreg, target_type)
                    else:
                        self.builder.mov(local.vreg, value, comment=f"{name} := ...")
                    return

                # Check params
                if name in self.ctx.params:
                    param = self.ctx.params[name]

                    # For controlled types: Finalize old, copy, Adjust new
                    if target_type and self._type_needs_adjustment(target_type):
                        self._call_finalize(param, target_type)
                        self.builder.mov(param, value, comment=f"{name} := ...")
                        self._call_adjust(param, target_type)
                    else:
                        self.builder.mov(param, value, comment=f"{name} := ...")
                    return

            elif isinstance(stmt.target, IndexedComponent):
                # Array assignment
                self._lower_indexed_store(stmt.target, value)

            elif isinstance(stmt.target, SelectedName):
                # Record field assignment
                self._lower_selected_store(stmt.target, value)

            elif isinstance(stmt.target, Slice):
                # Array slice assignment: A(1..5) := B(1..5)
                self._lower_slice_store(stmt.target, value)
        finally:
            # Restore previous assignment target
            self.ctx.assignment_target = old_target

    def _get_target_type(self, target):
        """Get the Ada type of an assignment target."""
        if isinstance(target, Identifier):
            sym = self.symbols.lookup(target.name)
            if sym:
                return sym.ada_type
        elif isinstance(target, SelectedName):
            # For record field access, we'd need to look up the field type
            # Simplified: look up the prefix's type
            if isinstance(target.prefix, Identifier):
                sym = self.symbols.lookup(target.prefix.name)
                if sym and sym.ada_type:
                    from uada80.type_system import RecordType
                    if isinstance(sym.ada_type, RecordType):
                        comp = sym.ada_type.get_component(target.selector)
                        if comp:
                            return comp.component_type
        return None

    def _lower_if(self, stmt: IfStmt) -> None:
        """Lower an if statement."""
        if self.ctx is None:
            return

        then_label = self._new_label("then")
        else_label = self._new_label("else")
        end_label = self._new_label("endif")

        # Evaluate condition
        cond = self._lower_expr(stmt.condition)

        # Jump to else if condition is false (zero)
        self.builder.jz(cond, Label(else_label))

        # Then block
        then_block = self.builder.new_block(then_label)
        self.builder.set_block(then_block)

        for s in stmt.then_stmts:
            self._lower_statement(s)

        if not self._block_has_return(self.builder.block):
            self.builder.jmp(Label(end_label))

        # Elsif parts
        for elsif_cond, elsif_stmts in stmt.elsif_parts:
            elsif_label = self._new_label("elsif")
            elsif_block = self.builder.new_block(elsif_label)
            self.builder.set_block(elsif_block)

            cond = self._lower_expr(elsif_cond)
            next_label = self._new_label("elsif_next")
            self.builder.jz(cond, Label(next_label))

            for s in elsif_stmts:
                self._lower_statement(s)

            if not self._block_has_return(self.builder.block):
                self.builder.jmp(Label(end_label))

        # Else block
        else_block = self.builder.new_block(else_label)
        self.builder.set_block(else_block)

        for s in stmt.else_stmts:
            self._lower_statement(s)

        if not self._block_has_return(self.builder.block):
            self.builder.jmp(Label(end_label))

        # End block
        end_block = self.builder.new_block(end_label)
        self.builder.set_block(end_block)

    def _lower_loop(self, stmt: LoopStmt) -> None:
        """Lower a loop statement."""
        if self.ctx is None:
            return

        loop_label = self._new_label("loop")
        end_label = self._new_label("endloop")

        # Save exit label for exit statements
        old_exit = self.ctx.loop_exit_label
        self.ctx.loop_exit_label = end_label

        # Register named loop label if present
        stmt_label_lower = None
        if stmt.label:
            stmt_label_lower = stmt.label.lower()
            self.ctx.loop_exit_labels[stmt_label_lower] = end_label

        if stmt.iteration_scheme is None:
            # Simple infinite loop
            loop_block = self.builder.new_block(loop_label)
            self.builder.set_block(loop_block)

            for s in stmt.statements:
                self._lower_statement(s)

            self.builder.jmp(Label(loop_label))

        elif isinstance(stmt.iteration_scheme, WhileScheme):
            # While loop
            cond_label = self._new_label("while_cond")
            body_label = self._new_label("while_body")

            # Jump to condition check
            self.builder.jmp(Label(cond_label))

            # Condition block
            cond_block = self.builder.new_block(cond_label)
            self.builder.set_block(cond_block)

            cond = self._lower_expr(stmt.iteration_scheme.condition)
            self.builder.jz(cond, Label(end_label))

            # Body block
            body_block = self.builder.new_block(body_label)
            self.builder.set_block(body_block)

            for s in stmt.statements:
                self._lower_statement(s)

            self.builder.jmp(Label(cond_label))

        elif isinstance(stmt.iteration_scheme, ForScheme):
            # For loop
            iterator = stmt.iteration_scheme.iterator

            # Create loop variable
            loop_var = self.builder.new_vreg(IRType.WORD, iterator.name)
            self.ctx.locals[iterator.name.lower()] = LocalVariable(
                name=iterator.name,
                vreg=loop_var,
                stack_offset=0,
                size=2,
            )

            # Get range bounds
            if isinstance(iterator.iterable, RangeExpr):
                low = self._lower_expr(iterator.iterable.low)
                high = self._lower_expr(iterator.iterable.high)
            else:
                # Default range
                low = Immediate(1, IRType.WORD)
                high = Immediate(10, IRType.WORD)

            # Initialize loop variable
            if stmt.iteration_scheme.iterator.is_reverse:
                self.builder.mov(loop_var, high, comment=f"init {iterator.name}")
            else:
                self.builder.mov(loop_var, low, comment=f"init {iterator.name}")

            # Store bounds in temp registers
            low_vreg = self.builder.new_vreg(IRType.WORD, "_low")
            high_vreg = self.builder.new_vreg(IRType.WORD, "_high")
            self.builder.mov(low_vreg, low)
            self.builder.mov(high_vreg, high)

            cond_label = self._new_label("for_cond")
            body_label = self._new_label("for_body")
            inc_label = self._new_label("for_inc")

            self.builder.jmp(Label(cond_label))

            # Condition check
            cond_block = self.builder.new_block(cond_label)
            self.builder.set_block(cond_block)

            cond = self.builder.new_vreg(IRType.BOOL, "_cond")
            if stmt.iteration_scheme.iterator.is_reverse:
                self.builder.cmp_ge(cond, loop_var, low_vreg)
            else:
                self.builder.cmp_le(cond, loop_var, high_vreg)
            self.builder.jz(cond, Label(end_label))

            # Body
            body_block = self.builder.new_block(body_label)
            self.builder.set_block(body_block)

            for s in stmt.statements:
                self._lower_statement(s)

            # Increment/decrement
            inc_block = self.builder.new_block(inc_label)
            self.builder.set_block(inc_block)

            one = Immediate(1, IRType.WORD)
            if stmt.iteration_scheme.iterator.is_reverse:
                self.builder.sub(loop_var, loop_var, one)
            else:
                self.builder.add(loop_var, loop_var, one)

            self.builder.jmp(Label(cond_label))

        # End block
        end_block = self.builder.new_block(end_label)
        self.builder.set_block(end_block)

        # Clean up named loop label
        if stmt_label_lower:
            del self.ctx.loop_exit_labels[stmt_label_lower]

        self.ctx.loop_exit_label = old_exit

    def _lower_block(self, stmt: BlockStmt) -> None:
        """Lower a block statement."""
        if self.ctx is None:
            return

        # Process declarations
        for decl in stmt.declarations:
            self._lower_declaration(decl)

        # Check if we have exception handlers
        if stmt.handled_exception_handlers:
            self._lower_block_with_handlers(
                stmt.statements, stmt.handled_exception_handlers
            )
        else:
            # No handlers - just lower statements directly
            for s in stmt.statements:
                self._lower_statement(s)

    def _lower_block_with_handlers(
        self, statements: list[Stmt], handlers: list[ExceptionHandler]
    ) -> None:
        """Lower a block with exception handlers.

        Structure:
        1. Push handler frames for each handler
        2. Execute body statements
        3. Pop handler frames (normal exit)
        4. Jump past handlers
        5. Handler code (jumped to by raise)
        """
        if self.ctx is None:
            return

        # Generate labels
        end_label = self._new_label("block_end")
        handler_labels = [
            self._new_label(f"handler_{i}") for i in range(len(handlers))
        ]

        # Push exception handlers (in reverse order so first handler is checked first)
        # Count total pushes for proper cleanup
        total_pushes = 0

        for i, handler in reversed(list(enumerate(handlers))):
            handler_label = handler_labels[i]

            # Determine exception ID(s) for this handler
            if not handler.exception_names:
                # "when others =>" catches all
                self.builder.emit(IRInstr(
                    OpCode.EXC_PUSH,
                    dst=Label(handler_label),
                    src1=Immediate(0, IRType.WORD),  # 0 = catch all
                ))
                total_pushes += 1
            else:
                # Support multiple exception names: when E1 | E2 | E3 =>
                # Push a handler for each exception name (they all jump to same handler)
                for exc_name in handler.exception_names:
                    if isinstance(exc_name, Identifier):
                        if exc_name.name.lower() == "others":
                            exc_id = 0  # catch all
                        else:
                            exc_id = self._get_exception_id(exc_name.name)
                    else:
                        exc_id = 0

                    self.builder.emit(IRInstr(
                        OpCode.EXC_PUSH,
                        dst=Label(handler_label),
                        src1=Immediate(exc_id, IRType.WORD),
                    ))
                    total_pushes += 1

        # Track handler count for this block (total pushes for proper cleanup)
        handler_count = total_pushes
        self.ctx.exception_handler_stack.append((handler_count, end_label))

        # Execute body statements
        for s in statements:
            self._lower_statement(s)

        # Pop exception handlers (normal exit)
        for _ in range(handler_count):
            self.builder.emit(IRInstr(OpCode.EXC_POP))

        # Jump past handler code
        self.builder.jmp(Label(end_label))

        # Generate handler code
        for i, handler in enumerate(handlers):
            handler_block = self.builder.new_block(handler_labels[i])
            self.builder.set_block(handler_block)

            # Execute handler statements
            for s in handler.statements:
                self._lower_statement(s)

            # Jump to end (after handler executes)
            self.builder.jmp(Label(end_label))

        # Pop from handler stack
        self.ctx.exception_handler_stack.pop()

        # End block
        end_block = self.builder.new_block(end_label)
        self.builder.set_block(end_block)

    def _lower_exit(self, stmt: ExitStmt) -> None:
        """Lower an exit statement."""
        if self.ctx is None or self.ctx.loop_exit_label is None:
            return

        # Determine which loop to exit (named or innermost)
        exit_label = self.ctx.loop_exit_label  # Default to innermost
        if stmt.loop_label:
            label_lower = stmt.loop_label.lower()
            if label_lower in self.ctx.loop_exit_labels:
                exit_label = self.ctx.loop_exit_labels[label_lower]

        if stmt.condition:
            # exit when condition
            cond = self._lower_expr(stmt.condition)
            self.builder.jnz(cond, Label(exit_label))
        else:
            # unconditional exit
            self.builder.jmp(Label(exit_label))

    def _lower_return(self, stmt: ReturnStmt) -> None:
        """Lower a return statement."""
        # Finalize all controlled objects before returning
        self._generate_finalizations()

        if stmt.value:
            value = self._lower_expr(stmt.value)
            self.builder.ret(value)
        else:
            self.builder.ret()

    def _lower_procedure_call(self, stmt: ProcedureCallStmt) -> None:
        """Lower a procedure call."""
        proc_name = ""
        if isinstance(stmt.name, Identifier):
            proc_name = stmt.name.name.lower()
        elif isinstance(stmt.name, SelectedName):
            # Handle Ada.Text_IO.Put_Line etc.
            proc_name = stmt.name.selector.lower()

        # Check for Text_IO built-in procedures
        if proc_name in ("put", "put_line", "new_line", "get", "get_line"):
            self._lower_text_io_call(proc_name, stmt.args)
            return

        # Check if this is a Free (Unchecked_Deallocation instantiation)
        if self._is_deallocation_call(proc_name):
            self._lower_deallocation_call(stmt.args)
            return

        if isinstance(stmt.name, Identifier):
            # Resolve overloaded procedure
            sym = self._resolve_overload(stmt.name.name, stmt.args)

            # Determine the call target - use external name if imported
            call_target = stmt.name.name
            if sym:
                if sym.is_imported and sym.external_name:
                    call_target = sym.external_name
                else:
                    call_target = sym.name

            # Build effective arguments list including defaults for missing parameters
            effective_args = self._build_effective_args(stmt.args, sym)

            # Check if this is a dispatching call
            is_dispatching = self._is_dispatching_call(sym, stmt.args)

            # Push arguments in reverse order
            for arg in reversed(effective_args):
                value = self._lower_expr(arg)
                self.builder.push(value)

            if is_dispatching and sym and sym.vtable_slot >= 0:
                # Dispatching call - emit DISPATCH instruction
                # First argument is the controlling operand (object pointer)
                first_arg = stmt.args[0].value if stmt.args else None
                if first_arg:
                    obj_ptr = self._lower_expr(first_arg)
                    self.builder.emit(IRInstr(
                        OpCode.DISPATCH,
                        src1=obj_ptr,
                        src2=Immediate(sym.vtable_slot, IRType.WORD),
                        comment=f"dispatch {sym.name}"
                    ))
            else:
                # Static call (using external name for imported procedures)
                self.builder.call(Label(call_target))

            # Clean up stack
            if effective_args:
                # Pop arguments (2 bytes each)
                for _ in effective_args:
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)

    def _is_deallocation_call(self, proc_name: str) -> bool:
        """Check if a procedure is an Unchecked_Deallocation instantiation.

        Common naming conventions are Free, Deallocate, or any name chosen
        when instantiating Ada.Unchecked_Deallocation.
        """
        # Look up the symbol to check if it's a deallocation procedure
        sym = self.symbols.lookup(proc_name)
        if sym:
            # Check if marked as a deallocation procedure
            if hasattr(sym, 'is_deallocation') and sym.is_deallocation:
                return True

        # Also check for common names used for Free
        if proc_name in ("free", "deallocate", "release"):
            return True

        return False

    def _lower_deallocation_call(self, args: list) -> None:
        """Lower a call to a deallocation procedure (Free).

        This:
        1. Gets the pointer from the argument
        2. Calls the heap free routine
        3. Sets the pointer to null
        """
        if not args or self.ctx is None:
            return

        # Get the argument (which is the access value to free)
        first_arg = args[0].value if args else None
        if not first_arg:
            return

        # Lower the argument to get the pointer value
        ptr_value = self._lower_expr(first_arg)

        # Call _heap_free with the pointer
        self.builder.push(ptr_value)
        self.builder.call(Label("_heap_free"))
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

        # Set the access variable to null (for out parameter semantics)
        if isinstance(first_arg, Identifier):
            name = first_arg.name.lower()
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                self.builder.mov(local.vreg, Immediate(0, IRType.WORD),
                                comment=f"set {name} to null after free")
            elif name in self.ctx.params:
                param = self.ctx.params[name]
                self.builder.mov(param, Immediate(0, IRType.WORD),
                                comment=f"set {name} to null after free")

    def _is_dispatching_call(self, sym: Optional[Symbol], args: list) -> bool:
        """Check if a call should be dispatching (dynamic dispatch through vtable).

        A call is dispatching if:
        1. The called subprogram is a primitive of a tagged type
        2. The first argument is class-wide (T'Class)
        """
        if not sym or sym.vtable_slot < 0:
            return False

        if not args:
            return False

        # Check if first argument is class-wide
        first_arg = args[0].value if args[0].value else None
        if first_arg:
            arg_type = self._get_expr_type(first_arg)
            if arg_type:
                from uada80.type_system import RecordType
                if isinstance(arg_type, RecordType) and arg_type.is_class_wide:
                    return True

        return False

    def _build_effective_args(self, provided_args: list, sym: Optional[Symbol]) -> list:
        """Build effective argument list with defaults for missing parameters.

        Returns a list of expressions to use as arguments.
        """
        if not sym or not sym.parameters:
            # No symbol info - just use provided args
            return [arg.value for arg in provided_args if arg.value]

        # Build list of effective argument expressions
        effective_args = []

        # Map provided args by name for named parameter associations
        provided_by_name: dict[str, Any] = {}
        provided_by_pos: dict[int, Any] = {}

        for i, arg in enumerate(provided_args):
            if arg.value:
                if hasattr(arg, 'name') and arg.name:
                    # Named association: name => value
                    provided_by_name[arg.name.lower()] = arg.value
                else:
                    # Positional association
                    provided_by_pos[i] = arg.value

        # Process each parameter
        for param_index, param in enumerate(sym.parameters):
            param_name = param.name.lower() if param.name else ""

            # Check for named argument first
            if param_name in provided_by_name:
                effective_args.append(provided_by_name[param_name])
            elif param_index in provided_by_pos:
                # Use positional argument
                effective_args.append(provided_by_pos[param_index])
            elif param.default_value is not None:
                # Use the parameter's default value expression
                effective_args.append(param.default_value)
            else:
                # No default and no argument - use 0 as fallback
                # This should be caught by semantic analysis
                effective_args.append(IntegerLiteral(0))

        return effective_args

    def _lower_text_io_call(self, proc_name: str, args: list) -> None:
        """Lower a Text_IO procedure call to runtime calls."""
        if proc_name == "put":
            if args and args[0].value:
                # Determine if it's a string literal or expression
                arg_expr = args[0].value
                if isinstance(arg_expr, StringLiteral):
                    # Create string constant and print it
                    label = self.builder.new_string_label()
                    if self.builder.module:
                        # Add $ terminator for CP/M BDOS function 9
                        self.builder.module.add_string(label, arg_expr.value)
                    # Call _put_string runtime
                    self.builder.emit(IRInstr(
                        OpCode.MOV,
                        dst=self.builder.new_vreg(IRType.PTR, "_str"),
                        src1=Label(label),
                    ))
                    self.builder.call(Label("_put_string"))
                else:
                    # Expression - evaluate and print
                    value = self._lower_expr(arg_expr)
                    self.builder.push(value)
                    self.builder.call(Label("_put_int"))
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)

        elif proc_name == "put_line":
            if args and args[0].value:
                arg_expr = args[0].value
                if isinstance(arg_expr, StringLiteral):
                    # Create string constant and print it with newline
                    label = self.builder.new_string_label()
                    if self.builder.module:
                        self.builder.module.add_string(label, arg_expr.value)
                    # Load string address and call _put_line
                    str_reg = self.builder.new_vreg(IRType.PTR, "_str")
                    self.builder.mov(str_reg, Label(label))
                    self.builder.push(str_reg)
                    self.builder.call(Label("_put_line"))
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                else:
                    # Expression - evaluate, convert to string, print with newline
                    value = self._lower_expr(arg_expr)
                    self.builder.push(value)
                    self.builder.call(Label("_put_int_line"))
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
            else:
                # Just print newline
                self.builder.call(Label("_new_line"))

        elif proc_name == "new_line":
            self.builder.call(Label("_new_line"))

        elif proc_name == "get":
            # Get single character or integer into output parameter
            if args and args[0].value:
                arg_expr = args[0].value

                # Determine type of output parameter
                # For Character: call _get_char
                # For Integer: call _get_int
                # Default to character for now
                if self._is_integer_type(arg_expr):
                    self.builder.call(Label("_get_int"))
                else:
                    self.builder.call(Label("_get_char"))

                # Result is in HL, store to output parameter
                result = MemoryLocation(
                    is_global=False,
                    symbol_name="_HL",
                    ir_type=IRType.WORD,
                )
                self._store_to_target(arg_expr, result)

        elif proc_name == "get_line":
            # Get line into output string parameter
            if args and args[0].value:
                arg_expr = args[0].value

                # Get address of destination buffer
                if isinstance(arg_expr, Identifier):
                    name = arg_expr.name.lower()
                    if self.ctx and name in self.ctx.locals:
                        local = self.ctx.locals[name]
                        # Push buffer address
                        addr_reg = self.builder.new_vreg(IRType.PTR, "_buf")
                        self.builder.lea(addr_reg, local.vreg)
                        self.builder.push(addr_reg)
                        # Push max length (from type bounds if available)
                        max_len = self._get_string_max_length(arg_expr)
                        self.builder.push(Immediate(max_len, IRType.WORD))
                        # Call runtime to read line
                        self.builder.call(Label("_get_line"))
                        # Clean up stack
                        temp = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(temp)
                        self.builder.pop(temp)

    def _lower_case(self, stmt: CaseStmt) -> None:
        """Lower a case statement."""
        if self.ctx is None:
            return

        expr = self._lower_expr(stmt.expr)
        end_label = self._new_label("endcase")

        for i, alt in enumerate(stmt.alternatives):
            alt_label = self._new_label(f"case_{i}")
            next_label = self._new_label(f"case_{i}_next")

            # Check if this is "others" - handled differently
            has_others = any(isinstance(c, OthersChoice) for c in alt.choices)
            if has_others:
                # "others" is a catch-all - just emit the body
                alt_block = self.builder.new_block(alt_label)
                self.builder.set_block(alt_block)
                for s in alt.statements:
                    self._lower_statement(s)
                self.builder.jmp(Label(end_label))
                # Next block for following alternatives (if any)
                next_block = self.builder.new_block(next_label)
                self.builder.set_block(next_block)
                continue

            # Generate comparisons for each choice
            # Jump to alt_label if any choice matches
            for j, choice in enumerate(alt.choices):
                if isinstance(choice, RangeChoice):
                    # Range choice: low .. high
                    low_val = self._lower_expr(choice.range_expr.low)
                    high_val = self._lower_expr(choice.range_expr.high)
                    skip_label = self._new_label(f"case_{i}_skip_{j}")

                    # Check expr >= low: not (expr < low)
                    lt_result = self.builder.new_vreg(IRType.WORD, "_cmp_lt")
                    self.builder.cmp_lt(lt_result, expr, low_val)
                    self.builder.jnz(lt_result, Label(skip_label))  # expr < low, skip

                    # Check expr <= high: not (expr > high)
                    gt_result = self.builder.new_vreg(IRType.WORD, "_cmp_gt")
                    self.builder.cmp_gt(gt_result, expr, high_val)
                    self.builder.jz(gt_result, Label(alt_label))  # expr <= high, match

                    # Not in range
                    skip_block = self.builder.new_block(skip_label)
                    self.builder.set_block(skip_block)

                elif isinstance(choice, ExprChoice):
                    # Expression choice: single value
                    choice_val = self._lower_expr(choice.expr)
                    eq_result = self.builder.new_vreg(IRType.WORD, "_cmp_eq")
                    self.builder.cmp_eq(eq_result, expr, choice_val)
                    self.builder.jnz(eq_result, Label(alt_label))  # Equal - go to body

            # No choice matched, try next alternative
            self.builder.jmp(Label(next_label))

            # Alternative body
            alt_block = self.builder.new_block(alt_label)
            self.builder.set_block(alt_block)

            for s in alt.statements:
                self._lower_statement(s)

            self.builder.jmp(Label(end_label))

            # Next check
            next_block = self.builder.new_block(next_label)
            self.builder.set_block(next_block)

        # End
        end_block = self.builder.new_block(end_label)
        self.builder.set_block(end_block)

    def _lower_indexed_store(self, target: IndexedComponent, value) -> None:
        """Lower an indexed component store (array assignment)."""
        if self.ctx is None:
            return

        # Get array base address
        base_addr = self._get_array_base(target.prefix)
        if base_addr is None:
            return

        # Calculate element address
        elem_addr = self._calc_element_addr(target, base_addr)

        # Store value to element address
        self.builder.store(elem_addr, value)

    def _get_array_base(self, prefix: Expr) -> Optional[VReg]:
        """Get the base address of an array."""
        if self.ctx is None:
            return None

        if isinstance(prefix, Identifier):
            name = prefix.name.lower()

            # Check locals
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                # Get address of local array
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                # LEA: load effective address (IX + offset)
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr,
                    src1=MemoryLocation(offset=local.stack_offset, ir_type=IRType.PTR),
                ))
                return addr

            # Check for global arrays
            sym = self.symbols.lookup(name)
            if sym and sym.kind == SymbolKind.VARIABLE:
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr,
                    src1=MemoryLocation(is_global=True, symbol_name=name, ir_type=IRType.PTR),
                ))
                return addr

        return None

    def _calc_element_addr(
        self, indexed: IndexedComponent, base_addr: VReg, check_bounds: bool = True
    ) -> MemoryLocation:
        """Calculate the memory location for an array element.

        Supports multidimensional arrays using row-major order.
        For an array A(I, J, K):
          offset = ((I - I_lo) * J_size * K_size + (J - J_lo) * K_size + (K - K_lo)) * elem_size

        Args:
            indexed: The indexed component expression
            base_addr: Virtual register holding the base address
            check_bounds: If True, emit runtime bounds checks
        """
        # Get array type info
        bounds_list: list[tuple[int, int]] = []  # [(low, high), ...] for each dimension
        element_size = 2  # Default to word size

        if isinstance(indexed.prefix, Identifier):
            sym = self.symbols.lookup(indexed.prefix.name)
            if sym and sym.ada_type and hasattr(sym.ada_type, 'bounds') and sym.ada_type.bounds:
                bounds_list = list(sym.ada_type.bounds)
            if sym and sym.ada_type and hasattr(sym.ada_type, 'component_type'):
                comp_type = sym.ada_type.component_type
                if comp_type:
                    element_size = (comp_type.size_bits + 7) // 8

        # Default bounds if not found
        if not bounds_list:
            bounds_list = [(1, 10)] * len(indexed.indices)  # Default 1..10 for each dim

        # Calculate sizes for each dimension (for row-major order)
        # dim_sizes[i] = product of all subsequent dimension sizes
        num_dims = len(indexed.indices)
        dim_sizes: list[int] = [1] * num_dims
        for i in range(num_dims - 2, -1, -1):
            if i + 1 < len(bounds_list):
                low, high = bounds_list[i + 1]
                dim_sizes[i] = dim_sizes[i + 1] * (high - low + 1)
            else:
                dim_sizes[i] = dim_sizes[i + 1] * 10  # Default size

        # Calculate total offset
        total_offset = self.builder.new_vreg(IRType.WORD, "_total_offset")
        self.builder.mov(total_offset, Immediate(0, IRType.WORD))

        for dim_idx, index_expr in enumerate(indexed.indices):
            # Lower the index expression
            index = self._lower_expr(index_expr)

            # Get bounds for this dimension
            if dim_idx < len(bounds_list):
                lower_bound, upper_bound = bounds_list[dim_idx]
            else:
                lower_bound, upper_bound = 1, 10

            # Emit bounds check if requested
            if check_bounds:
                self._emit_array_bounds_check(index, lower_bound, upper_bound)

            # Adjust for lower bound: adjusted_idx = index - lower_bound
            if lower_bound != 0:
                adjusted_idx = self.builder.new_vreg(IRType.WORD, f"_adj_idx{dim_idx}")
                self.builder.sub(adjusted_idx, index, Immediate(lower_bound, IRType.WORD))
                index = adjusted_idx

            # Multiply by dimension size (product of subsequent dimension extents)
            if dim_sizes[dim_idx] > 1:
                scaled_idx = self.builder.new_vreg(IRType.WORD, f"_scaled_idx{dim_idx}")
                if dim_sizes[dim_idx] == 2:
                    # Optimize: shift left by 1
                    self.builder.add(scaled_idx, index, index)
                else:
                    self.builder.mul(scaled_idx, index, Immediate(dim_sizes[dim_idx], IRType.WORD))
                index = scaled_idx

            # Add to total offset
            new_total = self.builder.new_vreg(IRType.WORD, f"_offset{dim_idx}")
            self.builder.add(new_total, total_offset, index)
            total_offset = new_total

        # Multiply total offset by element size
        if element_size != 1:
            byte_offset = self.builder.new_vreg(IRType.WORD, "_byte_offset")
            if element_size == 2:
                self.builder.add(byte_offset, total_offset, total_offset)
            else:
                self.builder.mul(byte_offset, total_offset, Immediate(element_size, IRType.WORD))
            total_offset = byte_offset

        # Add offset to base address
        elem_addr_vreg = self.builder.new_vreg(IRType.PTR, "_elem_addr")
        self.builder.add(elem_addr_vreg, base_addr, total_offset)

        # Return as memory location using the computed address
        return MemoryLocation(base=elem_addr_vreg, offset=0, ir_type=IRType.WORD)

    def _lower_selected_store(self, target: SelectedName, value) -> None:
        """Lower a selected component store (record field assignment or pointer dereference)."""
        if self.ctx is None:
            return

        # Handle .all dereference store (Ptr.all := value)
        if target.selector.lower() == "all":
            ptr = self._lower_expr(target.prefix)
            mem = MemoryLocation(base=ptr, offset=0, ir_type=IRType.WORD)
            self.builder.store(mem, value)
            return

        # Check if prefix is an access type (implicit dereference for Ptr.Field)
        prefix_type = self._get_prefix_type(target.prefix)
        if prefix_type and isinstance(prefix_type, AccessType):
            ptr = self._lower_expr(target.prefix)
            field_offset = self._get_field_offset_for_type(
                prefix_type.designated_type, target.selector
            )
            if field_offset != 0:
                field_addr = self.builder.new_vreg(IRType.PTR, "_field_addr")
                self.builder.add(field_addr, ptr, Immediate(field_offset, IRType.WORD))
            else:
                field_addr = ptr

            mem = MemoryLocation(base=field_addr, offset=0, ir_type=IRType.WORD)
            self.builder.store(mem, value)
            return

        # Get record base address
        base_addr = self._get_record_base(target.prefix)
        if base_addr is None:
            return

        # Get field offset
        field_offset = self._get_field_offset(target)
        field_size = self._get_field_size(target)

        # Calculate field address
        if field_offset != 0:
            field_addr = self.builder.new_vreg(IRType.PTR, "_field_addr")
            self.builder.add(field_addr, base_addr, Immediate(field_offset, IRType.WORD))
        else:
            field_addr = base_addr

        # Store value to field
        mem = MemoryLocation(base=field_addr, offset=0, ir_type=IRType.WORD)
        self.builder.store(mem, value)

    def _get_record_base(self, prefix: Expr) -> Optional[VReg]:
        """Get the base address of a record."""
        if self.ctx is None:
            return None

        if isinstance(prefix, Identifier):
            name = prefix.name.lower()

            # Check locals
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr,
                    src1=MemoryLocation(offset=local.stack_offset, ir_type=IRType.PTR),
                ))
                return addr

            # Check for global records
            sym = self.symbols.lookup(name)
            if sym and sym.kind == SymbolKind.VARIABLE:
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr,
                    src1=MemoryLocation(is_global=True, symbol_name=name, ir_type=IRType.PTR),
                ))
                return addr

        return None

    def _get_field_offset(self, selected: SelectedName) -> int:
        """Get the byte offset of a record field."""
        from uada80.type_system import RecordType

        if isinstance(selected.prefix, Identifier):
            sym = self.symbols.lookup(selected.prefix.name)
            if sym and sym.ada_type and isinstance(sym.ada_type, RecordType):
                for comp in sym.ada_type.components:
                    if comp.name.lower() == selected.selector.lower():
                        return comp.offset_bits // 8
        return 0

    def _get_field_size(self, selected: SelectedName) -> int:
        """Get the byte size of a record field."""
        from uada80.type_system import RecordType

        if isinstance(selected.prefix, Identifier):
            sym = self.symbols.lookup(selected.prefix.name)
            if sym and sym.ada_type and isinstance(sym.ada_type, RecordType):
                for comp in sym.ada_type.components:
                    if comp.name.lower() == selected.selector.lower():
                        return (comp.component_type.size_bits + 7) // 8
        return 2  # Default to word size

    def _lower_selected(self, expr: SelectedName):
        """Lower a selected component (record field access or pointer dereference)."""
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Handle .all dereference
        if expr.selector.lower() == "all":
            ptr = self._lower_expr(expr.prefix)
            result = self.builder.new_vreg(IRType.WORD, "_deref")
            mem = MemoryLocation(base=ptr, offset=0, ir_type=IRType.WORD)
            self.builder.load(result, mem)
            return result

        # Check if prefix is an access type (implicit dereference for Ptr.Field)
        prefix_type = self._get_prefix_type(expr.prefix)
        if prefix_type and isinstance(prefix_type, AccessType):
            # Dereference the pointer first, then access the field
            ptr = self._lower_expr(expr.prefix)
            field_offset = self._get_field_offset_for_type(
                prefix_type.designated_type, expr.selector
            )
            # Check discriminant for variant fields if needed
            self._check_variant_discriminant(prefix_type.designated_type, expr.selector, ptr)
            if field_offset != 0:
                field_addr = self.builder.new_vreg(IRType.PTR, "_field_addr")
                self.builder.add(field_addr, ptr, Immediate(field_offset, IRType.WORD))
            else:
                field_addr = ptr

            result = self.builder.new_vreg(IRType.WORD, "_field")
            mem = MemoryLocation(base=field_addr, offset=0, ir_type=IRType.WORD)
            self.builder.load(result, mem)
            return result

        # Get record base address
        base_addr = self._get_record_base(expr.prefix)
        if base_addr is None:
            return Immediate(0, IRType.WORD)

        # Check discriminant for variant fields if needed
        if prefix_type:
            self._check_variant_discriminant(prefix_type, expr.selector, base_addr)

        # Get field offset
        field_offset = self._get_field_offset(expr)

        # Calculate field address
        if field_offset != 0:
            field_addr = self.builder.new_vreg(IRType.PTR, "_field_addr")
            self.builder.add(field_addr, base_addr, Immediate(field_offset, IRType.WORD))
        else:
            field_addr = base_addr

        # Load value from field
        result = self.builder.new_vreg(IRType.WORD, "_field")
        mem = MemoryLocation(base=field_addr, offset=0, ir_type=IRType.WORD)
        self.builder.load(result, mem)

        return result

    def _check_variant_discriminant(self, record_type, field_name: str, base_ptr) -> None:
        """Check that the discriminant matches the variant containing the field.

        Raises Constraint_Error if accessing a variant field with wrong discriminant.
        """
        if self.ctx is None:
            return

        if not isinstance(record_type, RecordType):
            return
        if not record_type.variant_part:
            return

        # Check if field_name is in a variant (not common components)
        for variant in record_type.variant_part.variants:
            for comp in variant.components:
                if comp.name.lower() == field_name.lower():
                    # Found it - need to check discriminant matches one of variant's choices
                    disc_name = record_type.variant_part.discriminant_name
                    disc_comp = record_type.get_discriminant(disc_name)
                    if disc_comp is None:
                        return

                    # Get the discriminant value from the record
                    disc_offset = disc_comp.offset_bits // 8
                    disc_addr = self.builder.new_vreg(IRType.PTR, "_disc_addr")
                    if disc_offset != 0:
                        self.builder.add(disc_addr, base_ptr, Immediate(disc_offset, IRType.WORD))
                    else:
                        disc_addr = base_ptr
                    disc_val = self.builder.new_vreg(IRType.WORD, "_disc_val")
                    self.builder.load(disc_val, MemoryLocation(base=disc_addr, offset=0, ir_type=IRType.WORD))

                    # Check if discriminant matches any of the variant's choices
                    # For simplicity, handle single value choices
                    match_found = self.builder.new_vreg(IRType.BOOL, "_match")
                    self.builder.mov(match_found, Immediate(0, IRType.WORD))

                    for choice in variant.choices:
                        if isinstance(choice, int):
                            # Simple value choice
                            is_match = self.builder.new_vreg(IRType.BOOL, "_choice_match")
                            self.builder.cmp_eq(is_match, disc_val, Immediate(choice, IRType.WORD))
                            # match_found = match_found OR is_match
                            self.builder.or_(match_found, match_found, is_match)

                    # If no match, raise Constraint_Error
                    self.builder.jz(match_found, Label("_raise_constraint_error"))
                    return

    def _get_prefix_type(self, expr: Expr):
        """Get the Ada type of an expression."""
        if isinstance(expr, Identifier):
            sym = self.symbols.lookup(expr.name)
            if sym:
                return sym.ada_type
        return None

    def _get_field_offset_for_type(self, record_type, field_name: str) -> int:
        """Get the byte offset of a field in a record type."""
        if not isinstance(record_type, RecordType):
            return 0
        offset = 0
        for comp in record_type.components:
            if comp.name.lower() == field_name.lower():
                return offset
            offset += (comp.component_type.size_bits + 7) // 8
        return 0

    # =========================================================================
    # Expressions
    # =========================================================================

    def _lower_expr(self, expr: Expr):
        """Lower an expression and return the result vreg or immediate."""
        if isinstance(expr, IntegerLiteral):
            return Immediate(expr.value, IRType.WORD)

        if isinstance(expr, RealLiteral):
            # Convert floating point to 16.16 fixed point representation
            # Format: 16 bits integer, 16 bits fraction
            # Range: -32768.0 to 32767.99998 with precision ~0.000015
            return self._lower_fixed_point_literal(expr.value)

        if isinstance(expr, CharacterLiteral):
            return Immediate(ord(expr.value), IRType.BYTE)

        if isinstance(expr, StringLiteral):
            return self._lower_string_literal(expr)

        if isinstance(expr, Identifier):
            return self._lower_identifier(expr)

        if isinstance(expr, BinaryExpr):
            return self._lower_binary(expr)

        if isinstance(expr, UnaryExpr):
            return self._lower_unary(expr)

        if isinstance(expr, FunctionCall):
            return self._lower_function_call(expr)

        if isinstance(expr, RangeExpr):
            # For ranges used as values, return the low bound
            return self._lower_expr(expr.low)

        if isinstance(expr, AttributeReference):
            return self._lower_attribute(expr)

        if isinstance(expr, IndexedComponent):
            return self._lower_indexed(expr)

        if isinstance(expr, SelectedName):
            return self._lower_selected(expr)

        if isinstance(expr, NullLiteral):
            # null is represented as 0 (null pointer)
            return Immediate(0, IRType.PTR)

        if isinstance(expr, Allocator):
            return self._lower_allocator(expr)

        if isinstance(expr, Dereference):
            return self._lower_dereference(expr)

        if isinstance(expr, Aggregate):
            return self._lower_aggregate(expr)

        if isinstance(expr, Slice):
            return self._lower_slice(expr)

        if isinstance(expr, ConditionalExpr):
            return self._lower_conditional_expr(expr)

        if isinstance(expr, QuantifiedExpr):
            return self._lower_quantified_expr(expr)

        if isinstance(expr, TypeConversion):
            return self._lower_type_conversion(expr)

        if isinstance(expr, QualifiedExpr):
            return self._lower_qualified_expr(expr)

        if isinstance(expr, MembershipTest):
            return self._lower_membership_test(expr)

        if isinstance(expr, CaseExpr):
            return self._lower_case_expr(expr)

        if isinstance(expr, RaiseExpr):
            return self._lower_raise_expr(expr)

        if isinstance(expr, DeclareExpr):
            return self._lower_declare_expr(expr)

        if isinstance(expr, DeltaAggregate):
            return self._lower_delta_aggregate(expr)

        if isinstance(expr, ContainerAggregate):
            return self._lower_container_aggregate(expr)

        if isinstance(expr, Parenthesized):
            # Parenthesized expression: just lower the inner expression
            return self._lower_expr(expr.expr)

        if isinstance(expr, TargetName):
            return self._lower_target_name(expr)

        # Default: return 0
        return Immediate(0, IRType.WORD)

    def _lower_allocator(self, expr: Allocator):
        """Lower an allocator (new Type) expression."""
        if self.ctx is None:
            return Immediate(0, IRType.PTR)

        # Determine size to allocate
        designated_type = self._resolve_type(expr.type_mark)
        if designated_type:
            size = designated_type.size_bytes()
        else:
            size = 2  # Default to word size

        # Allocate heap memory using calling convention:
        # 1. Push size argument
        # 2. Call _heap_alloc
        # 3. Result returned in designated vreg (convention: function returns in first vreg)
        # 4. Clean up stack

        size_val = Immediate(size, IRType.WORD)
        self.builder.push(size_val, comment=f"alloc size {size}")

        # Call heap allocator
        self.builder.call(Label("_heap_alloc"), comment="allocate heap memory")

        # Clean up stack (pop the size argument)
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

        # Result is returned in HL (Z80 convention), capture it in a vreg
        result = self.builder.new_vreg(IRType.PTR, "_alloc_ptr")
        # Use a special marker that codegen can recognize to capture HL
        self.builder.emit(IRInstr(
            OpCode.MOV, result,
            MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
            comment="capture heap result from HL"
        ))

        # If there's an initial value, store it
        if expr.initial_value:
            init_val = self._lower_expr(expr.initial_value)
            mem = MemoryLocation(base=result, offset=0, ir_type=IRType.WORD)
            self.builder.store(mem, init_val)

        return result

    def _lower_dereference(self, expr: Dereference):
        """Lower a dereference expression (P.all)."""
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Lower the prefix to get the pointer value
        ptr = self._lower_expr(expr.prefix)

        # Null pointer check - raise Constraint_Error if ptr is null
        self._emit_null_check(ptr, "dereference null pointer")

        # Load the value at the pointer address
        result = self.builder.new_vreg(IRType.WORD, "_deref")
        mem = MemoryLocation(base=ptr, offset=0, ir_type=IRType.WORD)
        self.builder.load(result, mem, comment="dereference .all")

        return result

    def _emit_null_check(self, ptr, comment: str = "") -> None:
        """Emit runtime check that pointer is not null.

        Raises Constraint_Error if pointer is null (0).
        """
        if self.ctx is None:
            return

        # Check if ptr == 0 (null)
        is_null = self.builder.new_vreg(IRType.BOOL, "_is_null")
        self.builder.cmp_eq(is_null, ptr, Immediate(0, IRType.WORD))
        # Jump to raise if null
        self.builder.jnz(is_null, Label("_raise_constraint_error"))

    def _emit_range_check(self, value, target_type, comment: str = "") -> None:
        """Emit runtime range check for type conversion or assignment.

        Raises Constraint_Error if value is out of range for the target type.
        This implements Ada's range checking semantics.
        """
        if self.ctx is None:
            return

        # Get the bounds of the target type
        low_bound = None
        high_bound = None

        if hasattr(target_type, 'low') and hasattr(target_type, 'high'):
            low_bound = target_type.low
            high_bound = target_type.high
        elif hasattr(target_type, 'first') and hasattr(target_type, 'last'):
            low_bound = target_type.first
            high_bound = target_type.last

        if low_bound is None or high_bound is None:
            return  # No bounds to check

        # Skip check if target can hold all possible values (full word range)
        if isinstance(low_bound, int) and isinstance(high_bound, int):
            if low_bound <= -32768 and high_bound >= 32767:
                return  # Full 16-bit range, no check needed

        # Emit the range check
        # Check value >= low_bound
        if isinstance(low_bound, int):
            too_low = self.builder.new_vreg(IRType.BOOL, "_too_low")
            self.builder.cmp_lt(too_low, value, Immediate(low_bound, IRType.WORD))
            self.builder.jnz(too_low, Label("_raise_constraint_error"))

        # Check value <= high_bound
        if isinstance(high_bound, int):
            too_high = self.builder.new_vreg(IRType.BOOL, "_too_high")
            self.builder.cmp_gt(too_high, value, Immediate(high_bound, IRType.WORD))
            self.builder.jnz(too_high, Label("_raise_constraint_error"))

    def _lower_conditional_expr(self, expr: ConditionalExpr):
        """Lower a conditional expression (if Cond then A else B).

        Conditional expressions are lowered like ternary operators,
        using phi-node style control flow.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Result vreg that will hold the final value
        result = self.builder.new_vreg(IRType.WORD, "_cond_result")

        # Create labels for control flow
        then_label = self.builder.new_label("cond_then")
        else_label = self.builder.new_label("cond_else")
        end_label = self.builder.new_label("cond_end")

        # Evaluate condition
        cond_val = self._lower_expr(expr.condition)
        self.builder.cmp(cond_val, Immediate(0, IRType.WORD))
        self.builder.jz(else_label, comment="if false, goto else")

        # Then branch
        self.builder.label(then_label.name)
        then_val = self._lower_expr(expr.then_expr)
        self.builder.mov(result, then_val)
        self.builder.jmp(end_label)

        # Handle elsif parts
        for i, (elsif_cond, elsif_expr) in enumerate(expr.elsif_parts):
            elsif_then_label = self.builder.new_label(f"elsif_then_{i}")
            next_label = self.builder.new_label(f"elsif_next_{i}")

            self.builder.label(elsif_then_label.name)
            elsif_cond_val = self._lower_expr(elsif_cond)
            self.builder.cmp(elsif_cond_val, Immediate(0, IRType.WORD))
            self.builder.jz(next_label)

            elsif_val = self._lower_expr(elsif_expr)
            self.builder.mov(result, elsif_val)
            self.builder.jmp(end_label)

            # Replace else_label target for next iteration
            else_label = next_label

        # Else branch (or final else)
        self.builder.label(else_label.name)
        if expr.else_expr:
            else_val = self._lower_expr(expr.else_expr)
            self.builder.mov(result, else_val)
        else:
            # No else - result is undefined (shouldn't happen in valid Ada 2012)
            self.builder.mov(result, Immediate(0, IRType.WORD))

        # End label
        self.builder.label(end_label.name)

        return result

    def _lower_quantified_expr(self, expr: QuantifiedExpr):
        """Lower a quantified expression (for all/some X in Range => Pred).

        for all returns True (1) if predicate holds for all values
        for some returns True (1) if predicate holds for at least one value
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Result vreg
        result = self.builder.new_vreg(IRType.WORD, "_quant_result")

        # Initialize result based on quantifier
        if expr.is_for_all:
            # for all: assume true, set false if any fails
            self.builder.mov(result, Immediate(1, IRType.WORD))
        else:
            # for some: assume false, set true if any succeeds
            self.builder.mov(result, Immediate(0, IRType.WORD))

        # Create loop variable
        loop_var_name = expr.iterator.name
        loop_var = self.builder.new_vreg(IRType.WORD, f"_quant_{loop_var_name}")

        # Get range bounds from the iterable
        if isinstance(expr.iterator.iterable, RangeExpr):
            low_val = self._lower_expr(expr.iterator.iterable.low)
            high_val = self._lower_expr(expr.iterator.iterable.high)
        else:
            # For non-range iterables, fall back to simple evaluation
            return self._lower_expr(expr.predicate)

        # Store low in loop var
        self.builder.mov(loop_var, low_val)

        # Create loop labels
        loop_start = self.builder.new_label("quant_loop")
        loop_body = self.builder.new_label("quant_body")
        loop_end = self.builder.new_label("quant_end")

        # Loop start - check if we're done
        self.builder.label(loop_start.name)
        self.builder.cmp(loop_var, high_val)
        self.builder.jg(loop_end, comment="exit if loop_var > high")

        # Loop body - evaluate predicate
        self.builder.label(loop_body.name)

        # Temporarily bind the loop variable in context for predicate evaluation
        # Save any existing local with the same name
        old_local = self.ctx.locals.get(loop_var_name.lower())

        # Register the loop variable as a local so the predicate can access it
        self.ctx.locals[loop_var_name.lower()] = LocalVariable(
            name=loop_var_name,
            vreg=loop_var,
            offset=0,
            size=2,
            ada_type=None
        )

        # Evaluate the predicate
        pred_val = self._lower_expr(expr.predicate)

        # Restore the old local (if any)
        if old_local is not None:
            self.ctx.locals[loop_var_name.lower()] = old_local
        else:
            del self.ctx.locals[loop_var_name.lower()]

        if expr.is_for_all:
            # for all: if predicate is false, set result to false and exit
            self.builder.cmp(pred_val, Immediate(0, IRType.WORD))
            early_exit = self.builder.new_label("quant_early_exit")
            self.builder.jnz(early_exit)  # predicate was true, continue
            self.builder.mov(result, Immediate(0, IRType.WORD))
            self.builder.jmp(loop_end)
            self.builder.label(early_exit.name)
        else:
            # for some: if predicate is true, set result to true and exit
            self.builder.cmp(pred_val, Immediate(0, IRType.WORD))
            continue_loop = self.builder.new_label("quant_continue")
            self.builder.jz(continue_loop)  # predicate was false, continue
            self.builder.mov(result, Immediate(1, IRType.WORD))
            self.builder.jmp(loop_end)
            self.builder.label(continue_loop.name)

        # Increment loop variable and continue
        temp = self.builder.new_vreg(IRType.WORD, "_inc_temp")
        self.builder.add(temp, loop_var, Immediate(1, IRType.WORD))
        self.builder.mov(loop_var, temp)
        self.builder.jmp(loop_start)

        # Loop end
        self.builder.label(loop_end.name)

        return result

    def _lower_type_conversion(self, expr: TypeConversion):
        """Lower a type conversion (Type(expr)).

        Type conversions in Ada can involve:
        - Numeric conversions (e.g., Float(X), Integer(Y))
        - Enumeration to/from integer
        - Array conversions between compatible types
        - For Z80, most conversions are simply evaluating the operand

        Range checking is performed for conversions to constrained types.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Get the target type
        target_type = self._resolve_type(expr.type_mark)

        # Evaluate the operand
        operand_val = self._lower_expr(expr.operand)

        # For most Z80 conversions, the representation is the same
        # Special cases that might need actual conversion:
        if target_type:
            target_kind = getattr(target_type, 'kind', None)
            if target_kind == TypeKind.INTEGER:
                # Integer conversion - emit range check for constrained subtypes
                self._emit_range_check(operand_val, target_type, "type conversion range check")
                return operand_val
            elif target_kind == TypeKind.FLOAT:
                # Float conversion - would need fixed-point conversion
                # For now, pass through
                return operand_val
            elif target_kind == TypeKind.ENUMERATION:
                # Enum to/from integer - check bounds
                self._emit_range_check(operand_val, target_type, "enum conversion range check")
                return operand_val
            elif target_kind == TypeKind.MODULAR:
                # Modular conversion - check bounds (0..modulus-1)
                self._emit_range_check(operand_val, target_type, "modular conversion range check")
                return operand_val

        # Default: pass through (most Ada type conversions are view conversions)
        return operand_val

    def _lower_qualified_expr(self, expr: QualifiedExpr):
        """Lower a qualified expression (Type'(expr)).

        Qualified expressions provide explicit type context for expressions,
        particularly useful for overload resolution and aggregates.
        The underlying value is the same as the expression.

        In Ada, qualified expressions perform a runtime check that the value
        is within the type's range (unlike type conversions which convert).
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Evaluate the inner expression
        result = self._lower_expr(expr.expr)

        # Get the qualifying type and emit range check
        target_type = self._resolve_type(expr.type_mark)
        if target_type:
            self._emit_range_check(result, target_type, "qualified expr range check")

        return result

    def _lower_membership_test(self, expr: MembershipTest):
        """Lower a membership test (X in Type, X in 1..10, X not in Choices).

        Returns 1 (True) if the expression is in the specified type/range,
        0 (False) otherwise. The is_not flag inverts the result.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Result vreg
        result = self.builder.new_vreg(IRType.WORD, "_member_result")
        self.builder.mov(result, Immediate(0, IRType.WORD))  # Default to False

        # Evaluate the expression being tested
        test_val = self._lower_expr(expr.expr)

        # Create labels for control flow
        match_label = self.builder.new_label("member_match")
        end_label = self.builder.new_label("member_end")

        # Check each choice
        for i, choice in enumerate(expr.choices):
            next_choice = self.builder.new_label(f"member_next_{i}")

            if isinstance(choice, RangeChoice):
                # Range test: low <= X <= high
                low_val = self._lower_expr(choice.range_expr.low)
                high_val = self._lower_expr(choice.range_expr.high)

                # Check X >= low
                self.builder.cmp(test_val, low_val)
                self.builder.jl(next_choice)  # X < low, try next choice

                # Check X <= high
                self.builder.cmp(test_val, high_val)
                self.builder.jg(next_choice)  # X > high, try next choice

                # X is in range
                self.builder.jmp(match_label)

            elif isinstance(choice, ExprChoice):
                # Check if this is a type reference (for type membership test)
                if isinstance(choice.expr, Identifier):
                    type_sym = self.symbols.lookup(choice.expr.name)
                    if type_sym and type_sym.kind in (SymbolKind.TYPE, SymbolKind.SUBTYPE):
                        # Type membership test - check against type bounds
                        type_info = type_sym.ada_type
                        if type_info and hasattr(type_info, 'first') and hasattr(type_info, 'last'):
                            # Check X >= Type'First
                            first_val = type_info.first
                            last_val = type_info.last
                            if isinstance(first_val, int) and isinstance(last_val, int):
                                self.builder.cmp(test_val, Immediate(first_val, IRType.WORD))
                                self.builder.jl(next_choice)  # X < First, try next
                                self.builder.cmp(test_val, Immediate(last_val, IRType.WORD))
                                self.builder.jg(next_choice)  # X > Last, try next
                                self.builder.jmp(match_label)
                                self.builder.label(next_choice.name)
                                continue
                        # Type without bounds - always matches
                        self.builder.jmp(match_label)
                        self.builder.label(next_choice.name)
                        continue

                # Single value test (equality)
                choice_val = self._lower_expr(choice.expr)
                self.builder.cmp(test_val, choice_val)
                self.builder.jz(match_label)  # X == choice

            elif isinstance(choice, OthersChoice):
                # Others always matches
                self.builder.jmp(match_label)

            self.builder.label(next_choice.name)

        # No match found - result stays 0
        self.builder.jmp(end_label)

        # Match found
        self.builder.label(match_label.name)
        self.builder.mov(result, Immediate(1, IRType.WORD))

        self.builder.label(end_label.name)

        # Handle NOT IN by inverting the result
        if expr.is_not:
            inverted = self.builder.new_vreg(IRType.WORD, "_member_inverted")
            self.builder.xor(inverted, result, Immediate(1, IRType.WORD))
            return inverted

        return result

    def _lower_case_expr(self, expr: CaseExpr):
        """Lower a case expression (Ada 2012 case expression).

        case X is
            when 1 => "one",
            when 2 => "two",
            when others => "other"
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Result vreg
        result = self.builder.new_vreg(IRType.WORD, "_case_result")

        # Evaluate the selector expression
        selector = self._lower_expr(expr.selector)

        # Create labels
        end_label = self.builder.new_label("case_end")

        # Process each alternative
        for i, alt in enumerate(expr.alternatives):
            next_alt = self.builder.new_label(f"case_next_{i}")
            match_label = self.builder.new_label(f"case_match_{i}")

            # Check each choice in this alternative
            for j, choice in enumerate(alt.choices):
                if isinstance(choice, OthersChoice):
                    # Others always matches - jump to expression evaluation
                    self.builder.jmp(match_label)
                elif isinstance(choice, RangeChoice):
                    # Range choice
                    low_val = self._lower_expr(choice.range_expr.low)
                    high_val = self._lower_expr(choice.range_expr.high)
                    next_choice = self.builder.new_label(f"case_next_choice_{i}_{j}")

                    self.builder.cmp(selector, low_val)
                    self.builder.jl(next_choice)
                    self.builder.cmp(selector, high_val)
                    self.builder.jg(next_choice)
                    self.builder.jmp(match_label)
                    self.builder.label(next_choice.name)
                elif isinstance(choice, ExprChoice):
                    # Single value choice
                    choice_val = self._lower_expr(choice.expr)
                    self.builder.cmp(selector, choice_val)
                    self.builder.jz(match_label)

            # No match in this alternative, try next
            self.builder.jmp(next_alt)

            # Match found - evaluate the result expression
            self.builder.label(match_label.name)
            alt_result = self._lower_expr(alt.result_expr)
            self.builder.mov(result, alt_result)
            self.builder.jmp(end_label)

            self.builder.label(next_alt.name)

        # Default case (shouldn't reach here in valid Ada)
        self.builder.mov(result, Immediate(0, IRType.WORD))

        self.builder.label(end_label.name)
        return result

    def _lower_raise_expr(self, expr: RaiseExpr):
        """Lower a raise expression (Ada 2012 raise in expression context).

        Example: (if X > 0 then X else raise Constraint_Error)
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Get the exception ID
        exc_name = self._get_exception_name(expr.exception_name)
        exc_id = self._get_exception_id(exc_name)

        # Store exception ID in global exception state
        self.builder.store(
            MemoryLocation(is_global=True, symbol_name="_current_exception", ir_type=IRType.WORD),
            Immediate(exc_id, IRType.WORD),
            comment=f"raise {exc_name}",
        )

        # If there's a message, handle it
        if expr.message:
            msg_val = self._lower_expr(expr.message)
            self.builder.store(
                MemoryLocation(is_global=True, symbol_name="_exception_msg", ir_type=IRType.WORD),
                msg_val,
                comment="exception message",
            )

        # Call the runtime exception handler
        self.builder.call(Label("_raise_exception"))

        # Return a dummy value (execution won't continue past raise)
        return Immediate(0, IRType.WORD)

    def _lower_string_literal(self, expr: StringLiteral):
        """Lower a string literal to a pointer to string data.

        Stores the string in the module's constant data section and
        returns a pointer to it.
        """
        if self.ctx is None:
            return Immediate(0, IRType.PTR)

        # Create a unique label for this string constant
        label = self.builder.new_string_label()

        # Add string to module's constant data
        if self.builder.module:
            self.builder.module.add_string(label, expr.value)

        # Return a pointer to the string (the label address)
        result = self.builder.new_vreg(IRType.PTR, "_str_ptr")
        self.builder.mov(result, Label(label), comment=f'string "{expr.value[:20]}..."' if len(expr.value) > 20 else f'string "{expr.value}"')
        return result

    def _lower_fixed_point_literal(self, value: float):
        """Lower a floating-point literal to 16.16 fixed-point.

        The 16.16 format stores:
        - High word (16 bits): integer part
        - Low word (16 bits): fractional part (as 65536ths)

        Range: -32768.0 to 32767.99998
        Precision: approximately 0.000015
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Convert to 16.16 fixed point (multiply by 65536)
        fixed_val = int(value * 65536)

        # Clamp to 32-bit signed range
        if fixed_val > 2147483647:
            fixed_val = 2147483647
        elif fixed_val < -2147483648:
            fixed_val = -2147483648

        # Split into high (integer) and low (fraction) words
        high_word = (fixed_val >> 16) & 0xFFFF
        low_word = fixed_val & 0xFFFF

        # Store in two virtual registers
        result_hi = self.builder.new_vreg(IRType.WORD, "_fixed_hi")
        result_lo = self.builder.new_vreg(IRType.WORD, "_fixed_lo")

        self.builder.mov(result_hi, Immediate(high_word, IRType.WORD),
                        comment=f"fixed {value:.4f} high word")
        self.builder.mov(result_lo, Immediate(low_word, IRType.WORD),
                        comment=f"fixed {value:.4f} low word")

        # For simple cases, return just the high word (truncated to integer)
        # Full 32-bit operations use the _fixed_* runtime functions
        return result_hi

    def _lower_fixed_point_binary(self, op: BinaryOp, left_val, right_val):
        """Lower a fixed-point binary operation.

        Uses runtime functions for 32-bit fixed-point arithmetic.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        result = self.builder.new_vreg(IRType.WORD, "_fixed_result")

        if op == BinaryOp.ADD:
            # Fixed-point addition: just add the 32-bit values
            # Call _fixed_add(left_hi, left_lo, right_hi, right_lo)
            self.builder.push(right_val)  # Simplified - push high words
            self.builder.push(left_val)
            self.builder.call(Label("_fixed_add"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            self.builder.pop(temp)
            self.builder.emit(IRInstr(OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="fixed add result"))

        elif op == BinaryOp.SUB:
            self.builder.push(right_val)
            self.builder.push(left_val)
            self.builder.call(Label("_fixed_sub"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            self.builder.pop(temp)
            self.builder.emit(IRInstr(OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="fixed sub result"))

        elif op == BinaryOp.MUL:
            # Fixed-point multiply requires scaling: (a * b) >> 16
            self.builder.push(right_val)
            self.builder.push(left_val)
            self.builder.call(Label("_fixed_mul"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            self.builder.pop(temp)
            self.builder.emit(IRInstr(OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="fixed mul result"))

        elif op == BinaryOp.DIV:
            # Fixed-point divide requires scaling: (a << 16) / b
            self.builder.push(right_val)
            self.builder.push(left_val)
            self.builder.call(Label("_fixed_div"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            self.builder.pop(temp)
            self.builder.emit(IRInstr(OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="fixed div result"))

        else:
            # For comparison ops, use integer comparison on high word
            return None

        return result

    def _lower_declare_expr(self, expr: DeclareExpr):
        """Lower a declare expression (Ada 2022).

        Syntax: (declare declarations begin expression)
        Example: (declare X : Integer := 5; begin X + 1)
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Process declarations (create local variables)
        for decl in expr.declarations:
            self._lower_decl(decl)

        # Evaluate and return the result expression
        return self._lower_expr(expr.result_expr)

    def _lower_delta_aggregate(self, expr: DeltaAggregate):
        """Lower a delta aggregate (Ada 2022).

        Syntax: (base_expression with delta component_associations)
        Creates a copy with specified components modified.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # First, evaluate the base expression to get the source
        base_ptr = self._lower_expr(expr.base_expression)

        # Try to get the record type from the base expression
        record_type = self._get_expr_type(expr.base_expression)

        # Allocate space for the new aggregate and copy the base
        size = 4  # Default record size
        if record_type and hasattr(record_type, 'size_bits') and record_type.size_bits:
            size = (record_type.size_bits + 7) // 8

        # Allocate stack space for the result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_delta_result")
        self.builder.emit(IRInstr(
            OpCode.SUB, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            Immediate(size, IRType.WORD),
            comment=f"allocate {size} bytes for delta aggregate"
        ))

        # Copy the base record to the new location
        # Use a block copy (or element-by-element for small records)
        if size <= 8:
            # For small records, copy word by word
            for offset in range(0, size, 2):
                temp = self.builder.new_vreg(IRType.WORD, "_copy_tmp")
                self.builder.emit(IRInstr(
                    OpCode.LOAD, temp, base_ptr, Immediate(offset, IRType.WORD),
                    comment=f"copy byte {offset} from base"
                ))
                self.builder.emit(IRInstr(
                    OpCode.STORE, result_ptr, temp, Immediate(offset, IRType.WORD),
                    comment=f"copy byte {offset} to result"
                ))
        else:
            # For larger records, call a block copy routine
            self.builder.push(Immediate(size, IRType.WORD))
            self.builder.push(base_ptr)
            self.builder.push(result_ptr)
            self.builder.call(Label("_memcpy"))
            # Clean up stack
            for _ in range(3):
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)

        # Build field info map if we have the record type
        field_info = {}
        if record_type and isinstance(record_type, RecordType):
            for comp in record_type.components:
                field_info[comp.name.lower()] = {
                    'offset': comp.offset_bits // 8,
                    'size': (comp.size_bits + 7) // 8 if comp.size_bits else 2
                }

        # Process each component modification
        for assoc in expr.components:
            if assoc.choices:
                for choice in assoc.choices:
                    if isinstance(choice, ExprChoice) and isinstance(choice.expr, Identifier):
                        field_name = choice.expr.name.lower()
                        if field_name in field_info:
                            # Store the new value at the field offset
                            value = self._lower_expr(assoc.value)
                            offset = field_info[field_name]['offset']
                            self._store_at_offset(result_ptr, offset, value)
                        elif assoc.value:
                            # Fallback: try to resolve field dynamically
                            value = self._lower_expr(assoc.value)
                            # Store at offset 0 if we can't determine the field
                            self._store_at_offset(result_ptr, 0, value)

        return result_ptr

    def _lower_container_aggregate(self, expr: ContainerAggregate):
        """Lower a container aggregate (Ada 2022).

        Syntax: [component_associations]
        Example: [for I in 1 .. 10 => I * 2]
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Container aggregates are similar to regular aggregates
        # but use square brackets and are for container types

        # Determine size needed
        num_components = len(expr.components) if hasattr(expr, 'components') else 0

        # Check for iterated components to determine final size
        total_elements = 0
        for comp in expr.components:
            if hasattr(comp, 'iterator') and comp.iterator:
                # Iterated component: for I in 1 .. 10 => expr
                if hasattr(comp.iterator, 'iterable') and isinstance(comp.iterator.iterable, RangeExpr):
                    low = self._eval_static(comp.iterator.iterable.low)
                    high = self._eval_static(comp.iterator.iterable.high)
                    if low is not None and high is not None:
                        total_elements += high - low + 1
                    else:
                        total_elements += 10  # Fallback
                else:
                    total_elements += 1
            else:
                total_elements += 1

        size = total_elements * 2  # Word-sized elements

        if size == 0:
            return Immediate(0, IRType.PTR)

        # Reserve stack space
        container_ptr = self.builder.new_vreg(IRType.PTR, "_container_ptr")
        self.builder.emit(IRInstr(
            OpCode.SUB, container_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            Immediate(size, IRType.WORD),
            comment=f"allocate container ({size} bytes)"
        ))

        # Store container metadata (length at offset 0)
        self.builder.emit(IRInstr(
            OpCode.STORE, container_ptr, Immediate(total_elements, IRType.WORD),
            comment="store container length"
        ))

        # Initialize elements
        offset = 2  # Skip length field
        for comp in expr.components:
            if hasattr(comp, 'iterator') and comp.iterator:
                # Iterated component: generate loop to fill elements
                iter_var_name = comp.iterator.name if hasattr(comp.iterator, 'name') else "_i"
                iter_var = self.builder.new_vreg(IRType.WORD, f"_iter_{iter_var_name}")

                if hasattr(comp.iterator, 'iterable') and isinstance(comp.iterator.iterable, RangeExpr):
                    low_val = self._lower_expr(comp.iterator.iterable.low)
                    high_val = self._lower_expr(comp.iterator.iterable.high)

                    # Initialize iterator
                    self.builder.mov(iter_var, low_val)

                    # Loop to fill elements
                    loop_start = self._new_label("container_loop")
                    loop_end = self._new_label("container_end")
                    offset_reg = self.builder.new_vreg(IRType.WORD, "_offset")
                    self.builder.mov(offset_reg, Immediate(offset, IRType.WORD))

                    # Register iterator as local for value expression
                    old_local = self.ctx.locals.get(iter_var_name.lower())
                    self.ctx.locals[iter_var_name.lower()] = LocalVariable(
                        name=iter_var_name, vreg=iter_var, offset=0, size=2, ada_type=None
                    )

                    self.builder.label(loop_start)

                    # Check loop condition
                    cond = self.builder.new_vreg(IRType.WORD, "_cond")
                    self.builder.cmp_gt(cond, iter_var, high_val)
                    self.builder.jnz(cond, Label(loop_end))

                    # Evaluate value expression and store
                    value = self._lower_expr(comp.value)
                    addr = self.builder.new_vreg(IRType.PTR, "_elem_addr")
                    self.builder.add(addr, container_ptr, offset_reg)
                    self.builder.store(addr, value)

                    # Increment offset and iterator
                    self.builder.add(offset_reg, offset_reg, Immediate(2, IRType.WORD))
                    self.builder.add(iter_var, iter_var, Immediate(1, IRType.WORD))
                    self.builder.jmp(Label(loop_start))

                    self.builder.label(loop_end)

                    # Restore old local
                    if old_local is not None:
                        self.ctx.locals[iter_var_name.lower()] = old_local
                    elif iter_var_name.lower() in self.ctx.locals:
                        del self.ctx.locals[iter_var_name.lower()]
            else:
                # Simple component
                value = self._lower_expr(comp.value)
                self._store_at_offset(container_ptr, offset, value)
                offset += 2

        return container_ptr

    def _lower_target_name(self, expr: TargetName):
        """Lower target name '@' (Ada 2022).

        The '@' symbol refers to the target of the enclosing assignment.
        Example: X := @ + 1;  -- equivalent to X := X + 1;
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        target = self.ctx.assignment_target
        if target is None:
            # No enclosing assignment - return zero (semantic error)
            return Immediate(0, IRType.WORD)

        # Load the current value of the assignment target
        # This reuses the expression lowering logic
        if isinstance(target, Identifier):
            name = target.name.lower()

            # Check locals
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                return local.vreg

            # Check params
            if name in self.ctx.params:
                return self.ctx.params[name]

            # Check globals (module-level variables)
            if name in self.globals:
                return self._load_global(name)

        elif isinstance(target, IndexedComponent):
            # Array element: load the current value
            return self._lower_indexed_load(target)

        elif isinstance(target, SelectedName):
            # Record field: load the current value
            return self._lower_selected_load(target)

        # Fallback for unsupported target types
        return Immediate(0, IRType.WORD)

    def _lower_aggregate(self, expr: Aggregate):
        """Lower an aggregate expression.

        For Z80, aggregates are built on the stack as temporary values.
        Returns a pointer to the aggregate in stack memory.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Try to get the aggregate's type from context (if available)
        agg_type = getattr(expr, 'resolved_type', None)

        # Build field info map if this is a record type
        field_info = {}
        if isinstance(agg_type, RecordType):
            for comp in agg_type.components:
                field_info[comp.name.lower()] = {
                    'offset': comp.offset_bits // 8,
                    'size': (comp.size_bits + 7) // 8 if comp.size_bits else 2
                }

        # Determine size of aggregate
        if isinstance(agg_type, RecordType):
            size = agg_type.size_bytes()
        elif isinstance(agg_type, ArrayType):
            size = agg_type.size_bytes()
        else:
            # Fall back to component count
            num_components = len(expr.components)
            size = num_components * 2

        # Allocate stack space for the aggregate
        agg_addr = self.builder.new_vreg(IRType.PTR, "_agg_tmp")

        # Reserve stack space
        if size > 0:
            self.builder.emit(IRInstr(
                OpCode.SUB,
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                Immediate(size, IRType.WORD),
                comment=f"allocate aggregate ({size} bytes)"
            ))
            # Point agg_addr to the allocated space
            self.builder.emit(IRInstr(
                OpCode.MOV, agg_addr,
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                comment="aggregate base address"
            ))

        # Track current positional offset for positional aggregates
        positional_offset = 0
        others_value = None

        # First pass: find others clause if present
        for comp in expr.components:
            if comp.choices:
                for choice in comp.choices:
                    if isinstance(choice, OthersChoice):
                        others_value = comp.value
                        break

        # Second pass: process each component
        for comp in expr.components:
            # Skip others - handled separately
            is_others = False
            if comp.choices:
                for choice in comp.choices:
                    if isinstance(choice, OthersChoice):
                        is_others = True
                        break
            if is_others:
                continue

            # Lower the component value
            value = self._lower_expr(comp.value)

            # Determine where to store this value
            if comp.choices:
                # Named aggregate: (X => 1, Y => 2) or (1 .. 5 => 0)
                for choice in comp.choices:
                    if isinstance(choice, ExprChoice):
                        if isinstance(choice.expr, Identifier):
                            # Named field - look up its offset
                            field_name = choice.expr.name.lower()
                            if field_name in field_info:
                                offset = field_info[field_name]['offset']
                            else:
                                # Field not found in type info, use positional
                                offset = positional_offset
                                positional_offset += 2
                            mem = MemoryLocation(base=agg_addr, offset=offset, ir_type=IRType.WORD)
                            self.builder.store(mem, value, comment=f"field {field_name}")
                        else:
                            # Index expression for array aggregate
                            idx = self._lower_expr(choice.expr)
                            # Calculate offset: (index - lower_bound) * element_size
                            # For simplicity, assume 0-based or 1-based indexing
                            offset_reg = self.builder.new_vreg(IRType.WORD, "_idx_off")
                            self.builder.mul(offset_reg, idx, Immediate(2, IRType.WORD))
                            # Create indexed memory location
                            mem = MemoryLocation(base=agg_addr, index=offset_reg, ir_type=IRType.WORD)
                            self.builder.store(mem, value)
                    elif isinstance(choice, RangeChoice):
                        # Range association: (1 .. 5 => 0)
                        low = self._lower_expr(choice.range_expr.low)
                        high = self._lower_expr(choice.range_expr.high)
                        # Generate loop to fill range
                        loop_var = self.builder.new_vreg(IRType.WORD, "_range_idx")
                        self.builder.mov(loop_var, low)
                        loop_start = self.builder.new_label("agg_range_loop")
                        loop_end = self.builder.new_label("agg_range_end")
                        self.builder.label(loop_start.name)
                        self.builder.cmp(loop_var, high)
                        self.builder.jg(loop_end)
                        # Store value at index
                        offset_reg = self.builder.new_vreg(IRType.WORD, "_range_off")
                        self.builder.mul(offset_reg, loop_var, Immediate(2, IRType.WORD))
                        mem = MemoryLocation(base=agg_addr, index=offset_reg, ir_type=IRType.WORD)
                        self.builder.store(mem, value)
                        # Increment and loop
                        self.builder.add(loop_var, loop_var, Immediate(1, IRType.WORD))
                        self.builder.jmp(loop_start)
                        self.builder.label(loop_end.name)
            else:
                # Positional aggregate: (1, 2, 3)
                mem = MemoryLocation(base=agg_addr, offset=positional_offset, ir_type=IRType.WORD)
                self.builder.store(mem, value)
                positional_offset += 2

        # Handle others clause - fill remaining fields/elements
        if others_value is not None and field_info:
            others_val = self._lower_expr(others_value)
            # For records, fill unassigned fields
            # This would require tracking which fields were assigned
            # For now, skip - the explicit assignments should cover it

        return agg_addr

    def _lower_aggregate_to_target(self, expr: Aggregate, target_addr, target_type):
        """Lower an aggregate directly to a target address.

        Used when the target type is known (e.g., record assignment).
        Supports:
        - Positional aggregates: (1, 2, 3)
        - Named aggregates: (X => 1, Y => 2)
        - Range associations: (1 .. 5 => 0)
        - Others clause: (others => 0)
        """
        if self.ctx is None:
            return

        # For record aggregates with known target type
        if isinstance(target_type, RecordType):
            # Map component names to offsets and sizes
            field_info = {}
            for comp in target_type.components:
                field_info[comp.name.lower()] = {
                    'offset': comp.offset_bits // 8,
                    'size': (comp.size_bits + 7) // 8 if comp.size_bits else 2
                }

            # Track which fields have been assigned
            assigned_fields = set()

            # Get ordered list of field names from record type
            ordered_fields = [comp.name.lower() for comp in target_type.components]
            positional_index = 0  # Current field index for positional assignment

            # First pass: handle explicit assignments
            others_value = None
            for comp_assoc in expr.components:
                if comp_assoc.choices:
                    for choice in comp_assoc.choices:
                        if isinstance(choice, OthersChoice):
                            others_value = comp_assoc.value
                            continue
                        if isinstance(choice, ExprChoice):
                            if isinstance(choice.expr, Identifier):
                                field_name = choice.expr.name.lower()
                                if field_name in field_info:
                                    value = self._lower_expr(comp_assoc.value)
                                    offset = field_info[field_name]['offset']
                                    self._store_at_offset(target_addr, offset, value)
                                    assigned_fields.add(field_name)
                else:
                    # Positional assignment - use field order
                    if positional_index < len(ordered_fields):
                        field_name = ordered_fields[positional_index]
                        if field_name in field_info:
                            value = self._lower_expr(comp_assoc.value)
                            offset = field_info[field_name]['offset']
                            self._store_at_offset(target_addr, offset, value)
                            assigned_fields.add(field_name)
                        positional_index += 1

            # Second pass: apply 'others' to unassigned fields
            if others_value is not None:
                value = self._lower_expr(others_value)
                for field_name, info in field_info.items():
                    if field_name not in assigned_fields:
                        self._store_at_offset(target_addr, info['offset'], value)

        # For array aggregates
        elif isinstance(target_type, ArrayType):
            element_size = 2  # Default
            if target_type.component_type:
                element_size = (target_type.component_type.size_bits + 7) // 8

            # Get array bounds
            lower_bound = 1
            upper_bound = 10  # Default
            if target_type.bounds:
                lower_bound = target_type.bounds[0][0]
                upper_bound = target_type.bounds[0][1]

            # Track which indices have been assigned
            assigned_indices = set()
            others_value = None
            positional_index = lower_bound

            for comp_assoc in expr.components:
                if comp_assoc.choices:
                    for choice in comp_assoc.choices:
                        if isinstance(choice, OthersChoice):
                            others_value = comp_assoc.value
                        elif isinstance(choice, RangeChoice):
                            # Range association: (1 .. 5 => value)
                            range_low = self._eval_static(choice.range_expr.low)
                            range_high = self._eval_static(choice.range_expr.high)
                            if range_low is not None and range_high is not None:
                                value = self._lower_expr(comp_assoc.value)
                                for idx in range(range_low, range_high + 1):
                                    offset = (idx - lower_bound) * element_size
                                    self._store_at_offset(target_addr, offset, value)
                                    assigned_indices.add(idx)
                        elif isinstance(choice, ExprChoice):
                            # Named index: (5 => value) or (Index_Name => value)
                            idx = self._eval_static(choice.expr)
                            if idx is not None:
                                value = self._lower_expr(comp_assoc.value)
                                offset = (idx - lower_bound) * element_size
                                self._store_at_offset(target_addr, offset, value)
                                assigned_indices.add(idx)
                else:
                    # Positional association
                    value = self._lower_expr(comp_assoc.value)
                    offset = (positional_index - lower_bound) * element_size
                    self._store_at_offset(target_addr, offset, value)
                    assigned_indices.add(positional_index)
                    positional_index += 1

            # Apply 'others' to unassigned indices
            if others_value is not None:
                value = self._lower_expr(others_value)
                for idx in range(lower_bound, upper_bound + 1):
                    if idx not in assigned_indices:
                        offset = (idx - lower_bound) * element_size
                        self._store_at_offset(target_addr, offset, value)

    def _store_at_offset(self, base_addr, offset: int, value) -> None:
        """Store a value at a given offset from a base address."""
        if offset != 0:
            addr = self.builder.new_vreg(IRType.PTR, "_addr")
            self.builder.add(addr, base_addr, Immediate(offset, IRType.WORD))
            mem = MemoryLocation(base=addr, offset=0, ir_type=IRType.WORD)
        else:
            mem = MemoryLocation(base=base_addr, offset=0, ir_type=IRType.WORD)
        self.builder.store(mem, value)

    def _eval_static(self, expr: Expr) -> Optional[int]:
        """Try to evaluate an expression statically at compile time."""
        if isinstance(expr, IntegerLiteral):
            return expr.value
        if isinstance(expr, Identifier):
            # Try to look up constant value
            sym = self.symbols.lookup(expr.name)
            if sym and sym.kind == SymbolKind.CONSTANT:
                if hasattr(sym, 'value') and isinstance(sym.value, int):
                    return sym.value
        if isinstance(expr, UnaryExpr):
            operand = self._eval_static(expr.operand)
            if operand is not None:
                if expr.op == UnaryOp.MINUS:
                    return -operand
                if expr.op == UnaryOp.PLUS:
                    return operand
        if isinstance(expr, BinaryExpr):
            left = self._eval_static(expr.left)
            right = self._eval_static(expr.right)
            if left is not None and right is not None:
                if expr.op == BinaryOp.ADD:
                    return left + right
                if expr.op == BinaryOp.SUB:
                    return left - right
                if expr.op == BinaryOp.MUL:
                    return left * right
                if expr.op == BinaryOp.DIV and right != 0:
                    return left // right
        return None

    def _lower_slice(self, expr: Slice):
        """Lower an array slice expression.

        A slice A(1..5) returns a view (pointer + bounds) of the array segment.
        For Z80 simplicity, returns a pointer to the first element of the slice.
        """
        if self.ctx is None:
            return Immediate(0, IRType.PTR)

        # Get the base array address
        base_addr = self._lower_expr(expr.prefix)

        # Get the low bound of the slice
        low_bound = self._lower_expr(expr.range_expr.low)

        # Determine element size
        element_size = 2  # Default to word

        # Look up the array type to get element size
        if isinstance(expr.prefix, Identifier):
            sym = self.symbols.lookup(expr.prefix.name)
            if sym and sym.ada_type and isinstance(sym.ada_type, ArrayType):
                if sym.ada_type.component_type:
                    element_size = (sym.ada_type.component_type.size_bits + 7) // 8

                # Account for array's lower bound
                array_low = 1
                if sym.ada_type.bounds:
                    array_low = sym.ada_type.bounds[0][0]

                # Calculate offset: (slice_low - array_low) * element_size
                if isinstance(low_bound, Immediate) and array_low != 0:
                    # Adjust for array's lower bound
                    offset = (low_bound.value - array_low) * element_size
                    slice_addr = self.builder.new_vreg(IRType.PTR, "_slice")
                    self.builder.add(slice_addr, base_addr, Immediate(offset, IRType.WORD))
                    return slice_addr

        # Dynamic calculation for non-constant bounds
        # offset = (low_bound - array_low) * element_size
        # Simplified: just offset from base
        offset_val = self.builder.new_vreg(IRType.WORD, "_offset")

        # Multiply index by element size
        if element_size != 1:
            size_imm = Immediate(element_size, IRType.WORD)
            self.builder.mul(offset_val, low_bound, size_imm)
        else:
            self.builder.mov(offset_val, low_bound)

        # Add to base address
        slice_addr = self.builder.new_vreg(IRType.PTR, "_slice")
        self.builder.add(slice_addr, base_addr, offset_val)

        return slice_addr

    def _lower_slice_store(self, target: Slice, value) -> None:
        """Lower array slice assignment.

        A(1..5) := B(1..5) copies elements from source to target slice.
        This is a block memory copy operation.
        """
        if self.ctx is None:
            return

        # Get destination slice address
        dest_addr = self._lower_slice(target)

        # Get source slice address (value should be a pointer to source slice)
        src_addr = value

        # Calculate number of elements to copy
        low_bound = self._lower_expr(target.range_expr.low)
        high_bound = self._lower_expr(target.range_expr.high)

        # Determine element size
        element_size = 2  # Default to word
        if isinstance(target.prefix, Identifier):
            sym = self.symbols.lookup(target.prefix.name)
            if sym and sym.ada_type and isinstance(sym.ada_type, ArrayType):
                if sym.ada_type.component_type:
                    element_size = (sym.ada_type.component_type.size_bits + 7) // 8

        # Calculate byte count: (high - low + 1) * element_size
        if isinstance(low_bound, Immediate) and isinstance(high_bound, Immediate):
            # Constant bounds
            num_elements = high_bound.value - low_bound.value + 1
            byte_count = num_elements * element_size
            byte_count_vreg = Immediate(byte_count, IRType.WORD)
        else:
            # Dynamic bounds
            count = self.builder.new_vreg(IRType.WORD, "_count")
            self.builder.sub(count, high_bound, low_bound)
            self.builder.add(count, count, Immediate(1, IRType.WORD))
            if element_size != 1:
                byte_count_vreg = self.builder.new_vreg(IRType.WORD, "_bytes")
                self.builder.mul(byte_count_vreg, count, Immediate(element_size, IRType.WORD))
            else:
                byte_count_vreg = count

        # Call block copy runtime function
        self.builder.push(byte_count_vreg)  # count
        self.builder.push(src_addr)          # source
        self.builder.push(dest_addr)         # destination
        self.builder.call(Label("_memcpy"), comment="slice assignment")
        # Clean up stack (3 words)
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)
        self.builder.pop(temp)
        self.builder.pop(temp)

    def _resolve_type(self, type_expr: Expr):
        """Resolve a type expression to an AdaType."""
        if isinstance(type_expr, Identifier):
            sym = self.symbols.lookup(type_expr.name)
            if sym and sym.kind == SymbolKind.TYPE:
                return sym.ada_type
        return None

    def _lower_identifier(self, expr: Identifier):
        """Lower an identifier reference."""
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        name = expr.name.lower()

        # Check locals
        if name in self.ctx.locals:
            return self.ctx.locals[name].vreg

        # Check params
        if name in self.ctx.params:
            return self.ctx.params[name]

        # Check for boolean literals
        if name == "true":
            return Immediate(1, IRType.BOOL)
        if name == "false":
            return Immediate(0, IRType.BOOL)

        # Default
        return Immediate(0, IRType.WORD)

    def _lower_binary(self, expr: BinaryExpr):
        """Lower a binary expression."""
        op = expr.op

        # Handle short-circuit operators specially - don't evaluate right operand unconditionally
        if op == BinaryOp.AND_THEN:
            return self._lower_short_circuit_and(expr)
        elif op == BinaryOp.OR_ELSE:
            return self._lower_short_circuit_or(expr)

        left = self._lower_expr(expr.left)
        right = self._lower_expr(expr.right)

        result = self.builder.new_vreg(IRType.WORD, "_tmp")

        # Check if this is a string comparison
        if op in (BinaryOp.EQ, BinaryOp.NE, BinaryOp.LT, BinaryOp.LE, BinaryOp.GT, BinaryOp.GE):
            left_type = self._get_expr_type(expr.left)
            if left_type and isinstance(left_type, ArrayType):
                if left_type.name == "String" or (left_type.component_type and
                    hasattr(left_type.component_type, 'name') and
                    left_type.component_type.name == "Character"):
                    return self._lower_string_comparison(op, left, right)

        if op == BinaryOp.ADD:
            self.builder.add(result, left, right)
        elif op == BinaryOp.SUB:
            self.builder.sub(result, left, right)
        elif op == BinaryOp.MUL:
            self.builder.mul(result, left, right)
        elif op == BinaryOp.DIV:
            self.builder.div(result, left, right)
        elif op == BinaryOp.AND:
            self.builder.and_(result, left, right)
        elif op == BinaryOp.OR:
            self.builder.or_(result, left, right)
        elif op == BinaryOp.XOR:
            self.builder.xor(result, left, right)
        elif op == BinaryOp.EQ:
            self.builder.cmp_eq(result, left, right)
        elif op == BinaryOp.NE:
            self.builder.cmp_ne(result, left, right)
        elif op == BinaryOp.LT:
            self.builder.cmp_lt(result, left, right)
        elif op == BinaryOp.LE:
            self.builder.cmp_le(result, left, right)
        elif op == BinaryOp.GT:
            self.builder.cmp_gt(result, left, right)
        elif op == BinaryOp.GE:
            self.builder.cmp_ge(result, left, right)
        elif op == BinaryOp.MOD:
            self.builder.emit(IRInstr(OpCode.MOD, result, left, right))
        elif op == BinaryOp.REM:
            # REM differs from MOD in sign handling for negative numbers
            # Ada REM: result has same sign as dividend
            self.builder.emit(IRInstr(OpCode.REM, result, left, right))
        elif op == BinaryOp.EXP:
            # Exponentiation: call runtime function
            self.builder.push(right)  # exponent
            self.builder.push(left)   # base
            self.builder.call(Label("_exp16"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            self.builder.pop(temp)
            # Capture result from HL register
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture exponentiation result from HL"
            ))
        elif op == BinaryOp.CONCAT:
            # String concatenation: call runtime function
            self.builder.push(right)  # second string
            self.builder.push(left)   # first string
            self.builder.call(Label("_str_concat"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            self.builder.pop(temp)
            # Capture result from HL register (pointer to concatenated string)
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture string concat result from HL"
            ))
        else:
            # Default: move left to result
            self.builder.mov(result, left)

        return result

    def _lower_short_circuit_and(self, expr: BinaryExpr):
        """Lower short-circuit AND THEN.

        Evaluates left operand first. If false, returns false without
        evaluating right operand. Only evaluates right if left is true.
        """
        result = self.builder.new_vreg(IRType.WORD, "_and_then")

        # Create labels
        eval_right = self.builder.new_label("and_eval_right")
        short_circuit = self.builder.new_label("and_short")
        done = self.builder.new_label("and_done")

        # Evaluate left operand
        left = self._lower_expr(expr.left)

        # Check if left is false (0)
        self.builder.cmp(left, Immediate(0, IRType.WORD))
        self.builder.jz(short_circuit)  # If false, short-circuit

        # Left was true, evaluate right
        self.builder.label(eval_right.name)
        right = self._lower_expr(expr.right)
        self.builder.mov(result, right)  # Result is right operand's value
        self.builder.jmp(done)

        # Short circuit: left was false, result is false
        self.builder.label(short_circuit.name)
        self.builder.mov(result, Immediate(0, IRType.WORD))

        self.builder.label(done.name)
        return result

    def _lower_short_circuit_or(self, expr: BinaryExpr):
        """Lower short-circuit OR ELSE.

        Evaluates left operand first. If true, returns true without
        evaluating right operand. Only evaluates right if left is false.
        """
        result = self.builder.new_vreg(IRType.WORD, "_or_else")

        # Create labels
        eval_right = self.builder.new_label("or_eval_right")
        short_circuit = self.builder.new_label("or_short")
        done = self.builder.new_label("or_done")

        # Evaluate left operand
        left = self._lower_expr(expr.left)

        # Check if left is true (non-zero)
        self.builder.cmp(left, Immediate(0, IRType.WORD))
        self.builder.jnz(short_circuit)  # If true, short-circuit

        # Left was false, evaluate right
        self.builder.label(eval_right.name)
        right = self._lower_expr(expr.right)
        self.builder.mov(result, right)  # Result is right operand's value
        self.builder.jmp(done)

        # Short circuit: left was true, result is true (use left's value)
        self.builder.label(short_circuit.name)
        self.builder.mov(result, left)

        self.builder.label(done.name)
        return result

    def _lower_string_comparison(self, op: BinaryOp, left, right):
        """Lower string comparison operations.

        Calls runtime function to compare null-terminated strings.
        Returns comparison result as 0/1 for boolean False/True.
        """
        result = self.builder.new_vreg(IRType.WORD, "_str_cmp_result")

        # Call _str_cmp which returns: negative if left < right, 0 if equal, positive if left > right
        self.builder.push(right)  # second string
        self.builder.push(left)   # first string
        self.builder.call(Label("_str_cmp"), comment="string comparison")
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)
        self.builder.pop(temp)

        # Get comparison result from HL
        cmp_result = self.builder.new_vreg(IRType.WORD, "_cmp")
        self.builder.emit(IRInstr(
            OpCode.MOV, cmp_result,
            MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
            comment="capture string comparison result"
        ))

        # Convert strcmp-style result to boolean based on operator
        if op == BinaryOp.EQ:
            # Equal: result == 0
            self.builder.cmp_eq(result, cmp_result, Immediate(0, IRType.WORD))
        elif op == BinaryOp.NE:
            # Not equal: result != 0
            self.builder.cmp_ne(result, cmp_result, Immediate(0, IRType.WORD))
        elif op == BinaryOp.LT:
            # Less than: result < 0
            self.builder.cmp_lt(result, cmp_result, Immediate(0, IRType.WORD))
        elif op == BinaryOp.LE:
            # Less or equal: result <= 0
            self.builder.cmp_le(result, cmp_result, Immediate(0, IRType.WORD))
        elif op == BinaryOp.GT:
            # Greater than: result > 0
            self.builder.cmp_gt(result, cmp_result, Immediate(0, IRType.WORD))
        elif op == BinaryOp.GE:
            # Greater or equal: result >= 0
            self.builder.cmp_ge(result, cmp_result, Immediate(0, IRType.WORD))
        else:
            # Default: equality
            self.builder.cmp_eq(result, cmp_result, Immediate(0, IRType.WORD))

        return result

    def _lower_unary(self, expr: UnaryExpr):
        """Lower a unary expression."""
        operand = self._lower_expr(expr.operand)
        result = self.builder.new_vreg(IRType.WORD, "_tmp")

        op = expr.op
        if op == UnaryOp.MINUS:
            self.builder.neg(result, operand)
        elif op == UnaryOp.NOT:
            self.builder.not_(result, operand)
        elif op == UnaryOp.ABS:
            # ABS: if negative, negate; otherwise keep same
            cond = self.builder.new_vreg(IRType.BOOL, "_abs_cond")
            self.builder.cmp_lt(cond, operand, Immediate(0, IRType.WORD))
            # If positive (not negative), skip negation
            pos_label = self._new_label("abs_pos")
            end_label = self._new_label("abs_end")
            self.builder.jz(cond, Label(pos_label))
            # Negative path: negate
            self.builder.neg(result, operand)
            self.builder.jmp(Label(end_label))
            # Positive path: copy as-is
            pos_block = self.builder.new_block(pos_label)
            self.builder.set_block(pos_block)
            self.builder.mov(result, operand)
            # End
            end_block = self.builder.new_block(end_label)
            self.builder.set_block(end_block)
        elif op == UnaryOp.PLUS:
            self.builder.mov(result, operand)
        else:
            self.builder.mov(result, operand)

        return result

    def _resolve_overload(self, name: str, args: list) -> Optional[Symbol]:
        """Resolve an overloaded function/procedure call.

        Returns the best matching symbol based on argument types.
        """
        from uada80.type_system import types_compatible

        overloads = self.symbols.all_overloads(name)
        if not overloads:
            return None

        if len(overloads) == 1:
            return overloads[0]

        # Get actual argument types
        arg_types = []
        for arg in args:
            if arg.value:
                arg_type = self._get_expr_type(arg.value)
                arg_types.append(arg_type)

        # Find best match
        best_match = None
        best_score = -1

        for sym in overloads:
            if len(sym.parameters) != len(arg_types):
                continue  # Wrong number of arguments

            # Check if all arguments are compatible
            score = 0
            all_match = True
            for i, (param, arg_type) in enumerate(zip(sym.parameters, arg_types)):
                if param.ada_type and arg_type:
                    if param.ada_type.name == arg_type.name:
                        score += 2  # Exact match
                    elif types_compatible(param.ada_type, arg_type):
                        score += 1  # Compatible
                    else:
                        all_match = False
                        break
                else:
                    score += 1  # Assume compatible if types unknown

            if all_match and score > best_score:
                best_score = score
                best_match = sym

        return best_match if best_match else overloads[0]

    def _get_expr_type(self, expr: Expr) -> Optional[AdaType]:
        """Get the Ada type of an expression."""
        if isinstance(expr, IntegerLiteral):
            return PREDEFINED_TYPES.get("Integer")
        elif isinstance(expr, StringLiteral):
            return PREDEFINED_TYPES.get("String")
        elif isinstance(expr, CharacterLiteral):
            return PREDEFINED_TYPES.get("Character")
        elif isinstance(expr, Identifier):
            sym = self.symbols.lookup(expr.name)
            if sym and sym.ada_type:
                return sym.ada_type
        elif isinstance(expr, BinaryExpr):
            # Result type depends on operator and operand types
            left_type = self._get_expr_type(expr.left)
            return left_type  # Simplified: assume result is same as left operand
        elif isinstance(expr, FunctionCall):
            if isinstance(expr.name, Identifier):
                sym = self.symbols.lookup(expr.name.name)
                if sym and sym.return_type:
                    return sym.return_type
        return None

    def _lower_function_call(self, expr: FunctionCall):
        """Lower a function call expression."""
        result = self.builder.new_vreg(IRType.WORD, "_result")

        # Check for intrinsic functions
        func_name = ""
        if isinstance(expr.name, Identifier):
            func_name = expr.name.name.lower()
        elif isinstance(expr.name, SelectedName):
            func_name = expr.name.selector.lower()

        # Handle Shift_Left intrinsic
        if func_name == "shift_left" and len(expr.args) >= 2:
            value = self._lower_expr(expr.args[0].value)
            amount = self._lower_expr(expr.args[1].value)
            self.builder.emit(IRInstr(OpCode.SHL, result, value, amount,
                                      comment="Shift_Left"))
            return result

        # Handle Shift_Right intrinsic
        if func_name == "shift_right" and len(expr.args) >= 2:
            value = self._lower_expr(expr.args[0].value)
            amount = self._lower_expr(expr.args[1].value)
            self.builder.emit(IRInstr(OpCode.SHR, result, value, amount,
                                      comment="Shift_Right"))
            return result

        # Handle Rotate_Left intrinsic
        if func_name == "rotate_left" and len(expr.args) >= 2:
            # For 16-bit: rotate left by N = (value << N) | (value >> (16 - N))
            value = self._lower_expr(expr.args[0].value)
            amount = self._lower_expr(expr.args[1].value)

            # Shift left part: value << amount
            left_part = self.builder.new_vreg(IRType.WORD, "_rol_left")
            self.builder.emit(IRInstr(OpCode.SHL, left_part, value, amount,
                                      comment="Rotate_Left: value << N"))

            # Calculate (16 - amount) for right shift
            complement = self.builder.new_vreg(IRType.WORD, "_rol_comp")
            self.builder.emit(IRInstr(OpCode.SUB, complement,
                                      Immediate(16, IRType.WORD), amount,
                                      comment="16 - N for wrap-around"))

            # Shift right part: value >> (16 - amount)
            right_part = self.builder.new_vreg(IRType.WORD, "_rol_right")
            self.builder.emit(IRInstr(OpCode.SHR, right_part, value, complement,
                                      comment="Rotate_Left: value >> (16-N)"))

            # Combine: result = left_part | right_part
            self.builder.emit(IRInstr(OpCode.OR, result, left_part, right_part,
                                      comment="Rotate_Left: combine parts"))
            return result

        # Handle Rotate_Right intrinsic
        if func_name == "rotate_right" and len(expr.args) >= 2:
            # For 16-bit: rotate right by N = (value >> N) | (value << (16 - N))
            value = self._lower_expr(expr.args[0].value)
            amount = self._lower_expr(expr.args[1].value)

            # Shift right part: value >> amount
            right_part = self.builder.new_vreg(IRType.WORD, "_ror_right")
            self.builder.emit(IRInstr(OpCode.SHR, right_part, value, amount,
                                      comment="Rotate_Right: value >> N"))

            # Calculate (16 - amount) for left shift
            complement = self.builder.new_vreg(IRType.WORD, "_ror_comp")
            self.builder.emit(IRInstr(OpCode.SUB, complement,
                                      Immediate(16, IRType.WORD), amount,
                                      comment="16 - N for wrap-around"))

            # Shift left part: value << (16 - amount)
            left_part = self.builder.new_vreg(IRType.WORD, "_ror_left")
            self.builder.emit(IRInstr(OpCode.SHL, left_part, value, complement,
                                      comment="Rotate_Right: value << (16-N)"))

            # Combine: result = right_part | left_part
            self.builder.emit(IRInstr(OpCode.OR, result, right_part, left_part,
                                      comment="Rotate_Right: combine parts"))
            return result

        if isinstance(expr.name, Identifier):
            # Resolve overloaded function
            sym = self._resolve_overload(expr.name.name, expr.args)

            # Check if this is an access-to-subprogram variable (indirect call)
            if sym and sym.ada_type:
                from uada80.type_system import AccessType
                if isinstance(sym.ada_type, AccessType) and sym.ada_type.is_access_subprogram:
                    # Indirect call through function pointer
                    return self._lower_indirect_call(expr, sym)

            # Determine the call target - use external name if imported
            call_target = expr.name.name
            if sym:
                if sym.is_imported and sym.external_name:
                    call_target = sym.external_name
                else:
                    call_target = sym.name

            # Check if this is a dispatching call
            is_dispatching = self._is_dispatching_call(sym, expr.args)

            # Push arguments (right to left for cdecl-style calling convention)
            for arg in reversed(expr.args):
                if arg.value:
                    value = self._lower_expr(arg.value)
                    self.builder.push(value)

            if is_dispatching and sym and sym.vtable_slot >= 0:
                # Dispatching call - emit DISPATCH instruction
                # First argument is the controlling operand (object pointer)
                first_arg = expr.args[0].value if expr.args else None
                if first_arg:
                    obj_ptr = self._lower_expr(first_arg)
                    self.builder.emit(IRInstr(
                        OpCode.DISPATCH,
                        src1=obj_ptr,
                        src2=Immediate(sym.vtable_slot, IRType.WORD),
                        comment=f"dispatch {sym.name}"
                    ))
            else:
                # Static call (using external name for imported functions)
                self.builder.call(Label(call_target))

            # Clean up stack (callee may not clean up on Z80)
            num_args = len(expr.args)
            if num_args > 0:
                # Pop arguments off stack (result is in HL, so we use DE for cleanup)
                for _ in range(num_args):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)

            # Result is already in HL after call - emit a store-from-HL instruction
            # Use a special marker that codegen can recognize
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture function return from HL"
            ))

        elif isinstance(expr.name, Dereference):
            # Explicit dereference call: Func_Ptr.all(args)
            return self._lower_indirect_call(expr, None)

        return result

    def _lower_indirect_call(self, expr: FunctionCall, sym: Optional[Symbol]):
        """Lower an indirect call through a function pointer.

        Used for access-to-subprogram types:
            type Func_Ptr is access function (X : Integer) return Integer;
            F : Func_Ptr := Some_Function'Access;
            Result := F(10);  -- indirect call
        """
        result = self.builder.new_vreg(IRType.WORD, "_indirect_result")

        # Get the function pointer value
        if isinstance(expr.name, Dereference):
            func_ptr = self._lower_expr(expr.name.prefix)
        elif isinstance(expr.name, Identifier):
            func_ptr = self._lower_expr(expr.name)
        else:
            func_ptr = self._lower_expr(expr.name)

        # Push arguments (right to left)
        for arg in reversed(expr.args):
            if arg.value:
                value = self._lower_expr(arg.value)
                self.builder.push(value)

        # Emit indirect call instruction
        self.builder.emit(IRInstr(
            OpCode.CALL_INDIRECT,
            src1=func_ptr,
            comment="indirect function call"
        ))

        # Clean up stack
        num_args = len(expr.args)
        for _ in range(num_args):
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)

        # Capture result from HL
        self.builder.emit(IRInstr(
            OpCode.MOV, result,
            MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
            comment="capture indirect call return from HL"
        ))

        return result

    def _lower_attribute(self, expr: AttributeReference):
        """Lower an attribute reference."""
        attr = expr.attribute.lower()

        if isinstance(expr.prefix, Identifier):
            sym = self.symbols.lookup(expr.prefix.name)
            if sym:
                # Get the type - either from a TYPE symbol or from a VARIABLE symbol
                ada_type = None
                if sym.kind == SymbolKind.TYPE:
                    ada_type = sym.ada_type
                elif sym.ada_type:
                    ada_type = sym.ada_type

                if ada_type:
                    # Handle array attributes
                    if isinstance(ada_type, ArrayType):
                        if ada_type.is_constrained and ada_type.bounds:
                            low, high = ada_type.bounds[0]  # First dimension
                            if attr == "first":
                                return Immediate(low, IRType.WORD)
                            if attr == "last":
                                return Immediate(high, IRType.WORD)
                            if attr == "length":
                                length = high - low + 1
                                return Immediate(length, IRType.WORD)
                        elif not ada_type.is_constrained:
                            # Unconstrained array (like String) - need runtime calculation
                            if attr == "length":
                                # Get pointer to string and call strlen
                                # _str_len expects HL = string pointer, returns length in HL
                                str_ptr = self._lower_expr(expr.prefix)
                                # Store to HL for the call
                                self.builder.emit(IRInstr(
                                    OpCode.MOV,
                                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                                    str_ptr,
                                    comment="set HL for _str_len"
                                ))
                                self.builder.call(Label("_str_len"), comment="String'Length")
                                result = self.builder.new_vreg(IRType.WORD, "_strlen")
                                self.builder.emit(IRInstr(
                                    OpCode.MOV, result,
                                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                                    comment="capture String'Length result from HL"
                                ))
                                return result
                            if attr == "first":
                                # Unconstrained strings are 1-indexed in Ada
                                return Immediate(1, IRType.WORD)

                    # Handle scalar type attributes (Integer'First, etc.)
                    if attr == "first" and hasattr(ada_type, "low"):
                        return Immediate(ada_type.low, IRType.WORD)
                    if attr == "last" and hasattr(ada_type, "high"):
                        return Immediate(ada_type.high, IRType.WORD)
                    if attr == "size":
                        return Immediate(ada_type.size_bits, IRType.WORD)

                    # Handle enumeration attributes
                    if isinstance(ada_type, EnumerationType):
                        if attr == "first" and ada_type.literals:
                            # Return position of first literal (always 0)
                            return Immediate(0, IRType.WORD)
                        if attr == "last" and ada_type.literals:
                            # Return position of last literal
                            return Immediate(len(ada_type.literals) - 1, IRType.WORD)

                    # Handle 'Modulus attribute for modular types
                    if attr == "modulus":
                        from uada80.type_system import ModularType
                        if isinstance(ada_type, ModularType):
                            return Immediate(ada_type.modulus, IRType.WORD)

                    # Handle 'Class attribute for tagged types
                    if attr == "class":
                        from uada80.type_system import RecordType
                        if isinstance(ada_type, RecordType) and ada_type.is_tagged:
                            # For T'Class, the value representation is the same
                            # Just the type is different (class-wide vs specific)
                            return self._lower_expr(expr.prefix)

                    # Handle 'Tag attribute for tagged types
                    if attr == "tag":
                        from uada80.type_system import RecordType
                        if isinstance(ada_type, RecordType) and ada_type.is_tagged:
                            # Get the tag (vtable pointer) from the object
                            obj_val = self._lower_expr(expr.prefix)
                            result = self.builder.new_vreg(IRType.PTR, "_tag")
                            # Tag is at offset 0 in tagged record
                            self.builder.emit(IRInstr(OpCode.LOAD, result, obj_val,
                                                      comment="get tag (vtable ptr)"))
                            return result

                    # Handle 'Range attribute (returns low bound for use in for loops)
                    if attr == "range":
                        if isinstance(ada_type, ArrayType) and ada_type.bounds:
                            # For arrays, 'Range means the index range
                            low, high = ada_type.bounds[0]
                            return Immediate(low, IRType.WORD)
                        elif hasattr(ada_type, "low"):
                            return Immediate(ada_type.low, IRType.WORD)

                # Handle 'Access attribute for subprograms
                if attr == "access":
                    if sym.kind in (SymbolKind.PROCEDURE, SymbolKind.FUNCTION):
                        # Return address of subprogram
                        result = self.builder.new_vreg(IRType.PTR, "_access")
                        proc_name = expr.prefix.name
                        # Use LEA to get address of the procedure label
                        self.builder.emit(IRInstr(
                            OpCode.LEA,
                            dst=result,
                            src1=Label(proc_name),
                            comment=f"{proc_name}'Access"
                        ))
                        return result
                    elif sym.kind == SymbolKind.VARIABLE:
                        # 'Access on variable returns pointer to variable
                        result = self.builder.new_vreg(IRType.PTR, "_access")
                        var_name = expr.prefix.name
                        if self.ctx and var_name.lower() in self.ctx.locals:
                            local_info = self.ctx.locals[var_name.lower()]
                            offset = local_info.offset
                            self.builder.emit(IRInstr(
                                OpCode.LEA,
                                dst=result,
                                src1=MemoryLocation(base=None, offset=offset, ir_type=IRType.PTR),
                                comment=f"{var_name}'Access"
                            ))
                        else:
                            self.builder.emit(IRInstr(
                                OpCode.LEA,
                                dst=result,
                                src1=MemoryLocation(is_global=True, symbol_name=var_name, ir_type=IRType.PTR),
                                comment=f"{var_name}'Access"
                            ))
                        return result

                # Handle 'Address attribute for variables
                if attr == "address":
                    if sym.kind == SymbolKind.VARIABLE:
                        # Get address of variable - use LEA instruction
                        result = self.builder.new_vreg(IRType.PTR, "_addr")
                        var_name = expr.prefix.name
                        # Check if it's a local or global
                        if self.ctx and var_name.lower() in self.ctx.locals:
                            # Local variable - compute address from frame pointer
                            local_info = self.ctx.locals[var_name.lower()]
                            offset = local_info.offset
                            self.builder.emit(IRInstr(
                                OpCode.LEA,
                                dst=result,
                                src1=MemoryLocation(base=None, offset=offset, ir_type=IRType.PTR),
                                comment=f"{var_name}'Address"
                            ))
                        else:
                            # Global variable - use label address
                            self.builder.emit(IRInstr(
                                OpCode.LEA,
                                dst=result,
                                src1=MemoryLocation(is_global=True, symbol_name=var_name, ir_type=IRType.PTR),
                                comment=f"{var_name}'Address"
                            ))
                        return result

        # Handle 'Pos, 'Val, 'Succ, 'Pred with arguments
        # Format: Type'Pos(X), Type'Val(N), Type'Succ(X), Type'Pred(X)
        if attr == "pos" and expr.args:
            # Type'Pos(X) - returns the position number of X
            # For enumeration, position is the internal value
            arg_value = self._lower_expr(expr.args[0])
            return arg_value  # For enums, value IS the position

        if attr == "val" and expr.args:
            # Type'Val(N) - returns the enumeration value at position N
            arg_value = self._lower_expr(expr.args[0])
            return arg_value  # For enums, position IS the value

        if attr == "succ" and expr.args:
            # Type'Succ(X) - returns the successor of X
            # Raises Constraint_Error if X is already at 'Last
            arg_value = self._lower_expr(expr.args[0])

            # Get type information for bounds check
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type
                    # Check that X < 'Last (can succeed)
                    if hasattr(ada_type, "high"):
                        self._emit_succ_check(arg_value, ada_type.high)
                    elif isinstance(ada_type, EnumerationType) and ada_type.literals:
                        self._emit_succ_check(arg_value, len(ada_type.literals) - 1)

            result = self.builder.new_vreg(IRType.WORD, "_succ")
            self.builder.add(result, arg_value, Immediate(1, IRType.WORD))
            return result

        if attr == "pred" and expr.args:
            # Type'Pred(X) - returns the predecessor of X
            # Raises Constraint_Error if X is already at 'First
            arg_value = self._lower_expr(expr.args[0])

            # Get type information for bounds check
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type
                    # Check that X > 'First (can precede)
                    if hasattr(ada_type, "low"):
                        self._emit_pred_check(arg_value, ada_type.low)
                    elif isinstance(ada_type, EnumerationType):
                        self._emit_pred_check(arg_value, 0)

            result = self.builder.new_vreg(IRType.WORD, "_pred")
            self.builder.sub(result, arg_value, Immediate(1, IRType.WORD))
            return result

        if attr == "image" and expr.args:
            # Type'Image(X) - returns string representation of X
            # For integers: convert to decimal string
            # Returns pointer to static buffer (not reentrant)
            arg_value = self._lower_expr(expr.args[0])
            self.builder.push(arg_value)
            self.builder.call(Label("_int_to_str"), comment="Integer'Image")
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            # Capture result from HL register
            result = self.builder.new_vreg(IRType.PTR, "_image")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                comment="capture Integer'Image result from HL"
            ))
            return result

        if attr == "img":
            # X'Img - Ada 2012 shorthand for Type_Of_X'Image(X)
            # The prefix is the object itself, not a type
            # Evaluate the prefix to get its value
            prefix_value = self._lower_expr(expr.prefix)
            self.builder.push(prefix_value)
            self.builder.call(Label("_int_to_str"), comment="X'Img")
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            # Capture result from HL register
            result = self.builder.new_vreg(IRType.PTR, "_img")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                comment="capture X'Img result from HL"
            ))
            return result

        if attr == "value" and expr.args:
            # Type'Value(S) - converts string to value
            # For integers: parse decimal string
            arg_value = self._lower_expr(expr.args[0])
            self.builder.push(arg_value)
            self.builder.call(Label("_str_to_int"), comment="Integer'Value")
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            # Capture result from HL register
            result = self.builder.new_vreg(IRType.WORD, "_value")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture Integer'Value result from HL"
            ))
            return result

        if attr == "min" and len(expr.args) >= 2:
            # Type'Min(X, Y) - returns minimum of X and Y
            x = self._lower_expr(expr.args[0])
            y = self._lower_expr(expr.args[1])
            result = self.builder.new_vreg(IRType.WORD, "_min")
            cond = self.builder.new_vreg(IRType.BOOL, "_cmp")
            self.builder.cmp_lt(cond, x, y)
            # Simple approach: always compute both, then select
            # More complex: use conditional jumps
            self.builder.mov(result, x)
            # If y < x, use y instead
            end_label = self._new_label("min_end")
            self.builder.jnz(cond, Label(end_label))
            self.builder.mov(result, y)
            min_end = self.builder.new_block(end_label)
            self.builder.set_block(min_end)
            return result

        if attr == "max" and len(expr.args) >= 2:
            # Type'Max(X, Y) - returns maximum of X and Y
            x = self._lower_expr(expr.args[0])
            y = self._lower_expr(expr.args[1])
            result = self.builder.new_vreg(IRType.WORD, "_max")
            cond = self.builder.new_vreg(IRType.BOOL, "_cmp")
            self.builder.cmp_gt(cond, x, y)
            self.builder.mov(result, x)
            end_label = self._new_label("max_end")
            self.builder.jnz(cond, Label(end_label))
            self.builder.mov(result, y)
            max_end = self.builder.new_block(end_label)
            self.builder.set_block(max_end)
            return result

        # Default
        return Immediate(0, IRType.WORD)

    def _lower_indexed(self, expr: IndexedComponent):
        """Lower an indexed component (array access)."""
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Get array base address
        base_addr = self._get_array_base(expr.prefix)
        if base_addr is None:
            return Immediate(0, IRType.WORD)

        # Calculate element address
        elem_addr = self._calc_element_addr(expr, base_addr)

        # Load value from element address
        result = self.builder.new_vreg(IRType.WORD, "_elem")
        self.builder.load(result, elem_addr)

        return result

    def _is_integer_type(self, expr) -> bool:
        """Check if expression has integer type."""
        if self.ctx is None:
            return False

        if isinstance(expr, Identifier):
            name = expr.name.lower()
            # Check locals
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                if local.ada_type:
                    type_name = getattr(local.ada_type, 'name', '').lower()
                    return type_name in ('integer', 'natural', 'positive', 'long_integer')
            # Check params
            if name in self.ctx.params:
                # Params are integers by default in this simple implementation
                return True
        elif isinstance(expr, IntegerLiteral):
            return True
        elif isinstance(expr, BinaryExpr):
            # Arithmetic operations return integer
            return True

        return False

    def _store_to_target(self, target, value) -> None:
        """Store a value to a target expression (lvalue)."""
        if self.ctx is None:
            return

        if isinstance(target, Identifier):
            name = target.name.lower()
            # Check locals
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                self.builder.mov(local.vreg, value, comment=f"store to {name}")
                return
            # Check params (out parameters)
            if name in self.ctx.params:
                param = self.ctx.params[name]
                self.builder.mov(param, value, comment=f"store to param {name}")
                return

        elif isinstance(target, IndexedComponent):
            # Array element store
            self._lower_indexed_store(target, value)

        elif isinstance(target, SelectedName):
            # Record field store
            self._lower_selected_store(target, value)

    def _get_string_max_length(self, expr) -> int:
        """Get maximum length for a string variable."""
        if self.ctx is None:
            return 80  # Default buffer size

        if isinstance(expr, Identifier):
            name = expr.name.lower()
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                if local.ada_type:
                    # Try to get bounds from string type
                    if hasattr(local.ada_type, 'first') and hasattr(local.ada_type, 'last'):
                        first = getattr(local.ada_type, 'first', 1)
                        last = getattr(local.ada_type, 'last', 80)
                        if isinstance(first, int) and isinstance(last, int):
                            return last - first + 1

        return 80  # Default buffer size


def lower_to_ir(program: Program, semantic_result: SemanticResult) -> IRModule:
    """Lower a program to IR."""
    lowering = ASTLowering(semantic_result.symbols)
    return lowering.lower(program)
