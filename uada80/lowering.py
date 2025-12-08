"""
AST to IR Lowering.

Translates type-checked AST to IR for code generation.
"""

from dataclasses import dataclass, field
from typing import Optional

from uada80.ast_nodes import (
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
    Slice,
    FunctionCall,
    TypeConversion,
    QualifiedExpr,
    Allocator,
    Dereference,
    ConditionalExpr,
    QuantifiedExpr,
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
        elif isinstance(decl, SubprogramBody):
            # Nested subprogram - lower separately
            self._lower_subprogram_body(decl)

    def _lower_object_decl(self, decl: ObjectDecl) -> None:
        """Lower an object declaration."""
        if self.ctx is None:
            return

        # Handle renaming declarations
        if decl.renames:
            self._lower_renaming_decl(decl)
            return

        # Process initialization
        if decl.init_expr:
            init_value = self._lower_expr(decl.init_expr)

            for name in decl.names:
                local = self.ctx.locals.get(name.lower())
                if local:
                    self.builder.mov(local.vreg, init_value,
                                    comment=f"init {name}")
                    # Check type invariant after initialization
                    if decl.type_mark and decl.type_mark.type_mark:
                        self._check_type_invariant(local.vreg, decl.type_mark.type_mark)

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

        value = self._lower_expr(stmt.value)

        # Get target
        if isinstance(stmt.target, Identifier):
            name = stmt.target.name.lower()

            # Check locals
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                self.builder.mov(local.vreg, value, comment=f"{name} := ...")
                return

            # Check params
            if name in self.ctx.params:
                param = self.ctx.params[name]
                self.builder.mov(param, value, comment=f"{name} := ...")
                return

        elif isinstance(stmt.target, IndexedComponent):
            # Array assignment
            self._lower_indexed_store(stmt.target, value)

        elif isinstance(stmt.target, SelectedName):
            # Record field assignment
            self._lower_selected_store(stmt.target, value)

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
        for i, handler in reversed(list(enumerate(handlers))):
            handler_label = handler_labels[i]

            # Determine exception ID(s) for this handler
            if not handler.exception_names:
                # "when others =>" catches all
                exc_id = 0
            else:
                # For now, use the first exception name
                # TODO: Support multiple exception names per handler
                first_name = handler.exception_names[0]
                if isinstance(first_name, Identifier):
                    if first_name.name.lower() == "others":
                        exc_id = 0
                    else:
                        exc_id = self._get_exception_id(first_name.name)
                else:
                    exc_id = 0

            self.builder.emit(IRInstr(
                OpCode.EXC_PUSH,
                dst=Label(handler_label),
                src1=Immediate(exc_id, IRType.WORD),
            ))

        # Track handler count for this block
        handler_count = len(handlers)
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

            # Push arguments in reverse order
            for arg in reversed(effective_args):
                value = self._lower_expr(arg)
                self.builder.push(value)

            # Call (using external name for imported procedures)
            self.builder.call(Label(call_target))

            # Clean up stack
            if effective_args:
                # Pop arguments (2 bytes each)
                for _ in effective_args:
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)

    def _build_effective_args(self, provided_args: list, sym: Optional[Symbol]) -> list:
        """Build effective argument list with defaults for missing parameters.

        Returns a list of expressions to use as arguments.
        """
        if not sym or not sym.parameters:
            # No symbol info - just use provided args
            return [arg.value for arg in provided_args if arg.value]

        # Build list of effective argument expressions
        effective_args = []

        # Map provided args by position (for now, named args not handled)
        provided_by_pos = {i: arg.value for i, arg in enumerate(provided_args) if arg.value}

        param_index = 0
        for param in sym.parameters:
            # Each ParameterSpec can have multiple names (X, Y : Integer := 0)
            # For simplicity, count each name separately
            num_names = 1  # Assuming single name per param_symbol

            for name_idx in range(num_names):
                if param_index in provided_by_pos:
                    # Use provided argument
                    effective_args.append(provided_by_pos[param_index])
                elif param.ada_type is not None:
                    # Check for default value in the symbol
                    # Note: Symbol doesn't store default values directly,
                    # they're in the AST. Use a default zero for now.
                    # Full implementation would need to store defaults in Symbol
                    effective_args.append(IntegerLiteral(0))
                else:
                    # No default and no argument - use 0 as fallback
                    effective_args.append(IntegerLiteral(0))
                param_index += 1

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
            # Get single character into output parameter
            if args and args[0].value:
                self.builder.call(Label("_get_char"))
                # Result is in HL, store to output parameter
                # TODO: handle output parameter properly

        elif proc_name == "get_line":
            # Get line into output string
            # TODO: implement full get_line
            pass

    def _lower_case(self, stmt: CaseStmt) -> None:
        """Lower a case statement."""
        if self.ctx is None:
            return

        expr = self._lower_expr(stmt.expr)
        end_label = self._new_label("endcase")

        for i, alt in enumerate(stmt.alternatives):
            alt_label = self._new_label(f"case_{i}")
            next_label = self._new_label(f"case_{i}_next")

            # For each choice, compare and jump
            # For now, handle simple expression choices
            for choice in alt.choices:
                # Check if this is an "others" choice
                from uada80.ast_nodes import OthersChoice
                if isinstance(choice, OthersChoice):
                    # Always matches
                    alt_block = self.builder.new_block(alt_label)
                    self.builder.set_block(alt_block)
                    for s in alt.statements:
                        self._lower_statement(s)
                    self.builder.jmp(Label(end_label))
                    break

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

        Args:
            indexed: The indexed component expression
            base_addr: Virtual register holding the base address
            check_bounds: If True, emit runtime bounds checks
        """
        # For now, assume 1-dimensional array with word elements
        # Address = base + (index - lower_bound) * element_size

        # Lower the index expression
        index = self._lower_expr(indexed.indices[0])

        # Get array type info to find lower bound, upper bound, and element size
        lower_bound = 1  # Default Ada array lower bound
        upper_bound = None  # Unknown by default
        element_size = 2  # Default to word size

        # Try to get actual bounds from type info
        if isinstance(indexed.prefix, Identifier):
            sym = self.symbols.lookup(indexed.prefix.name)
            if sym and sym.ada_type and hasattr(sym.ada_type, 'bounds') and sym.ada_type.bounds:
                lower_bound = sym.ada_type.bounds[0][0]
                upper_bound = sym.ada_type.bounds[0][1]
            if sym and sym.ada_type and hasattr(sym.ada_type, 'component_type'):
                comp_type = sym.ada_type.component_type
                if comp_type:
                    element_size = (comp_type.size_bits + 7) // 8

        # Emit bounds check if requested and bounds are known
        if check_bounds and upper_bound is not None:
            self._emit_array_bounds_check(index, lower_bound, upper_bound)

        # Calculate offset: (index - lower_bound) * element_size
        offset_vreg = self.builder.new_vreg(IRType.WORD, "_idx_offset")

        if lower_bound != 0:
            # Subtract lower bound from index
            adjusted_idx = self.builder.new_vreg(IRType.WORD, "_adj_idx")
            self.builder.sub(adjusted_idx, index, Immediate(lower_bound, IRType.WORD))
            index = adjusted_idx

        # Multiply by element size
        if element_size == 1:
            self.builder.mov(offset_vreg, index)
        elif element_size == 2:
            # Shift left by 1 (multiply by 2)
            self.builder.add(offset_vreg, index, index)
        else:
            # General multiply
            self.builder.mul(offset_vreg, index, Immediate(element_size, IRType.WORD))

        # Add offset to base address
        elem_addr_vreg = self.builder.new_vreg(IRType.PTR, "_elem_addr")
        self.builder.add(elem_addr_vreg, base_addr, offset_vreg)

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
            # Convert floating point to 32-bit fixed point representation
            # Format: 16.16 fixed point (16 bits integer, 16 bits fraction)
            # This gives us a range of about -32768.9999 to 32767.9999
            # with precision of about 0.000015
            fixed_val = int(expr.value * 65536)  # Multiply by 2^16
            # Clamp to 32-bit signed range
            if fixed_val > 2147483647:
                fixed_val = 2147483647
            elif fixed_val < -2147483648:
                fixed_val = -2147483648
            # Return as two-word value (high word first for our calling convention)
            # For now, return the high word - full 32-bit support would need two regs
            high_word = (fixed_val >> 16) & 0xFFFF
            return Immediate(high_word, IRType.WORD)

        if isinstance(expr, CharacterLiteral):
            return Immediate(ord(expr.value), IRType.BYTE)

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
        # Use a mov from a special return register convention
        # For now, emit a placeholder that codegen will handle
        self.builder.mov(result, Immediate(0, IRType.PTR), comment="capture heap result from HL")

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

        # Load the value at the pointer address
        result = self.builder.new_vreg(IRType.WORD, "_deref")
        mem = MemoryLocation(base=ptr, offset=0, ir_type=IRType.WORD)
        self.builder.load(result, mem, comment="dereference .all")

        return result

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

        # Temporarily bind the loop variable in symbol table for predicate evaluation
        # For now, we push it as a parameter-like value
        self.builder.push(loop_var)

        # Evaluate the predicate
        pred_val = self._lower_expr(expr.predicate)

        # Pop the loop variable
        discard = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(discard)

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

    def _lower_aggregate(self, expr: Aggregate):
        """Lower an aggregate expression.

        For Z80, aggregates are built on the stack as temporary values.
        Returns a pointer to the aggregate in stack memory.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Determine size of aggregate based on number of components
        # For now, assume 2 bytes per component (word-sized)
        num_components = len(expr.components)
        size = num_components * 2

        # Allocate stack space for the aggregate
        agg_addr = self.builder.new_vreg(IRType.PTR, "_agg_tmp")

        # For a simple implementation, we just lower each component value
        # and store them sequentially

        # Track current offset in the aggregate
        offset = 0

        for comp in expr.components:
            # Lower the component value
            value = self._lower_expr(comp.value)

            # Check if this is a named association
            if comp.choices:
                # Named aggregate: (X => 1, Y => 2)
                # For record aggregates, we need to find the field offset
                for choice in comp.choices:
                    if isinstance(choice, ExprChoice):
                        if isinstance(choice.expr, Identifier):
                            # This is a named field - look up its offset
                            # For now, just use sequential offset
                            pass

            # Store the value at the current offset
            # This is a simplified implementation - just build sequentially
            mem = MemoryLocation(base=agg_addr, offset=offset, ir_type=IRType.WORD)
            self.builder.store(mem, value)
            offset += 2

        return agg_addr

    def _lower_aggregate_to_target(self, expr: Aggregate, target_addr, target_type):
        """Lower an aggregate directly to a target address.

        Used when the target type is known (e.g., record assignment).
        """
        if self.ctx is None:
            return

        # For record aggregates with known target type
        if isinstance(target_type, RecordType):
            # Map component names to offsets
            field_offsets = {}
            for comp in target_type.components:
                field_offsets[comp.name.lower()] = comp.offset_bits // 8

            # Process each component
            for comp_assoc in expr.components:
                value = self._lower_expr(comp_assoc.value)

                # Determine offset from field name
                offset = 0
                if comp_assoc.choices:
                    for choice in comp_assoc.choices:
                        if isinstance(choice, ExprChoice):
                            if isinstance(choice.expr, Identifier):
                                field_name = choice.expr.name.lower()
                                if field_name in field_offsets:
                                    offset = field_offsets[field_name]
                                    break

                # Store to the computed offset
                if offset != 0:
                    field_addr = self.builder.new_vreg(IRType.PTR, "_field")
                    self.builder.add(field_addr, target_addr, Immediate(offset, IRType.WORD))
                    mem = MemoryLocation(base=field_addr, offset=0, ir_type=IRType.WORD)
                else:
                    mem = MemoryLocation(base=target_addr, offset=0, ir_type=IRType.WORD)

                self.builder.store(mem, value)

        # For array aggregates
        elif isinstance(target_type, ArrayType):
            element_size = 2  # Default
            if target_type.component_type:
                element_size = (target_type.component_type.size_bits + 7) // 8

            # Get lower bound
            lower_bound = 1
            if target_type.bounds:
                lower_bound = target_type.bounds[0][0]

            # Process each component - positional for now
            for i, comp_assoc in enumerate(expr.components):
                value = self._lower_expr(comp_assoc.value)

                # Calculate offset for this element
                offset = i * element_size

                if offset != 0:
                    elem_addr = self.builder.new_vreg(IRType.PTR, "_elem")
                    self.builder.add(elem_addr, target_addr, Immediate(offset, IRType.WORD))
                    mem = MemoryLocation(base=elem_addr, offset=0, ir_type=IRType.WORD)
                else:
                    mem = MemoryLocation(base=target_addr, offset=0, ir_type=IRType.WORD)

                self.builder.store(mem, value)

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
        left = self._lower_expr(expr.left)
        right = self._lower_expr(expr.right)

        result = self.builder.new_vreg(IRType.WORD, "_tmp")

        op = expr.op
        if op == BinaryOp.ADD:
            self.builder.add(result, left, right)
        elif op == BinaryOp.SUB:
            self.builder.sub(result, left, right)
        elif op == BinaryOp.MUL:
            self.builder.mul(result, left, right)
        elif op == BinaryOp.DIV:
            self.builder.div(result, left, right)
        elif op == BinaryOp.AND or op == BinaryOp.AND_THEN:
            self.builder.and_(result, left, right)
        elif op == BinaryOp.OR or op == BinaryOp.OR_ELSE:
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
            # Result is in HL, move to result
            self.builder.mov(result, Immediate(0, IRType.WORD))  # placeholder
        elif op == BinaryOp.CONCAT:
            # String concatenation: call runtime function
            self.builder.push(right)  # second string
            self.builder.push(left)   # first string
            self.builder.call(Label("_str_concat"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            self.builder.pop(temp)
            self.builder.mov(result, Immediate(0, IRType.WORD))  # placeholder
        else:
            # Default: move left to result
            self.builder.mov(result, left)

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
            # For 16-bit: rotate is shift with wrap-around
            value = self._lower_expr(expr.args[0].value)
            amount = self._lower_expr(expr.args[1].value)
            # Simplified: use shift left (full rotate would need more code)
            self.builder.emit(IRInstr(OpCode.SHL, result, value, amount,
                                      comment="Rotate_Left (approx)"))
            return result

        # Handle Rotate_Right intrinsic
        if func_name == "rotate_right" and len(expr.args) >= 2:
            value = self._lower_expr(expr.args[0].value)
            amount = self._lower_expr(expr.args[1].value)
            self.builder.emit(IRInstr(OpCode.SHR, result, value, amount,
                                      comment="Rotate_Right (approx)"))
            return result

        if isinstance(expr.name, Identifier):
            # Resolve overloaded function
            sym = self._resolve_overload(expr.name.name, expr.args)

            # Determine the call target - use external name if imported
            call_target = expr.name.name
            if sym:
                if sym.is_imported and sym.external_name:
                    call_target = sym.external_name
                else:
                    call_target = sym.name

            # Push arguments
            for arg in reversed(expr.args):
                if arg.value:
                    value = self._lower_expr(arg.value)
                    self.builder.push(value)

            # Call (using external name for imported functions)
            self.builder.call(Label(call_target))

            # Result is in HL (move to result vreg)
            self.builder.emit(IRInstr(OpCode.MOV, result, VReg(id=-1, ir_type=IRType.WORD)))

            # Clean up stack
            for _ in expr.args:
                temp = self.builder.new_vreg(IRType.WORD)
                self.builder.pop(temp)

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
                    if isinstance(ada_type, ArrayType) and ada_type.is_constrained:
                        if ada_type.bounds:
                            low, high = ada_type.bounds[0]  # First dimension
                            if attr == "first":
                                return Immediate(low, IRType.WORD)
                            if attr == "last":
                                return Immediate(high, IRType.WORD)
                            if attr == "length":
                                length = high - low + 1
                                return Immediate(length, IRType.WORD)

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
            result = self.builder.new_vreg(IRType.PTR, "_image")
            self.builder.mov(result, Immediate(0, IRType.PTR), comment="capture result from HL")
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
            result = self.builder.new_vreg(IRType.PTR, "_img")
            self.builder.mov(result, Immediate(0, IRType.PTR), comment="capture result from HL")
            return result

        if attr == "value" and expr.args:
            # Type'Value(S) - converts string to value
            # For integers: parse decimal string
            arg_value = self._lower_expr(expr.args[0])
            self.builder.push(arg_value)
            self.builder.call(Label("_str_to_int"), comment="Integer'Value")
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            result = self.builder.new_vreg(IRType.WORD, "_value")
            self.builder.mov(result, Immediate(0, IRType.WORD), comment="capture result from HL")
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


def lower_to_ir(program: Program, semantic_result: SemanticResult) -> IRModule:
    """Lower a program to IR."""
    lowering = ASTLowering(semantic_result.symbols)
    return lowering.lower(program)
