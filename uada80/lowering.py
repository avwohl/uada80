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
    EntryBody,
    TaskTypeDecl,
    TaskBody,
    EntryDecl,
    SubtypeIndication,
    ArrayTypeDef,
    ActualParameter,
    IndexConstraint,
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
    is_atomic: bool = False  # pragma Atomic - wrap accesses in DI/EI
    is_volatile: bool = False  # pragma Volatile - no caching


@dataclass
class LoweringContext:
    """Context for lowering a function."""

    function: IRFunction
    locals: dict[str, LocalVariable] = field(default_factory=dict)
    params: dict[str, VReg] = field(default_factory=dict)
    # Parameters that are passed by reference (out, in out modes)
    # Maps param name (lowercase) -> True if byref
    byref_params: dict[str, bool] = field(default_factory=dict)
    # Parameter types: maps param name (lowercase) -> type name (lowercase)
    param_types: dict[str, str] = field(default_factory=dict)
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
    # For task bodies: the task's ID (for Current_Task attribute, etc.)
    task_id: Optional[VReg] = None
    # Stack size for locals
    locals_size: int = 0
    # For nested subprograms: enclosing context (None for top-level)
    enclosing_ctx: Optional["LoweringContext"] = None
    # For nested subprograms: vreg holding the static link (pointer to enclosing frame)
    static_link: Optional[VReg] = None
    # Subprogram name (for lookup in nested subprogram calls)
    subprogram_name: str = ""


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
        # Z80 interrupt handler table: maps interrupt vector to handler
        # Z80 vectors: RST 0 (0x00), RST 8 (0x08), ... RST 0x38, NMI (0x66)
        self._interrupt_handlers: dict[int, str] = {}
        # Procedures marked as interrupt handlers (need special prologue/epilogue)
        self._is_interrupt_handler: set[str] = set()
        # Track parameter modes for all subprograms (needed for out/in out at call sites)
        # Maps subprogram name (lowercase) -> list of parameter modes
        self._subprogram_param_modes: dict[str, list[str]] = {}
        # Track parameter names for all subprograms (needed for named parameter reordering)
        # Maps subprogram name (lowercase) -> list of parameter names in order
        self._subprogram_param_names: dict[str, list[str]] = {}
        # Track which parameters are passed by reference (byref mode or record type)
        # Maps subprogram name (lowercase) -> list of booleans (True if byref)
        self._subprogram_byref_params: dict[str, list[bool]] = {}
        # Track parameter default values for all subprograms
        # Maps subprogram name (lowercase) -> list of default value expressions (None if no default)
        self._subprogram_param_defaults: dict[str, list] = {}
        # Track which subprograms are nested (need static link at call sites)
        self._nested_subprograms: set[str] = set()
        # Stack of body declarations for nested scope lookup
        self._body_declarations_stack: list[list] = []
        # Track which outer variables each nested subprogram needs
        # Maps subprogram name -> set of outer variable names
        self._nested_outer_vars: dict[str, set[str]] = {}
        # Track overloaded functions for unique label generation
        # Maps function name (lowercase) -> count of overloads
        self._function_overload_count: dict[str, int] = {}
        # Maps (function name, param count) -> unique label name
        self._function_label_map: dict[tuple[str, int], str] = {}
        # Global variables (name -> value info)
        self.globals: dict[str, Any] = {}

    def _make_memory_location(
        self,
        symbol_name: str,
        is_global: bool = False,
        offset: int = 0,
        base: Optional[VReg] = None,
        ir_type: IRType = IRType.WORD,
        symbol: Optional[Symbol] = None
    ) -> MemoryLocation:
        """Create a MemoryLocation with atomic/volatile flags from symbol.

        This helper ensures that when we access variables marked with pragma Atomic
        or pragma Volatile, the MemoryLocation has the appropriate flags set so that
        codegen can emit DI/EI brackets for atomic access or avoid caching for volatile.

        Also handles explicit address clauses (for Obj'Address use N;) for
        memory-mapped hardware registers.
        """
        is_atomic = False
        is_volatile = False
        actual_symbol_name = symbol_name

        # Get atomic/volatile from provided symbol
        if symbol is None:
            symbol = self.symbols.lookup(symbol_name)

        if symbol:
            is_atomic = symbol.is_atomic
            is_volatile = symbol.is_volatile
            # Check for explicit address clause
            if symbol.explicit_address is not None:
                # Use the explicit address as the symbol name (numeric address)
                actual_symbol_name = f"0x{symbol.explicit_address:04X}"

        return MemoryLocation(
            base=base,
            offset=offset,
            is_global=is_global,
            symbol_name=actual_symbol_name,
            is_atomic=is_atomic,
            is_volatile=is_volatile,
            ir_type=ir_type,
        )

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
        """Generate a vtable for a tagged type.

        The vtable layout is:
        - Offset 0: Parent vtable pointer (null for root types) - for 'in T'Class' check
        - Offset 2+: Primitive operation addresses (one 16-bit address per slot)

        Each primitive operation has a fixed slot index that's consistent across
        the inheritance hierarchy, so derived types override at the same slot.
        """
        vtable_name = f"_vtable_{tagged_type.name}"
        primitives = tagged_type.all_primitives()

        # Add vtable as a global with procedure addresses
        # Each entry is 2 bytes (16-bit address on Z80)
        vtable_data: list[str] = []

        # First entry: parent vtable pointer (for membership tests like "X in T'Class")
        if tagged_type.parent_type and tagged_type.parent_type.is_tagged:
            parent_vtable = f"_vtable_{tagged_type.parent_type.name}"
        else:
            parent_vtable = "0"  # Null for root tagged types
        vtable_data.append(parent_vtable)

        # Build a map of operation names to their implementing type
        # This handles inheritance: use overridden version if present,
        # otherwise use inherited version
        for op in primitives:
            # Find the actual implementation - check if this type overrides it
            impl_name = None
            found_local = False

            # Check if this type has its own implementation
            for local_op in tagged_type.primitive_ops:
                if local_op.name.lower() == op.name.lower():
                    impl_name = f"{tagged_type.name}_{op.name}"
                    found_local = True
                    break

            if not found_local:
                # Use inherited implementation - walk up to find it
                search_type = tagged_type.parent_type
                while search_type and isinstance(search_type, RecordType):
                    for parent_op in search_type.primitive_ops:
                        if parent_op.name.lower() == op.name.lower():
                            impl_name = f"{search_type.name}_{op.name}"
                            break
                    if impl_name:
                        break
                    search_type = getattr(search_type, 'parent_type', None)

            if impl_name:
                vtable_data.append(impl_name)
            else:
                # Fallback to just the operation name
                vtable_data.append(op.name)

        # Store vtable info in the module for codegen
        self.builder.module.vtables[vtable_name] = vtable_data

    def _find_outer_var_refs(
        self, body: SubprogramBody, enclosing_ctx: LoweringContext, spec
    ) -> set[str]:
        """Find references to variables from enclosing scope in a nested subprogram.

        Walks the AST to find Identifier nodes that reference variables defined
        in the enclosing scope (not local to this subprogram or its parameters).
        """
        outer_vars = set()

        # Get the names defined locally in this subprogram
        local_names = set()
        for param_spec in spec.parameters:
            for name in param_spec.names:
                local_names.add(name.lower())
        for decl in body.declarations:
            if isinstance(decl, ObjectDecl):
                for name in decl.names:
                    local_names.add(name.lower())

        # Get the names from enclosing scope
        enclosing_locals = set(enclosing_ctx.locals.keys()) if enclosing_ctx else set()

        # Get current nested package name for prefixed lookup
        current_pkg = getattr(self, '_current_nested_package', None)

        # Walk the AST to find identifier references
        def visit(node):
            if node is None:
                return
            if isinstance(node, Identifier):
                name = node.name.lower()
                # If it's not local and is in enclosing scope, it's an outer ref
                if name not in local_names:
                    if name in enclosing_locals:
                        outer_vars.add(name)
                    # Also check for package-prefixed name (for nested package variables)
                    elif current_pkg:
                        prefixed_name = f"{current_pkg}.{name}"
                        if prefixed_name in enclosing_locals:
                            outer_vars.add(prefixed_name)
            # Recursively visit child nodes
            for attr_name in dir(node):
                if attr_name.startswith('_'):
                    continue
                attr = getattr(node, attr_name, None)
                if attr is None:
                    continue
                if isinstance(attr, list):
                    for item in attr:
                        if hasattr(item, '__dict__'):
                            visit(item)
                elif hasattr(attr, '__dict__') and not callable(attr):
                    visit(attr)

        # Visit all statements
        for stmt in body.statements:
            visit(stmt)

        return outer_vars

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

        # Enter the subprogram's scope for symbol lookup
        # This mirrors what the semantic analyzer does
        self.symbols.enter_scope(spec.name)

        # Determine return type
        return_type = IRType.VOID
        if spec.is_function and spec.return_type:
            if sym and sym.return_type:
                return_type = self._ada_type_to_ir(sym.return_type)

        # Save builder state (for nested function support)
        old_function = self.builder.function
        old_block = self.builder.block
        old_ctx = self.ctx
        old_body_declarations = getattr(self, '_current_body_declarations', None)

        # Detect if this is a nested subprogram (enclosing context exists)
        is_nested = old_ctx is not None

        # Generate unique function label for overloaded functions
        # Use (name, param_count) to distinguish overloads
        func_name = spec.name
        func_name_lower = spec.name.lower()
        param_count = sum(len(ps.names) for ps in spec.parameters)
        label_key = (func_name_lower, param_count)

        if label_key in self._function_label_map:
            # Already have a label for this overload - this is a redefinition (error?)
            # But for now, generate a unique one
            func_label = self._function_label_map[label_key]
        else:
            # Check if this name was used before (different overload)
            if func_name_lower in self._function_overload_count:
                # This is an overload - append suffix
                count = self._function_overload_count[func_name_lower] + 1
                self._function_overload_count[func_name_lower] = count
                func_label = f"{func_name}_{count}"
            else:
                # First occurrence - use name as-is
                self._function_overload_count[func_name_lower] = 0
                func_label = func_name
            self._function_label_map[label_key] = func_label

        # Create function with unique label
        func = self.builder.new_function(func_label, return_type)

        # Create new context (still use original name for scope lookup)
        self.ctx = LoweringContext(function=func, subprogram_name=spec.name.lower())

        # If nested, set up outer variable access (identify vars first, add params later)
        outer_var_refs = set()
        if is_nested:
            self.ctx.enclosing_ctx = old_ctx
            # Find outer-scope variables referenced in this subprogram
            outer_var_refs = self._find_outer_var_refs(body, old_ctx, spec)
            # Track which outer variables this subprogram needs
            self._nested_subprograms.add(spec.name.lower())
            self._nested_outer_vars[spec.name.lower()] = outer_var_refs

        # Process parameters and record their modes, names, defaults, and byref info for call sites
        param_modes = []
        param_names = []
        param_defaults = []
        for param_spec in spec.parameters:
            self._lower_parameter(param_spec)
            # Record mode, name, and default for each parameter (a param_spec can have multiple names)
            # Get default value if present
            default_val = getattr(param_spec, 'default_value', None)
            for pname in param_spec.names:
                param_modes.append(param_spec.mode or "in")
                param_names.append(pname.lower() if isinstance(pname, str) else pname.name.lower())
                param_defaults.append(default_val)
        # Store parameter modes, names, and defaults for this subprogram (used when lowering calls)
        self._subprogram_param_modes[spec.name.lower()] = param_modes
        self._subprogram_param_names[spec.name.lower()] = param_names
        self._subprogram_param_defaults[spec.name.lower()] = param_defaults
        # Store byref info (from ctx.byref_params) for call sites
        byref_list = [pname in self.ctx.byref_params for pname in param_names]
        self._subprogram_byref_params[spec.name.lower()] = byref_list

        # Add outer variable pointer parameters AFTER regular params
        # (they are pushed before regular args at call site, so end up at higher offsets)
        if outer_var_refs:
            for var_name in outer_var_refs:
                param_name = f"_outer_{var_name}"
                ptr_vreg = self.builder.new_vreg(IRType.PTR, param_name)
                self.ctx.params[param_name] = ptr_vreg
                self.ctx.byref_params[param_name] = True  # Mark as byref for stores
                func.params.append(ptr_vreg)

        # Store declarations for later lookup (used by _get_field_offset)
        # Push to stack for nested scope lookup
        self._body_declarations_stack.append(body.declarations)
        self._current_body_declarations = body.declarations

        # First pass: collect pragma Atomic/Volatile annotations for variables
        atomic_vars: set[str] = set()
        volatile_vars: set[str] = set()
        for decl in body.declarations:
            if isinstance(decl, PragmaStmt):
                pragma_name = decl.name.lower() if decl.name else ""
                if pragma_name == "atomic" and decl.args:
                    for arg in decl.args:
                        if isinstance(arg, Identifier):
                            atomic_vars.add(arg.name.lower())
                            volatile_vars.add(arg.name.lower())  # Atomic implies volatile
                elif pragma_name == "volatile" and decl.args:
                    for arg in decl.args:
                        if isinstance(arg, Identifier):
                            volatile_vars.add(arg.name.lower())

        # Calculate local variable sizes
        stack_offset = 0

        def allocate_object_decl(obj_decl: ObjectDecl, prefix: str = "") -> int:
            """Allocate stack space for an object declaration. Returns new stack_offset."""
            nonlocal stack_offset
            for name in obj_decl.names:
                full_name = f"{prefix}{name}" if prefix else name
                size = self._calc_type_size(obj_decl, body.declarations)
                # Check if this variable has atomic/volatile from pragma
                is_atomic = full_name.lower() in atomic_vars
                is_volatile = full_name.lower() in volatile_vars
                vreg = self.builder.new_vreg(IRType.WORD, full_name, is_atomic=is_atomic, is_volatile=is_volatile)
                self.ctx.locals[full_name.lower()] = LocalVariable(
                    name=full_name,
                    vreg=vreg,
                    stack_offset=stack_offset,
                    size=size,
                    ada_type=obj_decl.type_mark if hasattr(obj_decl, 'type_mark') else None,
                    is_atomic=is_atomic,
                    is_volatile=is_volatile,
                )
                stack_offset += size
            return stack_offset

        for decl in body.declarations:
            if isinstance(decl, ObjectDecl):
                allocate_object_decl(decl)
            elif isinstance(decl, PackageDecl):
                # Nested package spec - allocate space for its variables
                pkg_prefix = decl.name + "." if decl.name else ""
                for pkg_decl in decl.declarations:
                    if isinstance(pkg_decl, ObjectDecl):
                        allocate_object_decl(pkg_decl, pkg_prefix)
                for pkg_decl in decl.private_declarations:
                    if isinstance(pkg_decl, ObjectDecl):
                        allocate_object_decl(pkg_decl, pkg_prefix)

        func.locals_size = stack_offset
        self.ctx.locals_size = stack_offset

        # Create entry block - use func.name which is the unique label
        entry = self.builder.new_block(f"{func.name}_entry")
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

        # Restore context and builder state, and leave scope
        self.ctx = old_ctx
        self.builder.function = old_function
        self.builder.block = old_block
        # Pop from stack and restore old declarations
        if self._body_declarations_stack:
            self._body_declarations_stack.pop()
        self._current_body_declarations = old_body_declarations
        self.symbols.leave_scope()

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
        """Lower a parameter specification.

        For unconstrained array parameters (like String), we pass a dope vector
        containing: (data_ptr, first, last) - 6 bytes total for 1D arrays.
        This allows the called function to access bounds at runtime.

        For out and in out parameters, we pass by reference (address of the variable).
        The callee receives a pointer and dereferences it to read/write the value.
        """
        if self.ctx is None:
            return

        # Check if this is a pass-by-reference parameter (out or in out mode)
        is_byref = param.mode in ("out", "in out")

        # Check if parameter type is unconstrained array or record
        param_type = None
        type_name_str = None
        if param.type_mark:
            type_name = param.type_mark
            if isinstance(type_name, Identifier):
                type_name_str = type_name.name
            elif isinstance(type_name, SelectedName):
                type_name_str = type_name.selector  # Use just the type name part
            else:
                type_name_str = str(type_name)

            # Try global symbol table first
            type_sym = self.symbols.lookup(type_name_str)
            if type_sym and type_sym.ada_type:
                param_type = type_sym.ada_type

            # Also check local type declarations
            if param_type is None and hasattr(self, '_current_body_declarations') and self._current_body_declarations:
                from uada80.type_system import RecordType as RecordTypeClass
                for d in self._current_body_declarations:
                    if isinstance(d, TypeDecl) and d.name.lower() == type_name_str.lower():
                        type_def = d.type_def
                        from uada80.ast_nodes import RecordTypeDef
                        if isinstance(type_def, RecordTypeDef):
                            # Create a RecordType for this local type
                            from uada80.type_system import RecordComponent, IntegerType
                            components = []
                            offset = 0
                            for comp_decl in type_def.components:
                                for comp_name in comp_decl.names:
                                    comp_type = IntegerType(name="Integer", size_bits=16)
                                    components.append(RecordComponent(
                                        name=comp_name,
                                        component_type=comp_type,
                                        offset_bits=offset * 8,
                                        size_bits=16,
                                    ))
                                    offset += 2
                            param_type = RecordTypeClass(
                                name=type_name_str,
                                components=components,
                                size_bits=offset * 8,
                            )
                        break

        is_unconstrained = (
            param_type and
            isinstance(param_type, ArrayType) and
            not param_type.is_constrained
        )

        from uada80.type_system import RecordType as RecordTypeClass
        is_record = param_type and isinstance(param_type, RecordTypeClass)

        for name in param.names:
            # Track parameter type for field offset lookup
            if type_name_str:
                self.ctx.param_types[name.lower()] = type_name_str.lower()

            if is_unconstrained:
                # Unconstrained array: pass dope vector (ptr, first, last)
                # Create three vregs for the dope vector components
                ptr_vreg = self.builder.new_vreg(IRType.PTR, f"{name}_ptr")
                first_vreg = self.builder.new_vreg(IRType.WORD, f"{name}_first")
                last_vreg = self.builder.new_vreg(IRType.WORD, f"{name}_last")

                # Store in params dict with special marker
                self.ctx.params[name.lower()] = ptr_vreg
                self.ctx.params[f"{name.lower()}'first"] = first_vreg
                self.ctx.params[f"{name.lower()}'last"] = last_vreg

                # Add all three to function params (in order: ptr, first, last)
                self.ctx.function.params.append(ptr_vreg)
                self.ctx.function.params.append(first_vreg)
                self.ctx.function.params.append(last_vreg)
            elif is_byref or is_record:
                # Out/in out parameter or record type: pass by reference (pointer)
                vreg = self.builder.new_vreg(IRType.PTR, f"{name}_ptr")
                self.ctx.params[name.lower()] = vreg
                self.ctx.byref_params[name.lower()] = True
                self.ctx.function.params.append(vreg)
            else:
                # Constrained type with in mode: single value
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
            if decl.type_mark:
                ada_type = self._resolve_type(decl.type_mark)
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
                # Store to global variable (with atomic/volatile flags)
                global_mem = self._make_memory_location(
                    global_name, is_global=True, ir_type=IRType.WORD
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

        # If symbol table lookup fails, search current body declarations directly
        # This handles local generic definitions that are in a different scope
        if generic_sym is None and hasattr(self, '_current_body_declarations') and self._current_body_declarations:
            from uada80.ast_nodes import GenericSubprogramUnit
            for decl in self._current_body_declarations:
                if isinstance(decl, GenericSubprogramUnit) and decl.name.lower() == generic_name.lower():
                    # Found the generic - create a pseudo-symbol for it
                    # Also find the corresponding body
                    generic_body = None
                    for body_decl in self._current_body_declarations:
                        if isinstance(body_decl, SubprogramBody) and body_decl.spec.name.lower() == generic_name.lower():
                            generic_body = body_decl
                            break
                    # Lower the instantiation directly
                    if inst.kind in ("procedure", "function"):
                        self._lower_generic_subprogram_from_ast(inst, decl, generic_body)
                    return

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

    def _lower_generic_subprogram_from_ast(self, inst: GenericInstantiation,
                                            gen_unit: 'GenericSubprogramUnit',
                                            body: Optional[SubprogramBody]) -> None:
        """Lower a generic subprogram instantiation from AST nodes directly.

        This is used when the generic is defined in the same declarative region
        and the symbol table lookup fails due to scope issues.
        """
        if body is None:
            # No body to instantiate
            return

        # Get generic formals
        formals = gen_unit.formals if hasattr(gen_unit, 'formals') else []

        # Build type mapping from formal to actual parameters
        type_map: dict[str, str] = {}
        if formals:
            type_map = self._build_generic_type_map(formals, inst.actual_parameters)

        # Store the type map for later use during expression lowering
        self._generic_type_map = type_map
        self._generic_prefix = inst.name

        # Create instantiated subprogram with the new name
        original_name = body.spec.name
        body.spec.name = inst.name
        self._lower_subprogram_body(body)
        body.spec.name = original_name  # Restore original name

        # Clear the type map
        self._generic_type_map = {}
        self._generic_prefix = None

    def _build_generic_type_map(self, formals: list, actuals: list) -> dict[str, str]:
        """Build a mapping from formal generic parameters to actual parameters.

        Handles:
        - Type parameters: formal type -> actual type name
        - Subprogram parameters: formal subprogram -> actual subprogram name
        - Object parameters: formal object -> actual object
        """
        type_map: dict[str, str] = {}
        actual_idx = 0

        for formal in formals:
            if actual_idx >= len(actuals):
                break

            formal_name = formal.name if hasattr(formal, "name") else str(formal)
            actual = actuals[actual_idx]

            # Get actual parameter name (handles both positional and named)
            if hasattr(actual, 'selector') and actual.selector:
                # Named association: Formal => Actual
                actual_name = (
                    actual.value.name
                    if hasattr(actual.value, "name")
                    else str(actual.value)
                )
            else:
                # Positional association
                actual_name = (
                    actual.value.name
                    if hasattr(actual.value, "name")
                    else str(actual.value)
                )

            # Handle different kinds of generic formals
            from uada80.ast_nodes import GenericSubprogramDecl, GenericTypeDecl, GenericObjectDecl

            if isinstance(formal, GenericSubprogramDecl):
                # Formal subprogram -> actual subprogram mapping
                # The actual is a subprogram name to call instead of the formal
                type_map[formal_name.lower()] = actual_name
                # Also store with _subp suffix for disambiguation during call lowering
                type_map[f"_subp_{formal_name.lower()}"] = actual_name
            elif isinstance(formal, GenericTypeDecl):
                # Formal type -> actual type mapping
                type_map[formal_name.lower()] = actual_name
            elif isinstance(formal, GenericObjectDecl):
                # Formal object -> actual value/variable
                type_map[formal_name.lower()] = actual_name
            else:
                # Default: just map the name
                type_map[formal_name.lower()] = actual_name

            actual_idx += 1

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
        elif isinstance(decl, TaskTypeDecl):
            self._lower_task_type_decl(decl)
        elif isinstance(decl, TaskBody):
            self._lower_task_body(decl)
        elif isinstance(decl, GenericInstantiation):
            # Generic instantiation in local scope
            self._lower_generic_instantiation(decl)
        elif isinstance(decl, PackageDecl):
            # Nested package spec - lower initializers for its variables
            self._lower_nested_package_decl(decl)
        elif isinstance(decl, PackageBody):
            # Nested package body - lower its subprogram bodies
            self._lower_nested_package_body(decl)
        # Representation clauses are processed during type analysis
        # and affect memory layout, not code generation directly

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
        type_name = None
        if decl.type_mark:
            # Handle SubtypeIndication (normal case) vs ArrayTypeDef (anonymous array)
            if isinstance(decl.type_mark, SubtypeIndication) and decl.type_mark.type_mark:
                if isinstance(decl.type_mark.type_mark, Identifier):
                    type_name = decl.type_mark.type_mark.name
                    type_sym = self.symbols.lookup(type_name)
                    if type_sym:
                        ada_type = type_sym.ada_type
            elif isinstance(decl.type_mark, ArrayTypeDef):
                # Anonymous array type - build ArrayType from definition
                from uada80.type_system import ArrayType, IntegerType
                array_def = decl.type_mark
                bounds = []
                if array_def.index_subtypes:
                    for idx_range in array_def.index_subtypes:
                        if isinstance(idx_range, RangeExpr):
                            low = self._eval_static_expr(idx_range.low)
                            high = self._eval_static_expr(idx_range.high)
                            bounds.append((low, high))
                comp_type = IntegerType(name="Integer", size_bits=16)
                ada_type = ArrayType(
                    name="<anonymous>",
                    component_type=comp_type,
                    bounds=bounds,
                    is_constrained=True,
                )
            elif isinstance(decl.type_mark, Identifier):
                # Direct type name
                type_name = decl.type_mark.name
                type_sym = self.symbols.lookup(type_name)
                if type_sym:
                    ada_type = type_sym.ada_type

        # Fallback: look up locally-declared types from _current_body_declarations
        if ada_type is None and type_name and hasattr(self, '_current_body_declarations'):
            from uada80.ast_nodes import RecordTypeDef
            from uada80.type_system import RecordType, RecordComponent, IntegerType, ArrayType
            for d in self._current_body_declarations:
                if isinstance(d, TypeDecl) and d.name.lower() == type_name.lower():
                    type_def = d.type_def
                    if isinstance(type_def, RecordTypeDef):
                        # Build a simple RecordType from the AST RecordTypeDef
                        components = []
                        offset = 0
                        for comp_decl in type_def.components:
                            # Resolve the actual component type
                            comp_type = self._resolve_local_type(comp_decl.type_mark)
                            if comp_type is None:
                                comp_type = IntegerType(name="Integer", size_bits=16)
                            comp_size = (comp_type.size_bits + 7) // 8 if comp_type.size_bits else 2
                            for comp_name in comp_decl.names:
                                components.append(RecordComponent(
                                    name=comp_name,
                                    component_type=comp_type,
                                    offset_bits=offset * 8,
                                    size_bits=comp_type.size_bits if comp_type.size_bits else 16,
                                    default_value=comp_decl.default_value,  # Capture default
                                ))
                                offset += comp_size
                        ada_type = RecordType(
                            name=type_name,
                            components=components,
                            size_bits=offset * 8,
                        )
                    elif isinstance(type_def, ArrayTypeDef):
                        # Build a simple ArrayType from the AST ArrayTypeDef
                        bounds = []
                        if type_def.index_subtypes:
                            # Handle all dimensions for multi-dimensional arrays
                            for idx_range in type_def.index_subtypes:
                                if isinstance(idx_range, RangeExpr):
                                    low = self._eval_static_expr(idx_range.low)
                                    high = self._eval_static_expr(idx_range.high)
                                    bounds.append((low, high))
                        # Resolve component type - may be record, array, or scalar
                        comp_type = self._resolve_local_type(type_def.component_type)
                        if comp_type is None:
                            comp_type = IntegerType(name="Integer", size_bits=16)
                        ada_type = ArrayType(
                            name=type_name,
                            component_type=comp_type,
                            bounds=bounds,
                            is_constrained=True,
                        )
                    break

        # Process initialization
        if decl.init_expr:
            for name in decl.names:
                local = self.ctx.locals.get(name.lower())
                if local:
                    # For record/array aggregates, write directly to the local variable
                    if isinstance(decl.init_expr, Aggregate) and ada_type:
                        # Get address of local variable
                        # Calculate frame offset: -(locals_size - stack_offset)
                        # This matches how field access computes record addresses
                        frame_offset = -(self.ctx.locals_size - local.stack_offset)
                        local_addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                        self.builder.emit(IRInstr(
                            OpCode.LEA,
                            dst=local_addr,
                            src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                        ))
                        # Write aggregate directly to the local
                        self._lower_aggregate_to_target(decl.init_expr, local_addr, ada_type)
                    elif self._is_float64_type(ada_type):
                        # Float64 initialization - copy 8 bytes to local
                        src_ptr = self._lower_float64_operand(decl.init_expr)
                        frame_offset = -(self.ctx.locals_size - local.stack_offset)
                        local_addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                        self.builder.emit(IRInstr(
                            OpCode.LEA,
                            dst=local_addr,
                            src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                        ))
                        # Copy 8 bytes from source to local
                        self.builder.push(src_ptr)
                        self.builder.push(local_addr)
                        self.builder.call(Label("_f64_copy"))
                        discard = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(discard)
                        self.builder.pop(discard)
                    else:
                        init_value = self._lower_expr(decl.init_expr)
                        self.builder.mov(local.vreg, init_value,
                                        comment=f"init {name}")

                    # For controlled types with initializer, call Adjust
                    # (Initialize is for default initialization only)
                    if ada_type and self._type_needs_adjustment(ada_type):
                        self._call_adjust(local.vreg, ada_type)

                    # Check type invariant after initialization
                    if decl.type_mark and isinstance(decl.type_mark, SubtypeIndication) and decl.type_mark.type_mark:
                        self._check_type_invariant(local.vreg, decl.type_mark.type_mark)
        else:
            # Default initialization
            from uada80.type_system import RecordType
            from uada80.ast_nodes import DiscriminantConstraint
            for name in decl.names:
                local = self.ctx.locals.get(name.lower())
                if local:
                    # Initialize discriminants from constraint (e.g., B : Buffer(10))
                    # Handle two cases:
                    # 1. DiscriminantConstraint: decl.type_mark.constraint is DiscriminantConstraint
                    # 2. IndexedComponent: decl.type_mark.type_mark is IndexedComponent (parser treats Buffer(10) as indexing)
                    disc_values = []
                    if isinstance(decl.type_mark, SubtypeIndication):
                        if decl.type_mark.constraint and isinstance(decl.type_mark.constraint, DiscriminantConstraint):
                            # Explicit DiscriminantConstraint
                            for disc_name, disc_value in decl.type_mark.constraint.discriminants:
                                disc_values.append(disc_value)
                        elif isinstance(decl.type_mark.type_mark, IndexedComponent):
                            # Parser treated Buffer(10) as IndexedComponent - extract indices as discriminant values
                            indexed = decl.type_mark.type_mark
                            # Check if the prefix is a discriminated record type
                            type_name_for_disc = None
                            if isinstance(indexed.prefix, Identifier):
                                type_name_for_disc = indexed.prefix.name.lower()
                            # Look up to see if this type has discriminants
                            has_discriminants = False
                            if type_name_for_disc and hasattr(self, '_current_body_declarations'):
                                for td in self._current_body_declarations:
                                    if isinstance(td, TypeDecl) and td.name.lower() == type_name_for_disc:
                                        if hasattr(td, 'discriminants') and td.discriminants:
                                            has_discriminants = True
                                        break
                            if has_discriminants and indexed.indices:
                                for idx in indexed.indices:
                                    disc_values.append(idx)

                    if disc_values:
                        # Get address of local variable
                        frame_offset = -(self.ctx.locals_size - local.stack_offset)
                        local_addr = self.builder.new_vreg(IRType.PTR, f"_{name}_disc_addr")
                        self.builder.emit(IRInstr(
                            OpCode.LEA,
                            dst=local_addr,
                            src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                        ))
                        # Store each discriminant value
                        disc_offset = 0
                        for disc_value in disc_values:
                            value = self._lower_expr(disc_value)
                            disc_mem = MemoryLocation(
                                base=local_addr,
                                offset=disc_offset,
                                ir_type=IRType.WORD,
                            )
                            self.builder.emit(IRInstr(
                                OpCode.STORE,
                                dst=disc_mem,
                                src1=value,
                                comment=f"init discriminant = {disc_value}",
                            ))
                            disc_offset += 2  # Each discriminant is 2 bytes

                    # For record types with component defaults, initialize each component
                    if isinstance(ada_type, RecordType) and ada_type.components:
                        has_defaults = any(c.default_value is not None for c in ada_type.components)
                        if has_defaults:
                            # Get address of local variable
                            frame_offset = -(self.ctx.locals_size - local.stack_offset)
                            local_addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                            self.builder.emit(IRInstr(
                                OpCode.LEA,
                                dst=local_addr,
                                src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                            ))
                            # Initialize each component with default value
                            for comp in ada_type.components:
                                if comp.default_value is not None:
                                    # Lower the default expression
                                    value = self._lower_expr(comp.default_value)
                                    # Calculate component offset
                                    comp_offset_bytes = comp.offset_bits // 8
                                    # Create MemoryLocation with base=local_addr and offset
                                    comp_mem = MemoryLocation(
                                        base=local_addr,
                                        offset=comp_offset_bytes,
                                        ir_type=IRType.WORD,
                                    )
                                    # Store the value to the memory location
                                    self.builder.emit(IRInstr(
                                        OpCode.STORE,
                                        dst=comp_mem,
                                        src1=value,
                                        comment=f"init {name}.{comp.name}",
                                    ))
                    # Call Initialize for controlled types
                    if ada_type and self._type_needs_finalization(ada_type):
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
                self.builder.module.add_global(exc_label, IRType.WORD, 2)

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
        """Register a controlled object for finalization at scope exit.

        Uses the runtime finalization chain (_fin_register) so that
        finalization happens correctly even on exceptions.
        """
        if self.ctx is None:
            return

        # Get object address
        obj_addr = self.builder.new_vreg(IRType.PTR, "_fin_obj")
        obj_mem = MemoryLocation(base=local.vreg, offset=0, ir_type=IRType.PTR)
        self.builder.lea(obj_addr, obj_mem, comment=f"addr of {name}")

        # Get finalize procedure address
        fin_name = f"{ada_type.name}_Finalize"
        fin_addr = self.builder.new_vreg(IRType.PTR, "_fin_proc")
        fin_mem = MemoryLocation(is_global=True, symbol_name=fin_name, ir_type=IRType.PTR)
        self.builder.lea(fin_addr, fin_mem, comment=f"Finalize proc for {ada_type.name}")

        # Call _fin_register(obj_addr, fin_addr)
        # Calling convention: HL = obj_ptr, DE = fin_proc
        # Store obj_addr to HL
        self.builder.emit(IRInstr(
            OpCode.MOV,
            MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
            obj_addr,
            comment="obj ptr to HL"
        ))
        # Store fin_addr to DE
        self.builder.emit(IRInstr(
            OpCode.MOV,
            MemoryLocation(is_global=False, symbol_name="_DE", ir_type=IRType.WORD),
            fin_addr,
            comment="fin proc to DE"
        ))
        self.builder.call(Label("_fin_register"), comment="register for finalization")

    def _emit_scope_push(self) -> None:
        """Emit a scope push for controlled types finalization."""
        self.builder.call(Label("_fin_push_scope"), comment="push finalization scope")

    def _emit_scope_pop(self) -> None:
        """Emit a scope pop that finalizes all controlled objects in current scope."""
        self.builder.call(Label("_fin_pop_scope"), comment="pop finalization scope")

    def _generate_finalizations(self) -> None:
        """Generate finalization by popping the scope (calls all Finalize in reverse order)."""
        if self.ctx is None:
            return
        # The scope-based finalization handles the LIFO ordering automatically
        self._emit_scope_pop()

    def _check_discriminant_constraints(self, obj_ptr, ada_type, disc_values: dict) -> None:
        """Check discriminant constraints for a record object.

        obj_ptr: pointer to the record object
        ada_type: the record type
        disc_values: dict mapping discriminant name -> value (vreg or immediate)
        """
        from uada80.type_system import RecordType

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
            elif isinstance(item, EntryBody):
                self._lower_protected_entry(decl.name, item, prot_sym.ada_type)

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
        ir_func = IRFunction(name=op_name, return_type=IRType.VOID)
        if self.builder.module:
            self.builder.module.functions.append(ir_func)

        # Create entry block
        entry_block = BasicBlock(label=f"L_{op_name}_entry")
        ir_func.blocks.append(entry_block)
        self.builder.set_function(ir_func)
        self.builder.set_block(entry_block)

        # Create new context
        self.ctx = LoweringContext(function=ir_func, subprogram_name=op_name)

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

    def _lower_protected_entry(self, prot_name: str, entry: EntryBody, prot_type: ProtectedType) -> None:
        """Lower a protected entry body with barrier.

        Protected entries have a barrier condition that must be true for callers
        to be served. If the barrier is false, callers are queued and serviced
        later when the barrier becomes true (re-evaluated on each exit from a
        protected operation).

        Entry families are entries indexed by a discrete range:
            entry Get_Item(for I in 1..10) when Ready(I) is ...

        Each index value has its own queue and barrier condition that may
        depend on the index.

        Generated code:
        1. Entry point that checks barrier (with index for families)
        2. If barrier false, queue caller and yield
        3. If barrier true, acquire lock and execute body
        4. Release lock and return
        """
        # Save current context
        old_ctx = self.ctx

        # Create the entry name (Protected_Name.Entry_Name)
        entry_name = f"{prot_name}_{entry.name}"

        # Create IR function for this entry
        ir_func = IRFunction(name=entry_name, return_type=IRType.VOID)
        if self.builder.module:
            self.builder.module.functions.append(ir_func)

        # Create entry block
        entry_block = BasicBlock(label=f"L_{entry_name}_start")
        ir_func.blocks.append(entry_block)
        self.builder.set_function(ir_func)
        self.builder.set_block(entry_block)

        # Create new context
        self.ctx = LoweringContext(function=ir_func)

        # The protected object address is passed as first implicit parameter
        prot_obj = self.builder.new_vreg(IRType.PTR, "_protected_obj")
        self.builder.pop(prot_obj)

        # Handle entry family index if present
        # For entry families, the index is passed as the second implicit parameter
        family_index_vreg = None
        if entry.family_index:
            # Pop the family index from stack
            family_index_vreg = self.builder.new_vreg(IRType.WORD, "_family_index")
            self.builder.pop(family_index_vreg)
            # Make the family index available as a local variable
            # so it can be used in the barrier expression and body
            family_local = LocalVariable(
                name=entry.family_index,
                vreg=family_index_vreg,
                stack_offset=self.ctx.locals_size if self.ctx else 0,
                size=2,
                ada_type=None  # Type from family_index range
            )
            if self.ctx:
                self.ctx.locals[entry.family_index.lower()] = family_local

        # Check barrier if present
        barrier_recheck_label = self._new_label("barrier_recheck")
        barrier_ok_label = self._new_label("barrier_ok")

        # Entry point for barrier rechecking
        barrier_block = self.builder.new_block(barrier_recheck_label)
        self.builder.jmp(Label(barrier_recheck_label))
        self.builder.set_block(barrier_block)

        if entry.barrier:
            # Acquire lock briefly to evaluate barrier
            self.builder.push(prot_obj)
            self.builder.call(Label("_protected_lock"))
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)

            # Evaluate barrier condition (may reference family index)
            barrier_val = self._lower_expr(entry.barrier)

            # Release lock after barrier evaluation
            self.builder.push(prot_obj)
            self.builder.call(Label("_protected_unlock"))
            self.builder.pop(temp)

            # If barrier true, continue; else queue and wait
            self.builder.jnz(barrier_val, Label(barrier_ok_label))

            # Barrier is false - queue this call and yield
            queue_label = self._new_label("entry_queue")
            queue_block = self.builder.new_block(queue_label)
            self.builder.set_block(queue_block)

            # Queue the entry call (runtime will re-evaluate barrier later)
            # For entry families, we need to store the family index with the queue entry
            self.builder.push(prot_obj)
            # Entry ID combines base entry ID with family index
            base_entry_id = hash(entry.name) & 0xFFFF
            if family_index_vreg:
                # entry_id = base_id * 256 + family_index
                # This allows up to 256 indices per family (adjustable)
                scaled_base = self.builder.new_vreg(IRType.WORD, "_entry_id")
                self.builder.emit(IRInstr(
                    OpCode.MUL, scaled_base,
                    Immediate(base_entry_id, IRType.WORD),
                    Immediate(256, IRType.WORD),
                    comment="scale base entry id"
                ))
                combined_id = self.builder.new_vreg(IRType.WORD, "_combined_id")
                self.builder.emit(IRInstr(
                    OpCode.ADD, combined_id, scaled_base, family_index_vreg,
                    comment="combine with family index"
                ))
                self.builder.push(combined_id)
            else:
                self.builder.push(Immediate(base_entry_id, IRType.WORD))
            self.builder.call(Label("_protected_entry_queue"))
            self.builder.pop(temp)
            self.builder.pop(temp)

            # Jump back to recheck barrier
            self.builder.jmp(Label(barrier_recheck_label))

        # Barrier OK block - acquire lock and execute
        ok_block = self.builder.new_block(barrier_ok_label)
        self.builder.set_block(ok_block)

        # Acquire lock for body execution
        self.builder.push(prot_obj)
        self.builder.call(Label("_protected_lock"))
        temp = self.builder.new_vreg(IRType.WORD, "_temp")
        self.builder.pop(temp)

        # Set up parameters
        self._setup_parameters(entry.parameters)

        # Lower declarations
        for d in entry.decls:
            self._lower_declaration(d)

        # Lower statements
        for stmt in entry.stmts:
            self._lower_statement(stmt)

        # Release lock
        self.builder.push(prot_obj)
        self.builder.call(Label("_protected_unlock"))
        self.builder.pop(temp)

        # Re-evaluate all entry barriers (someone may now be able to proceed)
        self.builder.push(prot_obj)
        self.builder.call(Label("_protected_reeval_barriers"))
        self.builder.pop(temp)

        # Return
        self.builder.ret()

        # Restore context
        self.ctx = old_ctx

    def _lower_renaming_decl(self, decl: ObjectDecl) -> None:
        """Lower a renaming declaration (X : T renames Y).

        A renaming doesn't allocate new storage - X becomes an alias for Y.
        For simple identifiers, we make the renamed variable share the same vreg
        as the original, so all accesses go to the same location.
        """
        if self.ctx is None:
            return

        renamed_expr = decl.renames

        if isinstance(renamed_expr, Identifier):
            # Simple identifier renaming - share the vreg
            name_lower = renamed_expr.name.lower()
            if name_lower in self.ctx.locals:
                src_local = self.ctx.locals[name_lower]
                # Make the rename use the source's vreg directly
                for name in decl.names:
                    local = self.ctx.locals.get(name.lower())
                    if local:
                        # Replace the rename's vreg with the source's vreg
                        local.vreg = src_local.vreg
                        local.is_rename = True
                        local.rename_source = name_lower
                return
            # Check params
            if name_lower in self.ctx.params:
                src_vreg = self.ctx.params[name_lower]
                for name in decl.names:
                    local = self.ctx.locals.get(name.lower())
                    if local:
                        local.vreg = src_vreg
                        local.is_rename = True
                        local.rename_source = name_lower
                return

        # For complex expressions (SelectedName, etc.), fall back to address-based
        # This requires dereferencing at each access
        renamed_addr = self._lower_expr(decl.renames)

        for name in decl.names:
            local = self.ctx.locals.get(name.lower())
            if local:
                # For complex renames, store address and dereference on access
                local.is_rename = True
                local.is_address_rename = True  # Different from simple rename
                self.builder.mov(local.vreg, renamed_addr,
                                comment=f"rename {name} -> address (complex)")

    # =========================================================================
    # Nested Packages
    # =========================================================================

    def _lower_nested_package_decl(self, decl: PackageDecl) -> None:
        """Lower a nested package spec declaration.

        Nested packages have their variables allocated on the enclosing procedure's
        stack (done during stack allocation pass). Here we just lower the initializers.
        """
        if self.ctx is None:
            return

        pkg_prefix = decl.name + "." if decl.name else ""

        # Track this nested package and its functions
        if not hasattr(self, '_nested_package_functions'):
            self._nested_package_functions = {}
        pkg_functions = set()
        for pkg_decl in decl.declarations:
            if isinstance(pkg_decl, SubprogramDecl) and pkg_decl.is_function:
                pkg_functions.add(pkg_decl.name.lower())
        self._nested_package_functions[decl.name.lower()] = pkg_functions

        # Enter package scope for symbol visibility
        self.symbols.enter_scope(decl.name)

        # Lower initializers for package variables (visible part)
        for pkg_decl in decl.declarations:
            if isinstance(pkg_decl, ObjectDecl):
                self._lower_nested_package_object_init(pkg_decl, pkg_prefix)

        # Lower initializers for private declarations
        for pkg_decl in decl.private_declarations:
            if isinstance(pkg_decl, ObjectDecl):
                self._lower_nested_package_object_init(pkg_decl, pkg_prefix)

        # Leave package scope
        self.symbols.leave_scope()

    def _lower_nested_package_object_init(self, decl: ObjectDecl, prefix: str) -> None:
        """Lower the initializer for a nested package variable."""
        if self.ctx is None:
            return

        for name in decl.names:
            full_name = f"{prefix}{name}"
            local = self.ctx.locals.get(full_name.lower())
            if local is None:
                continue

            # Handle initialization
            if decl.init_expr:
                value = self._lower_expr(decl.init_expr)
                self.builder.mov(local.vreg, value, comment=f"init {full_name}")

    def _lower_nested_package_body(self, body: PackageBody) -> None:
        """Lower a nested package body.

        This lowers the subprogram bodies defined in the package body.
        The subprograms become nested functions in the enclosing procedure.
        """
        pkg_prefix = body.name + "." if body.name else ""

        # Save enclosing context to share locals with nested functions
        saved_ctx = self.ctx

        # Track current nested package for identifier resolution
        old_nested_pkg = getattr(self, '_current_nested_package', None)
        self._current_nested_package = body.name.lower() if body.name else None

        # Enter package scope for symbol visibility
        self.symbols.enter_scope(body.name)

        for decl in body.declarations:
            if isinstance(decl, SubprogramBody):
                # Pass the enclosing context so nested functions can access package vars
                self._enclosing_ctx_for_nested_pkg = saved_ctx
                self._lower_subprogram_body(decl)
                self._enclosing_ctx_for_nested_pkg = None
            elif isinstance(decl, ObjectDecl):
                # Package body can have local variables too
                self._lower_nested_package_object_init(decl, pkg_prefix)

        # Leave package scope
        self.symbols.leave_scope()

        # Restore nested package tracking
        self._current_nested_package = old_nested_pkg

    # =========================================================================
    # Task Types
    # =========================================================================

    def _lower_task_type_decl(self, decl: TaskTypeDecl) -> None:
        """Lower a task type declaration.

        Task types create concurrent units of execution. Each task object
        has an associated Task Control Block (TCB) that tracks:
        - Stack pointer and base
        - Task state (ready, waiting, terminated)
        - Priority
        - Entry queue pointers

        The type declaration itself generates no code - it just defines
        the task's entry points. The task body generates the actual code.

        Single tasks (task T;) are both a type and an object declaration.
        Task types (task type T;) can have multiple instances.
        """
        # Task type declarations are handled during semantic analysis.
        # The entry points are registered there.
        # We just need to ensure the task initialization code will be called
        # when a task object is created (handled in _lower_task_object_decl).
        pass

    def _lower_task_body(self, decl: TaskBody) -> None:
        """Lower a task body (implementation of task execution).

        A task body is similar to a procedure body but:
        1. Runs concurrently with other tasks
        2. Can contain accept statements for entries
        3. Continues until it reaches 'end' (termination)

        Generated code:
        1. Task entry point (called by runtime when task starts)
        2. Task body statements
        3. Implicit task termination at end
        """
        # Save current context
        old_ctx = self.ctx

        # Create the task body procedure name
        task_name = f"_task_body_{decl.name}"

        # Create IR function for this task body (tasks are void procedures)
        ir_func = IRFunction(name=task_name, return_type=IRType.VOID)
        if self.builder.module:
            self.builder.module.functions.append(ir_func)

        # Create entry block
        entry_block = BasicBlock(label=f"L_{task_name}_start")
        ir_func.blocks.append(entry_block)
        self.builder.function = ir_func  # Set current function for builder
        self.builder.set_block(entry_block)

        # Create new context
        self.ctx = LoweringContext(function=ir_func)

        # Task body receives task ID as implicit parameter
        task_id_vreg = self.builder.new_vreg(IRType.WORD, "_task_id")
        self.builder.pop(task_id_vreg)

        # Store task ID for use by task attributes
        if self.ctx:
            self.ctx.task_id = task_id_vreg

        # Lower local declarations
        for d in decl.declarations:
            self._lower_declaration(d)

        # Lower task body statements
        for stmt in decl.statements:
            self._lower_statement(stmt)

        # Handle exception handlers if present
        if decl.handled_exception_handlers:
            self._lower_exception_handlers(decl.handled_exception_handlers)

        # At end of task body, call task termination
        self.builder.call(Label("_TASK_TERMINATE"))

        # Return (though task terminate doesn't return)
        self.builder.ret()

        # Restore context
        self.ctx = old_ctx

        # Generate entry stub functions for each entry in this task
        # The stubs call the entry queue mechanism
        self._generate_task_entry_stubs(decl)

    def _generate_task_entry_stubs(self, task_body: TaskBody) -> None:
        """Generate entry call stub functions for a task's entries.

        When caller does: My_Task.Some_Entry(Params);
        This calls the stub which:
        1. Queues the entry call
        2. Blocks until the task accepts the call
        3. Returns when the accept body completes
        """
        # Look up the task type to get entry declarations
        if not self.symbols:
            return

        task_sym = self.symbols.lookup(task_body.name)
        if not task_sym or not hasattr(task_sym, 'entries'):
            return

        # For each entry, generate a callable stub
        entries = getattr(task_sym, 'entries', [])
        for entry in entries:
            if isinstance(entry, EntryDecl):
                self._generate_entry_stub(task_body.name, entry)

    def _generate_entry_stub(self, task_name: str, entry: EntryDecl) -> None:
        """Generate a single entry call stub function.

        Entry call sequence:
        1. Push parameters onto stack
        2. Push entry ID (combined with family index if entry family)
        3. Push target task ID
        4. Call _ENTRY_CALL runtime
        5. Runtime blocks caller until accept completes

        For entry families (entry E(for I in Range)), the family index is
        passed as an additional implicit parameter after the task object.
        """
        # Save context
        old_ctx = self.ctx

        # Create stub name (Task_Name_Entry_Name)
        stub_name = f"{task_name}_{entry.name}"

        # Create IR function (entry stubs are void procedures)
        ir_func = IRFunction(name=stub_name, return_type=IRType.VOID)
        if self.builder.module:
            self.builder.module.functions.append(ir_func)

        # Create entry block
        entry_block = BasicBlock(label=f"L_{stub_name}_start")
        ir_func.blocks.append(entry_block)
        self.builder.function = ir_func  # Set current function for builder
        self.builder.set_block(entry_block)

        # Create context
        self.ctx = LoweringContext(function=ir_func)

        # Parameters:
        # - First implicit param: target task object (contains task ID)
        # - For entry families: second implicit param is family index
        # - Then the entry's formal parameters

        # Pop target task object pointer
        task_obj = self.builder.new_vreg(IRType.PTR, "_task_obj")
        self.builder.pop(task_obj)

        # Handle entry family index if present
        family_index_vreg = None
        if entry.family_index:
            family_index_vreg = self.builder.new_vreg(IRType.WORD, "_family_index")
            self.builder.pop(family_index_vreg)

        # Get task ID from task object (stored at offset 0 of task object)
        task_id = self.builder.new_vreg(IRType.WORD, "_task_id")
        self.builder.load_mem(task_id, MemoryLocation(base=task_obj, offset=0, ir_type=IRType.WORD),
                             comment="get task ID from object")

        # Set up entry parameters if any
        params_ptr = self.builder.new_vreg(IRType.PTR, "_params_ptr")
        if entry.parameters:
            # Allocate parameter block on stack
            total_param_size = sum(2 for _ in entry.parameters)  # Assume 2 bytes each
            # Use SP as parameter block pointer
            self.builder.emit(IRInstr(
                OpCode.MOV, params_ptr, VReg(0, IRType.PTR, "SP"),
                comment="params at current SP"
            ))
        else:
            # No parameters - use null pointer
            self.builder.mov(params_ptr, Immediate(0, IRType.PTR))

        # Compute entry ID (hash of entry name for uniqueness)
        base_entry_id = hash(entry.name) & 0xFFFF

        # For entry families, combine base entry ID with family index
        if family_index_vreg:
            # entry_id = base_id * 256 + family_index
            # This allows up to 256 indices per family
            scaled_base = self.builder.new_vreg(IRType.WORD, "_scaled_base")
            self.builder.emit(IRInstr(
                OpCode.MUL, scaled_base,
                Immediate(base_entry_id, IRType.WORD),
                Immediate(256, IRType.WORD),
                comment="scale base entry id"
            ))
            entry_id_vreg = self.builder.new_vreg(IRType.WORD, "_entry_id")
            self.builder.emit(IRInstr(
                OpCode.ADD, entry_id_vreg, scaled_base, family_index_vreg,
                comment="combine with family index"
            ))
        else:
            entry_id_vreg = self.builder.new_vreg(IRType.WORD, "_entry_id")
            self.builder.mov(entry_id_vreg, Immediate(base_entry_id, IRType.WORD))

        # Push parameters for _ENTRY_CALL: params_ptr, task_id, entry_id
        self.builder.push(params_ptr)
        self.builder.push(task_id)
        self.builder.push(entry_id_vreg)

        # Call runtime entry call
        self.builder.call(Label("_ENTRY_CALL"))

        # Clean up stack (3 * 2 = 6 bytes)
        temp = self.builder.new_vreg(IRType.WORD, "_temp")
        self.builder.pop(temp)
        self.builder.pop(temp)
        self.builder.pop(temp)

        # Return
        self.builder.ret()

        # Restore context
        self.ctx = old_ctx

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
            cmp_result = self.builder.new_vreg(IRType.WORD, f"cmp_{i}")
            self.builder.cmp_ne(cmp_result, selected, Immediate(i, IRType.WORD))
            self.builder.jnz(cmp_result, Label(next_label))

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
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if args:
                cond = self._lower_expr(args[0])
                # Jump to ok_label if condition is true (non-zero)
                ok_label = self._new_label("assert_ok")
                self.builder.jnz(cond, Label(ok_label))

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
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if args:
                arg = args[0]
                if isinstance(arg, ProcedureCallStmt):
                    self._lower_procedure_call(arg)

        elif pragma_name == "check":
            # pragma Check(Assertion_Kind, Condition [, Message]);
            # Similar to Assert but with a named assertion kind
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if len(args) >= 2:
                cond = self._lower_expr(args[1])
                # Jump to ok_label if condition is true (non-zero)
                ok_label = self._new_label("check_ok")
                self.builder.jnz(cond, Label(ok_label))

                # Check failed - raise Assertion_Error
                exc_id = self._get_exception_id("Assertion_Error")
                self.builder.emit(IRInstr(
                    OpCode.EXC_RAISE,
                    src1=Immediate(exc_id, IRType.WORD),
                    comment="check failed",
                ))

                self.builder.label(ok_label)

        elif pragma_name == "assume":
            # pragma Assume(Condition);
            # Hint to the compiler that Condition is always true
            # In generated code, we could skip checks based on this
            # For Z80, we don't generate code but note it for optimization
            pass

        elif pragma_name == "warnings":
            # pragma Warnings(On/Off, ...);
            # Compile-time only - no code generated
            pass

        elif pragma_name == "suppress":
            # pragma Suppress(Check_Name);
            # Disable runtime checks - handled by optimization flags
            pass

        elif pragma_name == "unsuppress":
            # pragma Unsuppress(Check_Name);
            # Re-enable runtime checks - handled by optimization flags
            pass

        elif pragma_name == "optimize":
            # pragma Optimize(Time/Space/Off);
            # Compile-time optimization hints - no runtime code
            pass

        elif pragma_name == "inline":
            # pragma Inline(Subprogram_Name);
            # Compile-time inlining hint - no runtime code
            pass

        elif pragma_name == "no_return":
            # pragma No_Return(Subprogram_Name);
            # Indicates procedure never returns normally
            # No runtime code needed
            pass

        elif pragma_name == "import":
            # pragma Import(Convention, Entity, External_Name);
            # Link external routine - mark symbol as imported
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if len(args) >= 2:
                # args[0] = convention (C, Ada, Intrinsic, etc.)
                # args[1] = entity name
                # args[2] = optional external name
                conv = args[0].name.lower() if isinstance(args[0], Identifier) else "c"
                entity_name = args[1].name if isinstance(args[1], Identifier) else str(args[1])
                external = None
                if len(args) >= 3:
                    if isinstance(args[2], StringLiteral):
                        external = args[2].value
                    elif isinstance(args[2], Identifier):
                        external = args[2].name
                sym = self.symbols.lookup(entity_name) if hasattr(self, 'symbols') else None
                if sym:
                    sym.is_imported = True
                    sym.calling_convention = conv
                    sym.external_name = external if external else entity_name

        elif pragma_name == "export":
            # pragma Export(Convention, Entity, External_Name);
            # Export routine for external linking
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if len(args) >= 2:
                conv = args[0].name.lower() if isinstance(args[0], Identifier) else "c"
                entity_name = args[1].name if isinstance(args[1], Identifier) else str(args[1])
                external = None
                if len(args) >= 3:
                    if isinstance(args[2], StringLiteral):
                        external = args[2].value
                    elif isinstance(args[2], Identifier):
                        external = args[2].name
                sym = self.symbols.lookup(entity_name) if hasattr(self, 'symbols') else None
                if sym:
                    sym.is_exported = True
                    sym.calling_convention = conv
                    sym.external_name = external if external else entity_name

        elif pragma_name == "convention":
            # pragma Convention(Convention, Entity);
            # Specify calling convention without import/export
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if len(args) >= 2:
                conv = args[0].name.lower() if isinstance(args[0], Identifier) else "ada"
                entity_name = args[1].name if isinstance(args[1], Identifier) else str(args[1])
                sym = self.symbols.lookup(entity_name) if hasattr(self, 'symbols') else None
                if sym:
                    sym.calling_convention = conv

        elif pragma_name == "volatile":
            # pragma Volatile(Object);
            # Ensure all reads/writes go to memory (disable register caching)
            # Mark the symbol as volatile for the code generator
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if args and isinstance(args[0], Identifier):
                sym = self.symbols.lookup(args[0].name) if hasattr(self, 'symbols') else None
                if sym:
                    sym.is_volatile = True
            # For Z80, all memory accesses are volatile by nature (no cache)
            # but marking ensures optimizer doesn't remove "redundant" loads

        elif pragma_name == "atomic":
            # pragma Atomic(Object);
            # Ensure atomic access (disable interrupts during access)
            # Mark the symbol as atomic so code generator emits DI/EI
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if args and isinstance(args[0], Identifier):
                sym = self.symbols.lookup(args[0].name) if hasattr(self, 'symbols') else None
                if sym:
                    sym.is_atomic = True
                    sym.is_volatile = True  # Atomic implies volatile

        elif pragma_name == "unreferenced":
            # pragma Unreferenced(Name);
            # Suppress warnings about unused entities
            pass

        elif pragma_name == "elaborate":
            # pragma Elaborate(Package_Name);
            # Force elaboration order - handled at link time
            pass

        elif pragma_name == "elaborate_all":
            # pragma Elaborate_All(Package_Name);
            # Force elaboration of all dependencies
            pass

        elif pragma_name == "preelaborate":
            # pragma Preelaborate;
            # Package can be preelaborated
            pass

        elif pragma_name == "pure":
            # pragma Pure;
            # Package is pure (no state)
            pass

        elif pragma_name == "pack":
            # pragma Pack(Type_Name);
            # Pack record/array components tightly
            # Handled during type analysis - no runtime code
            pass

        elif pragma_name == "unchecked_union":
            # pragma Unchecked_Union(Type_Name);
            # Create C-compatible union type
            pass

        elif pragma_name == "linker_options":
            # pragma Linker_Options("options");
            # Pass options to linker - no runtime code
            pass

        elif pragma_name == "linker_section":
            # pragma Linker_Section(Entity, ".section_name");
            # Place entity in specific section
            pass

        elif pragma_name == "machine_attribute":
            # pragma Machine_Attribute(Entity, "attribute");
            # Low-level machine attribute
            pass

        elif pragma_name == "normalize_scalars":
            # pragma Normalize_Scalars;
            # Initialize scalars to out-of-range values for debugging
            pass

        elif pragma_name == "restrictions":
            # pragma Restrictions(restriction_list);
            # Compile-time restrictions - no runtime code
            pass

        elif pragma_name == "reviewable":
            # pragma Reviewable;
            # Generate reviewable code
            pass

        elif pragma_name == "discard_names":
            # pragma Discard_Names;
            # Don't generate name strings for types
            pass

        elif pragma_name == "inspection_point":
            # pragma Inspection_Point(Object_Name);
            # Debugger can examine object here
            # Generate a NOP for breakpoint
            self.builder.emit(IRInstr(OpCode.NOP, comment="inspection point"))

        elif pragma_name == "storage_size":
            # pragma Storage_Size(expression);
            # Set task/access type storage - handled at allocation
            pass

        elif pragma_name == "priority":
            # pragma Priority(expression);
            # Set task priority - for Z80 single-tasking, ignored
            pass

        elif pragma_name == "interrupt_priority":
            # pragma Interrupt_Priority(expression);
            # Set interrupt handler priority
            pass

        elif pragma_name == "attach_handler":
            # pragma Attach_Handler(Handler_Name, Interrupt_ID);
            # Attach interrupt handler to a specific vector
            # Z80 interrupt vectors: RST 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38
            # NMI at 0x66
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if len(args) >= 2:
                handler_name = None
                vector_id = None
                if isinstance(args[0], Identifier):
                    handler_name = args[0].name
                if isinstance(args[1], IntegerLiteral):
                    vector_id = args[1].value
                elif isinstance(args[1], Identifier):
                    # Could be a named constant for the vector
                    vector_id = self._get_constant_value(args[1])
                if handler_name and vector_id is not None:
                    self._interrupt_handlers[vector_id] = handler_name
                    self._is_interrupt_handler.add(handler_name)

        elif pragma_name == "interrupt_handler":
            # pragma Interrupt_Handler(Procedure_Name);
            # Mark procedure as interrupt handler (uses EI/RETI epilogue)
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if args and isinstance(args[0], Identifier):
                self._is_interrupt_handler.add(args[0].name)

        elif pragma_name == "controlled":
            # pragma Controlled(Access_Type);
            # Enable controlled type semantics
            pass

        elif pragma_name == "convention_identifier":
            # pragma Convention_Identifier(Name, Convention);
            # Define new convention name
            pass

        elif pragma_name == "detect_blocking":
            # pragma Detect_Blocking;
            # Detect potentially blocking operations
            pass

        elif pragma_name == "locking_policy":
            # pragma Locking_Policy(policy);
            # Set locking policy for protected types
            pass

        elif pragma_name == "queuing_policy":
            # pragma Queuing_Policy(policy);
            # Set queuing policy for entry calls
            pass

        elif pragma_name == "task_dispatching_policy":
            # pragma Task_Dispatching_Policy(policy);
            # Set task scheduling policy
            pass

        elif pragma_name == "partition_elaboration_policy":
            # pragma Partition_Elaboration_Policy(policy);
            # Control elaboration in distributed systems
            pass

        elif pragma_name == "profile":
            # pragma Profile(Ravenscar/Jorvik/etc);
            # Apply a predefined set of restrictions
            pass

        elif pragma_name == "assertion_policy":
            # pragma Assertion_Policy(Check/Ignore/...);
            # Control assertion checking - affects code generation
            pass

        elif pragma_name == "overflow_mode":
            # pragma Overflow_Mode(General/Minimized/Eliminated/...);
            # Control overflow checking mode
            pass

        elif pragma_name == "default_storage_pool":
            # pragma Default_Storage_Pool(Pool_Name/null);
            # Set default storage pool
            pass

        elif pragma_name == "loop_invariant":
            # pragma Loop_Invariant(Condition);
            # Check loop invariant (like Assert inside loop)
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if args:
                cond = self._lower_expr(args[0])
                ok_label = self._new_label("loop_inv_ok")
                self.builder.jnz(cond, Label(ok_label))
                exc_id = self._get_exception_id("Assertion_Error")
                self.builder.emit(IRInstr(
                    OpCode.EXC_RAISE,
                    src1=Immediate(exc_id, IRType.WORD),
                    comment="loop invariant failed",
                ))
                self.builder.label(ok_label)

        elif pragma_name == "loop_variant":
            # pragma Loop_Variant(Increases => X | Decreases => Y);
            # Runtime check that variant changes in expected direction
            # This helps catch infinite loops during debugging
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            for arg in args:
                selector = getattr(arg, 'selector', None)
                value_expr = getattr(arg, 'value', arg)

                # Get the previous value (stored at end of last iteration)
                var_name = self._expr_to_name(value_expr)
                prev_name = f"_variant_prev_{var_name}"

                # Get current value
                current_val = self._lower_expr(value_expr)

                # Check if previous value was initialized
                if prev_name.lower() in (self.ctx.locals if self.ctx else {}):
                    prev_local = self.ctx.locals[prev_name.lower()]
                    prev_val = prev_local.vreg

                    ok_label = self._new_label("variant_ok")

                    if selector and selector.lower() == "decreases":
                        # Current should be < previous
                        self.builder.cmp(current_val, prev_val)
                        self.builder.jl(Label(ok_label))  # OK if current < prev
                    elif selector and selector.lower() == "increases":
                        # Current should be > previous
                        self.builder.cmp(current_val, prev_val)
                        self.builder.jg(Label(ok_label))  # OK if current > prev
                    else:
                        # Default: decreases
                        self.builder.cmp(current_val, prev_val)
                        self.builder.jl(Label(ok_label))

                    # Variant didn't change as expected - raise exception
                    exc_id = self._get_exception_id("Assertion_Error")
                    self.builder.emit(IRInstr(
                        OpCode.EXC_RAISE,
                        src1=Immediate(exc_id, IRType.WORD),
                        comment="loop variant check failed",
                    ))
                    self.builder.label(ok_label)

                    # Update previous value for next iteration
                    self.builder.mov(prev_val, current_val)
                else:
                    # First iteration - store initial value as previous
                    prev_vreg = self.builder.new_vreg(IRType.WORD, prev_name)
                    self.builder.mov(prev_vreg, current_val)
                    # Register it as a local for next iteration
                    if self.ctx:
                        prev_local = LocalVariable(
                            name=prev_name,
                            vreg=prev_vreg,
                            stack_offset=self.ctx.locals_size,
                            size=2,
                            ada_type=None
                        )
                        self.ctx.locals[prev_name.lower()] = prev_local
                        self.ctx.locals_size += 2

        elif pragma_name == "precondition":
            # pragma Precondition(Condition);
            # Check precondition on entry
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if args:
                cond = self._lower_expr(args[0])
                ok_label = self._new_label("precond_ok")
                self.builder.jnz(cond, Label(ok_label))
                exc_id = self._get_exception_id("Assertion_Error")
                self.builder.emit(IRInstr(
                    OpCode.EXC_RAISE,
                    src1=Immediate(exc_id, IRType.WORD),
                    comment="precondition failed",
                ))
                self.builder.label(ok_label)

        elif pragma_name == "postcondition":
            # pragma Postcondition(Condition);
            # Check postcondition on exit
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if args:
                cond = self._lower_expr(args[0])
                ok_label = self._new_label("postcond_ok")
                self.builder.jnz(cond, Label(ok_label))
                exc_id = self._get_exception_id("Assertion_Error")
                self.builder.emit(IRInstr(
                    OpCode.EXC_RAISE,
                    src1=Immediate(exc_id, IRType.WORD),
                    comment="postcondition failed",
                ))
                self.builder.label(ok_label)

        elif pragma_name == "type_invariant":
            # pragma Type_Invariant(Entity, Condition);
            # Check type invariant
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if len(args) >= 2:
                cond = self._lower_expr(args[1])
                ok_label = self._new_label("type_inv_ok")
                self.builder.jnz(cond, Label(ok_label))
                exc_id = self._get_exception_id("Assertion_Error")
                self.builder.emit(IRInstr(
                    OpCode.EXC_RAISE,
                    src1=Immediate(exc_id, IRType.WORD),
                    comment="type invariant failed",
                ))
                self.builder.label(ok_label)

        elif pragma_name == "static_predicate":
            # pragma Static_Predicate(Entity, Condition);
            # Compile-time only
            pass

        elif pragma_name == "dynamic_predicate":
            # pragma Dynamic_Predicate(Entity, Condition);
            # Check dynamic predicate at runtime
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if len(args) >= 2:
                cond = self._lower_expr(args[1])
                ok_label = self._new_label("dyn_pred_ok")
                self.builder.jnz(cond, Label(ok_label))
                exc_id = self._get_exception_id("Assertion_Error")
                self.builder.emit(IRInstr(
                    OpCode.EXC_RAISE,
                    src1=Immediate(exc_id, IRType.WORD),
                    comment="dynamic predicate failed",
                ))
                self.builder.label(ok_label)

        elif pragma_name == "interrupt_state":
            # pragma Interrupt_State(Interrupt, State);
            # Set interrupt state
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            if len(args) >= 2:
                # For Z80, this could control IM modes
                pass

        elif pragma_name == "machine_code":
            # pragma Machine_Code(...);
            # Inline machine code insertion - multiple forms supported:
            #   pragma Machine_Code(16#CD#, 16#00#, 16#00#);  -- Raw bytes
            #   pragma Machine_Code("ld hl, 0");              -- Assembly string
            #   pragma Machine_Code(Asm("nop"));              -- Asm record
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            for arg in args:
                if hasattr(arg, 'value'):
                    if isinstance(arg.value, int):
                        # Emit raw byte using special inline asm instruction
                        self.builder.emit(IRInstr(
                            OpCode.INLINE_ASM,
                            comment=f".db {arg.value:#04x}"
                        ))
                    elif isinstance(arg.value, str):
                        # Emit assembly string directly
                        self.builder.emit(IRInstr(
                            OpCode.INLINE_ASM,
                            comment=arg.value
                        ))
                elif isinstance(arg, StringLiteral):
                    # String literal containing assembly
                    self.builder.emit(IRInstr(
                        OpCode.INLINE_ASM,
                        comment=arg.value
                    ))
                elif isinstance(arg, FunctionCall) and hasattr(arg, 'name'):
                    # Asm() or other machine code record
                    name = arg.name.name if hasattr(arg.name, 'name') else str(arg.name)
                    if name.lower() == 'asm' and arg.args:
                        for asm_arg in arg.args:
                            if hasattr(asm_arg, 'value'):
                                self.builder.emit(IRInstr(
                                    OpCode.INLINE_ASM,
                                    comment=str(asm_arg.value)
                                ))

        elif pragma_name == "linker_alias":
            # pragma Linker_Alias(Entity, External_Name);
            # Create linker alias - no runtime code
            pass

        elif pragma_name == "weak_external":
            # pragma Weak_External(Entity);
            # Weak external reference
            pass

        elif pragma_name == "no_inline":
            # pragma No_Inline(Subprogram_Name);
            # Prevent inlining
            pass

        elif pragma_name == "inline_always":
            # pragma Inline_Always(Subprogram_Name);
            # Force inlining
            pass

        elif pragma_name == "obsolescent":
            # pragma Obsolescent(Entity, [Message]);
            # Mark as obsolete - compile-time warning only
            pass

        elif pragma_name == "exception_policy":
            # pragma Exception_Policy(policy);
            # Set exception handling policy
            pass

        elif pragma_name == "finalize_storage_only":
            # pragma Finalize_Storage_Only(First_Subtype);
            # Control finalization
            pass

        elif pragma_name == "no_elaboration_code_all":
            # pragma No_Elaboration_Code_All;
            # Ensure no elaboration code
            pass

        elif pragma_name == "partition_elaboration_policy":
            # Already handled above
            pass

        elif pragma_name == "external":
            # pragma External(Convention, Entity, External_Name);
            # Similar to Import/Export combined
            pass

        elif pragma_name == "external_name":
            # pragma External_Name(Entity, External_Name);
            # Set external name
            pass

        elif pragma_name == "interface_name":
            # pragma Interface_Name(Entity, External_Name);
            # Set interface name
            pass

        elif pragma_name == "ident":
            # pragma Ident("version-string");
            # Set identification string - emit to data section
            args = getattr(stmt, 'args', None) or getattr(stmt, 'arguments', [])
            # No runtime code, but could add to metadata

        elif pragma_name == "comment":
            # pragma Comment(Position, Text);
            # Add comment to object file
            pass

        elif pragma_name == "source_reference":
            # pragma Source_Reference(Line, "filename");
            # Change source line tracking
            pass

        # Other pragmas are compile-time only - no runtime code needed

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
            # Get target type first to determine how to lower the value
            target_type = self._get_target_type(stmt.target)

            # For Float64 targets, use _lower_float64_operand to get a proper pointer
            # This handles RealLiteral correctly (creates Float64 constant in code segment)
            if self._is_float64_type(target_type):
                value = self._lower_float64_operand(stmt.value)
            else:
                value = self._lower_expr(stmt.value)

            # Emit range check for constrained types
            if target_type:
                self._emit_range_check(value, target_type, "assignment range check")

            # Get target
            if isinstance(stmt.target, Identifier):
                name = stmt.target.name.lower()

                # Check locals
                if name in self.ctx.locals:
                    local = self.ctx.locals[name]

                    # For address-based renamed variables: store through the address
                    # Simple renames share the source's vreg, so normal assignment works
                    if getattr(local, 'is_address_rename', False):
                        # local.vreg contains the address of the original variable
                        # Store the value at that address
                        self.builder.emit(IRInstr(
                            OpCode.STORE,
                            dst=MemoryLocation(offset=0, ir_type=IRType.WORD, base=local.vreg),
                            src1=value,
                            comment=f"{name} := ... (via address rename)"
                        ))
                        return

                    # For record types: copy the record data, not just the pointer
                    if target_type and isinstance(target_type, RecordType):
                        size = target_type.size_bytes()
                        # Get address of target local record
                        frame_offset = -(self.ctx.locals_size - local.stack_offset)
                        local_addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                        self.builder.emit(IRInstr(
                            OpCode.LEA,
                            dst=local_addr,
                            src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                        ))
                        # Get address of source record
                        src_addr = self._get_record_base(stmt.value)
                        if src_addr is None:
                            # Fallback: value might already be an address (from function return, etc.)
                            src_addr = value
                        # Copy size bytes from source to target
                        self._emit_memcpy(local_addr, src_addr, size, f"copy record to {name}")
                        return

                    # For Float64 types: copy 8 bytes
                    if self._is_float64_type(target_type):
                        # Get address of local Float64
                        frame_offset = -(self.ctx.locals_size - local.stack_offset)
                        local_addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                        self.builder.emit(IRInstr(
                            OpCode.LEA,
                            dst=local_addr,
                            src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                        ))
                        # value is a pointer to Float64 result - copy 8 bytes
                        self.builder.push(value)
                        self.builder.push(local_addr)
                        self.builder.call(Label("_f64_copy"))
                        discard = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(discard)
                        self.builder.pop(discard)
                        return

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

                    # For byref parameters (out/in out), store through the pointer
                    if name in self.ctx.byref_params:
                        # For controlled types: Finalize old, store, Adjust new
                        if target_type and self._type_needs_adjustment(target_type):
                            # Load current value for Finalize
                            old_val = self.builder.new_vreg(IRType.WORD, f"_{name}_old")
                            self.builder.emit(IRInstr(
                                OpCode.LOAD, old_val,
                                MemoryLocation(offset=0, ir_type=IRType.WORD, base=param),
                                comment=f"load old {name} for finalize"
                            ))
                            self._call_finalize(old_val, target_type)
                            self.builder.emit(IRInstr(
                                OpCode.STORE,
                                dst=MemoryLocation(offset=0, ir_type=IRType.WORD, base=param),
                                src1=value,
                                comment=f"store to byref param {name}"
                            ))
                            self._call_adjust(value, target_type)
                        else:
                            self.builder.emit(IRInstr(
                                OpCode.STORE,
                                dst=MemoryLocation(offset=0, ir_type=IRType.WORD, base=param),
                                src1=value,
                                comment=f"store to byref param {name}"
                            ))
                        return

                    # For controlled types: Finalize old, copy, Adjust new
                    if target_type and self._type_needs_adjustment(target_type):
                        self._call_finalize(param, target_type)
                        self.builder.mov(param, value, comment=f"{name} := ...")
                        self._call_adjust(param, target_type)
                    else:
                        self.builder.mov(param, value, comment=f"{name} := ...")
                    return

                # Check for outer-scope variable (passed as hidden pointer)
                outer_param_name = f"_outer_{name}"
                if outer_param_name in self.ctx.params:
                    ptr_vreg = self.ctx.params[outer_param_name]
                    self.builder.emit(IRInstr(
                        OpCode.STORE,
                        dst=MemoryLocation(offset=0, ir_type=IRType.WORD, base=ptr_vreg),
                        src1=value,
                        comment=f"store to outer {name}"
                    ))
                    return

            elif isinstance(stmt.target, IndexedComponent):
                # Array assignment
                self._lower_indexed_store(stmt.target, value)

            elif isinstance(stmt.target, SelectedName):
                # Record field assignment
                self._lower_selected_store(stmt.target, value)

            elif isinstance(stmt.target, Dereference):
                # Pointer dereference assignment: P.all := value
                ptr = self._lower_expr(stmt.target.prefix)
                mem = MemoryLocation(base=ptr, offset=0, ir_type=IRType.WORD)
                self.builder.store(mem, value)

            elif isinstance(stmt.target, Slice):
                # Array slice assignment: A(1..5) := B(1..5)
                self._lower_slice_store(stmt.target, value)
        finally:
            # Restore previous assignment target
            self.ctx.assignment_target = old_target

    def _get_target_type(self, target):
        """Get the Ada type of an assignment target."""
        if isinstance(target, Identifier):
            # First check local variables
            if self.ctx:
                name = target.name.lower()
                if name in self.ctx.locals:
                    local = self.ctx.locals[name]
                    if local.ada_type:
                        # Resolve the type (may be AST node or AdaType)
                        return self._resolve_local_type(local.ada_type)
            # Then check symbol table
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
        end_label = self._new_label("endif")

        # Determine where to jump if main condition is false
        # If there are elsif parts, jump to first elsif; otherwise jump to else
        if stmt.elsif_parts:
            first_elsif_label = self._new_label("elsif")
            false_target = first_elsif_label
        else:
            else_label = self._new_label("else")
            false_target = else_label

        # Evaluate condition
        cond = self._lower_expr(stmt.condition)

        # Jump to elsif/else if condition is false (zero)
        self.builder.jz(cond, Label(false_target))

        # Then block
        then_block = self.builder.new_block(then_label)
        self.builder.set_block(then_block)

        for s in stmt.then_stmts:
            self._lower_statement(s)

        if not self._block_has_return(self.builder.block):
            self.builder.jmp(Label(end_label))

        # Elsif parts
        next_elsif_label = first_elsif_label if stmt.elsif_parts else None
        for elsif_cond, elsif_stmts in stmt.elsif_parts:
            # Create block for this elsif (using pending label)
            elsif_block = self.builder.new_block(next_elsif_label)
            self.builder.set_block(elsif_block)

            # Evaluate elsif condition
            cond = self._lower_expr(elsif_cond)
            next_elsif_label = self._new_label("elsif_next")
            self.builder.jz(cond, Label(next_elsif_label))

            # Elsif body
            for s in elsif_stmts:
                self._lower_statement(s)

            if not self._block_has_return(self.builder.block):
                self.builder.jmp(Label(end_label))

        # Else block - use pending elsif_next label if available, else use else_label
        if next_elsif_label is not None:
            else_block = self.builder.new_block(next_elsif_label)
        else:
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

            # Check if this is an "of" iterator (for X of Array)
            if iterator.is_of_iterator:
                # Ada 2012 "for X of Array" - iterate over array elements
                self._lower_for_of_loop(stmt, iterator, end_label)
                # Clean up named loop label
                if stmt_label_lower:
                    del self.ctx.loop_exit_labels[stmt_label_lower]
                self.ctx.loop_exit_label = old_exit
                return

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
            elif isinstance(iterator.iterable, AttributeReference):
                # Handle A'Range attribute
                attr = iterator.iterable.attribute.lower()
                if attr == "range":
                    # Get array bounds from the prefix
                    bounds = self._get_array_bounds_from_expr(iterator.iterable.prefix)
                    if bounds:
                        low_val, high_val = bounds[0]  # First dimension
                        low = Immediate(low_val, IRType.WORD)
                        high = Immediate(high_val, IRType.WORD)
                    else:
                        # Fallback
                        low = Immediate(1, IRType.WORD)
                        high = Immediate(10, IRType.WORD)
                else:
                    # Other attributes - default range
                    low = Immediate(1, IRType.WORD)
                    high = Immediate(10, IRType.WORD)
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

    def _lower_for_of_loop(self, stmt: LoopStmt, iterator, end_label: str) -> None:
        """Lower an Ada 2012 'for X of Array' loop.

        Iterates over each element of an array/container.
        The loop variable X refers to each element in turn (not the index).
        """
        if self.ctx is None:
            return

        # Get the array expression and its type
        array_expr = iterator.iterable
        array_type = self._get_expr_type(array_expr)

        # Evaluate array base address
        array_ptr = self._lower_expr(array_expr)

        # Get array bounds
        if array_type and isinstance(array_type, ArrayType):
            if hasattr(array_type, 'index_constraint') and array_type.index_constraint:
                low = array_type.index_constraint.low if hasattr(array_type.index_constraint, 'low') else 0
                high = array_type.index_constraint.high if hasattr(array_type.index_constraint, 'high') else 9
            else:
                low, high = 0, 9  # Default
            element_size = 2  # Default element size
            if array_type.element_type and hasattr(array_type.element_type, 'size_bits'):
                element_size = (array_type.element_type.size_bits + 7) // 8
        else:
            low, high = 0, 9
            element_size = 2

        # Create index variable (internal)
        index_var = self.builder.new_vreg(IRType.WORD, "_of_index")

        # Create element variable (visible to user code)
        elem_var = self.builder.new_vreg(IRType.WORD, iterator.name)
        self.ctx.locals[iterator.name.lower()] = LocalVariable(
            name=iterator.name,
            vreg=elem_var,
            stack_offset=0,
            size=element_size,
        )

        # Store bounds
        low_vreg = self.builder.new_vreg(IRType.WORD, "_of_low")
        high_vreg = self.builder.new_vreg(IRType.WORD, "_of_high")
        self.builder.mov(low_vreg, Immediate(low, IRType.WORD))
        self.builder.mov(high_vreg, Immediate(high, IRType.WORD))

        # Initialize index
        if iterator.is_reverse:
            self.builder.mov(index_var, high_vreg, comment="init of_index (reverse)")
        else:
            self.builder.mov(index_var, low_vreg, comment="init of_index")

        cond_label = self._new_label("forof_cond")
        body_label = self._new_label("forof_body")
        inc_label = self._new_label("forof_inc")

        self.builder.jmp(Label(cond_label))

        # Condition check
        cond_block = self.builder.new_block(cond_label)
        self.builder.set_block(cond_block)

        cond = self.builder.new_vreg(IRType.BOOL, "_of_cond")
        if iterator.is_reverse:
            self.builder.cmp_ge(cond, index_var, low_vreg)
        else:
            self.builder.cmp_le(cond, index_var, high_vreg)
        self.builder.jz(cond, Label(end_label))

        # Body - load element into loop variable
        body_block = self.builder.new_block(body_label)
        self.builder.set_block(body_block)

        # Calculate element address: array_ptr + (index - low) * element_size
        offset_var = self.builder.new_vreg(IRType.WORD, "_of_offset")
        self.builder.sub(offset_var, index_var, low_vreg)

        if element_size > 1:
            # Multiply by element size
            self.builder.emit(IRInstr(
                OpCode.MUL, offset_var, offset_var,
                Immediate(element_size, IRType.WORD),
                comment=f"offset * {element_size}"
            ))

        elem_addr = self.builder.new_vreg(IRType.PTR, "_of_elem_addr")
        self.builder.add(elem_addr, array_ptr, offset_var)

        # Load element value into loop variable
        self.builder.load(elem_var, MemoryLocation(base=elem_addr, offset=0, ir_type=IRType.WORD))

        # Execute loop body
        for s in stmt.statements:
            self._lower_statement(s)

        # Increment/decrement index
        inc_block = self.builder.new_block(inc_label)
        self.builder.set_block(inc_block)

        one = Immediate(1, IRType.WORD)
        if iterator.is_reverse:
            self.builder.sub(index_var, index_var, one)
        else:
            self.builder.add(index_var, index_var, one)

        self.builder.jmp(Label(cond_label))

        # End block
        end_block = self.builder.new_block(end_label)
        self.builder.set_block(end_block)

    def _lower_block(self, stmt: BlockStmt) -> None:
        """Lower a block statement."""
        if self.ctx is None:
            return

        # First, allocate stack space for any local variables in the block
        # This is needed because block declarations aren't part of the initial locals scan
        for decl in stmt.declarations:
            if isinstance(decl, ObjectDecl):
                for name in decl.names:
                    if name.lower() not in self.ctx.locals:
                        size = self._calc_type_size(decl, stmt.declarations)
                        vreg = self.builder.new_vreg(IRType.WORD, name)
                        stack_offset = self.ctx.locals_size
                        self.ctx.locals[name.lower()] = LocalVariable(
                            name=name,
                            vreg=vreg,
                            stack_offset=stack_offset,
                            size=size,
                            ada_type=decl.type_mark if hasattr(decl, 'type_mark') else None,
                        )
                        self.ctx.locals_size += size

        # Process declarations (initializations)
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

        # Check for generic formal subprogram substitution
        # If we're inside a generic instantiation and the called name is a formal subprogram,
        # replace it with the actual subprogram
        if self._generic_type_map:
            subp_key = f"_subp_{proc_name}"
            if subp_key in self._generic_type_map:
                # This is a call to a generic formal subprogram - substitute the actual
                actual_name = self._generic_type_map[subp_key]
                proc_name = actual_name.lower()
                # Update stmt.name to point to actual subprogram
                stmt = ProcedureCallStmt(
                    name=Identifier(actual_name),
                    args=stmt.args
                )

        # Check for Text_IO built-in procedures
        if proc_name in ("put", "put_line", "new_line", "get", "get_line"):
            # Determine if this is from Integer_Text_IO
            is_integer_io = False
            if isinstance(stmt.name, SelectedName):
                prefix = self._get_selected_name_prefix(stmt.name)
                is_integer_io = "integer_text_io" in prefix.lower()
            self._lower_text_io_call(proc_name, stmt.args, is_integer_io)
            return

        # Check for Text_IO/Sequential_IO file operations
        if proc_name in ("create", "open", "close", "delete", "reset", "flush"):
            self._lower_file_operation(proc_name, stmt.args)
            return

        # Check if this is a Free (Unchecked_Deallocation instantiation)
        if self._is_deallocation_call(proc_name):
            self._lower_deallocation_call(stmt.args)
            return

        if isinstance(stmt.name, Identifier):
            # Resolve overloaded procedure
            sym = self._resolve_overload(stmt.name.name, stmt.args)

            # Determine the call target - use external name if imported
            # or runtime_name for built-in container operations
            call_target = stmt.name.name
            if sym:
                if sym.runtime_name:
                    # Built-in container/library operation
                    call_target = sym.runtime_name
                elif sym.is_imported and sym.external_name:
                    call_target = sym.external_name
                else:
                    call_target = sym.name

            # Check for overloaded procedure - use unique label if available
            arg_count = len(stmt.args) if stmt.args else 0
            label_key = (call_target.lower(), arg_count)
            if label_key in self._function_label_map:
                call_target = self._function_label_map[label_key]

            # Get parameter modes for out/in out handling
            # First try locally-tracked modes (for nested subprograms), then symbol table
            proc_name_lower = proc_name.lower() if proc_name else call_target.lower()

            # Build effective arguments list including defaults for missing parameters
            effective_args = self._build_effective_args(stmt.args, sym, proc_name_lower)

            # Check if this is a dispatching call
            is_dispatching = self._is_dispatching_call(sym, stmt.args)

            param_modes = []
            if proc_name_lower in self._subprogram_param_modes:
                param_modes = self._subprogram_param_modes[proc_name_lower]
            elif sym and sym.parameters:
                param_modes = [p.mode for p in sym.parameters]

            # For nested subprograms, push outer variable addresses (in reverse order)
            if proc_name_lower in self._nested_outer_vars:
                outer_vars = self._nested_outer_vars[proc_name_lower]
                for var_name in reversed(list(outer_vars)):
                    # Get address of the outer variable
                    if self.ctx and var_name in self.ctx.locals:
                        local = self.ctx.locals[var_name]
                        addr = self.builder.new_vreg(IRType.PTR, f"_{var_name}_addr")
                        self.builder.emit(IRInstr(
                            OpCode.LEA, addr,
                            MemoryLocation(ir_type=IRType.PTR, addr_vreg=local.vreg),
                            comment=f"addr of outer {var_name}"
                        ))
                        self.builder.push(addr)

            # Push arguments in reverse order
            for idx, arg in enumerate(reversed(effective_args)):
                # Check if this argument corresponds to an out/in out parameter
                # idx is reversed, so we need forward_idx
                forward_idx = len(effective_args) - 1 - idx
                param_mode = param_modes[forward_idx] if forward_idx < len(param_modes) else "in"

                if param_mode in ("out", "in out"):
                    # Pass address of the argument
                    addr = self._get_arg_address(arg)
                    self.builder.push(addr)
                else:
                    # Pass value
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

            # Clean up stack (regular args + outer variable addresses)
            num_args = len(effective_args)
            if proc_name_lower in self._nested_outer_vars:
                num_args += len(self._nested_outer_vars[proc_name_lower])
            if num_args > 0:
                # Pop arguments (2 bytes each)
                for _ in range(num_args):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)

    def _get_arg_address(self, arg):
        """Get the address of an argument for pass-by-reference.

        For out/in out parameters, we pass the address of the variable
        so the callee can modify it.
        """
        if self.ctx is None:
            return Immediate(0, IRType.PTR)

        if isinstance(arg, Identifier):
            name = arg.name.lower()

            # Check locals - get address of the local's stack location
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                # For records and large locals (size > 2), use explicit frame offset
                # because they're stored at fixed stack locations, not in vreg slots
                if local.size > 2:
                    frame_offset = -(self.ctx.locals_size - local.stack_offset)
                    self.builder.emit(IRInstr(
                        OpCode.LEA, addr,
                        MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                        comment=f"addr of local {name}"
                    ))
                else:
                    # For simple 2-byte locals, use addr_vreg to get vreg's actual location
                    self.builder.emit(IRInstr(
                        OpCode.LEA, addr,
                        MemoryLocation(ir_type=IRType.PTR, addr_vreg=local.vreg),
                        comment=f"addr of local {name}"
                    ))
                return addr

            # Check params - if it's already a byref param, return its value (address)
            if name in self.ctx.params:
                param_vreg = self.ctx.params[name]
                if name in self.ctx.byref_params:
                    # Already a pointer, return as-is
                    return param_vreg
                # Value param: need to get its address from stack frame
                # Use addr_vreg to get the address
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA, addr,
                    MemoryLocation(ir_type=IRType.PTR, addr_vreg=param_vreg),
                    comment=f"addr of param {name}"
                ))
                return addr

            # Check globals
            if name in self.globals:
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA, addr,
                    MemoryLocation(is_global=True, symbol_name=name, ir_type=IRType.PTR),
                    comment=f"addr of global {name}"
                ))
                return addr

        # For other expressions, evaluate them and the address can't be taken
        # This is an error case in Ada (can only pass variables to out/in out)
        # Return a dummy value; semantic analyzer should catch this
        return self._lower_expr(arg)

    def _is_deallocation_call(self, proc_name: str) -> bool:
        """Check if a procedure is an Unchecked_Deallocation instantiation.

        Common naming conventions are Free, Deallocate, or any name chosen
        when instantiating Ada.Unchecked_Deallocation.
        """
        # Look up the symbol to check if it's a deallocation procedure
        sym = self.symbols.lookup(proc_name)
        if sym:
            # Check if marked as a deallocation procedure (from generic instantiation)
            if sym.is_deallocation:
                return True

        # Also check for common names used for Free (heuristic fallback)
        lower_name = proc_name.lower()
        if lower_name in ("free", "deallocate", "release"):
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

    def _can_inline(self, sym: Symbol, args: list) -> bool:
        """Check if a function call can be inlined.

        Inlining is possible when:
        1. The function has a definition we can access
        2. The function is not recursive
        3. The function body is simple enough (few statements)
        4. We're not already inlining (prevent infinite recursion)
        """
        # Check for function definition
        func_def = getattr(sym, 'definition', None)
        if func_def is None:
            return False

        # Check if we're already inlining to prevent infinite recursion
        if hasattr(self, '_inline_depth') and self._inline_depth > 3:
            return False

        # Import here to avoid circular imports
        from uada80.parser import SubprogramBody

        if not isinstance(func_def, SubprogramBody):
            return False

        # Check statement count - only inline small functions
        body_stmts = getattr(func_def, 'statements', [])
        if len(body_stmts) > 10:
            return False

        # Don't inline functions with local declarations (variables, nested subprograms)
        decls = getattr(func_def, 'declarations', [])
        if len(decls) > 5:
            return False

        # Don't inline functions with exception handlers
        if getattr(func_def, 'handlers', None):
            return False

        return True

    def _inline_function_call(self, sym: Symbol, args: list, result_vreg) -> None:
        """Inline a function call by generating its body directly.

        This replaces the call instruction with the function's body statements,
        with parameters replaced by the actual arguments.
        """
        from uada80.parser import SubprogramBody, ReturnStmt

        # Track inline depth
        if not hasattr(self, '_inline_depth'):
            self._inline_depth = 0
        self._inline_depth += 1

        try:
            func_def = sym.definition
            if not isinstance(func_def, SubprogramBody):
                # Fall back to regular call
                self.builder.call(Label(sym.name))
                return

            # Save current context
            old_ctx = self.ctx
            old_inline_params = getattr(self, '_inline_params', {})

            # Create mapping from parameter names to argument values
            self._inline_params = {}
            for i, param in enumerate(sym.parameters):
                if i < len(args) and args[i].value:
                    param_name = param.name.lower()
                    arg_value = self._lower_expr(args[i].value)
                    self._inline_params[param_name] = arg_value

            # Generate unique labels for this inline instance
            inline_id = self._get_unique_label("inline")
            inline_end_label = f"_inline_end_{inline_id}"

            self.builder.emit(IRInstr(OpCode.NOP, comment=f"begin inline {sym.name}"))

            # Process function body statements
            for stmt in func_def.statements:
                if isinstance(stmt, ReturnStmt):
                    # For return statements, evaluate expression and store to result
                    if stmt.expression:
                        ret_val = self._lower_expr(stmt.expression)
                        self.builder.mov(result_vreg, ret_val,
                                        comment=f"inline return value")
                    # Jump to end of inline block
                    self.builder.emit(IRInstr(OpCode.JMP, dst=Label(inline_end_label),
                                             comment="exit inline"))
                else:
                    # Lower other statements normally
                    self._lower_statement(stmt)

            # Emit end label
            self.builder.emit(IRInstr(OpCode.LABEL, dst=Label(inline_end_label)))
            self.builder.emit(IRInstr(OpCode.NOP, comment=f"end inline {sym.name}"))

            # Restore context
            self._inline_params = old_inline_params

        finally:
            self._inline_depth -= 1

    def _is_unconstrained_array_arg(self, sym: Optional[Symbol], arg_idx: int) -> bool:
        """Check if a parameter at given index expects an unconstrained array.

        Returns True if the corresponding parameter type is an unconstrained array,
        meaning we need to pass a dope vector (ptr, first, last).
        """
        if not sym or not sym.parameters:
            return False

        if arg_idx >= len(sym.parameters):
            return False

        param = sym.parameters[arg_idx]
        param_type = param.ada_type if hasattr(param, 'ada_type') else None

        if param_type and isinstance(param_type, ArrayType):
            return not param_type.is_constrained

        return False

    def _get_array_dope_vector(self, expr) -> tuple:
        """Get the dope vector (first, last, ptr) for an array expression.

        For constrained arrays, bounds come from the type.
        For unconstrained arrays (parameters), bounds come from the dope vector.
        For string literals, bounds are derived from the literal.

        Returns: (first_val, last_val, ptr_val) - IRValues for the dope vector
        """
        # Get the pointer to the array data
        ptr_val = self._lower_expr(expr)

        # Determine bounds based on expression type
        if isinstance(expr, StringLiteral):
            # String literal: 1 to length
            length = len(expr.value)
            first_val = Immediate(1, IRType.WORD)
            last_val = Immediate(length, IRType.WORD)
        elif isinstance(expr, Identifier):
            sym = self.symbols.lookup(expr.name)
            if sym and sym.ada_type and isinstance(sym.ada_type, ArrayType):
                if sym.ada_type.is_constrained and sym.ada_type.bounds:
                    # Constrained array - bounds from type
                    low, high = sym.ada_type.bounds[0]
                    first_val = Immediate(low, IRType.WORD)
                    last_val = Immediate(high, IRType.WORD)
                else:
                    # Unconstrained - check if it's a parameter with dope vector
                    param_name = expr.name.lower()
                    if self.ctx and f"{param_name}'first" in self.ctx.params:
                        first_val = self.ctx.params[f"{param_name}'first"]
                        last_val = self.ctx.params[f"{param_name}'last"]
                    else:
                        # Default: assume 1-indexed
                        first_val = Immediate(1, IRType.WORD)
                        # For length, we'd need to call strlen - use placeholder
                        last_val = Immediate(0, IRType.WORD)  # Unknown
            else:
                # Unknown - use defaults
                first_val = Immediate(1, IRType.WORD)
                last_val = Immediate(0, IRType.WORD)
        elif isinstance(expr, Slice):
            # Slice: bounds come from the slice range
            first_val = self._lower_expr(expr.range_expr.low)
            last_val = self._lower_expr(expr.range_expr.high)
        else:
            # Unknown expression - default to 1-indexed
            first_val = Immediate(1, IRType.WORD)
            last_val = Immediate(0, IRType.WORD)

        return (first_val, last_val, ptr_val)

    def _build_effective_args(self, provided_args: list, sym: Optional[Symbol],
                               func_name: str = "") -> list:
        """Build effective argument list with defaults for missing parameters.

        Returns a list of expressions to use as arguments.
        """
        # Get parameter names from symbol or from local tracking for nested subprograms
        param_names_list: list[str] = []
        if sym and sym.parameters:
            param_names_list = [p.name.lower() if p.name else "" for p in sym.parameters]
        elif func_name and func_name in self._subprogram_param_names:
            param_names_list = self._subprogram_param_names[func_name]

        if not param_names_list:
            # No param info - just use provided args in order
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

        # Get default values list for this subprogram
        param_defaults_list = []
        if func_name and func_name in self._subprogram_param_defaults:
            param_defaults_list = self._subprogram_param_defaults[func_name]

        # Process each parameter
        for param_index, param_name in enumerate(param_names_list):
            # Check for named argument first
            if param_name in provided_by_name:
                effective_args.append(provided_by_name[param_name])
            elif param_index in provided_by_pos:
                # Use positional argument
                effective_args.append(provided_by_pos[param_index])
            elif sym and sym.parameters and param_index < len(sym.parameters):
                param = sym.parameters[param_index]
                if param.default_value is not None:
                    # Use the parameter's default value expression
                    effective_args.append(param.default_value)
                else:
                    # No default and no argument - use 0 as fallback
                    effective_args.append(IntegerLiteral(value=0, text="0"))
            elif param_index < len(param_defaults_list) and param_defaults_list[param_index] is not None:
                # Use default from our tracking (for nested functions)
                effective_args.append(param_defaults_list[param_index])
            else:
                # No default and no argument - use 0 as fallback
                effective_args.append(IntegerLiteral(value=0, text="0"))

        return effective_args

    def _get_selected_name_prefix(self, name: SelectedName) -> str:
        """Get the full prefix of a selected name as a string."""
        parts = []
        current = name.prefix
        while isinstance(current, SelectedName):
            parts.insert(0, current.selector)
            current = current.prefix
        if isinstance(current, Identifier):
            parts.insert(0, current.name)
        return ".".join(parts)

    def _is_file_type(self, expr) -> bool:
        """Check if expression is a File_Type (for file-based I/O)."""
        if isinstance(expr, Identifier):
            name = expr.name.lower()
            # Check local type
            if self.ctx and name in self.ctx.locals:
                local = self.ctx.locals[name]
                if hasattr(local, 'type_ref') and local.type_ref:
                    type_name = str(local.type_ref).lower()
                    return "file_type" in type_name
                # Also check the ada_type attribute
                if hasattr(local, 'ada_type') and local.ada_type:
                    type_name = str(local.ada_type).lower()
                    return "file_type" in type_name
            # Check symbol table
            if self.symbol_table:
                sym = self.symbol_table.lookup(expr.name)
                if sym:
                    if hasattr(sym, 'type_ref') and sym.type_ref:
                        type_name = str(sym.type_ref).lower()
                        return "file_type" in type_name
                    if hasattr(sym, 'ada_type') and sym.ada_type:
                        type_name = str(sym.ada_type).lower()
                        return "file_type" in type_name
        return False

    def _lower_text_io_call(self, proc_name: str, args: list, is_integer_io: bool = False) -> None:
        """Lower a Text_IO procedure call to runtime calls.

        Supports both console I/O (single arg) and file I/O (File_Type first arg).
        """
        # Check if first arg is File_Type for file-based operations
        is_file_based = False
        file_handle = None
        item_arg_idx = 0

        if args and len(args) >= 2:
            first_arg = args[0].value
            if self._is_file_type(first_arg):
                is_file_based = True
                file_handle = self._lower_expr(first_arg)
                item_arg_idx = 1

        if proc_name == "put":
            if args and len(args) > item_arg_idx and args[item_arg_idx].value:
                arg_expr = args[item_arg_idx].value
                if isinstance(arg_expr, StringLiteral):
                    # Create string constant
                    label = self.builder.new_string_label()
                    if self.builder.module:
                        self.builder.module.add_string(label, arg_expr.value)

                    if is_file_based:
                        # File-based: call _file_write(handle, buffer, length)
                        str_len = len(arg_expr.value)
                        self.builder.push(Immediate(str_len, IRType.WORD))
                        self.builder.push(Label(label))
                        self.builder.push(file_handle)
                        self.builder.call(Label("_file_write"), comment="Put to file")
                        for _ in range(3):
                            temp = self.builder.new_vreg(IRType.WORD, "_discard")
                            self.builder.pop(temp)
                    else:
                        # Console: call _put_string
                        self.builder.emit(IRInstr(
                            OpCode.MOV,
                            dst=self.builder.new_vreg(IRType.PTR, "_str"),
                            src1=Label(label),
                        ))
                        self.builder.call(Label("_put_string"))
                else:
                    # Expression - evaluate and print
                    value = self._lower_expr(arg_expr)
                    if is_file_based:
                        # For file output, we need to convert to string first
                        # For now, just write the raw bytes (works for characters)
                        if self._is_character_type(arg_expr):
                            # Single character to file
                            self.builder.push(Immediate(1, IRType.WORD))
                            # Need address of value - push value then get SP
                            self.builder.push(value)
                            # Use stack location as buffer
                            sp_reg = self.builder.new_vreg(IRType.PTR, "_sp")
                            self.builder.emit(IRInstr(OpCode.GETSP, dst=sp_reg))
                            self.builder.push(sp_reg)
                            self.builder.push(file_handle)
                            self.builder.call(Label("_file_write"), comment="Put char to file")
                            for _ in range(4):
                                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                                self.builder.pop(temp)
                        else:
                            # Integer to file - would need conversion
                            self.builder.push(value)
                            self.builder.call(Label("_put_int"))
                            temp = self.builder.new_vreg(IRType.WORD, "_discard")
                            self.builder.pop(temp)
                    else:
                        # Console
                        self.builder.push(value)
                        if self._is_character_type(arg_expr):
                            self.builder.call(Label("_put_char"))
                        else:
                            self.builder.call(Label("_put_int"))
                        temp = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(temp)

        elif proc_name == "put_line":
            # Determine if file-based (first arg is File_Type)
            put_line_file_based = False
            put_line_file_handle = None
            put_line_item_idx = 0
            if args and len(args) >= 2:
                first_arg = args[0].value
                if self._is_file_type(first_arg):
                    put_line_file_based = True
                    put_line_file_handle = self._lower_expr(first_arg)
                    put_line_item_idx = 1

            if args and len(args) > put_line_item_idx and args[put_line_item_idx].value:
                arg_expr = args[put_line_item_idx].value
                if isinstance(arg_expr, StringLiteral):
                    # Create string constant
                    label = self.builder.new_string_label()
                    if self.builder.module:
                        self.builder.module.add_string(label, arg_expr.value)

                    if put_line_file_based:
                        # File-based: write string then newline
                        str_len = len(arg_expr.value)
                        self.builder.push(Immediate(str_len, IRType.WORD))
                        self.builder.push(Label(label))
                        self.builder.push(put_line_file_handle)
                        self.builder.call(Label("_file_write"), comment="Put_Line to file")
                        for _ in range(3):
                            temp = self.builder.new_vreg(IRType.WORD, "_discard")
                            self.builder.pop(temp)
                        # Write newline
                        nl_label = self.builder.new_string_label()
                        if self.builder.module:
                            self.builder.module.add_string(nl_label, "\r\n")
                        self.builder.push(Immediate(2, IRType.WORD))
                        self.builder.push(Label(nl_label))
                        self.builder.push(put_line_file_handle)
                        self.builder.call(Label("_file_write"), comment="newline to file")
                        for _ in range(3):
                            temp = self.builder.new_vreg(IRType.WORD, "_discard")
                            self.builder.pop(temp)
                    else:
                        # Console
                        str_reg = self.builder.new_vreg(IRType.PTR, "_str")
                        self.builder.mov(str_reg, Label(label))
                        self.builder.push(str_reg)
                        self.builder.call(Label("_put_line"))
                        temp = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(temp)
                elif self._is_string_type(arg_expr):
                    value = self._lower_expr(arg_expr)
                    if put_line_file_based:
                        # For file, write string then newline
                        # Get string length - use _str_len runtime
                        self.builder.push(value)
                        self.builder.call(Label("_str_len"))
                        len_reg = self.builder.new_vreg(IRType.WORD, "_len")
                        self.builder.mov(len_reg, MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD))
                        temp = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(temp)
                        # Write string
                        self.builder.push(len_reg)
                        self.builder.push(value)
                        self.builder.push(put_line_file_handle)
                        self.builder.call(Label("_file_write"), comment="Put_Line string to file")
                        for _ in range(3):
                            temp = self.builder.new_vreg(IRType.WORD, "_discard")
                            self.builder.pop(temp)
                        # Write newline
                        nl_label = self.builder.new_string_label()
                        if self.builder.module:
                            self.builder.module.add_string(nl_label, "\r\n")
                        self.builder.push(Immediate(2, IRType.WORD))
                        self.builder.push(Label(nl_label))
                        self.builder.push(put_line_file_handle)
                        self.builder.call(Label("_file_write"), comment="newline to file")
                        for _ in range(3):
                            temp = self.builder.new_vreg(IRType.WORD, "_discard")
                            self.builder.pop(temp)
                    else:
                        self.builder.push(value)
                        self.builder.call(Label("_put_line"))
                        temp = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(temp)
                else:
                    value = self._lower_expr(arg_expr)
                    self.builder.push(value)
                    self.builder.call(Label("_put_int_line"))
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
            else:
                # Just print newline
                if put_line_file_based:
                    nl_label = self.builder.new_string_label()
                    if self.builder.module:
                        self.builder.module.add_string(nl_label, "\r\n")
                    self.builder.push(Immediate(2, IRType.WORD))
                    self.builder.push(Label(nl_label))
                    self.builder.push(put_line_file_handle)
                    self.builder.call(Label("_file_write"), comment="newline to file")
                    for _ in range(3):
                        temp = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(temp)
                else:
                    self.builder.call(Label("_new_line"))

        elif proc_name == "new_line":
            # Check for file-based new_line
            if is_file_based:
                nl_label = self.builder.new_string_label()
                if self.builder.module:
                    self.builder.module.add_string(nl_label, "\r\n")
                self.builder.push(Immediate(2, IRType.WORD))
                self.builder.push(Label(nl_label))
                self.builder.push(file_handle)
                self.builder.call(Label("_file_write"), comment="New_Line to file")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
            else:
                self.builder.call(Label("_new_line"))

        elif proc_name == "get":
            # Get single character or integer into output parameter
            # Check for file-based Get(File, Item)
            get_file_based = False
            get_file_handle = None
            get_item_idx = 0
            if args and len(args) >= 2:
                first_arg = args[0].value
                if self._is_file_type(first_arg):
                    get_file_based = True
                    get_file_handle = self._lower_expr(first_arg)
                    get_item_idx = 1

            if args and len(args) > get_item_idx and args[get_item_idx].value:
                arg_expr = args[get_item_idx].value

                if get_file_based:
                    # File-based: read one byte from file
                    # Allocate temp buffer on stack
                    temp_buf = self.builder.new_vreg(IRType.PTR, "_buf")
                    self.builder.emit(IRInstr(OpCode.GETSP, dst=temp_buf))
                    self.builder.push(Immediate(0, IRType.WORD))  # Reserve space
                    # Read 1 byte
                    self.builder.push(Immediate(1, IRType.WORD))
                    self.builder.push(temp_buf)
                    self.builder.push(get_file_handle)
                    self.builder.call(Label("_file_read"), comment="Get from file")
                    for _ in range(3):
                        temp = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(temp)
                    # Result is in the temp buffer
                    result = self.builder.new_vreg(IRType.WORD, "_char")
                    self.builder.pop(result)  # Pop the reserved space (contains char)
                    self._store_to_target(arg_expr, result)
                else:
                    # Console-based
                    if is_integer_io or self._is_integer_type(arg_expr):
                        self.builder.call(Label("_get_int"))
                    else:
                        self.builder.call(Label("_get_char"))

                    result = MemoryLocation(
                        is_global=False,
                        symbol_name="_HL",
                        ir_type=IRType.WORD,
                    )
                    self._store_to_target(arg_expr, result)

        elif proc_name == "get_line":
            # Get line into output string parameter
            # Check for file-based Get_Line(File, Item, Last)
            get_line_file_based = False
            get_line_file_handle = None
            get_line_item_idx = 0
            if args and len(args) >= 2:
                first_arg = args[0].value
                if self._is_file_type(first_arg):
                    get_line_file_based = True
                    get_line_file_handle = self._lower_expr(first_arg)
                    get_line_item_idx = 1

            if args and len(args) > get_line_item_idx and args[get_line_item_idx].value:
                arg_expr = args[get_line_item_idx].value

                if isinstance(arg_expr, Identifier):
                    name = arg_expr.name.lower()
                    if self.ctx and name in self.ctx.locals:
                        local = self.ctx.locals[name]
                        neg_offset = -(self.ctx.locals_size - local.stack_offset)
                        addr_reg = self.builder.new_vreg(IRType.PTR, "_buf")
                        self.builder.emit(IRInstr(
                            OpCode.LEA,
                            dst=addr_reg,
                            src1=MemoryLocation(offset=neg_offset, ir_type=IRType.PTR),
                        ))
                        max_len = self._get_string_max_length(arg_expr)

                        if get_line_file_based:
                            # File-based: read line from file
                            self.builder.push(Immediate(max_len, IRType.WORD))
                            self.builder.push(addr_reg)
                            self.builder.push(get_line_file_handle)
                            self.builder.call(Label("_file_read"), comment="Get_Line from file")
                            # Result in HL is bytes read
                            last_idx = get_line_item_idx + 1
                            if len(args) > last_idx and args[last_idx].value:
                                last_expr = args[last_idx].value
                                result = MemoryLocation(
                                    is_global=False,
                                    symbol_name="_HL",
                                    ir_type=IRType.WORD,
                                )
                                self._store_to_target(last_expr, result)
                            for _ in range(3):
                                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                                self.builder.pop(temp)
                        else:
                            # Console-based
                            self.builder.push(addr_reg)
                            self.builder.push(Immediate(max_len, IRType.WORD))
                            self.builder.call(Label("_get_line"))
                            if len(args) >= 2 and args[1].value:
                                last_expr = args[1].value
                                result = MemoryLocation(
                                    is_global=False,
                                    symbol_name="_HL",
                                    ir_type=IRType.WORD,
                                )
                                self._store_to_target(last_expr, result)
                            temp = self.builder.new_vreg(IRType.WORD, "_discard")
                            self.builder.pop(temp)
                            self.builder.pop(temp)

    def _lower_file_operation(self, op: str, args: list) -> None:
        """Lower file I/O operations (Create, Open, Close, Delete, Reset, Flush).

        These operations work with File_Type parameters and call the runtime
        file I/O functions (_file_open, _file_create, _file_close, etc.)
        """
        if not args:
            return

        if op == "create":
            # Create(File : in out File_Type; Mode := Out_File; Name := ""; Form := "")
            # First arg is the File variable (in out)
            # Name is the filename (usually 3rd positional or named)
            file_arg = args[0].value
            file_addr = self._get_lvalue_address(file_arg)

            # Get filename - usually 3rd arg or default to empty string
            filename_val = None
            if len(args) >= 3 and args[2].value:
                name_arg = args[2].value
                if isinstance(name_arg, StringLiteral):
                    # Create string constant
                    label = self.builder.new_string_label()
                    if self.builder.module:
                        self.builder.module.add_string(label, name_arg.value)
                    filename_val = Label(label)
                else:
                    filename_val = self._lower_expr(name_arg)
            else:
                # Check for named parameter "Name"
                for arg in args[1:]:
                    if arg.name and arg.name.lower() == "name" and arg.value:
                        if isinstance(arg.value, StringLiteral):
                            label = self.builder.new_string_label()
                            if self.builder.module:
                                self.builder.module.add_string(label, arg.value.value)
                            filename_val = Label(label)
                        else:
                            filename_val = self._lower_expr(arg.value)
                        break

            if filename_val:
                # Push filename pointer
                self.builder.push(filename_val)
                # Call _file_create
                self.builder.call(Label("_file_create"), comment="Text_IO.Create")
                # Result in HL is file handle
                # Store to file variable
                result = MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD)
                self._store_to_address(file_addr, result)
                # Clean up stack
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)

        elif op == "open":
            # Open(File : in out File_Type; Mode : File_Mode; Name : String; Form := "")
            file_arg = args[0].value
            file_addr = self._get_lvalue_address(file_arg)

            # Get mode - 2nd arg (0=read, 1=write, 2=read/write)
            mode_val = Immediate(0, IRType.WORD)  # Default In_File
            if len(args) >= 2 and args[1].value:
                mode_arg = args[1].value
                if isinstance(mode_arg, Identifier):
                    mode_name = mode_arg.name.lower()
                    if mode_name == "in_file":
                        mode_val = Immediate(0, IRType.WORD)
                    elif mode_name == "out_file":
                        mode_val = Immediate(1, IRType.WORD)
                    elif mode_name in ("append_file", "inout_file"):
                        mode_val = Immediate(2, IRType.WORD)
                else:
                    mode_val = self._lower_expr(mode_arg)

            # Get filename - 3rd arg
            filename_val = None
            if len(args) >= 3 and args[2].value:
                name_arg = args[2].value
                if isinstance(name_arg, StringLiteral):
                    label = self.builder.new_string_label()
                    if self.builder.module:
                        self.builder.module.add_string(label, name_arg.value)
                    filename_val = Label(label)
                else:
                    filename_val = self._lower_expr(name_arg)

            if filename_val:
                # Push mode, then filename
                self.builder.push(mode_val)
                self.builder.push(filename_val)
                # Call _file_open(filename, mode) - args in reverse order on stack
                self.builder.call(Label("_file_open"), comment="Text_IO.Open")
                # Result in HL is file handle
                result = MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD)
                self._store_to_address(file_addr, result)
                # Clean up stack (2 words)
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)

        elif op == "close":
            # Close(File : in out File_Type)
            file_arg = args[0].value
            file_val = self._lower_expr(file_arg)
            self.builder.push(file_val)
            self.builder.call(Label("_file_close"), comment="Text_IO.Close")
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)

        elif op == "delete":
            # Delete(File : in out File_Type) - close and delete
            file_arg = args[0].value
            file_val = self._lower_expr(file_arg)
            # First close
            self.builder.push(file_val)
            self.builder.call(Label("_file_close"), comment="Text_IO.Delete (close)")
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            # Then delete (would need filename - for now just close)
            # TODO: Implement proper file deletion

        elif op == "reset":
            # Reset(File : in out File_Type; Mode : File_Mode := In_File)
            # For now, just close and reopen is too complex
            # Simple implementation: just rewind to start
            file_arg = args[0].value
            file_val = self._lower_expr(file_arg)
            # For now, just call close - proper reset would need to track filename
            self.builder.push(file_val)
            self.builder.call(Label("_file_close"), comment="Text_IO.Reset (close only)")
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)

        elif op == "flush":
            # Flush(File : File_Type)
            # For now, no-op - CP/M flushes on close
            pass

    def _get_lvalue_address(self, expr):
        """Get the address of an lvalue expression for storing into."""
        if isinstance(expr, Identifier):
            name = expr.name.lower()
            if self.ctx and name in self.ctx.locals:
                local = self.ctx.locals[name]
                neg_offset = -(self.ctx.locals_size - local.stack_offset)
                addr_reg = self.builder.new_vreg(IRType.PTR, "_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr_reg,
                    src1=MemoryLocation(offset=neg_offset, ir_type=IRType.PTR),
                ))
                return addr_reg
            else:
                # Global variable
                return Label(f"_{name}")
        return None

    def _store_to_address(self, addr, value):
        """Store a value to an address (for file handle storage)."""
        if addr is None:
            return
        if isinstance(addr, Label):
            self.builder.store(addr, value)
        else:
            # Indirect store through pointer
            self.builder.store_indirect(addr, value)

    def _get_type_size(self, expr) -> int:
        """Get the size in bytes of an expression's type.

        Used for Sequential_IO and stream operations.
        """
        # Try to determine size from type
        if isinstance(expr, Identifier):
            name = expr.name.lower()
            if self.ctx and name in self.ctx.locals:
                local = self.ctx.locals[name]
                if local.size:
                    return local.size
                if local.type_ref:
                    return self._sizeof_type(local.type_ref)
            if self.symbol_table:
                sym = self.symbol_table.lookup(expr.name)
                if sym and sym.type_ref:
                    return self._sizeof_type(sym.type_ref)
        # Default to word size (2 bytes)
        return 2

    def _sizeof_type(self, type_ref) -> int:
        """Calculate size of a type in bytes."""
        if type_ref is None:
            return 2
        type_name = str(type_ref).lower()
        # Standard types
        if type_name in ("integer", "natural", "positive"):
            return 2
        if type_name in ("character", "boolean"):
            return 1
        if type_name in ("long_integer",):
            return 4
        if type_name in ("long_float", "float64"):
            return 8
        if type_name in ("short_integer",):
            return 2
        # Check for array/record in symbol table
        if self.symbol_table:
            sym = self.symbol_table.lookup(str(type_ref))
            if sym:
                if hasattr(sym, 'size') and sym.size:
                    return sym.size
                if hasattr(sym, 'type_def') and sym.type_def:
                    td = sym.type_def
                    if hasattr(td, 'size') and td.size:
                        return td.size
        # Default
        return 2

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
                # Calculate frame offset
                frame_offset = -(self.ctx.locals_size - local.stack_offset)

                # Check if this is an array type that stores a pointer (size == 2)
                # vs inline array data (size > 2)
                # When size == 2 for an array, the local stores a pointer to the array data
                # (e.g., from concatenation result or aggregate allocation)
                local_type = self._resolve_local_type(local.ada_type) if local.ada_type else None
                is_array_type = (local_type and hasattr(local_type, 'kind') and
                                 local_type.kind == TypeKind.ARRAY)

                if is_array_type and local.size == 2:
                    # Local stores a pointer to array data - LOAD the pointer value
                    addr = self.builder.new_vreg(IRType.PTR, f"_{name}_ptr")
                    self.builder.emit(IRInstr(
                        OpCode.LOAD,
                        dst=addr,
                        src1=MemoryLocation(offset=frame_offset, ir_type=IRType.WORD, is_frame_offset=True),
                        comment=f"load array pointer from {name}"
                    ))
                    return addr
                else:
                    # Inline array data or non-array - use LEA
                    addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                    self.builder.emit(IRInstr(
                        OpCode.LEA,
                        dst=addr,
                        src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                    ))
                    return addr

            # Check for global arrays
            sym = self.symbols.lookup(name)
            if sym and sym.kind == SymbolKind.VARIABLE:
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr,
                    src1=self._make_memory_location(
                        name, is_global=True, ir_type=IRType.PTR, symbol=sym
                    ),
                ))
                return addr

        # Handle record field that is an array (e.g., X.Values where Values is an array field)
        if isinstance(prefix, SelectedName):
            outer_base = self._get_record_base(prefix.prefix)
            if outer_base is not None:
                # Get the field offset of the array within the record
                field_offset = self._get_field_offset(prefix)
                if field_offset != 0:
                    array_addr = self.builder.new_vreg(IRType.PTR, "_array_field_addr")
                    self.builder.add(array_addr, outer_base, Immediate(field_offset, IRType.WORD))
                    return array_addr
                else:
                    # Array field at offset 0, just use outer base
                    return outer_base

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
            name = indexed.prefix.name.lower()
            sym = self.symbols.lookup(name)
            if sym and sym.ada_type and hasattr(sym.ada_type, 'bounds') and sym.ada_type.bounds:
                bounds_list = list(sym.ada_type.bounds)
            if sym and sym.ada_type and hasattr(sym.ada_type, 'component_type'):
                comp_type = sym.ada_type.component_type
                if comp_type:
                    element_size = (comp_type.size_bits + 7) // 8

            # For local variables, try to get bounds from the type declaration
            if not bounds_list and self.ctx and name in self.ctx.locals:
                local = self.ctx.locals[name]
                # local.ada_type may be SubtypeIndication, Identifier, or other
                type_name = None
                if local.ada_type:
                    if hasattr(local.ada_type, 'type_mark'):
                        # SubtypeIndication
                        tm = local.ada_type.type_mark
                        if hasattr(tm, 'name'):
                            type_name = tm.name.lower()
                    elif hasattr(local.ada_type, 'name'):
                        # Direct Identifier
                        type_name = local.ada_type.name.lower()
                if type_name:
                    # Look up the type in local declarations
                    if hasattr(self, '_current_body_declarations'):
                        for d in self._current_body_declarations:
                            if isinstance(d, TypeDecl) and d.name.lower() == type_name:
                                type_def = d.type_def
                                if hasattr(type_def, 'index_subtypes') and type_def.index_subtypes:
                                    # Get bounds for ALL dimensions
                                    for idx_range in type_def.index_subtypes:
                                        first = self._eval_static_expr(getattr(idx_range, 'low', None))
                                        last = self._eval_static_expr(getattr(idx_range, 'high', None))
                                        if first is not None and last is not None:
                                            bounds_list.append((first, last))
                                    # Get element size
                                    if hasattr(type_def, 'component_type') and type_def.component_type:
                                        comp_name = ''
                                        if isinstance(type_def.component_type, Identifier):
                                            comp_name = type_def.component_type.name.lower()
                                        elif hasattr(type_def.component_type, 'name'):
                                            comp_name = type_def.component_type.name.lower()
                                        if comp_name in ('character', 'boolean'):
                                            element_size = 1
                                        elif comp_name == 'float':
                                            element_size = 6
                                        elif comp_name:
                                            # Check if it's a record type
                                            for rd in self._current_body_declarations:
                                                if isinstance(rd, TypeDecl) and rd.name.lower() == comp_name:
                                                    rd_type_def = rd.type_def
                                                    if hasattr(rd_type_def, 'components') or hasattr(rd_type_def, 'fields'):
                                                        # Calculate record size
                                                        fields = getattr(rd_type_def, 'fields', None) or getattr(rd_type_def, 'components', [])
                                                        if isinstance(fields, dict):
                                                            element_size = len(fields) * 2
                                                        else:
                                                            total_fields = 0
                                                            for comp in fields:
                                                                if hasattr(comp, 'names') and isinstance(comp.names, list):
                                                                    total_fields += len(comp.names)
                                                                else:
                                                                    total_fields += 1
                                                            element_size = total_fields * 2
                                                    break
                                break

        # Handle SelectedName prefix (e.g., X.Values where X is a record with array field Values)
        elif isinstance(indexed.prefix, SelectedName):
            from uada80.type_system import RecordType, ArrayType
            # Get the record variable's type
            rec_prefix = indexed.prefix.prefix
            field_name = indexed.prefix.selector.lower()
            rec_type = None

            if isinstance(rec_prefix, Identifier):
                var_name = rec_prefix.name.lower()
                # Check local variables
                if self.ctx and var_name in self.ctx.locals:
                    local = self.ctx.locals[var_name]
                    if local.ada_type:
                        rec_type = self._resolve_local_type(local.ada_type)
                # Check symbol table
                if rec_type is None:
                    sym = self.symbols.lookup(var_name)
                    if sym and sym.ada_type:
                        rec_type = sym.ada_type

            # If we found a record type, find the array field
            if rec_type and isinstance(rec_type, RecordType):
                for comp in rec_type.components:
                    if comp.name.lower() == field_name:
                        field_type = comp.component_type
                        if isinstance(field_type, ArrayType):
                            if field_type.bounds:
                                bounds_list = list(field_type.bounds)
                            if field_type.component_type:
                                element_size = (field_type.component_type.size_bits + 7) // 8
                        break

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

        # Calculate element address by adding offset to base
        # Note: _get_array_base now returns the correct frame offset, so we always ADD
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

        # Get field bit-level info and atomic/volatile flags
        byte_offset, bit_offset, bit_size, is_packed = self._get_field_bit_info(target)
        is_atomic, is_volatile = self._get_field_atomic_volatile(target)

        # Calculate field address (byte-aligned)
        if byte_offset != 0:
            field_addr = self.builder.new_vreg(IRType.PTR, "_field_addr")
            self.builder.add(field_addr, base_addr, Immediate(byte_offset, IRType.WORD))
        else:
            field_addr = base_addr

        if is_packed and (bit_offset != 0 or bit_size < 8):
            # Packed field with sub-byte access - generate read-modify-write
            # Load the byte containing the field
            byte_val = self.builder.new_vreg(IRType.BYTE, "_packed_byte")
            mem = MemoryLocation(
                base=field_addr, offset=0, ir_type=IRType.BYTE,
                is_atomic=is_atomic, is_volatile=is_volatile
            )
            self.builder.load(byte_val, mem)

            # Clear the bits for this field
            field_mask = ((1 << bit_size) - 1) << bit_offset
            clear_mask = 0xFF ^ field_mask
            cleared = self.builder.new_vreg(IRType.BYTE, "_cleared")
            self.builder.and_(cleared, byte_val, Immediate(clear_mask, IRType.BYTE))

            # Shift and mask the new value, then OR it in
            masked_val = self.builder.new_vreg(IRType.BYTE, "_masked_val")
            self.builder.and_(masked_val, value, Immediate((1 << bit_size) - 1, IRType.BYTE))

            if bit_offset > 0:
                shifted_val = self.builder.new_vreg(IRType.BYTE, "_shifted_val")
                self.builder.shl(shifted_val, masked_val, Immediate(bit_offset, IRType.BYTE))
                new_byte = self.builder.new_vreg(IRType.BYTE, "_new_byte")
                self.builder.or_(new_byte, cleared, shifted_val)
            else:
                new_byte = self.builder.new_vreg(IRType.BYTE, "_new_byte")
                self.builder.or_(new_byte, cleared, masked_val)

            # Store the modified byte back
            self.builder.store(mem, new_byte)
        else:
            # Normal byte-aligned field store
            mem = MemoryLocation(
                base=field_addr, offset=0, ir_type=IRType.WORD,
                is_atomic=is_atomic, is_volatile=is_volatile
            )
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
                # Calculate frame offset: records start at IX-(locals_size - stack_offset)
                # E.g., for a 4-byte record at stack_offset=0 with locals_size=4:
                #   frame_offset = -(4 - 0) = -4, so record is at IX-4 through IX-1
                frame_offset = -(self.ctx.locals_size - local.stack_offset)
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr,
                    src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                ))
                return addr

            # Check parameters (records passed by reference)
            if name in self.ctx.params:
                param_vreg = self.ctx.params[name]
                # If byref, the vreg already holds the address
                if name in self.ctx.byref_params:
                    return param_vreg
                # Otherwise, get address of the parameter
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr,
                    src1=param_vreg,
                ))
                return addr

            # Check for global records
            sym = self.symbols.lookup(name)
            if sym and sym.kind == SymbolKind.VARIABLE:
                addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=addr,
                    src1=self._make_memory_location(
                        name, is_global=True, ir_type=IRType.PTR, symbol=sym
                    ),
                ))
                return addr

        # Handle nested record access (e.g., O.I in O.I.X)
        # For SelectedName prefix, get the outer record base and add field offset
        if isinstance(prefix, SelectedName):
            outer_base = self._get_record_base(prefix.prefix)
            if outer_base is not None:
                # Get the field offset of the inner record within the outer
                field_offset = self._get_field_offset(prefix)
                if field_offset != 0:
                    inner_addr = self.builder.new_vreg(IRType.PTR, "_nested_addr")
                    self.builder.add(inner_addr, outer_base, Immediate(field_offset, IRType.WORD))
                    return inner_addr
                else:
                    # Field at offset 0, just use outer base
                    return outer_base

        # Handle array element that is a record (e.g., P(2) in P(2).X)
        # For IndexedComponent prefix, compute the element address
        if isinstance(prefix, IndexedComponent):
            # Get the array base address
            array_base = self._get_array_base(prefix.prefix)
            if array_base is not None:
                # Calculate element address (similar to _calc_element_addr but for records)
                elem_addr = self._calc_element_addr(prefix, array_base, check_bounds=False)
                # elem_addr is a MemoryLocation, we need to compute the actual address
                if elem_addr.base:
                    return elem_addr.base
                else:
                    # Static offset - compute address from array base
                    addr = self.builder.new_vreg(IRType.PTR, "_elem_addr")
                    self.builder.add(addr, array_base, Immediate(elem_addr.offset, IRType.WORD))
                    return addr

        return None

    def _get_field_offset(self, selected: SelectedName) -> int:
        """Get the byte offset of a record field."""
        from uada80.type_system import RecordType

        if isinstance(selected.prefix, Identifier):
            var_name = selected.prefix.name.lower()

            # First try symbol table lookup
            sym = self.symbols.lookup(selected.prefix.name)
            if sym and sym.ada_type and isinstance(sym.ada_type, RecordType):
                for comp in sym.ada_type.components:
                    if comp.name.lower() == selected.selector.lower():
                        return comp.offset_bits // 8

            # If symbol table lookup failed, try to find type from local declarations
            # This handles locally-declared record types
            if self.ctx and var_name in self.ctx.locals:
                local = self.ctx.locals[var_name]
                type_mark = local.ada_type
                if type_mark:
                    # Get type name from type_mark (which is an AST node)
                    type_name = None
                    if isinstance(type_mark, SubtypeIndication):
                        inner = getattr(type_mark, 'type_mark', None)
                        if isinstance(inner, Identifier):
                            type_name = inner.name.lower()
                        elif isinstance(inner, IndexedComponent):
                            # Discriminated record: Buffer(10) parsed as IndexedComponent
                            if isinstance(inner.prefix, Identifier):
                                type_name = inner.prefix.name.lower()
                    elif isinstance(type_mark, Identifier):
                        type_name = type_mark.name.lower()

                    # Look up the type definition in declarations
                    if type_name and hasattr(self, '_current_body_declarations'):
                        for decl in self._current_body_declarations:
                            if isinstance(decl, TypeDecl) and decl.name.lower() == type_name:
                                type_def = decl.type_def
                                # Calculate field offset based on position
                                offset = 0
                                # First check discriminants (they come first in memory)
                                if hasattr(decl, 'discriminants') and decl.discriminants:
                                    for disc in decl.discriminants:
                                        disc_names = getattr(disc, 'names', [])
                                        for dn in disc_names:
                                            if dn.lower() == selected.selector.lower():
                                                return offset
                                            offset += 2  # Each discriminant is 2 bytes
                                # Then check regular components
                                if hasattr(type_def, 'components'):
                                    for comp in type_def.components:
                                        # ComponentDecl has 'names' (list), not 'name'
                                        comp_names = getattr(comp, 'names', [])
                                        if not comp_names:
                                            comp_names = [getattr(comp, 'name', '')]
                                        for cn in comp_names:
                                            if cn.lower() == selected.selector.lower():
                                                return offset
                                            offset += 2  # Each field is 2 bytes
                                elif hasattr(type_def, 'fields'):
                                    for field_name in type_def.fields:
                                        if field_name.lower() == selected.selector.lower():
                                            return offset
                                        offset += 2
                                break
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

    def _get_field_bit_info(self, selected: SelectedName) -> tuple[int, int, int, bool]:
        """Get detailed bit-level information for a record field.

        Returns (byte_offset, bit_offset, bit_size, is_packed) tuple.
        - byte_offset: Offset in bytes from start of record
        - bit_offset: Bit offset within the byte (0-7)
        - bit_size: Size of field in bits (packed size if applicable)
        - is_packed: True if this is a packed field with sub-byte access
        """
        from uada80.type_system import RecordType

        if isinstance(selected.prefix, Identifier):
            var_name = selected.prefix.name.lower()

            # First try symbol table lookup (for global variables)
            sym = self.symbols.lookup(selected.prefix.name)
            if sym and sym.ada_type and isinstance(sym.ada_type, RecordType):
                record_type = sym.ada_type
                for comp in record_type.components:
                    if comp.name.lower() == selected.selector.lower():
                        offset_bits = comp.offset_bits
                        # Use packed size if set, otherwise component type size
                        size_bits = comp.size_bits if comp.size_bits else comp.component_type.size_bits
                        byte_offset = offset_bits // 8
                        bit_offset = offset_bits % 8
                        # It's a packed sub-byte field if bit_offset != 0 or size < 8
                        is_packed = record_type.is_packed and (bit_offset != 0 or size_bits < 8)
                        return (byte_offset, bit_offset, size_bits, is_packed)

            # If symbol table lookup failed, try to find type from local declarations
            if self.ctx and var_name in self.ctx.locals:
                local = self.ctx.locals[var_name]
                type_mark = local.ada_type
                if type_mark:
                    # Get type name from type_mark (which is an AST node)
                    type_name = None
                    if isinstance(type_mark, SubtypeIndication):
                        inner = getattr(type_mark, 'type_mark', None)
                        if isinstance(inner, Identifier):
                            type_name = inner.name.lower()
                        elif isinstance(inner, IndexedComponent):
                            # Discriminated record: Buffer(10) parsed as IndexedComponent
                            if isinstance(inner.prefix, Identifier):
                                type_name = inner.prefix.name.lower()
                    elif isinstance(type_mark, Identifier):
                        type_name = type_mark.name.lower()

                    # Look up the type from _current_body_declarations
                    # This preserves pragma Pack info stored on the AST node
                    if type_name and hasattr(self, '_current_body_declarations'):
                        for decl in self._current_body_declarations:
                            if isinstance(decl, TypeDecl) and decl.name.lower() == type_name:
                                # For discriminated records, check discriminants first
                                offset = 0
                                if hasattr(decl, 'discriminants') and decl.discriminants:
                                    for disc in decl.discriminants:
                                        disc_names = getattr(disc, 'names', [])
                                        for dn in disc_names:
                                            if dn.lower() == selected.selector.lower():
                                                return (offset, 0, 16, False)  # Discriminant at byte offset
                                            offset += 2  # Each discriminant is 2 bytes

                                # Then check regular components - use ada_type if available
                                if hasattr(decl, 'ada_type') and decl.ada_type and isinstance(decl.ada_type, RecordType):
                                    record_type = decl.ada_type
                                    for comp in record_type.components:
                                        if comp.name.lower() == selected.selector.lower():
                                            # Add discriminant offset to component's offset
                                            comp_offset_bits = comp.offset_bits + (offset * 8)
                                            size_bits = comp.size_bits if comp.size_bits else comp.component_type.size_bits
                                            byte_offset = comp_offset_bits // 8
                                            bit_offset = comp_offset_bits % 8
                                            is_packed = record_type.is_packed and (bit_offset != 0 or size_bits < 8)
                                            return (byte_offset, bit_offset, size_bits, is_packed)
                                else:
                                    # Fallback: calculate offset from type_def.components
                                    type_def = decl.type_def
                                    if hasattr(type_def, 'components'):
                                        for comp in type_def.components:
                                            comp_names = getattr(comp, 'names', [])
                                            for cn in comp_names:
                                                if cn.lower() == selected.selector.lower():
                                                    return (offset, 0, 16, False)
                                                offset += 2
                                break

            # Check parameters: look up type from param_types
            if self.ctx and var_name in self.ctx.params and var_name in self.ctx.param_types:
                type_name = self.ctx.param_types[var_name]
                # Look up the type from all declarations in the stack (for nested scopes)
                if type_name and hasattr(self, '_body_declarations_stack'):
                    for decl_list in self._body_declarations_stack:
                        for decl in decl_list:
                            if isinstance(decl, TypeDecl) and decl.name.lower() == type_name:
                                # Use the ada_type stored on the AST node during semantic analysis
                                if hasattr(decl, 'ada_type') and decl.ada_type and isinstance(decl.ada_type, RecordType):
                                    record_type = decl.ada_type
                                    for comp in record_type.components:
                                        if comp.name.lower() == selected.selector.lower():
                                            offset_bits = comp.offset_bits
                                            size_bits = comp.size_bits if comp.size_bits else comp.component_type.size_bits
                                            byte_offset = offset_bits // 8
                                            bit_offset = offset_bits % 8
                                            is_packed = record_type.is_packed and (bit_offset != 0 or size_bits < 8)
                                            return (byte_offset, bit_offset, size_bits, is_packed)
                                break

        # Handle array element record field (e.g., P(2).X where P is array of records)
        if isinstance(selected.prefix, IndexedComponent):
            # Get the array's element type (record type)
            array_prefix = selected.prefix.prefix
            array_name = None
            if isinstance(array_prefix, Identifier):
                array_name = array_prefix.name.lower()

            if array_name and hasattr(self, '_current_body_declarations'):
                # Find the array variable and its type
                array_type_name = None
                if self.ctx and array_name in self.ctx.locals:
                    local = self.ctx.locals[array_name]
                    if local.ada_type:
                        if isinstance(local.ada_type, Identifier):
                            array_type_name = local.ada_type.name.lower()
                        elif isinstance(local.ada_type, SubtypeIndication):
                            inner = getattr(local.ada_type, 'type_mark', None)
                            if isinstance(inner, Identifier):
                                array_type_name = inner.name.lower()

                if array_type_name:
                    # Find the array type declaration to get component type
                    for decl in self._current_body_declarations:
                        if isinstance(decl, TypeDecl) and decl.name.lower() == array_type_name:
                            from uada80.ast_nodes import ArrayTypeDef
                            if isinstance(decl.type_def, ArrayTypeDef):
                                # Get the component type name (e.g., Point)
                                comp_type = decl.type_def.component_type
                                comp_type_name = None
                                if isinstance(comp_type, Identifier):
                                    comp_type_name = comp_type.name.lower()
                                elif isinstance(comp_type, SubtypeIndication):
                                    inner = getattr(comp_type, 'type_mark', None)
                                    if isinstance(inner, Identifier):
                                        comp_type_name = inner.name.lower()

                                if comp_type_name:
                                    # Find the record type to get field offset
                                    for rd in self._current_body_declarations:
                                        if isinstance(rd, TypeDecl) and rd.name.lower() == comp_type_name:
                                            from uada80.ast_nodes import RecordTypeDef
                                            if isinstance(rd.type_def, RecordTypeDef):
                                                # Find the field offset
                                                offset = 0
                                                for comp_decl in rd.type_def.components:
                                                    comp_size = 2  # Default integer size
                                                    for comp_name in comp_decl.names:
                                                        if comp_name.lower() == selected.selector.lower():
                                                            return (offset, 0, 16, False)
                                                        offset += comp_size
                                            break
                            break

        return (0, 0, 16, False)  # Default to word at offset 0

    def _get_field_atomic_volatile(self, selected: SelectedName) -> tuple[bool, bool]:
        """Get the atomic and volatile flags for a record field.

        Returns (is_atomic, is_volatile) tuple.
        Checks both the field's own flags and the parent record's flags.
        """
        from uada80.type_system import RecordType

        is_atomic = False
        is_volatile = False

        if isinstance(selected.prefix, Identifier):
            sym = self.symbols.lookup(selected.prefix.name)
            if sym:
                # Check record-level atomic/volatile (applies to all fields)
                if sym.is_atomic:
                    is_atomic = True
                if sym.is_volatile:
                    is_volatile = True

                # Check field-level atomic/volatile
                if sym.ada_type and isinstance(sym.ada_type, RecordType):
                    for comp in sym.ada_type.components:
                        if comp.name.lower() == selected.selector.lower():
                            if comp.is_atomic:
                                is_atomic = True
                            if comp.is_volatile:
                                is_volatile = True
                            break

        return (is_atomic, is_volatile)

    def _lower_selected(self, expr: SelectedName):
        """Lower a selected component (record field access or pointer dereference)."""
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Check if this is a function call from a nested package (e.g., Inner.Get)
        if isinstance(expr.prefix, Identifier):
            pkg_name = expr.prefix.name.lower()
            func_name = expr.selector.lower()
            # Check tracked nested package functions
            if hasattr(self, '_nested_package_functions'):
                pkg_funcs = self._nested_package_functions.get(pkg_name, set())
                if func_name in pkg_funcs:
                    # It's a parameterless function call to nested package function
                    func_call = FunctionCall(
                        name=Identifier(name=expr.selector),
                        args=[],
                    )
                    return self._lower_function_call(func_call)

        # Check symbol table for function (handles top-level packages)
        full_name = self._get_selected_name_str(expr)
        func_sym = self.symbols.lookup(full_name)
        if func_sym is None:
            # Also check just the selector as a nested function
            func_sym = self.symbols.lookup(expr.selector)
        if func_sym is not None and getattr(func_sym, 'is_function', False):
            # It's a parameterless function call
            func_call = FunctionCall(
                name=Identifier(name=full_name),
                args=[],
            )
            return self._lower_function_call(func_call)

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

        # Get field bit-level info and atomic/volatile flags
        byte_offset, bit_offset, bit_size, is_packed = self._get_field_bit_info(expr)
        is_atomic, is_volatile = self._get_field_atomic_volatile(expr)

        # Calculate field address (byte-aligned)
        if byte_offset != 0:
            field_addr = self.builder.new_vreg(IRType.PTR, "_field_addr")
            self.builder.add(field_addr, base_addr, Immediate(byte_offset, IRType.WORD))
        else:
            field_addr = base_addr

        if is_packed and (bit_offset != 0 or bit_size < 8):
            # Packed field with sub-byte access - generate bit extraction
            # Load the byte containing the field
            byte_val = self.builder.new_vreg(IRType.BYTE, "_packed_byte")
            mem = MemoryLocation(
                base=field_addr, offset=0, ir_type=IRType.BYTE,
                is_atomic=is_atomic, is_volatile=is_volatile
            )
            self.builder.load(byte_val, mem)

            result = self.builder.new_vreg(IRType.WORD, "_field")
            if bit_offset > 0:
                # Shift right to align the field
                shifted = self.builder.new_vreg(IRType.BYTE, "_shifted")
                self.builder.shr(shifted, byte_val, Immediate(bit_offset, IRType.BYTE))
                # Mask to get only the field bits
                mask = (1 << bit_size) - 1
                self.builder.and_(result, shifted, Immediate(mask, IRType.WORD))
            else:
                # Field is at bit 0, just mask
                mask = (1 << bit_size) - 1
                self.builder.and_(result, byte_val, Immediate(mask, IRType.WORD))
        else:
            # Normal byte-aligned field access
            result = self.builder.new_vreg(IRType.WORD, "_field")
            mem = MemoryLocation(
                base=field_addr, offset=0, ir_type=IRType.WORD,
                is_atomic=is_atomic, is_volatile=is_volatile
            )
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
            # Tagged types need an extra 2 bytes at offset 0 for the tag (vtable ptr)
            is_tagged = (isinstance(designated_type, RecordType) and
                        designated_type.is_tagged and
                        not designated_type.is_class_wide)
            if is_tagged:
                size += 2  # Add space for tag
        else:
            size = 2  # Default to word size
            is_tagged = False

        # Check for custom storage pool
        # Access types can have a storage pool specified via representation clause
        storage_pool = None
        if hasattr(expr, 'subtype_mark'):
            access_type_name = self._get_type_name_from_expr(expr.subtype_mark) if expr.subtype_mark else None
            if access_type_name:
                access_sym = self.symbols.lookup(access_type_name)
                if access_sym and isinstance(access_sym.ada_type, AccessType):
                    storage_pool = access_sym.ada_type.storage_pool

        # Allocate using storage pool
        size_val = Immediate(size, IRType.WORD)

        if storage_pool:
            # Use custom storage pool
            # Load pool dispatch table address
            pool_addr = self.builder.new_vreg(IRType.PTR, "_pool_addr")
            pool_mem = MemoryLocation(is_global=True, symbol_name=f"_{storage_pool}_pool", ir_type=IRType.PTR)
            self.builder.lea(pool_addr, pool_mem, comment=f"pool {storage_pool}")

            # Store size in DE, pool addr in HL
            self.builder.emit(IRInstr(
                OpCode.MOV,
                MemoryLocation(is_global=False, symbol_name="_DE", ir_type=IRType.WORD),
                size_val,
                comment="size to DE"
            ))
            self.builder.emit(IRInstr(
                OpCode.MOV,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                pool_addr,
                comment="pool to HL"
            ))
            self.builder.call(Label("_pool_alloc"), comment=f"alloc from {storage_pool}")
            # No stack cleanup needed - _pool_alloc uses registers
        else:
            # Default: use _heap_alloc directly
            self.builder.push(size_val, comment=f"alloc size {size}")
            self.builder.call(Label("_heap_alloc"), comment="allocate heap memory")
            # Result is returned in HL (Z80 convention), capture it BEFORE stack cleanup
            # because pop destroys HL
            result = self.builder.new_vreg(IRType.PTR, "_alloc_ptr")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture heap result from HL"
            ))
            # Clean up stack (pop the size argument)
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)

        # For custom storage pool case, capture result here
        if storage_pool:
            result = self.builder.new_vreg(IRType.PTR, "_alloc_ptr")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture pool alloc result from HL"
            ))

        # Initialize tag for tagged types (vtable pointer at offset 0)
        if is_tagged:
            vtable_name = f"_vtable_{designated_type.name}"
            vtable_addr = MemoryLocation(
                is_global=True, symbol_name=vtable_name, ir_type=IRType.PTR
            )
            tag_loc = MemoryLocation(base=result, offset=0, ir_type=IRType.PTR)
            # Load vtable address and store as tag
            vtable_val = self.builder.new_vreg(IRType.PTR, "_vtable_addr")
            self.builder.lea(vtable_val, vtable_addr, comment=f"vtable addr for {designated_type.name}")
            self.builder.store(tag_loc, vtable_val, comment="init tag (vtable ptr)")

        # If there's an initial value, store it
        if expr.initial_value:
            # Handle single-component aggregate as simple value (e.g., Integer'(42))
            init_expr = expr.initial_value
            if isinstance(init_expr, Aggregate) and len(init_expr.components) == 1:
                # Single positional component - just use the value directly
                comp = init_expr.components[0]
                if not comp.choices:  # Positional (no named association)
                    init_expr = comp.value
            init_val = self._lower_expr(init_expr)
            # For tagged types, data starts at offset 2 (after the tag)
            data_offset = 2 if is_tagged else 0
            mem = MemoryLocation(base=result, offset=data_offset, ir_type=IRType.WORD)
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

    def _emit_memcpy(self, dst, src, size: int, comment: str = "") -> None:
        """Emit memory copy from src to dst for size bytes.

        For small sizes (<=8), inline the copy. For larger sizes, call runtime.
        """
        if self.ctx is None:
            return

        if size <= 0:
            return

        # For small records, inline the copy (word at a time)
        if size <= 8:
            offset = 0
            while offset < size:
                if size - offset >= 2:
                    # Copy word (2 bytes)
                    tmp = self.builder.new_vreg(IRType.WORD, f"_copy_tmp_{offset}")
                    src_mem = MemoryLocation(base=src, offset=offset, ir_type=IRType.WORD)
                    dst_mem = MemoryLocation(base=dst, offset=offset, ir_type=IRType.WORD)
                    self.builder.load(tmp, src_mem, comment=f"copy {comment} word @{offset}")
                    self.builder.store(dst_mem, tmp)
                    offset += 2
                else:
                    # Copy byte
                    tmp = self.builder.new_vreg(IRType.BYTE, f"_copy_tmp_{offset}")
                    src_mem = MemoryLocation(base=src, offset=offset, ir_type=IRType.BYTE)
                    dst_mem = MemoryLocation(base=dst, offset=offset, ir_type=IRType.BYTE)
                    self.builder.load(tmp, src_mem, comment=f"copy {comment} byte @{offset}")
                    self.builder.store(dst_mem, tmp)
                    offset += 1
        else:
            # For larger copies, call runtime _memcpy(dst, src, size)
            self.builder.push(Immediate(size, IRType.WORD))
            self.builder.push(src)
            self.builder.push(dst)
            self.builder.call(Label("_memcpy"), comment=f"memcpy {comment}")
            # Clean up 3 arguments
            for _ in range(3):
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)

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
        end_label = self.builder.new_label("cond_end")
        else_label = self.builder.new_label("cond_else")

        # If there are elsif parts, the initial false branch goes to first elsif check
        # Otherwise it goes directly to else
        if expr.elsif_parts:
            first_elsif_label = self.builder.new_label("elsif_0")
            false_target = first_elsif_label
        else:
            first_elsif_label = None
            false_target = else_label

        # Evaluate condition and branch
        cond_val = self._lower_expr(expr.condition)
        self.builder.jz(cond_val, false_target, comment="if false, goto elsif/else")

        # Then branch
        then_val = self._lower_expr(expr.then_expr)
        self.builder.mov(result, then_val)
        self.builder.jmp(end_label)

        # Handle elsif parts
        current_else_target = else_label
        for i, (elsif_cond, elsif_expr) in enumerate(expr.elsif_parts):
            # Emit label for this elsif check
            if i == 0:
                self.builder.label(first_elsif_label)
            else:
                self.builder.label(current_else_target)

            # Determine where to jump if this elsif condition is false
            if i + 1 < len(expr.elsif_parts):
                current_else_target = self.builder.new_label(f"elsif_{i+1}")
            else:
                current_else_target = else_label

            elsif_cond_val = self._lower_expr(elsif_cond)
            self.builder.jz(elsif_cond_val, current_else_target, comment="if elsif false, goto next")

            elsif_val = self._lower_expr(elsif_expr)
            self.builder.mov(result, elsif_val)
            self.builder.jmp(end_label)

        # Else branch (or final else)
        self.builder.label(else_label)
        if expr.else_expr:
            else_val = self._lower_expr(expr.else_expr)
            self.builder.mov(result, else_val)
        else:
            # No else - result is undefined (shouldn't happen in valid Ada 2012)
            self.builder.mov(result, Immediate(0, IRType.WORD))

        # End label
        self.builder.label(end_label)

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
        self.builder.label(loop_start)
        self.builder.cmp(loop_var, high_val)
        self.builder.jg(loop_end, comment="exit if loop_var > high")

        # Loop body - evaluate predicate
        self.builder.label(loop_body)

        # Temporarily bind the loop variable in context for predicate evaluation
        # Save any existing local with the same name
        old_local = self.ctx.locals.get(loop_var_name.lower())

        # Register the loop variable as a local so the predicate can access it
        self.ctx.locals[loop_var_name.lower()] = LocalVariable(
            name=loop_var_name,
            vreg=loop_var,
            stack_offset=0,
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
            early_exit = self.builder.new_label("quant_early_exit")
            is_true = self.builder.new_vreg(IRType.WORD, "_is_true")
            self.builder.cmp_ne(is_true, pred_val, Immediate(0, IRType.WORD))
            self.builder.jnz(is_true, early_exit)  # predicate was true, continue
            self.builder.mov(result, Immediate(0, IRType.WORD))
            self.builder.jmp(loop_end)
            self.builder.label(early_exit)
        else:
            # for some: if predicate is true, set result to true and exit
            continue_loop = self.builder.new_label("quant_continue")
            self.builder.jz(pred_val, continue_loop)  # predicate was false, continue
            self.builder.mov(result, Immediate(1, IRType.WORD))
            self.builder.jmp(loop_end)
            self.builder.label(continue_loop)

        # Increment loop variable and continue
        temp = self.builder.new_vreg(IRType.WORD, "_inc_temp")
        self.builder.add(temp, loop_var, Immediate(1, IRType.WORD))
        self.builder.mov(loop_var, temp)
        self.builder.jmp(loop_start)

        # Loop end
        self.builder.label(loop_end)

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

        # Get the target type and source type
        target_type = self._resolve_type(expr.type_mark)
        source_type = self._get_expr_type(expr.operand)

        # Check for Float64 conversions
        source_is_float64 = self._is_float64_type(source_type)
        target_is_float64 = self._is_float64_type(target_type)

        # Handle Float64 -> Integer conversion
        if source_is_float64 and target_type and getattr(target_type, 'kind', None) == TypeKind.INTEGER:
            # Get pointer to Float64 operand
            float_ptr = self._lower_float64_operand(expr.operand)
            # Call _f64_ftoi to convert float to integer
            result = self.builder.new_vreg(IRType.WORD, "_f64_to_int")
            self.builder.push(float_ptr)
            self.builder.call(Label("_f64_ftoi"))
            # Result is in HL - save it BEFORE cleaning up stack
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="result from _f64_ftoi"
            ))
            # Clean up stack (1 word = 2 bytes) without using POP (which would trash HL)
            # Format: ADD _SP, Immediate (dst=_SP, src1=immediate to add)
            self.builder.emit(IRInstr(
                OpCode.ADD,
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                Immediate(2, IRType.WORD),
                comment="clean up 1 argument (adjust SP)"
            ))
            return result

        # Handle Integer -> Float64 conversion
        if target_is_float64 and source_type and getattr(source_type, 'kind', None) in (TypeKind.INTEGER, TypeKind.MODULAR):
            # Evaluate the integer operand
            int_val = self._lower_expr(expr.operand)
            # Allocate space for Float64 result
            result_ptr = self.builder.new_vreg(IRType.PTR, "_int_to_f64")
            self.builder.emit(IRInstr(
                OpCode.SUB,
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                Immediate(8, IRType.WORD),
                comment="allocate 8 bytes for float64 result"
            ))
            self.builder.emit(IRInstr(
                OpCode.MOV, result_ptr,
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
                comment="result_ptr = SP"
            ))
            # Push result pointer and integer value
            self.builder.push(result_ptr)
            self.builder.push(int_val)
            self.builder.call(Label("_f64_itof"))
            # Pop arguments (2 words = 4 bytes)
            discard = self.builder.new_vreg(IRType.WORD, "_pop_discard")
            self.builder.pop(discard)
            self.builder.pop(discard)
            return result_ptr

        # Evaluate the operand for non-float64 cases
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
                                self.builder.label(next_choice)
                                continue
                        # Type without bounds - always matches
                        self.builder.jmp(match_label)
                        self.builder.label(next_choice)
                        continue

                # Check for T'Class attribute (class-wide membership test)
                if isinstance(choice.expr, AttributeReference) and choice.expr.attribute.lower() == "class":
                    # X in T'Class - check if X's tag is in T's class hierarchy
                    base_type_name = self._get_type_name_from_expr(choice.expr.prefix)
                    if base_type_name:
                        type_sym = self.symbols.lookup(base_type_name)
                        if type_sym and isinstance(type_sym.ada_type, RecordType) and type_sym.ada_type.is_tagged:
                            # Get object's tag (vtable pointer at offset 0)
                            obj_tag = self.builder.new_vreg(IRType.PTR, "_obj_tag")
                            obj_mem = MemoryLocation(base=test_val, offset=0, ir_type=IRType.PTR)
                            self.builder.load(obj_tag, obj_mem, comment="get object's tag")

                            # Get target type's vtable address
                            vtable_name = f"_vtable_{type_sym.ada_type.name}"
                            vtable_addr = self.builder.new_vreg(IRType.PTR, "_target_vtable")
                            vtable_mem = MemoryLocation(is_global=True, symbol_name=vtable_name, ir_type=IRType.PTR)
                            self.builder.lea(vtable_addr, vtable_mem, comment="target type vtable")

                            # Push args for _class_membership: obj_tag in HL, target in DE
                            # Call returns 1 if in class, 0 if not
                            self.builder.push(vtable_addr, comment="target vtable")
                            self.builder.push(obj_tag, comment="object tag")
                            self.builder.call(Label("_class_membership"), comment="check class membership")

                            # Clean stack
                            temp1 = self.builder.new_vreg(IRType.WORD, "_discard1")
                            temp2 = self.builder.new_vreg(IRType.WORD, "_discard2")
                            self.builder.pop(temp1)
                            self.builder.pop(temp2)

                            # Capture result from HL
                            membership_result = self.builder.new_vreg(IRType.WORD, "_cm_result")
                            self.builder.emit(IRInstr(
                                OpCode.MOV, membership_result,
                                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                                comment="capture class membership result"
                            ))

                            # Branch based on result
                            self.builder.jnz(membership_result, match_label)
                            self.builder.label(next_choice)
                            continue

                # Single value test (equality)
                choice_val = self._lower_expr(choice.expr)
                cmp_result = self.builder.new_vreg(IRType.WORD, "_eq")
                self.builder.cmp_eq(cmp_result, test_val, choice_val)
                self.builder.jnz(cmp_result, match_label)  # X == choice

            elif isinstance(choice, OthersChoice):
                # Others always matches
                self.builder.jmp(match_label)

            self.builder.label(next_choice)

        # No match found - result stays 0
        self.builder.jmp(end_label)

        # Match found
        self.builder.label(match_label)
        self.builder.mov(result, Immediate(1, IRType.WORD))

        self.builder.label(end_label)

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
                    self.builder.label(next_choice)
                elif isinstance(choice, ExprChoice):
                    # Single value choice
                    choice_val = self._lower_expr(choice.expr)
                    cmp_result = self.builder.new_vreg(IRType.WORD, "_eq")
                    self.builder.cmp_eq(cmp_result, selector, choice_val)
                    self.builder.jnz(cmp_result, match_label)

            # No match in this alternative, try next
            self.builder.jmp(next_alt)

            # Match found - evaluate the result expression
            self.builder.label(match_label)
            alt_result = self._lower_expr(alt.result_expr)
            self.builder.mov(result, alt_result)
            self.builder.jmp(end_label)

            self.builder.label(next_alt)

        # Default case (shouldn't reach here in valid Ada)
        self.builder.mov(result, Immediate(0, IRType.WORD))

        self.builder.label(end_label)
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

    # =========================================================================
    # Float64 (IEEE 754 Double Precision) Support
    # =========================================================================

    def _float64_to_bytes(self, value: float) -> bytes:
        """Convert Python float to 8-byte IEEE 754 double (little-endian)."""
        import struct
        return struct.pack('<d', value)

    def _lower_float64_literal(self, value: float):
        """Lower a Float64 literal.

        Allocates 8 bytes in the data segment and returns a pointer to it.
        Float64 values are too large for registers and must be in memory.
        """
        if self.ctx is None:
            return Immediate(0, IRType.PTR)

        # Convert to IEEE 754 bytes
        float_bytes = self._float64_to_bytes(value)

        # Create a unique label for this float constant
        label = self._new_label("_f64_const")

        # Store the constant in the module's data segment
        self.builder.module.add_float64(label, float_bytes)

        # Return a pointer to the constant
        result = self.builder.new_vreg(IRType.PTR, "_f64_ptr")
        self.builder.emit(IRInstr(
            OpCode.LEA, result,
            Label(label),
            comment=f"float64 constant {value}"
        ))
        return result

    def _is_float64_type(self, ada_type) -> bool:
        """Check if a type is Float64 (Long_Float or Long_Long_Float)."""
        if ada_type is None:
            return False
        from uada80.type_system import FloatType
        if isinstance(ada_type, FloatType):
            # Float64 has 64 bits (8 bytes)
            return ada_type.size_bits == 64 or ada_type.name in ('Long_Float', 'Long_Long_Float')
        return False

    def _lower_float64_operand(self, expr):
        """Lower an expression to a Float64 pointer.

        For literals, creates a constant in the data segment.
        For variables, returns the address of the variable.
        """
        if self.ctx is None:
            return Immediate(0, IRType.PTR)

        if isinstance(expr, RealLiteral):
            return self._lower_float64_literal(expr.value)

        if isinstance(expr, Identifier):
            # Get the address of the variable
            name = expr.name.lower()
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                result = self.builder.new_vreg(IRType.PTR, f"_addr_{name}")
                # Calculate address: IX - (locals_size - stack_offset)
                frame_offset = -(self.ctx.locals_size - local.stack_offset)
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                    comment=f"address of {name}"
                ))
                return result
            # Could be a global
            sym = self.symbols.lookup(name)
            if sym:
                result = self.builder.new_vreg(IRType.PTR, f"_addr_{name}")
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_{name}"),
                    comment=f"address of global {name}"
                ))
                return result

        # For other expressions, evaluate and get address
        # This is a simplified approach - complex expressions may need temp storage
        val = self._lower_expr(expr)
        if isinstance(val, VReg) and val.ir_type == IRType.PTR:
            return val

        # Store result in temp and return address
        temp_ptr = self.builder.new_vreg(IRType.PTR, "_f64_temp_ptr")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for float64 temp"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, temp_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="temp_ptr = SP"
        ))
        # Copy value to temp (this handles the case where val is already a float64 ptr)
        self.builder.push(val)
        self.builder.push(temp_ptr)
        self.builder.call(Label("_f64_copy"))
        discard = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(discard)
        self.builder.pop(discard)
        return temp_ptr

    def _lower_float64_binary(self, op: BinaryOp, left_ptr, right_ptr, result_type):
        """Lower a Float64 binary operation.

        Calls the float64 runtime library functions.
        Float64 calling convention:
          - Push pointer to result (6 bytes from SP after call)
          - Push pointer to first operand (4 bytes from SP)
          - Push pointer to second operand (2 bytes from SP)
          - Call function
          - Pop arguments (6 bytes)
        """
        if self.ctx is None:
            return Immediate(0, IRType.PTR)

        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for float64 result"
        ))
        # Result pointer is now SP
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push pointers for function call
        # Runtime expects: IX+4=a_ptr, IX+6=b_ptr, IX+8=result_ptr
        # So push order: result first (ends at highest addr), then b, then a
        self.builder.push(result_ptr)  # result location (IX+8)
        self.builder.push(right_ptr)   # second operand (IX+6)
        self.builder.push(left_ptr)    # first operand (IX+4)

        # Select the right function
        if op == BinaryOp.ADD:
            func = "_f64_add"
        elif op == BinaryOp.SUB:
            func = "_f64_sub"
        elif op == BinaryOp.MUL:
            func = "_f64_mul"
        elif op == BinaryOp.DIV:
            func = "_f64_div"
        elif op == BinaryOp.REM:
            func = "_f64_rem"
        elif op == BinaryOp.MOD:
            func = "_f64_mod"
        else:
            # Comparison ops handled differently
            func = "_f64_cmp"

        self.builder.call(Label(func))

        # Clean up pushed arguments (3 pointers = 6 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(6, IRType.WORD),
            comment="pop 3 float64 pointers"
        ))

        # Result is in the allocated stack space, pointed to by result_ptr
        return result_ptr

    def _lower_float64_comparison(self, op: BinaryOp, left_ptr, right_ptr):
        """Lower a Float64 comparison operation.

        Calls _f64_cmp which returns -1 (a<b), 0 (a==b), or 1 (a>b) in A.
        Then generates code to check the result based on the comparison op.
        """
        # Push pointers for function call
        # Runtime expects: IX+4=a_ptr, IX+6=b_ptr
        # So push order: b first (ends at higher addr), then a
        self.builder.push(right_ptr)   # second operand (IX+6)
        self.builder.push(left_ptr)    # first operand (IX+4)

        # Always call _f64_cmp - the wrapper functions have a bug with stack offsets
        self.builder.call(Label("_f64_cmp"))

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop 2 float64 pointers"
        ))

        # A register now contains: -1 (0xFF) if a<b, 0 if a==b, 1 if a>b
        # Generate comparison based on op
        result = self.builder.new_vreg(IRType.WORD, "_f64cmp")

        if op == BinaryOp.EQ:
            # A == 0 means equal
            self.builder.emit(IRInstr(
                OpCode.INLINE_ASM, None, None, None,
                comment="OR A\nJR NZ, $+5\nLD HL, 1\nJR $+3\nLD HL, 0"
            ))
        elif op == BinaryOp.NE:
            # A != 0 means not equal
            self.builder.emit(IRInstr(
                OpCode.INLINE_ASM, None, None, None,
                comment="OR A\nJR Z, $+5\nLD HL, 1\nJR $+3\nLD HL, 0"
            ))
        elif op == BinaryOp.LT:
            # A == -1 (0xFF) means less than
            self.builder.emit(IRInstr(
                OpCode.INLINE_ASM, None, None, None,
                comment="CP 0FFH\nJR NZ, $+5\nLD HL, 1\nJR $+3\nLD HL, 0"
            ))
        elif op == BinaryOp.GT:
            # A == 1 means greater than
            self.builder.emit(IRInstr(
                OpCode.INLINE_ASM, None, None, None,
                comment="CP 1\nJR NZ, $+5\nLD HL, 1\nJR $+3\nLD HL, 0"
            ))
        elif op == BinaryOp.LE:
            # A != 1 means less or equal (A == -1 or A == 0)
            self.builder.emit(IRInstr(
                OpCode.INLINE_ASM, None, None, None,
                comment="CP 1\nJR Z, $+5\nLD HL, 1\nJR $+3\nLD HL, 0"
            ))
        elif op == BinaryOp.GE:
            # A != -1 means greater or equal (A == 0 or A == 1)
            self.builder.emit(IRInstr(
                OpCode.INLINE_ASM, None, None, None,
                comment="CP 0FFH\nJR Z, $+5\nLD HL, 1\nJR $+3\nLD HL, 0"
            ))

        # Capture from HL
        self.builder.emit(IRInstr(
            OpCode.MOV, result,
            MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
            comment="capture float64 comparison result"
        ))

        return result

    def _lower_float64_unary(self, op: UnaryOp, operand_ptr, operand_type):
        """Lower a Float64 unary operation (negation, abs, plus).

        For negation: calls _f64_neg which flips the sign bit.
        For abs: calls _f64_abs which clears the sign bit.
        For plus: just copies the value (no-op).
        """
        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.WORD, "_f64unary")

        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for float64 unary result"
        ))

        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            comment="result_ptr = SP"
        ))

        if op == UnaryOp.PLUS:
            # Plus is just a copy - call _f64_copy
            # Runtime expects: IX+4=dest_ptr, IX+6=src_ptr
            self.builder.push(operand_ptr)   # src (IX+6)
            self.builder.push(result_ptr)    # dest (IX+4)
            self.builder.call(Label("_f64_copy"))
        elif op == UnaryOp.MINUS:
            # Negation - call _f64_neg
            # Runtime expects: IX+4=dest_ptr, IX+6=src_ptr
            self.builder.push(operand_ptr)   # src (IX+6)
            self.builder.push(result_ptr)    # dest (IX+4)
            self.builder.call(Label("_f64_neg"))
        elif op == UnaryOp.ABS:
            # Absolute value - call _f64_abs
            # Runtime expects: IX+4=dest_ptr, IX+6=src_ptr
            self.builder.push(operand_ptr)   # src (IX+6)
            self.builder.push(result_ptr)    # dest (IX+4)
            self.builder.call(Label("_f64_abs"))

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop 2 float64 pointers"
        ))

        # Result is in the allocated stack space, pointed to by result_ptr
        return result_ptr

    def _lower_float64_math_attr(self, func_name: str, operand_ptr, operand_type):
        """Lower a Float64 math attribute (floor, ceil, trunc).

        These functions take (dest_ptr, src_ptr) and write the result to dest.
        The result is a Float64 (not an integer).
        """
        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.WORD, "_f64math")

        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment=f"allocate 8 bytes for {func_name} result"
        ))

        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            comment="result_ptr = SP"
        ))

        # Runtime expects: IX+4=src_ptr, IX+6=dest_ptr
        # Push order: first pushed ends at higher offset (IX+6)
        self.builder.push(result_ptr)    # dest (IX+6) - pushed first
        self.builder.push(operand_ptr)   # src (IX+4) - pushed last
        self.builder.call(Label(func_name))

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop 2 float64 pointers"
        ))

        # Result is in the allocated stack space, pointed to by result_ptr
        return result_ptr

    def _lower_float64_exp(self, base_ptr, exponent, operand_type):
        """Lower Float64 ** Integer exponentiation.

        Calls _f64_exp_int(result_ptr, exponent, base_ptr).
        Stack layout after call:
          - IX+4 = base_ptr (Float64 pointer)
          - IX+6 = exponent (Integer)
          - IX+8 = result_ptr (Float64 pointer)
        """
        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_exp_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for float64 exp result"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push arguments: result_ptr (IX+8), exponent (IX+6), base_ptr (IX+4)
        self.builder.push(result_ptr)  # result location (IX+8)
        self.builder.push(exponent)    # integer exponent (IX+6)
        self.builder.push(base_ptr)    # Float64 base (IX+4)

        self.builder.call(Label("_f64_exp_int"))

        # Clean up pushed arguments (2 pointers + 1 integer = 6 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(6, IRType.WORD),
            comment="pop float64 exp args"
        ))

        return result_ptr

    def _lower_float64_sqrt(self, operand_expr):
        """Lower Float64 sqrt function call.

        Calls _f64_sqrt(result_ptr, src_ptr).
        Stack layout after call:
          - IX+4 = src_ptr (Float64 pointer)
          - IX+6 = result_ptr (Float64 pointer)
        """
        # Get the operand as a Float64 pointer
        operand_ptr = self._lower_float64_operand(operand_expr)

        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_sqrt_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for sqrt result"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push arguments: result_ptr (IX+6), src_ptr (IX+4)
        self.builder.push(result_ptr)   # result location (IX+6)
        self.builder.push(operand_ptr)  # source operand (IX+4)

        self.builder.call(Label("_f64_sqrt"))

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop sqrt args"
        ))

        return result_ptr

    def _lower_float64_sin(self, operand_expr):
        """sin(x) = x - x³/6 + x⁵/120 - x⁷/5040 + ...

        Taylor series implementation, inlined for correct operation.
        """
        x_ptr = self._lower_float64_operand(operand_expr)
        return self._sin_from_ptr(x_ptr)

    def _sin_from_ptr(self, x_ptr):
        """Internal helper: compute sin from an already-evaluated pointer."""
        # Generate local constants (more reliable than runtime constants)
        const_6 = self._lower_float64_literal(6.0)
        const_120 = self._lower_float64_literal(120.0)
        const_5040 = self._lower_float64_literal(5040.0)

        # x² = x * x
        x2 = self._f64_alloc_temp("x2")
        self._f64_call_binary("_f64_mul", x2, x_ptr, x_ptr)

        # result = x (first term)
        result = self._f64_alloc_temp("sin_result")
        self._f64_call_copy(result, x_ptr)

        # x³ = x * x²
        xn = self._f64_alloc_temp("xn")
        self._f64_call_binary("_f64_mul", xn, x_ptr, x2)

        # term = x³ / 6
        term = self._f64_alloc_temp("term")
        self._f64_call_binary("_f64_div", term, xn, const_6)

        # result = result - term (subtract x³/6)
        self._f64_call_binary("_f64_sub", result, result, term)

        # x⁵ = x³ * x²
        self._f64_call_binary("_f64_mul", xn, xn, x2)

        # term = x⁵ / 120
        self._f64_call_binary("_f64_div", term, xn, const_120)

        # result = result + term (add x⁵/120)
        self._f64_call_binary("_f64_add", result, result, term)

        # x⁷ = x⁵ * x²
        self._f64_call_binary("_f64_mul", xn, xn, x2)

        # term = x⁷ / 5040
        self._f64_call_binary("_f64_div", term, xn, const_5040)

        # result = result - term (subtract x⁷/5040)
        self._f64_call_binary("_f64_sub", result, result, term)

        return result

    def _lower_float64_cos(self, operand_expr):
        """cos(x) = 1 - x²/2 + x⁴/24 - x⁶/720 + ...

        Taylor series implementation, inlined for correct operation.
        """
        x_ptr = self._lower_float64_operand(operand_expr)
        return self._cos_from_ptr(x_ptr)

    def _cos_from_ptr(self, x_ptr):
        """Internal helper: compute cos from an already-evaluated pointer."""
        # Generate local constants (more reliable than runtime constants)
        const_1 = self._lower_float64_literal(1.0)
        const_2 = self._lower_float64_literal(2.0)
        const_24 = self._lower_float64_literal(24.0)
        const_720 = self._lower_float64_literal(720.0)

        # x² = x * x
        x2 = self._f64_alloc_temp("x2")
        self._f64_call_binary("_f64_mul", x2, x_ptr, x_ptr)

        # result = 1.0 (first term)
        result = self._f64_alloc_temp("cos_result")
        self._f64_call_copy(result, const_1)

        # xn = x² (current power, starting at x²)
        xn = self._f64_alloc_temp("xn")
        self._f64_call_copy(xn, x2)

        # term = x² / 2
        term = self._f64_alloc_temp("term")
        self._f64_call_binary("_f64_div", term, xn, const_2)

        # result = result - term (subtract x²/2)
        self._f64_call_binary("_f64_sub", result, result, term)

        # x⁴ = x² * x²
        self._f64_call_binary("_f64_mul", xn, xn, x2)

        # term = x⁴ / 24
        self._f64_call_binary("_f64_div", term, xn, const_24)

        # result = result + term (add x⁴/24)
        self._f64_call_binary("_f64_add", result, result, term)

        # x⁶ = x⁴ * x²
        self._f64_call_binary("_f64_mul", xn, xn, x2)

        # term = x⁶ / 720
        self._f64_call_binary("_f64_div", term, xn, const_720)

        # result = result - term (subtract x⁶/720)
        self._f64_call_binary("_f64_sub", result, result, term)

        return result

    def _lower_float64_tan(self, operand_expr):
        """tan(x) = sin(x) / cos(x)

        Uses inlined sin/cos Taylor series for correct operation.
        Evaluates operand once and uses same value for both sin and cos.
        """
        # Evaluate operand once
        x_ptr = self._lower_float64_operand(operand_expr)

        # Save x to a temporary so stack allocations in sin/cos don't affect it
        x_saved = self._f64_alloc_temp("tan_x")
        self._f64_call_copy(x_saved, x_ptr)

        # Compute sin(x) and cos(x) using the saved value
        sin_result = self._sin_from_ptr(x_saved)
        cos_result = self._cos_from_ptr(x_saved)

        # result = sin(x) / cos(x)
        result = self._f64_alloc_temp("tan_result")
        self._f64_call_binary("_f64_div", result, sin_result, cos_result)

        return result

    def _lower_float64_cot(self, operand_expr):
        """cot(x) = 1.0 / tan(x)

        Matches Ada implementation in adalib/ada-numerics-elementary_functions.adb
        """
        # First compute tan(x)
        tan_result = self._lower_float64_tan(operand_expr)

        # result = 1.0 / tan(x)
        result = self._f64_alloc_temp("cot_result")
        self._f64_call_binary("_f64_div", result, Label("_const_one_f64"), tan_result)

        return result

    def _lower_float64_atan(self, operand_expr):
        """Lower Float64 arctan function call.

        Calls _f64_atan(result_ptr, src_ptr).
        Stack layout after call:
          - IX+4 = src_ptr (Float64 pointer)
          - IX+6 = result_ptr (Float64 pointer)
        """
        # Get the operand as a Float64 pointer
        operand_ptr = self._lower_float64_operand(operand_expr)

        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_atan_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for atan result"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push arguments: result_ptr (IX+6), src_ptr (IX+4)
        self.builder.push(result_ptr)   # result location (IX+6)
        self.builder.push(operand_ptr)  # source operand (IX+4)

        self.builder.call(Label("_f64_atan"))

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop atan args"
        ))

        return result_ptr

    def _lower_float64_asin(self, operand_expr):
        """Lower Float64 arcsine function call.

        Calls _f64_asin(result_ptr, src_ptr).
        Stack layout after call:
          - IX+4 = src_ptr (Float64 pointer)
          - IX+6 = result_ptr (Float64 pointer)
        """
        # Get the operand as a Float64 pointer
        operand_ptr = self._lower_float64_operand(operand_expr)

        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_asin_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for asin result"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push arguments: result_ptr (IX+6), src_ptr (IX+4)
        self.builder.push(result_ptr)   # result location (IX+6)
        self.builder.push(operand_ptr)  # source operand (IX+4)

        self.builder.call(Label("_f64_asin"))

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop asin args"
        ))

        return result_ptr

    def _lower_float64_acos(self, operand_expr):
        """Lower Float64 arccosine function call.

        Calls _f64_acos(result_ptr, src_ptr).
        Stack layout after call:
          - IX+4 = src_ptr (Float64 pointer)
          - IX+6 = result_ptr (Float64 pointer)
        """
        # Get the operand as a Float64 pointer
        operand_ptr = self._lower_float64_operand(operand_expr)

        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_acos_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for acos result"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push arguments: result_ptr (IX+6), src_ptr (IX+4)
        self.builder.push(result_ptr)   # result location (IX+6)
        self.builder.push(operand_ptr)  # source operand (IX+4)

        self.builder.call(Label("_f64_acos"))

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop acos args"
        ))

        return result_ptr

    def _lower_float64_atan2(self, y_expr, x_expr):
        """Lower Float64 two-argument arctangent function call.

        Calls _f64_atan2(result_ptr, x_ptr, y_ptr).
        Stack layout after call:
          - IX+4 = y_ptr (Float64 pointer)
          - IX+6 = x_ptr (Float64 pointer)
          - IX+8 = result_ptr (Float64 pointer)
        """
        # Get the operands as Float64 pointers
        y_ptr = self._lower_float64_operand(y_expr)
        x_ptr = self._lower_float64_operand(x_expr)

        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_atan2_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for atan2 result"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push arguments: result_ptr (IX+8), x_ptr (IX+6), y_ptr (IX+4)
        self.builder.push(result_ptr)   # result location (IX+8)
        self.builder.push(x_ptr)        # x operand (IX+6)
        self.builder.push(y_ptr)        # y operand (IX+4)

        self.builder.call(Label("_f64_at2"))

        # Clean up pushed arguments (3 pointers = 6 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(6, IRType.WORD),
            comment="pop atan2 args"
        ))

        return result_ptr

    # =========================================================================
    # Float64 helper functions for calling primitives with pointer arguments
    # =========================================================================

    def _f64_alloc_temp(self, name: str):
        """Allocate 8 bytes on stack for a Float64 temporary, return pointer."""
        temp_ptr = self.builder.new_vreg(IRType.PTR, name)
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment=f"alloc {name}"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, temp_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment=f"{name} = SP"
        ))
        return temp_ptr

    def _f64_call_unary(self, func_name: str, result_ptr, src_ptr):
        """Call unary Float64 function: result = func(src)."""
        # Register runtime dependencies
        if self.builder.module:
            self.builder.module.need_runtime(func_name)
            if isinstance(src_ptr, Label):
                self.builder.module.need_runtime(src_ptr.name)
        self.builder.push(result_ptr)
        self.builder.push(src_ptr)
        self.builder.call(Label(func_name))
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment=f"pop {func_name} args"
        ))

    def _f64_call_copy(self, dest_ptr, src_ptr):
        """Call _f64_copy with correct argument order: dest = copy of src.

        Note: _f64_copy has different calling convention than other unary functions:
        - dest_ptr at IX+4 (pushed last)
        - src_ptr at IX+6 (pushed first)
        """
        if self.builder.module:
            self.builder.module.need_runtime("_f64_copy")
            if isinstance(src_ptr, Label):
                self.builder.module.need_runtime(src_ptr.name)
        self.builder.push(src_ptr)   # pushed first -> IX+6
        self.builder.push(dest_ptr)  # pushed last -> IX+4
        self.builder.call(Label("_f64_copy"))
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop _f64_copy args"
        ))

    def _f64_call_binary(self, func_name: str, result_ptr, left_ptr, right_ptr):
        """Call binary Float64 function: result = func(left, right)."""
        # Register runtime dependencies
        if self.builder.module:
            self.builder.module.need_runtime(func_name)
            if isinstance(left_ptr, Label):
                self.builder.module.need_runtime(left_ptr.name)
            if isinstance(right_ptr, Label):
                self.builder.module.need_runtime(right_ptr.name)
        self.builder.push(result_ptr)
        self.builder.push(right_ptr)
        self.builder.push(left_ptr)
        self.builder.call(Label(func_name))
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(6, IRType.WORD),
            comment=f"pop {func_name} args"
        ))

    # =========================================================================
    # Composite Float64 functions - inlined formulas calling primitives
    # These match the Ada implementations in adalib/ada-numerics-elementary_functions.adb
    # =========================================================================

    def _lower_float64_sinh(self, operand_expr):
        """sinh(x) = (E_X - 1.0/E_X) / 2.0  where E_X = exp(x)

        Matches Ada implementation in adalib/ada-numerics-elementary_functions.adb
        """
        x_ptr = self._lower_float64_operand(operand_expr)

        # Generate local constants
        const_1 = self._lower_float64_literal(1.0)
        const_2 = self._lower_float64_literal(2.0)

        # E_X = exp(x)
        exp_x = self._f64_alloc_temp("exp_x")
        self._f64_call_unary("_f64_e2x", exp_x, x_ptr)

        # inv_exp_x = 1.0 / E_X
        inv_exp_x = self._f64_alloc_temp("inv_exp_x")
        self._f64_call_binary("_f64_div", inv_exp_x, const_1, exp_x)

        # diff = E_X - 1.0/E_X
        diff = self._f64_alloc_temp("diff")
        self._f64_call_binary("_f64_sub", diff, exp_x, inv_exp_x)

        # result = diff / 2.0
        result = self._f64_alloc_temp("sinh_result")
        self._f64_call_binary("_f64_div", result, diff, const_2)

        return result

    def _lower_float64_cosh(self, operand_expr):
        """cosh(x) = (E_X + 1.0/E_X) / 2.0  where E_X = exp(x)

        Matches Ada implementation in adalib/ada-numerics-elementary_functions.adb
        """
        x_ptr = self._lower_float64_operand(operand_expr)

        # Generate local constants
        const_1 = self._lower_float64_literal(1.0)
        const_2 = self._lower_float64_literal(2.0)

        # E_X = exp(x)
        exp_x = self._f64_alloc_temp("exp_x")
        self._f64_call_unary("_f64_e2x", exp_x, x_ptr)

        # inv_exp_x = 1.0 / E_X
        inv_exp_x = self._f64_alloc_temp("inv_exp_x")
        self._f64_call_binary("_f64_div", inv_exp_x, const_1, exp_x)

        # sum = E_X + 1.0/E_X
        sum_val = self._f64_alloc_temp("sum")
        self._f64_call_binary("_f64_add", sum_val, exp_x, inv_exp_x)

        # result = sum / 2.0
        result = self._f64_alloc_temp("cosh_result")
        self._f64_call_binary("_f64_div", result, sum_val, const_2)

        return result

    def _lower_float64_tanh(self, operand_expr):
        """tanh(x) = (E_2X - 1.0) / (E_2X + 1.0)  where E_2X = exp(2*x)

        Matches Ada implementation in adalib/ada-numerics-elementary_functions.adb
        """
        x_ptr = self._lower_float64_operand(operand_expr)

        # Generate local constants
        const_1 = self._lower_float64_literal(1.0)
        const_2 = self._lower_float64_literal(2.0)

        # two_x = 2.0 * x
        two_x = self._f64_alloc_temp("two_x")
        self._f64_call_binary("_f64_mul", two_x, const_2, x_ptr)

        # E_2X = exp(2*x)
        exp_2x = self._f64_alloc_temp("exp_2x")
        self._f64_call_unary("_f64_e2x", exp_2x, two_x)

        # num = E_2X - 1.0
        num = self._f64_alloc_temp("num")
        self._f64_call_binary("_f64_sub", num, exp_2x, const_1)

        # den = E_2X + 1.0
        den = self._f64_alloc_temp("den")
        self._f64_call_binary("_f64_add", den, exp_2x, const_1)

        # result = num / den
        result = self._f64_alloc_temp("tanh_result")
        self._f64_call_binary("_f64_div", result, num, den)

        return result

    def _lower_float64_coth(self, operand_expr):
        """coth(x) = 1.0 / tanh(x)

        Matches Ada implementation in adalib/ada-numerics-elementary_functions.adb
        """
        # First compute tanh(x)
        tanh_result = self._lower_float64_tanh(operand_expr)

        # Generate local constant
        const_1 = self._lower_float64_literal(1.0)

        # result = 1.0 / tanh(x)
        result = self._f64_alloc_temp("coth_result")
        self._f64_call_binary("_f64_div", result, const_1, tanh_result)

        return result

    def _lower_float64_arcsinh(self, operand_expr):
        """arcsinh(x) = log(x + sqrt(x*x + 1))

        Inlined implementation using primitive Float64 operations.
        See Ada reference: adalib/ada-numerics-elementary_functions.adb
        """
        x_ptr = self._lower_float64_operand(operand_expr)

        # x_sq = x * x
        x_sq = self._f64_alloc_temp("x_sq")
        self._f64_call_binary("_f64_mul", x_sq, x_ptr, x_ptr)

        # x_sq_plus_1 = x*x + 1
        x_sq_plus_1 = self._f64_alloc_temp("x_sq_plus_1")
        self._f64_call_binary("_f64_add", x_sq_plus_1, x_sq, Label("_const_one_f64"))

        # sqrt_val = sqrt(x*x + 1)
        sqrt_val = self._f64_alloc_temp("sqrt_val")
        self._f64_call_unary("_f64_sqrt", sqrt_val, x_sq_plus_1)

        # sum = x + sqrt(x*x + 1)
        sum_val = self._f64_alloc_temp("sum")
        self._f64_call_binary("_f64_add", sum_val, x_ptr, sqrt_val)

        # result = log(x + sqrt(x*x + 1))
        result = self._f64_alloc_temp("arcsinh_result")
        self._f64_call_unary("_f64_log", result, sum_val)

        return result

    def _lower_float64_arccosh(self, operand_expr):
        """arccosh(x) = log(x + sqrt(x*x - 1))

        Inlined implementation using primitive Float64 operations.
        See Ada reference: adalib/ada-numerics-elementary_functions.adb
        """
        x_ptr = self._lower_float64_operand(operand_expr)

        # x_sq = x * x
        x_sq = self._f64_alloc_temp("x_sq")
        self._f64_call_binary("_f64_mul", x_sq, x_ptr, x_ptr)

        # x_sq_minus_1 = x*x - 1
        x_sq_minus_1 = self._f64_alloc_temp("x_sq_minus_1")
        self._f64_call_binary("_f64_sub", x_sq_minus_1, x_sq, Label("_const_one_f64"))

        # sqrt_val = sqrt(x*x - 1)
        sqrt_val = self._f64_alloc_temp("sqrt_val")
        self._f64_call_unary("_f64_sqrt", sqrt_val, x_sq_minus_1)

        # sum = x + sqrt(x*x - 1)
        sum_val = self._f64_alloc_temp("sum")
        self._f64_call_binary("_f64_add", sum_val, x_ptr, sqrt_val)

        # result = log(x + sqrt(x*x - 1))
        result = self._f64_alloc_temp("arccosh_result")
        self._f64_call_unary("_f64_log", result, sum_val)

        return result

    def _lower_float64_arctanh(self, operand_expr):
        """arctanh(x) = 0.5 * log((1 + x) / (1 - x))

        Inlined implementation using primitive Float64 operations.
        See Ada reference: adalib/ada-numerics-elementary_functions.adb
        """
        x_ptr = self._lower_float64_operand(operand_expr)

        # Generate local constants
        const_1 = self._lower_float64_literal(1.0)
        const_2 = self._lower_float64_literal(2.0)

        # one_plus_x = 1 + x
        one_plus_x = self._f64_alloc_temp("one_plus_x")
        self._f64_call_binary("_f64_add", one_plus_x, const_1, x_ptr)

        # one_minus_x = 1 - x
        one_minus_x = self._f64_alloc_temp("one_minus_x")
        self._f64_call_binary("_f64_sub", one_minus_x, const_1, x_ptr)

        # ratio = (1 + x) / (1 - x)
        ratio = self._f64_alloc_temp("ratio")
        self._f64_call_binary("_f64_div", ratio, one_plus_x, one_minus_x)

        # log_ratio = log((1 + x) / (1 - x))
        log_ratio = self._f64_alloc_temp("log_ratio")
        self._f64_call_unary("_f64_log", log_ratio, ratio)

        # result = log_ratio / 2.0 (equivalent to 0.5 * log_ratio)
        result = self._f64_alloc_temp("arctanh_result")
        self._f64_call_binary("_f64_div", result, log_ratio, const_2)

        return result

    def _lower_float64_arccoth(self, operand_expr):
        """arccoth(x) = 0.5 * log((x + 1) / (x - 1))

        Matches Ada implementation in adalib/ada-numerics-elementary_functions.adb
        """
        x_ptr = self._lower_float64_operand(operand_expr)

        # Generate local constants
        const_1 = self._lower_float64_literal(1.0)
        const_2 = self._lower_float64_literal(2.0)

        # x_plus_1 = x + 1
        x_plus_1 = self._f64_alloc_temp("x_plus_1")
        self._f64_call_binary("_f64_add", x_plus_1, x_ptr, const_1)

        # x_minus_1 = x - 1
        x_minus_1 = self._f64_alloc_temp("x_minus_1")
        self._f64_call_binary("_f64_sub", x_minus_1, x_ptr, const_1)

        # ratio = (x + 1) / (x - 1)
        ratio = self._f64_alloc_temp("ratio")
        self._f64_call_binary("_f64_div", ratio, x_plus_1, x_minus_1)

        # log_ratio = log((x + 1) / (x - 1))
        log_ratio = self._f64_alloc_temp("log_ratio")
        self._f64_call_unary("_f64_log", log_ratio, ratio)

        # result = log_ratio / 2.0 (equivalent to 0.5 * log_ratio)
        result = self._f64_alloc_temp("arccoth_result")
        self._f64_call_binary("_f64_div", result, log_ratio, const_2)

        return result

    def _lower_float64_exp_func(self, operand_expr):
        """Lower Float64 exp function call (Ada.Numerics.Elementary_Functions.Exp).

        Calls _f64_e2x(result_ptr, src_ptr).
        Note: Named _f64_e2x (8 chars) to avoid symbol collision with _f64_exp_int.
        Stack layout after call:
          - IX+4 = src_ptr (Float64 pointer)
          - IX+6 = result_ptr (Float64 pointer)
        """
        # Get the operand as a Float64 pointer
        operand_ptr = self._lower_float64_operand(operand_expr)

        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_exp_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for exp result"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push arguments: result_ptr (IX+6), src_ptr (IX+4)
        self.builder.push(result_ptr)   # result location (IX+6)
        self.builder.push(operand_ptr)  # source operand (IX+4)

        self.builder.call(Label("_f64_e2x"))  # Named _f64_e2x to avoid 8-char symbol collision with _f64_exp_int

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop exp args"
        ))

        return result_ptr

    def _lower_float64_log(self, operand_expr):
        """Lower Float64 log (natural logarithm) function call.

        Calls _f64_log(result_ptr, src_ptr).
        Stack layout after call:
          - IX+4 = src_ptr (Float64 pointer)
          - IX+6 = result_ptr (Float64 pointer)
        """
        # Get the operand as a Float64 pointer
        operand_ptr = self._lower_float64_operand(operand_expr)

        # Allocate 8 bytes on stack for result
        result_ptr = self.builder.new_vreg(IRType.PTR, "_f64_log_result")
        self.builder.emit(IRInstr(
            OpCode.SUB,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(8, IRType.WORD),
            comment="allocate 8 bytes for log result"
        ))
        self.builder.emit(IRInstr(
            OpCode.MOV, result_ptr,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.PTR),
            comment="result_ptr = SP"
        ))

        # Push arguments: result_ptr (IX+6), src_ptr (IX+4)
        self.builder.push(result_ptr)   # result location (IX+6)
        self.builder.push(operand_ptr)  # source operand (IX+4)

        self.builder.call(Label("_f64_log"))

        # Clean up pushed arguments (2 pointers = 4 bytes)
        self.builder.emit(IRInstr(
            OpCode.ADD,
            MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
            Immediate(4, IRType.WORD),
            comment="pop log args"
        ))

        return result_ptr

    def _lower_float64_log10(self, operand_expr):
        """log10(x) = log(x) / ln(10)"""
        # Get the operand as a Float64 pointer
        x_ptr = self._lower_float64_operand(operand_expr)

        # log(x)
        log_x = self._f64_alloc_temp("log_x")
        self._f64_call_unary("_f64_log", log_x, x_ptr)

        # log(x) / ln(10)
        result = self._f64_alloc_temp("log10_result")
        self._f64_call_binary("_f64_div", result, log_x, Label("_const_ln10"))

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
                        name=iter_var_name, vreg=iter_var, stack_offset=0, size=2, ada_type=None
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
                param_vreg = self.ctx.params[name]
                # For byref parameters (out/in out), dereference the pointer to get value
                if name in self.ctx.byref_params:
                    result = self.builder.new_vreg(IRType.WORD, f"_{name}_val")
                    self.builder.emit(IRInstr(
                        OpCode.LOAD, result,
                        MemoryLocation(offset=0, ir_type=IRType.WORD, base=param_vreg),
                        comment=f"deref byref param {name}"
                    ))
                    return result
                return param_vreg

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
                            # Create indexed memory location - compute address first
                            addr_reg = self.builder.new_vreg(IRType.PTR, "_elem_addr")
                            self.builder.add(addr_reg, agg_addr, offset_reg)
                            mem = MemoryLocation(base=addr_reg, offset=0, ir_type=IRType.WORD)
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
                        self.builder.label(loop_start)
                        self.builder.cmp(loop_var, high)
                        self.builder.jg(loop_end)
                        # Store value at index
                        offset_reg = self.builder.new_vreg(IRType.WORD, "_range_off")
                        self.builder.mul(offset_reg, loop_var, Immediate(2, IRType.WORD))
                        addr_reg = self.builder.new_vreg(IRType.PTR, "_elem_addr")
                        self.builder.add(addr_reg, agg_addr, offset_reg)
                        mem = MemoryLocation(base=addr_reg, offset=0, ir_type=IRType.WORD)
                        self.builder.store(mem, value)
                        # Increment and loop
                        self.builder.add(loop_var, loop_var, Immediate(1, IRType.WORD))
                        self.builder.jmp(loop_start)
                        self.builder.label(loop_end)
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
            # Map component names to offsets, sizes, and types
            field_info = {}
            for comp in target_type.components:
                field_info[comp.name.lower()] = {
                    'offset': comp.offset_bits // 8,
                    'size': (comp.size_bits + 7) // 8 if comp.size_bits else 2,
                    'type': comp.component_type  # Store the field's type
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
                                    offset = field_info[field_name]['offset']
                                    field_type = field_info[field_name]['type']
                                    # Handle nested aggregates for record/array fields
                                    if isinstance(comp_assoc.value, Aggregate) and isinstance(field_type, (RecordType, ArrayType)):
                                        # Compute field address and recursively init
                                        field_addr = self.builder.new_vreg(IRType.PTR, f"_{field_name}_addr")
                                        self.builder.add(field_addr, target_addr, Immediate(offset, IRType.WORD))
                                        self._lower_aggregate_to_target(comp_assoc.value, field_addr, field_type)
                                    else:
                                        value = self._lower_expr(comp_assoc.value)
                                        self._store_at_offset(target_addr, offset, value)
                                    assigned_fields.add(field_name)
                else:
                    # Positional assignment - use field order
                    if positional_index < len(ordered_fields):
                        field_name = ordered_fields[positional_index]
                        if field_name in field_info:
                            offset = field_info[field_name]['offset']
                            field_type = field_info[field_name]['type']
                            # Handle nested aggregates for record/array fields
                            if isinstance(comp_assoc.value, Aggregate) and isinstance(field_type, (RecordType, ArrayType)):
                                # Compute field address and recursively init
                                field_addr = self.builder.new_vreg(IRType.PTR, f"_{field_name}_addr")
                                self.builder.add(field_addr, target_addr, Immediate(offset, IRType.WORD))
                                self._lower_aggregate_to_target(comp_assoc.value, field_addr, field_type)
                            else:
                                value = self._lower_expr(comp_assoc.value)
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

            # Handle multi-dimensional arrays
            # For a 2D array like Matrix(1..3, 1..3), each row aggregate fills a slice
            is_multidim = target_type.bounds and len(target_type.bounds) > 1
            row_size = 0
            row_type = None
            if is_multidim:
                # Calculate row size: product of remaining dimensions * element_size
                row_count = 1
                for i in range(1, len(target_type.bounds)):
                    dim_low, dim_high = target_type.bounds[i]
                    row_count *= (dim_high - dim_low + 1)
                row_size = row_count * element_size
                # Create a 1D array type for each row
                row_type = ArrayType(
                    name="row",
                    component_type=target_type.component_type,
                    bounds=[target_type.bounds[1]] if len(target_type.bounds) > 1 else [(1, row_count)],
                    is_constrained=True,
                )

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
                    # For multi-dimensional arrays, use row_size for offset calculation
                    if is_multidim and row_type:
                        offset = (positional_index - lower_bound) * row_size
                        # Handle row aggregate for 2D arrays
                        if isinstance(comp_assoc.value, Aggregate):
                            # Compute row address and recursively init with row_type
                            row_addr = self.builder.new_vreg(IRType.PTR, f"_row_{positional_index}_addr")
                            self.builder.add(row_addr, target_addr, Immediate(offset, IRType.WORD))
                            self._lower_aggregate_to_target(comp_assoc.value, row_addr, row_type)
                        else:
                            value = self._lower_expr(comp_assoc.value)
                            self._store_at_offset(target_addr, offset, value)
                    else:
                        offset = (positional_index - lower_bound) * element_size
                        # Handle nested aggregates for arrays of records/arrays
                        if isinstance(comp_assoc.value, Aggregate) and target_type.component_type:
                            comp_type = target_type.component_type
                            if isinstance(comp_type, (RecordType, ArrayType)):
                                # Compute element address and recursively init
                                elem_addr = self.builder.new_vreg(IRType.PTR, f"_elem_{positional_index}_addr")
                                self.builder.add(elem_addr, target_addr, Immediate(offset, IRType.WORD))
                                self._lower_aggregate_to_target(comp_assoc.value, elem_addr, comp_type)
                            else:
                                value = self._lower_expr(comp_assoc.value)
                                self._store_at_offset(target_addr, offset, value)
                        else:
                            value = self._lower_expr(comp_assoc.value)
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
        array_low = 1  # Default array lower bound

        # Look up the array type to get element size
        if isinstance(expr.prefix, Identifier):
            name = expr.prefix.name.lower()
            sym = self.symbols.lookup(expr.prefix.name)

            # Check global symbol first
            if sym and sym.ada_type and isinstance(sym.ada_type, ArrayType):
                if sym.ada_type.component_type:
                    element_size = (sym.ada_type.component_type.size_bits + 7) // 8

                # Account for array's lower bound
                if sym.ada_type.bounds:
                    array_low = sym.ada_type.bounds[0][0]

            # Check local variables for string types
            elif self.ctx and name in self.ctx.locals:
                local = self.ctx.locals[name]
                if local.ada_type:
                    # Extract type name from local's ada_type
                    type_name = self._extract_type_name(local.ada_type)
                    if type_name and type_name.lower() == 'string':
                        element_size = 1  # Strings are Character arrays (1 byte)
                        array_low = 1  # Ada strings are 1-indexed

            # Check parameters
            elif self.ctx and name in self.ctx.params:
                param = self.ctx.params[name]
                if hasattr(param, 'ada_type') and param.ada_type:
                    type_name = self._extract_type_name(param.ada_type)
                    if type_name and type_name.lower() == 'string':
                        element_size = 1
                        array_low = 1

            # Calculate offset: (slice_low - array_low) * element_size
            if isinstance(low_bound, Immediate):
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

        # Check inline parameters first (for inlined function calls)
        if hasattr(self, '_inline_params') and name in self._inline_params:
            return self._inline_params[name]

        # Check locals
        if name in self.ctx.locals:
            local = self.ctx.locals[name]
            # For address-based renames (complex expressions), dereference the stored address
            # Simple renames share the source's vreg, so no dereferencing needed
            if getattr(local, 'is_address_rename', False):
                result = self.builder.new_vreg(IRType.WORD, f"_{name}_deref")
                self.builder.emit(IRInstr(
                    OpCode.LOAD, result,
                    MemoryLocation(offset=0, ir_type=IRType.WORD, base=local.vreg),
                    comment=f"deref rename {name}"
                ))
                return result
            # For Float64 types, return the address of the 8-byte value on stack
            # Need to resolve the type since ada_type may be AST node (SubtypeIndication)
            resolved_type = self._resolve_local_type(local.ada_type) if local.ada_type else None
            if resolved_type and self._is_float64_type(resolved_type):
                frame_offset = -(self.ctx.locals_size - local.stack_offset)
                local_addr = self.builder.new_vreg(IRType.PTR, f"_{name}_addr")
                self.builder.emit(IRInstr(
                    OpCode.LEA,
                    dst=local_addr,
                    src1=MemoryLocation(offset=frame_offset, ir_type=IRType.PTR, is_frame_offset=True),
                    comment=f"address of {name}"
                ))
                return local_addr
            return local.vreg

        # Check params
        if name in self.ctx.params:
            param_vreg = self.ctx.params[name]
            # For byref parameters (out/in out), dereference the pointer to get value
            if name in self.ctx.byref_params:
                result = self.builder.new_vreg(IRType.WORD, f"_{name}_val")
                self.builder.emit(IRInstr(
                    OpCode.LOAD, result,
                    MemoryLocation(offset=0, ir_type=IRType.WORD, base=param_vreg),
                    comment=f"deref byref param {name}"
                ))
                return result
            return param_vreg

        # Check for nested package variables (passed as hidden outer parameters)
        if hasattr(self, '_current_nested_package') and self._current_nested_package:
            pkg_name = self._current_nested_package
            prefixed_name = f"{pkg_name}.{name}"
            # Check if passed as hidden outer parameter
            outer_param_prefixed = f"_outer_{prefixed_name}"
            if outer_param_prefixed in self.ctx.params:
                ptr_vreg = self.ctx.params[outer_param_prefixed]
                result = self.builder.new_vreg(IRType.WORD, f"_{name}_outer")
                self.builder.emit(IRInstr(
                    OpCode.LOAD, result,
                    MemoryLocation(offset=0, ir_type=IRType.WORD, base=ptr_vreg),
                    comment=f"load {name} from package {pkg_name}"
                ))
                return result

        # Check enclosing scopes via static link (for nested subprograms)
        # For outer-scope variables, we have hidden byref parameters with names like "_outer_<varname>"
        outer_param_name = f"_outer_{name}"
        if outer_param_name in self.ctx.params:
            # This is an outer-scope variable passed as a hidden pointer
            ptr_vreg = self.ctx.params[outer_param_name]
            result = self.builder.new_vreg(IRType.WORD, f"_{name}_outer")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(offset=0, ir_type=IRType.WORD, base=ptr_vreg),
                comment=f"load {name} from enclosing scope"
            ))
            return result

        # Check named numbers (universal integer constants)
        if name in self.ctx.named_numbers:
            return Immediate(self.ctx.named_numbers[name], IRType.WORD)

        # Check for boolean literals
        if name == "true":
            return Immediate(1, IRType.BOOL)
        if name == "false":
            return Immediate(0, IRType.BOOL)

        # Check if this identifier is a parameterless function call
        # In Ada, Get_Value without () is still a function call if it's a function with no required params
        sym = self.symbols.lookup(expr.name)
        if sym and sym.kind == SymbolKind.FUNCTION:
            # Check if function has no required parameters
            has_required_params = False
            if sym.parameters:
                for p in sym.parameters:
                    if p.default_value is None:
                        has_required_params = True
                        break
            if not has_required_params:
                # This is a parameterless function call - synthesize a FunctionCall and lower it
                func_call = FunctionCall(name=expr, args=[])
                return self._lower_function_call(func_call)

        # Also check for nested subprograms tracked locally (not in symbol table)
        if name in self._subprogram_param_modes:
            param_modes = self._subprogram_param_modes[name]
            if len(param_modes) == 0:
                # This is a parameterless nested function - call it
                func_call = FunctionCall(name=expr, args=[])
                return self._lower_function_call(func_call)

        # Check for enumeration literals in symbol table
        sym = self.symbols.lookup(expr.name)
        if sym and sym.is_constant and sym.ada_type:
            from uada80.type_system import EnumerationType
            if isinstance(sym.ada_type, EnumerationType):
                # Get the position value for this literal
                pos = sym.ada_type.positions.get(expr.name)
                if pos is not None:
                    return Immediate(pos, IRType.WORD)

        # Fallback: check local type declarations for enum literals
        if hasattr(self, '_current_body_declarations'):
            from uada80.ast_nodes import EnumerationTypeDef
            for d in self._current_body_declarations:
                if isinstance(d, TypeDecl) and d.type_def:
                    if isinstance(d.type_def, EnumerationTypeDef) and d.type_def.literals:
                        # Check if this literal is in this enum type
                        for i, lit in enumerate(d.type_def.literals):
                            if lit.lower() == name:
                                return Immediate(i, IRType.WORD)

        # Default
        return Immediate(0, IRType.WORD)

    def _operator_to_name(self, op: BinaryOp) -> Optional[str]:
        """Convert a binary operator to its Ada operator function name."""
        op_names = {
            BinaryOp.ADD: '"+'+'"',
            BinaryOp.SUB: '"-'+'"',
            BinaryOp.MUL: '"*'+'"',
            BinaryOp.DIV: '"/'+'"',
            BinaryOp.MOD: '"mod'+'"',
            BinaryOp.REM: '"rem'+'"',
            BinaryOp.EXP: '"**'+'"',
            BinaryOp.AND: '"and'+'"',
            BinaryOp.OR: '"or'+'"',
            BinaryOp.XOR: '"xor'+'"',
            BinaryOp.EQ: '"='+'"',
            BinaryOp.NE: '"/='+'"',
            BinaryOp.LT: '"<'+'"',
            BinaryOp.LE: '"<='+'"',
            BinaryOp.GT: '">'+'"',
            BinaryOp.GE: '">='+'"',
            BinaryOp.CONCAT: '"&'+'"',
        }
        return op_names.get(op)

    def _lookup_user_operator(self, op: BinaryOp, left: Expr, right: Expr) -> Optional[Symbol]:
        """Look up a user-defined operator for the given operands.

        Returns the Symbol for the user-defined operator function if one exists,
        otherwise returns None to use the built-in operator.
        """
        # Get the operator function name
        op_name = self._operator_to_name(op)
        if op_name is None:
            return None

        # Get operand types
        left_type = self._get_expr_type(left)
        right_type = self._get_expr_type(right)

        if left_type is None or right_type is None:
            return None

        # Don't look up operators for predefined types - use built-ins
        predefined_type_names = {'integer', 'boolean', 'character', 'natural', 'positive'}
        if hasattr(left_type, 'name') and left_type.name.lower() in predefined_type_names:
            # For equality operators, still check for user overloads on composite types
            if op not in (BinaryOp.EQ, BinaryOp.NE):
                return None
            if not isinstance(left_type, (RecordType, ArrayType)):
                return None

        # Look up operator overloads
        overloads = self.symbols.all_overloads(op_name)
        if not overloads:
            # Also try without quotes (some symbol tables store operators differently)
            alt_name = op_name.strip('"')
            overloads = self.symbols.all_overloads(alt_name)

        # Also check locally-tracked operators (nested functions)
        if not overloads:
            # Check _subprogram_param_names for locally-defined operators
            alt_name = op_name.strip('"')
            if alt_name in self._subprogram_param_names:
                # Found a locally-defined operator - create a minimal symbol to represent it
                from uada80.symbol_table import Symbol, SymbolKind
                local_sym = Symbol(
                    name=alt_name,
                    kind=SymbolKind.FUNCTION,
                )
                return local_sym

        if not overloads:
            return None

        # Find matching overload based on operand types
        from uada80.type_system import types_compatible

        for sym in overloads:
            if sym.kind != SymbolKind.FUNCTION:
                continue
            if len(sym.parameters) != 2:
                continue

            # Check if parameter types match operand types
            param1_type = sym.parameters[0].ada_type if sym.parameters[0].ada_type else None
            param2_type = sym.parameters[1].ada_type if sym.parameters[1].ada_type else None

            if param1_type is None or param2_type is None:
                continue

            left_match = (param1_type.name == left_type.name or
                          types_compatible(param1_type, left_type))
            right_match = (param2_type.name == right_type.name or
                           types_compatible(param2_type, right_type))

            if left_match and right_match:
                return sym

        return None

    def _call_user_operator(self, sym: Symbol, left_operand: Expr, right_operand: Expr):
        """Generate a call to a user-defined operator function.

        Generates code to:
        1. Evaluate left and right operands
        2. Push arguments on stack (by value or by reference for records)
        3. Call the operator function
        4. Capture and return the result
        """
        result = self.builder.new_vreg(IRType.WORD, "_op_result")

        # Check if this operator has byref parameters (e.g., records)
        op_name = sym.name.lower()
        byref_params = self._subprogram_byref_params.get(op_name, [])

        # Determine if left operand should be passed by reference
        left_byref = len(byref_params) > 0 and byref_params[0]
        # Determine if right operand should be passed by reference
        right_byref = len(byref_params) > 1 and byref_params[1]

        # Evaluate and push operands (right-to-left for Z80 calling convention)
        if right_byref:
            right_val = self._get_arg_address(right_operand)
        else:
            right_val = self._lower_expr(right_operand)
        self.builder.push(right_val)

        if left_byref:
            left_val = self._get_arg_address(left_operand)
        else:
            left_val = self._lower_expr(left_operand)
        self.builder.push(left_val)

        # Call the operator function
        op_func_name = sym.name
        self.builder.call(Label(op_func_name), comment=f"user-defined operator {sym.name}")

        # Capture result from HL register IMMEDIATELY after call
        # (before POPs which would clobber HL)
        self.builder.emit(IRInstr(
            OpCode.MOV, result,
            MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
            comment=f"capture {sym.name} result from HL"
        ))

        # Clean up stack (2 arguments)
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)
        self.builder.pop(temp)

        return result

    def _lower_binary(self, expr: BinaryExpr):
        """Lower a binary expression."""
        op = expr.op

        # Handle short-circuit operators specially - don't evaluate right operand unconditionally
        if op == BinaryOp.AND_THEN:
            return self._lower_short_circuit_and(expr)
        elif op == BinaryOp.OR_ELSE:
            return self._lower_short_circuit_or(expr)

        # Check for user-defined operator overloading
        user_op = self._lookup_user_operator(op, expr.left, expr.right)
        if user_op is not None:
            return self._call_user_operator(user_op, expr.left, expr.right)

        # Check if this is a Float64 operation
        left_type = self._get_expr_type(expr.left)
        right_type = self._get_expr_type(expr.right)
        if self._is_float64_type(left_type) or self._is_float64_type(right_type):
            if op in (BinaryOp.ADD, BinaryOp.SUB, BinaryOp.MUL, BinaryOp.DIV, BinaryOp.REM, BinaryOp.MOD):
                left_ptr = self._lower_float64_operand(expr.left)
                right_ptr = self._lower_float64_operand(expr.right)
                return self._lower_float64_binary(op, left_ptr, right_ptr, left_type or right_type)
            elif op in (BinaryOp.EQ, BinaryOp.NE, BinaryOp.LT, BinaryOp.LE, BinaryOp.GT, BinaryOp.GE):
                left_ptr = self._lower_float64_operand(expr.left)
                right_ptr = self._lower_float64_operand(expr.right)
                return self._lower_float64_comparison(op, left_ptr, right_ptr)
            elif op == BinaryOp.EXP and self._is_float64_type(left_type):
                # Float64 ** Integer exponentiation
                left_ptr = self._lower_float64_operand(expr.left)
                right_val = self._lower_expr(expr.right)  # Integer exponent
                return self._lower_float64_exp(left_ptr, right_val, left_type)

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
            # Push base first, then exponent (ends at lower offset)
            self.builder.push(left)   # base (IX+10 in runtime)
            self.builder.push(right)  # exponent (IX+8 in runtime)
            self.builder.call(Label("_exp16"))
            # Capture result from HL register BEFORE popping arguments
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture exponentiation result from HL"
            ))
            # Clean up stack (4 bytes for two 16-bit args)
            # Codegen expects: ADD _SP, immediate (dst=_SP, src1=immediate)
            self.builder.emit(IRInstr(
                OpCode.ADD,
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                Immediate(4, IRType.WORD),
                comment="clean up stack after _exp16"
            ))
        elif op == BinaryOp.CONCAT:
            # Check if this is array concatenation or string concatenation
            # String = array of Character, use runtime strcat
            # Other arrays = element-wise copy
            expr_type = self._get_expr_type(expr)

            # Determine if this is a string type by checking:
            # 1. Type name is "String" (or "string")
            # 2. Component type is Character
            is_string_type = False
            if expr_type and expr_type.kind == TypeKind.ARRAY:
                type_name = getattr(expr_type, 'name', '').lower()
                if type_name == 'string':
                    is_string_type = True
                elif hasattr(expr_type, 'component_type') and expr_type.component_type:
                    comp_name = getattr(expr_type.component_type, 'name', '').lower()
                    if comp_name == 'character':
                        is_string_type = True

            is_array_concat = (expr_type and expr_type.kind == TypeKind.ARRAY and
                               not is_string_type)
            if is_array_concat:
                # Array concatenation: copy elements from both arrays
                result = self._lower_array_concat(expr, left, right)
            else:
                # String concatenation: call runtime function
                # _strcat expects: s1 at IX+8 (first push), s2 at IX+6 (second push)
                # Result = s1 + s2 = left + right
                self.builder.push(left)   # first string (s1) -> will be at IX+8
                self.builder.push(right)  # second string (s2) -> will be at IX+6
                self.builder.call(Label("_strcat"))
                # Capture result from HL register BEFORE stack cleanup
                # (pop uses HL which would overwrite the result)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture string concat result from HL"
                ))
                # Now clean up the stack (2 words = 4 bytes)
                self.builder.emit(IRInstr(
                    OpCode.ADD,
                    MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                    Immediate(4, IRType.WORD),
                    comment="clean up 2 pushed arguments"
                ))
        else:
            # Default: move left to result
            self.builder.mov(result, left)

        # Apply modular masking for modular type arithmetic
        # For operations like B + 10 where B is mod 256, result must be masked
        if op in (BinaryOp.ADD, BinaryOp.SUB, BinaryOp.MUL):
            expr_type = self._get_expr_type(expr)
            if expr_type:
                from uada80.type_system import ModularType
                if isinstance(expr_type, ModularType):
                    modulus = expr_type.modulus
                    # Check if modulus is a power of 2 (use AND for efficiency)
                    if modulus > 0 and (modulus & (modulus - 1)) == 0:
                        mask = modulus - 1
                        masked = self.builder.new_vreg(IRType.WORD, "_mod_masked")
                        self.builder.and_(masked, result, Immediate(mask, IRType.WORD))
                        result = masked
                    else:
                        # Non-power-of-2: use MOD operation
                        masked = self.builder.new_vreg(IRType.WORD, "_mod_result")
                        self.builder.emit(IRInstr(OpCode.MOD, masked, result, Immediate(modulus, IRType.WORD)))
                        result = masked

        return result

    def _lower_array_concat(self, expr: BinaryExpr, left_addr, right_addr):
        """Lower array concatenation A & B.

        Creates a new array on the stack with elements from both arrays.
        Returns the address of the new array.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Get the array type to determine element size and bounds
        array_type = self._get_expr_type(expr)
        if not array_type or not isinstance(array_type, ArrayType):
            # Fallback to string concatenation
            return left_addr

        # Get element size (assume 2 bytes for Integer)
        elem_size = 2
        if array_type.component_type:
            elem_size = (array_type.component_type.size_bits + 7) // 8

        # Get the sizes of the left and right arrays
        left_type = self._get_expr_type(expr.left)
        right_type = self._get_expr_type(expr.right)

        left_len = 0
        right_len = 0
        if left_type and isinstance(left_type, ArrayType) and left_type.bounds:
            low, high = left_type.bounds[0]
            left_len = high - low + 1
        if right_type and isinstance(right_type, ArrayType) and right_type.bounds:
            low, high = right_type.bounds[0]
            right_len = high - low + 1

        total_len = left_len + right_len
        total_size = total_len * elem_size

        # Allocate space on stack for the result array
        result_addr = self.builder.new_vreg(IRType.PTR, "_concat_result")
        if total_size > 0:
            self.builder.emit(IRInstr(
                OpCode.SUB,
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                Immediate(total_size, IRType.WORD),
                comment=f"allocate {total_size} bytes for concat result"
            ))
            # Get SP into result_addr
            self.builder.emit(IRInstr(
                OpCode.MOV, result_addr,
                MemoryLocation(is_global=False, symbol_name="_SP", ir_type=IRType.WORD),
                comment="concat result address"
            ))

        # Copy elements from left array
        if left_len > 0:
            for i in range(left_len):
                offset = i * elem_size
                # Load element from source
                src_elem = self.builder.new_vreg(IRType.WORD, f"_concat_src{i}")
                self.builder.emit(IRInstr(
                    OpCode.LOAD, src_elem,
                    MemoryLocation(base=left_addr, offset=offset, ir_type=IRType.WORD),
                    comment=f"load left[{i}]"
                ))
                # Store to destination
                dest_offset = i * elem_size
                self.builder.emit(IRInstr(
                    OpCode.STORE,
                    dst=MemoryLocation(base=result_addr, offset=dest_offset, ir_type=IRType.WORD),
                    src1=src_elem,
                    comment=f"store result[{i}]"
                ))

        # Copy elements from right array
        if right_len > 0:
            for i in range(right_len):
                offset = i * elem_size
                # Load element from source
                src_elem = self.builder.new_vreg(IRType.WORD, f"_concat_src{left_len + i}")
                self.builder.emit(IRInstr(
                    OpCode.LOAD, src_elem,
                    MemoryLocation(base=right_addr, offset=offset, ir_type=IRType.WORD),
                    comment=f"load right[{i}]"
                ))
                # Store to destination
                dest_offset = (left_len + i) * elem_size
                self.builder.emit(IRInstr(
                    OpCode.STORE,
                    dst=MemoryLocation(base=result_addr, offset=dest_offset, ir_type=IRType.WORD),
                    src1=src_elem,
                    comment=f"store result[{left_len + i}]"
                ))

        return result_addr

    def _lower_short_circuit_and(self, expr: BinaryExpr):
        """Lower short-circuit AND THEN.

        Evaluates left operand first. If false, returns false without
        evaluating right operand. Only evaluates right if left is true.
        """
        result = self.builder.new_vreg(IRType.WORD, "_and_then")

        # Create labels (new_label returns a string, not Label object)
        eval_right = self.builder.new_label("and_eval_right")
        short_circuit = self.builder.new_label("and_short")
        done = self.builder.new_label("and_done")

        # Evaluate left operand
        left = self._lower_expr(expr.left)

        # Check if left is false (0)
        is_false = self.builder.new_vreg(IRType.WORD, "_is_false")
        self.builder.cmp_eq(is_false, left, Immediate(0, IRType.WORD))
        self.builder.jnz(is_false, short_circuit)  # If false, short-circuit

        # Left was true, evaluate right
        self.builder.label(eval_right)
        right = self._lower_expr(expr.right)
        self.builder.mov(result, right)  # Result is right operand's value
        self.builder.jmp(done)

        # Short circuit: left was false, result is false
        self.builder.label(short_circuit)
        self.builder.mov(result, Immediate(0, IRType.WORD))

        self.builder.label(done)
        return result

    def _lower_short_circuit_or(self, expr: BinaryExpr):
        """Lower short-circuit OR ELSE.

        Evaluates left operand first. If true, returns true without
        evaluating right operand. Only evaluates right if left is false.
        """
        result = self.builder.new_vreg(IRType.WORD, "_or_else")

        # Create labels (new_label returns a string, not Label object)
        eval_right = self.builder.new_label("or_eval_right")
        short_circuit = self.builder.new_label("or_short")
        done = self.builder.new_label("or_done")

        # Evaluate left operand
        left = self._lower_expr(expr.left)

        # Check if left is true (non-zero)
        is_true = self.builder.new_vreg(IRType.WORD, "_is_true")
        self.builder.cmp_ne(is_true, left, Immediate(0, IRType.WORD))
        self.builder.jnz(is_true, short_circuit)  # If true, short-circuit

        # Left was false, evaluate right
        self.builder.label(eval_right)
        right = self._lower_expr(expr.right)
        self.builder.mov(result, right)  # Result is right operand's value
        self.builder.jmp(done)

        # Short circuit: left was true, result is true (use left's value)
        self.builder.label(short_circuit)
        self.builder.mov(result, left)

        self.builder.label(done)
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

    def _unary_operator_to_name(self, op: UnaryOp) -> Optional[str]:
        """Convert a unary operator to its Ada operator function name."""
        op_names = {
            UnaryOp.MINUS: '"-"',
            UnaryOp.PLUS: '"+"',
            UnaryOp.NOT: '"not"',
            UnaryOp.ABS: '"abs"',
        }
        return op_names.get(op)

    def _lookup_user_unary_operator(self, op: UnaryOp, operand: Expr) -> Optional[Symbol]:
        """Look up a user-defined unary operator for the given operand.

        Returns the Symbol for the user-defined operator function if one exists,
        otherwise returns None to use the built-in operator.
        """
        # Get the operator function name
        op_name = self._unary_operator_to_name(op)
        if op_name is None:
            return None

        # Get operand type
        operand_type = self._get_expr_type(operand)
        if operand_type is None:
            return None

        # Don't look up operators for predefined types - use built-ins
        predefined_type_names = {'integer', 'boolean', 'character', 'natural', 'positive'}
        if hasattr(operand_type, 'name') and operand_type.name.lower() in predefined_type_names:
            return None

        # Look up operator overloads
        overloads = self.symbols.all_overloads(op_name)
        if not overloads:
            # Also try without quotes
            alt_name = op_name.strip('"')
            overloads = self.symbols.all_overloads(alt_name)
            if not overloads:
                return None

        # Find matching overload based on operand type
        from uada80.type_system import types_compatible

        for sym in overloads:
            if sym.kind != SymbolKind.FUNCTION:
                continue
            if len(sym.parameters) != 1:
                continue

            # Check if parameter type matches operand type
            param_type = sym.parameters[0].ada_type if sym.parameters[0].ada_type else None
            if param_type is None:
                continue

            if (param_type.name == operand_type.name or
                types_compatible(param_type, operand_type)):
                return sym

        return None

    def _call_user_unary_operator(self, sym: Symbol, operand: Expr):
        """Generate a call to a user-defined unary operator function."""
        result = self.builder.new_vreg(IRType.WORD, "_unary_result")

        # Evaluate operand
        operand_val = self._lower_expr(operand)

        # Push argument
        self.builder.push(operand_val)

        # Call the operator function
        self.builder.call(Label(sym.name), comment=f"user-defined unary {sym.name}")

        # Clean up stack (1 argument)
        temp = self.builder.new_vreg(IRType.WORD, "_discard")
        self.builder.pop(temp)

        # Capture result from HL register
        self.builder.emit(IRInstr(
            OpCode.MOV, result,
            MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
            comment=f"capture {sym.name} result from HL"
        ))

        return result

    def _lower_unary(self, expr: UnaryExpr):
        """Lower a unary expression."""
        op = expr.op

        # Check for user-defined operator overloading
        user_op = self._lookup_user_unary_operator(op, expr.operand)
        if user_op is not None:
            return self._call_user_unary_operator(user_op, expr.operand)

        # Check if this is a Float64 operation
        operand_type = self._get_expr_type(expr.operand)
        if self._is_float64_type(operand_type):
            if op in (UnaryOp.MINUS, UnaryOp.ABS, UnaryOp.PLUS):
                operand_ptr = self._lower_float64_operand(expr.operand)
                return self._lower_float64_unary(op, operand_ptr, operand_type)

        operand = self._lower_expr(expr.operand)
        result = self.builder.new_vreg(IRType.WORD, "_tmp")

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

    def _get_type_name_from_expr(self, expr: Expr) -> Optional[str]:
        """Get the type name from an expression (for type references)."""
        if isinstance(expr, Identifier):
            return expr.name
        elif isinstance(expr, SelectedName):
            # Package.Type -> Type
            return expr.selector
        elif isinstance(expr, AttributeReference):
            # T'Base, T'Class etc. -> T
            return self._get_type_name_from_expr(expr.prefix)
        return None

    def _expr_to_name(self, expr) -> str:
        """Convert an expression to a string name (for generating unique identifiers)."""
        if isinstance(expr, Identifier):
            return expr.name
        elif isinstance(expr, SelectedName):
            return f"{self._expr_to_name(expr.prefix)}_{expr.selector}"
        elif isinstance(expr, IndexedComponent):
            return f"{self._expr_to_name(expr.prefix)}_idx"
        elif isinstance(expr, IntegerLiteral):
            return f"lit_{expr.value}"
        else:
            # Fallback - use hash to generate unique name
            return f"expr_{hash(str(expr)) & 0xFFFF:04x}"

    def _get_expr_type(self, expr: Expr) -> Optional[AdaType]:
        """Get the Ada type of an expression."""
        if isinstance(expr, IntegerLiteral):
            return PREDEFINED_TYPES.get("Integer")
        elif isinstance(expr, RealLiteral):
            # Real literals are Long_Float (Float64) by default
            return PREDEFINED_TYPES.get("Long_Float")
        elif isinstance(expr, StringLiteral):
            return PREDEFINED_TYPES.get("String")
        elif isinstance(expr, CharacterLiteral):
            return PREDEFINED_TYPES.get("Character")
        elif isinstance(expr, Identifier):
            # First check local variables
            if self.ctx:
                name = expr.name.lower()
                if name in self.ctx.locals:
                    local = self.ctx.locals[name]
                    if local.ada_type:
                        # ada_type might be AST node or AdaType - resolve it
                        return self._resolve_local_type(local.ada_type)
            # Then check symbol table
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

    def _resolve_local_type(self, type_node) -> Optional[AdaType]:
        """Resolve a local variable's type (may be AST node or AdaType)."""
        if type_node is None:
            return None
        # If it's already an AdaType, return it
        from uada80.type_system import AdaType, ModularType, ArrayType, IntegerType
        if isinstance(type_node, AdaType):
            return type_node
        # Handle anonymous ArrayTypeDef directly
        if isinstance(type_node, ArrayTypeDef):
            bounds = []
            if type_node.index_subtypes:
                for idx_range in type_node.index_subtypes:
                    if isinstance(idx_range, RangeExpr):
                        low = self._eval_static_expr(idx_range.low)
                        high = self._eval_static_expr(idx_range.high)
                        bounds.append((low, high))
            comp_type = IntegerType(name="Integer", size_bits=16)
            return ArrayType(
                name="<anonymous>",
                component_type=comp_type,
                bounds=bounds,
                is_constrained=True,
            )
        # If it's an Identifier, look up the type name
        if isinstance(type_node, Identifier):
            type_name = type_node.name.lower()
            # First check symbol table
            sym = self.symbols.lookup(type_node.name)
            if sym and sym.ada_type:
                return sym.ada_type
            # Check local type declarations from current body
            if hasattr(self, '_current_body_declarations') and self._current_body_declarations:
                from uada80.ast_nodes import ModularTypeDef, RecordTypeDef
                from uada80.type_system import RecordType, RecordComponent, IntegerType
                for d in self._current_body_declarations:
                    if isinstance(d, TypeDecl) and d.name.lower() == type_name:
                        type_def = d.type_def
                        # Handle modular type definition
                        if isinstance(type_def, ModularTypeDef):
                            modulus = self._eval_static_expr(type_def.modulus)
                            return ModularType(name=d.name, modulus=modulus)
                        # Handle record type definition
                        elif isinstance(type_def, RecordTypeDef):
                            components = []
                            offset = 0
                            for comp_decl in type_def.components:
                                # Resolve the actual component type
                                comp_type = self._resolve_local_type(comp_decl.type_mark)
                                if comp_type is None:
                                    comp_type = IntegerType(name="Integer", size_bits=16)
                                comp_size = (comp_type.size_bits + 7) // 8 if comp_type.size_bits else 2
                                for comp_name in comp_decl.names:
                                    components.append(RecordComponent(
                                        name=comp_name,
                                        component_type=comp_type,
                                        offset_bits=offset * 8,
                                        size_bits=comp_type.size_bits if comp_type.size_bits else 16,
                                    ))
                                    offset += comp_size
                            return RecordType(
                                name=type_name,
                                components=components,
                                size_bits=offset * 8,
                            )
                        # Handle array type definition
                        elif isinstance(type_def, ArrayTypeDef):
                            from uada80.type_system import ArrayType, IntegerType
                            bounds = []
                            is_unconstrained = False
                            if type_def.index_subtypes:
                                idx_range = type_def.index_subtypes[0]
                                if isinstance(idx_range, RangeExpr):
                                    low = self._eval_static_expr(idx_range.low)
                                    high = self._eval_static_expr(idx_range.high)
                                    bounds.append((low, high))
                                elif isinstance(idx_range, SubtypeIndication):
                                    # Check if it's an unconstrained array (has BoxConstraint)
                                    from uada80.ast_nodes import BoxConstraint
                                    if isinstance(idx_range.constraint, BoxConstraint):
                                        is_unconstrained = True
                            # Resolve component type
                            comp_type = IntegerType(name="Integer", size_bits=16)
                            if type_def.component_type:
                                if isinstance(type_def.component_type, Identifier):
                                    comp_name = type_def.component_type.name.lower()
                                    if comp_name == "integer":
                                        comp_type = IntegerType(name="Integer", size_bits=16)
                            return ArrayType(
                                name=type_name,
                                component_type=comp_type,
                                bounds=bounds if bounds else None,
                                is_constrained=not is_unconstrained and bool(bounds),
                            )
        # If it's a SubtypeIndication, get the type_mark and apply constraints
        if isinstance(type_node, SubtypeIndication):
            base_type = None
            if type_node.type_mark:
                base_type = self._resolve_local_type(type_node.type_mark)
            # Apply index constraint if present (for arrays like Arr(1..2))
            if type_node.constraint and isinstance(type_node.constraint, IndexConstraint):
                if base_type and isinstance(base_type, ArrayType):
                    # Extract bounds from constraint
                    bounds = []
                    for rng in type_node.constraint.ranges:
                        if isinstance(rng, RangeExpr):
                            low = self._eval_static_expr(rng.low)
                            high = self._eval_static_expr(rng.high)
                            if low is not None and high is not None:
                                bounds.append((low, high))
                    if bounds:
                        # Create constrained array type
                        return ArrayType(
                            name=base_type.name,
                            kind=base_type.kind,
                            size_bits=sum((h - l + 1) for l, h in bounds) * (base_type.component_type.size_bits if base_type.component_type else 16),
                            component_type=base_type.component_type,
                            bounds=bounds,
                            is_constrained=True,
                        )
            return base_type
        # Handle Slice (constrained subtype like String(1..5))
        if isinstance(type_node, Slice):
            base_type = self._resolve_local_type(type_node.prefix)
            if isinstance(base_type, ArrayType) and not base_type.is_constrained:
                # Extract bounds from the range expression
                if isinstance(type_node.range_expr, RangeExpr):
                    low = self._eval_static_expr(type_node.range_expr.low)
                    high = self._eval_static_expr(type_node.range_expr.high)
                    if low is not None and high is not None:
                        return ArrayType(
                            name=f"{base_type.name}({low}..{high})",
                            kind=base_type.kind,
                            size_bits=(high - low + 1) * (base_type.component_type.size_bits if base_type.component_type else 8),
                            component_type=base_type.component_type,
                            index_types=base_type.index_types if hasattr(base_type, 'index_types') else None,
                            bounds=[(low, high)],
                            is_constrained=True,
                            base_type=base_type,
                        )
            return base_type
        return None

    def _get_array_bounds_from_expr(self, expr) -> Optional[list[tuple[int, int]]]:
        """Get array bounds from an expression (variable name).

        Returns a list of (low, high) tuples for each dimension, or None if not found.
        """
        from uada80.type_system import ArrayType

        if not isinstance(expr, Identifier):
            return None

        var_name = expr.name.lower()

        # Check local variables
        if self.ctx and var_name in self.ctx.locals:
            local = self.ctx.locals[var_name]
            ada_type = self._resolve_local_type(local.ada_type)
            if isinstance(ada_type, ArrayType) and ada_type.bounds:
                return ada_type.bounds

        # Check global symbol table
        sym = self.symbols.lookup(expr.name)
        if sym and sym.ada_type:
            if isinstance(sym.ada_type, ArrayType) and sym.ada_type.bounds:
                return sym.ada_type.bounds

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

        # Check for generic formal subprogram substitution
        # If we're inside a generic instantiation and the called name is a formal function,
        # replace it with the actual function
        if self._generic_type_map:
            subp_key = f"_subp_{func_name}"
            if subp_key in self._generic_type_map:
                # This is a call to a generic formal function - substitute the actual
                actual_name = self._generic_type_map[subp_key]
                func_name = actual_name.lower()
                # Update expr.name to point to actual function
                expr = FunctionCall(
                    name=Identifier(actual_name),
                    args=expr.args
                )

        # Check for Unchecked_Conversion instantiation call
        sym = self.symbols.lookup(func_name)
        if sym and sym.is_unchecked_conversion and len(expr.args) >= 1:
            # Unchecked_Conversion just returns the bit pattern unchanged
            return self._lower_expr(expr.args[0].value)

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

        # Handle Ada.Numerics.Elementary_Functions.Sqrt for Float64
        if func_name == "sqrt" and len(expr.args) >= 1:
            arg_type = self._get_expr_type(expr.args[0].value)
            if self._is_float64_type(arg_type):
                # Float64 sqrt - call _f64_sqrt
                return self._lower_float64_sqrt(expr.args[0].value)

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
            # or runtime_name for built-in container operations
            call_target = expr.name.name
            if sym:
                if sym.runtime_name:
                    # Built-in container/library operation
                    call_target = sym.runtime_name
                elif sym.is_imported and sym.external_name:
                    call_target = sym.external_name
                else:
                    call_target = sym.name

            # Check for overloaded function - use unique label if available
            # Count actual arguments being passed
            arg_count = len(expr.args) if expr.args else 0
            label_key = (call_target.lower(), arg_count)
            if label_key in self._function_label_map:
                call_target = self._function_label_map[label_key]

            # Check if this is a dispatching call
            is_dispatching = self._is_dispatching_call(sym, expr.args)

            # Get parameter modes for out/in out handling
            # First try locally-tracked modes (for nested subprograms), then symbol table
            param_modes = []
            func_name_lower = func_name.lower() if func_name else call_target.lower()
            if func_name_lower in self._subprogram_param_modes:
                param_modes = self._subprogram_param_modes[func_name_lower]
            elif sym and sym.parameters:
                param_modes = [p.mode for p in sym.parameters]

            # For nested subprograms, push outer variable addresses (in reverse order)
            outer_var_slots = 0
            if func_name_lower in self._nested_outer_vars:
                outer_vars = self._nested_outer_vars[func_name_lower]
                for var_name in reversed(list(outer_vars)):
                    # Get address of the outer variable
                    if self.ctx and var_name in self.ctx.locals:
                        local = self.ctx.locals[var_name]
                        addr = self.builder.new_vreg(IRType.PTR, f"_{var_name}_addr")
                        self.builder.emit(IRInstr(
                            OpCode.LEA, addr,
                            MemoryLocation(ir_type=IRType.PTR, addr_vreg=local.vreg),
                            comment=f"addr of outer {var_name}"
                        ))
                        self.builder.push(addr)
                        outer_var_slots += 1

            # Build effective args in formal parameter order (handles named parameters)
            effective_exprs = self._build_effective_args(expr.args, sym, func_name_lower)

            # Get byref info for this function (includes record types)
            byref_params = []
            if func_name_lower in self._subprogram_byref_params:
                byref_params = self._subprogram_byref_params[func_name_lower]

            # Push arguments (right to left for cdecl-style calling convention)
            # Track number of stack slots used (for cleanup)
            stack_slots = 0
            for arg_idx, arg_value in enumerate(reversed(effective_exprs)):
                if arg_value:
                    # Check if this argument is an unconstrained array
                    forward_idx = len(effective_exprs) - 1 - arg_idx
                    arg_is_unconstrained = self._is_unconstrained_array_arg(sym, forward_idx)

                    # Check parameter mode for out/in out handling
                    param_mode = param_modes[forward_idx] if forward_idx < len(param_modes) else "in"

                    # Check if parameter is byref (mode or record type)
                    is_byref = byref_params[forward_idx] if forward_idx < len(byref_params) else False

                    if arg_is_unconstrained:
                        # Push dope vector: last, first, ptr (reverse order for stack)
                        first_val, last_val, ptr_val = self._get_array_dope_vector(arg_value)
                        self.builder.push(last_val)
                        self.builder.push(first_val)
                        self.builder.push(ptr_val)
                        stack_slots += 3
                    elif param_mode in ("out", "in out") or is_byref:
                        # Pass address of the argument
                        addr = self._get_arg_address(arg_value)
                        self.builder.push(addr)
                        stack_slots += 1
                    else:
                        # Regular argument - pass by value
                        value = self._lower_expr(arg_value)
                        self.builder.push(value)
                        stack_slots += 1

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
            elif sym and sym.is_inline and self._can_inline(sym, expr.args):
                # Inline expansion for pragma Inline functions
                self._inline_function_call(sym, expr.args, result)
                # For inline calls, result is already set - just skip normal call path
                # by returning early after cleanup
                if stack_slots > 0:
                    for _ in range(stack_slots):
                        temp = self.builder.new_vreg(IRType.WORD, "_discard")
                        self.builder.pop(temp)
                return result
            else:
                # Static call (using external name for imported functions)
                self.builder.call(Label(call_target))

            # Result is already in HL after call - capture it BEFORE cleanup
            # (cleanup uses POP HL which would destroy the return value)
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture function return from HL"
            ))

            # Clean up stack (callee may not clean up on Z80)
            total_slots = stack_slots + outer_var_slots
            if total_slots > 0:
                # Pop arguments off stack
                for _ in range(total_slots):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)

            # Skip the duplicate MOV emission below since we already captured HL
            return result

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
            # First check local variables (higher priority)
            ada_type = None
            var_name = expr.prefix.name.lower()
            if self.ctx and var_name in self.ctx.locals:
                local_info = self.ctx.locals[var_name]
                ada_type = self._resolve_local_type(local_info.ada_type)

            # Always look up in symbol table (needed for 'Access, 'Address, etc.)
            sym = self.symbols.lookup(expr.prefix.name)

            # Fall back to symbol table for type if not found from locals
            if ada_type is None and sym:
                # Get the type - either from a TYPE symbol or from a VARIABLE symbol
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
                        # Check if this is a parameter with dope vector
                        param_name = expr.prefix.name.lower()
                        if self.ctx and f"{param_name}'first" in self.ctx.params:
                            # Parameter has dope vector - use bounds from dope
                            if attr == "first":
                                return self.ctx.params[f"{param_name}'first"]
                            if attr == "last":
                                return self.ctx.params[f"{param_name}'last"]
                            if attr == "length":
                                # length = last - first + 1
                                first = self.ctx.params[f"{param_name}'first"]
                                last = self.ctx.params[f"{param_name}'last"]
                                result = self.builder.new_vreg(IRType.WORD, "_length")
                                temp = self.builder.new_vreg(IRType.WORD, "_temp")
                                self.builder.sub(temp, last, first)
                                self.builder.add(result, temp, Immediate(1, IRType.WORD))
                                return result
                        else:
                            # Not a parameter - fall back to strlen for strings
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

            # Handle 'Access attribute for subprograms (needs sym lookup)
            if attr == "access" and sym:
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
            if attr == "address" and sym:
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
            # IMPORTANT: Capture result from HL BEFORE popping argument
            # The pop would otherwise destroy HL which contains the result
            result = self.builder.new_vreg(IRType.PTR, "_image")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                comment="capture Integer'Image result from HL"
            ))
            # Now clean up the pushed argument
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
            return result

        if attr == "img":
            # X'Img - Ada 2012 shorthand for Type_Of_X'Image(X)
            # The prefix is the object itself, not a type
            # Evaluate the prefix to get its value
            prefix_value = self._lower_expr(expr.prefix)
            self.builder.push(prefix_value)
            self.builder.call(Label("_int_to_str"), comment="X'Img")
            # IMPORTANT: Capture result from HL BEFORE popping argument
            result = self.builder.new_vreg(IRType.PTR, "_img")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                comment="capture X'Img result from HL"
            ))
            # Now clean up the pushed argument
            temp = self.builder.new_vreg(IRType.WORD, "_discard")
            self.builder.pop(temp)
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

        if attr == "update":
            # Record'Update(Component => Value, ...)
            # Array'Update(Index => Value, ...)
            # Returns a copy of the prefix with specified components/elements updated
            #
            # For Z80, we:
            # 1. Copy the original to a temporary
            # 2. Update the specified components
            # 3. Return the temporary
            prefix_value = self._lower_expr(expr.prefix)

            # Get type information
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type

            if ada_type and isinstance(ada_type, RecordType):
                # Record update
                # Allocate space for result (same size as original)
                size_bytes = (ada_type.size_bits + 7) // 8
                result = self.builder.new_vreg(IRType.PTR, "_update_rec")

                # Copy original record to result
                self.builder.push(Immediate(size_bytes, IRType.WORD))
                self.builder.push(prefix_value)
                self.builder.push(result)
                self.builder.call(Label("_memcpy"), comment="copy record for 'Update")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.pop(temp)

                # Apply updates from arguments
                # Arguments are (field => value) pairs
                for arg in expr.args:
                    if isinstance(arg, Aggregate):
                        # Named aggregate with component associations
                        for assoc in getattr(arg, 'associations', []):
                            if hasattr(assoc, 'choices') and hasattr(assoc, 'value'):
                                for choice in assoc.choices:
                                    if isinstance(choice, Identifier):
                                        field_name = choice.name.lower()
                                        # Find field offset
                                        if field_name in ada_type.components:
                                            comp = ada_type.components[field_name]
                                            offset = (comp.offset_bits + 7) // 8
                                            # Update the field
                                            new_val = self._lower_expr(assoc.value)
                                            field_addr = self.builder.new_vreg(IRType.PTR, "_field")
                                            self.builder.add(field_addr, result, Immediate(offset, IRType.WORD))
                                            self.builder.store(field_addr, new_val)

                return result

            elif ada_type and isinstance(ada_type, ArrayType):
                # Array update
                element_size = 2  # Default word
                if ada_type.component_type:
                    element_size = (ada_type.component_type.size_bits + 7) // 8

                # Calculate array size
                num_elements = 1
                if ada_type.bounds:
                    for low, high in ada_type.bounds:
                        num_elements *= (high - low + 1)
                size_bytes = num_elements * element_size

                result = self.builder.new_vreg(IRType.PTR, "_update_arr")

                # Copy original array
                self.builder.push(Immediate(size_bytes, IRType.WORD))
                self.builder.push(prefix_value)
                self.builder.push(result)
                self.builder.call(Label("_memcpy"), comment="copy array for 'Update")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.pop(temp)

                # Apply index updates
                for arg in expr.args:
                    if isinstance(arg, Aggregate):
                        for assoc in getattr(arg, 'associations', []):
                            if hasattr(assoc, 'choices') and hasattr(assoc, 'value'):
                                for choice in assoc.choices:
                                    idx = self._lower_expr(choice)
                                    new_val = self._lower_expr(assoc.value)
                                    # Calculate offset
                                    offset = self.builder.new_vreg(IRType.WORD, "_idx_off")
                                    self.builder.mul(offset, idx, Immediate(element_size, IRType.WORD))
                                    elem_addr = self.builder.new_vreg(IRType.PTR, "_elem")
                                    self.builder.add(elem_addr, result, offset)
                                    self.builder.store(elem_addr, new_val)

                return result

            # Fallback: just return the prefix
            return prefix_value

        if attr == "valid":
            # X'Valid - returns True if X has a valid representation
            # For scalar types, check if value is in range
            prefix_value = self._lower_expr(expr.prefix)
            result = self.builder.new_vreg(IRType.WORD, "_valid")

            # Get type information from prefix
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type

            if ada_type:
                # Check if value is within type's valid range
                low_bound = None
                high_bound = None

                if hasattr(ada_type, "low"):
                    low_bound = ada_type.low
                if hasattr(ada_type, "high"):
                    high_bound = ada_type.high
                if isinstance(ada_type, EnumerationType) and ada_type.literals:
                    low_bound = 0
                    high_bound = len(ada_type.literals) - 1

                if low_bound is not None and high_bound is not None:
                    # result := (value >= low) AND (value <= high)
                    # Use short-circuit evaluation
                    valid_label = self._new_label("valid_ok")
                    invalid_label = self._new_label("valid_fail")
                    end_label = self._new_label("valid_end")

                    # Check low bound
                    cmp_low = self.builder.new_vreg(IRType.BOOL, "_cmp_low")
                    self.builder.cmp_lt(cmp_low, prefix_value, Immediate(low_bound, IRType.WORD))
                    self.builder.jnz(cmp_low, Label(invalid_label))

                    # Check high bound
                    cmp_high = self.builder.new_vreg(IRType.BOOL, "_cmp_high")
                    self.builder.cmp_gt(cmp_high, prefix_value, Immediate(high_bound, IRType.WORD))
                    self.builder.jnz(cmp_high, Label(invalid_label))

                    # Valid
                    self.builder.label(valid_label)
                    self.builder.mov(result, Immediate(1, IRType.WORD))
                    self.builder.jmp(Label(end_label))

                    # Invalid
                    self.builder.label(invalid_label)
                    self.builder.mov(result, Immediate(0, IRType.WORD))

                    # End
                    self.builder.label(end_label)
                    return result

            # Default: always valid
            self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "constrained":
            # X'Constrained - returns True if X is constrained (for discriminated records)
            # For most objects, this is a static True
            result = self.builder.new_vreg(IRType.WORD, "_constrained")
            self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "callable":
            # T'Callable - returns True if task T is callable
            # Used in tasking - for Z80 single-threaded, always True
            result = self.builder.new_vreg(IRType.WORD, "_callable")
            self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "terminated":
            # T'Terminated - returns True if task T has terminated
            # Used in tasking - for Z80 single-threaded, always False
            result = self.builder.new_vreg(IRType.WORD, "_terminated")
            self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "identity":
            # T'Identity - returns the unique identity of task T
            # For Z80 single-threaded, return a fixed value (main task = 0)
            result = self.builder.new_vreg(IRType.WORD, "_identity")
            self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "count":
            # E'Count - returns number of calls queued on entry E
            # For Z80 single-threaded tasking, always 0
            result = self.builder.new_vreg(IRType.WORD, "_count")
            self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "storage_size":
            # T'Storage_Size - storage allocated for task type T
            # For Z80, return stack size (default 256 bytes)
            result = self.builder.new_vreg(IRType.WORD, "_storage_size")
            self.builder.mov(result, Immediate(256, IRType.WORD))
            return result

        if attr == "bit_order":
            # S'Bit_Order - returns the bit ordering of type S
            # For Z80 (little-endian), return Low_Order_First (0)
            result = self.builder.new_vreg(IRType.WORD, "_bit_order")
            self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "machine_radix":
            # S'Machine_Radix - returns the radix of type S
            # For Z80 binary machine, return 2
            result = self.builder.new_vreg(IRType.WORD, "_machine_radix")
            self.builder.mov(result, Immediate(2, IRType.WORD))
            return result

        if attr == "machine_mantissa":
            # S'Machine_Mantissa - returns mantissa digits for floating type
            # Not applicable for Z80 without FPU, return 0
            result = self.builder.new_vreg(IRType.WORD, "_machine_mantissa")
            self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "digits":
            # S'Digits - returns decimal digits for floating/fixed type
            # For fixed-point on Z80, return 4 (16-bit fractional)
            result = self.builder.new_vreg(IRType.WORD, "_digits")
            self.builder.mov(result, Immediate(4, IRType.WORD))
            return result

        if attr == "delta":
            # S'Delta - returns delta for fixed-point type
            # Not directly supported, return 1
            result = self.builder.new_vreg(IRType.WORD, "_delta")
            self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "base":
            # T'Base - returns the base type
            # For code generation, just return the prefix value
            return self._lower_expr(expr.prefix)

        if attr == "component_size":
            # A'Component_Size - size in bits of array components
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type and isinstance(sym.ada_type, ArrayType):
                    if sym.ada_type.component_type:
                        return Immediate(sym.ada_type.component_type.size_bits, IRType.WORD)
            return Immediate(16, IRType.WORD)  # Default word size

        if attr == "alignment":
            # T'Alignment - alignment requirement
            # For Z80, alignment is typically 1 (byte-aligned)
            return Immediate(1, IRType.WORD)

        if attr == "object_size":
            # X'Object_Size - size in bits of object X
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    return Immediate(sym.ada_type.size_bits, IRType.WORD)
            return Immediate(16, IRType.WORD)

        if attr == "value_size":
            # T'Value_Size - minimum bits to represent values of type T
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    return Immediate(sym.ada_type.size_bits, IRType.WORD)
            return Immediate(16, IRType.WORD)

        if attr == "old":
            # X'Old - value of X at subprogram entry (for postconditions)
            # This requires saving values at entry; for now, we look up
            # the saved value from a special prefix
            #
            # The semantic analyzer should have created a copy at entry.
            # We look for it in the locals with the prefix "_old_"
            if isinstance(expr.prefix, Identifier):
                old_name = f"_old_{expr.prefix.name.lower()}"
                if self.ctx and old_name in self.ctx.locals:
                    return self.ctx.locals[old_name].vreg

            # Fallback: just evaluate the current value
            # (This is incorrect but prevents crashes for unhandled cases)
            return self._lower_expr(expr.prefix)

        if attr == "result":
            # F'Result - the result of function F (for postconditions)
            # This should be the return value being computed
            # In our calling convention, the result is in HL register
            result = self.builder.new_vreg(IRType.WORD, "_func_result")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="F'Result - function return value"
            ))
            return result

        if attr == "loop_entry":
            # X'Loop_Entry - value of X at loop entry (for loop invariants)
            # Similar to 'Old but for loop iterations
            if isinstance(expr.prefix, Identifier):
                entry_name = f"_loop_entry_{expr.prefix.name.lower()}"
                if self.ctx and entry_name in self.ctx.locals:
                    return self.ctx.locals[entry_name].vreg

            # Fallback: just evaluate the current value
            return self._lower_expr(expr.prefix)

        if attr == "reduce":
            # Container'Reduce(Reducer, Initial) - Ada 2022 reduction expression
            # Applies Reducer to each element with accumulator starting at Initial
            #
            # For arrays: iterate and apply reducer
            # Result = Initial
            # for E in Container loop
            #     Result := Reducer(Result, E);
            # end loop;
            result = self.builder.new_vreg(IRType.WORD, "_reduce_result")

            if len(expr.args) >= 2:
                reducer = expr.args[0]  # The reducer function/operator
                initial = expr.args[1]  # Initial value

                # Initialize result with initial value
                init_val = self._lower_expr(initial)
                self.builder.mov(result, init_val)

                # Get array info from prefix
                ada_type = None
                if isinstance(expr.prefix, Identifier):
                    sym = self.symbols.lookup(expr.prefix.name)
                    if sym and sym.ada_type:
                        ada_type = sym.ada_type

                if ada_type and isinstance(ada_type, ArrayType) and ada_type.bounds:
                    low, high = ada_type.bounds[0]
                    element_size = 2
                    if ada_type.component_type:
                        element_size = (ada_type.component_type.size_bits + 7) // 8

                    # Get base address
                    base_addr = self._lower_expr(expr.prefix)

                    # Loop variable
                    loop_idx = self.builder.new_vreg(IRType.WORD, "_reduce_idx")
                    self.builder.mov(loop_idx, Immediate(low, IRType.WORD))

                    # Loop labels
                    loop_start = self._new_label("reduce_loop")
                    loop_end = self._new_label("reduce_end")

                    # Loop start
                    self.builder.label(loop_start)

                    # Check loop condition
                    cmp_result = self.builder.new_vreg(IRType.BOOL, "_reduce_cmp")
                    self.builder.cmp_gt(cmp_result, loop_idx, Immediate(high, IRType.WORD))
                    self.builder.jnz(cmp_result, Label(loop_end))

                    # Get current element
                    offset = self.builder.new_vreg(IRType.WORD, "_reduce_off")
                    temp = self.builder.new_vreg(IRType.WORD, "_reduce_temp")
                    self.builder.sub(temp, loop_idx, Immediate(low, IRType.WORD))
                    self.builder.mul(offset, temp, Immediate(element_size, IRType.WORD))

                    elem_addr = self.builder.new_vreg(IRType.PTR, "_reduce_elem_addr")
                    self.builder.add(elem_addr, base_addr, offset)

                    elem_val = self.builder.new_vreg(IRType.WORD, "_reduce_elem")
                    self.builder.load(elem_val, elem_addr)

                    # Apply reducer (if it's a binary operator like "+")
                    if isinstance(reducer, Identifier):
                        op_name = reducer.name.lower()
                        if op_name == '"+"' or op_name == '+':
                            self.builder.add(result, result, elem_val)
                        elif op_name == '"-"' or op_name == '-':
                            self.builder.sub(result, result, elem_val)
                        elif op_name == '"*"' or op_name == '*':
                            self.builder.mul(result, result, elem_val)
                        elif op_name == '"and"' or op_name == 'and':
                            self.builder.and_(result, result, elem_val)
                        elif op_name == '"or"' or op_name == 'or':
                            self.builder.or_(result, result, elem_val)
                        else:
                            # Call reducer as function
                            self.builder.push(elem_val)
                            self.builder.push(result)
                            self.builder.call(Label(op_name))
                            temp2 = self.builder.new_vreg(IRType.WORD, "_discard")
                            self.builder.pop(temp2)
                            self.builder.pop(temp2)
                            self.builder.emit(IRInstr(
                                OpCode.MOV, result,
                                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                                comment="capture reducer result"
                            ))

                    # Increment loop index
                    inc_temp = self.builder.new_vreg(IRType.WORD, "_reduce_inc")
                    self.builder.add(inc_temp, loop_idx, Immediate(1, IRType.WORD))
                    self.builder.mov(loop_idx, inc_temp)
                    self.builder.jmp(Label(loop_start))

                    # Loop end
                    self.builder.label(loop_end)

            return result

        if attr == "initialized":
            # X'Initialized - True if X has been initialized
            # For Z80, we always return True (no runtime tracking)
            result = self.builder.new_vreg(IRType.WORD, "_initialized")
            self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "copy_sign":
            # T'Copy_Sign(Value, Sign) - copy sign from Sign to Value
            # For integers: if Sign < 0 then -abs(Value) else abs(Value)
            if len(expr.args) >= 2:
                value = self._lower_expr(expr.args[0])
                sign = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_copy_sign")

                # Get abs(value)
                abs_val = self.builder.new_vreg(IRType.WORD, "_abs_val")
                cmp1 = self.builder.new_vreg(IRType.BOOL, "_sign_cmp1")
                self.builder.cmp_lt(cmp1, value, Immediate(0, IRType.WORD))
                pos_label = self._new_label("copysign_pos")
                end_label = self._new_label("copysign_end")
                self.builder.jz(cmp1, Label(pos_label))
                self.builder.neg(abs_val, value)
                self.builder.jmp(Label(end_label))
                self.builder.label(pos_label)
                self.builder.mov(abs_val, value)
                self.builder.label(end_label)

                # Apply sign
                cmp2 = self.builder.new_vreg(IRType.BOOL, "_sign_cmp2")
                self.builder.cmp_lt(cmp2, sign, Immediate(0, IRType.WORD))
                neg_label = self._new_label("copysign_neg")
                done_label = self._new_label("copysign_done")
                self.builder.jz(cmp2, Label(neg_label))
                self.builder.neg(result, abs_val)
                self.builder.jmp(Label(done_label))
                self.builder.label(neg_label)
                self.builder.mov(result, abs_val)
                self.builder.label(done_label)

                return result
            return Immediate(0, IRType.WORD)

        if attr == "adjacent":
            # T'Adjacent(X, Towards) - value adjacent to X in direction of Towards
            # For integers: if Towards > X then X + 1 else if Towards < X then X - 1 else X
            if len(expr.args) >= 2:
                x = self._lower_expr(expr.args[0])
                towards = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_adjacent")

                cmp_gt = self.builder.new_vreg(IRType.BOOL, "_adj_cmp_gt")
                self.builder.cmp_gt(cmp_gt, towards, x)
                inc_label = self._new_label("adj_inc")
                check_lt_label = self._new_label("adj_check_lt")
                end_label = self._new_label("adj_end")

                self.builder.jz(cmp_gt, Label(check_lt_label))
                # Towards > X: increment
                self.builder.add(result, x, Immediate(1, IRType.WORD))
                self.builder.jmp(Label(end_label))

                self.builder.label(check_lt_label)
                cmp_lt = self.builder.new_vreg(IRType.BOOL, "_adj_cmp_lt")
                self.builder.cmp_lt(cmp_lt, towards, x)
                equal_label = self._new_label("adj_equal")
                self.builder.jz(cmp_lt, Label(equal_label))
                # Towards < X: decrement
                self.builder.sub(result, x, Immediate(1, IRType.WORD))
                self.builder.jmp(Label(end_label))

                self.builder.label(equal_label)
                # Equal: no change
                self.builder.mov(result, x)

                self.builder.label(end_label)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "unrestricted_access":
            # X'Unrestricted_Access - like 'Access but without accessibility checks
            # Same implementation as 'Access for Z80
            prefix_val = self._lower_expr(expr.prefix)
            result = self.builder.new_vreg(IRType.PTR, "_unrestricted_access")

            if isinstance(expr.prefix, Identifier):
                name = expr.prefix.name.lower()
                # Check if it's a variable (get address) or subprogram (get label)
                sym = self.symbols.lookup(name)
                if sym and sym.kind == "variable":
                    self.builder.lea(result, prefix_val)
                elif sym and sym.kind in ("function", "procedure"):
                    self.builder.emit(IRInstr(
                        OpCode.LEA, result, Label(name),
                        comment=f"subprogram address '{name}'"
                    ))
                else:
                    self.builder.lea(result, prefix_val)
            else:
                self.builder.lea(result, prefix_val)

            return result

        if attr == "ceiling":
            # T'Ceiling(X) - smallest integer >= X
            if expr.args:
                arg_type = self._get_expr_type(expr.args[0])
                if self._is_float64_type(arg_type):
                    # Float64: call _f64_ceil
                    arg_ptr = self._lower_float64_operand(expr.args[0])
                    return self._lower_float64_math_attr("_f64_ceil", arg_ptr, arg_type)
                x = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_ceiling")
                self.builder.mov(result, x)
                return result
            return self._lower_expr(expr.prefix)

        if attr == "floor":
            # T'Floor(X) - largest integer <= X
            if expr.args:
                arg_type = self._get_expr_type(expr.args[0])
                if self._is_float64_type(arg_type):
                    # Float64: call _f64_floor
                    arg_ptr = self._lower_float64_operand(expr.args[0])
                    return self._lower_float64_math_attr("_f64_floor", arg_ptr, arg_type)
                x = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_floor")
                self.builder.mov(result, x)
                return result
            return self._lower_expr(expr.prefix)

        if attr == "rounding":
            # T'Rounding(X) - round to nearest integer (half away from zero)
            if expr.args:
                arg_type = self._get_expr_type(expr.args[0])
                if self._is_float64_type(arg_type):
                    # Float64: call _f64_round
                    arg_ptr = self._lower_float64_operand(expr.args[0])
                    return self._lower_float64_math_attr("_f64_round", arg_ptr, arg_type)
                x = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_rounding")
                self.builder.mov(result, x)
                return result
            return self._lower_expr(expr.prefix)

        if attr == "truncation":
            # T'Truncation(X) - truncate toward zero
            if expr.args:
                arg_type = self._get_expr_type(expr.args[0])
                if self._is_float64_type(arg_type):
                    # Float64: call _f64_trunc
                    arg_ptr = self._lower_float64_operand(expr.args[0])
                    return self._lower_float64_math_attr("_f64_trunc", arg_ptr, arg_type)
                x = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_truncation")
                self.builder.mov(result, x)
                return result
            return self._lower_expr(expr.prefix)

        if attr == "remainder":
            # T'Remainder(X, Y) - IEEE remainder
            # For integers: X mod Y with sign of X
            if len(expr.args) >= 2:
                x = self._lower_expr(expr.args[0])
                y = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_remainder")
                self.builder.rem(result, x, y)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "machine":
            # T'Machine(X) - machine representation of X
            # For integers on Z80, this is just the value
            if expr.args:
                return self._lower_expr(expr.args[0])
            return self._lower_expr(expr.prefix)

        if attr == "model":
            # T'Model(X) - model number closest to X
            # For integers on Z80, this is just the value
            if expr.args:
                return self._lower_expr(expr.args[0])
            return self._lower_expr(expr.prefix)

        if attr == "compose":
            # T'Compose(Fraction, Exponent) - compose floating-point value
            # Not fully applicable to integers, return a scaled value
            if len(expr.args) >= 2:
                fraction = self._lower_expr(expr.args[0])
                exponent = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_compose")
                # Simplified: result = fraction * 2^exponent (via shift)
                self.builder.shl(result, fraction, exponent)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "scaling":
            # T'Scaling(X, Adjustment) - scale by power of radix
            # For integers: X * 2^Adjustment
            if len(expr.args) >= 2:
                x = self._lower_expr(expr.args[0])
                adj = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_scaling")
                self.builder.shl(result, x, adj)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "exponent":
            # T'Exponent(X) - exponent of X in canonical form
            # For integers: find position of highest set bit
            # Simplified implementation - just return a fixed value
            result = self.builder.new_vreg(IRType.WORD, "_exponent")
            self.builder.mov(result, Immediate(16, IRType.WORD))  # Assume 16-bit
            return result

        if attr == "fraction":
            # T'Fraction(X) - fractional part of X
            # For integers: 0 (no fractional part)
            result = self.builder.new_vreg(IRType.WORD, "_fraction")
            self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "leading_part":
            # T'Leading_Part(X, Radix_Digits) - leading digits of X
            # Simplified: return X itself for integers
            if expr.args:
                return self._lower_expr(expr.args[0])
            return self._lower_expr(expr.prefix)

        if attr == "machine_rounding":
            # T'Machine_Rounding(X) - round according to machine mode
            # For integers, same as value
            if expr.args:
                return self._lower_expr(expr.args[0])
            return self._lower_expr(expr.prefix)

        if attr == "unbiased_rounding":
            # T'Unbiased_Rounding(X) - round to even (banker's rounding)
            # For integers, same as value
            if expr.args:
                return self._lower_expr(expr.args[0])
            return self._lower_expr(expr.prefix)

        if attr == "enum_rep":
            # T'Enum_Rep(X) - internal representation of enumeration value
            # For our compiler, enum values are already represented as integers
            if expr.args:
                return self._lower_expr(expr.args[0])
            return self._lower_expr(expr.prefix)

        if attr == "enum_val":
            # T'Enum_Val(X) - enumeration value from representation
            # Inverse of Enum_Rep - for us, identity function
            if expr.args:
                return self._lower_expr(expr.args[0])
            return self._lower_expr(expr.prefix)

        if attr == "wide_image" or attr == "wide_value":
            # Wide string versions - for Z80, same as regular Image/Value
            # since we don't support Unicode
            if attr == "wide_image":
                if expr.args:
                    val = self._lower_expr(expr.args[0])
                else:
                    val = self._lower_expr(expr.prefix)
                self.builder.push(val)
                self.builder.call(Label("_int_to_str"), comment="Wide_Image")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                result = self.builder.new_vreg(IRType.PTR, "_wide_image")
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture Wide_Image result"
                ))
                return result
            else:  # wide_value
                if expr.args:
                    str_val = self._lower_expr(expr.args[0])
                else:
                    str_val = self._lower_expr(expr.prefix)
                self.builder.push(str_val)
                self.builder.call(Label("_str_to_int"), comment="Wide_Value")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                result = self.builder.new_vreg(IRType.WORD, "_wide_value")
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture Wide_Value result"
                ))
                return result

        if attr == "overlaps_storage":
            # X'Overlaps_Storage(Y) - True if X and Y share any storage
            # Compare address ranges for overlap
            if expr.args:
                x_addr = self._lower_expr(expr.prefix)
                y_addr = self._lower_expr(expr.args[0])

                # Get sizes (default to 2 bytes)
                x_size = 2
                y_size = 2
                if isinstance(expr.prefix, Identifier):
                    sym = self.symbols.lookup(expr.prefix.name)
                    if sym and sym.ada_type and hasattr(sym.ada_type, 'size_bits'):
                        x_size = (sym.ada_type.size_bits + 7) // 8
                if isinstance(expr.args[0], Identifier):
                    sym = self.symbols.lookup(expr.args[0].name)
                    if sym and sym.ada_type and hasattr(sym.ada_type, 'size_bits'):
                        y_size = (sym.ada_type.size_bits + 7) // 8

                result = self.builder.new_vreg(IRType.WORD, "_overlaps")

                # Overlaps if NOT (x_end <= y_start OR y_end <= x_start)
                # Simplified: compare addresses
                x_end = self.builder.new_vreg(IRType.PTR, "_x_end")
                y_end = self.builder.new_vreg(IRType.PTR, "_y_end")
                self.builder.add(x_end, x_addr, Immediate(x_size, IRType.WORD))
                self.builder.add(y_end, y_addr, Immediate(y_size, IRType.WORD))

                # Check x_end <= y_start
                cmp1 = self.builder.new_vreg(IRType.BOOL, "_cmp1")
                self.builder.cmp_le(cmp1, x_end, y_addr)

                # Check y_end <= x_start
                cmp2 = self.builder.new_vreg(IRType.BOOL, "_cmp2")
                self.builder.cmp_le(cmp2, y_end, x_addr)

                # No overlap if either is true
                no_overlap = self.builder.new_vreg(IRType.BOOL, "_no_overlap")
                self.builder.or_(no_overlap, cmp1, cmp2)

                # Result is NOT no_overlap
                self.builder.not_(result, no_overlap)
                return result

            return Immediate(0, IRType.WORD)

        if attr == "has_same_storage":
            # X'Has_Same_Storage(Y) - True if X and Y occupy exactly the same storage
            # Compare addresses and sizes
            if expr.args:
                x_addr = self._lower_expr(expr.prefix)
                y_addr = self._lower_expr(expr.args[0])

                # Get sizes
                x_size = 2
                y_size = 2
                if isinstance(expr.prefix, Identifier):
                    sym = self.symbols.lookup(expr.prefix.name)
                    if sym and sym.ada_type and hasattr(sym.ada_type, 'size_bits'):
                        x_size = (sym.ada_type.size_bits + 7) // 8
                if isinstance(expr.args[0], Identifier):
                    sym = self.symbols.lookup(expr.args[0].name)
                    if sym and sym.ada_type and hasattr(sym.ada_type, 'size_bits'):
                        y_size = (sym.ada_type.size_bits + 7) // 8

                result = self.builder.new_vreg(IRType.WORD, "_same_storage")

                # Same storage if addresses equal and sizes equal
                addr_eq = self.builder.new_vreg(IRType.BOOL, "_addr_eq")
                self.builder.cmp_eq(addr_eq, x_addr, y_addr)

                if x_size == y_size:
                    self.builder.mov(result, addr_eq)
                else:
                    # Different sizes, cannot be same storage
                    self.builder.mov(result, Immediate(0, IRType.WORD))

                return result

            return Immediate(0, IRType.WORD)

        if attr == "valid_scalars":
            # X'Valid_Scalars - True if all scalar components of X are valid
            # For simple scalars, same as 'Valid
            # For aggregates, would need to check each component
            result = self.builder.new_vreg(IRType.WORD, "_valid_scalars")
            self.builder.mov(result, Immediate(1, IRType.WORD))  # Assume valid
            return result

        if attr == "descriptor_size":
            # T'Descriptor_Size - size of fat pointer descriptor
            # For unconstrained arrays, this includes bounds info
            # Default: 2 bytes for pointer + 4 bytes for bounds (1D array)
            return Immediate(6, IRType.WORD)

        if attr == "default_bit_order":
            # System'Default_Bit_Order - default bit ordering
            # Z80 is little-endian: Low_Order_First = 0
            return Immediate(0, IRType.WORD)

        if attr == "storage_unit":
            # System'Storage_Unit - bits per storage unit
            # Z80: 8 bits per byte
            return Immediate(8, IRType.WORD)

        if attr == "word_size":
            # System'Word_Size - bits per machine word
            # Z80: 16-bit words (even though it's an 8-bit CPU)
            return Immediate(16, IRType.WORD)

        if attr == "max_int" or attr == "max_integer":
            # System'Max_Int - largest integer value
            return Immediate(32767, IRType.WORD)

        if attr == "min_int" or attr == "min_integer":
            # System'Min_Int - smallest integer value
            return Immediate(-32768, IRType.WORD)

        if attr == "tick":
            # System'Tick - clock tick duration (in seconds)
            # For Z80/CP/M, this is system-dependent
            # Return a small positive value (1/100 second = 10ms)
            return Immediate(10, IRType.WORD)  # milliseconds

        if attr == "target_name":
            # System'Target_Name - target platform name
            # Return pointer to string "Z80-CPM"
            result = self.builder.new_vreg(IRType.PTR, "_target_name")
            # Use a string constant (would need to be in data section)
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_target_name_str"),
                comment="System'Target_Name"
            ))
            return result

        if attr == "compiler_version":
            # GNAT specific but useful - return version string pointer
            result = self.builder.new_vreg(IRType.PTR, "_compiler_version")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_compiler_version_str"),
                comment="Compiler_Version"
            ))
            return result

        if attr == "null_parameter":
            # Used in imported subprograms - returns null pointer
            return Immediate(0, IRType.WORD)

        if attr == "passed_by_reference":
            # T'Passed_By_Reference - True if type is passed by reference
            # For Z80: arrays, records > 2 bytes, tagged types
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type

            if ada_type:
                if isinstance(ada_type, ArrayType):
                    return Immediate(1, IRType.WORD)
                if isinstance(ada_type, RecordType):
                    size = (ada_type.size_bits + 7) // 8 if ada_type.size_bits else 4
                    return Immediate(1 if size > 2 else 0, IRType.WORD)
            return Immediate(0, IRType.WORD)

        if attr == "type_class":
            # T'Type_Class - returns enumeration indicating type classification
            # Values: 0=Integer, 1=Boolean, 2=Character, 3=Enumeration,
            #         4=Float, 5=Fixed, 6=Array, 7=Record, 8=Access, 9=Task, etc.
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type

            if ada_type:
                from uada80.type_system import IntegerType
                # Boolean and Character are EnumerationType with specific names
                if isinstance(ada_type, EnumerationType):
                    type_name = getattr(ada_type, 'name', '').lower()
                    if type_name == 'boolean':
                        return Immediate(1, IRType.WORD)
                    if type_name == 'character':
                        return Immediate(2, IRType.WORD)
                    return Immediate(3, IRType.WORD)  # Other enumeration
                if isinstance(ada_type, ArrayType):
                    return Immediate(6, IRType.WORD)
                if isinstance(ada_type, RecordType):
                    return Immediate(7, IRType.WORD)
                if isinstance(ada_type, AccessType):
                    return Immediate(8, IRType.WORD)
                if isinstance(ada_type, IntegerType):
                    return Immediate(0, IRType.WORD)
            return Immediate(0, IRType.WORD)  # Default to Integer

        if attr == "put_image":
            # T'Put_Image(Buffer, Item) - Ada 2022 custom image output
            # Calls the Put_Image procedure associated with the type
            if len(expr.args) >= 2:
                buffer = self._lower_expr(expr.args[0])
                item = self._lower_expr(expr.args[1])
                # Call runtime Put_Image procedure
                self.builder.push(item)
                self.builder.push(buffer)
                self.builder.call(Label("_put_image"), comment="T'Put_Image")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
            return Immediate(0, IRType.WORD)

        if attr == "max_size_in_storage_elements":
            # T'Max_Size_In_Storage_Elements - max size in storage elements
            # For Z80: size in bytes (storage element = 1 byte)
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type
            if ada_type and hasattr(ada_type, 'size_bits'):
                return Immediate((ada_type.size_bits + 7) // 8, IRType.WORD)
            return Immediate(2, IRType.WORD)

        if attr == "index":
            # T'Index(Iterator) - Ada 2012 iterator index
            # Returns current index in generalized iteration
            if expr.args:
                return self._lower_expr(expr.args[0])
            return Immediate(0, IRType.WORD)

        if attr == "finalization_size":
            # T'Finalization_Size - size needed for finalization
            # For Z80 with no finalization, return 0
            return Immediate(0, IRType.WORD)

        if attr == "definite":
            # T'Definite - True if type is definite (not indefinite)
            # Most types on Z80 are definite (unconstrained arrays are indefinite)
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type
            if ada_type and isinstance(ada_type, ArrayType):
                return Immediate(1 if ada_type.is_constrained else 0, IRType.WORD)
            return Immediate(1, IRType.WORD)

        if attr == "has_discriminants":
            # T'Has_Discriminants - True if type has discriminants
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type
            if ada_type and isinstance(ada_type, RecordType):
                return Immediate(1 if ada_type.discriminants else 0, IRType.WORD)
            return Immediate(0, IRType.WORD)

        if attr == "preelaborable_initialization":
            # T'Preelaborable_Initialization - True if type has preelaborable init
            # For simple types on Z80, return True
            return Immediate(1, IRType.WORD)

        if attr == "denorm_min":
            # T'Denorm_Min - smallest denormalized value
            # Not applicable to integers, return 0
            return Immediate(0, IRType.WORD)

        if attr == "safe_first" or attr == "safe_last":
            # T'Safe_First / T'Safe_Last - safe range bounds
            # For integers, same as First/Last
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type
            if ada_type and hasattr(ada_type, 'low') and hasattr(ada_type, 'high'):
                if attr == "safe_first":
                    return Immediate(ada_type.low, IRType.WORD)
                else:
                    return Immediate(ada_type.high, IRType.WORD)
            return Immediate(-32768 if attr == "safe_first" else 32767, IRType.WORD)

        if attr == "model_small":
            # T'Model_Small - smallest model number > 0
            # For integers, return 1
            return Immediate(1, IRType.WORD)

        if attr == "model_epsilon":
            # T'Model_Epsilon - model epsilon (spacing at 1.0)
            # For integers, return 1
            return Immediate(1, IRType.WORD)

        if attr == "model_emin":
            # T'Model_Emin - minimum model exponent
            # For 16-bit integers, log2(1) = 0
            return Immediate(0, IRType.WORD)

        if attr == "model_mantissa":
            # T'Model_Mantissa - model mantissa digits
            # For 16-bit signed integers, ~15 bits of precision
            return Immediate(15, IRType.WORD)

        if attr == "small":
            # T'Small - smallest positive value
            # For integers and fixed-point
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type
            if ada_type and hasattr(ada_type, 'small'):
                return Immediate(ada_type.small, IRType.WORD)
            return Immediate(1, IRType.WORD)

        if attr == "signed_zeros":
            # T'Signed_Zeros - True if type has signed zeros
            # Not applicable to integers
            return Immediate(0, IRType.WORD)

        if attr == "stream_size":
            # T'Stream_Size - size for streaming in bits
            ada_type = None
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    ada_type = sym.ada_type
            if ada_type and hasattr(ada_type, 'size_bits'):
                return Immediate(ada_type.size_bits, IRType.WORD)
            return Immediate(16, IRType.WORD)

        if attr == "write":
            # T'Write(Stream, Item) - Write Item to Stream
            # Stream is treated as a file handle on Z80/CP/M
            if len(expr.args) >= 2:
                stream = self._lower_expr(expr.args[0])
                item_arg = expr.args[1]
                # Get address of item
                item_addr = self._get_lvalue_address(item_arg)
                if item_addr is None:
                    # For expressions, evaluate and store to temp
                    item_val = self._lower_expr(item_arg)
                    self.builder.push(item_val)
                    item_addr = self.builder.new_vreg(IRType.PTR, "_item_addr")
                    self.builder.emit(IRInstr(OpCode.GETSP, dst=item_addr))
                # Get item size
                item_size = self._get_type_size(item_arg)
                # Call file write (stream = file handle)
                self.builder.push(Immediate(item_size, IRType.WORD))
                self.builder.push(item_addr)
                self.builder.push(stream)
                self.builder.call(Label("_file_write"), comment="T'Write")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
            return Immediate(0, IRType.WORD)

        if attr == "read":
            # T'Read(Stream, Item) - Read Item from Stream
            # Stream is treated as a file handle on Z80/CP/M
            if len(expr.args) >= 2:
                stream = self._lower_expr(expr.args[0])
                item_arg = expr.args[1]
                # Get address of item for writing result
                item_addr = self._get_lvalue_address(item_arg)
                if item_addr is None:
                    item_addr = self._lower_expr(item_arg)
                # Get item size
                item_size = self._get_type_size(item_arg)
                # Call file read (stream = file handle)
                self.builder.push(Immediate(item_size, IRType.WORD))
                self.builder.push(item_addr)
                self.builder.push(stream)
                self.builder.call(Label("_file_read"), comment="T'Read")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
            return Immediate(0, IRType.WORD)

        if attr == "input":
            # T'Input(Stream) - Read and return value from Stream
            # For Z80, read a word (2 bytes) from the stream/file
            result = self.builder.new_vreg(IRType.WORD, "_stream_input")
            if expr.args:
                stream = self._lower_expr(expr.args[0])
                # Allocate temp buffer on stack
                self.builder.push(Immediate(0, IRType.WORD))  # Reserve space
                temp_buf = self.builder.new_vreg(IRType.PTR, "_buf")
                self.builder.emit(IRInstr(OpCode.GETSP, dst=temp_buf))
                # Read 2 bytes
                self.builder.push(Immediate(2, IRType.WORD))
                self.builder.push(temp_buf)
                self.builder.push(stream)
                self.builder.call(Label("_file_read"), comment="T'Input")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                # Pop result from stack
                self.builder.pop(result)
            return result

        if attr == "output":
            # T'Output(Stream, Item) - Write Item with tag to Stream
            # For Z80, this is similar to T'Write but includes type tag
            if len(expr.args) >= 2:
                stream = self._lower_expr(expr.args[0])
                item_arg = expr.args[1]
                # Get address of item
                item_addr = self._get_lvalue_address(item_arg)
                if item_addr is None:
                    item_val = self._lower_expr(item_arg)
                    self.builder.push(item_val)
                    item_addr = self.builder.new_vreg(IRType.PTR, "_item_addr")
                    self.builder.emit(IRInstr(OpCode.GETSP, dst=item_addr))
                # Get item size
                item_size = self._get_type_size(item_arg)
                # Write item to stream (stream = file handle)
                self.builder.push(Immediate(item_size, IRType.WORD))
                self.builder.push(item_addr)
                self.builder.push(stream)
                self.builder.call(Label("_file_write"), comment="T'Output")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
            return Immediate(0, IRType.WORD)

        if attr == "external_tag":
            # T'External_Tag - Get external tag string for tagged type
            result = self.builder.new_vreg(IRType.PTR, "_ext_tag")
            if isinstance(expr.prefix, Identifier):
                # Return pointer to type name string
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_tag_{expr.prefix.name.lower()}"),
                    comment=f"T'External_Tag for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "internal_tag":
            # S'Internal_Tag - Get tag from external tag string
            result = self.builder.new_vreg(IRType.PTR, "_int_tag")
            if expr.args:
                ext_tag = self._lower_expr(expr.args[0])
                self.builder.push(ext_tag)
                self.builder.call(Label("_get_internal_tag"), comment="S'Internal_Tag")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture S'Internal_Tag result"
                ))
            return result

        if attr == "descendant_tag":
            # S'Descendant_Tag(Ext_Tag, Ancestor) - Get descendant tag
            result = self.builder.new_vreg(IRType.PTR, "_desc_tag")
            if len(expr.args) >= 2:
                ext_tag = self._lower_expr(expr.args[0])
                ancestor = self._lower_expr(expr.args[1])
                self.builder.push(ancestor)
                self.builder.push(ext_tag)
                self.builder.call(Label("_get_descendant_tag"), comment="S'Descendant_Tag")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture S'Descendant_Tag result"
                ))
            return result

        if attr == "is_abstract":
            # T'Is_Abstract - True if type is abstract
            # For Z80, return 0 (no abstract types at runtime)
            return Immediate(0, IRType.WORD)

        if attr == "parent_tag":
            # T'Parent_Tag - Get parent type's tag
            # For single inheritance, vtable has parent pointer at offset 0
            if isinstance(expr.prefix, Identifier):
                tag_val = self._lower_expr(expr.prefix)
                result = self.builder.new_vreg(IRType.PTR, "_parent_tag")
                self.builder.load(result, tag_val)  # Load parent ptr from vtable
                return result
            return Immediate(0, IRType.PTR)

        if attr == "interface_ancestor_tags":
            # T'Interface_Ancestor_Tags - array of interface tags
            # For Z80 without interfaces, return null
            return Immediate(0, IRType.PTR)

        if attr == "type_key":
            # T'Type_Key - unique type identifier string
            result = self.builder.new_vreg(IRType.PTR, "_type_key")
            if isinstance(expr.prefix, Identifier):
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_typekey_{expr.prefix.name.lower()}"),
                    comment=f"T'Type_Key for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        # Exception information attributes (Ada.Exceptions)
        if attr == "exception_name":
            # Exception_Name(X) - Get name string of exception occurrence
            result = self.builder.new_vreg(IRType.PTR, "_exc_name")
            if expr.args:
                occ = self._lower_expr(expr.args[0])
                self.builder.push(occ)
                self.builder.call(Label("_exc_get_name"), comment="Exception_Name")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture exception name"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "exception_message":
            # Exception_Message(X) - Get message string of exception occurrence
            result = self.builder.new_vreg(IRType.PTR, "_exc_msg")
            if expr.args:
                occ = self._lower_expr(expr.args[0])
                self.builder.push(occ)
                self.builder.call(Label("_exc_get_message"), comment="Exception_Message")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture exception message"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "exception_information":
            # Exception_Information(X) - Get full info string
            result = self.builder.new_vreg(IRType.PTR, "_exc_info")
            if expr.args:
                occ = self._lower_expr(expr.args[0])
                self.builder.push(occ)
                self.builder.call(Label("_exc_get_info"), comment="Exception_Information")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture exception information"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "exception_identity":
            # Exception_Identity(X) - Get exception identity
            result = self.builder.new_vreg(IRType.WORD, "_exc_identity")
            if expr.args:
                occ = self._lower_expr(expr.args[0])
                # Exception occurrence has identity at offset 0
                self.builder.load(result, occ)
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "wide_exception_name":
            # Wide_Exception_Name(X) - Wide string version
            # For Z80, same as Exception_Name (no Unicode)
            result = self.builder.new_vreg(IRType.PTR, "_wide_exc_name")
            if expr.args:
                occ = self._lower_expr(expr.args[0])
                self.builder.push(occ)
                self.builder.call(Label("_exc_get_name"), comment="Wide_Exception_Name")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture wide exception name"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        # Address-related attributes
        if attr == "code_address":
            # S'Code_Address - Address of subprogram's code
            result = self.builder.new_vreg(IRType.PTR, "_code_addr")
            if isinstance(expr.prefix, Identifier):
                self.builder.emit(IRInstr(
                    OpCode.LEA, result, Label(expr.prefix.name.lower()),
                    comment=f"S'Code_Address for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "pool_address":
            # X'Pool_Address - Address within storage pool
            result = self.builder.new_vreg(IRType.PTR, "_pool_addr")
            prefix_val = self._lower_expr(expr.prefix)
            # For simple allocations, pool address is same as address
            self.builder.mov(result, prefix_val)
            return result

        if attr == "body_version":
            # P'Body_Version - Version string of package body
            result = self.builder.new_vreg(IRType.PTR, "_body_ver")
            if isinstance(expr.prefix, Identifier):
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_bodyver_{expr.prefix.name.lower()}"),
                    comment=f"P'Body_Version for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "version":
            # P'Version - Version string of package spec
            result = self.builder.new_vreg(IRType.PTR, "_version")
            if isinstance(expr.prefix, Identifier):
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_version_{expr.prefix.name.lower()}"),
                    comment=f"P'Version for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        # Unchecked conversion attributes
        if attr == "unchecked_access":
            # X'Unchecked_Access - Access without accessibility check
            # Same as 'Unrestricted_Access for Z80
            prefix_val = self._lower_expr(expr.prefix)
            result = self.builder.new_vreg(IRType.PTR, "_unchecked_access")
            if isinstance(expr.prefix, Identifier):
                self.builder.lea(result, prefix_val)
            else:
                self.builder.mov(result, prefix_val)
            return result

        # Elaboration attributes
        if attr == "elaborated":
            # P'Elaborated - True if package has been elaborated
            # For static compilation, always True at runtime
            return Immediate(1, IRType.WORD)

        if attr == "partition_id":
            # P'Partition_Id - Partition identifier (for distributed systems)
            # Z80 is single partition, return 0
            return Immediate(0, IRType.WORD)

        # Storage pool attributes
        if attr == "storage_pool":
            # S'Storage_Pool - The storage pool for access type S
            result = self.builder.new_vreg(IRType.PTR, "_storage_pool")
            # Return default pool (heap base)
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_heap_base"),
                comment="S'Storage_Pool"
            ))
            return result

        if attr == "simple_storage_pool":
            # S'Simple_Storage_Pool - Simple storage pool for S
            result = self.builder.new_vreg(IRType.PTR, "_simple_pool")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_heap_base"),
                comment="S'Simple_Storage_Pool"
            ))
            return result

        # Calendar/Time attributes
        if attr == "clock":
            # Ada.Calendar.Clock - Current time
            result = self.builder.new_vreg(IRType.WORD, "_clock")
            self.builder.call(Label("_calendar_clock"), comment="Ada.Calendar.Clock")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture clock value"
            ))
            return result

        if attr == "year":
            # Time'Year - Extract year from time value
            result = self.builder.new_vreg(IRType.WORD, "_year")
            if expr.args:
                time_val = self._lower_expr(expr.args[0])
                self.builder.push(time_val)
                self.builder.call(Label("_time_year"), comment="Year")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture year"
                ))
            else:
                self.builder.mov(result, Immediate(1970, IRType.WORD))
            return result

        if attr == "month":
            # Time'Month - Extract month (1-12)
            result = self.builder.new_vreg(IRType.WORD, "_month")
            if expr.args:
                time_val = self._lower_expr(expr.args[0])
                self.builder.push(time_val)
                self.builder.call(Label("_time_month"), comment="Month")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture month"
                ))
            else:
                self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "day":
            # Time'Day - Extract day (1-31)
            result = self.builder.new_vreg(IRType.WORD, "_day")
            if expr.args:
                time_val = self._lower_expr(expr.args[0])
                self.builder.push(time_val)
                self.builder.call(Label("_time_day"), comment="Day")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture day"
                ))
            else:
                self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "seconds":
            # Time'Seconds - Seconds since midnight
            result = self.builder.new_vreg(IRType.WORD, "_seconds")
            if expr.args:
                time_val = self._lower_expr(expr.args[0])
                self.builder.push(time_val)
                self.builder.call(Label("_time_seconds"), comment="Seconds")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture seconds"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        # Machine code attributes (for System.Machine_Code)
        if attr == "asm_input":
            # Asm_Input("constraint", value) - Input operand for inline asm
            if len(expr.args) >= 2:
                return self._lower_expr(expr.args[1])
            return Immediate(0, IRType.WORD)

        if attr == "asm_output":
            # Asm_Output("constraint", variable) - Output operand
            if len(expr.args) >= 2:
                return self._lower_expr(expr.args[1])
            return Immediate(0, IRType.WORD)

        # Bit manipulation attributes
        if attr == "rotate_left":
            # Interfaces.Rotate_Left(Value, Amount)
            if len(expr.args) >= 2:
                value = self._lower_expr(expr.args[0])
                amount = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_rotl")
                self.builder.rol(result, value, amount)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "rotate_right":
            # Interfaces.Rotate_Right(Value, Amount)
            if len(expr.args) >= 2:
                value = self._lower_expr(expr.args[0])
                amount = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_rotr")
                self.builder.ror(result, value, amount)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "shift_left":
            # Interfaces.Shift_Left(Value, Amount)
            if len(expr.args) >= 2:
                value = self._lower_expr(expr.args[0])
                amount = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_shl")
                self.builder.shl(result, value, amount)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "shift_right":
            # Interfaces.Shift_Right(Value, Amount) - logical shift
            if len(expr.args) >= 2:
                value = self._lower_expr(expr.args[0])
                amount = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_shr")
                self.builder.shr(result, value, amount)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "shift_right_arithmetic":
            # Interfaces.Shift_Right_Arithmetic(Value, Amount)
            if len(expr.args) >= 2:
                value = self._lower_expr(expr.args[0])
                amount = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_sra")
                self.builder.sar(result, value, amount)
                return result
            return Immediate(0, IRType.WORD)

        # Interfaces.C attributes
        if attr == "to_c":
            # Convert Ada string to C string (null-terminated)
            if expr.args:
                str_val = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.PTR, "_c_str")
                self.builder.push(str_val)
                self.builder.call(Label("_to_c_string"), comment="To_C")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture C string"
                ))
                return result
            return Immediate(0, IRType.PTR)

        if attr == "to_ada":
            # Convert C string to Ada string
            if expr.args:
                c_str = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.PTR, "_ada_str")
                self.builder.push(c_str)
                self.builder.call(Label("_to_ada_string"), comment="To_Ada")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture Ada string"
                ))
                return result
            return Immediate(0, IRType.PTR)

        # Unchecked conversion
        if attr == "unchecked_conversion":
            # Ada.Unchecked_Conversion - type punning
            if expr.args:
                # Just return the bit pattern unchanged
                return self._lower_expr(expr.args[0])
            return Immediate(0, IRType.WORD)

        # Lock-free attributes
        if attr == "lock_free":
            # S'Lock_Free - True if protected object is lock-free
            # Z80 is single-threaded, so effectively lock-free
            return Immediate(1, IRType.WORD)

        # Real-time attributes
        if attr == "priority":
            # T'Priority - Task priority (for Z80, always same)
            return Immediate(0, IRType.WORD)

        if attr == "interrupt_priority":
            # Protected'Interrupt_Priority
            return Immediate(255, IRType.WORD)  # Highest priority

        # CPU attributes
        if attr == "cpu":
            # T'CPU - CPU affinity (Z80 has only one CPU)
            return Immediate(0, IRType.WORD)

        # Dispatching domain
        if attr == "dispatching_domain":
            # T'Dispatching_Domain - Task dispatching domain
            return Immediate(0, IRType.WORD)

        # System.Address_To_Access_Conversions
        if attr == "to_pointer":
            # To_Pointer(Address) - Convert address to access type
            if expr.args:
                addr = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.PTR, "_ptr")
                self.builder.mov(result, addr)
                return result
            return Immediate(0, IRType.PTR)

        if attr == "to_address":
            # To_Address(Pointer) - Convert access to address
            if expr.args:
                ptr = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_addr")
                self.builder.mov(result, ptr)
                return result
            return Immediate(0, IRType.WORD)

        # Random number support
        if attr == "random":
            # Random - Generate random number
            result = self.builder.new_vreg(IRType.WORD, "_random")
            self.builder.call(Label("_random"), comment="Random")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture random value"
            ))
            return result

        # Command line arguments
        if attr == "argument_count":
            # Ada.Command_Line.Argument_Count
            result = self.builder.new_vreg(IRType.WORD, "_argc")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(is_global=True, symbol_name="_argc", ir_type=IRType.WORD),
                comment="Argument_Count"
            ))
            return result

        if attr == "argument":
            # Ada.Command_Line.Argument(N)
            result = self.builder.new_vreg(IRType.PTR, "_argv")
            if expr.args:
                n = self._lower_expr(expr.args[0])
                self.builder.push(n)
                self.builder.call(Label("_get_argument"), comment="Argument")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture argument"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "command_name":
            # Ada.Command_Line.Command_Name
            result = self.builder.new_vreg(IRType.PTR, "_cmd_name")
            self.builder.call(Label("_get_command_name"), comment="Command_Name")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                comment="capture command name"
            ))
            return result

        # Environment variables
        if attr == "environment_count":
            # Count of environment variables
            result = self.builder.new_vreg(IRType.WORD, "_envc")
            self.builder.call(Label("_get_env_count"), comment="Environment_Count")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture env count"
            ))
            return result

        # File I/O attributes (Ada.Text_IO, Ada.Sequential_IO, etc.)
        if attr == "is_open":
            # File'Is_Open - Check if file is open
            result = self.builder.new_vreg(IRType.WORD, "_is_open")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_is_open"), comment="Is_Open")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture is_open result"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "mode":
            # File'Mode - Get file mode (In_File, Out_File, etc.)
            result = self.builder.new_vreg(IRType.WORD, "_file_mode")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_mode"), comment="Mode")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture file mode"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "name":
            # File'Name - Get file name string
            result = self.builder.new_vreg(IRType.PTR, "_file_name")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_name"), comment="Name")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture file name"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "form":
            # File'Form - Get file form string
            result = self.builder.new_vreg(IRType.PTR, "_file_form")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_form"), comment="Form")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture file form"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "line":
            # Current line number
            result = self.builder.new_vreg(IRType.WORD, "_line_num")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_line"), comment="Line")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture line number"
                ))
            else:
                self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "col":
            # Current column number
            result = self.builder.new_vreg(IRType.WORD, "_col_num")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_col"), comment="Col")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture column number"
                ))
            else:
                self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "page":
            # Current page number
            result = self.builder.new_vreg(IRType.WORD, "_page_num")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_page"), comment="Page")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture page number"
                ))
            else:
                self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "end_of_file":
            # File'End_Of_File
            result = self.builder.new_vreg(IRType.WORD, "_eof")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_eof"), comment="End_Of_File")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture EOF status"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "end_of_line":
            # File'End_Of_Line
            result = self.builder.new_vreg(IRType.WORD, "_eol")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_eol"), comment="End_Of_Line")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture EOL status"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "end_of_page":
            # File'End_Of_Page
            result = self.builder.new_vreg(IRType.WORD, "_eop")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_eop"), comment="End_Of_Page")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture EOP status"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        # Direct I/O attributes
        if attr == "index":
            # File'Index - Current position in direct I/O file
            result = self.builder.new_vreg(IRType.WORD, "_file_index")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_index"), comment="Index")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture file index"
                ))
            else:
                self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "file_size":
            # File'Size - Size of direct I/O file
            result = self.builder.new_vreg(IRType.WORD, "_file_size")
            if expr.args:
                file_val = self._lower_expr(expr.args[0])
                self.builder.push(file_val)
                self.builder.call(Label("_file_size"), comment="Size")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture file size"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        # Standard I/O file handles
        if attr == "standard_input":
            # Ada.Text_IO.Standard_Input
            result = self.builder.new_vreg(IRType.WORD, "_stdin")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(is_global=True, symbol_name="_stdin", ir_type=IRType.WORD),
                comment="Standard_Input"
            ))
            return result

        if attr == "standard_output":
            # Ada.Text_IO.Standard_Output
            result = self.builder.new_vreg(IRType.WORD, "_stdout")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(is_global=True, symbol_name="_stdout", ir_type=IRType.WORD),
                comment="Standard_Output"
            ))
            return result

        if attr == "standard_error":
            # Ada.Text_IO.Standard_Error
            result = self.builder.new_vreg(IRType.WORD, "_stderr")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(is_global=True, symbol_name="_stderr", ir_type=IRType.WORD),
                comment="Standard_Error"
            ))
            return result

        if attr == "current_input":
            # Ada.Text_IO.Current_Input
            result = self.builder.new_vreg(IRType.WORD, "_cur_in")
            self.builder.call(Label("_get_current_input"), comment="Current_Input")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture current input"
            ))
            return result

        if attr == "current_output":
            # Ada.Text_IO.Current_Output
            result = self.builder.new_vreg(IRType.WORD, "_cur_out")
            self.builder.call(Label("_get_current_output"), comment="Current_Output")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture current output"
            ))
            return result

        if attr == "current_error":
            # Ada.Text_IO.Current_Error
            result = self.builder.new_vreg(IRType.WORD, "_cur_err")
            self.builder.call(Label("_get_current_error"), comment="Current_Error")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture current error"
            ))
            return result

        # Numerics attributes
        if attr == "pi":
            # Ada.Numerics.Pi - for fixed point, scaled
            # Pi ≈ 3.14159 * 65536 = 205887 (but fits in 16 bits as 3 integer + frac)
            return Immediate(205887 & 0xFFFF, IRType.WORD)  # 16.16 fixed

        if attr == "e":
            # Ada.Numerics.e - Euler's number
            # e ≈ 2.71828 * 65536 = 178145
            return Immediate(178145 & 0xFFFF, IRType.WORD)

        # Complex number attributes (for Interfaces.Fortran or Ada.Numerics.Complex)
        if attr == "re":
            # Complex'Re - Real part
            if expr.args:
                complex_val = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_re")
                # Real part is at offset 0
                self.builder.load(result, complex_val)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "im":
            # Complex'Im - Imaginary part
            if expr.args:
                complex_val = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_im")
                # Imaginary part is at offset 2
                offset_ptr = self.builder.new_vreg(IRType.PTR, "_im_ptr")
                self.builder.add(offset_ptr, complex_val, Immediate(2, IRType.WORD))
                self.builder.load(result, offset_ptr)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "modulus":
            # Complex'Modulus - |z| = sqrt(re^2 + im^2)
            if expr.args:
                complex_val = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_modulus")
                self.builder.push(complex_val)
                self.builder.call(Label("_complex_modulus"), comment="Modulus")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture modulus"
                ))
                return result
            return Immediate(0, IRType.WORD)

        if attr == "argument":
            # Complex'Argument - Phase angle
            if expr.args:
                complex_val = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_arg")
                self.builder.push(complex_val)
                self.builder.call(Label("_complex_argument"), comment="Argument")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture argument"
                ))
                return result
            return Immediate(0, IRType.WORD)

        if attr == "conjugate":
            # Complex'Conjugate - Complex conjugate
            if expr.args:
                complex_val = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.PTR, "_conj")
                self.builder.push(complex_val)
                self.builder.call(Label("_complex_conjugate"), comment="Conjugate")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture conjugate"
                ))
                return result
            return Immediate(0, IRType.PTR)

        # Containers attributes
        if attr == "capacity":
            # Container'Capacity - Maximum capacity
            result = self.builder.new_vreg(IRType.WORD, "_capacity")
            if expr.args:
                container = self._lower_expr(expr.args[0])
                self.builder.push(container)
                self.builder.call(Label("_container_capacity"), comment="Capacity")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture capacity"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_empty":
            # Container'Is_Empty
            result = self.builder.new_vreg(IRType.WORD, "_is_empty")
            if expr.args:
                container = self._lower_expr(expr.args[0])
                self.builder.push(container)
                self.builder.call(Label("_container_is_empty"), comment="Is_Empty")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture is_empty"
                ))
            else:
                self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        # String handling attributes (Ada.Strings.Fixed, Ada.Strings.Unbounded, etc.)
        if attr == "head":
            # Head(Source, Count) - First Count characters
            result = self.builder.new_vreg(IRType.PTR, "_head")
            if len(expr.args) >= 2:
                source = self._lower_expr(expr.args[0])
                count = self._lower_expr(expr.args[1])
                self.builder.push(count)
                self.builder.push(source)
                self.builder.call(Label("_str_head"), comment="Head")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture head result"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "tail":
            # Tail(Source, Count) - Last Count characters
            result = self.builder.new_vreg(IRType.PTR, "_tail")
            if len(expr.args) >= 2:
                source = self._lower_expr(expr.args[0])
                count = self._lower_expr(expr.args[1])
                self.builder.push(count)
                self.builder.push(source)
                self.builder.call(Label("_str_tail"), comment="Tail")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture tail result"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "trim":
            # Trim(Source, Side) - Remove leading/trailing spaces
            result = self.builder.new_vreg(IRType.PTR, "_trim")
            if expr.args:
                source = self._lower_expr(expr.args[0])
                side = self._lower_expr(expr.args[1]) if len(expr.args) >= 2 else Immediate(0, IRType.WORD)
                self.builder.push(side)
                self.builder.push(source)
                self.builder.call(Label("_str_trim"), comment="Trim")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture trim result"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "index_non_blank":
            # Index of first non-blank character
            result = self.builder.new_vreg(IRType.WORD, "_index_nb")
            if expr.args:
                source = self._lower_expr(expr.args[0])
                self.builder.push(source)
                self.builder.call(Label("_str_index_non_blank"), comment="Index_Non_Blank")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture index"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "count":
            # Count occurrences of pattern in string
            result = self.builder.new_vreg(IRType.WORD, "_count")
            if len(expr.args) >= 2:
                source = self._lower_expr(expr.args[0])
                pattern = self._lower_expr(expr.args[1])
                self.builder.push(pattern)
                self.builder.push(source)
                self.builder.call(Label("_str_count"), comment="Count")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture count"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "translate":
            # Translate(Source, Mapping) - Character mapping
            result = self.builder.new_vreg(IRType.PTR, "_translate")
            if len(expr.args) >= 2:
                source = self._lower_expr(expr.args[0])
                mapping = self._lower_expr(expr.args[1])
                self.builder.push(mapping)
                self.builder.push(source)
                self.builder.call(Label("_str_translate"), comment="Translate")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture translated string"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "replace_slice":
            # Replace_Slice(Source, Low, High, By) - Replace substring
            result = self.builder.new_vreg(IRType.PTR, "_replace")
            if len(expr.args) >= 4:
                source = self._lower_expr(expr.args[0])
                low = self._lower_expr(expr.args[1])
                high = self._lower_expr(expr.args[2])
                by = self._lower_expr(expr.args[3])
                self.builder.push(by)
                self.builder.push(high)
                self.builder.push(low)
                self.builder.push(source)
                self.builder.call(Label("_str_replace_slice"), comment="Replace_Slice")
                for _ in range(4):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture replaced string"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "insert":
            # Insert(Source, Before, New_Item) - Insert substring
            result = self.builder.new_vreg(IRType.PTR, "_insert")
            if len(expr.args) >= 3:
                source = self._lower_expr(expr.args[0])
                before = self._lower_expr(expr.args[1])
                new_item = self._lower_expr(expr.args[2])
                self.builder.push(new_item)
                self.builder.push(before)
                self.builder.push(source)
                self.builder.call(Label("_str_insert"), comment="Insert")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture inserted string"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "overwrite":
            # Overwrite(Source, Position, New_Item) - Overwrite substring
            result = self.builder.new_vreg(IRType.PTR, "_overwrite")
            if len(expr.args) >= 3:
                source = self._lower_expr(expr.args[0])
                position = self._lower_expr(expr.args[1])
                new_item = self._lower_expr(expr.args[2])
                self.builder.push(new_item)
                self.builder.push(position)
                self.builder.push(source)
                self.builder.call(Label("_str_overwrite"), comment="Overwrite")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture overwritten string"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "delete":
            # Delete(Source, From, Through) - Delete substring
            result = self.builder.new_vreg(IRType.PTR, "_delete")
            if len(expr.args) >= 3:
                source = self._lower_expr(expr.args[0])
                from_pos = self._lower_expr(expr.args[1])
                through = self._lower_expr(expr.args[2])
                self.builder.push(through)
                self.builder.push(from_pos)
                self.builder.push(source)
                self.builder.call(Label("_str_delete"), comment="Delete")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture deleted string"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        # Math function attributes (Ada.Numerics.Elementary_Functions)
        if attr == "sqrt":
            # Sqrt(X) - Square root
            result = self.builder.new_vreg(IRType.WORD, "_sqrt")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_sqrt"), comment="Sqrt")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture sqrt"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "log":
            # Log(X) - Natural logarithm
            result = self.builder.new_vreg(IRType.WORD, "_log")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_log"), comment="Log")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture log"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "exp":
            # Exp(X) - e^X
            result = self.builder.new_vreg(IRType.WORD, "_exp")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_exp"), comment="Exp")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture exp"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "sin":
            # Sin(X) - Sine
            result = self.builder.new_vreg(IRType.WORD, "_sin")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_sin"), comment="Sin")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture sin"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "cos":
            # Cos(X) - Cosine
            result = self.builder.new_vreg(IRType.WORD, "_cos")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_cos"), comment="Cos")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture cos"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "tan":
            # Tan(X) - Tangent
            result = self.builder.new_vreg(IRType.WORD, "_tan")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_tan"), comment="Tan")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture tan"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "arcsin":
            # Arcsin(X) - Inverse sine
            result = self.builder.new_vreg(IRType.WORD, "_arcsin")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_arcsin"), comment="Arcsin")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture arcsin"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "arccos":
            # Arccos(X) - Inverse cosine
            result = self.builder.new_vreg(IRType.WORD, "_arccos")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_arccos"), comment="Arccos")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture arccos"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "arctan":
            # Arctan(X [, Y]) - Inverse tangent
            result = self.builder.new_vreg(IRType.WORD, "_arctan")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                if len(expr.args) >= 2:
                    y = self._lower_expr(expr.args[1])
                    self.builder.push(y)
                    self.builder.push(x)
                    self.builder.call(Label("_arctan2"), comment="Arctan2")
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                    self.builder.pop(temp)
                else:
                    self.builder.push(x)
                    self.builder.call(Label("_arctan"), comment="Arctan")
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture arctan"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "sinh":
            # Sinh(X) - Hyperbolic sine
            result = self.builder.new_vreg(IRType.WORD, "_sinh")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_sinh"), comment="Sinh")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture sinh"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "cosh":
            # Cosh(X) - Hyperbolic cosine
            result = self.builder.new_vreg(IRType.WORD, "_cosh")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_cosh"), comment="Cosh")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture cosh"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "tanh":
            # Tanh(X) - Hyperbolic tangent
            result = self.builder.new_vreg(IRType.WORD, "_tanh")
            if expr.args:
                x = self._lower_expr(expr.args[0])
                self.builder.push(x)
                self.builder.call(Label("_tanh"), comment="Tanh")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture tanh"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        # Directory operations (Ada.Directories)
        if attr == "current_directory":
            # Current working directory
            result = self.builder.new_vreg(IRType.PTR, "_cwd")
            self.builder.call(Label("_get_cwd"), comment="Current_Directory")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                comment="capture cwd"
            ))
            return result

        if attr == "exists":
            # File/directory exists check
            result = self.builder.new_vreg(IRType.WORD, "_exists")
            if expr.args:
                path = self._lower_expr(expr.args[0])
                self.builder.push(path)
                self.builder.call(Label("_file_exists"), comment="Exists")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture exists result"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "kind":
            # File kind (directory, ordinary file, special file)
            result = self.builder.new_vreg(IRType.WORD, "_kind")
            if expr.args:
                path = self._lower_expr(expr.args[0])
                self.builder.push(path)
                self.builder.call(Label("_file_kind"), comment="Kind")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture file kind"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "simple_name":
            # Extract simple name from path
            result = self.builder.new_vreg(IRType.PTR, "_simple_name")
            if expr.args:
                path = self._lower_expr(expr.args[0])
                self.builder.push(path)
                self.builder.call(Label("_simple_name"), comment="Simple_Name")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture simple name"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "containing_directory":
            # Extract containing directory from path
            result = self.builder.new_vreg(IRType.PTR, "_containing_dir")
            if expr.args:
                path = self._lower_expr(expr.args[0])
                self.builder.push(path)
                self.builder.call(Label("_containing_directory"), comment="Containing_Directory")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture containing directory"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "extension":
            # Extract file extension
            result = self.builder.new_vreg(IRType.PTR, "_extension")
            if expr.args:
                path = self._lower_expr(expr.args[0])
                self.builder.push(path)
                self.builder.call(Label("_file_extension"), comment="Extension")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture extension"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "base_name":
            # Extract base name (without extension)
            result = self.builder.new_vreg(IRType.PTR, "_base_name")
            if expr.args:
                path = self._lower_expr(expr.args[0])
                self.builder.push(path)
                self.builder.call(Label("_base_name"), comment="Base_Name")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture base name"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "full_name":
            # Get full/absolute path name
            result = self.builder.new_vreg(IRType.PTR, "_full_name")
            if expr.args:
                path = self._lower_expr(expr.args[0])
                self.builder.push(path)
                self.builder.call(Label("_full_name"), comment="Full_Name")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture full name"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "compose":
            # Compose(Directory, Name, Extension) - Build path
            result = self.builder.new_vreg(IRType.PTR, "_composed")
            if len(expr.args) >= 2:
                directory = self._lower_expr(expr.args[0])
                name = self._lower_expr(expr.args[1])
                ext = self._lower_expr(expr.args[2]) if len(expr.args) >= 3 else Immediate(0, IRType.PTR)
                self.builder.push(ext)
                self.builder.push(name)
                self.builder.push(directory)
                self.builder.call(Label("_compose_path"), comment="Compose")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture composed path"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        # Character handling attributes (Ada.Characters.Handling)
        if attr == "is_control":
            # Is_Control(C) - Check if control character
            result = self.builder.new_vreg(IRType.WORD, "_is_ctrl")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                # Control chars: 0-31, 127
                temp = self.builder.new_vreg(IRType.WORD, "_temp")
                self.builder.emit(IRInstr(
                    OpCode.CMP_LT, temp, c, Immediate(32, IRType.WORD),
                    comment="check < 32"
                ))
                temp2 = self.builder.new_vreg(IRType.WORD, "_temp2")
                self.builder.emit(IRInstr(
                    OpCode.CMP_EQ, temp2, c, Immediate(127, IRType.WORD),
                    comment="check == 127"
                ))
                self.builder.emit(IRInstr(OpCode.OR, result, temp, temp2))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_graphic":
            # Is_Graphic(C) - Check if graphic (printable) character
            result = self.builder.new_vreg(IRType.WORD, "_is_graphic")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                # Graphic chars: 32-126
                temp1 = self.builder.new_vreg(IRType.WORD, "_t1")
                temp2 = self.builder.new_vreg(IRType.WORD, "_t2")
                self.builder.emit(IRInstr(
                    OpCode.CMP_GE, temp1, c, Immediate(32, IRType.WORD),
                    comment="check >= 32"
                ))
                self.builder.emit(IRInstr(
                    OpCode.CMP_LE, temp2, c, Immediate(126, IRType.WORD),
                    comment="check <= 126"
                ))
                self.builder.emit(IRInstr(OpCode.AND, result, temp1, temp2))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_letter":
            # Is_Letter(C) - Check if letter (A-Z, a-z)
            result = self.builder.new_vreg(IRType.WORD, "_is_letter")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                self.builder.push(c)
                self.builder.call(Label("_is_letter"), comment="Is_Letter")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture is_letter"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_lower":
            # Is_Lower(C) - Check if lowercase letter
            result = self.builder.new_vreg(IRType.WORD, "_is_lower")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                temp1 = self.builder.new_vreg(IRType.WORD, "_t1")
                temp2 = self.builder.new_vreg(IRType.WORD, "_t2")
                self.builder.emit(IRInstr(
                    OpCode.CMP_GE, temp1, c, Immediate(ord('a'), IRType.WORD),
                    comment="check >= 'a'"
                ))
                self.builder.emit(IRInstr(
                    OpCode.CMP_LE, temp2, c, Immediate(ord('z'), IRType.WORD),
                    comment="check <= 'z'"
                ))
                self.builder.emit(IRInstr(OpCode.AND, result, temp1, temp2))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_upper":
            # Is_Upper(C) - Check if uppercase letter
            result = self.builder.new_vreg(IRType.WORD, "_is_upper")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                temp1 = self.builder.new_vreg(IRType.WORD, "_t1")
                temp2 = self.builder.new_vreg(IRType.WORD, "_t2")
                self.builder.emit(IRInstr(
                    OpCode.CMP_GE, temp1, c, Immediate(ord('A'), IRType.WORD),
                    comment="check >= 'A'"
                ))
                self.builder.emit(IRInstr(
                    OpCode.CMP_LE, temp2, c, Immediate(ord('Z'), IRType.WORD),
                    comment="check <= 'Z'"
                ))
                self.builder.emit(IRInstr(OpCode.AND, result, temp1, temp2))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_digit":
            # Is_Digit(C) - Check if digit (0-9)
            result = self.builder.new_vreg(IRType.WORD, "_is_digit")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                temp1 = self.builder.new_vreg(IRType.WORD, "_t1")
                temp2 = self.builder.new_vreg(IRType.WORD, "_t2")
                self.builder.emit(IRInstr(
                    OpCode.CMP_GE, temp1, c, Immediate(ord('0'), IRType.WORD),
                    comment="check >= '0'"
                ))
                self.builder.emit(IRInstr(
                    OpCode.CMP_LE, temp2, c, Immediate(ord('9'), IRType.WORD),
                    comment="check <= '9'"
                ))
                self.builder.emit(IRInstr(OpCode.AND, result, temp1, temp2))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_hexadecimal_digit":
            # Is_Hexadecimal_Digit(C) - Check if hex digit
            result = self.builder.new_vreg(IRType.WORD, "_is_hex")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                self.builder.push(c)
                self.builder.call(Label("_is_hex_digit"), comment="Is_Hexadecimal_Digit")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture is_hex"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_alphanumeric":
            # Is_Alphanumeric(C) - Letter or digit
            result = self.builder.new_vreg(IRType.WORD, "_is_alnum")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                self.builder.push(c)
                self.builder.call(Label("_is_alphanumeric"), comment="Is_Alphanumeric")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture is_alnum"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "is_special":
            # Is_Special(C) - Special graphic character
            result = self.builder.new_vreg(IRType.WORD, "_is_special")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                self.builder.push(c)
                self.builder.call(Label("_is_special"), comment="Is_Special")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture is_special"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "to_lower":
            # To_Lower(C) - Convert to lowercase
            result = self.builder.new_vreg(IRType.WORD, "_to_lower")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                # If 'A'-'Z', add 32
                is_upper = self.builder.new_vreg(IRType.WORD, "_is_up")
                temp1 = self.builder.new_vreg(IRType.WORD, "_t1")
                temp2 = self.builder.new_vreg(IRType.WORD, "_t2")
                self.builder.emit(IRInstr(
                    OpCode.CMP_GE, temp1, c, Immediate(ord('A'), IRType.WORD)
                ))
                self.builder.emit(IRInstr(
                    OpCode.CMP_LE, temp2, c, Immediate(ord('Z'), IRType.WORD)
                ))
                self.builder.emit(IRInstr(OpCode.AND, is_upper, temp1, temp2))
                # result = is_upper ? c + 32 : c
                lower = self.builder.new_vreg(IRType.WORD, "_lower")
                self.builder.add(lower, c, Immediate(32, IRType.WORD))
                # Conditional select
                ok_label = self._new_label("tolower_ok")
                end_label = self._new_label("tolower_end")
                self.builder.jnz(is_upper, Label(ok_label))
                self.builder.mov(result, c)
                self.builder.jmp(Label(end_label))
                self.builder.label(ok_label)
                self.builder.mov(result, lower)
                self.builder.label(end_label)
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "to_upper":
            # To_Upper(C) - Convert to uppercase
            result = self.builder.new_vreg(IRType.WORD, "_to_upper")
            if expr.args:
                c = self._lower_expr(expr.args[0])
                # If 'a'-'z', subtract 32
                is_lower = self.builder.new_vreg(IRType.WORD, "_is_low")
                temp1 = self.builder.new_vreg(IRType.WORD, "_t1")
                temp2 = self.builder.new_vreg(IRType.WORD, "_t2")
                self.builder.emit(IRInstr(
                    OpCode.CMP_GE, temp1, c, Immediate(ord('a'), IRType.WORD)
                ))
                self.builder.emit(IRInstr(
                    OpCode.CMP_LE, temp2, c, Immediate(ord('z'), IRType.WORD)
                ))
                self.builder.emit(IRInstr(OpCode.AND, is_lower, temp1, temp2))
                # result = is_lower ? c - 32 : c
                upper = self.builder.new_vreg(IRType.WORD, "_upper")
                self.builder.sub(upper, c, Immediate(32, IRType.WORD))
                # Conditional select
                ok_label = self._new_label("toupper_ok")
                end_label = self._new_label("toupper_end")
                self.builder.jnz(is_lower, Label(ok_label))
                self.builder.mov(result, c)
                self.builder.jmp(Label(end_label))
                self.builder.label(ok_label)
                self.builder.mov(result, upper)
                self.builder.label(end_label)
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "to_basic":
            # To_Basic(C) - Remove diacritical marks (for Z80, just identity)
            if expr.args:
                return self._lower_expr(expr.args[0])
            return Immediate(0, IRType.WORD)

        # Sequential I/O operations
        if attr == "read":
            # Sequential_IO.Read(File, Item)
            # File is the file handle, Item is the output variable
            if len(expr.args) >= 2:
                file_val = self._lower_expr(expr.args[0])
                item_arg = expr.args[1]
                # Get address of item for writing result
                item_addr = self._get_lvalue_address(item_arg)
                if item_addr is None:
                    item_addr = self._lower_expr(item_arg)
                # Determine item size from type
                item_size = self._get_type_size(item_arg)
                self.builder.push(Immediate(item_size, IRType.WORD))
                self.builder.push(item_addr)
                self.builder.push(file_val)
                self.builder.call(Label("_file_read"), comment="Sequential_IO.Read")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
            return Immediate(0, IRType.WORD)

        if attr == "write":
            # Sequential_IO.Write(File, Item)
            # File is the file handle, Item is the value to write
            if len(expr.args) >= 2:
                file_val = self._lower_expr(expr.args[0])
                item_arg = expr.args[1]
                # Get address of item for reading value
                item_addr = self._get_lvalue_address(item_arg)
                if item_addr is None:
                    # For literals or expressions, we need to evaluate and store to temp
                    item_val = self._lower_expr(item_arg)
                    # Push value to stack and use stack address
                    self.builder.push(item_val)
                    item_addr = self.builder.new_vreg(IRType.PTR, "_item_addr")
                    self.builder.emit(IRInstr(OpCode.GETSP, dst=item_addr))
                # Determine item size from type
                item_size = self._get_type_size(item_arg)
                self.builder.push(Immediate(item_size, IRType.WORD))
                self.builder.push(item_addr)
                self.builder.push(file_val)
                self.builder.call(Label("_file_write"), comment="Sequential_IO.Write")
                for _ in range(3):
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
                    self.builder.pop(temp)
            return Immediate(0, IRType.WORD)

        # Finalization and controlled types
        if attr == "needs_finalization":
            # T'Needs_Finalization - Check if type needs finalization
            # For Z80 simple types, usually False
            return Immediate(0, IRType.WORD)

        if attr == "has_tagged_values":
            # T'Has_Tagged_Values - Check if type has tagged values
            return Immediate(0, IRType.WORD)

        if attr == "is_controlled":
            # T'Is_Controlled - Check if controlled type
            return Immediate(0, IRType.WORD)

        # Shared memory attributes
        if attr == "atomic_always_lock_free":
            # T'Atomic_Always_Lock_Free
            # Z80 is single-threaded, so always lock-free for small types
            return Immediate(1, IRType.WORD)

        if attr == "max_alignment_for_allocation":
            # Maximum alignment for allocation (Z80: 1 byte)
            return Immediate(1, IRType.WORD)

        # Wide character attributes
        if attr == "wide_width":
            # T'Wide_Width - Max width for wide character type
            return Immediate(1, IRType.WORD)

        if attr == "wide_wide_width":
            # T'Wide_Wide_Width - Max width for wide_wide character
            return Immediate(1, IRType.WORD)

        # Access type attributes
        if attr == "designated_storage_model":
            # Access type's storage model
            return Immediate(0, IRType.WORD)

        if attr == "storage_model_type":
            # Storage model type identifier
            return Immediate(0, IRType.WORD)

        # Scalar representation
        if attr == "machine_size":
            # S'Machine_Size - Size as seen by machine
            result = self.builder.new_vreg(IRType.WORD, "_msize")
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type and hasattr(sym.ada_type, 'size_bits'):
                    return Immediate(sym.ada_type.size_bits, IRType.WORD)
            return Immediate(16, IRType.WORD)  # Default word size

        if attr == "machine_overflows":
            # S'Machine_Overflows - Does machine detect overflow?
            # Z80 has overflow flag
            return Immediate(1, IRType.WORD)

        if attr == "machine_rounds":
            # S'Machine_Rounds - Does machine round?
            return Immediate(0, IRType.WORD)  # Z80 truncates

        # Bounded string attributes
        if attr == "max_length":
            # Bounded_String'Max_Length
            result = self.builder.new_vreg(IRType.WORD, "_max_len")
            if expr.args:
                str_val = self._lower_expr(expr.args[0])
                # Max length is at offset 0 in bounded string
                self.builder.load(result, str_val)
            else:
                self.builder.mov(result, Immediate(255, IRType.WORD))  # Default max
            return result

        if attr == "bounded_length":
            # Current length of bounded string
            result = self.builder.new_vreg(IRType.WORD, "_bnd_len")
            if expr.args:
                str_val = self._lower_expr(expr.args[0])
                # Current length is at offset 2 in bounded string
                temp = self.builder.new_vreg(IRType.PTR, "_temp")
                self.builder.add(temp, str_val, Immediate(2, IRType.WORD))
                self.builder.load(result, temp)
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        # Generic intrinsic attributes
        if attr == "target":
            # Access T'Target - dereference access type
            if expr.args:
                acc_val = self._lower_expr(expr.args[0])
                result = self.builder.new_vreg(IRType.WORD, "_target")
                self.builder.load(result, acc_val)
                return result
            return Immediate(0, IRType.WORD)

        if attr == "update":
            # Attribute reference to update (used in delta aggregates)
            # This is handled separately in _lower_attribute for 'Update
            pass

        # Representation attributes
        if attr == "default_value":
            # T'Default_Value - default value for scalar type
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type:
                    if hasattr(sym.ada_type, 'low'):
                        return Immediate(sym.ada_type.low, IRType.WORD)
            return Immediate(0, IRType.WORD)

        if attr == "has_default_value":
            # T'Has_Default_Value
            return Immediate(1, IRType.WORD)  # Scalars have default

        # Parallel attributes (Ada 2022)
        if attr == "reduce_access":
            # For parallel reduction
            if expr.args:
                return self._lower_expr(expr.args[0])
            return Immediate(0, IRType.WORD)

        if attr == "chunk_count":
            # Number of chunks for parallel iteration
            # Z80 is single-threaded, always 1
            return Immediate(1, IRType.WORD)

        # Iterator/Cursor attributes (Ada.Iterator_Interfaces)
        if attr == "has_element":
            # Cursor'Has_Element - Check if cursor points to valid element
            result = self.builder.new_vreg(IRType.WORD, "_has_elem")
            if expr.args:
                cursor = self._lower_expr(expr.args[0])
                self.builder.push(cursor)
                self.builder.call(Label("_cursor_has_element"), comment="Has_Element")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture has_element"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "element":
            # Container.Element(Cursor) - Get element at cursor
            result = self.builder.new_vreg(IRType.WORD, "_element")
            if expr.args:
                cursor = self._lower_expr(expr.args[0])
                self.builder.push(cursor)
                self.builder.call(Label("_cursor_element"), comment="Element")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture element"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "key":
            # Map.Key(Cursor) - Get key at cursor (for maps)
            result = self.builder.new_vreg(IRType.WORD, "_key")
            if expr.args:
                cursor = self._lower_expr(expr.args[0])
                self.builder.push(cursor)
                self.builder.call(Label("_cursor_key"), comment="Key")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture key"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "next":
            # Container.Next(Cursor) - Advance cursor
            result = self.builder.new_vreg(IRType.WORD, "_next")
            if expr.args:
                cursor = self._lower_expr(expr.args[0])
                self.builder.push(cursor)
                self.builder.call(Label("_cursor_next"), comment="Next")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture next cursor"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "previous":
            # Container.Previous(Cursor) - Move cursor backward
            result = self.builder.new_vreg(IRType.WORD, "_prev")
            if expr.args:
                cursor = self._lower_expr(expr.args[0])
                self.builder.push(cursor)
                self.builder.call(Label("_cursor_previous"), comment="Previous")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture previous cursor"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "first":
            # Container.First - Get first cursor
            # Note: 'First for arrays is handled earlier
            result = self.builder.new_vreg(IRType.WORD, "_first_cursor")
            if expr.args:
                container = self._lower_expr(expr.args[0])
                self.builder.push(container)
                self.builder.call(Label("_container_first"), comment="First")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture first cursor"
                ))
                return result
            # Fall through to default for array 'First

        if attr == "last":
            # Container.Last - Get last cursor
            # Note: 'Last for arrays is handled earlier
            result = self.builder.new_vreg(IRType.WORD, "_last_cursor")
            if expr.args:
                container = self._lower_expr(expr.args[0])
                self.builder.push(container)
                self.builder.call(Label("_container_last"), comment="Last")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture last cursor"
                ))
                return result
            # Fall through to default for array 'Last

        if attr == "find":
            # Container.Find(Item) - Find element, return cursor
            result = self.builder.new_vreg(IRType.WORD, "_find")
            if len(expr.args) >= 2:
                container = self._lower_expr(expr.args[0])
                item = self._lower_expr(expr.args[1])
                self.builder.push(item)
                self.builder.push(container)
                self.builder.call(Label("_container_find"), comment="Find")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture find cursor"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "contains":
            # Container.Contains(Item) - Check if container has item
            result = self.builder.new_vreg(IRType.WORD, "_contains")
            if len(expr.args) >= 2:
                container = self._lower_expr(expr.args[0])
                item = self._lower_expr(expr.args[1])
                self.builder.push(item)
                self.builder.push(container)
                self.builder.call(Label("_container_contains"), comment="Contains")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture contains result"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        # Contract/Assertion attributes
        if attr == "class_wide":
            # T'Class - Class-wide type (for dispatching)
            # Return the prefix value (type tag handles dispatching)
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym:
                    return self._lower_expr(expr.prefix)
            return Immediate(0, IRType.WORD)

        if attr == "static_predicate":
            # T'Static_Predicate - Get static predicate (compile-time)
            return Immediate(1, IRType.WORD)  # Assume satisfied

        if attr == "dynamic_predicate":
            # T'Dynamic_Predicate - Get dynamic predicate
            return Immediate(1, IRType.WORD)  # Assume satisfied

        if attr == "predicate_check":
            # Check predicate for value
            result = self.builder.new_vreg(IRType.WORD, "_pred_check")
            if expr.args:
                val = self._lower_expr(expr.args[0])
                # For now, assume predicate passes
                self.builder.mov(result, Immediate(1, IRType.WORD))
            else:
                self.builder.mov(result, Immediate(1, IRType.WORD))
            return result

        if attr == "type_invariant":
            # T'Type_Invariant - Get type invariant
            return Immediate(1, IRType.WORD)  # Assume satisfied

        if attr == "invariant_check":
            # Check invariant for object
            return Immediate(1, IRType.WORD)  # Assume satisfied

        # Subprogram contract attributes
        if attr == "precondition":
            # S'Precondition - Get precondition expression
            return Immediate(1, IRType.WORD)  # Assume satisfied

        if attr == "postcondition":
            # S'Postcondition - Get postcondition expression
            return Immediate(1, IRType.WORD)  # Assume satisfied

        if attr == "stable_properties":
            # T'Stable_Properties - Stable property list
            return Immediate(0, IRType.WORD)

        # Ghost code attributes (Ada 2022)
        if attr == "ghost":
            # X'Ghost - Ghost aspect of entity
            # Ghost code is not generated for production
            return Immediate(0, IRType.WORD)

        if attr == "ghost_code":
            # Ghost code marker
            return Immediate(0, IRType.WORD)

        # Additional type introspection
        if attr == "scalar_storage_order":
            # T'Scalar_Storage_Order - Byte order
            # Z80 is little-endian (Low_Order_First)
            return Immediate(0, IRType.WORD)  # 0 = Low_Order_First

        if attr == "bit_order":
            # T'Bit_Order - Bit order within bytes
            return Immediate(0, IRType.WORD)  # Low_Order_First

        if attr == "machine_code_convention":
            # Calling convention for machine code
            return Immediate(0, IRType.WORD)

        if attr == "convention_info":
            # Get convention information for entity
            return Immediate(0, IRType.WORD)

        if attr == "import_convention":
            # Convention used for Import pragma
            return Immediate(0, IRType.WORD)

        if attr == "export_convention":
            # Convention used for Export pragma
            return Immediate(0, IRType.WORD)

        # Representation/Layout attributes
        if attr == "position":
            # Component'Position - Bit position in record
            result = self.builder.new_vreg(IRType.WORD, "_position")
            # Would need type info to compute actual position
            self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "first_bit":
            # Component'First_Bit - First bit of component
            return Immediate(0, IRType.WORD)

        if attr == "last_bit":
            # Component'Last_Bit - Last bit of component
            return Immediate(15, IRType.WORD)  # 16-bit word

        # Array layout attributes
        if attr == "adjacent":
            # A'Adjacent(X, Towards) - Adjacent value in direction
            if len(expr.args) >= 2:
                x = self._lower_expr(expr.args[0])
                towards = self._lower_expr(expr.args[1])
                result = self.builder.new_vreg(IRType.WORD, "_adjacent")
                # Compare x with towards to determine direction
                cmp = self.builder.new_vreg(IRType.WORD, "_cmp")
                self.builder.emit(IRInstr(OpCode.CMP_LT, cmp, x, towards))
                # If x < towards, increment; else decrement
                inc_label = self._new_label("adj_inc")
                end_label = self._new_label("adj_end")
                self.builder.jnz(cmp, Label(inc_label))
                # x >= towards, decrement
                self.builder.sub(result, x, Immediate(1, IRType.WORD))
                self.builder.jmp(Label(end_label))
                self.builder.label(inc_label)
                # x < towards, increment
                self.builder.add(result, x, Immediate(1, IRType.WORD))
                self.builder.label(end_label)
                return result
            return Immediate(0, IRType.WORD)

        # Discriminant attributes
        if attr == "discriminant_constraint":
            # T'Discriminant_Constraint - Constraint info
            return Immediate(0, IRType.WORD)

        if attr == "known_discriminant_part":
            # Check for known discriminant part
            return Immediate(0, IRType.WORD)

        if attr == "unknown_discriminant_part":
            # Check for unknown discriminant part
            return Immediate(0, IRType.WORD)

        # Private type attributes
        if attr == "has_private_part":
            # P'Has_Private_Part - Package has private part
            return Immediate(0, IRType.WORD)

        if attr == "private_part":
            # Access to private part info
            return Immediate(0, IRType.WORD)

        # Compilation unit attributes
        if attr == "unit_name":
            # Get compilation unit name string
            result = self.builder.new_vreg(IRType.PTR, "_unit_name")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_unit_name"),
                comment="Unit_Name"
            ))
            return result

        if attr == "enclosing_entity":
            # Get enclosing entity name
            result = self.builder.new_vreg(IRType.PTR, "_encl_entity")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_enclosing_entity"),
                comment="Enclosing_Entity"
            ))
            return result

        # Library level attributes
        if attr == "library_level":
            # Check if entity is at library level
            return Immediate(1, IRType.WORD)  # Assume library level

        if attr == "library_unit":
            # Get library unit name
            result = self.builder.new_vreg(IRType.PTR, "_lib_unit")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_library_unit"),
                comment="Library_Unit"
            ))
            return result

        # Object attributes
        if attr == "object_size":
            # X'Object_Size - Size of object in bits
            result = self.builder.new_vreg(IRType.WORD, "_obj_size")
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type and hasattr(sym.ada_type, 'size_bits'):
                    return Immediate(sym.ada_type.size_bits, IRType.WORD)
            return Immediate(16, IRType.WORD)  # Default word

        if attr == "value_size":
            # T'Value_Size - Size needed for value
            result = self.builder.new_vreg(IRType.WORD, "_val_size")
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type and hasattr(sym.ada_type, 'size_bits'):
                    return Immediate(sym.ada_type.size_bits, IRType.WORD)
            return Immediate(16, IRType.WORD)

        # Loop attributes
        if attr == "loop_iteration":
            # Current loop iteration count
            result = self.builder.new_vreg(IRType.WORD, "_loop_iter")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(is_global=False, symbol_name="_loop_count", ir_type=IRType.WORD),
                comment="Loop_Iteration"
            ))
            return result

        # GNAT-specific attributes (implementation-defined)
        if attr == "elab_body":
            # Elab_Body - Elaboration procedure for body
            result = self.builder.new_vreg(IRType.PTR, "_elab_body")
            if isinstance(expr.prefix, Identifier):
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_elab_body_{expr.prefix.name.lower()}"),
                    comment=f"Elab_Body for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "elab_spec":
            # Elab_Spec - Elaboration procedure for spec
            result = self.builder.new_vreg(IRType.PTR, "_elab_spec")
            if isinstance(expr.prefix, Identifier):
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_elab_spec_{expr.prefix.name.lower()}"),
                    comment=f"Elab_Spec for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "elab_subp_body":
            # Elab_Subp_Body - Elaboration for subprogram body
            result = self.builder.new_vreg(IRType.PTR, "_elab_subp")
            if isinstance(expr.prefix, Identifier):
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_elab_subp_{expr.prefix.name.lower()}"),
                    comment=f"Elab_Subp_Body for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "elaboration_checks":
            # Elaboration_Checks - Are elaboration checks enabled?
            return Immediate(1, IRType.WORD)  # Yes, enabled

        if attr == "from_any":
            # From_Any - CORBA interop (not supported on Z80)
            return Immediate(0, IRType.WORD)

        if attr == "to_any":
            # To_Any - CORBA interop
            return Immediate(0, IRType.WORD)

        if attr == "typecode":
            # TypeCode - CORBA type code
            return Immediate(0, IRType.WORD)

        if attr == "stub_type":
            # Stub_Type - RPC stub type
            return Immediate(0, IRType.WORD)

        if attr == "machine_attribute":
            # Implementation-defined machine attribute
            return Immediate(0, IRType.WORD)

        if attr == "system_allocator_alignment":
            # Alignment used by system allocator
            return Immediate(1, IRType.WORD)  # Z80: byte aligned

        if attr == "finalize_address":
            # Address of finalization routine
            result = self.builder.new_vreg(IRType.PTR, "_finalize_addr")
            if isinstance(expr.prefix, Identifier):
                self.builder.emit(IRInstr(
                    OpCode.LEA, result,
                    Label(f"_finalize_{expr.prefix.name.lower()}"),
                    comment=f"Finalize_Address for {expr.prefix.name}"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        if attr == "put_image":
            # Put_Image procedure (Ada 2022)
            # Already handled earlier, but provide fallback
            return Immediate(0, IRType.WORD)

        if attr == "object_tag":
            # Get tag of tagged object
            result = self.builder.new_vreg(IRType.WORD, "_obj_tag")
            if expr.args:
                obj = self._lower_expr(expr.args[0])
                # Tag is at offset 0 of tagged object
                self.builder.load(result, obj)
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "tag_info":
            # Get tag information structure
            result = self.builder.new_vreg(IRType.PTR, "_tag_info")
            if expr.args:
                tag = self._lower_expr(expr.args[0])
                self.builder.push(tag)
                self.builder.call(Label("_get_tag_info"), comment="Tag_Info")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.PTR),
                    comment="capture tag info"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.PTR))
            return result

        # Memory and allocation attributes
        if attr == "allocation_size":
            # Size to allocate for type
            result = self.builder.new_vreg(IRType.WORD, "_alloc_size")
            if isinstance(expr.prefix, Identifier):
                sym = self.symbols.lookup(expr.prefix.name)
                if sym and sym.ada_type and hasattr(sym.ada_type, 'size_bits'):
                    size_bytes = (sym.ada_type.size_bits + 7) // 8
                    return Immediate(size_bytes, IRType.WORD)
            return Immediate(2, IRType.WORD)  # Default word

        if attr == "null_address":
            # Null address constant
            return Immediate(0, IRType.WORD)

        if attr == "memory_size":
            # Total available memory
            # CP/M TPA typically 0x100-0xBFFF (~48KB)
            return Immediate(0xBF00, IRType.WORD)

        if attr == "heap_size":
            # Current heap size
            result = self.builder.new_vreg(IRType.WORD, "_heap_size")
            self.builder.call(Label("_get_heap_size"), comment="Heap_Size")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture heap size"
            ))
            return result

        if attr == "stack_size":
            # Current stack usage
            result = self.builder.new_vreg(IRType.WORD, "_stack_size")
            self.builder.call(Label("_get_stack_size"), comment="Stack_Size")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture stack size"
            ))
            return result

        if attr == "free_memory":
            # Available free memory
            result = self.builder.new_vreg(IRType.WORD, "_free_mem")
            self.builder.call(Label("_get_free_memory"), comment="Free_Memory")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture free memory"
            ))
            return result

        # Debug/Trace attributes
        if attr == "source_location":
            # Current source location string
            result = self.builder.new_vreg(IRType.PTR, "_src_loc")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_current_source_loc"),
                comment="Source_Location"
            ))
            return result

        if attr == "source_line":
            # Current source line number
            result = self.builder.new_vreg(IRType.WORD, "_src_line")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(is_global=True, symbol_name="_current_line", ir_type=IRType.WORD),
                comment="Source_Line"
            ))
            return result

        if attr == "source_file":
            # Current source file name
            result = self.builder.new_vreg(IRType.PTR, "_src_file")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_current_file"),
                comment="Source_File"
            ))
            return result

        if attr == "compilation_date":
            # Compilation date string
            result = self.builder.new_vreg(IRType.PTR, "_comp_date")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_compilation_date"),
                comment="Compilation_Date"
            ))
            return result

        if attr == "compilation_time":
            # Compilation time string
            result = self.builder.new_vreg(IRType.PTR, "_comp_time")
            self.builder.emit(IRInstr(
                OpCode.LEA, result, Label("_compilation_time"),
                comment="Compilation_Time"
            ))
            return result

        # Optimization hints
        if attr == "likely":
            # Branch prediction hint - likely
            if expr.args:
                return self._lower_expr(expr.args[0])
            return Immediate(1, IRType.WORD)

        if attr == "unlikely":
            # Branch prediction hint - unlikely
            if expr.args:
                return self._lower_expr(expr.args[0])
            return Immediate(0, IRType.WORD)

        if attr == "cold":
            # Cold code - rarely executed
            return Immediate(0, IRType.WORD)

        if attr == "hot":
            # Hot code - frequently executed
            return Immediate(1, IRType.WORD)

        # Inline assembly support
        if attr == "register":
            # Get register value by name
            result = self.builder.new_vreg(IRType.WORD, "_reg")
            if expr.args and isinstance(expr.args[0], StringLiteral):
                reg_name = expr.args[0].value.upper()
                # Map Ada register names to Z80
                if reg_name in ("A", "BC", "DE", "HL", "SP", "IX", "IY"):
                    self.builder.emit(IRInstr(
                        OpCode.MOV, result,
                        MemoryLocation(is_global=False, symbol_name=f"_{reg_name}", ir_type=IRType.WORD),
                        comment=f"Register {reg_name}"
                    ))
                else:
                    self.builder.mov(result, Immediate(0, IRType.WORD))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "set_register":
            # Set register value
            if len(expr.args) >= 2 and isinstance(expr.args[0], StringLiteral):
                reg_name = expr.args[0].value.upper()
                value = self._lower_expr(expr.args[1])
                if reg_name in ("A", "BC", "DE", "HL", "SP", "IX", "IY"):
                    self.builder.emit(IRInstr(
                        OpCode.STORE,
                        MemoryLocation(is_global=False, symbol_name=f"_{reg_name}", ir_type=IRType.WORD),
                        value,
                        comment=f"Set Register {reg_name}"
                    ))
            return Immediate(0, IRType.WORD)

        # Port I/O (Z80 specific)
        if attr == "port_in":
            # Read from I/O port
            result = self.builder.new_vreg(IRType.WORD, "_port_in")
            if expr.args:
                port = self._lower_expr(expr.args[0])
                self.builder.push(port)
                self.builder.call(Label("_port_in"), comment="Port_In")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture port input"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "port_out":
            # Write to I/O port
            if len(expr.args) >= 2:
                port = self._lower_expr(expr.args[0])
                value = self._lower_expr(expr.args[1])
                self.builder.push(value)
                self.builder.push(port)
                self.builder.call(Label("_port_out"), comment="Port_Out")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
            return Immediate(0, IRType.WORD)

        # Interrupt control (Z80 specific)
        if attr == "interrupts_enabled":
            # Check if interrupts are enabled
            result = self.builder.new_vreg(IRType.WORD, "_int_enabled")
            self.builder.call(Label("_get_interrupt_state"), comment="Interrupts_Enabled")
            self.builder.emit(IRInstr(
                OpCode.MOV, result,
                MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                comment="capture interrupt state"
            ))
            return result

        if attr == "enable_interrupts":
            # Enable interrupts (EI)
            self.builder.emit(IRInstr(OpCode.NOP, comment="EI - enable interrupts"))
            return Immediate(1, IRType.WORD)

        if attr == "disable_interrupts":
            # Disable interrupts (DI)
            self.builder.emit(IRInstr(OpCode.NOP, comment="DI - disable interrupts"))
            return Immediate(0, IRType.WORD)

        # CP/M specific attributes
        if attr == "bdos_call":
            # Make BDOS system call
            result = self.builder.new_vreg(IRType.WORD, "_bdos_result")
            if len(expr.args) >= 1:
                func = self._lower_expr(expr.args[0])
                param = self._lower_expr(expr.args[1]) if len(expr.args) >= 2 else Immediate(0, IRType.WORD)
                self.builder.push(param)
                self.builder.push(func)
                self.builder.call(Label("_bdos"), comment="BDOS_Call")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture BDOS result"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "bios_call":
            # Make BIOS system call
            result = self.builder.new_vreg(IRType.WORD, "_bios_result")
            if len(expr.args) >= 1:
                func = self._lower_expr(expr.args[0])
                self.builder.push(func)
                self.builder.call(Label("_bios"), comment="BIOS_Call")
                temp = self.builder.new_vreg(IRType.WORD, "_discard")
                self.builder.pop(temp)
                self.builder.emit(IRInstr(
                    OpCode.MOV, result,
                    MemoryLocation(is_global=False, symbol_name="_HL", ir_type=IRType.WORD),
                    comment="capture BIOS result"
                ))
            else:
                self.builder.mov(result, Immediate(0, IRType.WORD))
            return result

        if attr == "tpa_start":
            # Transient Program Area start address
            return Immediate(0x0100, IRType.WORD)

        if attr == "tpa_end":
            # TPA end address (from BDOS base)
            result = self.builder.new_vreg(IRType.WORD, "_tpa_end")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(is_global=True, symbol_name="0x0006", ir_type=IRType.WORD),
                comment="TPA_End from BDOS pointer"
            ))
            return result

        if attr == "dma_address":
            # Current DMA buffer address
            result = self.builder.new_vreg(IRType.WORD, "_dma")
            self.builder.emit(IRInstr(
                OpCode.LOAD, result,
                MemoryLocation(is_global=True, symbol_name="_dma_address", ir_type=IRType.WORD),
                comment="DMA_Address"
            ))
            return result

        if attr == "fcb_address":
            # Default FCB address
            return Immediate(0x005C, IRType.WORD)

        if attr == "command_line":
            # Command line buffer address
            return Immediate(0x0080, IRType.WORD)

        # Default
        return Immediate(0, IRType.WORD)

    def _lower_indexed(self, expr: IndexedComponent):
        """Lower an indexed component (array access, function call, or type conversion).

        In Ada, Foo(X) is syntactically ambiguous - it could be array indexing,
        a function call, or a type conversion. We disambiguate here by checking
        what Foo refers to.
        """
        if self.ctx is None:
            return Immediate(0, IRType.WORD)

        # Check if the prefix is actually a function or type (not an array)
        # In Ada, Foo(X) could be either array indexing, function call, or type conversion
        if isinstance(expr.prefix, Identifier):
            func_name = expr.prefix.name.lower()

            # Check symbol table first
            sym = self.symbols.lookup(expr.prefix.name)
            is_function = sym and sym.kind == SymbolKind.FUNCTION
            is_type = sym and sym.kind == SymbolKind.TYPE

            # Also check if this name matches any function in the IR module
            # This handles nested functions and recursive calls where the
            # symbol table scopes don't match during lowering
            if not is_function and self.builder.module:
                for func in self.builder.module.functions:
                    if func.name.lower() == func_name:
                        is_function = True
                        break

            # Check for common type names (Integer, Character, etc.)
            # These are built-in types that may not have explicit symbols
            if not is_function and not is_type:
                builtin_types = {'integer', 'natural', 'positive', 'character',
                                 'boolean', 'float', 'long_float', 'long_long_float',
                                 'duration', 'string', 'wide_character', 'wide_string'}
                if func_name in builtin_types:
                    is_type = True

            # Check for local type declarations in body declarations
            if not is_function and not is_type:
                for decl_list in self._body_declarations_stack:
                    for decl in decl_list:
                        if isinstance(decl, TypeDecl):
                            decl_name = decl.name.lower() if isinstance(decl.name, str) else decl.name.name.lower()
                            if decl_name == func_name:
                                is_type = True
                                break
                    if is_type:
                        break

            if is_type and expr.indices and len(expr.indices) == 1:
                # This is a type conversion: Type(expr)
                # Create a TypeConversion and lower it
                type_conv = TypeConversion(
                    type_mark=expr.prefix,
                    operand=expr.indices[0]
                )
                return self._lower_type_conversion(type_conv)

            if is_function:
                # This is a function call, not array indexing
                # Convert IndexedComponent to FunctionCall
                # Use actual_params if available (preserves named parameter info)
                if expr.actual_params:
                    args = expr.actual_params
                else:
                    args = [ActualParameter(name=None, value=idx) for idx in expr.indices]
                func_call = FunctionCall(name=expr.prefix, args=args)
                return self._lower_function_call(func_call)

        # Handle SelectedName prefix - could be:
        # 1. Package-qualified function call: Ada.Numerics.Elementary_Functions.Sqrt(X)
        # 2. Array field access: X.Values(2) where Values is an array field of record X
        if isinstance(expr.prefix, SelectedName):
            selector = expr.prefix.selector.lower()

            # Check if this is Ada.Numerics.Elementary_Functions.Sqrt for Float64
            if selector == "sqrt" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 sqrt - call _f64_sqrt
                    return self._lower_float64_sqrt(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Sin for Float64
            if selector == "sin" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 sin - call _f64_sin
                    return self._lower_float64_sin(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Cos for Float64
            if selector == "cos" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 cos - call _f64_cos
                    return self._lower_float64_cos(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Tan for Float64
            if selector == "tan" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 tan - call _f64_tan
                    return self._lower_float64_tan(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Cot for Float64
            if selector == "cot" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 cot - inlined as 1/tan(x)
                    return self._lower_float64_cot(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Arctan for Float64
            if selector == "arctan" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Check for two-argument form: Arctan(Y, X)
                    if len(expr.indices) >= 2:
                        x_expr = expr.indices[1]
                        x_type = self._get_expr_type(x_expr)
                        if self._is_float64_type(x_type):
                            # Float64 atan2 - call _f64_atan2(y, x)
                            return self._lower_float64_atan2(arg_expr, x_expr)
                    # Single-argument form: Arctan(X)
                    return self._lower_float64_atan(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Arcsin for Float64
            if selector == "arcsin" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 arcsin - call _f64_asin
                    return self._lower_float64_asin(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Arccos for Float64
            if selector == "arccos" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 arccos - call _f64_acos
                    return self._lower_float64_acos(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Sinh for Float64
            if selector == "sinh" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 sinh - call _f64_sinh
                    return self._lower_float64_sinh(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Cosh for Float64
            if selector == "cosh" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 cosh - call _f64_cosh
                    return self._lower_float64_cosh(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Tanh for Float64
            if selector == "tanh" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 tanh - call _f64_tanh
                    return self._lower_float64_tanh(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Coth for Float64
            if selector == "coth" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 coth - inlined as 1/tanh(x)
                    return self._lower_float64_coth(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Arcsinh for Float64
            if selector == "arcsinh" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 arcsinh - call _f64_asnh
                    return self._lower_float64_arcsinh(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Arccosh for Float64
            if selector == "arccosh" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 arccosh - call _f64_acsh
                    return self._lower_float64_arccosh(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Arctanh for Float64
            if selector == "arctanh" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 arctanh - call _f64_atnh
                    return self._lower_float64_arctanh(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Arccoth for Float64
            if selector == "arccoth" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 arccoth - inlined as 0.5 * log((x+1)/(x-1))
                    return self._lower_float64_arccoth(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Exp for Float64
            if selector == "exp" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 exp - call _f64_e2x
                    return self._lower_float64_exp_func(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Log for Float64
            if selector == "log" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 log - call _f64_log
                    return self._lower_float64_log(arg_expr)

            # Check if this is Ada.Numerics.Elementary_Functions.Log10 for Float64
            if selector == "log10" and expr.indices and len(expr.indices) >= 1:
                arg_expr = expr.indices[0]
                arg_type = self._get_expr_type(arg_expr)
                if self._is_float64_type(arg_type):
                    # Float64 log10 - call _f64_lg10
                    return self._lower_float64_log10(arg_expr)

            # Check if this is a record field that is an array (not a function call)
            # E.g., X.Values(2) where X is a record with array field Values
            from uada80.type_system import RecordType, ArrayType
            is_array_field = False
            rec_prefix = expr.prefix.prefix
            if isinstance(rec_prefix, Identifier):
                var_name = rec_prefix.name.lower()
                if self.ctx and var_name in self.ctx.locals:
                    local = self.ctx.locals[var_name]
                    if local.ada_type:
                        rec_type = self._resolve_local_type(local.ada_type)
                        if rec_type and isinstance(rec_type, RecordType):
                            for comp in rec_type.components:
                                if comp.name.lower() == selector:
                                    if isinstance(comp.component_type, ArrayType):
                                        is_array_field = True
                                    break

            # If it's an array field, fall through to array indexing below
            if not is_array_field:
                # For package-qualified function calls, treat as function call
                # Build the FunctionCall and dispatch
                if expr.actual_params:
                    args = expr.actual_params
                else:
                    args = [ActualParameter(name=None, value=idx) for idx in expr.indices]
                func_call = FunctionCall(name=expr.prefix, args=args)
                return self._lower_function_call(func_call)

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
            # Only arithmetic operations return integer, not CONCAT
            if expr.op != BinaryOp.CONCAT:
                return True

        return False

    def _is_character_type(self, expr) -> bool:
        """Check if expression has Character type."""
        if self.ctx is None:
            return False

        if isinstance(expr, CharacterLiteral):
            return True

        if isinstance(expr, Identifier):
            name = expr.name.lower()
            # Check locals
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                if local.ada_type:
                    # ada_type might be an AST node (SubtypeIndication/Identifier) or AdaType
                    type_name = self._extract_type_name(local.ada_type)
                    if type_name and type_name.lower() in ('character', 'wide_character'):
                        return True
            # Check params
            if name in self.ctx.params:
                param = self.ctx.params[name]
                if hasattr(param, 'ada_type') and param.ada_type:
                    type_name = self._extract_type_name(param.ada_type)
                    if type_name and type_name.lower() in ('character', 'wide_character'):
                        return True

        # Check using _get_expr_type
        expr_type = self._get_expr_type(expr)
        if expr_type:
            type_name = getattr(expr_type, 'name', '').lower()
            if type_name in ('character', 'wide_character'):
                return True

        return False

    def _is_string_type(self, expr) -> bool:
        """Check if expression has string type."""
        # Quick check for obvious string types
        if isinstance(expr, StringLiteral):
            return True

        # Concatenation always produces a string
        if isinstance(expr, BinaryExpr) and expr.op == BinaryOp.CONCAT:
            return True

        # Slice of a string is also a string
        if isinstance(expr, Slice):
            return self._is_string_type(expr.prefix)

        # 'Image attribute always returns string
        if isinstance(expr, AttributeReference):
            if expr.attribute and expr.attribute.lower() == 'image':
                return True

        # Use _get_expr_type to get the actual Ada type
        expr_type = self._get_expr_type(expr)
        if expr_type:
            # Check if type name is String
            type_name = getattr(expr_type, 'name', '').lower()
            if type_name == 'string' or type_name == 'wide_string':
                return True

            # Check if it's an ArrayType with Character component
            if isinstance(expr_type, ArrayType):
                if expr_type.component_type:
                    comp_name = getattr(expr_type.component_type, 'name', '').lower()
                    if comp_name == 'character' or comp_name == 'wide_character':
                        return True

        # Check locals/params if we have context
        if self.ctx is not None and isinstance(expr, Identifier):
            name = expr.name.lower()
            # Check locals
            if name in self.ctx.locals:
                local = self.ctx.locals[name]
                if local.ada_type:
                    # Extract type name from ada_type (could be Identifier or SubtypeIndication)
                    type_name = self._extract_type_name(local.ada_type)
                    if type_name == 'string':
                        return True
                    # Check if it's an array of characters (which is string)
                    if hasattr(local.ada_type, 'is_string') and local.ada_type.is_string:
                        return True
                    # Check component type for array types
                    if hasattr(local.ada_type, 'component_type'):
                        comp_name = getattr(local.ada_type.component_type, 'name', '').lower()
                        if comp_name == 'character':
                            return True
            # Check params
            if name in self.ctx.params:
                param_info = self.ctx.params[name]
                if hasattr(param_info, 'ada_type') and param_info.ada_type:
                    type_name = getattr(param_info.ada_type, 'name', '').lower()
                    if type_name == 'string':
                        return True

        # Handle function calls that return strings
        if isinstance(expr, FunctionCall):
            if isinstance(expr.name, Identifier):
                sym = self.symbols.lookup(expr.name.name) if self.symbols else None
                if sym and sym.return_type:
                    ret_name = getattr(sym.return_type, 'name', '').lower()
                    if ret_name == 'string':
                        return True
                    if isinstance(sym.return_type, ArrayType):
                        if sym.return_type.component_type:
                            comp_name = getattr(sym.return_type.component_type, 'name', '').lower()
                            if comp_name == 'character':
                                return True

        # Handle slices - a slice of a string is a string
        if isinstance(expr, Slice):
            return self._is_string_type(expr.prefix)

        return False

    def _extract_type_name(self, type_node) -> str:
        """Extract the type name from a type node (Identifier or SubtypeIndication)."""
        if type_node is None:
            return ''
        # Direct Identifier: String, Integer, etc.
        if isinstance(type_node, Identifier):
            return type_node.name.lower()
        # SubtypeIndication: has type_mark field
        if isinstance(type_node, SubtypeIndication):
            return self._extract_type_name(type_node.type_mark)
        # Slice (e.g., String(1..10)): extract from prefix
        if isinstance(type_node, Slice):
            return self._extract_type_name(type_node.prefix)
        # SelectedName: Ada.Text_IO.File_Type, etc.
        if isinstance(type_node, SelectedName):
            return self._extract_type_name(type_node.selector)
        # Fallback - try name attribute
        name = getattr(type_node, 'name', '')
        return name.lower() if isinstance(name, str) else ''

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

    def _calc_type_size(self, decl: ObjectDecl, declarations: list = None) -> int:
        """Calculate the size in bytes for a local variable declaration.

        Args:
            decl: The object declaration
            declarations: List of sibling declarations to search for local types
        """
        # Default size for scalar types
        size = 2

        if not hasattr(decl, 'type_mark') or decl.type_mark is None:
            return size

        type_mark = decl.type_mark
        type_name = ""
        constraint = None

        # Handle type mark with constraint: String(1..80)
        if isinstance(type_mark, SubtypeIndication):
            inner = getattr(type_mark, 'type_mark', None)

            # Handle Slice: String(1..80) is parsed as Slice(prefix=String, range_expr=RangeExpr)
            if isinstance(inner, Slice):
                prefix = getattr(inner, 'prefix', None)
                if isinstance(prefix, Identifier) and prefix.name.lower() == 'string':
                    range_expr = getattr(inner, 'range_expr', None)
                    if range_expr:
                        # RangeExpr has 'low' and 'high' attributes
                        first = self._eval_static_expr(getattr(range_expr, 'low', None))
                        last = self._eval_static_expr(getattr(range_expr, 'high', None))
                        if first is not None and last is not None:
                            return last - first + 1
            elif isinstance(inner, Identifier):
                type_name = inner.name.lower()
            elif isinstance(inner, SelectedName):
                type_name = inner.selector.lower()
            elif isinstance(inner, IndexedComponent):
                # Discriminated record: Buffer(10) parsed as IndexedComponent
                if isinstance(inner.prefix, Identifier):
                    type_name = inner.prefix.name.lower()

            constraint = getattr(type_mark, 'constraint', None)
        elif isinstance(type_mark, Identifier):
            type_name = type_mark.name.lower()
        elif isinstance(type_mark, SelectedName):
            type_name = type_mark.selector.lower()
        elif isinstance(type_mark, ArrayTypeDef):
            # Anonymous array type: A : array (1..5) of Integer
            # Calculate size from bounds (RangeExpr already imported at module level)
            if type_mark.index_subtypes:
                total_elem_count = 1
                for idx_range in type_mark.index_subtypes:
                    first = None
                    last = None
                    if isinstance(idx_range, RangeExpr):
                        first = self._eval_static_expr(idx_range.low)
                        last = self._eval_static_expr(idx_range.high)
                    if first is not None and last is not None:
                        total_elem_count *= (last - first + 1)
                # Get element size
                elem_size = 2  # Default
                if type_mark.component_type:
                    comp_name = ''
                    if isinstance(type_mark.component_type, Identifier):
                        comp_name = type_mark.component_type.name.lower()
                    elif hasattr(type_mark.component_type, 'name'):
                        comp_name = type_mark.component_type.name.lower()
                    if comp_name in ('character', 'boolean'):
                        elem_size = 1
                    elif comp_name == 'float':
                        elem_size = 6
                    elif comp_name in ('long_float', 'long_long_float'):
                        elem_size = 8
                return total_elem_count * elem_size

        # Handle String type with constraint (IndexConstraint case)
        if type_name == 'string' and constraint:
            if isinstance(constraint, IndexConstraint) and constraint.ranges:
                range_expr = constraint.ranges[0]
                if isinstance(range_expr, RangeExpr):
                    first = self._eval_static_expr(range_expr.first)
                    last = self._eval_static_expr(range_expr.last)
                    if first is not None and last is not None:
                        return last - first + 1

        # For other array types, try to get size from type definition
        if type_name in ('integer', 'natural', 'positive'):
            return 2
        elif type_name == 'character':
            return 1
        elif type_name == 'boolean':
            return 1
        elif type_name == 'float':
            return 6  # 48-bit float
        elif type_name in ('long_float', 'long_long_float'):
            return 8  # 64-bit IEEE 754 double

        # First try to look up the type in local declarations
        # This handles locally-declared types that aren't in the symbol table yet
        if type_name and declarations:
            for d in declarations:
                if isinstance(d, TypeDecl) and d.name.lower() == type_name:
                    # Found the local type declaration
                    type_def = d.type_def
                    if hasattr(type_def, 'components') or hasattr(type_def, 'fields'):
                        # Record type - sum up actual field sizes
                        # (RangeExpr already imported at module level)
                        fields = getattr(type_def, 'fields', None) or getattr(type_def, 'components', [])
                        if isinstance(fields, dict):
                            total = len(fields) * 2  # Each field is 2 bytes by default
                        else:
                            total = 0
                            # Add discriminants size first (each discriminant is 2 bytes)
                            if hasattr(d, 'discriminants') and d.discriminants:
                                for disc in d.discriminants:
                                    disc_names = getattr(disc, 'names', [])
                                    total += len(disc_names) * 2
                            for comp in fields:
                                # Get field size based on its type
                                field_size = 2  # Default size
                                if hasattr(comp, 'type_mark'):
                                    field_type = comp.type_mark
                                    # Resolve SubtypeIndication
                                    if isinstance(field_type, SubtypeIndication):
                                        field_type = getattr(field_type, 'type_mark', field_type)
                                    # Get field type name
                                    field_type_name = None
                                    if isinstance(field_type, Identifier):
                                        field_type_name = field_type.name.lower()
                                    # Look up field type in local declarations
                                    if field_type_name:
                                        for fd in declarations:
                                            if isinstance(fd, TypeDecl) and fd.name.lower() == field_type_name:
                                                # Check if it's an array type
                                                field_typedef = fd.type_def
                                                if hasattr(field_typedef, 'index_subtypes') and field_typedef.index_subtypes:
                                                    # Calculate array size
                                                    elem_count = 1
                                                    for idx_range in field_typedef.index_subtypes:
                                                        if isinstance(idx_range, RangeExpr):
                                                            first = self._eval_static_expr(idx_range.low)
                                                            last = self._eval_static_expr(idx_range.high)
                                                            if first is not None and last is not None:
                                                                elem_count *= (last - first + 1)
                                                    field_size = elem_count * 2  # Each element is 2 bytes
                                                break
                                # Multiply by number of names in this component
                                num_fields = len(comp.names) if hasattr(comp, 'names') and isinstance(comp.names, list) else 1
                                total += field_size * num_fields
                        return max(total, 2)
                    elif hasattr(type_def, 'index_subtypes'):
                        # ArrayTypeDef - calculate size from ALL index ranges
                        # (ArrayTypeDef and RangeExpr already imported at module level)
                        if isinstance(type_def, ArrayTypeDef) and type_def.index_subtypes:
                            total_elem_count = 1
                            for idx_range in type_def.index_subtypes:
                                first = None
                                last = None
                                # Handle RangeExpr (has 'low' and 'high' attributes)
                                if isinstance(idx_range, RangeExpr):
                                    first = self._eval_static_expr(idx_range.low)
                                    last = self._eval_static_expr(idx_range.high)
                                else:
                                    # Check for 'low'/'high' or 'first'/'last' attributes
                                    first = self._eval_static_expr(getattr(idx_range, 'low', None) or
                                                                   getattr(idx_range, 'first', None))
                                    last = self._eval_static_expr(getattr(idx_range, 'high', None) or
                                                                  getattr(idx_range, 'last', None))
                                if first is not None and last is not None:
                                    total_elem_count *= (last - first + 1)
                            # Get element size
                            elem_size = 2  # Default
                            if type_def.component_type:
                                comp_name = ''
                                if isinstance(type_def.component_type, Identifier):
                                    comp_name = type_def.component_type.name.lower()
                                elif hasattr(type_def.component_type, 'name'):
                                    comp_name = type_def.component_type.name.lower()
                                if comp_name in ('character', 'boolean'):
                                    elem_size = 1
                                elif comp_name == 'float':
                                    elem_size = 6
                            return total_elem_count * elem_size
                    break

        # Try to look up the type in symbol table
        if type_name:
            type_sym = self.symbols.lookup(type_name) if self.symbols else None
            if type_sym and type_sym.ada_type:
                from uada80.type_system import RecordType, ArrayType, EnumerationType
                ada_type = type_sym.ada_type

                if isinstance(ada_type, RecordType):
                    # Calculate total record size from fields
                    total = 0
                    for field_name, field_type in ada_type.fields.items():
                        # Get field size (default to 2 for unknown types)
                        field_size = 2
                        if hasattr(field_type, 'name'):
                            fn = field_type.name.lower()
                            if fn in ('character', 'boolean'):
                                field_size = 1
                            elif fn == 'float':
                                field_size = 6
                        total += field_size
                    return max(total, 2)  # At least 2 bytes

                elif isinstance(ada_type, ArrayType):
                    # For constrained arrays, calculate element_count * element_size
                    if ada_type.index_types:
                        idx_type = ada_type.index_types[0]
                        if hasattr(idx_type, 'first') and hasattr(idx_type, 'last'):
                            first = idx_type.first
                            last = idx_type.last
                            if isinstance(first, int) and isinstance(last, int):
                                elem_count = last - first + 1
                                elem_size = 2  # Default element size
                                if ada_type.element_type:
                                    et_name = getattr(ada_type.element_type, 'name', '').lower()
                                    if et_name in ('character', 'boolean'):
                                        elem_size = 1
                                return elem_count * elem_size

                elif isinstance(ada_type, EnumerationType):
                    return 1  # Enumeration fits in a byte

        return size

    def _eval_static_expr(self, expr) -> int | None:
        """Evaluate a static expression to an integer value."""
        if isinstance(expr, IntegerLiteral):
            return expr.value
        if isinstance(expr, Identifier):
            # Try to look up named number
            name = expr.name.lower()
            sym = self.symbols.lookup(name) if self.symbols else None
            if sym and hasattr(sym, 'value') and isinstance(sym.value, int):
                return sym.value
        if isinstance(expr, UnaryExpr):
            # Handle unary operators (especially for negative numbers)
            operand = self._eval_static_expr(expr.operand)
            if operand is not None:
                if expr.op == UnaryOp.MINUS:
                    return -operand
                if expr.op == UnaryOp.PLUS:
                    return operand
                if expr.op == UnaryOp.ABS:
                    return abs(operand)
            return None
        if isinstance(expr, BinaryExpr):
            # Handle binary expressions
            left = self._eval_static_expr(expr.left)
            right = self._eval_static_expr(expr.right)
            if left is not None and right is not None:
                if expr.op == BinaryOp.ADD:
                    return left + right
                if expr.op == BinaryOp.SUB:
                    return left - right
                if expr.op == BinaryOp.MUL:
                    return left * right
                if expr.op == BinaryOp.DIV:
                    return left // right if right != 0 else None
                if expr.op == BinaryOp.MOD:
                    return left % right if right != 0 else None
            return None
        return None

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
                # Also check size stored in LocalVariable
                if local.size > 2:
                    return local.size

        return 80  # Default buffer size

    # --- Stub methods for features not yet implemented ---

    def _setup_parameters(self, params: list) -> None:
        """Set up parameter locals from parameter list (stub)."""
        # TODO: Implement parameter setup for subprogram bodies
        pass

    def _lower_exception_handlers(self, handlers: list) -> None:
        """Lower exception handlers (stub)."""
        # TODO: Implement exception handler lowering
        pass

    def _get_constant_value(self, expr) -> Optional[int]:
        """Get compile-time constant value from expression (stub)."""
        # TODO: Implement constant evaluation
        if hasattr(expr, 'value') and isinstance(expr.value, int):
            return expr.value
        return None

    def _get_unique_label(self, prefix: str) -> str:
        """Get a unique label with given prefix."""
        return self.builder.new_label(prefix)

    def _get_exception_name(self, exc_id: int) -> str:
        """Get exception name from ID (stub)."""
        # TODO: Implement reverse lookup of exception names
        for name, eid in self._exception_ids.items():
            if eid == exc_id:
                return name
        return f"exception_{exc_id}"

    def _lower_decl(self, decl) -> None:
        """Lower a declaration (alias for _lower_declaration)."""
        self._lower_declaration(decl)

    def _load_global(self, name: str) -> VReg:
        """Load a global variable value (stub)."""
        result = self.builder.new_vreg(IRType.WORD, f"_{name}")
        self.builder.load(
            result,
            MemoryLocation(is_global=True, symbol_name=name, ir_type=IRType.WORD)
        )
        return result

    def _lower_indexed_load(self, target: IndexedComponent) -> VReg:
        """Load value from array element (stub)."""
        # Get array base address
        base = self._lower_expr(target.prefix)
        # Get index
        if target.args:
            idx = self._lower_expr(target.args[0])
        else:
            idx = Immediate(0, IRType.WORD)
        # Calculate offset (assume 2-byte elements)
        offset = self.builder.new_vreg(IRType.WORD, "_arr_offset")
        self.builder.mul(offset, idx, Immediate(2, IRType.WORD))
        # Add to base
        addr = self.builder.new_vreg(IRType.PTR, "_elem_addr")
        self.builder.add(addr, base, offset)
        # Load value
        result = self.builder.new_vreg(IRType.WORD, "_elem_val")
        self.builder.load(result, MemoryLocation(base=addr, offset=0, ir_type=IRType.WORD))
        return result

    def _lower_selected_load(self, target: SelectedName) -> VReg:
        """Load value from record field (stub)."""
        # Get record base address
        base = self._lower_expr(target.prefix)
        # For now, assume first field at offset 0
        result = self.builder.new_vreg(IRType.WORD, f"_{target.selector}")
        self.builder.load(result, MemoryLocation(base=base, offset=0, ir_type=IRType.WORD))
        return result


def lower_to_ir(program: Program, semantic_result: SemanticResult) -> IRModule:
    """Lower a program to IR."""
    lowering = ASTLowering(semantic_result.symbols)
    return lowering.lower(program)
