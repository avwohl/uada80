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
from uada80.type_system import AdaType, TypeKind, PREDEFINED_TYPES
from uada80.semantic import SemanticResult


@dataclass
class LocalVariable:
    """Information about a local variable."""

    name: str
    vreg: VReg
    stack_offset: int
    size: int


@dataclass
class LoweringContext:
    """Context for lowering a function."""

    function: IRFunction
    locals: dict[str, LocalVariable] = field(default_factory=dict)
    params: dict[str, VReg] = field(default_factory=dict)
    loop_exit_label: Optional[str] = None  # For exit statements
    loop_continue_label: Optional[str] = None  # For continue


class ASTLowering:
    """Lowers AST to IR."""

    def __init__(self, symbols: SymbolTable) -> None:
        self.symbols = symbols
        self.builder = IRBuilder()
        self.ctx: Optional[LoweringContext] = None
        self._label_counter = 0

    def lower(self, program: Program) -> IRModule:
        """Lower an entire program to IR."""
        module = self.builder.new_module("main")

        for unit in program.units:
            self._lower_compilation_unit(unit)

        return module

    def _new_label(self, prefix: str = "L") -> str:
        """Generate a unique label."""
        name = f"{prefix}{self._label_counter}"
        self._label_counter += 1
        return name

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

    def _lower_subprogram_body(self, body: SubprogramBody) -> None:
        """Lower a subprogram body."""
        spec = body.spec

        # Determine return type
        return_type = IRType.VOID
        if spec.is_function and spec.return_type:
            sym = self.symbols.lookup(spec.name)
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

        # Process declarations (initializations)
        for decl in body.declarations:
            self._lower_declaration(decl)

        # Process statements
        for stmt in body.statements:
            self._lower_statement(stmt)

        # Add implicit return if needed
        if not self._block_has_return(self.builder.block):
            if return_type == IRType.VOID:
                self.builder.ret()

        self.ctx = None

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
        # For now, just process any subprogram declarations
        for decl in pkg.declarations:
            if isinstance(decl, SubprogramBody):
                self._lower_subprogram_body(decl)

    def _lower_package_body(self, body: PackageBody) -> None:
        """Lower a package body."""
        for decl in body.declarations:
            if isinstance(decl, SubprogramBody):
                self._lower_subprogram_body(decl)

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

        # Process initialization
        if decl.init_expr:
            init_value = self._lower_expr(decl.init_expr)

            for name in decl.names:
                local = self.ctx.locals.get(name.lower())
                if local:
                    self.builder.mov(local.vreg, init_value,
                                    comment=f"init {name}")

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

        self.ctx.loop_exit_label = old_exit

    def _lower_block(self, stmt: BlockStmt) -> None:
        """Lower a block statement."""
        if self.ctx is None:
            return

        # Process declarations
        for decl in stmt.declarations:
            self._lower_declaration(decl)

        # Process statements
        for s in stmt.statements:
            self._lower_statement(s)

    def _lower_exit(self, stmt: ExitStmt) -> None:
        """Lower an exit statement."""
        if self.ctx is None or self.ctx.loop_exit_label is None:
            return

        if stmt.condition:
            # exit when condition
            cond = self._lower_expr(stmt.condition)
            self.builder.jnz(cond, Label(self.ctx.loop_exit_label))
        else:
            # unconditional exit
            self.builder.jmp(Label(self.ctx.loop_exit_label))

    def _lower_return(self, stmt: ReturnStmt) -> None:
        """Lower a return statement."""
        if stmt.value:
            value = self._lower_expr(stmt.value)
            self.builder.ret(value)
        else:
            self.builder.ret()

    def _lower_procedure_call(self, stmt: ProcedureCallStmt) -> None:
        """Lower a procedure call."""
        if isinstance(stmt.name, Identifier):
            # Push arguments in reverse order
            for arg in reversed(stmt.args):
                if arg.value:
                    value = self._lower_expr(arg.value)
                    self.builder.push(value)

            # Call
            self.builder.call(Label(stmt.name.name))

            # Clean up stack
            if stmt.args:
                # Pop arguments (2 bytes each)
                for _ in stmt.args:
                    temp = self.builder.new_vreg(IRType.WORD, "_discard")
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
        # For now, just generate a placeholder
        pass

    # =========================================================================
    # Expressions
    # =========================================================================

    def _lower_expr(self, expr: Expr):
        """Lower an expression and return the result vreg or immediate."""
        if isinstance(expr, IntegerLiteral):
            return Immediate(expr.value, IRType.WORD)

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

        # Default: return 0
        return Immediate(0, IRType.WORD)

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
            # ABS: if negative, negate
            self.builder.mov(result, operand)
            cond = self.builder.new_vreg(IRType.BOOL)
            self.builder.cmp_lt(cond, operand, Immediate(0, IRType.WORD))
            # This is simplified - real ABS needs conditional
        elif op == UnaryOp.PLUS:
            self.builder.mov(result, operand)
        else:
            self.builder.mov(result, operand)

        return result

    def _lower_function_call(self, expr: FunctionCall):
        """Lower a function call expression."""
        result = self.builder.new_vreg(IRType.WORD, "_result")

        if isinstance(expr.name, Identifier):
            # Push arguments
            for arg in reversed(expr.args):
                if arg.value:
                    value = self._lower_expr(arg.value)
                    self.builder.push(value)

            # Call
            self.builder.call(Label(expr.name.name))

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

        # For type attributes, return compile-time constant
        if isinstance(expr.prefix, Identifier):
            sym = self.symbols.lookup(expr.prefix.name)
            if sym and sym.kind == SymbolKind.TYPE and sym.ada_type:
                t = sym.ada_type
                if attr == "first" and hasattr(t, "low"):
                    return Immediate(t.low, IRType.WORD)
                if attr == "last" and hasattr(t, "high"):
                    return Immediate(t.high, IRType.WORD)
                if attr == "size":
                    return Immediate(t.size_bits, IRType.WORD)

        # Default
        return Immediate(0, IRType.WORD)

    def _lower_indexed(self, expr: IndexedComponent):
        """Lower an indexed component (array access)."""
        # Simplified: just return a temp register
        result = self.builder.new_vreg(IRType.WORD, "_elem")
        return result


def lower_to_ir(program: Program, semantic_result: SemanticResult) -> IRModule:
    """Lower a program to IR."""
    lowering = ASTLowering(semantic_result.symbols)
    return lowering.lower(program)
