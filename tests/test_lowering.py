"""Tests for AST to IR lowering."""

import pytest
from uada80.ast_nodes import (
    Program,
    CompilationUnit,
    SubprogramBody,
    SubprogramDecl,
    ObjectDecl,
    ParameterSpec,
    Identifier,
    IntegerLiteral,
    BinaryExpr,
    UnaryExpr,
    BinaryOp,
    UnaryOp,
    AssignmentStmt,
    IfStmt,
    LoopStmt,
    WhileScheme,
    ForScheme,
    IteratorSpec,
    RangeExpr,
    ReturnStmt,
    NullStmt,
    ExitStmt,
    ProcedureCallStmt,
    ActualParameter,
    FunctionCall,
    SubtypeIndication,
)
from uada80.ir import IRType, OpCode, VReg, Immediate
from uada80.symbol_table import SymbolTable, Symbol, SymbolKind
from uada80.type_system import PREDEFINED_TYPES
from uada80.semantic import SemanticResult
from uada80.lowering import ASTLowering, lower_to_ir


# ============================================================================
# Helper Functions
# ============================================================================


def int_lit(value: int) -> IntegerLiteral:
    """Create an IntegerLiteral with value and text."""
    return IntegerLiteral(value=value, text=str(value))


def subtype(name: str) -> SubtypeIndication:
    """Create a SubtypeIndication from a type name."""
    return SubtypeIndication(type_mark=Identifier(name))


def create_simple_function(name: str, stmts: list, decls: list = None) -> Program:
    """Create a simple function for testing."""
    spec = SubprogramDecl(
        name=name,
        is_function=True,
        return_type=Identifier("Integer"),
        parameters=[],
    )
    body = SubprogramBody(
        spec=spec,
        declarations=decls or [],
        statements=stmts,
    )
    unit = CompilationUnit(unit=body)
    return Program(units=[unit])


def create_procedure(name: str, stmts: list, decls: list = None) -> Program:
    """Create a simple procedure for testing."""
    spec = SubprogramDecl(
        name=name,
        is_function=False,
        return_type=None,
        parameters=[],
    )
    body = SubprogramBody(
        spec=spec,
        declarations=decls or [],
        statements=stmts,
    )
    unit = CompilationUnit(unit=body)
    return Program(units=[unit])


def create_lowering() -> ASTLowering:
    """Create a lowering instance with initialized symbol table."""
    symbols = SymbolTable()
    return ASTLowering(symbols)


# ============================================================================
# Basic Lowering Tests
# ============================================================================


def test_lowering_empty_function():
    """Test lowering an empty function."""
    program = create_simple_function("empty", [ReturnStmt(value=int_lit(0))])
    lowering = create_lowering()

    module = lowering.lower(program)

    assert module is not None
    assert module.name == "main"
    assert len(module.functions) == 1
    assert module.functions[0].name == "empty"


def test_lowering_empty_procedure():
    """Test lowering an empty procedure."""
    program = create_procedure("do_nothing", [NullStmt()])
    lowering = create_lowering()

    module = lowering.lower(program)

    assert module is not None
    assert len(module.functions) == 1
    assert module.functions[0].return_type == IRType.VOID


def test_lowering_return_literal():
    """Test lowering a function that returns a literal."""
    program = create_simple_function("get_42", [ReturnStmt(value=int_lit(42))])
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    assert func.entry_block is not None

    # Should have RET instruction
    has_ret = False
    for block in func.blocks:
        for instr in block.instructions:
            if instr.opcode == OpCode.RET:
                has_ret = True
    assert has_ret


def test_lowering_return_expression():
    """Test lowering a function that returns an expression."""
    expr = BinaryExpr(
        left=int_lit(10),
        op=BinaryOp.ADD,
        right=int_lit(20),
    )
    program = create_simple_function("add", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    # Should have ADD instruction
    has_add = False
    for block in func.blocks:
        for instr in block.instructions:
            if instr.opcode == OpCode.ADD:
                has_add = True
    assert has_add


# ============================================================================
# Variable Declaration Tests
# ============================================================================


def test_lowering_local_variable():
    """Test lowering local variable declaration."""
    decl = ObjectDecl(
        names=["X"],
        type_mark=subtype("Integer"),
        init_expr=int_lit(100),
    )
    program = create_simple_function(
        "test",
        [ReturnStmt(value=int_lit(0))],
        decls=[decl],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    assert func.locals_size >= 2  # At least 2 bytes for Integer


def test_lowering_multiple_locals():
    """Test lowering multiple local variables."""
    decl1 = ObjectDecl(names=["A"], type_mark=subtype("Integer"), init_expr=int_lit(1))
    decl2 = ObjectDecl(names=["B"], type_mark=subtype("Integer"), init_expr=int_lit(2))
    program = create_simple_function(
        "test",
        [ReturnStmt(value=int_lit(0))],
        decls=[decl1, decl2],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    assert func.locals_size >= 4  # At least 4 bytes for two Integers


# ============================================================================
# Assignment Statement Tests
# ============================================================================


def test_lowering_assignment():
    """Test lowering assignment statement."""
    decl = ObjectDecl(names=["X"], type_mark=subtype("Integer"))
    assign = AssignmentStmt(
        target=Identifier("X"),
        value=int_lit(42),
    )
    program = create_simple_function(
        "test",
        [assign, ReturnStmt(value=int_lit(0))],
        decls=[decl],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    # Should have MOV instruction
    has_mov = False
    for block in func.blocks:
        for instr in block.instructions:
            if instr.opcode == OpCode.MOV:
                has_mov = True
    assert has_mov


def test_lowering_assignment_expression():
    """Test lowering assignment with expression."""
    decl = ObjectDecl(names=["X"], type_mark=subtype("Integer"))
    assign = AssignmentStmt(
        target=Identifier("X"),
        value=BinaryExpr(
            left=int_lit(10),
            op=BinaryOp.MUL,
            right=int_lit(5),
        ),
    )
    program = create_simple_function(
        "test",
        [assign, ReturnStmt(value=int_lit(0))],
        decls=[decl],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    has_mul = any(
        instr.opcode == OpCode.MUL
        for block in func.blocks
        for instr in block.instructions
    )
    assert has_mul


# ============================================================================
# If Statement Tests
# ============================================================================


def test_lowering_if_statement():
    """Test lowering if statement."""
    condition = BinaryExpr(
        left=int_lit(1),
        op=BinaryOp.GT,
        right=int_lit(0),
    )
    if_stmt = IfStmt(
        condition=condition,
        then_stmts=[ReturnStmt(value=int_lit(1))],
        elsif_parts=[],
        else_stmts=[ReturnStmt(value=int_lit(0))],
    )
    program = create_simple_function("test", [if_stmt])
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    # Should have multiple blocks
    assert len(func.blocks) >= 3  # entry, then, else, end

    # Should have conditional jump
    has_jz = any(
        instr.opcode == OpCode.JZ
        for block in func.blocks
        for instr in block.instructions
    )
    assert has_jz


def test_lowering_if_without_else():
    """Test lowering if statement without else."""
    condition = BinaryExpr(
        left=int_lit(1),
        op=BinaryOp.EQ,
        right=int_lit(1),
    )
    if_stmt = IfStmt(
        condition=condition,
        then_stmts=[NullStmt()],
        elsif_parts=[],
        else_stmts=[],
    )
    program = create_simple_function(
        "test",
        [if_stmt, ReturnStmt(value=int_lit(0))],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    assert module is not None
    assert len(module.functions[0].blocks) >= 2


# ============================================================================
# Loop Statement Tests
# ============================================================================


def test_lowering_simple_loop():
    """Test lowering simple (infinite) loop with exit."""
    exit_stmt = ExitStmt(
        loop_label=None,
        condition=BinaryExpr(
            left=Identifier("True"),
            op=BinaryOp.EQ,
            right=Identifier("True"),
        ),
    )
    loop = LoopStmt(
        label=None,
        iteration_scheme=None,  # Simple loop
        statements=[exit_stmt],
    )
    program = create_simple_function(
        "test",
        [loop, ReturnStmt(value=int_lit(0))],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    # Should have JMP instruction for loop back
    has_jmp = any(
        instr.opcode == OpCode.JMP
        for block in func.blocks
        for instr in block.instructions
    )
    assert has_jmp


def test_lowering_while_loop():
    """Test lowering while loop."""
    condition = BinaryExpr(
        left=int_lit(1),
        op=BinaryOp.LT,
        right=int_lit(10),
    )
    loop = LoopStmt(
        label=None,
        iteration_scheme=WhileScheme(condition=condition),
        statements=[NullStmt()],
    )
    program = create_simple_function(
        "test",
        [loop, ReturnStmt(value=int_lit(0))],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    # Should have comparison and conditional jump
    has_cmp = any(
        instr.opcode == OpCode.CMP_LT
        for block in func.blocks
        for instr in block.instructions
    )
    assert has_cmp


def test_lowering_for_loop():
    """Test lowering for loop."""
    iterator = IteratorSpec(
        name="I",
        is_reverse=False,
        iterable=RangeExpr(
            low=int_lit(1),
            high=int_lit(10),
        ),
    )
    loop = LoopStmt(
        label=None,
        iteration_scheme=ForScheme(iterator=iterator),
        statements=[NullStmt()],
    )
    program = create_simple_function(
        "test",
        [loop, ReturnStmt(value=int_lit(0))],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    # Should have increment (ADD)
    has_add = any(
        instr.opcode == OpCode.ADD
        for block in func.blocks
        for instr in block.instructions
    )
    assert has_add


def test_lowering_reverse_for_loop():
    """Test lowering reverse for loop."""
    iterator = IteratorSpec(
        name="I",
        is_reverse=True,
        iterable=RangeExpr(
            low=int_lit(1),
            high=int_lit(10),
        ),
    )
    loop = LoopStmt(
        label=None,
        iteration_scheme=ForScheme(iterator=iterator),
        statements=[NullStmt()],
    )
    program = create_simple_function(
        "test",
        [loop, ReturnStmt(value=int_lit(0))],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    # Should have decrement (SUB)
    has_sub = any(
        instr.opcode == OpCode.SUB
        for block in func.blocks
        for instr in block.instructions
    )
    assert has_sub


# ============================================================================
# Expression Tests
# ============================================================================


def test_lowering_binary_add():
    """Test lowering binary addition."""
    expr = BinaryExpr(left=int_lit(5), op=BinaryOp.ADD, right=int_lit(3))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_add = any(
        instr.opcode == OpCode.ADD
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_add


def test_lowering_binary_sub():
    """Test lowering binary subtraction."""
    expr = BinaryExpr(left=int_lit(10), op=BinaryOp.SUB, right=int_lit(3))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_sub = any(
        instr.opcode == OpCode.SUB
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_sub


def test_lowering_binary_mul():
    """Test lowering binary multiplication."""
    expr = BinaryExpr(left=int_lit(4), op=BinaryOp.MUL, right=int_lit(5))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_mul = any(
        instr.opcode == OpCode.MUL
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_mul


def test_lowering_binary_div():
    """Test lowering binary division."""
    expr = BinaryExpr(left=int_lit(20), op=BinaryOp.DIV, right=int_lit(4))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_div = any(
        instr.opcode == OpCode.DIV
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_div


def test_lowering_binary_and():
    """Test lowering binary AND."""
    expr = BinaryExpr(left=int_lit(0xFF), op=BinaryOp.AND, right=int_lit(0x0F))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_and = any(
        instr.opcode == OpCode.AND
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_and


def test_lowering_binary_or():
    """Test lowering binary OR."""
    expr = BinaryExpr(left=int_lit(0xF0), op=BinaryOp.OR, right=int_lit(0x0F))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_or = any(
        instr.opcode == OpCode.OR
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_or


def test_lowering_binary_xor():
    """Test lowering binary XOR."""
    expr = BinaryExpr(left=int_lit(0xFF), op=BinaryOp.XOR, right=int_lit(0x0F))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_xor = any(
        instr.opcode == OpCode.XOR
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_xor


def test_lowering_comparison_eq():
    """Test lowering equality comparison."""
    expr = BinaryExpr(left=int_lit(1), op=BinaryOp.EQ, right=int_lit(1))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_cmp_eq = any(
        instr.opcode == OpCode.CMP_EQ
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_cmp_eq


def test_lowering_comparison_lt():
    """Test lowering less-than comparison."""
    expr = BinaryExpr(left=int_lit(1), op=BinaryOp.LT, right=int_lit(2))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_cmp_lt = any(
        instr.opcode == OpCode.CMP_LT
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_cmp_lt


def test_lowering_unary_minus():
    """Test lowering unary minus."""
    expr = UnaryExpr(op=UnaryOp.MINUS, operand=int_lit(42))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_neg = any(
        instr.opcode == OpCode.NEG
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_neg


def test_lowering_unary_not():
    """Test lowering unary NOT."""
    expr = UnaryExpr(op=UnaryOp.NOT, operand=int_lit(0xFF))
    program = create_simple_function("test", [ReturnStmt(value=expr)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_not = any(
        instr.opcode == OpCode.NOT
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_not


# ============================================================================
# Procedure Call Tests
# ============================================================================


def test_lowering_procedure_call():
    """Test lowering procedure call."""
    call = ProcedureCallStmt(
        name=Identifier("Print"),
        args=[ActualParameter(value=int_lit(42))],
    )
    program = create_procedure("test", [call])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_call = any(
        instr.opcode == OpCode.CALL
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_call


def test_lowering_procedure_call_no_args():
    """Test lowering procedure call without arguments."""
    call = ProcedureCallStmt(
        name=Identifier("Do_Something"),
        args=[],
    )
    program = create_procedure("test", [call])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_call = any(
        instr.opcode == OpCode.CALL
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_call


# ============================================================================
# Function Call Expression Tests
# ============================================================================


def test_lowering_function_call():
    """Test lowering function call expression."""
    call = FunctionCall(
        name=Identifier("Get_Value"),
        args=[],
    )
    program = create_simple_function("test", [ReturnStmt(value=call)])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_call = any(
        instr.opcode == OpCode.CALL
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_call


def test_lowering_function_call_with_args():
    """Test lowering function call with arguments."""
    call = FunctionCall(
        name=Identifier("Add"),
        args=[
            ActualParameter(value=int_lit(10)),
            ActualParameter(value=int_lit(20)),
        ],
    )
    program = create_simple_function("test", [ReturnStmt(value=call)])
    lowering = create_lowering()

    module = lowering.lower(program)

    # Should have PUSH for args
    has_push = any(
        instr.opcode == OpCode.PUSH
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_push


# ============================================================================
# Type Conversion Tests
# ============================================================================


def test_ada_type_to_ir_integer():
    """Test Ada to IR type conversion for Integer."""
    lowering = create_lowering()
    integer_type = PREDEFINED_TYPES["Integer"]

    ir_type = lowering._ada_type_to_ir(integer_type)

    assert ir_type == IRType.WORD


def test_ada_type_to_ir_boolean():
    """Test Ada to IR type conversion for Boolean."""
    lowering = create_lowering()
    bool_type = PREDEFINED_TYPES["Boolean"]

    ir_type = lowering._ada_type_to_ir(bool_type)

    assert ir_type == IRType.BOOL


def test_ada_type_to_ir_none():
    """Test Ada to IR type conversion for None (void)."""
    lowering = create_lowering()

    ir_type = lowering._ada_type_to_ir(None)

    assert ir_type == IRType.WORD  # Default


# ============================================================================
# Complex Program Tests
# ============================================================================


def test_lowering_factorial():
    """Test lowering a factorial-like function."""
    # Simple version: if n <= 1 return 1 else return n
    decl = ObjectDecl(names=["N"], type_mark=subtype("Integer"), init_expr=int_lit(5))
    condition = BinaryExpr(
        left=Identifier("N"),
        op=BinaryOp.LE,
        right=int_lit(1),
    )
    if_stmt = IfStmt(
        condition=condition,
        then_stmts=[ReturnStmt(value=int_lit(1))],
        elsif_parts=[],
        else_stmts=[ReturnStmt(value=Identifier("N"))],
    )
    program = create_simple_function("factorial", [if_stmt], decls=[decl])
    lowering = create_lowering()

    module = lowering.lower(program)

    func = module.functions[0]
    assert func.name == "factorial"
    assert len(func.blocks) >= 3


def test_lowering_sum_loop():
    """Test lowering a sum computation with loop."""
    sum_decl = ObjectDecl(names=["Sum"], type_mark=subtype("Integer"), init_expr=int_lit(0))

    # Loop body: Sum := Sum + I
    assign = AssignmentStmt(
        target=Identifier("Sum"),
        value=BinaryExpr(
            left=Identifier("Sum"),
            op=BinaryOp.ADD,
            right=Identifier("I"),
        ),
    )

    iterator = IteratorSpec(
        name="I",
        is_reverse=False,
        iterable=RangeExpr(low=int_lit(1), high=int_lit(10)),
    )
    loop = LoopStmt(
        label=None,
        iteration_scheme=ForScheme(iterator=iterator),
        statements=[assign],
    )

    program = create_simple_function(
        "sum_1_to_10",
        [loop, ReturnStmt(value=Identifier("Sum"))],
        decls=[sum_decl],
    )
    lowering = create_lowering()

    module = lowering.lower(program)

    # Should have ADD instructions
    add_count = sum(
        1 for block in module.functions[0].blocks
        for instr in block.instructions
        if instr.opcode == OpCode.ADD
    )
    assert add_count >= 2  # Loop increment + sum update


# ============================================================================
# lower_to_ir API Tests
# ============================================================================


def test_lower_to_ir_api():
    """Test the lower_to_ir convenience function."""
    program = create_simple_function("test", [ReturnStmt(value=int_lit(0))])
    symbols = SymbolTable()
    semantic_result = SemanticResult(symbols=symbols, errors=[])

    module = lower_to_ir(program, semantic_result)

    assert module is not None
    assert len(module.functions) == 1


def test_unique_labels():
    """Test that generated labels are unique."""
    program = create_simple_function("test", [
        IfStmt(
            condition=BinaryExpr(left=int_lit(1), op=BinaryOp.GT, right=int_lit(0)),
            then_stmts=[NullStmt()],
            elsif_parts=[],
            else_stmts=[],
        ),
        IfStmt(
            condition=BinaryExpr(left=int_lit(2), op=BinaryOp.GT, right=int_lit(0)),
            then_stmts=[NullStmt()],
            elsif_parts=[],
            else_stmts=[],
        ),
        ReturnStmt(value=int_lit(0)),
    ])
    lowering = create_lowering()

    module = lowering.lower(program)

    # Collect all block labels
    labels = [block.label for block in module.functions[0].blocks]
    # All labels should be unique
    assert len(labels) == len(set(labels))


# ============================================================================
# Exception Handling Tests
# ============================================================================


def test_lowering_raise_statement():
    """Test lowering a raise statement."""
    from uada80.ast_nodes import RaiseStmt

    raise_stmt = RaiseStmt(exception_name=Identifier("Constraint_Error"))
    program = create_procedure("test", [raise_stmt])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_exc_raise = any(
        instr.opcode == OpCode.EXC_RAISE
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_exc_raise


def test_lowering_reraise_statement():
    """Test lowering a re-raise statement (raise without exception name)."""
    from uada80.ast_nodes import RaiseStmt

    raise_stmt = RaiseStmt(exception_name=None)  # re-raise
    program = create_procedure("test", [raise_stmt])
    lowering = create_lowering()

    module = lowering.lower(program)

    has_exc_reraise = any(
        instr.opcode == OpCode.EXC_RERAISE
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_exc_reraise


def test_lowering_exception_handler():
    """Test lowering a block with exception handlers."""
    from uada80.ast_nodes import BlockStmt, ExceptionHandler

    # Create a block with a handler
    handler = ExceptionHandler(
        exception_names=[Identifier("Constraint_Error")],
        statements=[NullStmt()],
    )
    block_stmt = BlockStmt(
        label=None,
        declarations=[],
        statements=[NullStmt()],
        handled_exception_handlers=[handler],
    )
    program = create_procedure("test", [block_stmt])
    lowering = create_lowering()

    module = lowering.lower(program)

    # Should have EXC_PUSH and EXC_POP
    has_exc_push = any(
        instr.opcode == OpCode.EXC_PUSH
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    has_exc_pop = any(
        instr.opcode == OpCode.EXC_POP
        for block in module.functions[0].blocks
        for instr in block.instructions
    )
    assert has_exc_push
    assert has_exc_pop


def test_lowering_exception_others_handler():
    """Test lowering an 'others' exception handler."""
    from uada80.ast_nodes import BlockStmt, ExceptionHandler

    # Create a block with "when others =>" handler
    handler = ExceptionHandler(
        exception_names=[Identifier("others")],
        statements=[NullStmt()],
    )
    block_stmt = BlockStmt(
        label=None,
        declarations=[],
        statements=[NullStmt()],
        handled_exception_handlers=[handler],
    )
    program = create_procedure("test", [block_stmt])
    lowering = create_lowering()

    module = lowering.lower(program)

    # Find EXC_PUSH instruction and verify it uses ID 0 (catch all)
    exc_push_instrs = [
        instr
        for block in module.functions[0].blocks
        for instr in block.instructions
        if instr.opcode == OpCode.EXC_PUSH
    ]
    assert len(exc_push_instrs) == 1
    # src1 should be Immediate(0, ...) for "others"
    assert exc_push_instrs[0].src1.value == 0


def test_lowering_exception_id_assignment():
    """Test that exception names get unique IDs."""
    from uada80.ast_nodes import RaiseStmt

    # Create two different exceptions
    raise1 = RaiseStmt(exception_name=Identifier("Constraint_Error"))
    raise2 = RaiseStmt(exception_name=Identifier("Storage_Error"))
    raise3 = RaiseStmt(exception_name=Identifier("Constraint_Error"))  # same as raise1

    program = create_procedure("test", [raise1, raise2, raise3])
    lowering = create_lowering()

    module = lowering.lower(program)

    # Get all EXC_RAISE instructions
    exc_raise_instrs = [
        instr
        for block in module.functions[0].blocks
        for instr in block.instructions
        if instr.opcode == OpCode.EXC_RAISE
    ]
    assert len(exc_raise_instrs) == 3

    # First and third should have the same ID (Constraint_Error)
    # Second should have a different ID (Storage_Error)
    id1 = exc_raise_instrs[0].src1.value
    id2 = exc_raise_instrs[1].src1.value
    id3 = exc_raise_instrs[2].src1.value

    assert id1 == id3  # Same exception
    assert id1 != id2  # Different exceptions
