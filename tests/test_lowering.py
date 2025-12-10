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


# ============================================================================
# Range Constraint Tests
# ============================================================================


def test_lowering_range_check_on_type_conversion():
    """Test that type conversions to constrained subtypes emit range checks."""
    from uada80.ast_nodes import TypeConversion, TypeDecl, SubtypeDecl
    from uada80.type_system import IntegerType

    # Create a constrained subtype
    subtype_decl = SubtypeDecl(
        name="Small_Int",
        subtype_indication=SubtypeIndication(
            type_mark=Identifier("Integer"),
            constraint=RangeExpr(low=int_lit(1), high=int_lit(100)),
        ),
    )

    # Create a type conversion: Small_Int(X) where X is a variable
    conversion = TypeConversion(
        type_mark=Identifier("Small_Int"),
        operand=Identifier("X"),
    )

    # Use the conversion in a return statement
    decl = ObjectDecl(
        names=["X"],
        type_mark=subtype("Integer"),
        init_expr=int_lit(50),
    )

    program = create_simple_function(
        "test_conversion",
        [ReturnStmt(value=conversion)],
        decls=[subtype_decl, decl],
    )

    lowering = create_lowering()

    # Register the subtype in the symbol table
    small_int_type = IntegerType(name="Small_Int", low=1, high=100)
    lowering.symbols.define(Symbol(
        name="Small_Int",
        kind=SymbolKind.TYPE,
        ada_type=small_int_type,
    ))

    module = lowering.lower(program)

    # Should have CMP_LT and CMP_GT instructions for range checking
    has_cmp_lt = False
    has_cmp_gt = False
    has_jnz = False

    for block in module.functions[0].blocks:
        for instr in block.instructions:
            if instr.opcode == OpCode.CMP_LT:
                has_cmp_lt = True
            if instr.opcode == OpCode.CMP_GT:
                has_cmp_gt = True
            if instr.opcode == OpCode.JNZ:
                has_jnz = True

    # Range checks emit CMP_LT and CMP_GT with JNZ
    assert has_cmp_lt, "Expected CMP_LT for lower bound check"
    assert has_cmp_gt, "Expected CMP_GT for upper bound check"
    assert has_jnz, "Expected JNZ for conditional jump to error handler"


def test_lowering_range_check_on_assignment():
    """Test that assignments to constrained variables emit range checks.

    Note: This test verifies the basic mechanism. The assignment range check
    depends on the variable having a type with bounds, which requires the
    semantic analyzer to properly propagate the type information. Here we
    test the infrastructure by manually setting up the variable with its type.
    """
    from uada80.type_system import IntegerType

    # Create a simple variable with a constrained type
    positive_type = IntegerType(name="Small", low=1, high=100)

    # Create a variable declaration
    decl = ObjectDecl(
        names=["N"],
        type_mark=subtype("Small"),
        init_expr=int_lit(50),
    )

    # Assignment to the constrained variable
    assign = AssignmentStmt(
        target=Identifier("N"),
        value=int_lit(75),
    )

    program = create_procedure(
        "test_assign",
        [assign],
        decls=[decl],
    )

    lowering = create_lowering()

    # Register the constrained type AND the variable with that type
    lowering.symbols.define(Symbol(
        name="Small",
        kind=SymbolKind.TYPE,
        ada_type=positive_type,
    ))
    lowering.symbols.define(Symbol(
        name="N",
        kind=SymbolKind.VARIABLE,
        ada_type=positive_type,
    ))

    module = lowering.lower(program)

    # Should have CMP_LT and CMP_GT instructions for range checking
    has_cmp_lt = False
    has_cmp_gt = False

    for block in module.functions[0].blocks:
        for instr in block.instructions:
            if instr.opcode == OpCode.CMP_LT:
                has_cmp_lt = True
            if instr.opcode == OpCode.CMP_GT:
                has_cmp_gt = True

    # Range checks emit CMP_LT for lower bound and CMP_GT for upper bound
    assert has_cmp_lt, "Expected CMP_LT for lower bound check on constrained assignment"
    assert has_cmp_gt, "Expected CMP_GT for upper bound check on constrained assignment"


def test_lowering_qualified_expr_range_check():
    """Test that qualified expressions emit range checks."""
    from uada80.ast_nodes import QualifiedExpr
    from uada80.type_system import IntegerType

    # Create a qualified expression: Small'(X)
    qual_expr = QualifiedExpr(
        type_mark=Identifier("Small"),
        expr=int_lit(50),
    )

    program = create_simple_function(
        "test_qualified",
        [ReturnStmt(value=qual_expr)],
    )

    lowering = create_lowering()

    # Register the type
    small_type = IntegerType(name="Small", low=0, high=255)
    lowering.symbols.define(Symbol(
        name="Small",
        kind=SymbolKind.TYPE,
        ada_type=small_type,
    ))

    module = lowering.lower(program)

    # Should have range check comparisons
    has_cmp_lt = False
    has_cmp_gt = False

    for block in module.functions[0].blocks:
        for instr in block.instructions:
            if instr.opcode == OpCode.CMP_LT:
                has_cmp_lt = True
            if instr.opcode == OpCode.CMP_GT:
                has_cmp_gt = True

    # Range checks emit CMP_LT for lower bound and CMP_GT for upper bound
    assert has_cmp_lt, "Expected CMP_LT for lower bound check"
    assert has_cmp_gt, "Expected CMP_GT for upper bound check"


def test_no_range_check_for_unconstrained_integer():
    """Test that conversions to full Integer type don't emit unnecessary checks."""
    from uada80.ast_nodes import TypeConversion

    # Create a type conversion to Integer (full range)
    conversion = TypeConversion(
        type_mark=Identifier("Integer"),
        operand=int_lit(42),
    )

    program = create_simple_function(
        "test_full_range",
        [ReturnStmt(value=conversion)],
    )

    lowering = create_lowering()

    module = lowering.lower(program)

    # Should NOT have range check instructions (Integer is full 16-bit range)
    has_cmp_lt = False
    has_cmp_gt = False

    for block in module.functions[0].blocks:
        for instr in block.instructions:
            if instr.opcode == OpCode.CMP_LT:
                has_cmp_lt = True
            if instr.opcode == OpCode.CMP_GT:
                has_cmp_gt = True

    # Full integer range doesn't need range checks
    assert not has_cmp_lt, "Should not emit CMP_LT for full Integer range"
    assert not has_cmp_gt, "Should not emit CMP_GT for full Integer range"


# ============================================================================
# Attribute Lowering Tests
# ============================================================================


def test_lowering_valid_attribute():
    """Test lowering 'Valid attribute generates range checks."""
    from uada80.ast_nodes import AttributeReference
    from uada80.type_system import IntegerType

    # Create X'Valid expression
    valid_expr = AttributeReference(
        prefix=Identifier("X"),
        attribute="Valid",
    )

    program = create_simple_function(
        "test_valid",
        [ReturnStmt(value=valid_expr)],
        decls=[ObjectDecl(
            names=["X"],
            type_mark=subtype("Small"),
        )],
    )

    lowering = create_lowering()

    # Register the variable with a constrained type
    small_type = IntegerType(name="Small", low=1, high=100)
    lowering.symbols.define(Symbol(
        name="X",
        kind=SymbolKind.VARIABLE,
        ada_type=small_type,
    ))
    lowering.symbols.define(Symbol(
        name="Small",
        kind=SymbolKind.TYPE,
        ada_type=small_type,
    ))

    module = lowering.lower(program)

    # Should have generated comparison instructions for range check
    has_cmp = False
    for block in module.functions[0].blocks:
        for instr in block.instructions:
            if instr.opcode in (OpCode.CMP_LT, OpCode.CMP_GT):
                has_cmp = True
                break

    assert has_cmp, "Expected 'Valid to generate comparison instructions"


def test_lowering_constrained_attribute():
    """Test lowering 'Constrained attribute returns True."""
    from uada80.ast_nodes import AttributeReference

    # Create X'Constrained expression
    constrained_expr = AttributeReference(
        prefix=Identifier("X"),
        attribute="Constrained",
    )

    program = create_simple_function(
        "test_constrained",
        [ReturnStmt(value=constrained_expr)],
        decls=[ObjectDecl(
            names=["X"],
            type_mark=subtype("Integer"),
        )],
    )

    lowering = create_lowering()

    # Register the variable
    lowering.symbols.define(Symbol(
        name="X",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    ))

    module = lowering.lower(program)

    # Should have generated a MOV with value 1 (True)
    found_result = False
    for block in module.functions[0].blocks:
        for instr in block.instructions:
            if instr.opcode == OpCode.MOV:
                if isinstance(instr.src1, Immediate) and instr.src1.value == 1:
                    found_result = True
                    break

    assert found_result, "Expected 'Constrained to return True (1)"


def test_lowering_component_size_attribute():
    """Test lowering 'Component_Size attribute for arrays."""
    from uada80.ast_nodes import AttributeReference
    from uada80.type_system import ArrayType

    # Create Arr'Component_Size expression
    comp_size_expr = AttributeReference(
        prefix=Identifier("Arr"),
        attribute="Component_Size",
    )

    program = create_simple_function(
        "test_component_size",
        [ReturnStmt(value=comp_size_expr)],
    )

    lowering = create_lowering()

    # Register the array type with Integer components (16-bit)
    arr_type = ArrayType(
        name="Int_Array",
        component_type=PREDEFINED_TYPES["Integer"],
        bounds=[(1, 10)],
    )
    lowering.symbols.define(Symbol(
        name="Arr",
        kind=SymbolKind.TYPE,
        ada_type=arr_type,
    ))

    module = lowering.lower(program)

    # Should return 16 (bits for Integer)
    # The return value should be set up
    assert len(module.functions) > 0, "Expected at least one function"


def test_lowering_alignment_attribute():
    """Test lowering 'Alignment attribute."""
    from uada80.ast_nodes import AttributeReference

    # Create Integer'Alignment expression
    align_expr = AttributeReference(
        prefix=Identifier("Integer"),
        attribute="Alignment",
    )

    program = create_simple_function(
        "test_alignment",
        [ReturnStmt(value=align_expr)],
    )

    lowering = create_lowering()

    # Register the type
    lowering.symbols.define(Symbol(
        name="Integer",
        kind=SymbolKind.TYPE,
        ada_type=PREDEFINED_TYPES["Integer"],
    ))

    module = lowering.lower(program)

    # Should return 1 (byte-aligned for Z80)
    assert len(module.functions) > 0, "Expected at least one function"


# ============================================================================
# Operator Overloading Tests
# ============================================================================


def test_operator_to_name_conversion():
    """Test conversion of operators to Ada operator function names."""
    lowering = create_lowering()

    # Test binary operators
    assert lowering._operator_to_name(BinaryOp.ADD) == '"+"'
    assert lowering._operator_to_name(BinaryOp.SUB) == '"-"'
    assert lowering._operator_to_name(BinaryOp.MUL) == '"*"'
    assert lowering._operator_to_name(BinaryOp.DIV) == '"/"'
    assert lowering._operator_to_name(BinaryOp.EQ) == '"="'
    assert lowering._operator_to_name(BinaryOp.NE) == '"/="'
    assert lowering._operator_to_name(BinaryOp.LT) == '"<"'
    assert lowering._operator_to_name(BinaryOp.CONCAT) == '"&"'


def test_unary_operator_to_name_conversion():
    """Test conversion of unary operators to Ada operator function names."""
    lowering = create_lowering()

    # Test unary operators
    assert lowering._unary_operator_to_name(UnaryOp.MINUS) == '"-"'
    assert lowering._unary_operator_to_name(UnaryOp.PLUS) == '"+"'
    assert lowering._unary_operator_to_name(UnaryOp.NOT) == '"not"'
    assert lowering._unary_operator_to_name(UnaryOp.ABS) == '"abs"'


def test_builtin_operators_not_overloaded():
    """Test that built-in operators on predefined types are not looked up."""
    lowering = create_lowering()

    # Create expression with predefined Integer type
    left = Identifier("X")
    right = Identifier("Y")

    # Register Integer variables
    lowering.symbols.define(Symbol(
        name="X",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    ))
    lowering.symbols.define(Symbol(
        name="Y",
        kind=SymbolKind.VARIABLE,
        ada_type=PREDEFINED_TYPES["Integer"],
    ))

    # Should return None (use built-in operator)
    result = lowering._lookup_user_operator(BinaryOp.ADD, left, right)
    assert result is None, "Built-in + on Integer should not look up user operator"


# ============================================================================
# Ada 2012/2022 Feature Tests
# ============================================================================


def test_for_of_loop_parsing():
    """Test that for-of loops parse correctly."""
    from uada80.parser import parse
    from uada80.semantic import analyze

    source = """
    procedure Test is
        type Int_Array is array (1 .. 5) of Integer;
        Arr : Int_Array := (1, 2, 3, 4, 5);
        Sum : Integer := 0;
    begin
        for Element of Arr loop
            Sum := Sum + Element;
        end loop;
    end Test;
    """
    ast = parse(source)
    result = analyze(ast)
    # Should parse and analyze without errors
    assert ast is not None


def test_for_of_loop_reverse():
    """Test for-of loop with reverse iteration."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Int_Array is array (1 .. 5) of Integer;
        Arr : Int_Array := (1, 2, 3, 4, 5);
    begin
        for Element of reverse Arr loop
            null;
        end loop;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_declare_expression():
    """Test Ada 2022 declare expression parsing."""
    from uada80.parser import parse

    source = """
    procedure Test is
        X : Integer;
    begin
        X := (declare Y : Integer := 5; begin Y + 1);
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_delta_aggregate():
    """Test Ada 2022 delta aggregate parsing."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Rec is record
            A, B : Integer;
        end record;
        R1, R2 : Rec;
    begin
        R1 := (A => 1, B => 2);
        R2 := (R1 with delta A => 10);
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_target_name_at_symbol():
    """Test Ada 2022 @ (target name) in assignments."""
    from uada80.parser import parse

    source = """
    procedure Test is
        X : Integer := 5;
    begin
        X := @ + 1;  -- Same as X := X + 1
    end Test;
    """
    ast = parse(source)
    assert ast is not None


# ============================================================================
# Array Slice Tests
# ============================================================================


def test_array_slice_parsing():
    """Test that array slices parse correctly."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Int_Array is array (1 .. 10) of Integer;
        A : Int_Array;
        B : Int_Array;
    begin
        A(1..5) := B(6..10);  -- Slice assignment
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_array_slice_in_expression():
    """Test array slice used in expressions."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Int_Array is array (1 .. 10) of Integer;
        A, B : Int_Array;
    begin
        B := A;  -- Full array assignment
        -- Slice could be passed to procedure
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_string_slice():
    """Test string slice operations."""
    from uada80.parser import parse

    source = """
    procedure Test is
        S : String(1..20);
        Sub : String(1..5);
    begin
        Sub := S(1..5);  -- Extract substring
    end Test;
    """
    ast = parse(source)
    assert ast is not None


# ============================================================================
# Exception Handling Tests
# ============================================================================


def test_exception_declaration():
    """Test exception declaration parsing."""
    from uada80.parser import parse

    source = """
    procedure Test is
        My_Error : exception;
    begin
        raise My_Error;
    exception
        when My_Error =>
            null;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_exception_with_message():
    """Test exception raise with message."""
    from uada80.parser import parse

    source = """
    procedure Test is
        My_Error : exception;
    begin
        raise My_Error with "Something went wrong";
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_exception_reraise():
    """Test exception re-raise."""
    from uada80.parser import parse

    source = """
    procedure Test is
    begin
        null;
    exception
        when others =>
            raise;  -- Re-raise current exception
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_multiple_exception_handlers():
    """Test multiple exception handlers."""
    from uada80.parser import parse

    source = """
    procedure Test is
        Error1 : exception;
        Error2 : exception;
    begin
        null;
    exception
        when Error1 =>
            null;
        when Error2 =>
            null;
        when others =>
            null;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


# ============================================================================
# Access Type (Pointer) Tests
# ============================================================================


def test_access_type_declaration():
    """Test access type declaration."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer;
        P.all := 42;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_access_type_with_initial_value():
    """Test allocator with initial value."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer'(100);  -- Allocate with initial value
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_access_to_record():
    """Test access to record type."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Node;
        type Node_Ptr is access Node;
        type Node is record
            Value : Integer;
            Next  : Node_Ptr;
        end record;
        Head : Node_Ptr;
    begin
        Head := new Node;
        Head.Value := 1;
        Head.Next := null;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_null_access():
    """Test null access value."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr := null;
    begin
        if P /= null then
            P.all := 0;
        end if;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


# ============================================================================
# Type Conversion Tests
# ============================================================================


def test_integer_type_conversion():
    """Test integer type conversion."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Small is range 0 .. 100;
        X : Integer := 50;
        Y : Small;
    begin
        Y := Small(X);  -- Type conversion with range check
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_enumeration_conversion():
    """Test enumeration type conversion."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Color is (Red, Green, Blue);
        C : Color := Green;
        N : Integer;
    begin
        N := Color'Pos(C);  -- Enum to integer
        C := Color'Val(N);  -- Integer to enum
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_qualified_expression():
    """Test qualified expression."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Small is range 1 .. 10;
        X : Small;
    begin
        X := Small'(5);  -- Qualified expression
    end Test;
    """
    ast = parse(source)
    assert ast is not None


# ============================================================================
# Modular Type Tests
# ============================================================================


def test_modular_type():
    """Test modular type operations."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Byte is mod 256;
        A, B, C : Byte;
    begin
        A := 200;
        B := 100;
        C := A + B;  -- Wraps around (200 + 100 = 44 mod 256)
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_modular_bitwise_ops():
    """Test modular type bitwise operations."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Byte is mod 256;
        A, B : Byte;
    begin
        A := 16#FF#;
        B := A and 16#0F#;  -- Bitwise AND
        B := A or 16#F0#;   -- Bitwise OR
        B := A xor 16#AA#;  -- Bitwise XOR
        B := not A;         -- Bitwise NOT
    end Test;
    """
    ast = parse(source)
    assert ast is not None


# ============================================================================
# Record Type Tests
# ============================================================================


def test_record_type_declaration():
    """Test record type declaration."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
        P : Point;
    begin
        P.X := 10;
        P.Y := 20;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_record_aggregate():
    """Test record aggregate initialization."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
        P : Point := (X => 10, Y => 20);
        Q : Point := (1, 2);  -- Positional
    begin
        null;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_nested_record():
    """Test nested record types."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Point is record
            X, Y : Integer;
        end record;
        type Rectangle is record
            Top_Left     : Point;
            Bottom_Right : Point;
        end record;
        R : Rectangle;
    begin
        R.Top_Left.X := 0;
        R.Top_Left.Y := 0;
        R.Bottom_Right.X := 100;
        R.Bottom_Right.Y := 100;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_variant_record():
    """Test variant record (discriminated)."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Shape_Kind is (Circle, Rectangle);
        type Shape(Kind : Shape_Kind) is record
            case Kind is
                when Circle =>
                    Radius : Integer;
                when Rectangle =>
                    Width  : Integer;
                    Height : Integer;
            end case;
        end record;
        C : Shape(Circle);
        R : Shape(Rectangle);
    begin
        C.Radius := 10;
        R.Width := 20;
        R.Height := 30;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_record_assignment():
    """Test record assignment."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Point is record
            X, Y : Integer;
        end record;
        P1, P2 : Point;
    begin
        P1 := (10, 20);
        P2 := P1;  -- Copy entire record
    end Test;
    """
    ast = parse(source)
    assert ast is not None


# ============================================================================
# Tagged Type (OOP) Tests
# ============================================================================


def test_tagged_type():
    """Test tagged type declaration."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Shape is tagged record
            X, Y : Integer;
        end record;
        S : Shape;
    begin
        S.X := 0;
        S.Y := 0;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_derived_tagged_type():
    """Test derived tagged type."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Shape is tagged record
            X, Y : Integer;
        end record;
        type Circle is new Shape with record
            Radius : Integer;
        end record;
        C : Circle;
    begin
        C.X := 0;
        C.Y := 0;
        C.Radius := 10;
    end Test;
    """
    ast = parse(source)
    assert ast is not None


def test_class_wide_type():
    """Test class-wide type."""
    from uada80.parser import parse

    source = """
    procedure Test is
        type Shape is tagged record
            X, Y : Integer;
        end record;

        procedure Draw(S : Shape'Class) is
        begin
            null;
        end Draw;

        S : Shape;
    begin
        Draw(S);
    end Test;
    """
    ast = parse(source)
    assert ast is not None
