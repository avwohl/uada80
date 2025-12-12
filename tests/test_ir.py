"""Tests for the IR module."""

import pytest
from uada80.ir import (
    IRType,
    IRValue,
    VReg,
    Immediate,
    Label,
    MemoryLocation,
    OpCode,
    IRInstr,
    BasicBlock,
    IRFunction,
    IRModule,
    IRBuilder,
    ir_type_size,
    ir_type_from_bits,
)


# ============================================================================
# IR Type Tests
# ============================================================================


def test_ir_type_size():
    """Test IR type size calculation."""
    assert ir_type_size(IRType.BYTE) == 1
    assert ir_type_size(IRType.BOOL) == 1
    assert ir_type_size(IRType.WORD) == 2
    assert ir_type_size(IRType.PTR) == 2
    assert ir_type_size(IRType.VOID) == 0


def test_ir_type_from_bits():
    """Test IR type from bit size."""
    assert ir_type_from_bits(1) == IRType.BYTE
    assert ir_type_from_bits(8) == IRType.BYTE
    assert ir_type_from_bits(16) == IRType.WORD
    assert ir_type_from_bits(32) == IRType.DWORD  # 32-bit values use DWORD


# ============================================================================
# IR Value Tests
# ============================================================================


def test_vreg():
    """Test virtual register."""
    vreg = VReg(id=0, name="x", ir_type=IRType.WORD)

    assert vreg.id == 0
    assert vreg.name == "x"
    assert vreg.ir_type == IRType.WORD
    assert "v0" in repr(vreg)


def test_vreg_equality():
    """Test virtual register equality."""
    v1 = VReg(id=1, ir_type=IRType.WORD)
    v2 = VReg(id=1, ir_type=IRType.WORD)
    v3 = VReg(id=2, ir_type=IRType.WORD)

    assert v1 == v2
    assert v1 != v3
    assert hash(v1) == hash(v2)


def test_immediate():
    """Test immediate value."""
    imm = Immediate(value=42, ir_type=IRType.WORD)

    assert imm.value == 42
    assert "#42" in repr(imm)


def test_label():
    """Test label."""
    lbl = Label("loop_start")

    assert lbl.name == "loop_start"
    assert lbl.ir_type == IRType.PTR
    assert "@loop_start" in repr(lbl)


def test_memory_location_global():
    """Test global memory location."""
    mem = MemoryLocation(
        is_global=True,
        symbol_name="my_var",
        ir_type=IRType.WORD,
    )

    assert mem.is_global
    assert "[my_var]" in repr(mem)


def test_memory_location_stack():
    """Test stack memory location."""
    base = VReg(id=0, ir_type=IRType.PTR)
    mem = MemoryLocation(
        base=base,
        offset=-4,
        ir_type=IRType.WORD,
    )

    assert not mem.is_global
    assert mem.offset == -4


# ============================================================================
# IR Instruction Tests
# ============================================================================


def test_ir_instr():
    """Test IR instruction creation."""
    dst = VReg(id=0, ir_type=IRType.WORD)
    src = Immediate(value=10, ir_type=IRType.WORD)

    instr = IRInstr(OpCode.MOV, dst=dst, src1=src)

    assert instr.opcode == OpCode.MOV
    assert instr.dst == dst
    assert instr.src1 == src
    assert "mov" in repr(instr).lower()


def test_ir_instr_with_comment():
    """Test IR instruction with comment."""
    instr = IRInstr(OpCode.NOP, comment="do nothing")

    assert "do nothing" in repr(instr)


def test_ir_instr_binary():
    """Test binary IR instruction."""
    dst = VReg(id=0, ir_type=IRType.WORD)
    src1 = VReg(id=1, ir_type=IRType.WORD)
    src2 = VReg(id=2, ir_type=IRType.WORD)

    instr = IRInstr(OpCode.ADD, dst=dst, src1=src1, src2=src2)

    assert instr.opcode == OpCode.ADD
    assert instr.dst == dst
    assert instr.src1 == src1
    assert instr.src2 == src2


# ============================================================================
# Basic Block Tests
# ============================================================================


def test_basic_block():
    """Test basic block creation."""
    block = BasicBlock(label="entry")

    assert block.label == "entry"
    assert len(block.instructions) == 0


def test_basic_block_add_instr():
    """Test adding instructions to basic block."""
    block = BasicBlock(label="entry")

    instr = IRInstr(OpCode.NOP)
    block.add_instr(instr)

    assert len(block.instructions) == 1
    assert block.instructions[0] == instr


def test_basic_block_successors():
    """Test basic block successors."""
    block1 = BasicBlock(label="block1")
    block2 = BasicBlock(label="block2")

    block1.add_successor(block2)

    assert block2 in block1.successors
    assert block1 in block2.predecessors


# ============================================================================
# IR Function Tests
# ============================================================================


def test_ir_function():
    """Test IR function creation."""
    func = IRFunction(name="test", return_type=IRType.VOID)

    assert func.name == "test"
    assert func.return_type == IRType.VOID
    assert len(func.blocks) == 0
    assert func.entry_block is None


def test_ir_function_new_block():
    """Test creating blocks in function."""
    func = IRFunction(name="test", return_type=IRType.VOID)

    entry = func.new_block("entry")

    assert len(func.blocks) == 1
    assert func.entry_block == entry


def test_ir_function_params():
    """Test function with parameters."""
    func = IRFunction(name="add", return_type=IRType.WORD)

    param1 = VReg(id=0, name="x", ir_type=IRType.WORD)
    param2 = VReg(id=1, name="y", ir_type=IRType.WORD)
    func.params = [param1, param2]

    assert len(func.params) == 2


# ============================================================================
# IR Module Tests
# ============================================================================


def test_ir_module():
    """Test IR module creation."""
    module = IRModule(name="test_module")

    assert module.name == "test_module"
    assert len(module.functions) == 0


def test_ir_module_add_function():
    """Test adding function to module."""
    module = IRModule(name="test_module")
    func = IRFunction(name="main", return_type=IRType.VOID)

    module.add_function(func)

    assert len(module.functions) == 1
    assert module.functions[0] == func


def test_ir_module_globals():
    """Test module globals."""
    module = IRModule(name="test_module")

    module.add_global("counter", IRType.WORD, 2)

    assert "counter" in module.globals
    assert module.globals["counter"] == (IRType.WORD, 2)


def test_ir_module_strings():
    """Test module string literals."""
    module = IRModule(name="test_module")

    module.add_string("_str0", "Hello")

    assert "_str0" in module.string_literals
    assert module.string_literals["_str0"] == "Hello"


# ============================================================================
# IR Builder Tests
# ============================================================================


def test_ir_builder():
    """Test IR builder initialization."""
    builder = IRBuilder()

    assert builder.module is None
    assert builder.function is None
    assert builder.block is None


def test_ir_builder_new_module():
    """Test creating module with builder."""
    builder = IRBuilder()

    module = builder.new_module("test")

    assert module is not None
    assert module.name == "test"
    assert builder.module == module


def test_ir_builder_new_function():
    """Test creating function with builder."""
    builder = IRBuilder()
    builder.new_module("test")

    func = builder.new_function("main", IRType.VOID)

    assert func is not None
    assert func.name == "main"
    assert builder.function == func


def test_ir_builder_new_block():
    """Test creating block with builder."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("main", IRType.VOID)

    block = builder.new_block("entry")

    assert block is not None
    assert block.label == "entry"


def test_ir_builder_new_vreg():
    """Test creating virtual register with builder."""
    builder = IRBuilder()

    v1 = builder.new_vreg(IRType.WORD, "x")
    v2 = builder.new_vreg(IRType.BYTE, "y")

    assert v1.id == 0
    assert v2.id == 1
    assert v1.name == "x"
    assert v2.ir_type == IRType.BYTE


def test_ir_builder_emit():
    """Test emitting instructions with builder."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("main", IRType.VOID)
    block = builder.new_block("entry")
    builder.set_block(block)

    dst = builder.new_vreg(IRType.WORD)
    src = Immediate(value=42, ir_type=IRType.WORD)

    builder.mov(dst, src)

    assert len(block.instructions) == 1
    assert block.instructions[0].opcode == OpCode.MOV


def test_ir_builder_arithmetic():
    """Test arithmetic operations with builder."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("main", IRType.VOID)
    block = builder.new_block("entry")
    builder.set_block(block)

    a = builder.new_vreg(IRType.WORD, "a")
    b = builder.new_vreg(IRType.WORD, "b")
    result = builder.new_vreg(IRType.WORD, "result")

    builder.add(result, a, b)
    builder.sub(result, a, b)
    builder.mul(result, a, b)

    assert len(block.instructions) == 3
    assert block.instructions[0].opcode == OpCode.ADD
    assert block.instructions[1].opcode == OpCode.SUB
    assert block.instructions[2].opcode == OpCode.MUL


def test_ir_builder_control_flow():
    """Test control flow with builder."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("main", IRType.VOID)
    block = builder.new_block("entry")
    builder.set_block(block)

    builder.jmp(Label("target"))
    builder.ret()

    assert len(block.instructions) == 2
    assert block.instructions[0].opcode == OpCode.JMP
    assert block.instructions[1].opcode == OpCode.RET


def test_ir_builder_comparisons():
    """Test comparison operations with builder."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("main", IRType.VOID)
    block = builder.new_block("entry")
    builder.set_block(block)

    a = builder.new_vreg(IRType.WORD)
    b = builder.new_vreg(IRType.WORD)
    result = builder.new_vreg(IRType.BOOL)

    builder.cmp_eq(result, a, b)
    builder.cmp_lt(result, a, b)
    builder.cmp_gt(result, a, b)

    assert len(block.instructions) == 3
    assert block.instructions[0].opcode == OpCode.CMP_EQ
    assert block.instructions[1].opcode == OpCode.CMP_LT
    assert block.instructions[2].opcode == OpCode.CMP_GT


# ============================================================================
# Complete IR Program Tests
# ============================================================================


def test_complete_ir_program():
    """Test building a complete IR program."""
    builder = IRBuilder()
    module = builder.new_module("test")

    # Create main function
    func = builder.new_function("main", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    # Local variables
    x = builder.new_vreg(IRType.WORD, "x")
    y = builder.new_vreg(IRType.WORD, "y")
    result = builder.new_vreg(IRType.WORD, "result")

    # x = 10
    builder.mov(x, Immediate(10, IRType.WORD))

    # y = 20
    builder.mov(y, Immediate(20, IRType.WORD))

    # result = x + y
    builder.add(result, x, y)

    # return result
    builder.ret(result)

    # Verify structure
    assert len(module.functions) == 1
    assert len(func.blocks) == 1
    assert len(entry.instructions) == 4


def test_ir_with_conditionals():
    """Test IR with conditional branching."""
    builder = IRBuilder()
    module = builder.new_module("test")
    func = builder.new_function("abs", IRType.WORD)

    # Blocks
    entry = builder.new_block("entry")
    then_block = builder.new_block("then")
    else_block = builder.new_block("else")
    end_block = builder.new_block("end")

    # Entry block: if x < 0
    builder.set_block(entry)
    x = builder.new_vreg(IRType.WORD, "x")
    cond = builder.new_vreg(IRType.BOOL, "cond")
    builder.cmp_lt(cond, x, Immediate(0, IRType.WORD))
    builder.jz(cond, Label("else"))

    # Then block: x = -x
    builder.set_block(then_block)
    builder.neg(x, x)
    builder.jmp(Label("end"))

    # Else block: do nothing
    builder.set_block(else_block)
    builder.jmp(Label("end"))

    # End block: return x
    builder.set_block(end_block)
    builder.ret(x)

    assert len(func.blocks) == 4
