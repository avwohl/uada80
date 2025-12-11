"""Tests for the Z80 code generator."""

import pytest
from uada80.ir import (
    IRType,
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
)
from uada80.codegen import Z80CodeGen, generate_z80


# ============================================================================
# Basic Code Generation Tests
# ============================================================================


def test_empty_module():
    """Test generating code for empty module."""
    module = IRModule(name="empty")

    code = generate_z80(module)

    assert "empty" in code or "Generated" in code
    assert "CSEG" in code or ".code" in code  # Accept either MACRO-80 or generic syntax


def test_empty_function():
    """Test generating code for empty function."""
    builder = IRBuilder()
    builder.new_module("test")
    func = builder.new_function("empty_func", IRType.VOID)
    entry = builder.new_block("entry")
    builder.set_block(entry)
    builder.ret()

    code = generate_z80(builder.module)

    assert "empty_func:" in code
    assert "ret" in code.lower()


def test_globals():
    """Test generating code with globals."""
    module = IRModule(name="test")
    module.add_global("counter", IRType.WORD, 2)
    module.add_global("flag", IRType.BYTE, 1)

    code = generate_z80(module)

    assert "counter:" in code
    assert "flag:" in code
    assert "DSEG" in code or ".data" in code  # Accept either MACRO-80 or generic syntax


def test_string_literals():
    """Test generating code with string literals."""
    module = IRModule(name="test")
    module.add_string("_str0", "Hello")

    code = generate_z80(module)

    assert "_str0:" in code
    assert "Hello" in code


# ============================================================================
# Instruction Generation Tests
# ============================================================================


def test_mov_immediate():
    """Test MOV with immediate value."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.VOID)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    dst = builder.new_vreg(IRType.WORD)
    builder.mov(dst, Immediate(42, IRType.WORD))
    builder.ret()

    code = generate_z80(builder.module)

    assert "ld" in code.lower()
    assert "42" in code


def test_add_instruction():
    """Test ADD instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    a = builder.new_vreg(IRType.WORD)
    b = builder.new_vreg(IRType.WORD)
    result = builder.new_vreg(IRType.WORD)

    builder.mov(a, Immediate(10, IRType.WORD))
    builder.mov(b, Immediate(20, IRType.WORD))
    builder.add(result, a, b)
    builder.ret(result)

    code = generate_z80(builder.module)

    assert "add" in code.lower()
    assert "HL" in code or "hl" in code.lower()


def test_sub_instruction():
    """Test SUB instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    a = builder.new_vreg(IRType.WORD)
    b = builder.new_vreg(IRType.WORD)
    result = builder.new_vreg(IRType.WORD)

    builder.sub(result, a, b)
    builder.ret()

    code = generate_z80(builder.module)

    assert "sbc" in code.lower()


def test_neg_instruction():
    """Test NEG instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    x = builder.new_vreg(IRType.WORD)
    builder.neg(x, x)
    builder.ret()

    code = generate_z80(builder.module)

    assert "sub" in code.lower() or "cpl" in code.lower()


def test_and_instruction():
    """Test AND instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    a = builder.new_vreg(IRType.WORD)
    b = builder.new_vreg(IRType.WORD)
    result = builder.new_vreg(IRType.WORD)

    builder.and_(result, a, b)
    builder.ret()

    code = generate_z80(builder.module)

    assert "and" in code.lower()


def test_or_instruction():
    """Test OR instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    a = builder.new_vreg(IRType.WORD)
    b = builder.new_vreg(IRType.WORD)
    result = builder.new_vreg(IRType.WORD)

    builder.or_(result, a, b)
    builder.ret()

    code = generate_z80(builder.module)

    assert "or" in code.lower()


def test_xor_instruction():
    """Test XOR instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    a = builder.new_vreg(IRType.WORD)
    b = builder.new_vreg(IRType.WORD)
    result = builder.new_vreg(IRType.WORD)

    builder.xor(result, a, b)
    builder.ret()

    code = generate_z80(builder.module)

    assert "xor" in code.lower()


def test_not_instruction():
    """Test NOT instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    x = builder.new_vreg(IRType.WORD)
    builder.not_(x, x)
    builder.ret()

    code = generate_z80(builder.module)

    assert "cpl" in code.lower()


# ============================================================================
# Control Flow Tests
# ============================================================================


def test_jump_instruction():
    """Test JMP instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.VOID)
    entry = builder.new_block("entry")
    target = builder.new_block("target")

    builder.set_block(entry)
    builder.jmp(Label("target"))

    builder.set_block(target)
    builder.ret()

    code = generate_z80(builder.module)

    assert "jp" in code.lower()
    assert "target" in code


def test_conditional_jump():
    """Test conditional jump generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.VOID)
    entry = builder.new_block("entry")

    builder.set_block(entry)
    cond = builder.new_vreg(IRType.BOOL)
    builder.jz(cond, Label("target"))
    builder.ret()

    code = generate_z80(builder.module)

    assert "jp" in code.lower()
    assert "Z" in code or "z" in code.lower()


def test_call_instruction():
    """Test CALL instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("caller", IRType.VOID)
    entry = builder.new_block("entry")

    builder.set_block(entry)
    builder.call(Label("callee"))
    builder.ret()

    code = generate_z80(builder.module)

    assert "call" in code.lower()
    assert "callee" in code


def test_push_pop():
    """Test PUSH/POP instruction generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.VOID)
    entry = builder.new_block("entry")

    builder.set_block(entry)
    x = builder.new_vreg(IRType.WORD)
    builder.push(x)
    builder.pop(x)
    builder.ret()

    code = generate_z80(builder.module)

    assert "push" in code.lower()
    assert "pop" in code.lower()


# ============================================================================
# Comparison Tests
# ============================================================================


def test_compare_equal():
    """Test equality comparison generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.BOOL)
    entry = builder.new_block("entry")

    builder.set_block(entry)
    a = builder.new_vreg(IRType.WORD)
    b = builder.new_vreg(IRType.WORD)
    result = builder.new_vreg(IRType.BOOL)

    builder.cmp_eq(result, a, b)
    builder.ret(result)

    code = generate_z80(builder.module)

    # Should use SBC for comparison
    assert "sbc" in code.lower()


def test_compare_less_than():
    """Test less-than comparison generation."""
    builder = IRBuilder()
    builder.new_module("test")
    builder.new_function("test", IRType.BOOL)
    entry = builder.new_block("entry")

    builder.set_block(entry)
    a = builder.new_vreg(IRType.WORD)
    b = builder.new_vreg(IRType.WORD)
    result = builder.new_vreg(IRType.BOOL)

    builder.cmp_lt(result, a, b)
    builder.ret(result)

    code = generate_z80(builder.module)

    assert "sbc" in code.lower()


# ============================================================================
# Memory Operation Tests
# ============================================================================


def test_load_global():
    """Test loading from global variable."""
    builder = IRBuilder()
    module = builder.new_module("test")
    module.add_global("my_var", IRType.WORD, 2)

    builder.new_function("test", IRType.WORD)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    x = builder.new_vreg(IRType.WORD)
    mem = MemoryLocation(is_global=True, symbol_name="my_var", ir_type=IRType.WORD)
    builder.load(x, mem)
    builder.ret(x)

    code = generate_z80(module)

    assert "my_var" in code


def test_store_global():
    """Test storing to global variable."""
    builder = IRBuilder()
    module = builder.new_module("test")
    module.add_global("my_var", IRType.WORD, 2)

    builder.new_function("test", IRType.VOID)
    entry = builder.new_block("entry")
    builder.set_block(entry)

    x = builder.new_vreg(IRType.WORD)
    builder.mov(x, Immediate(42, IRType.WORD))
    mem = MemoryLocation(is_global=True, symbol_name="my_var", ir_type=IRType.WORD)
    builder.store(mem, x)
    builder.ret()

    code = generate_z80(module)

    assert "my_var" in code


# ============================================================================
# Function Prologue/Epilogue Tests
# ============================================================================


def test_function_prologue_epilogue():
    """Test function prologue and epilogue."""
    builder = IRBuilder()
    builder.new_module("test")
    func = builder.new_function("test", IRType.WORD)
    func.locals_size = 10  # 10 bytes of locals

    entry = builder.new_block("entry")
    builder.set_block(entry)

    result = builder.new_vreg(IRType.WORD)
    builder.mov(result, Immediate(42, IRType.WORD))
    builder.ret(result)

    code = generate_z80(builder.module)

    # Should have IX frame pointer setup
    assert "IX" in code or "ix" in code.lower()
    assert "push" in code.lower()
    assert "ret" in code.lower()


# ============================================================================
# Complete Program Tests
# ============================================================================


def test_simple_add_function():
    """Test generating a simple add function."""
    builder = IRBuilder()
    module = builder.new_module("test")

    # function add(x, y: Word) return Word
    func = builder.new_function("add", IRType.WORD)
    x = builder.new_vreg(IRType.WORD, "x")
    y = builder.new_vreg(IRType.WORD, "y")
    func.params = [x, y]

    entry = builder.new_block("entry")
    builder.set_block(entry)

    result = builder.new_vreg(IRType.WORD)
    builder.add(result, x, y)
    builder.ret(result)

    code = generate_z80(module)

    assert "add:" in code.lower()
    assert "ret" in code.lower()


def test_conditional_function():
    """Test generating function with conditionals."""
    builder = IRBuilder()
    module = builder.new_module("test")

    func = builder.new_function("max", IRType.WORD)
    entry = builder.new_block("entry")
    then_block = builder.new_block("then")
    else_block = builder.new_block("else")
    end_block = builder.new_block("end")

    builder.set_block(entry)
    a = builder.new_vreg(IRType.WORD, "a")
    b = builder.new_vreg(IRType.WORD, "b")
    result = builder.new_vreg(IRType.WORD)
    cond = builder.new_vreg(IRType.BOOL)

    builder.cmp_gt(cond, a, b)
    builder.jz(cond, Label("else"))

    builder.set_block(then_block)
    builder.mov(result, a)
    builder.jmp(Label("end"))

    builder.set_block(else_block)
    builder.mov(result, b)
    builder.jmp(Label("end"))

    builder.set_block(end_block)
    builder.ret(result)

    code = generate_z80(module)

    assert "max:" in code.lower()
    assert "then:" in code.lower()
    assert "else:" in code.lower()
    assert "end:" in code.lower()


def test_loop_function():
    """Test generating function with a loop."""
    builder = IRBuilder()
    module = builder.new_module("test")

    func = builder.new_function("sum_to_n", IRType.WORD)
    entry = builder.new_block("entry")
    loop = builder.new_block("loop")
    end = builder.new_block("end")

    builder.set_block(entry)
    n = builder.new_vreg(IRType.WORD, "n")
    i = builder.new_vreg(IRType.WORD, "i")
    sum_reg = builder.new_vreg(IRType.WORD, "sum")

    builder.mov(i, Immediate(0, IRType.WORD))
    builder.mov(sum_reg, Immediate(0, IRType.WORD))
    builder.jmp(Label("loop"))

    builder.set_block(loop)
    cond = builder.new_vreg(IRType.BOOL)
    builder.cmp_le(cond, i, n)
    builder.jz(cond, Label("end"))

    builder.add(sum_reg, sum_reg, i)
    builder.add(i, i, Immediate(1, IRType.WORD))
    builder.jmp(Label("loop"))

    builder.set_block(end)
    builder.ret(sum_reg)

    code = generate_z80(module)

    assert "loop:" in code.lower()
    assert "jp" in code.lower()
