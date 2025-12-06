"""
Intermediate Representation for Z80 code generation.

A low-level IR designed for the Z80 architecture:
- Three-address code style instructions
- Virtual registers (allocated to real registers later)
- Explicit memory operations
- Basic blocks for control flow
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Optional


class IRType(Enum):
    """IR value types matching Z80 capabilities."""

    BYTE = auto()  # 8-bit value
    WORD = auto()  # 16-bit value
    BOOL = auto()  # Boolean (stored as byte)
    VOID = auto()  # No value (for procedures)
    PTR = auto()  # 16-bit pointer


@dataclass
class IRValue:
    """Base class for IR values (operands)."""

    ir_type: IRType


@dataclass
class VReg(IRValue):
    """Virtual register."""

    id: int
    name: str = ""  # Optional name for debugging

    def __hash__(self) -> int:
        return hash(self.id)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, VReg):
            return self.id == other.id
        return False

    def __repr__(self) -> str:
        if self.name:
            return f"v{self.id}:{self.name}"
        return f"v{self.id}"


@dataclass
class Immediate(IRValue):
    """Immediate constant value."""

    value: int

    def __init__(self, value: int, ir_type: IRType):
        super().__init__(ir_type)
        self.value = value

    def __repr__(self) -> str:
        return f"#{self.value}"


@dataclass
class Label(IRValue):
    """Code label reference."""

    name: str

    def __init__(self, name: str):
        super().__init__(IRType.PTR)
        self.name = name

    def __repr__(self) -> str:
        return f"@{self.name}"


@dataclass
class MemoryLocation(IRValue):
    """Memory location (stack slot or global)."""

    base: Optional[VReg] = None  # Base register (None = absolute)
    offset: int = 0  # Offset from base
    is_global: bool = False  # True for global variables
    symbol_name: str = ""  # Name of symbol for globals

    def __repr__(self) -> str:
        if self.is_global:
            return f"[{self.symbol_name}]"
        if self.base:
            if self.offset >= 0:
                return f"[{self.base}+{self.offset}]"
            return f"[{self.base}{self.offset}]"
        return f"[{self.offset}]"


# ============================================================================
# IR Instructions
# ============================================================================


class OpCode(Enum):
    """IR operation codes."""

    # Data movement
    MOV = auto()  # dst = src
    LOAD = auto()  # dst = [addr]
    STORE = auto()  # [addr] = src
    LEA = auto()  # dst = &addr (load effective address)

    # Arithmetic (8-bit and 16-bit)
    ADD = auto()  # dst = src1 + src2
    SUB = auto()  # dst = src1 - src2
    MUL = auto()  # dst = src1 * src2
    DIV = auto()  # dst = src1 / src2
    MOD = auto()  # dst = src1 mod src2
    NEG = auto()  # dst = -src

    # Logical (bitwise)
    AND = auto()  # dst = src1 & src2
    OR = auto()  # dst = src1 | src2
    XOR = auto()  # dst = src1 ^ src2
    NOT = auto()  # dst = ~src
    SHL = auto()  # dst = src1 << src2
    SHR = auto()  # dst = src1 >> src2

    # Comparison (sets dst to 0 or 1)
    CMP_EQ = auto()  # dst = src1 == src2
    CMP_NE = auto()  # dst = src1 != src2
    CMP_LT = auto()  # dst = src1 < src2 (signed)
    CMP_LE = auto()  # dst = src1 <= src2 (signed)
    CMP_GT = auto()  # dst = src1 > src2 (signed)
    CMP_GE = auto()  # dst = src1 >= src2 (signed)
    CMP_ULT = auto()  # dst = src1 < src2 (unsigned)
    CMP_ULE = auto()  # dst = src1 <= src2 (unsigned)
    CMP_UGT = auto()  # dst = src1 > src2 (unsigned)
    CMP_UGE = auto()  # dst = src1 >= src2 (unsigned)

    # Control flow
    JMP = auto()  # unconditional jump
    JZ = auto()  # jump if zero
    JNZ = auto()  # jump if not zero
    CALL = auto()  # call subroutine
    RET = auto()  # return from subroutine

    # Stack operations
    PUSH = auto()  # push value onto stack
    POP = auto()  # pop value from stack

    # Type conversion
    EXTEND_S = auto()  # sign-extend byte to word
    EXTEND_Z = auto()  # zero-extend byte to word
    TRUNC = auto()  # truncate word to byte

    # Special
    NOP = auto()  # no operation
    LABEL = auto()  # label definition (pseudo-instruction)


@dataclass
class IRInstr:
    """IR instruction."""

    opcode: OpCode
    dst: Optional[IRValue] = None  # Destination operand
    src1: Optional[IRValue] = None  # First source operand
    src2: Optional[IRValue] = None  # Second source operand
    comment: str = ""  # Optional comment for debugging

    def __repr__(self) -> str:
        parts = [self.opcode.name.lower()]

        if self.dst is not None:
            parts.append(str(self.dst))
        if self.src1 is not None:
            parts.append(str(self.src1))
        if self.src2 is not None:
            parts.append(str(self.src2))

        result = " ".join(parts)
        if self.comment:
            result += f"  ; {self.comment}"
        return result


# ============================================================================
# Basic Blocks and Control Flow Graph
# ============================================================================


@dataclass
class BasicBlock:
    """A basic block of straight-line code."""

    label: str
    instructions: list[IRInstr] = field(default_factory=list)
    successors: list["BasicBlock"] = field(default_factory=list)
    predecessors: list["BasicBlock"] = field(default_factory=list)

    def add_instr(self, instr: IRInstr) -> None:
        """Add an instruction to the block."""
        self.instructions.append(instr)

    def add_successor(self, block: "BasicBlock") -> None:
        """Add a successor block."""
        if block not in self.successors:
            self.successors.append(block)
            block.predecessors.append(self)

    def __repr__(self) -> str:
        lines = [f"{self.label}:"]
        for instr in self.instructions:
            lines.append(f"  {instr}")
        return "\n".join(lines)


@dataclass
class IRFunction:
    """IR representation of a function/procedure."""

    name: str
    return_type: IRType
    params: list[VReg] = field(default_factory=list)
    locals_size: int = 0  # Total size of local variables in bytes
    blocks: list[BasicBlock] = field(default_factory=list)
    entry_block: Optional[BasicBlock] = None

    def new_block(self, label: str) -> BasicBlock:
        """Create a new basic block."""
        block = BasicBlock(label=label)
        self.blocks.append(block)
        if self.entry_block is None:
            self.entry_block = block
        return block

    def __repr__(self) -> str:
        lines = [f"function {self.name}({', '.join(str(p) for p in self.params)}) -> {self.return_type.name}:"]
        lines.append(f"  locals: {self.locals_size} bytes")
        for block in self.blocks:
            for line in repr(block).split("\n"):
                lines.append(f"  {line}")
        return "\n".join(lines)


@dataclass
class IRModule:
    """IR representation of a complete compilation unit."""

    name: str
    functions: list[IRFunction] = field(default_factory=list)
    globals: dict[str, tuple[IRType, int]] = field(default_factory=dict)  # name -> (type, size)
    string_literals: dict[str, str] = field(default_factory=dict)  # label -> value

    def add_function(self, func: IRFunction) -> None:
        """Add a function to the module."""
        self.functions.append(func)

    def add_global(self, name: str, ir_type: IRType, size: int) -> None:
        """Add a global variable."""
        self.globals[name] = (ir_type, size)

    def add_string(self, label: str, value: str) -> None:
        """Add a string literal."""
        self.string_literals[label] = value

    def __repr__(self) -> str:
        lines = [f"module {self.name}"]

        if self.globals:
            lines.append("globals:")
            for name, (ir_type, size) in self.globals.items():
                lines.append(f"  {name}: {ir_type.name} ({size} bytes)")

        if self.string_literals:
            lines.append("strings:")
            for label, value in self.string_literals.items():
                lines.append(f"  {label}: \"{value}\"")

        for func in self.functions:
            lines.append("")
            lines.append(repr(func))

        return "\n".join(lines)


# ============================================================================
# IR Builder
# ============================================================================


class IRBuilder:
    """Helper class for building IR."""

    def __init__(self) -> None:
        self.module: Optional[IRModule] = None
        self.function: Optional[IRFunction] = None
        self.block: Optional[BasicBlock] = None
        self._vreg_counter = 0
        self._label_counter = 0
        self._string_counter = 0

    def new_module(self, name: str) -> IRModule:
        """Create a new IR module."""
        self.module = IRModule(name=name)
        return self.module

    def new_function(self, name: str, return_type: IRType) -> IRFunction:
        """Create a new function."""
        if self.module is None:
            raise RuntimeError("No module to add function to")
        func = IRFunction(name=name, return_type=return_type)
        self.module.add_function(func)
        self.function = func
        return func

    def new_block(self, name: str = "") -> BasicBlock:
        """Create a new basic block."""
        if self.function is None:
            raise RuntimeError("No function to add block to")
        if not name:
            name = self._new_label()
        block = self.function.new_block(name)
        return block

    def set_block(self, block: BasicBlock) -> None:
        """Set the current block for insertion."""
        self.block = block

    def new_vreg(self, ir_type: IRType, name: str = "") -> VReg:
        """Create a new virtual register."""
        vreg = VReg(id=self._vreg_counter, name=name, ir_type=ir_type)
        self._vreg_counter += 1
        return vreg

    def _new_label(self) -> str:
        """Generate a unique label name."""
        name = f"L{self._label_counter}"
        self._label_counter += 1
        return name

    def new_string_label(self) -> str:
        """Generate a unique string literal label."""
        name = f"_str{self._string_counter}"
        self._string_counter += 1
        return name

    def emit(self, instr: IRInstr) -> None:
        """Emit an instruction to the current block."""
        if self.block is None:
            raise RuntimeError("No block to emit instruction to")
        self.block.add_instr(instr)

    # Convenience methods for common instructions

    def mov(self, dst: VReg, src: IRValue, comment: str = "") -> None:
        """Emit a move instruction."""
        self.emit(IRInstr(OpCode.MOV, dst, src, comment=comment))

    def load(self, dst: VReg, addr: MemoryLocation, comment: str = "") -> None:
        """Emit a load instruction."""
        self.emit(IRInstr(OpCode.LOAD, dst, addr, comment=comment))

    def store(self, addr: MemoryLocation, src: IRValue, comment: str = "") -> None:
        """Emit a store instruction."""
        self.emit(IRInstr(OpCode.STORE, addr, src, comment=comment))

    def add(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit an add instruction."""
        self.emit(IRInstr(OpCode.ADD, dst, src1, src2, comment=comment))

    def sub(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a subtract instruction."""
        self.emit(IRInstr(OpCode.SUB, dst, src1, src2, comment=comment))

    def mul(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a multiply instruction."""
        self.emit(IRInstr(OpCode.MUL, dst, src1, src2, comment=comment))

    def div(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a divide instruction."""
        self.emit(IRInstr(OpCode.DIV, dst, src1, src2, comment=comment))

    def neg(self, dst: VReg, src: IRValue, comment: str = "") -> None:
        """Emit a negate instruction."""
        self.emit(IRInstr(OpCode.NEG, dst, src, comment=comment))

    def and_(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a bitwise AND instruction."""
        self.emit(IRInstr(OpCode.AND, dst, src1, src2, comment=comment))

    def or_(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a bitwise OR instruction."""
        self.emit(IRInstr(OpCode.OR, dst, src1, src2, comment=comment))

    def xor(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a bitwise XOR instruction."""
        self.emit(IRInstr(OpCode.XOR, dst, src1, src2, comment=comment))

    def not_(self, dst: VReg, src: IRValue, comment: str = "") -> None:
        """Emit a bitwise NOT instruction."""
        self.emit(IRInstr(OpCode.NOT, dst, src, comment=comment))

    def cmp_eq(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit an equality comparison."""
        self.emit(IRInstr(OpCode.CMP_EQ, dst, src1, src2, comment=comment))

    def cmp_ne(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a not-equal comparison."""
        self.emit(IRInstr(OpCode.CMP_NE, dst, src1, src2, comment=comment))

    def cmp_lt(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a less-than comparison (signed)."""
        self.emit(IRInstr(OpCode.CMP_LT, dst, src1, src2, comment=comment))

    def cmp_le(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a less-or-equal comparison (signed)."""
        self.emit(IRInstr(OpCode.CMP_LE, dst, src1, src2, comment=comment))

    def cmp_gt(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a greater-than comparison (signed)."""
        self.emit(IRInstr(OpCode.CMP_GT, dst, src1, src2, comment=comment))

    def cmp_ge(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a greater-or-equal comparison (signed)."""
        self.emit(IRInstr(OpCode.CMP_GE, dst, src1, src2, comment=comment))

    def jmp(self, target: Label, comment: str = "") -> None:
        """Emit an unconditional jump."""
        self.emit(IRInstr(OpCode.JMP, target, comment=comment))

    def jz(self, cond: IRValue, target: Label, comment: str = "") -> None:
        """Emit a jump-if-zero."""
        self.emit(IRInstr(OpCode.JZ, target, cond, comment=comment))

    def jnz(self, cond: IRValue, target: Label, comment: str = "") -> None:
        """Emit a jump-if-not-zero."""
        self.emit(IRInstr(OpCode.JNZ, target, cond, comment=comment))

    def call(self, target: Label, comment: str = "") -> None:
        """Emit a call instruction."""
        self.emit(IRInstr(OpCode.CALL, target, comment=comment))

    def ret(self, value: Optional[IRValue] = None, comment: str = "") -> None:
        """Emit a return instruction."""
        self.emit(IRInstr(OpCode.RET, src1=value, comment=comment))

    def push(self, value: IRValue, comment: str = "") -> None:
        """Emit a push instruction."""
        self.emit(IRInstr(OpCode.PUSH, src1=value, comment=comment))

    def pop(self, dst: VReg, comment: str = "") -> None:
        """Emit a pop instruction."""
        self.emit(IRInstr(OpCode.POP, dst, comment=comment))

    def label(self, name: str) -> None:
        """Emit a label pseudo-instruction."""
        self.emit(IRInstr(OpCode.LABEL, Label(name), comment=name))


# ============================================================================
# Utility Functions
# ============================================================================


def ir_type_size(ir_type: IRType) -> int:
    """Return the size in bytes of an IR type."""
    if ir_type == IRType.BYTE:
        return 1
    if ir_type == IRType.BOOL:
        return 1
    if ir_type == IRType.WORD:
        return 2
    if ir_type == IRType.PTR:
        return 2
    return 0


def ir_type_from_bits(bits: int, signed: bool = True) -> IRType:
    """Get IR type from size in bits."""
    if bits <= 8:
        return IRType.BYTE
    return IRType.WORD
