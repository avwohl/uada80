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
    DWORD = auto()  # 32-bit value (for fixed-point, uses two registers)
    BOOL = auto()  # Boolean (stored as byte)
    VOID = auto()  # No value (for procedures)
    PTR = auto()  # 16-bit pointer
    FIXED = auto()  # 16.16 fixed-point (32-bit)
    FLOAT48 = auto()  # 48-bit floating point (z88dk math48 format)
    FLOAT64 = auto()  # 64-bit IEEE 754 double precision


@dataclass
class IRValue:
    """Base class for IR values (operands)."""

    ir_type: IRType


@dataclass
class VReg(IRValue):
    """Virtual register."""

    id: int
    name: str = ""  # Optional name for debugging
    is_atomic: bool = False  # For pragma Atomic - wrap accesses in DI/EI
    is_volatile: bool = False  # For pragma Volatile - no caching

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
    is_atomic: bool = False  # pragma Atomic - wrap in DI/EI
    is_volatile: bool = False  # pragma Volatile - no caching
    is_frame_offset: bool = False  # True if offset is already a frame offset (negative)
    addr_vreg: Optional[VReg] = None  # If set, compute address of this vreg's storage
    # Bit-field support for pragma Pack
    bit_offset: int = 0  # Bit offset within byte (0-7)
    bit_size: int = 0  # Size in bits (0 = full byte/word)

    def __repr__(self) -> str:
        if self.addr_vreg:
            return f"&{self.addr_vreg}"
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
    MOD = auto()  # dst = src1 mod src2 (Ada mod: sign of divisor)
    REM = auto()  # dst = src1 rem src2 (Ada rem: sign of dividend)
    NEG = auto()  # dst = -src

    # Logical (bitwise)
    AND = auto()  # dst = src1 & src2
    OR = auto()  # dst = src1 | src2
    XOR = auto()  # dst = src1 ^ src2
    NOT = auto()  # dst = ~src
    SHL = auto()  # dst = src1 << src2
    SHR = auto()  # dst = src1 >> src2
    SAR = auto()  # dst = src1 >> src2 (arithmetic, sign-extend)
    ROL = auto()  # dst = rotate left src1 by src2
    ROR = auto()  # dst = rotate right src1 by src2

    # Comparison (sets flags only)
    CMP = auto()  # compare src1 with src2, set flags

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
    JL = auto()  # jump if less (signed), after CMP
    JLE = auto()  # jump if less or equal (signed), after CMP
    JG = auto()  # jump if greater (signed), after CMP
    JGE = auto()  # jump if greater or equal (signed), after CMP
    JC = auto()  # jump if carry (unsigned less), after CMP
    JNC = auto()  # jump if no carry (unsigned greater or equal), after CMP
    CALL = auto()  # call subroutine
    CALL_INDIRECT = auto()  # indirect call through function pointer (src1=ptr)
    DISPATCH = auto()  # dispatching call through vtable (src1=object, src2=slot)
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
    INLINE_ASM = auto()  # inline assembly (asm code in comment field)

    # Exception handling
    EXC_PUSH = auto()  # push exception handler (dst=handler_label, src1=exc_id)
    EXC_POP = auto()  # pop exception handler (normal exit)
    EXC_RAISE = auto()  # raise exception (src1=exc_id, src2=message_ptr or None)
    EXC_RERAISE = auto()  # re-raise current exception

    # 48-bit floating point operations (z88dk math48 format)
    FADD = auto()  # dst = src1 + src2 (float)
    FSUB = auto()  # dst = src1 - src2 (float)
    FMUL = auto()  # dst = src1 * src2 (float)
    FDIV = auto()  # dst = src1 / src2 (float)
    FNEG = auto()  # dst = -src (float)
    FCMP = auto()  # compare floats, set flags
    FABS = auto()  # dst = |src| (float absolute value)
    ITOF = auto()  # dst = float(src) (int to float)
    FTOI = auto()  # dst = int(src) (float to int, truncate)

    # Tasking operations
    TASK_CREATE = auto()  # create new task (dst=task_id, src1=entry_point)
    TASK_YIELD = auto()  # yield to scheduler
    TASK_TERMINATE = auto()  # terminate current task
    TASK_DELAY = auto()  # delay for src1 ticks
    TASK_DELAY_UNTIL = auto()  # delay until src1 time
    ENTRY_CALL = auto()  # call task entry (src1=task_id, src2=entry_id)
    ENTRY_ACCEPT = auto()  # accept entry call (src1=entry_id)


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
    float64_constants: dict[str, bytes] = field(default_factory=dict)  # label -> 8 bytes IEEE 754
    vtables: dict[str, list[str]] = field(default_factory=dict)  # vtable_name -> [proc_names]
    runtime_deps: set[str] = field(default_factory=set)  # Runtime routines needed from libada.lib

    def add_function(self, func: IRFunction) -> None:
        """Add a function to the module."""
        self.functions.append(func)

    def add_global(self, name: str, ir_type: IRType, size: int) -> None:
        """Add a global variable."""
        self.globals[name] = (ir_type, size)

    def add_string(self, label: str, value: str) -> None:
        """Add a string literal."""
        self.string_literals[label] = value

    def add_float64(self, label: str, value: bytes) -> None:
        """Add a Float64 constant (8 bytes IEEE 754 little-endian)."""
        self.float64_constants[label] = value

    def need_runtime(self, routine_name: str) -> None:
        """Mark a runtime library routine as needed."""
        self.runtime_deps.add(routine_name)

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

    def new_vreg(self, ir_type: IRType, name: str = "", is_atomic: bool = False, is_volatile: bool = False) -> VReg:
        """Create a new virtual register."""
        vreg = VReg(id=self._vreg_counter, name=name, ir_type=ir_type, is_atomic=is_atomic, is_volatile=is_volatile)
        self._vreg_counter += 1
        return vreg

    def _new_label(self) -> str:
        """Generate a unique label name."""
        name = f"L{self._label_counter}"
        self._label_counter += 1
        return name

    def new_label(self, prefix: str = "L") -> str:
        """Generate a unique label name with optional prefix."""
        name = f"{prefix}_{self._label_counter}"
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

    def shl(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a shift left instruction."""
        self.emit(IRInstr(OpCode.SHL, dst, src1, src2, comment=comment))

    def shr(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a shift right instruction."""
        self.emit(IRInstr(OpCode.SHR, dst, src1, src2, comment=comment))

    def cmp(self, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a comparison instruction (sets flags)."""
        self.emit(IRInstr(OpCode.CMP, None, src1, src2, comment=comment))

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

    def _ensure_label(self, target: Label | str) -> Label:
        """Convert string to Label if needed."""
        return Label(target) if isinstance(target, str) else target

    def jmp(self, target: Label | str, comment: str = "") -> None:
        """Emit an unconditional jump."""
        self.emit(IRInstr(OpCode.JMP, self._ensure_label(target), comment=comment))

    def jz(self, cond: IRValue, target: Label | str, comment: str = "") -> None:
        """Emit a jump-if-zero."""
        self.emit(IRInstr(OpCode.JZ, self._ensure_label(target), cond, comment=comment))

    def jnz(self, cond: IRValue, target: Label | str, comment: str = "") -> None:
        """Emit a jump-if-not-zero."""
        self.emit(IRInstr(OpCode.JNZ, self._ensure_label(target), cond, comment=comment))

    def jl(self, target: Label | str, comment: str = "") -> None:
        """Emit a jump-if-less (signed), used after CMP."""
        self.emit(IRInstr(OpCode.JL, self._ensure_label(target), comment=comment))

    def jle(self, target: Label | str, comment: str = "") -> None:
        """Emit a jump-if-less-or-equal (signed), used after CMP."""
        self.emit(IRInstr(OpCode.JLE, self._ensure_label(target), comment=comment))

    def jg(self, target: Label | str, comment: str = "") -> None:
        """Emit a jump-if-greater (signed), used after CMP."""
        self.emit(IRInstr(OpCode.JG, self._ensure_label(target), comment=comment))

    def jge(self, target: Label | str, comment: str = "") -> None:
        """Emit a jump-if-greater-or-equal (signed), used after CMP."""
        self.emit(IRInstr(OpCode.JGE, self._ensure_label(target), comment=comment))

    def jc(self, target: Label | str, comment: str = "") -> None:
        """Emit a jump-if-carry (unsigned less), used after CMP."""
        self.emit(IRInstr(OpCode.JC, self._ensure_label(target), comment=comment))

    def jnc(self, target: Label | str, comment: str = "") -> None:
        """Emit a jump-if-no-carry (unsigned >=), used after CMP."""
        self.emit(IRInstr(OpCode.JNC, self._ensure_label(target), comment=comment))

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

    # 48-bit floating point operations
    def fadd(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a floating-point add instruction."""
        self.emit(IRInstr(OpCode.FADD, dst, src1, src2, comment=comment))

    def fsub(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a floating-point subtract instruction."""
        self.emit(IRInstr(OpCode.FSUB, dst, src1, src2, comment=comment))

    def fmul(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a floating-point multiply instruction."""
        self.emit(IRInstr(OpCode.FMUL, dst, src1, src2, comment=comment))

    def fdiv(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a floating-point divide instruction."""
        self.emit(IRInstr(OpCode.FDIV, dst, src1, src2, comment=comment))

    def fneg(self, dst: VReg, src: IRValue, comment: str = "") -> None:
        """Emit a floating-point negate instruction."""
        self.emit(IRInstr(OpCode.FNEG, dst, src, comment=comment))

    def fcmp(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a floating-point compare instruction."""
        self.emit(IRInstr(OpCode.FCMP, dst, src1, src2, comment=comment))

    def fabs(self, dst: VReg, src: IRValue, comment: str = "") -> None:
        """Emit a floating-point absolute value instruction."""
        self.emit(IRInstr(OpCode.FABS, dst, src, comment=comment))

    def itof(self, dst: VReg, src: IRValue, comment: str = "") -> None:
        """Emit an integer-to-float conversion instruction."""
        self.emit(IRInstr(OpCode.ITOF, dst, src, comment=comment))

    def ftoi(self, dst: VReg, src: IRValue, comment: str = "") -> None:
        """Emit a float-to-integer conversion instruction."""
        self.emit(IRInstr(OpCode.FTOI, dst, src, comment=comment))

    # Tasking operations
    def task_create(self, dst: VReg, entry_point: Label, comment: str = "") -> None:
        """Emit a task creation instruction."""
        self.emit(IRInstr(OpCode.TASK_CREATE, dst, entry_point, comment=comment))

    def task_yield(self, comment: str = "") -> None:
        """Emit a task yield instruction."""
        self.emit(IRInstr(OpCode.TASK_YIELD, comment=comment))

    def task_terminate(self, comment: str = "") -> None:
        """Emit a task termination instruction."""
        self.emit(IRInstr(OpCode.TASK_TERMINATE, comment=comment))

    def task_delay(self, ticks: IRValue, comment: str = "") -> None:
        """Emit a task delay instruction."""
        self.emit(IRInstr(OpCode.TASK_DELAY, src1=ticks, comment=comment))

    def task_delay_until(self, time: IRValue, comment: str = "") -> None:
        """Emit a task delay-until instruction."""
        self.emit(IRInstr(OpCode.TASK_DELAY_UNTIL, src1=time, comment=comment))

    def entry_call(self, task_id: IRValue, entry_id: IRValue, comment: str = "") -> None:
        """Emit an entry call instruction."""
        self.emit(IRInstr(OpCode.ENTRY_CALL, src1=task_id, src2=entry_id, comment=comment))

    def entry_accept(self, entry_id: IRValue, comment: str = "") -> None:
        """Emit an entry accept instruction."""
        self.emit(IRInstr(OpCode.ENTRY_ACCEPT, src1=entry_id, comment=comment))

    # Additional operations for protected types and tasking

    def set_function(self, func: IRFunction) -> None:
        """Set the current function for building."""
        self.function = func
        if func.blocks:
            self.block = func.blocks[-1]

    def lea(self, dst: VReg, addr: MemoryLocation, comment: str = "") -> None:
        """Emit a load effective address instruction."""
        self.emit(IRInstr(OpCode.LEA, dst, addr, comment=comment))

    def load_mem(self, dst: VReg, addr: MemoryLocation, comment: str = "") -> None:
        """Emit a load from memory instruction (alias for load)."""
        self.load(dst, addr, comment=comment)

    def rem(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a remainder (modulo) instruction."""
        self.emit(IRInstr(OpCode.MOD, dst, src1, src2, comment=comment))

    def rol(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a rotate left instruction."""
        self.emit(IRInstr(OpCode.ROL, dst, src1, src2, comment=comment))

    def ror(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a rotate right instruction."""
        self.emit(IRInstr(OpCode.ROR, dst, src1, src2, comment=comment))

    def sar(self, dst: VReg, src1: IRValue, src2: IRValue, comment: str = "") -> None:
        """Emit a shift arithmetic right instruction."""
        self.emit(IRInstr(OpCode.SAR, dst, src1, src2, comment=comment))


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
    if ir_type == IRType.DWORD:
        return 4
    if ir_type == IRType.FIXED:
        return 4  # 16.16 fixed point
    if ir_type == IRType.FLOAT48:
        return 6  # 48-bit z88dk format
    if ir_type == IRType.FLOAT64:
        return 8  # 64-bit IEEE 754 double
    return 0


def ir_type_from_bits(bits: int, signed: bool = True) -> IRType:
    """Get IR type from size in bits."""
    if bits <= 8:
        return IRType.BYTE
    if bits <= 16:
        return IRType.WORD
    if bits <= 32:
        return IRType.DWORD
    if bits <= 48:
        return IRType.FLOAT48
    if bits <= 64:
        return IRType.FLOAT64
    return IRType.WORD  # Default fallback
