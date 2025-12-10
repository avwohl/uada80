# UADA80 Development Notes - December 10, 2025

## Current State

### Test Results
- **376 unit tests passing** (parser:118, semantic:69, lowering:81, codegen+compiler:63, optimizer:45)
- **2,849 ACATS tests passing** (99.7% pass rate)

### Project Statistics
- **Source code**: ~25,000 lines Python
- **Runtime library**: 4,958 lines Z80 assembly (8 files)
- Total: ~30,000 lines

## Recent Session Accomplishments

### Parser Fixes (Recovered 9 ACATS timeout files)
1. Aspect specifications on abstract subprograms: `procedure X is abstract with Aspect => Value;`
2. Aspect specifications on null procedures: `procedure X is null with Aspect => Value;`
3. Interface inheritance for protected types: `protected type T is new Interface with ...`
4. Interface inheritance for task types: `task type T is new Interface with ...`
5. Entry body parameters in protected bodies: `entry E (Params) when Cond is ...`
6. Anonymous array types in object declarations: `X : array (1..10) of T;`
7. Parser timeout fix via progress tracking in parse_declarative_part()

### IR/Codegen Enhancements
- Added `is_atomic` and `is_volatile` fields to MemoryLocation (ir.py:92-93)
- Codegen emits DI/EI around atomic loads/stores (codegen/__init__.py:1741-1768, 1777-1806)

### New Runtime Files
- **cpm_float.asm** (590 lines): 48-bit floating point library (z88dk math48-compatible format)
  - 8-bit exponent, 40-bit mantissa
  - Functions: _FADD48, _FSUB48, _FMUL48, _FDIV48, _FCMP48, _FNEG48, _ITOF48, _FTOI48

- **cpm_tasking.asm** (746 lines): Preemptive multitasking runtime
  - Timer interrupt handler (_TIMER_ISR) for preemptive scheduling
  - Supports IM 1 (RST 38h) and IM 2 vectored interrupts
  - Full context save/restore including alternate register set
  - Task Control Blocks with state, priority, queues
  - Functions: _TASK_INIT, _TASK_START, _TASK_STOP, _TASK_CREATE, _TASK_YIELD,
    _TASK_TERMINATE, _ENTRY_CALL, _ENTRY_ACCEPT

## Key File Locations

### Parser Changes
- parser.py:2360-2368 - Aspect specs on abstract subprograms
- parser.py:2370-2378 - Aspect specs on null procedures
- parser.py:3170-3177 - Protected type interface inheritance
- parser.py:2989-2996 - Task type interface inheritance
- parser.py:3133-3136 - Entry body parameters
- parser.py:2288-2292 - Anonymous array types in object declarations
- parser.py:1718-1732 - Progress tracking to prevent infinite loops

### AST Node Changes
- ast_nodes.py:975 - ProtectedTypeDecl.interfaces field
- ast_nodes.py:923 - TaskTypeDecl.interfaces field

### IR Changes
- ir.py:92-93 - MemoryLocation.is_atomic, is_volatile fields

### Codegen Changes
- codegen/__init__.py:1741-1768 - DI/EI for atomic loads
- codegen/__init__.py:1777-1806 - DI/EI for atomic stores

## What To Do Next

### High Priority
1. **Complete floating point implementation** - The _FADD48_IMPL, _FMUL48_IMPL, _FDIV48_IMPL
   are stubs. Either implement full algorithms or link to z88dk math48 library.

2. **Wire up atomic flags in lowering** - MemoryLocation has is_atomic field but lowering.py
   doesn't propagate symbol.is_atomic when creating MemoryLocations. Need to update all
   MemoryLocation() calls to check symbol flags.

3. **Complete task creation** - _TASK_CREATE in cpm_tasking.asm is incomplete (stack setup messy).
   Need to properly initialize new task stack with return address to task body.

### Medium Priority
4. **Add delay statement support** - semantic.py:2005-2008 has stub for delay until
5. **Protected object lowering** - Generate DI/EI around protected procedure bodies
6. **Entry family support** - Parse and lower entry families with index parameters

### Lower Priority
7. **Link to z88dk libraries** - Can use their optimized math48/math32 instead of our stubs
8. **Add more pragma support** - Many pragmas still have empty pass bodies in lowering.py
9. **Alignment/representation clauses** - semantic.py:1020-1026 has stubs

## Runtime Library Summary

| File | Lines | Description |
|------|-------|-------------|
| cpm_startup.asm | 322 | CP/M entry, stack/heap init |
| cpm_bdos.asm | 595 | 40+ BDOS wrappers |
| cpm_heap.asm | 552 | First-fit allocator |
| cpm_math.asm | 690 | 16/32-bit integer math |
| cpm_string.asm | 864 | String operations |
| cpm_exception.asm | 599 | Exception handler chain |
| cpm_float.asm | 590 | 48-bit floating point |
| cpm_tasking.asm | 746 | Preemptive multitasking |
| **Total** | **4,958** | |

## Floating Point Format (z88dk math48 compatible)
- 6 bytes (48 bits) total
- Byte 0-4: Mantissa (40 bits, LSB first, sign in bit 7 of byte 4)
- Byte 5: Exponent (8 bits, bias 128)
- Registers: Primary accumulator in BC'DE'HL', secondary in BCDEHL

## Tasking Model
- Preemptive via timer interrupt (configurable time slice, default 10 ticks)
- Round-robin scheduling with priority support
- Task states: INACTIVE(0), READY(1), WAITING(2), TERMINATED(3)
- TCB size: 16 bytes (SP, stack base/size, state, priority, queues, task ID)
- Entry calls block caller until accepted
