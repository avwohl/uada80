# UADA80 - Remaining Work for Full Ada/ACATS

## Current Status (2025-12-13)

| Phase | Pass Rate | Notes |
|-------|-----------|-------|
| Parsing | 2849/2849 (100%) | Full Ada syntax supported |
| Semantic | 2742/2742 (100%) | All grouped ACATS tests pass |
| Lowering | 105/105 (100%) | All lowering tests pass |
| Codegen | 24/24 (100%) | All code generation tests pass |
| Unit Tests | 6775/6775 (100%) | All pass |

## Recent Fixes (2025-12-13)

1. [x] Ada 2022 declare expressions in expression functions
   - Parser now handles `(declare ... begin Expr)` syntax in expression function bodies

2. [x] Extended GNAT Standard Library Integration
   - Added Ada.Exceptions (Exception_Name, Exception_Message, Raise_Exception, Reraise_Occurrence)
   - Added Ada.Tags (Tag type, External_Tag, Internal_Tag, Parent_Tag, etc.)
   - Added Ada.Characters.Handling (Is_Letter, Is_Digit, To_Upper, To_Lower, etc.)
   - Added Ada.Characters.Latin_1 (all 256 named character constants)
   - Added Ada.Characters.Conversions
   - Added Ada.Text_IO child packages (Integer_IO, Float_IO, Enumeration_IO, Modular_IO)
   - Added Ada.Integer_Text_IO and Ada.Float_Text_IO
   - Added Ada.Sequential_IO (generic sequential file I/O)
   - Added Ada.Direct_IO (generic random access file I/O)
   - Added Ada.Streams (Root_Stream_Type, Stream_Element types)
   - Added Ada.Streams.Stream_IO
   - Added Interfaces package (Integer_8/16/32/64, Unsigned_8/16/32/64)
   - Added Interfaces.C (int, char, size_t, ptrdiff_t, etc.)
   - Added Interfaces.C.Strings (chars_ptr, New_String, Value, Strlen)
   - Added System.Storage_Elements (Storage_Offset, To_Address, To_Integer)
   - Added System.Address_To_Access_Conversions (generic)
   - Added System constants (Storage_Unit, Word_Size, Memory_Size, etc.)

## Implemented Features

### Core Language (Ada 83/95)
- [x] All basic types (Integer, Float, Boolean, Character, String)
- [x] Enumeration types with attributes
- [x] Array types (constrained and unconstrained)
- [x] Record types (simple and variant)
- [x] Access types (pointers)
- [x] Subprograms (procedures and functions)
- [x] Packages (specification and body)
- [x] Exception handling
- [x] Generics (packages, procedures, functions)
- [x] Tasking (task types, protected types, entries)

### Ada 2005 Features
- [x] Interfaces
- [x] Null procedures
- [x] Overriding indicators

### Ada 2012 Features
- [x] Expression functions
- [x] Conditional expressions (if/case)
- [x] Quantified expressions (for all/some)
- [x] Membership tests with multiple choices
- [x] Contract aspects (Pre, Post)

### Ada 2022 Features
- [x] Declare expressions
- [x] Parallel blocks (`parallel do ... and do ... end parallel`)
- [x] Parallel loops (`parallel for`)
- [x] Delta aggregates
- [x] Container aggregates

### Type System
- [x] Type checking edge cases (Universal_Real, type conversion)
- [x] Overload resolution with expected type context
- [x] Enumeration literal overloading
- [x] Generic instantiation type matching
- [x] Discriminant handling
- [x] Array/record constraint checking

### Standard Library
- [x] Wide_Character / Wide_String types
- [x] System.Address type
- [x] Predefined exceptions
- [x] Ada.Text_IO (with child packages: Integer_IO, Float_IO, Enumeration_IO, Modular_IO)
- [x] Ada.Integer_Text_IO, Ada.Float_Text_IO
- [x] Ada.Finalization (Controlled types)
- [x] Ada.Strings (with Fixed, Maps, Bounded)
- [x] Ada.Command_Line
- [x] Ada.Unchecked_Conversion, Ada.Unchecked_Deallocation
- [x] Ada.Calendar
- [x] Ada.Numerics (with Elementary_Functions, Float_Random, Discrete_Random)
- [x] Ada.Containers (Vectors, Lists, Maps, Sets, Trees)
- [x] Ada.Exceptions (Exception_Name, Exception_Message, Raise_Exception)
- [x] Ada.Tags (Tag type, External_Tag, Internal_Tag, etc.)
- [x] Ada.Characters.Handling (Is_Letter, To_Upper, To_Lower, etc.)
- [x] Ada.Characters.Latin_1 (NUL, LF, CR, Space, all named constants)
- [x] Ada.Characters.Conversions
- [x] Ada.Sequential_IO (generic file I/O)
- [x] Ada.Direct_IO (random access file I/O)
- [x] Ada.Streams (Root_Stream_Type, Stream_Element_Array)
- [x] Ada.Streams.Stream_IO
- [x] Interfaces (Integer_8/16/32/64, Unsigned_8/16/32/64, shift/rotate ops)
- [x] Interfaces.C (int, char, size_t, etc.)
- [x] Interfaces.C.Strings (chars_ptr, New_String, Value)
- [x] System.Storage_Elements (Storage_Offset, To_Address, To_Integer)
- [x] System.Address_To_Access_Conversions (generic)

## Remaining Work

### ACATS Test Suite Support

The ACATS test suite requires certain support libraries:

1. [x] Ada.Text_IO and Ada.Calendar (now available for Report.a)
2. [ ] ACATS support library (Report.a) - can now be compiled
3. [x] ImpDef (implementation defined values for Z80/CP/M target)
4. [x] SPPRT13 (address constants for chapter 13 representation clause tests)
5. [ ] Various FCxx packages (foundation code)

### Optional Standard Library Extensions (Future)

These packages can be added for extended GNAT compatibility:

- Ada.Wide_Text_IO (wide character I/O)
- Ada.Directories (file system operations - not applicable to CP/M)
- Ada.Environment_Variables
- Ada.Real_Time (not applicable to Z80)
- GNAT-specific packages (GNAT.*, etc.)

## Z80/CP/M Target Notes

This compiler generates Z80 assembly for CP/M 2.2. Key characteristics:

- 16-bit integers (default)
- Software floating point (48-bit)
- 64KB address space (~57KB usable TPA)
- Cooperative multitasking (for tasking support)
- CP/M BDOS interface for I/O

## Usage

```bash
# Compile Ada source to Z80 assembly
uada80 program.ada -o program.asm

# Assemble with Z80 assembler (e.g., SLR's Z80ASM)
z80asm program.asm -o program.rel

# Link with runtime library
ul80 program.rel -l libada.lib -o program.com
```
