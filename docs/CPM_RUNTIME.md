# Ada Runtime Library for CP/M 2.2

## Overview

This document describes how the Ada runtime library maps to CP/M 2.2 BDOS and BIOS services. The uada80 compiler targets CP/M as its primary operating system, providing Ada programs with access to console I/O, file operations, and system services.

## Memory Layout

### CP/M Memory Map

```
0xFFFF  ┌─────────────────┐
        │ BIOS (~1.5K)    │  Hardware-specific I/O
        ├─────────────────┤  FBASE (typically 0xE400 for 64K)
        │ BDOS (~3.5K)    │  File system & I/O services
        ├─────────────────┤  CBASE (typically 0xDC00 for 64K)
        │ CCP (~2K)       │  Console Command Processor
        ├─────────────────┤  0x0100
        │                 │
        │ Ada Program     │
        │                 │
        │ Code            │
        │ Data/BSS        │
        │ Heap ↓          │
        │                 │
        │                 │
        │ ↑ Stack         │
        │                 │
        ├─────────────────┤  0x0100
        │ DMA/Cmd Line    │  0x0080-0x00FF
        │ Default FCB     │  0x005C-0x007F
        │ System Area     │  0x0000-0x005B
0x0000  └─────────────────┘
```

### Ada Runtime Memory Setup

The Ada runtime initialization performs these steps on program startup:

```ada
procedure Runtime_Init is
   FBASE : System.Address;
   Top_Of_Memory : System.Address;
begin
   -- 1. Get BDOS entry point (top of TPA)
   FBASE := Peek_Word(16#0006#);
   Top_Of_Memory := FBASE - 1;

   -- 2. Set up stack at top of TPA
   Set_Stack_Pointer(Top_Of_Memory);

   -- 3. Initialize heap between BSS end and stack
   Heap_Init(End_Of_BSS, Top_Of_Memory - Stack_Size);

   -- 4. Initialize exception handlers
   Exception_Init;

   -- 5. Call user's main program
   Main;

   -- 6. Exit to CP/M
   BDOS_Call(0, 0);  -- System reset
end Runtime_Init;
```

### Memory Regions

| Region | Location | Description |
|--------|----------|-------------|
| System | 0x0000-0x005B | CP/M system area, FCB, scratch |
| Default FCB | 0x005C-0x007F | File Control Block for first file |
| DMA Buffer | 0x0080-0x00FF | Command line and disk I/O buffer |
| Code | 0x0100+ | Ada program code |
| Data/BSS | After code | Initialized and uninitialized data |
| Heap | After BSS | Dynamic allocations (grows up) |
| Stack | Top of TPA | Call stack (grows down) |

## BDOS Interface

### Calling Convention

All BDOS functions are accessed via:
```asm
MVI C, function_number
; Set other registers as needed (E, DE, HL)
CALL 0005H
; Result typically in A or HL
```

### Ada BDOS Interface Package

```ada
package System.BDOS is
   -- Console I/O
   procedure Console_Input(C : out Character);           -- Function 1
   procedure Console_Output(C : Character);              -- Function 2
   procedure Write_String(S : String);                   -- Function 9
   function Console_Status return Boolean;               -- Function 11

   -- File I/O
   type FCB is record
      Drive : Byte;                    -- 0=default, 1=A:, 2=B:
      Filename : String(1..8);         -- Space-padded
      Extension : String(1..3);        -- Space-padded
      Extent : Byte;
      Reserved : array(1..2) of Byte;
      Record_Count : Byte;
      Allocation : array(1..16) of Byte;
      Current_Record : Byte;
      Random_Record : array(1..3) of Byte;
   end record;

   function Open_File(F : access FCB) return Byte;       -- Function 15
   function Close_File(F : access FCB) return Byte;      -- Function 16
   function Read_Sequential(F : access FCB) return Byte; -- Function 20
   function Write_Sequential(F : access FCB) return Byte;-- Function 21
   function Make_File(F : access FCB) return Byte;       -- Function 22
   function Delete_File(F : access FCB) return Byte;     -- Function 19

   -- System
   procedure System_Reset;                               -- Function 0
   procedure Select_Disk(Drive : Byte);                  -- Function 14
   function Get_Current_Disk return Byte;                -- Function 25
   procedure Set_DMA(Address : System.Address);          -- Function 26

private
   procedure BDOS_Call(Func : Byte; Param : Word);
end System.BDOS;
```

### BDOS Function Map

| BDOS # | Name | Ada Interface | Description |
|--------|------|---------------|-------------|
| 0 | P_TERMCPM | System_Reset | Exit to CP/M |
| 1 | C_READ | Console_Input | Read character |
| 2 | C_WRITE | Console_Output | Write character |
| 9 | C_WRITESTR | Write_String | Print $-terminated string |
| 10 | C_READSTR | Read_Console_Buffer | Read line |
| 11 | C_STAT | Console_Status | Check if key pressed |
| 15 | F_OPEN | Open_File | Open file |
| 16 | F_CLOSE | Close_File | Close file |
| 19 | F_DELETE | Delete_File | Delete file |
| 20 | F_READ | Read_Sequential | Read 128-byte record |
| 21 | F_WRITE | Write_Sequential | Write 128-byte record |
| 22 | F_MAKE | Make_File | Create new file |
| 26 | F_DMAOFF | Set_DMA | Set I/O buffer address |

## Ada.Text_IO Implementation

### Console I/O

```ada
package Ada.Text_IO is
   procedure Put(Item : Character);
   procedure Put(Item : String);
   procedure Put_Line(Item : String);
   procedure New_Line;

   function Get return Character;
   procedure Get(Item : out Character);
   procedure Get_Line(Item : out String; Last : out Natural);
end Ada.Text_IO;
```

**Implementation:**
- `Put(Character)` → BDOS Function 2 (C_WRITE)
- `Put(String)` → Loop calling Function 2, or use Function 9 with $ terminator
- `Put_Line` → Put(String) + New_Line
- `New_Line` → Put(CR) + Put(LF) [CP/M uses CR+LF]
- `Get` → BDOS Function 1 (C_READ)
- `Get_Line` → BDOS Function 10 (C_READSTR)

### File I/O

```ada
package Ada.Text_IO is
   type File_Type is private;
   type File_Mode is (In_File, Out_File, Append_File);

   procedure Open(File : in out File_Type;
                  Mode : File_Mode;
                  Name : String);
   procedure Create(File : in out File_Type;
                    Mode : File_Mode := Out_File;
                    Name : String);
   procedure Close(File : in out File_Type);

   procedure Put(File : File_Type; Item : Character);
   procedure Put_Line(File : File_Type; Item : String);
   function Get(File : File_Type) return Character;
   function End_Of_File(File : File_Type) return Boolean;

private
   type File_Type is record
      FCB : System.BDOS.FCB;
      DMA_Buffer : array(0..127) of Byte;
      Buffer_Index : Byte;
      Is_Open : Boolean;
      Mode : File_Mode;
   end record;
end Ada.Text_IO;
```

**Implementation Details:**

- **Buffering**: CP/M works with 128-byte sectors, Ada's Text_IO provides character-level I/O
- Each File_Type includes a 128-byte DMA buffer
- Read operations fill buffer from disk, then return characters one at a time
- Write operations buffer characters, flush when buffer full or on Close
- EOF detection: BDOS Function 20 returns 1 for EOF

### String Formatting

CP/M BDOS Function 9 expects `$`-terminated strings:

```ada
procedure Put_String_BDOS(S : String) is
   Buffer : array(0..S'Length) of Character;
begin
   Buffer(0..S'Length-1) := S;
   Buffer(S'Length) := '$';
   System.BDOS.Write_String(Buffer);
end Put_String_BDOS;
```

## BIOS Direct Access (Optional)

For performance-critical code, Ada programs can call BIOS directly:

```ada
package System.BIOS is
   function Console_Status return Boolean;  -- BIOS offset 6
   function Console_Input return Character; -- BIOS offset 9
   procedure Console_Output(C : Character); -- BIOS offset 12

private
   function Get_BIOS_Base return System.Address;
   -- Implementation: Read 0x0001, subtract 3
end System.BIOS;
```

**Warning**: Direct BIOS calls reduce portability across different CP/M systems.

## Code Generation Examples

### Console Output

Ada source:
```ada
Put("Hello, Ada!");
New_Line;
```

Generated Z80 assembly:
```asm
        ; Put string
        LXI D,MSG1
        MVI C,9         ; BDOS Function 9 (print string)
        CALL 5

        ; New line
        MVI E,13        ; CR
        MVI C,2         ; BDOS Function 2 (console output)
        CALL 5
        MVI E,10        ; LF
        MVI C,2
        CALL 5

MSG1:   DB 'Hello, Ada!$'
```

### File Operations

Ada source:
```ada
F : File_Type;
Create(F, Out_File, "OUTPUT.TXT");
Put_Line(F, "Test");
Close(F);
```

Generated assembly (simplified):
```asm
        ; Create file
        LXI D,FCB1      ; Point to FCB
        MVI C,22        ; BDOS Function 22 (make file)
        CALL 5

        ; Write data to buffer
        ; ... (buffer management code)

        ; Write 128-byte sector
        LXI D,FCB1
        MVI C,21        ; BDOS Function 21 (write sequential)
        CALL 5

        ; Close file
        LXI D,FCB1
        MVI C,16        ; BDOS Function 16 (close file)
        CALL 5

FCB1:   DB 0            ; Drive (default)
        DB 'OUTPUT  '   ; Filename (8 bytes)
        DB 'TXT'        ; Extension (3 bytes)
        ; ... rest of FCB structure
```

## Runtime Library Components

### Required Runtime Modules

1. **cpm_startup.asm** - Entry point, memory setup, call Main
2. **cpm_bdos.asm** - BDOS call wrappers
3. **cpm_io.ada** - Ada.Text_IO implementation
4. **cpm_heap.ada** - Dynamic memory allocation
5. **cpm_exceptions.ada** - Exception handling (if supported)
6. **cpm_math.ada** - Integer arithmetic helpers

### Startup Code

```asm
; cpm_startup.asm
        ORG 0100H       ; CP/M programs start at 0x0100

START:
        ; Save BDOS entry point
        LHLD 0006H      ; Get FBASE
        SHLD FBASE_SAVE

        ; Set up stack at top of TPA
        DCX H           ; HL = Top of memory - 1
        SPHL            ; SP = top of TPA

        ; Initialize heap
        LXI H,END_BSS   ; Start of heap
        SHLD HEAP_START
        LHLD FBASE_SAVE
        LXI D,-1000H    ; Reserve 4K for stack
        DAD D
        SHLD HEAP_END

        ; Call Ada main program
        CALL MAIN

        ; Exit to CP/M
        MVI C,0         ; BDOS Function 0
        CALL 5

FBASE_SAVE: DW 0
HEAP_START: DW 0
HEAP_END:   DW 0

; Link with compiled Ada code
        EXTERN MAIN
        EXTERN END_BSS
```

## Optimization Strategies

### BDOS Call Optimization

The PL/M-80 compiler (uplm80) optimizes CP/M calls using MON1/MON2/MON3 wrappers. Ada can use similar approach:

**Standard approach** (verbose):
```asm
MVI C,2         ; Function 2
MVI E,'A'       ; Character
CALL 5          ; BDOS
```

**Optimized wrapper**:
```asm
MVI E,'A'
CALL PUT_CHAR   ; Inline wrapper

PUT_CHAR:
        MVI C,2
        JMP 5           ; Tail call to BDOS
```

### String Output Optimization

For constant strings known at compile time, use BDOS Function 9:
```asm
        LXI D,STR1
        MVI C,9
        CALL 5

STR1:   DB 'Constant string$'
```

For runtime strings, copy to temporary buffer and add `$` terminator.

## CP/M Limitations and Workarounds

### No Floating Point

- Z80 has no FPU
- Options: Software floating point library (slow, large) or restrict to integer-only
- Phase 1: Integer only
- Phase 2+: Optional software FP library

### Small Memory

- 64K total, ~57K for programs in typical system
- Careful heap management required
- Consider memory overlays for large programs

### No Preemptive Multitasking

- CP/M is single-tasking
- Ada tasking (rendezvous) requires cooperative multitasking or skip entirely
- Phase 1-2: No tasking support
- Phase 3: Optional cooperative tasking using coroutines

### File System Limitations

- 8.3 filenames only
- No directories (user areas 0-15 simulate directories)
- 128-byte sector I/O
- Buffer management required for efficient text I/O

### Line Endings

- CP/M uses CR+LF (0x0D, 0x0A)
- Text files end with Ctrl-Z (0x1A) as EOF marker
- Binary vs. text mode handling needed

## Testing

### Test Programs

1. **hello.ada** - Basic console output
2. **fileio.ada** - File creation, writing, reading
3. **calc.ada** - Integer arithmetic
4. **args.ada** - Command line argument parsing

### Running on cpmemu

```bash
# Compile Ada program
uada80 hello.ada -o hello.asm

# Assemble
um80 hello.asm -o hello.rel

# Link with runtime
ul80 -o hello.com hello.rel ada_runtime.rel

# Run on cpmemu
cpmemu hello.com
```

## References

- **cpm22_bdos_calls.pdf** - Complete BDOS function reference
- **cpm22_bios_calls.pdf** - BIOS interface documentation
- **cpm22_memory_layout.pdf** - Memory organization details
- **uplm80** - PL/M-80 compiler with CP/M support (reference implementation)
- **CP/M 2.2 Operating System Manual** - Digital Research

## Implementation Priority

### Phase 1: Console I/O
- Ada.Text_IO.Put, Put_Line, Get
- Basic string operations
- Program startup/exit

### Phase 2: File I/O
- Ada.Text_IO file operations
- FCB management
- Buffered I/O

### Phase 3: Advanced Features
- Command line argument parsing
- User area management
- Direct BIOS access for performance
