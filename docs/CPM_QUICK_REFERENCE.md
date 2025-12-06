# CP/M Quick Reference for Ada Developers

## Essential CP/M Knowledge

### Memory Map

```
0x0000  JMP WBOOT       Warm boot vector
0x0003  IOBYTE          I/O device mapping
0x0004  Disk/User       Current drive and user number
0x0005  JMP BDOS        Main OS entry point ← CALL THIS
0x0006  FBASE (low)     BDOS address = top of TPA + 1
0x0007  FBASE (high)
0x005C  FCB             Default File Control Block (36 bytes)
0x0080  DMA Buffer      Command line / disk I/O (128 bytes)
0x0100  TPA START       Your Ada program starts here
...
FBASE-1                 Top of available memory
```

### BDOS Calling Convention

```asm
MVI C, function_number    ; Function in C
; Set parameters in E, DE, or HL as needed
CALL 5                    ; Call BDOS
; Result in A or HL
```

### Most Important BDOS Functions

| # | Name | Input | Output | Use |
|---|------|-------|--------|-----|
| 0 | Exit | C=0 | - | Exit to CP/M |
| 1 | Console Input | C=1 | A=char | Get character (Ada.Text_IO.Get) |
| 2 | Console Output | C=2, E=char | - | Put character (Ada.Text_IO.Put) |
| 9 | Print String | C=9, DE=addr | - | Print $-terminated string |
| 11 | Console Status | C=11 | A=0xFF/0 | Check if key ready |
| 15 | Open File | C=15, DE=FCB | A=0-3/0xFF | Open file (0-3=OK, 0xFF=fail) |
| 16 | Close File | C=16, DE=FCB | A=0-3/0xFF | Close file |
| 19 | Delete File | C=19, DE=FCB | A=0-3/0xFF | Delete file |
| 20 | Read Sequential | C=20, DE=FCB | A=0/1/0xFF | Read 128 bytes (0=OK, 1=EOF) |
| 21 | Write Sequential | C=21, DE=FCB | A=0/1-2/0xFF | Write 128 bytes |
| 22 | Make File | C=22, DE=FCB | A=0-3/0xFF | Create new file |
| 26 | Set DMA | C=26, DE=addr | - | Set I/O buffer address |

### File Control Block (FCB) Structure

```
Offset  Size  Field
0       1     Drive (0=default, 1=A:, 2=B:, etc)
1       8     Filename (space-padded, uppercase)
9       3     Extension (space-padded, uppercase)
12      1     Extent number (EX)
13      2     Reserved (S1, S2)
15      1     Record count (RC)
16      16    Disk allocation
32      1     Current record (CR)
33      3     Random record number (r0, r1, r2)
```

Example FCB for "TEST.TXT":
```
00 54 45 53 54 20 20 20  ; Drive 0, "TEST    "
20 54 58 54 00 00 00 00  ; "TXT", extent=0, reserved
00 00 00 00 00 00 00 00  ; RC=0, allocation
00 00 00 00 00 00 00 00
00 00 00 00              ; CR=0, random record
```

## Ada to CP/M Mapping

### Console I/O

```ada
-- Ada code
Put("Hello");
Put_Line(" World!");
C := Get;
```

Maps to:
```asm
; Put("Hello")
LXI D,STR1
MVI C,9         ; BDOS 9: Print string
CALL 5

; Put_Line(" World!")
LXI D,STR2
MVI C,9
CALL 5
MVI E,13        ; CR
MVI C,2         ; BDOS 2: Console output
CALL 5
MVI E,10        ; LF
MVI C,2
CALL 5

; C := Get
MVI C,1         ; BDOS 1: Console input
CALL 5
STA VARNAME     ; Store in variable C

STR1: DB 'Hello$'
STR2: DB ' World!$'
```

### File Operations

```ada
-- Ada code
F : File_Type;
Create(F, Out_File, "OUTPUT.TXT");
Put_Line(F, "Test");
Close(F);
```

Requires:
1. Fill FCB with filename "OUTPUT  .TXT"
2. BDOS 22: Make file
3. Copy "Test" to DMA buffer (+ padding to 128 bytes)
4. BDOS 21: Write sequential
5. BDOS 16: Close file

## Program Structure

### Minimal Ada Program for CP/M

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Hello is
begin
   Put_Line("Hello from Ada on CP/M!");
end Hello;
```

### Generated Assembly Structure

```asm
        ORG 0100H       ; CP/M load address

        ; Runtime initialization
        LHLD 0006H      ; Get FBASE (top of TPA)
        DCX H           ; Top of memory
        SPHL            ; Set stack pointer

        ; Call Ada main
        CALL HELLO_MAIN

        ; Exit to CP/M
        MVI C,0         ; BDOS 0: System reset
        CALL 5

HELLO_MAIN:
        ; Put_Line("Hello from Ada on CP/M!")
        LXI D,MSG1
        MVI C,9         ; Print string
        CALL 5
        MVI E,13        ; CR
        MVI C,2
        CALL 5
        MVI E,10        ; LF
        MVI C,2
        CALL 5
        RET

MSG1:   DB 'Hello from Ada on CP/M!$'
```

## Memory Management

### Getting Available Memory

```ada
-- At program startup
FBASE := Peek_Word(16#0006#);  -- Read address at 0x0006
Top_Of_TPA := FBASE - 1;
Available := Top_Of_TPA - End_Of_Program;
```

### Typical Memory Layout for Ada Program

```
0x0100  Code segment
????    Data/BSS segment
????    Heap starts here (grows up →)

        Free space

        (← grows down) Stack
0xE3FF  Top of TPA (typical 64K system)
0xE400  CCP starts
```

### Stack and Heap Setup

```asm
; At startup (before calling main)
LHLD 0006H      ; Get FBASE
LXI D,-1000H    ; Reserve 4K for stack
DAD D           ; HL = stack bottom
SHLD HEAP_END   ; Set heap limit

LHLD 0006H      ; Get FBASE again
DCX H           ; HL = top of TPA
SPHL            ; SP = top of memory

LXI H,END_BSS   ; Start of heap
SHLD HEAP_PTR
```

## Common Pitfalls

### 1. String Termination
CP/M BDOS function 9 needs `$` terminator, not null:
```ada
-- WRONG
S : String := "Hello" & Character'Val(0);

-- RIGHT for BDOS 9
S : String := "Hello";
Buffer : array(0..5) of Character;
Buffer(0..4) := S;
Buffer(5) := '$';
```

### 2. Line Endings
CP/M uses CR+LF (0x0D, 0x0A), not just LF:
```asm
MVI E,13        ; Must send CR
MVI C,2
CALL 5
MVI E,10        ; Then LF
MVI C,2
CALL 5
```

### 3. File I/O is 128-Byte Sectors
Can't just write arbitrary bytes - must buffer to 128-byte sectors:
```ada
-- Need a buffer
type Sector is array(0..127) of Byte;
Buffer : Sector;
Index : Natural := 0;

procedure Write_Char(C : Character) is
begin
   Buffer(Index) := C;
   Index := Index + 1;
   if Index = 128 then
      -- Write sector via BDOS 21
      Flush_Buffer;
      Index := 0;
   end if;
end Write_Char;
```

### 4. FCB Must Be Uppercase and Space-Padded
```ada
-- Filename "test.txt" must become:
-- Drive: 0
-- Name:  'T','E','S','T',' ',' ',' ',' '
-- Ext:   'T','X','T'
```

### 5. Command Line is in DMA Buffer
Must copy command line BEFORE any file I/O:
```ada
-- At program start, BEFORE opening any files
Cmd_Length := Peek(16#80#);
for I in 1..Cmd_Length loop
   Args(I) := Peek(16#80# + I);
end loop;
-- Now safe to do file I/O
```

## Useful CP/M Utilities

### um80 - Universal Macro Assembler
```bash
um80 program.asm           # Creates program.rel
um80 program.asm -o out.rel
```

### ul80 - Universal Linker
```bash
ul80 -o program.com main.rel runtime.rel
ul80 -p 100 -o prog.com file.rel  # Set origin
```

### cpmemu - CP/M Emulator
```bash
cpmemu program.com         # Run CP/M program on Linux
cpmemu --z80 program.com   # Z80 mode (vs 8080)
```

## Quick Reference Tables

### BDOS Functions by Category

**Console:**
- 1: Input char
- 2: Output char
- 6: Direct I/O
- 9: Print string ($)
- 10: Read buffer
- 11: Console status

**File:**
- 15: Open
- 16: Close
- 17: Search first
- 18: Search next
- 19: Delete
- 20: Read sequential
- 21: Write sequential
- 22: Make (create)
- 23: Rename

**Disk:**
- 13: Reset disk system
- 14: Select disk
- 24: Login vector
- 25: Get current disk
- 26: Set DMA address
- 32: Get/set user number

**System:**
- 0: System reset (exit)
- 12: Get version

### CP/M Error Codes

**File operations (BDOS 15-23):**
- 0-3: Success (directory code)
- 0xFF: Error/not found

**Read/Write (BDOS 20-21):**
- 0: Success
- 1: EOF (read) or directory full (write)
- 2: Disk full (write only)
- 0xFF: Error

## Documentation References

In `docs/` directory:
- **CPM_RUNTIME.md** - Complete Ada/CP/M runtime spec
- **cpm22_bdos_calls.pdf** - All BDOS functions
- **cpm22_bios_calls.pdf** - BIOS interface
- **cpm22_memory_layout.pdf** - Memory organization

External:
- http://www.gaby.de/cpm/
- http://www.cpm.z80.de/
