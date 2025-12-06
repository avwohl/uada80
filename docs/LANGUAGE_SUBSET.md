# Ada Language Subset for uada80

## Overview

This document defines the Ada language subset to be implemented in phases, working toward full ACATS compliance.

## Phase 1: Minimal Viable Compiler (MVP)

**Goal**: Compile simple Ada programs with basic control flow and data types.

### Types

#### Scalar Types
```ada
-- Integer types (Z80 native sizes)
type Byte is range 0 .. 255;            -- 8-bit unsigned
type Short_Integer is range -128 .. 127; -- 8-bit signed
type Integer is range -32768 .. 32767;   -- 16-bit signed (default)
type Natural is range 0 .. 32767;        -- 16-bit non-negative
type Positive is range 1 .. 32767;       -- 16-bit positive

-- Boolean type
type Boolean is (False, True);

-- Character type (ASCII)
type Character is range 0 .. 127;
```

#### Type Declarations
```ada
-- Type definition
type My_Int is range -100 .. 100;

-- Subtype
subtype Index is Integer range 1 .. 10;

-- Type alias
subtype Counter is Integer;
```

### Variables and Constants

```ada
-- Variable declaration
X : Integer;
Y : Integer := 42;  -- With initialization

-- Constant declaration
Max : constant Integer := 100;
PI : constant := 3.14159;  -- Named number

-- Multiple declaration
A, B, C : Integer := 0;
```

### Expressions

#### Literals
```ada
42          -- Decimal integer
16#FF#      -- Hexadecimal
2#1010#     -- Binary
8#377#      -- Octal
'A'         -- Character
"Hello"     -- String
```

#### Operators
```ada
-- Arithmetic
+, -, *, /, mod, rem, abs, **

-- Relational
=, /=, <, <=, >, >=

-- Logical
and, or, xor, not

-- Precedence (highest to lowest):
-- 1. ** (exponentiation)
-- 2. abs, not, unary +/-
-- 3. *, /, mod, rem
-- 4. binary +, -, &
-- 5. =, /=, <, <=, >, >=
-- 6. and, or, xor
```

### Subprograms

#### Procedures
```ada
procedure Swap(X, Y : in out Integer) is
   Temp : Integer;
begin
   Temp := X;
   X := Y;
   Y := Temp;
end Swap;

-- No parameters
procedure Reset is
begin
   Counter := 0;
end Reset;
```

#### Functions
```ada
function Max(A, B : Integer) return Integer is
begin
   if A > B then
      return A;
   else
      return B;
   end if;
end Max;

-- Expression function (if simple enough)
function Is_Positive(X : Integer) return Boolean is
   (X > 0);
```

#### Parameter Modes
```ada
in      -- Read-only (default)
out     -- Write-only
in out  -- Read-write
```

### Control Flow

#### If Statement
```ada
if X > 0 then
   Y := 1;
elsif X < 0 then
   Y := -1;
else
   Y := 0;
end if;
```

#### Case Statement
```ada
case Day is
   when Monday =>
      Work;
   when Tuesday .. Thursday =>
      Work;
   when Friday =>
      Work;
      Party;
   when Saturday | Sunday =>
      Relax;
end case;
```

#### Loop Statements
```ada
-- Infinite loop
loop
   Do_Something;
   exit when Done;
end loop;

-- While loop
while X > 0 loop
   X := X - 1;
end loop;

-- For loop
for I in 1 .. 10 loop
   Sum := Sum + I;
end loop;

-- Reverse for
for I in reverse 1 .. 10 loop
   Array(I) := 0;
end loop;

-- Exit statement
exit;
exit when X = 0;
```

### Arrays

#### Constrained Arrays
```ada
-- Array type declaration
type Int_Array is array(1 .. 10) of Integer;

-- Anonymous array
Buffer : array(0 .. 255) of Byte;

-- Multi-dimensional
type Matrix is array(1 .. 3, 1 .. 3) of Integer;

-- Array access
X := A(5);
A(5) := 42;

-- Array attributes
A'First      -- First index
A'Last       -- Last index
A'Length     -- Number of elements
A'Range      -- Shorthand for A'First .. A'Last
```

### Records

#### Record Types
```ada
type Point is record
   X : Integer;
   Y : Integer;
end record;

-- Record declaration
P : Point;

-- Component access
P.X := 10;
P.Y := 20;

-- Record aggregates
P := (X => 10, Y => 20);
P := (10, 20);  -- Positional
```

### Assignment
```ada
X := 42;
A(5) := X;
P.X := Y;
```

## Phase 2: Expanded Features

### Advanced Types

#### Enumeration Types
```ada
type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
type Color is (Red, Green, Blue);

-- Enumeration attributes
Day'First        -- Mon
Day'Last         -- Sun
Day'Succ(Mon)    -- Tue
Day'Pred(Tue)    -- Mon
Day'Pos(Wed)     -- 2
Day'Val(0)       -- Mon
```

#### Derived Types
```ada
type Miles is new Integer;
type Kilometers is new Integer;

-- Cannot mix without conversion
M : Miles := 100;
K : Kilometers := Kilometers(M);  -- Explicit conversion
```

#### Access Types (Pointers)
```ada
type Int_Ptr is access Integer;

P : Int_Ptr;

-- Allocation
P := new Integer;
P := new Integer'(42);

-- Dereferencing
P.all := 10;
X := P.all;

-- Null pointer
P := null;
if P = null then ...
```

### Packages

#### Package Specification
```ada
package Math is
   Pi : constant := 3.14159;

   function Sqrt(X : Integer) return Integer;
   procedure Swap(X, Y : in out Integer);
end Math;
```

#### Package Body
```ada
package body Math is
   function Sqrt(X : Integer) return Integer is
      -- Implementation
   begin
      -- Newton's method or similar
      return Result;
   end Sqrt;

   procedure Swap(X, Y : in out Integer) is
      Temp : Integer;
   begin
      Temp := X;
      X := Y;
      Y := Temp;
   end Swap;
end Math;
```

#### Using Packages
```ada
with Math;
use Math;

X := Sqrt(16);
```

### Advanced Control Flow

#### Named Loops
```ada
Outer:
for I in 1 .. 10 loop
   Inner:
   for J in 1 .. 10 loop
      exit Outer when I * J > 50;
   end loop Inner;
end loop Outer;
```

#### Goto (limited support)
```ada
<<Label>>
goto Label;
```

### Unconstrained Arrays
```ada
type String is array(Positive range <>) of Character;

function Get_Name return String is
begin
   return "Ada";
end Get_Name;

Name : String := Get_Name;  -- Constrained by initialization
```

### Discriminated Records
```ada
type Shape(Kind : Shape_Type) is record
   case Kind is
      when Circle =>
         Radius : Integer;
      when Rectangle =>
         Width, Height : Integer;
   end case;
end record;

C : Shape(Circle);
C.Radius := 10;
```

## Phase 3: ACATS Compliance Features

### Generics

#### Generic Procedures
```ada
generic
   type Item is private;
procedure Swap(X, Y : in out Item);

procedure Swap(X, Y : in out Item) is
   Temp : Item;
begin
   Temp := X;
   X := Y;
   Y := Temp;
end Swap;

-- Instantiation
procedure Swap_Int is new Swap(Integer);
```

#### Generic Functions
```ada
generic
   type Item is private;
   with function "<"(Left, Right : Item) return Boolean;
function Max(A, B : Item) return Item;
```

#### Generic Packages
```ada
generic
   type Element is private;
   Max_Size : Positive;
package Stacks is
   type Stack is private;

   procedure Push(S : in out Stack; E : Element);
   function Pop(S : in out Stack) return Element;
private
   type Stack is record
      Data : array(1 .. Max_Size) of Element;
      Top : Natural := 0;
   end record;
end Stacks;
```

### Exception Handling

```ada
-- Exception declaration
Overflow_Error : exception;

-- Raising exceptions
raise Overflow_Error;
raise Overflow_Error with "Value too large";

-- Exception handling
begin
   X := Y / Z;
exception
   when Constraint_Error =>
      Put_Line("Division by zero");
   when Overflow_Error =>
      Put_Line("Overflow occurred");
   when others =>
      Put_Line("Unknown error");
end;
```

### Tasking (Simplified for Z80)

**Note**: Full tasking may not be feasible on Z80. Consider cooperative multitasking or simplified version.

```ada
task type Worker is
   entry Start;
   entry Stop;
end Worker;

task body Worker is
begin
   loop
      accept Start;
      -- Do work
      accept Stop;
      exit;
   end loop;
end Worker;
```

### Attributes

```ada
-- Type attributes
Integer'First
Integer'Last
Integer'Size
Integer'Base

-- Array attributes
A'First(1)       -- First index of dimension 1
A'Last(1)
A'Length(1)
A'Range(1)

-- Scalar attributes
X'Succ           -- Successor
X'Pred           -- Predecessor
X'Image          -- String representation
Character'Val(65) -- 'A'
Character'Pos('A') -- 65
```

### Representation Clauses

```ada
-- Size specification
for Integer'Size use 16;

-- Record representation
for Point use record
   X at 0 range 0 .. 15;
   Y at 2 range 0 .. 15;
end record;

-- Address clause
for Device use at 16#FF00#;
```

### Standard Library (Adapted for CP/M)

**Target Platform**: CP/M 2.2 on Z80

See [CPM_RUNTIME.md](CPM_RUNTIME.md) for complete CP/M interface specification.

```ada
-- Ada.Text_IO (CP/M BDOS interface)
package Ada.Text_IO is
   -- Console I/O (maps to BDOS functions 1, 2, 9, 11)
   procedure Put(Item : Character);
   procedure Put(Item : String);
   procedure Put_Line(Item : String);
   procedure New_Line;

   function Get return Character;
   procedure Get(Item : out Character);

   -- File I/O (maps to BDOS functions 15-23)
   type File_Type is private;
   type File_Mode is (In_File, Out_File);

   procedure Open(File : in out File_Type; Mode : File_Mode; Name : String);
   procedure Create(File : in out File_Type; Mode : File_Mode; Name : String);
   procedure Close(File : in out File_Type);
   function End_Of_File(File : File_Type) return Boolean;
end Ada.Text_IO;

-- Ada.Integer_Text_IO
package Ada.Integer_Text_IO is
   procedure Put(Item : Integer);
   procedure Get(Item : out Integer);
end Ada.Integer_Text_IO;

-- System interface to CP/M BDOS
package System.BDOS is
   procedure Console_Output(C : Character);     -- BDOS 2
   function Console_Input return Character;      -- BDOS 1
   procedure System_Reset;                       -- BDOS 0 (exit)
   -- See CPM_RUNTIME.md for complete interface
end System.BDOS;
```

## Implementation Priority

### Must Have (Phase 1)
1. Basic types: Integer, Boolean, Character
2. Variables and constants
3. Expressions and operators
4. Procedures and functions
5. If/case/loop statements
6. Arrays (constrained)
7. Records (simple)

### Should Have (Phase 2)
8. Enumerations
9. Packages
10. Access types
11. Derived types
12. Unconstrained arrays
13. Discriminated records

### Nice to Have (Phase 3)
14. Generics
15. Exceptions
16. Advanced attributes
17. Representation clauses
18. Full standard library

## ACATS Test Coverage

The ACATS test suite is organized into chapters corresponding to Ada RM chapters:

- **Chapter 2**: Lexical Elements
- **Chapter 3**: Declarations and Types
- **Chapter 4**: Names and Expressions
- **Chapter 5**: Statements
- **Chapter 6**: Subprograms
- **Chapter 7**: Packages
- **Chapter 8**: Visibility Rules
- **Chapter 9**: Tasks and Synchronization
- **Chapter 10**: Program Structure and Compilation Issues
- **Chapter 11**: Exceptions
- **Chapter 12**: Generics
- **Chapter 13**: Representation Issues

**Initial focus**: Chapters 2-6 (core language)
**Secondary focus**: Chapters 7-8, 10 (packages and structure)
**Advanced**: Chapters 11-12 (exceptions, generics)
**Optional**: Chapter 9 (tasking - may be impractical for Z80)

## Target Platform: CP/M 2.2 on Z80

The uada80 compiler targets CP/M 2.2 as its primary operating system. This provides:

- **File System**: CP/M file I/O via BDOS (functions 15-23)
- **Console I/O**: Character and string I/O via BDOS (functions 1-2, 9-11)
- **Memory**: Approximately 57K TPA on typical 64K system
- **Startup**: Programs load at 0x0100
- **Exit**: Return to CP/M via BDOS function 0

See [CPM_RUNTIME.md](CPM_RUNTIME.md) for complete details.

## Limitations for Z80/CP/M Target

1. **No floating point** (Z80 has no FPU; software implementation possible but slow)
2. **Limited memory** (~57K TPA on 64K CP/M system)
3. **No preemptive tasking** (CP/M is single-tasking; cooperative tasking possible)
4. **8.3 filenames** (CP/M file system limitation)
5. **Integer sizes**: 8-bit and 16-bit only (Z80 limitations)
6. **No 32-bit or 64-bit integers** (without software multi-precision)
7. **128-byte disk I/O** (CP/M sector size; requires buffering for text I/O)

## Test Program Examples

### Phase 1 Test
```ada
procedure Hello is
   Message : String := "Hello, Ada on Z80!";
begin
   for I in Message'Range loop
      Put(Message(I));
   end loop;
   New_Line;
end Hello;
```

### Phase 2 Test
```ada
with Ada.Text_IO;
package body Math is
   function GCD(A, B : Integer) return Integer is
      Temp : Integer;
      M : Integer := A;
      N : Integer := B;
   begin
      while N /= 0 loop
         Temp := M mod N;
         M := N;
         N := Temp;
      end loop;
      return M;
   end GCD;
end Math;
```

### Phase 3 Test
```ada
generic
   type T is private;
   with function ">"(Left, Right : T) return Boolean;
procedure Generic_Sort(A : in out array(Positive range <>) of T);
```

## Success Metrics

1. **Phase 1**: Compile and run simple programs (1-2 weeks)
2. **Phase 2**: Support packages and modular programming (1-2 months)
3. **Phase 3**: Pass core ACATS tests (3-6 months)
4. **Final**: Pass full ACATS suite (6-12 months)
