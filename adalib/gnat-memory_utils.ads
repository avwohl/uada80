-- GNAT.Memory_Utils for Z80
-- Memory manipulation utilities

package GNAT.Memory_Utils is
   pragma Preelaborate;

   -- Byte type
   subtype Byte is Natural range 0 .. 255;

   -- Word type (16-bit)
   subtype Word is Natural range 0 .. 65535;

   -- Memory address
   subtype Address is Word;

   -- Memory block operations
   procedure Fill (Addr : Address; Size : Word; Value : Byte);
   procedure Zero (Addr : Address; Size : Word);
   procedure Copy (Src, Dst : Address; Size : Word);
   procedure Move (Src, Dst : Address; Size : Word);  -- Handles overlap
   procedure Swap (Addr1, Addr2 : Address; Size : Word);

   -- Single byte operations
   procedure Poke (Addr : Address; Value : Byte);
   function Peek (Addr : Address) return Byte;

   -- Word operations (16-bit)
   procedure Poke_Word (Addr : Address; Value : Word);
   function Peek_Word (Addr : Address) return Word;

   -- Bit operations on memory
   procedure Set_Bit (Addr : Address; Bit : Natural);
   procedure Clear_Bit (Addr : Address; Bit : Natural);
   procedure Toggle_Bit (Addr : Address; Bit : Natural);
   function Test_Bit (Addr : Address; Bit : Natural) return Boolean;

   -- Memory comparison
   function Compare (Addr1, Addr2 : Address; Size : Word) return Integer;
   function Equal (Addr1, Addr2 : Address; Size : Word) return Boolean;

   -- Search operations
   function Find_Byte (Addr : Address; Size : Word;
                       Value : Byte) return Address;
   function Find_Word (Addr : Address; Size : Word;
                       Value : Word) return Address;
   function Count_Byte (Addr : Address; Size : Word;
                        Value : Byte) return Natural;

   -- Memory info
   function Checksum_8 (Addr : Address; Size : Word) return Byte;
   function Checksum_16 (Addr : Address; Size : Word) return Word;
   function XOR_Sum (Addr : Address; Size : Word) return Byte;

   -- High/Low byte extraction
   function High_Byte (W : Word) return Byte;
   function Low_Byte (W : Word) return Byte;
   function Make_Word (Hi, Lo : Byte) return Word;

   -- Nibble operations
   function High_Nibble (B : Byte) return Byte;
   function Low_Nibble (B : Byte) return Byte;
   function Make_Byte (Hi_Nib, Lo_Nib : Byte) return Byte;

   -- BCD conversion
   function To_BCD (Value : Byte) return Byte;
   function From_BCD (BCD : Byte) return Byte;

   -- Byte reversal
   function Reverse_Bits (B : Byte) return Byte;
   function Reverse_Bytes (W : Word) return Word;

   -- Rotate operations
   function Rotate_Left_8 (B : Byte; Count : Natural := 1) return Byte;
   function Rotate_Right_8 (B : Byte; Count : Natural := 1) return Byte;
   function Rotate_Left_16 (W : Word; Count : Natural := 1) return Word;
   function Rotate_Right_16 (W : Word; Count : Natural := 1) return Word;

   -- Shift operations
   function Shift_Left_8 (B : Byte; Count : Natural := 1) return Byte;
   function Shift_Right_8 (B : Byte; Count : Natural := 1) return Byte;
   function Shift_Left_16 (W : Word; Count : Natural := 1) return Word;
   function Shift_Right_16 (W : Word; Count : Natural := 1) return Word;

   -- Sign extension
   function Sign_Extend (B : Byte) return Integer;

   -- Bit counting
   function Count_Ones (B : Byte) return Natural;
   function Count_Zeros (B : Byte) return Natural;
   function Leading_Zeros (B : Byte) return Natural;
   function Trailing_Zeros (B : Byte) return Natural;

   -- Memory dump (returns hex string)
   function Dump_Hex (Addr : Address; Size : Natural) return String;

   -- Stack pointer (read-only)
   function Get_Stack_Pointer return Address;

   -- Free memory estimate
   function Free_Memory return Word;

   -- Z80-specific I/O port access
   procedure Port_Out (Port : Byte; Value : Byte);
   function Port_In (Port : Byte) return Byte;

end GNAT.Memory_Utils;
