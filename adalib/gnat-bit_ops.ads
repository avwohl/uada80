-- GNAT.Bit_Ops for Z80
-- Bit manipulation utilities for 8-bit and 16-bit values

package GNAT.Bit_Ops is
   pragma Pure;

   -- 8-bit operations (single byte)
   subtype Byte is Natural range 0 .. 255;
   subtype Bit_Index_8 is Natural range 0 .. 7;

   -- Get/set individual bits
   function Get_Bit (Value : Byte; Bit : Bit_Index_8) return Boolean;
   function Set_Bit (Value : Byte; Bit : Bit_Index_8) return Byte;
   function Clear_Bit (Value : Byte; Bit : Bit_Index_8) return Byte;
   function Toggle_Bit (Value : Byte; Bit : Bit_Index_8) return Byte;

   -- Set bit to specific value
   function Put_Bit (Value : Byte; Bit : Bit_Index_8;
                     State : Boolean) return Byte;

   -- Count bits
   function Bit_Count (Value : Byte) return Natural;  -- Count set bits
   function Leading_Zeros (Value : Byte) return Natural;
   function Trailing_Zeros (Value : Byte) return Natural;

   -- Find bits
   function Highest_Set_Bit (Value : Byte) return Integer;  -- -1 if none
   function Lowest_Set_Bit (Value : Byte) return Integer;   -- -1 if none

   -- Bit patterns
   function Is_Power_Of_Two (Value : Byte) return Boolean;
   function Next_Power_Of_Two (Value : Byte) return Byte;

   -- Shifts and rotations
   function Shift_Left (Value : Byte; Count : Natural) return Byte;
   function Shift_Right (Value : Byte; Count : Natural) return Byte;
   function Rotate_Left (Value : Byte; Count : Natural) return Byte;
   function Rotate_Right (Value : Byte; Count : Natural) return Byte;

   -- Nibble operations (4-bit)
   subtype Nibble is Natural range 0 .. 15;

   function High_Nibble (Value : Byte) return Nibble;
   function Low_Nibble (Value : Byte) return Nibble;
   function Make_Byte (High, Low : Nibble) return Byte;
   function Swap_Nibbles (Value : Byte) return Byte;

   -- 16-bit operations (word)
   subtype Word is Natural range 0 .. 65535;
   subtype Bit_Index_16 is Natural range 0 .. 15;

   -- Get/set individual bits (16-bit)
   function Get_Bit (Value : Word; Bit : Bit_Index_16) return Boolean;
   function Set_Bit (Value : Word; Bit : Bit_Index_16) return Word;
   function Clear_Bit (Value : Word; Bit : Bit_Index_16) return Word;
   function Toggle_Bit (Value : Word; Bit : Bit_Index_16) return Word;

   -- Bit counting (16-bit)
   function Bit_Count (Value : Word) return Natural;
   function Leading_Zeros_16 (Value : Word) return Natural;
   function Trailing_Zeros_16 (Value : Word) return Natural;

   -- Shifts and rotations (16-bit)
   function Shift_Left (Value : Word; Count : Natural) return Word;
   function Shift_Right (Value : Word; Count : Natural) return Word;
   function Rotate_Left_16 (Value : Word; Count : Natural) return Word;
   function Rotate_Right_16 (Value : Word; Count : Natural) return Word;

   -- Byte operations on words
   function High_Byte (Value : Word) return Byte;
   function Low_Byte (Value : Word) return Byte;
   function Make_Word (High, Low : Byte) return Word;
   function Swap_Bytes (Value : Word) return Word;

   -- Bitwise operations
   function Bit_And (A, B : Byte) return Byte;
   function Bit_Or (A, B : Byte) return Byte;
   function Bit_Xor (A, B : Byte) return Byte;
   function Bit_Not (Value : Byte) return Byte;

   function Bit_And (A, B : Word) return Word;
   function Bit_Or (A, B : Word) return Word;
   function Bit_Xor (A, B : Word) return Word;
   function Bit_Not (Value : Word) return Word;

   -- Bit mask generation
   function Mask_Low (N : Natural) return Byte;   -- N low bits set
   function Mask_High (N : Natural) return Byte;  -- N high bits set
   function Mask_Range (Low, High : Bit_Index_8) return Byte;

   function Mask_Low_16 (N : Natural) return Word;
   function Mask_High_16 (N : Natural) return Word;
   function Mask_Range_16 (Low, High : Bit_Index_16) return Word;

   -- Bit field extraction/insertion
   function Extract_Field (Value : Byte; Low, Width : Natural) return Byte;
   function Insert_Field (Value, Field : Byte;
                          Low, Width : Natural) return Byte;

   function Extract_Field (Value : Word; Low, Width : Natural) return Word;
   function Insert_Field (Value, Field : Word;
                          Low, Width : Natural) return Word;

   -- Parity
   function Parity (Value : Byte) return Boolean;  -- True if odd number of 1s
   function Parity (Value : Word) return Boolean;

   -- Reverse bit order
   function Reverse_Bits (Value : Byte) return Byte;
   function Reverse_Bits (Value : Word) return Word;

   -- Gray code conversion
   function To_Gray (Value : Byte) return Byte;
   function From_Gray (Value : Byte) return Byte;

end GNAT.Bit_Ops;
