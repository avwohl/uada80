-- System.Packed_Arrays for Z80
-- Support for packed boolean arrays and bit manipulation

package System.Packed_Arrays is
   pragma Pure;

   -- Packed boolean array in bytes (8 bits per byte)
   Max_Bits : constant := 128;  -- Maximum bits for Z80
   Max_Bytes : constant := Max_Bits / 8;

   type Bit_Array is private;

   function Create (Size : Positive) return Bit_Array;
   --  Create bit array of given size (up to Max_Bits)

   function Size (A : Bit_Array) return Natural;
   --  Return number of bits

   function Get (A : Bit_Array; Index : Natural) return Boolean;
   --  Get bit at index (0-based)

   procedure Set (A : in Out Bit_Array; Index : Natural; Value : Boolean);
   --  Set bit at index

   procedure Set_All (A : out Bit_Array; Value : Boolean);
   --  Set all bits to value

   procedure Clear (A : out Bit_Array);
   --  Clear all bits (set to False)

   function Count_True (A : Bit_Array) return Natural;
   --  Count number of True bits

   function Count_False (A : Bit_Array) return Natural;
   --  Count number of False bits

   function First_True (A : Bit_Array) return Integer;
   --  Return index of first True bit (-1 if none)

   function First_False (A : Bit_Array) return Integer;
   --  Return index of first False bit (-1 if none)

   -- Bitwise operations
   function "and" (Left, Right : Bit_Array) return Bit_Array;
   function "or" (Left, Right : Bit_Array) return Bit_Array;
   function "xor" (Left, Right : Bit_Array) return Bit_Array;
   function "not" (A : Bit_Array) return Bit_Array;

   -- Shift operations
   function Shift_Left (A : Bit_Array; Count : Natural) return Bit_Array;
   function Shift_Right (A : Bit_Array; Count : Natural) return Bit_Array;

   -- Pack/Unpack to byte arrays
   type Byte_Array is array (Positive range <>) of Natural range 0 .. 255;

   procedure Pack (A : Bit_Array; Data : out Byte_Array; Last : out Natural);
   --  Pack bits into byte array

   procedure Unpack (Data : Byte_Array; A : out Bit_Array);
   --  Unpack byte array into bits

private

   type Internal_Bytes is array (1 .. Max_Bytes) of Natural range 0 .. 255;

   type Bit_Array is record
      Data : Internal_Bytes := (others => 0);
      Bits : Natural := 0;  -- Actual number of bits
   end record;

end System.Packed_Arrays;
