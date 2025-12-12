-- GNAT.Bit_Array for Z80
-- Efficient bit array operations

package GNAT.Bit_Array is
   pragma Pure;

   generic
      Size : Positive;
   package Fixed is

      type Bit_Array is private;
      --  Fixed-size bit array

      procedure Clear (B : out Bit_Array);
      --  Clear all bits

      procedure Set_All (B : out Bit_Array);
      --  Set all bits

      procedure Set (B : in Out Bit_Array; Index : Positive);
      --  Set bit at Index

      procedure Clear_Bit (B : in Out Bit_Array; Index : Positive);
      --  Clear bit at Index

      procedure Toggle (B : in Out Bit_Array; Index : Positive);
      --  Toggle bit at Index

      function Test (B : Bit_Array; Index : Positive) return Boolean;
      --  Test if bit is set

      function Count_Set (B : Bit_Array) return Natural;
      --  Count number of set bits

      function Count_Clear (B : Bit_Array) return Natural;
      --  Count number of clear bits

      function Find_First_Set (B : Bit_Array) return Natural;
      --  Find first set bit (0 if none)

      function Find_First_Clear (B : Bit_Array) return Natural;
      --  Find first clear bit (0 if none)

      function "and" (Left, Right : Bit_Array) return Bit_Array;
      function "or" (Left, Right : Bit_Array) return Bit_Array;
      function "xor" (Left, Right : Bit_Array) return Bit_Array;
      function "not" (B : Bit_Array) return Bit_Array;

   private

      Byte_Size : constant Positive := (Size + 7) / 8;
      type Byte_Array is array (1 .. Byte_Size) of Natural range 0 .. 255;

      type Bit_Array is record
         Data : Byte_Array := (others => 0);
      end record;

   end Fixed;

end GNAT.Bit_Array;
