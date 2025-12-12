-- System.Compare_Array_Unsigned_8 for Z80
-- Compare arrays of 8-bit unsigned values

with Interfaces;

package System.Compare_Array_Unsigned_8 is
   pragma Pure;

   type Big_Bytes is array (Natural range <>) of Interfaces.Unsigned_8;

   -- Compare two arrays
   function Compare_Array_U8
     (Left      : Big_Bytes;
      Right     : Big_Bytes) return Integer;
   -- Returns -1 if Left < Right, 0 if Left = Right, +1 if Left > Right

   function Compare_Array_U8_Unaligned
     (Left      : System.Address;
      Right     : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer;
   -- Same but using addresses

end System.Compare_Array_Unsigned_8;
