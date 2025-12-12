-- System.Compare_Array_Signed_8 for Z80
-- Array comparison for signed 8-bit elements

package System.Compare_Array_Signed_8 is
   pragma Pure;

   type Signed_8 is range -128 .. 127;
   for Signed_8'Size use 8;

   type Signed_8_Array is array (Natural range <>) of Signed_8;

   -- Compare two arrays lexicographically
   function Compare_Array_S8
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer;
   -- Returns -1 if Left < Right
   -- Returns  0 if Left = Right
   -- Returns +1 if Left > Right

   function Compare_Array_S8_Unaligned
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer;
   -- Same as above, for potentially unaligned data

end System.Compare_Array_Signed_8;
