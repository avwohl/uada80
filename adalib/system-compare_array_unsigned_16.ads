-- System.Compare_Array_Unsigned_16 for Z80
-- Array comparison for unsigned 16-bit elements

package System.Compare_Array_Unsigned_16 is
   pragma Pure;

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   -- Compare two arrays lexicographically
   function Compare_Array_U16
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer;

end System.Compare_Array_Unsigned_16;
