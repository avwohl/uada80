-- System.Compare_Array_Unsigned_32 for Z80
-- Array comparison for unsigned 32-bit elements

package System.Compare_Array_Unsigned_32 is
   pragma Pure;

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;

   -- Compare two arrays lexicographically
   function Compare_Array_U32
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer;

end System.Compare_Array_Unsigned_32;
