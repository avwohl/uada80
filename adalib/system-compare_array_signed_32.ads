-- System.Compare_Array_Signed_32 for Z80
-- Array comparison for signed 32-bit elements

package System.Compare_Array_Signed_32 is
   pragma Pure;

   type Signed_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Signed_32'Size use 32;

   -- Compare two arrays lexicographically
   function Compare_Array_S32
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer;

end System.Compare_Array_Signed_32;
