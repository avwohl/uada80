-- System.Compare_Array_Signed_16 for Z80
-- Array comparison for signed 16-bit elements

package System.Compare_Array_Signed_16 is
   pragma Pure;

   type Signed_16 is range -32768 .. 32767;
   for Signed_16'Size use 16;

   -- Compare two arrays lexicographically
   function Compare_Array_S16
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer;
   -- Returns -1 if Left < Right
   -- Returns  0 if Left = Right
   -- Returns +1 if Left > Right

end System.Compare_Array_Signed_16;
