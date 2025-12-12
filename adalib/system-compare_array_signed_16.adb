-- System.Compare_Array_Signed_16 body for Z80
-- Array comparison implementation

with System.Storage_Elements;

package body System.Compare_Array_Signed_16 is

   use System.Storage_Elements;

   -----------------------
   -- Compare_Array_S16 --
   -----------------------

   function Compare_Array_S16
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer
   is
      type S16_Ptr is access all Signed_16;
      for S16_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, S16_Ptr);

      Min_Len : constant Natural := Natural'Min (Left_Len, Right_Len);
      L_Val, R_Val : Signed_16;
   begin
      for I in 0 .. Min_Len - 1 loop
         L_Val := To_Ptr (Left + Storage_Offset (I * 2)).all;
         R_Val := To_Ptr (Right + Storage_Offset (I * 2)).all;

         if L_Val < R_Val then
            return -1;
         elsif L_Val > R_Val then
            return 1;
         end if;
      end loop;

      if Left_Len < Right_Len then
         return -1;
      elsif Left_Len > Right_Len then
         return 1;
      else
         return 0;
      end if;
   end Compare_Array_S16;

end System.Compare_Array_Signed_16;
