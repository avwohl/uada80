-- System.Compare_Array_Signed_8 body for Z80
-- Array comparison implementation

with System.Storage_Elements;

package body System.Compare_Array_Signed_8 is

   use System.Storage_Elements;

   ----------------------
   -- Compare_Array_S8 --
   ----------------------

   function Compare_Array_S8
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer
   is
      type S8_Ptr is access all Signed_8;
      for S8_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, S8_Ptr);

      Min_Len : constant Natural := Natural'Min (Left_Len, Right_Len);
      L_Val, R_Val : Signed_8;
   begin
      for I in 0 .. Min_Len - 1 loop
         L_Val := To_Ptr (Left + Storage_Offset (I)).all;
         R_Val := To_Ptr (Right + Storage_Offset (I)).all;

         if L_Val < R_Val then
            return -1;
         elsif L_Val > R_Val then
            return 1;
         end if;
      end loop;

      -- Equal up to Min_Len, compare lengths
      if Left_Len < Right_Len then
         return -1;
      elsif Left_Len > Right_Len then
         return 1;
      else
         return 0;
      end if;
   end Compare_Array_S8;

   --------------------------------
   -- Compare_Array_S8_Unaligned --
   --------------------------------

   function Compare_Array_S8_Unaligned
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer
   is
   begin
      -- On Z80, byte access is always aligned
      return Compare_Array_S8 (Left, Right, Left_Len, Right_Len);
   end Compare_Array_S8_Unaligned;

end System.Compare_Array_Signed_8;
