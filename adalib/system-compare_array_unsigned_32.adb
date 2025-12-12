-- System.Compare_Array_Unsigned_32 body for Z80
-- Array comparison implementation

with System.Storage_Elements;
with Ada.Unchecked_Conversion;

package body System.Compare_Array_Unsigned_32 is

   use System.Storage_Elements;

   -----------------------
   -- Compare_Array_U32 --
   -----------------------

   function Compare_Array_U32
     (Left  : System.Address;
      Right : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer
   is
      type U32_Ptr is access all Unsigned_32;
      for U32_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, U32_Ptr);

      Min_Len : constant Natural := Natural'Min (Left_Len, Right_Len);
      L_Val, R_Val : Unsigned_32;
   begin
      for I in 0 .. Min_Len - 1 loop
         L_Val := To_Ptr (Left + Storage_Offset (I * 4)).all;
         R_Val := To_Ptr (Right + Storage_Offset (I * 4)).all;

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
   end Compare_Array_U32;

end System.Compare_Array_Unsigned_32;
