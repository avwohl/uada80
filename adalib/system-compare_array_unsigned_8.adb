-- System.Compare_Array_Unsigned_8 body for Z80
-- Compare arrays of 8-bit unsigned values implementation

with System.Storage_Elements;

package body System.Compare_Array_Unsigned_8 is

   use Interfaces;
   use System.Storage_Elements;

   ---------------------
   -- Compare_Array_U8 --
   ---------------------

   function Compare_Array_U8
     (Left      : Big_Bytes;
      Right     : Big_Bytes) return Integer
   is
      Min_Len : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      -- Compare element by element
      for I in 0 .. Min_Len - 1 loop
         if Left (Left'First + I) < Right (Right'First + I) then
            return -1;
         elsif Left (Left'First + I) > Right (Right'First + I) then
            return 1;
         end if;
      end loop;

      -- All compared elements equal, compare lengths
      if Left'Length < Right'Length then
         return -1;
      elsif Left'Length > Right'Length then
         return 1;
      else
         return 0;
      end if;
   end Compare_Array_U8;

   -------------------------------
   -- Compare_Array_U8_Unaligned --
   -------------------------------

   function Compare_Array_U8_Unaligned
     (Left      : System.Address;
      Right     : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer
   is
      type Byte_Ptr is access all Unsigned_8;
      function To_Ptr is new Ada.Unchecked_Conversion (Address, Byte_Ptr);

      L_Ptr   : Byte_Ptr := To_Ptr (Left);
      R_Ptr   : Byte_Ptr := To_Ptr (Right);
      Min_Len : constant Natural := Natural'Min (Left_Len, Right_Len);
   begin
      -- Compare byte by byte
      for I in 1 .. Min_Len loop
         declare
            L_Byte : constant Unsigned_8 := L_Ptr.all;
            R_Byte : constant Unsigned_8 := R_Ptr.all;
         begin
            if L_Byte < R_Byte then
               return -1;
            elsif L_Byte > R_Byte then
               return 1;
            end if;
         end;
         L_Ptr := To_Ptr (Left + Storage_Offset (I));
         R_Ptr := To_Ptr (Right + Storage_Offset (I));
      end loop;

      -- Compare lengths
      if Left_Len < Right_Len then
         return -1;
      elsif Left_Len > Right_Len then
         return 1;
      else
         return 0;
      end if;
   end Compare_Array_U8_Unaligned;

end System.Compare_Array_Unsigned_8;
