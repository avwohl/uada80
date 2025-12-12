-- System.Address_Image body for Z80
-- Returns hexadecimal representation of 16-bit address

with System.Storage_Elements;

function System.Address_Image (Value : System.Address) return String is
   use System.Storage_Elements;

   Hex_Chars : constant String := "0123456789ABCDEF";
   Addr      : constant Integer_Address := To_Integer (Value);
   Result    : String (1 .. 4);  -- 16-bit = 4 hex digits
   Val       : Integer_Address := Addr;
begin
   for I in reverse Result'Range loop
      Result (I) := Hex_Chars (Integer (Val mod 16) + 1);
      Val := Val / 16;
   end loop;
   return Result;
end System.Address_Image;
