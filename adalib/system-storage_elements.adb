-- System.Storage_Elements body for Z80
-- Low-level storage manipulation implementation

package body System.Storage_Elements is

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Value : Integer_Address) return System.Address is
   begin
      return System.Address (Value);
   end To_Address;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : System.Address) return Integer_Address is
   begin
      return Integer_Address (Value);
   end To_Integer;

end System.Storage_Elements;
