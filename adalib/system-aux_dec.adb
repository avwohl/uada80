-- System.Aux_DEC body for Z80
-- DEC Ada compatibility support implementation

with System.Storage_Elements;

package body System.Aux_DEC is

   use System.Storage_Elements;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Value : Integer) return System.Address is
   begin
      return System'To_Address (Integer_Address (Value));
   end To_Address;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : System.Address) return Integer is
   begin
      return Integer (To_Integer (Value));
   end To_Integer;

end System.Aux_DEC;
