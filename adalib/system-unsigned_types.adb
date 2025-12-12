-- System.Unsigned_Types body for Z80
-- Unsigned integer types implementation

package body System.Unsigned_Types is

   --------------------
   -- To_Unsigned_8 --
   --------------------

   function To_Unsigned_8 (Value : Integer) return Unsigned_8 is
   begin
      return Unsigned_8 (Value mod 256);
   end To_Unsigned_8;

   ---------------------
   -- To_Unsigned_16 --
   ---------------------

   function To_Unsigned_16 (Value : Integer) return Unsigned_16 is
   begin
      return Unsigned_16 (Value mod 65536);
   end To_Unsigned_16;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : Unsigned_8) return Integer is
   begin
      return Integer (Value);
   end To_Integer;

   function To_Integer (Value : Unsigned_16) return Integer is
   begin
      return Integer (Value);
   end To_Integer;

   ---------------
   -- High_Byte --
   ---------------

   function High_Byte (Value : Unsigned_16) return Unsigned_8 is
   begin
      return Unsigned_8 (Value / 256);
   end High_Byte;

   --------------
   -- Low_Byte --
   --------------

   function Low_Byte (Value : Unsigned_16) return Unsigned_8 is
   begin
      return Unsigned_8 (Value mod 256);
   end Low_Byte;

   ---------------
   -- Make_Word --
   ---------------

   function Make_Word (High, Low : Unsigned_8) return Unsigned_16 is
   begin
      return Unsigned_16 (High) * 256 + Unsigned_16 (Low);
   end Make_Word;

end System.Unsigned_Types;
