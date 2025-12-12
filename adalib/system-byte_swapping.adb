-- System.Byte_Swapping body for Z80
-- Byte order conversion utilities

package body System.Byte_Swapping is

   -------------
   -- Swap_16 --
   -------------

   function Swap_16 (Value : Unsigned_16) return Unsigned_16 is
      Hi : constant Unsigned_16 := Value / 256;
      Lo : constant Unsigned_16 := Value mod 256;
   begin
      return Lo * 256 + Hi;
   end Swap_16;

   -------------
   -- Swap_32 --
   -------------

   function Swap_32 (Value : Unsigned_32) return Unsigned_32 is
      B0 : constant Unsigned_32 := Value mod 256;
      B1 : constant Unsigned_32 := (Value / 256) mod 256;
      B2 : constant Unsigned_32 := (Value / 65536) mod 256;
      B3 : constant Unsigned_32 := Value / 16777216;
   begin
      return B0 * 16777216 + B1 * 65536 + B2 * 256 + B3;
   end Swap_32;

   -------------------------
   -- To_Little_Endian_16 --
   -------------------------

   function To_Little_Endian_16 (Value : Unsigned_16) return Unsigned_16 is
   begin
      -- Z80 is already little-endian
      return Value;
   end To_Little_Endian_16;

   -------------------------
   -- To_Little_Endian_32 --
   -------------------------

   function To_Little_Endian_32 (Value : Unsigned_32) return Unsigned_32 is
   begin
      -- Z80 is already little-endian
      return Value;
   end To_Little_Endian_32;

   ---------------------------
   -- From_Little_Endian_16 --
   ---------------------------

   function From_Little_Endian_16 (Value : Unsigned_16) return Unsigned_16 is
   begin
      -- Z80 is already little-endian
      return Value;
   end From_Little_Endian_16;

   ---------------------------
   -- From_Little_Endian_32 --
   ---------------------------

   function From_Little_Endian_32 (Value : Unsigned_32) return Unsigned_32 is
   begin
      -- Z80 is already little-endian
      return Value;
   end From_Little_Endian_32;

end System.Byte_Swapping;
