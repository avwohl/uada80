-- System.Byte_Swapping for Z80
-- Byte order conversion utilities

package System.Byte_Swapping is
   pragma Pure;

   type Unsigned_8 is mod 2 ** 8;
   type Unsigned_16 is mod 2 ** 16;
   type Unsigned_32 is mod 2 ** 32;

   -- Swap bytes in 16-bit value
   function Swap_16 (Value : Unsigned_16) return Unsigned_16;

   -- Swap bytes in 32-bit value
   function Swap_32 (Value : Unsigned_32) return Unsigned_32;

   -- Z80 is little-endian, so these convert to/from big-endian

   -- Convert from host (little-endian) to big-endian
   function To_Big_Endian_16 (Value : Unsigned_16) return Unsigned_16
     renames Swap_16;

   function To_Big_Endian_32 (Value : Unsigned_32) return Unsigned_32
     renames Swap_32;

   -- Convert from big-endian to host (little-endian)
   function From_Big_Endian_16 (Value : Unsigned_16) return Unsigned_16
     renames Swap_16;

   function From_Big_Endian_32 (Value : Unsigned_32) return Unsigned_32
     renames Swap_32;

   -- Little-endian conversions are no-ops on Z80
   function To_Little_Endian_16 (Value : Unsigned_16) return Unsigned_16;
   function To_Little_Endian_32 (Value : Unsigned_32) return Unsigned_32;
   function From_Little_Endian_16 (Value : Unsigned_16) return Unsigned_16;
   function From_Little_Endian_32 (Value : Unsigned_32) return Unsigned_32;

end System.Byte_Swapping;
