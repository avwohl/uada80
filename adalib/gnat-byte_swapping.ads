-- GNAT.Byte_Swapping for Z80
-- Byte swapping utilities

with Interfaces;

package GNAT.Byte_Swapping is
   pragma Pure;

   function Swapped2 (U : Interfaces.Unsigned_16) return Interfaces.Unsigned_16;
   pragma Inline (Swapped2);
   --  Swap bytes in 16-bit value

   function Swapped4 (U : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   pragma Inline (Swapped4);
   --  Swap bytes in 32-bit value

   function Swapped8 (U : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
   pragma Inline (Swapped8);
   --  Swap bytes in 64-bit value

   procedure Swap2 (U : in Out Interfaces.Unsigned_16);
   pragma Inline (Swap2);
   --  In-place swap of 16-bit value

   procedure Swap4 (U : in Out Interfaces.Unsigned_32);
   pragma Inline (Swap4);
   --  In-place swap of 32-bit value

   procedure Swap8 (U : in Out Interfaces.Unsigned_64);
   pragma Inline (Swap8);
   --  In-place swap of 64-bit value

end GNAT.Byte_Swapping;
