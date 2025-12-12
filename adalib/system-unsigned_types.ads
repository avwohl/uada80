-- System.Unsigned_Types for Z80
-- Unsigned integer types for low-level operations

package System.Unsigned_Types is
   pragma Preelaborate;

   -- 8-bit unsigned (Z80 native byte)
   type Unsigned_8 is mod 2**8;
   for Unsigned_8'Size use 8;

   -- 16-bit unsigned (Z80 native word)
   type Unsigned_16 is mod 2**16;
   for Unsigned_16'Size use 16;

   -- 32-bit unsigned (for compatibility)
   type Unsigned_32 is mod 2**32;
   for Unsigned_32'Size use 32;

   -- Type aliases
   subtype Byte is Unsigned_8;
   subtype Word is Unsigned_16;
   subtype Long_Word is Unsigned_32;

   -- Conversion functions
   function To_Unsigned_8 (Value : Integer) return Unsigned_8;
   function To_Unsigned_16 (Value : Integer) return Unsigned_16;
   function To_Integer (Value : Unsigned_8) return Integer;
   function To_Integer (Value : Unsigned_16) return Integer;

   -- High/Low byte extraction for 16-bit values
   function High_Byte (Value : Unsigned_16) return Unsigned_8;
   function Low_Byte (Value : Unsigned_16) return Unsigned_8;
   function Make_Word (High, Low : Unsigned_8) return Unsigned_16;

end System.Unsigned_Types;
