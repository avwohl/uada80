-- GNAT.CRC32 for Z80
-- CRC-32 checksum computation

package GNAT.CRC32 is
   pragma Pure;

   type CRC32 is private;

   -- Initialize CRC value
   procedure Initialize (C : out CRC32);

   -- Update CRC with a string
   procedure Update (C : in Out CRC32; S : String);

   -- Update CRC with a single character
   procedure Update (C : in Out CRC32; Ch : Character);

   -- Get final CRC value
   function Get_Value (C : CRC32) return Interfaces.Unsigned_32;

private

   type CRC32 is new Interfaces.Unsigned_32;

end GNAT.CRC32;
