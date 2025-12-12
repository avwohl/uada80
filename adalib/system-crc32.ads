-- System.CRC32 for Z80
-- CRC-32 checksum computation

package System.CRC32 is
   pragma Pure;

   -- CRC-32 value type
   type CRC32_Value is mod 2 ** 32;

   -- Initial CRC value
   Initial_CRC : constant CRC32_Value := 16#FFFFFFFF#;

   -- Update CRC with a single byte
   function Update
     (CRC  : CRC32_Value;
      Data : Character) return CRC32_Value;

   -- Update CRC with a string
   function Update
     (CRC  : CRC32_Value;
      Data : String) return CRC32_Value;

   -- Finalize CRC (invert bits)
   function Finalize (CRC : CRC32_Value) return CRC32_Value;

   -- Compute CRC of a string (complete operation)
   function CRC32 (Data : String) return CRC32_Value;

end System.CRC32;
