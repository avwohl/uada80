-- GNAT.Checksum for Z80
-- Various checksum algorithms

package GNAT.Checksum is
   pragma Pure;

   type Checksum_16 is mod 2**16;  -- 16-bit checksum for Z80
   type Checksum_8 is mod 2**8;    -- 8-bit checksum

   -- Simple sum checksum
   function Sum_8 (S : String) return Checksum_8;
   function Sum_16 (S : String) return Checksum_16;

   -- XOR checksum
   function XOR_8 (S : String) return Checksum_8;
   function XOR_16 (S : String) return Checksum_16;

   -- Fletcher checksum (good error detection)
   function Fletcher_16 (S : String) return Checksum_16;

   -- BSD checksum
   function BSD_16 (S : String) return Checksum_16;

   -- Adler-like checksum (simplified for 16-bit)
   function Adler_16 (S : String) return Checksum_16;

   -- Internet checksum (RFC 1071 style, for Z80)
   function Internet_16 (S : String) return Checksum_16;

   -- Modular sum with 1's complement
   function Ones_Complement_16 (S : String) return Checksum_16;

   -- Longitudinal parity (XOR of all bytes)
   function LRC (S : String) return Checksum_8;
   --  Longitudinal Redundancy Check

   -- Two's complement checksum
   function Twos_Complement_8 (S : String) return Checksum_8;

   -- Byte array variants
   type Byte_Array is array (Positive range <>) of Natural range 0 .. 255;

   function Sum_8 (Data : Byte_Array) return Checksum_8;
   function Sum_16 (Data : Byte_Array) return Checksum_16;
   function XOR_8 (Data : Byte_Array) return Checksum_8;
   function Fletcher_16 (Data : Byte_Array) return Checksum_16;

   -- Verify functions
   function Verify_Sum_8 (S : String; Expected : Checksum_8) return Boolean;
   function Verify_Sum_16 (S : String; Expected : Checksum_16) return Boolean;
   function Verify_Fletcher_16 (S : String; Expected : Checksum_16) return Boolean;

end GNAT.Checksum;
