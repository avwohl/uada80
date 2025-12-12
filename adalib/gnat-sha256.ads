-- GNAT.SHA256 for Z80
-- SHA-256 cryptographic hash

package GNAT.SHA256 is
   pragma Preelaborate;

   type Context is private;

   -- Digest is 32 bytes (256 bits)
   subtype Binary_Digest is String (1 .. 32);
   subtype Hex_Digest is String (1 .. 64);

   -- Initialize context
   procedure Initialize (C : out Context);

   -- Update with more data
   procedure Update (C : in Out Context; Input : String);

   -- Finalize and get digest
   function Finalize (C : Context) return Binary_Digest;

   -- Get hex string digest
   function Hex_Finalize (C : Context) return Hex_Digest;

   -- One-shot hash
   function Hash (Input : String) return Binary_Digest;
   function Hex_Hash (Input : String) return Hex_Digest;

private
   type Word is mod 2**32;
   type Word_Array is array (Natural range <>) of Word;

   type Context is record
      State      : Word_Array (0 .. 7);
      Count      : Natural := 0;
      Buffer     : String (1 .. 64);
      Buffer_Len : Natural := 0;
   end record;

end GNAT.SHA256;
