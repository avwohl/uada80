-- GNAT.Secure_Hashes.SHA256 for Z80
-- SHA-256 secure hash algorithm

package GNAT.Secure_Hashes.SHA256 is
   pragma Preelaborate;

   -- SHA-256 produces 32-byte (256-bit) digest
   Digest_Length : constant := 32;
   Hex_Digest_Length : constant := 64;

   type Context is new Hash_State with private;

   -- Initialize
   overriding procedure Reset (H : out Context);

   -- Update
   overriding procedure Update (H : in Out Context; Data : String);
   overriding procedure Update
     (H : in Out Context; Data : Ada.Streams.Stream_Element_Array);

   -- Finalize
   overriding function Digest (H : Context) return String;
   overriding function Hex_Digest (H : Context) return String;

   -- Convenience
   overriding function Hash (Data : String) return String;
   overriding function Hex_Hash (Data : String) return String;

private

   type Word is new Interfaces.Unsigned_32;
   type Word_Array is array (Natural range <>) of Word;

   type Context is new Hash_State with record
      State  : Word_Array (0 .. 7);
      Buffer : String (1 .. 64);
      Buf_Len : Natural := 0;
      Length : Interfaces.Unsigned_64 := 0;
   end record;

end GNAT.Secure_Hashes.SHA256;
