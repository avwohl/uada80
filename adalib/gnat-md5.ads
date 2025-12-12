-- GNAT.MD5 for Z80
-- MD5 message digest algorithm

package GNAT.MD5 is
   pragma Pure;

   -- MD5 digest (16 bytes = 128 bits)
   type Message_Digest is array (1 .. 16) of Interfaces.Unsigned_8;

   -- Hex string representation (32 characters)
   subtype Digest_String is String (1 .. 32);

   -- Context for incremental computation
   type Context is private;

   -- Initialize context
   procedure Initialize (C : out Context);

   -- Update context with data
   procedure Update (C : in Out Context; Input : String);
   procedure Update (C : in Out Context; Input : Ada.Streams.Stream_Element_Array);

   -- Get final digest
   function Digest (C : Context) return Message_Digest;
   function Digest (C : Context) return Digest_String;

   -- Convenience: compute digest of string
   function Digest (S : String) return Message_Digest;
   function Digest (S : String) return Digest_String;

private

   type Word is new Interfaces.Unsigned_32;
   type Word_Array is array (Natural range <>) of Word;

   type Context is record
      State  : Word_Array (0 .. 3);
      Count  : Interfaces.Unsigned_64;
      Buffer : String (1 .. 64);
      Buf_Len : Natural := 0;
   end record;

end GNAT.MD5;
