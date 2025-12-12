-- GNAT.SHA1 for Z80
-- SHA-1 secure hash algorithm

package GNAT.SHA1 is
   pragma Pure;

   -- Digest (20 bytes = 160 bits)
   type Message_Digest is array (1 .. 20) of Interfaces.Unsigned_8;

   -- Hex string representation (40 characters)
   subtype Digest_String is String (1 .. 40);

   -- Context for incremental computation
   type Context is private;

   -- Initialize a context
   procedure Initialize (C : out Context);

   -- Update context with data
   procedure Update (C : in Out Context; S : String);
   procedure Update (C : in Out Context; A : Ada.Streams.Stream_Element_Array);

   -- Get final digest
   function Digest (C : Context) return Message_Digest;
   function Digest (C : Context) return Digest_String;

   -- Convenience: compute digest of a string
   function Digest (S : String) return Message_Digest;
   function Digest (S : String) return Digest_String;

private

   type Word is new Interfaces.Unsigned_32;
   type Word_Array is array (Natural range <>) of Word;

   type Context is record
      H      : Word_Array (0 .. 4);    -- Hash state
      W      : Word_Array (0 .. 79);   -- Working buffer
      Buffer : String (1 .. 64);       -- Message buffer
      Count  : Natural;                -- Buffer count
      Length : Interfaces.Unsigned_64; -- Total length
   end record;

end GNAT.SHA1;
