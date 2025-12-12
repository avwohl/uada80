-- GNAT.Secure_Hashes for Z80
-- Secure hash algorithm interfaces

with Ada.Streams;

package GNAT.Secure_Hashes is
   pragma Pure;

   -- Generic hash context
   type Hash_State is abstract tagged private;

   -- Initialize hash state
   procedure Reset (H : out Hash_State) is abstract;

   -- Update hash with data
   procedure Update
     (H    : in Out Hash_State;
      Data : String) is abstract;

   procedure Update
     (H    : in Out Hash_State;
      Data : Ada.Streams.Stream_Element_Array) is abstract;

   -- Get hash digest as binary
   function Digest (H : Hash_State) return String is abstract;

   -- Get hash digest as hex string
   function Hex_Digest (H : Hash_State) return String is abstract;

   -- Convenience: hash a string directly
   function Hash (Data : String) return String is abstract;
   function Hex_Hash (Data : String) return String is abstract;

private

   type Hash_State is abstract tagged record
      Initialized : Boolean := False;
   end record;

end GNAT.Secure_Hashes;
