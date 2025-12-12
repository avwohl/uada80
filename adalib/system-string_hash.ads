-- System.String_Hash for Z80
-- Hash function for strings

package System.String_Hash is
   pragma Pure;

   type Hash_Type is mod 2 ** 16;
   -- 16-bit hash for Z80

   -- Compute hash of a string
   function Hash (S : String) return Hash_Type;

   -- Compute hash of a string with seed
   function Hash (S : String; Seed : Hash_Type) return Hash_Type;

   -- Compute hash with case folding
   function Hash_Case_Insensitive (S : String) return Hash_Type;

end System.String_Hash;
