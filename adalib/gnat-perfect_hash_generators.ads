-- GNAT.Perfect_Hash_Generators for Z80
-- Perfect hash function generation (simplified)

package GNAT.Perfect_Hash_Generators is
   pragma Preelaborate;

   -- Word type for hash tables
   type Word_Type is new String (1 .. 32);
   type Word_Array is array (Positive range <>) of Word_Type;

   -- Generate a simple hash function
   function Hash (S : String; Table_Size : Positive) return Natural;

   -- Hash with seed
   function Hash
     (S          : String;
      Table_Size : Positive;
      Seed       : Natural) return Natural;

end GNAT.Perfect_Hash_Generators;
