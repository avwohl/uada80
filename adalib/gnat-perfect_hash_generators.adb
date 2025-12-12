-- GNAT.Perfect_Hash_Generators body for Z80
-- Perfect hash function generation (simplified) implementation

package body GNAT.Perfect_Hash_Generators is

   ----------
   -- Hash --
   ----------

   function Hash (S : String; Table_Size : Positive) return Natural is
   begin
      return Hash (S, Table_Size, 0);
   end Hash;

   function Hash
     (S          : String;
      Table_Size : Positive;
      Seed       : Natural) return Natural
   is
      H : Natural := Seed;
   begin
      for I in S'Range loop
         H := H * 31 + Character'Pos (S (I));
      end loop;
      return H mod Table_Size;
   end Hash;

end GNAT.Perfect_Hash_Generators;
