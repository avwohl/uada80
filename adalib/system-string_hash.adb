-- System.String_Hash body for Z80
-- Hash function for strings

package body System.String_Hash is

   -- Simple and efficient hash for Z80
   -- Uses FNV-1a style algorithm with 16-bit values

   FNV_Prime  : constant Hash_Type := 16#0193#;  -- 403
   FNV_Offset : constant Hash_Type := 16#811C#;  -- Truncated to 16 bits

   ----------
   -- Hash --
   ----------

   function Hash (S : String) return Hash_Type is
   begin
      return Hash (S, FNV_Offset);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (S : String; Seed : Hash_Type) return Hash_Type is
      H : Hash_Type := Seed;
   begin
      for I in S'Range loop
         H := H xor Hash_Type (Character'Pos (S (I)));
         H := H * FNV_Prime;
      end loop;
      return H;
   end Hash;

   ---------------------------
   -- Hash_Case_Insensitive --
   ---------------------------

   function Hash_Case_Insensitive (S : String) return Hash_Type is
      H : Hash_Type := FNV_Offset;
      C : Character;
   begin
      for I in S'Range loop
         C := S (I);
         -- Convert to uppercase for hashing
         if C in 'a' .. 'z' then
            C := Character'Val (Character'Pos (C) - 32);
         end if;
         H := H xor Hash_Type (Character'Pos (C));
         H := H * FNV_Prime;
      end loop;
      return H;
   end Hash_Case_Insensitive;

end System.String_Hash;
