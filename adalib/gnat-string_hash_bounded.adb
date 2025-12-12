-- GNAT.String_Hash_Bounded body for Z80
-- Bounded string hash implementation

package body GNAT.String_Hash_Bounded is

   ----------
   -- Hash --
   ----------

   function Hash (Key : Bounded.Bounded_String) return Ada.Containers.Hash_Type is
      S      : constant String := Bounded.To_String (Key);
      Result : Ada.Containers.Hash_Type := 0;
   begin
      -- FNV-1a hash
      for C of S loop
         Result := Result xor Ada.Containers.Hash_Type (Character'Pos (C));
         Result := Result * 16777619;
      end loop;
      return Result;
   end Hash;

end GNAT.String_Hash_Bounded;
