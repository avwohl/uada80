-- GNAT.String_Hash body for Z80
-- String hashing functions implementation

package body GNAT.String_Hash is

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Ada.Containers.Hash_Type is
      Result : Ada.Containers.Hash_Type := 0;
   begin
      -- FNV-1a style hash
      for I in Key'Range loop
         Result := Result xor Ada.Containers.Hash_Type (Character'Pos (Key (I)));
         Result := Result * 16777619;
      end loop;
      return Result;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Wide_String) return Ada.Containers.Hash_Type is
      Result : Ada.Containers.Hash_Type := 0;
   begin
      for I in Key'Range loop
         Result := Result xor Ada.Containers.Hash_Type (Wide_Character'Pos (Key (I)));
         Result := Result * 16777619;
      end loop;
      return Result;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Wide_Wide_String) return Ada.Containers.Hash_Type is
      Result : Ada.Containers.Hash_Type := 0;
   begin
      for I in Key'Range loop
         Result := Result xor Ada.Containers.Hash_Type (Wide_Wide_Character'Pos (Key (I)));
         Result := Result * 16777619;
      end loop;
      return Result;
   end Hash;

end GNAT.String_Hash;
