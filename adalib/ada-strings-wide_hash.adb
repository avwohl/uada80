-- Ada.Strings.Wide_Hash body for Z80
-- Wide string hash function implementation

package body Ada.Strings.Wide_Hash is

   ----------
   -- Hash --
   ----------

   function Hash (Key : Wide_String) return Ada.Containers.Hash_Type is
      -- DJB2 hash algorithm
      Result : Ada.Containers.Hash_Type := 5381;
   begin
      for I in Key'Range loop
         Result := Result * 33 +
           Ada.Containers.Hash_Type (Wide_Character'Pos (Key (I)));
      end loop;
      return Result;
   end Hash;

end Ada.Strings.Wide_Hash;
