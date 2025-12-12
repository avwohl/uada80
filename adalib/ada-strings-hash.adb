-- Ada.Strings.Hash body for Z80
-- Hash function implementation

package body Ada.Strings.Hash is

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Ada.Containers.Hash_Type is
      -- DJB2 hash algorithm (Dan Bernstein)
      -- Good distribution, efficient for Z80
      Result : Ada.Containers.Hash_Type := 5381;
   begin
      for I in Key'Range loop
         Result := Result * 33 + Ada.Containers.Hash_Type (Character'Pos (Key (I)));
      end loop;
      return Result;
   end Hash;

end Ada.Strings.Hash;
