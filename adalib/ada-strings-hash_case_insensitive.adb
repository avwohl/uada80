-- Ada.Strings.Hash_Case_Insensitive body for Z80
-- Case-insensitive string hash implementation

with Ada.Characters.Handling;

package body Ada.Strings.Hash_Case_Insensitive is

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Ada.Containers.Hash_Type is
      use Ada.Characters.Handling;
      -- DJB2 hash algorithm using lowercase characters
      Result : Ada.Containers.Hash_Type := 5381;
   begin
      for I in Key'Range loop
         Result := Result * 33 +
           Ada.Containers.Hash_Type (Character'Pos (To_Lower (Key (I))));
      end loop;
      return Result;
   end Hash;

end Ada.Strings.Hash_Case_Insensitive;
