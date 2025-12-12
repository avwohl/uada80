-- Ada.Strings.Hash_Case_Insensitive for Z80
-- Case-insensitive string hash function

with Ada.Containers;

package Ada.Strings.Hash_Case_Insensitive is
   pragma Preelaborate;

   -- Case-insensitive hash for String
   function Hash (Key : String) return Ada.Containers.Hash_Type;

end Ada.Strings.Hash_Case_Insensitive;
