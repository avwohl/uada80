-- Ada.Strings.Wide_Wide_Hash for Z80
-- Hash functions for wide wide strings

with Ada.Containers;

package Ada.Strings.Wide_Wide_Hash is
   pragma Preelaborate;

   function Hash (Key : Wide_Wide_String) return Ada.Containers.Hash_Type;

end Ada.Strings.Wide_Wide_Hash;
