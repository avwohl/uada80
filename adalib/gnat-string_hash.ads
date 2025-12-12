-- GNAT.String_Hash for Z80
-- String hashing functions

with Ada.Containers;

package GNAT.String_Hash is
   pragma Preelaborate;

   function Hash (Key : String) return Ada.Containers.Hash_Type;
   --  Compute hash value for a string

   function Hash (Key : Wide_String) return Ada.Containers.Hash_Type;
   --  Compute hash value for a wide string

   function Hash (Key : Wide_Wide_String) return Ada.Containers.Hash_Type;
   --  Compute hash value for a wide wide string

end GNAT.String_Hash;
