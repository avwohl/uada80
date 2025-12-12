-- Ada.Strings.Hash for Z80
-- Hash functions for strings
--
-- Provides hash values for use with hashed containers

with Ada.Containers;

package Ada.Strings.Hash is
   pragma Preelaborate;

   -- Hash function for String
   function Hash (Key : String) return Ada.Containers.Hash_Type;

end Ada.Strings.Hash;
