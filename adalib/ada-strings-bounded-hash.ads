-- Ada.Strings.Bounded.Hash for Z80
-- Hash function for Bounded_String

with Ada.Containers;

generic
function Ada.Strings.Bounded.Hash (Key : Bounded_String)
   return Ada.Containers.Hash_Type;
pragma Preelaborate (Ada.Strings.Bounded.Hash);
