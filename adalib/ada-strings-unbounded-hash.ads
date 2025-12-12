-- Ada.Strings.Unbounded.Hash for Z80
-- Hash function for Unbounded_String

with Ada.Containers;

function Ada.Strings.Unbounded.Hash (Key : Unbounded_String)
   return Ada.Containers.Hash_Type;
pragma Preelaborate (Ada.Strings.Unbounded.Hash);
