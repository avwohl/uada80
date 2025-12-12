-- Ada.Strings.Unbounded.Hash_Case_Insensitive for Z80
-- Case-insensitive hash for unbounded strings

with Ada.Strings.Unbounded;
with Ada.Containers;

function Ada.Strings.Unbounded.Hash_Case_Insensitive
  (Key : Ada.Strings.Unbounded.Unbounded_String) return Ada.Containers.Hash_Type;
pragma Preelaborate (Ada.Strings.Unbounded.Hash_Case_Insensitive);
