-- Ada.Strings.Unbounded.Hash_Case_Insensitive body for Z80
-- Case-insensitive hash implementation

with Ada.Strings.Hash_Case_Insensitive;

function Ada.Strings.Unbounded.Hash_Case_Insensitive
  (Key : Ada.Strings.Unbounded.Unbounded_String) return Ada.Containers.Hash_Type
is
begin
   return Ada.Strings.Hash_Case_Insensitive (To_String (Key));
end Ada.Strings.Unbounded.Hash_Case_Insensitive;
