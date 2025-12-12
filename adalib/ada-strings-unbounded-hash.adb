-- Ada.Strings.Unbounded.Hash body for Z80
-- Hash function for Unbounded_String implementation

with Ada.Strings.Hash;

function Ada.Strings.Unbounded.Hash (Key : Unbounded_String)
   return Ada.Containers.Hash_Type
is
begin
   return Ada.Strings.Hash (To_String (Key));
end Ada.Strings.Unbounded.Hash;
