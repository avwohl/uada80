-- Ada.Strings.Bounded.Hash body for Z80
-- Hash function for Bounded_String implementation

with Ada.Strings.Hash;

function Ada.Strings.Bounded.Hash (Key : Bounded_String)
   return Ada.Containers.Hash_Type
is
begin
   return Ada.Strings.Hash (To_String (Key));
end Ada.Strings.Bounded.Hash;
