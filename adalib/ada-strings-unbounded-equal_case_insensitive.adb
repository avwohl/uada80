-- Ada.Strings.Unbounded.Equal_Case_Insensitive body for Z80
-- Case-insensitive equality implementation

with Ada.Strings.Equal_Case_Insensitive;

function Ada.Strings.Unbounded.Equal_Case_Insensitive
  (Left, Right : Ada.Strings.Unbounded.Unbounded_String) return Boolean
is
begin
   return Ada.Strings.Equal_Case_Insensitive (To_String (Left), To_String (Right));
end Ada.Strings.Unbounded.Equal_Case_Insensitive;
