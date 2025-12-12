-- Ada.Strings.Unbounded.Less_Case_Insensitive body for Z80
-- Case-insensitive less-than implementation

with Ada.Strings.Less_Case_Insensitive;

function Ada.Strings.Unbounded.Less_Case_Insensitive
  (Left, Right : Ada.Strings.Unbounded.Unbounded_String) return Boolean
is
begin
   return Ada.Strings.Less_Case_Insensitive (To_String (Left), To_String (Right));
end Ada.Strings.Unbounded.Less_Case_Insensitive;
