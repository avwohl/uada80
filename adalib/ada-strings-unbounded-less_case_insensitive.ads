-- Ada.Strings.Unbounded.Less_Case_Insensitive for Z80
-- Case-insensitive less-than for unbounded strings

with Ada.Strings.Unbounded;

function Ada.Strings.Unbounded.Less_Case_Insensitive
  (Left, Right : Ada.Strings.Unbounded.Unbounded_String) return Boolean;
pragma Preelaborate (Ada.Strings.Unbounded.Less_Case_Insensitive);
