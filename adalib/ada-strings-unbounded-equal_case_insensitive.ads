-- Ada.Strings.Unbounded.Equal_Case_Insensitive for Z80
-- Case-insensitive equality for unbounded strings

with Ada.Strings.Unbounded;

function Ada.Strings.Unbounded.Equal_Case_Insensitive
  (Left, Right : Ada.Strings.Unbounded.Unbounded_String) return Boolean;
pragma Preelaborate (Ada.Strings.Unbounded.Equal_Case_Insensitive);
