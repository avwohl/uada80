-- Ada.Strings.Less_Case_Insensitive for Z80
-- Case-insensitive string ordering

package Ada.Strings.Less_Case_Insensitive is
   pragma Preelaborate;

   -- Case-insensitive less-than for String
   function Less (Left, Right : String) return Boolean;

end Ada.Strings.Less_Case_Insensitive;
