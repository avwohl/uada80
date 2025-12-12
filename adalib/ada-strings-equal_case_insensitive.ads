-- Ada.Strings.Equal_Case_Insensitive for Z80
-- Case-insensitive string comparison

package Ada.Strings.Equal_Case_Insensitive is
   pragma Preelaborate;

   -- Case-insensitive equality for String
   function Equal (Left, Right : String) return Boolean;

end Ada.Strings.Equal_Case_Insensitive;
