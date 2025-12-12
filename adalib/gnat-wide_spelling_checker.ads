-- GNAT.Wide_Spelling_Checker for Z80
-- Wide string spelling suggestions

package GNAT.Wide_Spelling_Checker is
   pragma Preelaborate;

   function Is_Bad_Spelling_Of (Found, Expect : Wide_String) return Boolean;
   --  Return True if Found is a plausible misspelling of Expect

   function Edit_Distance (S1, S2 : Wide_String) return Natural;
   --  Return Levenshtein edit distance between strings

end GNAT.Wide_Spelling_Checker;
