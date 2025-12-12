-- GNAT.Spelling_Checker for Z80
-- String spelling suggestions

package GNAT.Spelling_Checker is
   pragma Preelaborate;

   function Is_Bad_Spelling_Of (Found, Expect : String) return Boolean;
   --  Return True if Found is a plausible misspelling of Expect

   function Edit_Distance (S1, S2 : String) return Natural;
   --  Return Levenshtein edit distance between strings

end GNAT.Spelling_Checker;
