-- GNAT.Levenshtein for Z80
-- Levenshtein (edit) distance calculation

package GNAT.Levenshtein is
   pragma Pure;

   function Distance (S1, S2 : String) return Natural;
   --  Return Levenshtein distance between two strings

   function Distance_Case_Insensitive (S1, S2 : String) return Natural;
   --  Case-insensitive Levenshtein distance

   function Similarity (S1, S2 : String) return Float;
   --  Return similarity ratio (0.0 to 1.0)

   function Is_Similar
     (S1, S2     : String;
      Max_Dist   : Natural := 2) return Boolean;
   --  Check if strings are within Max_Dist edits

end GNAT.Levenshtein;
