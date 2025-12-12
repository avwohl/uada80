-- GNAT.String_Comparison for Z80
-- Various string comparison utilities

package GNAT.String_Comparison is
   pragma Pure;

   -- Basic comparisons
   function Equal (Left, Right : String) return Boolean;
   function Equal_Case_Insensitive (Left, Right : String) return Boolean;
   function Less_Than (Left, Right : String) return Boolean;
   function Less_Than_Case_Insensitive (Left, Right : String) return Boolean;

   -- Prefix/Suffix matching
   function Starts_With (Source, Prefix : String) return Boolean;
   function Starts_With_Case_Insensitive (Source, Prefix : String) return Boolean;
   function Ends_With (Source, Suffix : String) return Boolean;
   function Ends_With_Case_Insensitive (Source, Suffix : String) return Boolean;

   -- Substring matching
   function Contains (Source, Pattern : String) return Boolean;
   function Contains_Case_Insensitive (Source, Pattern : String) return Boolean;

   -- Natural sorting (handles embedded numbers)
   function Natural_Compare (Left, Right : String) return Integer;
   --  Returns -1, 0, or 1; handles "file2" < "file10"

   function Natural_Less_Than (Left, Right : String) return Boolean;

   -- Wildcard matching
   function Match_Wildcard (Source, Pattern : String) return Boolean;
   --  Simple wildcards: * matches any sequence, ? matches single char

   -- Similarity measures
   function Common_Prefix_Length (Left, Right : String) return Natural;
   function Common_Suffix_Length (Left, Right : String) return Natural;
   function Hamming_Distance (Left, Right : String) return Natural;
   --  Count of differing characters (strings must be same length)

   -- Normalized comparison
   function Equal_Normalized (Left, Right : String) return Boolean;
   --  Ignores leading/trailing whitespace, collapses internal whitespace

   -- Empty/blank checks
   function Is_Empty (S : String) return Boolean;
   function Is_Blank (S : String) return Boolean;
   function Is_Whitespace (C : Character) return Boolean;

   -- Character class checks
   function Is_Alpha (C : Character) return Boolean;
   function Is_Digit (C : Character) return Boolean;
   function Is_Alphanumeric (C : Character) return Boolean;

   function All_Alpha (S : String) return Boolean;
   function All_Digit (S : String) return Boolean;
   function All_Alphanumeric (S : String) return Boolean;

end GNAT.String_Comparison;
