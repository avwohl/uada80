-- Ada.Strings.Search for Z80
-- String search functions used by Fixed, Bounded, and Unbounded
--
-- Internal package that implements the search algorithms

package Ada.Strings.Search is
   pragma Preelaborate;

   -- Count occurrences of Pattern in Source
   function Count
     (Source  : String;
      Pattern : String) return Natural;

   -- Count characters from Set in Source
   function Count
     (Source : String;
      Set    : String) return Natural;

   -- Find first/last occurrence of Pattern
   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward) return Natural;

   -- Find first/last character from Set
   function Index
     (Source : String;
      Set    : String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   -- Find first/last non-blank character
   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural;

end Ada.Strings.Search;
