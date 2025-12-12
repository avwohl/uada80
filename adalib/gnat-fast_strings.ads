-- GNAT.Fast_Strings for Z80
-- Fast string operations without heap allocation

package GNAT.Fast_Strings is
   pragma Pure;

   function Equal
     (Left  : String;
      Right : String) return Boolean;
   --  Fast string equality (same as "=" but may be optimized)

   function Equal_Case_Insensitive
     (Left  : String;
      Right : String) return Boolean;
   --  Case-insensitive equality

   function Less_Than
     (Left  : String;
      Right : String) return Boolean;
   --  Fast lexicographic comparison

   function Less_Than_Case_Insensitive
     (Left  : String;
      Right : String) return Boolean;
   --  Case-insensitive lexicographic comparison

   function Index
     (Source  : String;
      Pattern : Character) return Natural;
   --  Find first occurrence of Pattern in Source (0 if not found)

   function Index
     (Source  : String;
      Pattern : String) return Natural;
   --  Find first occurrence of Pattern string in Source

   function Index_Reverse
     (Source  : String;
      Pattern : Character) return Natural;
   --  Find last occurrence of Pattern in Source

end GNAT.Fast_Strings;
