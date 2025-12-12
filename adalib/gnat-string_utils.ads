-- GNAT.String_Utils for Z80
-- Additional string utilities

package GNAT.String_Utils is
   pragma Pure;

   function Count
     (Source  : String;
      Pattern : Character) return Natural;
   --  Count occurrences of Pattern in Source

   function Count
     (Source  : String;
      Pattern : String) return Natural;
   --  Count occurrences of Pattern string in Source

   function Replace_Character
     (S    : String;
      From : Character;
      To   : Character) return String;
   --  Replace all occurrences of From with To

   function Remove_Character
     (S : String;
      C : Character) return String;
   --  Remove all occurrences of C from S

   function Duplicate
     (S     : String;
      Count : Natural) return String;
   --  Return S repeated Count times

   function Reverse_String (S : String) return String;
   --  Return S with characters reversed

   function Is_Prefix
     (Prefix : String;
      S      : String) return Boolean;
   --  Return True if S starts with Prefix

   function Is_Suffix
     (Suffix : String;
      S      : String) return Boolean;
   --  Return True if S ends with Suffix

end GNAT.String_Utils;
