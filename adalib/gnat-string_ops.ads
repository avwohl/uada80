-- GNAT.String_Ops for Z80
-- String manipulation utilities

package GNAT.String_Ops is
   pragma Preelaborate;

   function Starts_With (Str : String; Prefix : String) return Boolean;
   function Ends_With (Str : String; Suffix : String) return Boolean;

   function Contains (Str : String; Pattern : String) return Boolean;
   function Index (Str : String; Pattern : String) return Natural;
   function Index (Str : String; Pattern : String; From : Positive) return Natural;

   function Replace
     (Str     : String;
      Pattern : String;
      Replace : String) return String;
   --  Replace first occurrence

   function Replace_All
     (Str     : String;
      Pattern : String;
      Replace : String) return String;
   --  Replace all occurrences

   function Trim (Str : String) return String;
   function Trim_Left (Str : String) return String;
   function Trim_Right (Str : String) return String;

   function Repeat (Str : String; Count : Natural) return String;
   function Center (Str : String; Width : Natural; Fill : Character := ' ') return String;

end GNAT.String_Ops;
