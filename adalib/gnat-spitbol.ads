-- GNAT.Spitbol for Z80
-- SPITBOL-style pattern matching utilities

package GNAT.Spitbol is
   pragma Preelaborate;

   -- Variable-length string type (simplified)
   subtype VString is String;

   -- String functions
   function S (Str : String) return VString renames "";
   function V (Str : VString) return String renames "";

   -- Duplication
   function Dup (Str : String; Count : Natural) return String;

   -- Character position
   function Lpad (Str : String; Len : Natural; Pad : Character := ' ') return String;
   function Rpad (Str : String; Len : Natural; Pad : Character := ' ') return String;

   -- Trimming
   function Trim (Str : String) return String;
   function Ltrim (Str : String) return String;
   function Rtrim (Str : String) return String;

   -- Substitution
   function Sub
     (Source  : String;
      Start   : Positive;
      Len     : Natural;
      Replace : String) return String;

   -- Reverse
   function Reverse_String (Str : String) return String;

end GNAT.Spitbol;
