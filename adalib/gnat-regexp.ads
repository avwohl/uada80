-- GNAT.Regexp for Z80
-- Simple regular expression matching

package GNAT.Regexp is
   pragma Preelaborate;

   type Regexp is private;

   -- Compile a pattern into a regexp
   function Compile
     (Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True) return Regexp;

   -- Match a string against a compiled regexp
   function Match (S : String; R : Regexp) return Boolean;

   -- Match a string against a pattern (convenience)
   function Match
     (S              : String;
      Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True) return Boolean;

   -- Exceptions
   Error_In_Regexp : exception;

private

   Max_Pattern_Length : constant := 64;

   type Regexp is record
      Pattern        : String (1 .. Max_Pattern_Length);
      Length         : Natural := 0;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True;
   end record;

end GNAT.Regexp;
