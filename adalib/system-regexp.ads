-- System.Regexp for Z80
-- Simple regular expression matching

package System.Regexp is
   pragma Preelaborate;

   -- Compiled regular expression
   type Regexp is private;

   -- Compile a pattern into a Regexp
   function Compile
     (Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True) return Regexp;
   -- If Glob is True, uses glob wildcards (* and ?)
   -- Otherwise uses simplified regexp

   -- Match a string against a compiled Regexp
   function Match (S : String; R : Regexp) return Boolean;

   -- Match error exception
   Error_In_Regexp : exception;

private

   Max_Pattern_Length : constant := 64;

   type Pattern_Kind is (Literal, Any_Char, Any_String, Char_Class);

   type Pattern_Element is record
      Kind   : Pattern_Kind := Literal;
      Char   : Character := ' ';
      Negate : Boolean := False;  -- For character classes
   end record;

   type Pattern_Array is array (1 .. Max_Pattern_Length) of Pattern_Element;

   type Regexp is record
      Pattern : Pattern_Array;
      Length  : Natural := 0;
      Case_Sensitive : Boolean := True;
   end record;

end System.Regexp;
