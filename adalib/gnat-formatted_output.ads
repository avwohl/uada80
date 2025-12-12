-- GNAT.Formatted_Output for Z80
-- Printf-style formatted output

package GNAT.Formatted_Output is
   pragma Preelaborate;

   function Format (Fmt : String) return String;
   --  Return format string as-is

   function "&" (Fmt : String; Value : Integer) return String;
   --  Substitute %d with integer value

   function "&" (Fmt : String; Value : Float) return String;
   --  Substitute %f with float value

   function "&" (Fmt : String; Value : String) return String;
   --  Substitute %s with string value

   function "&" (Fmt : String; Value : Character) return String;
   --  Substitute %c with character value

   function "&" (Fmt : String; Value : Boolean) return String;
   --  Substitute %b with boolean value

   procedure Put (Fmt : String);
   --  Output formatted string

   procedure Put_Line (Fmt : String);
   --  Output formatted string with newline

end GNAT.Formatted_Output;
