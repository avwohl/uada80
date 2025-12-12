-- GNAT.Formatted_String for Z80
-- Printf-style formatted strings

package GNAT.Formatted_String is
   pragma Preelaborate;

   type Formatted_String is private;

   -- Create from format string
   function "+" (Format : String) return Formatted_String;

   -- Append values
   function "&" (Fmt : Formatted_String; Value : String) return Formatted_String;
   function "&" (Fmt : Formatted_String; Value : Character) return Formatted_String;
   function "&" (Fmt : Formatted_String; Value : Integer) return Formatted_String;
   function "&" (Fmt : Formatted_String; Value : Float) return Formatted_String;

   -- Convert to string
   function "-" (Fmt : Formatted_String) return String;

   -- Format errors
   Format_Error : exception;

private

   Max_Format_Length : constant := 128;
   Max_Result_Length : constant := 256;

   type Formatted_String is record
      Format  : String (1 .. Max_Format_Length);
      Fmt_Len : Natural := 0;
      Result  : String (1 .. Max_Result_Length);
      Res_Len : Natural := 0;
      Arg_Pos : Natural := 1;
   end record;

end GNAT.Formatted_String;
