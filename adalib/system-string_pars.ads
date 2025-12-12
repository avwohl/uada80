-- System.String_Pars for Z80
-- String parsing utilities

package System.String_Pars is
   pragma Pure;

   -- Skip whitespace
   procedure Skip_Blanks
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer);

   -- Skip to next non-blank
   procedure Skip_To_Nonblank
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer);

   -- Check if at end (only blanks remain)
   function At_End (Str : String; Ptr : Integer; Max : Integer) return Boolean;

   -- Check if character is digit
   function Is_Digit (C : Character) return Boolean;

   -- Check if character is hex digit
   function Is_Hex_Digit (C : Character) return Boolean;

   -- Get digit value (0-9)
   function Digit_Value (C : Character) return Natural;

   -- Get hex digit value (0-15)
   function Hex_Digit_Value (C : Character) return Natural;

   -- Scan integer digits, return value
   procedure Scan_Digits
     (Str   : String;
      Ptr   : in Out Integer;
      Max   : Integer;
      Value : out Natural;
      Count : out Natural);

end System.String_Pars;
