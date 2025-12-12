-- GNAT.Char_Utils for Z80
-- Character utilities

package GNAT.Char_Utils is
   pragma Pure;

   function Is_Control (C : Character) return Boolean;
   --  True if C is a control character

   function Is_Printable (C : Character) return Boolean;
   --  True if C is printable

   function Is_Graphic (C : Character) return Boolean;
   --  True if C is a graphic character

   function Is_Alphanumeric (C : Character) return Boolean;
   --  True if C is alphanumeric

   function Is_Vowel (C : Character) return Boolean;
   --  True if C is a vowel (a, e, i, o, u)

   function Is_Consonant (C : Character) return Boolean;
   --  True if C is a consonant

   function Is_Hex_Digit (C : Character) return Boolean;
   --  True if C is a hex digit (0-9, A-F, a-f)

   function Is_Octal_Digit (C : Character) return Boolean;
   --  True if C is an octal digit (0-7)

   function Is_Binary_Digit (C : Character) return Boolean;
   --  True if C is a binary digit (0-1)

   function Hex_Value (C : Character) return Natural;
   --  Return hex value of character (0-15)

   function To_Hex (Value : Natural) return Character;
   --  Return hex character for value (0-15)

   function Char_Code (C : Character) return Natural;
   --  Return ASCII code

   function Code_Char (Code : Natural) return Character;
   --  Return character for code

end GNAT.Char_Utils;
