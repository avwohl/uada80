-- System.Val_Char for Z80
-- Character value conversion from string representation

package System.Val_Char is
   pragma Pure;

   -- Convert character image to Character
   function Value_Character (Str : String) return Character;
   -- Converts a string representation to Character
   -- Handles 'X' form and control character names

   -- Check if string is valid character literal
   function Is_Character_Literal (Str : String) return Boolean;

end System.Val_Char;
