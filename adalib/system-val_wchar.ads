-- System.Val_Wchar for Z80
-- Wide_Character value conversion from string

package System.Val_Wchar is
   pragma Pure;

   -- Convert string to Wide_Character
   function Value_Wide_Character (Str : String) return Wide_Character;
   -- Accepts 'X' form for single characters
   -- Accepts Hex form for encoded characters

   -- Check if string is valid Wide_Character literal
   function Is_Wide_Character_Literal (Str : String) return Boolean;

end System.Val_Wchar;
