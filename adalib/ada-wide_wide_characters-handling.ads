-- Ada.Wide_Wide_Characters.Handling for Z80
-- Wide wide character classification and conversion
--
-- Note: On Z80, only Latin-1 subset (0-255) is fully supported.
-- Unicode support beyond Latin-1 is minimal.

package Ada.Wide_Wide_Characters.Handling is
   pragma Pure;

   -- Character classification
   function Is_Control (Item : Wide_Wide_Character) return Boolean;
   function Is_Letter (Item : Wide_Wide_Character) return Boolean;
   function Is_Lower (Item : Wide_Wide_Character) return Boolean;
   function Is_Upper (Item : Wide_Wide_Character) return Boolean;
   function Is_Digit (Item : Wide_Wide_Character) return Boolean;
   function Is_Decimal_Digit (Item : Wide_Wide_Character) return Boolean;
   function Is_Hexadecimal_Digit (Item : Wide_Wide_Character) return Boolean;
   function Is_Alphanumeric (Item : Wide_Wide_Character) return Boolean;
   function Is_Special (Item : Wide_Wide_Character) return Boolean;
   function Is_Line_Terminator (Item : Wide_Wide_Character) return Boolean;
   function Is_Mark (Item : Wide_Wide_Character) return Boolean;
   function Is_Space (Item : Wide_Wide_Character) return Boolean;
   function Is_Graphic (Item : Wide_Wide_Character) return Boolean;

   -- Basic character conversion
   function Is_Basic (Item : Wide_Wide_Character) return Boolean;
   function To_Basic (Item : Wide_Wide_Character) return Wide_Wide_Character;

   -- Case conversion
   function To_Lower (Item : Wide_Wide_Character) return Wide_Wide_Character;
   function To_Upper (Item : Wide_Wide_Character) return Wide_Wide_Character;

   -- String case conversion
   function To_Lower (Item : Wide_Wide_String) return Wide_Wide_String;
   function To_Upper (Item : Wide_Wide_String) return Wide_Wide_String;

end Ada.Wide_Wide_Characters.Handling;
