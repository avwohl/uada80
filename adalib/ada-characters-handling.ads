-- Ada.Characters.Handling for Z80
-- Provides character classification and case conversion

package Ada.Characters.Handling is
   pragma Pure;

   -- Character classification functions
   function Is_Control           (Item : Character) return Boolean;
   function Is_Graphic           (Item : Character) return Boolean;
   function Is_Letter            (Item : Character) return Boolean;
   function Is_Lower             (Item : Character) return Boolean;
   function Is_Upper             (Item : Character) return Boolean;
   function Is_Basic             (Item : Character) return Boolean;
   function Is_Digit             (Item : Character) return Boolean;
   function Is_Decimal_Digit     (Item : Character) return Boolean renames Is_Digit;
   function Is_Hexadecimal_Digit (Item : Character) return Boolean;
   function Is_Alphanumeric      (Item : Character) return Boolean;
   function Is_Special           (Item : Character) return Boolean;

   -- Case conversion
   function To_Lower (Item : Character) return Character;
   function To_Upper (Item : Character) return Character;
   function To_Basic (Item : Character) return Character;

   -- String classification and conversion
   function Is_String (Item : String) return Boolean;

   function To_Lower (Item : String) return String;
   function To_Upper (Item : String) return String;
   function To_Basic (Item : String) return String;

   -- Character set membership
   subtype ISO_646 is Character range Character'Val (0) .. Character'Val (127);

   function Is_ISO_646 (Item : Character) return Boolean;
   function Is_ISO_646 (Item : String) return Boolean;

   function To_ISO_646 (Item : Character; Substitute : ISO_646 := ' ') return ISO_646;
   function To_ISO_646 (Item : String; Substitute : ISO_646 := ' ') return String;

end Ada.Characters.Handling;
