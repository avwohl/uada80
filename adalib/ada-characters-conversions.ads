-- Ada.Characters.Conversions for Z80
-- Conversions between Character, Wide_Character, and Wide_Wide_Character
--
-- Note: On Z80 with limited Unicode support, conversions beyond
-- Latin-1 (0-255) may be limited.

package Ada.Characters.Conversions is
   pragma Pure;

   -- Character to Wide_Character conversion
   function To_Wide_Character (Item : Character) return Wide_Character;

   -- Wide_Character to Character conversion
   -- Returns Substitute if not representable
   function To_Character
     (Item       : Wide_Character;
      Substitute : Character := ' ') return Character;

   -- Check if Wide_Character fits in Character
   function Is_Character (Item : Wide_Character) return Boolean;

   -- String to Wide_String conversion
   function To_Wide_String (Item : String) return Wide_String;

   -- Wide_String to String conversion
   function To_String
     (Item       : Wide_String;
      Substitute : Character := ' ') return String;

   -- Character to Wide_Wide_Character conversion
   function To_Wide_Wide_Character (Item : Character) return Wide_Wide_Character;

   -- Wide_Wide_Character to Character conversion
   function To_Character
     (Item       : Wide_Wide_Character;
      Substitute : Character := ' ') return Character;

   -- Check if Wide_Wide_Character fits in Character
   function Is_Character (Item : Wide_Wide_Character) return Boolean;

   -- String to Wide_Wide_String conversion
   function To_Wide_Wide_String (Item : String) return Wide_Wide_String;

   -- Wide_Wide_String to String conversion
   function To_String
     (Item       : Wide_Wide_String;
      Substitute : Character := ' ') return String;

   -- Wide_Character to Wide_Wide_Character conversion
   function To_Wide_Wide_Character (Item : Wide_Character) return Wide_Wide_Character;

   -- Wide_Wide_Character to Wide_Character conversion
   function To_Wide_Character
     (Item       : Wide_Wide_Character;
      Substitute : Wide_Character := ' ') return Wide_Character;

   -- Check if Wide_Wide_Character fits in Wide_Character
   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean;

   -- Wide_String to Wide_Wide_String conversion
   function To_Wide_Wide_String (Item : Wide_String) return Wide_Wide_String;

   -- Wide_Wide_String to Wide_String conversion
   function To_Wide_String
     (Item       : Wide_Wide_String;
      Substitute : Wide_Character := ' ') return Wide_String;

end Ada.Characters.Conversions;
