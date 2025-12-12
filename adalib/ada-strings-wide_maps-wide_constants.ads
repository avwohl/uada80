-- Ada.Strings.Wide_Maps.Wide_Constants for Z80
-- Predefined wide character sets and mappings

package Ada.Strings.Wide_Maps.Wide_Constants is
   pragma Preelaborate;

   -- Predefined Wide Character Sets
   Control_Set           : constant Wide_Character_Set;
   Graphic_Set           : constant Wide_Character_Set;
   Letter_Set            : constant Wide_Character_Set;
   Lower_Set             : constant Wide_Character_Set;
   Upper_Set             : constant Wide_Character_Set;
   Basic_Set             : constant Wide_Character_Set;
   Decimal_Digit_Set     : constant Wide_Character_Set;
   Hexadecimal_Digit_Set : constant Wide_Character_Set;
   Alphanumeric_Set      : constant Wide_Character_Set;
   Special_Set           : constant Wide_Character_Set;
   ISO_646_Set           : constant Wide_Character_Set;

   -- Predefined Wide Character Mappings
   Lower_Case_Map : constant Wide_Character_Mapping;
   Upper_Case_Map : constant Wide_Character_Mapping;
   Basic_Map      : constant Wide_Character_Mapping;

private

   -- Control characters: 0-31 and 127
   Control_Set : constant Wide_Character_Set :=
     To_Set ((Wide_Character_Range'(Wide_Character'Val (0), Wide_Character'Val (31)),
              Wide_Character_Range'(Wide_Character'Val (127), Wide_Character'Val (127))));

   -- Graphic characters: 32-126
   Graphic_Set : constant Wide_Character_Set :=
     To_Set (Wide_Character_Range'(Wide_Character'Val (32), Wide_Character'Val (126)));

   -- Lowercase letters
   Lower_Set : constant Wide_Character_Set :=
     To_Set (Wide_Character_Range'(Wide_Character'Val (Character'Pos ('a')),
                                   Wide_Character'Val (Character'Pos ('z'))));

   -- Uppercase letters
   Upper_Set : constant Wide_Character_Set :=
     To_Set (Wide_Character_Range'(Wide_Character'Val (Character'Pos ('A')),
                                   Wide_Character'Val (Character'Pos ('Z'))));

   -- All letters
   Letter_Set : constant Wide_Character_Set := Lower_Set or Upper_Set;

   -- Basic set (same as Letter_Set for ASCII)
   Basic_Set : constant Wide_Character_Set := Letter_Set;

   -- Decimal digits
   Decimal_Digit_Set : constant Wide_Character_Set :=
     To_Set (Wide_Character_Range'(Wide_Character'Val (Character'Pos ('0')),
                                   Wide_Character'Val (Character'Pos ('9'))));

   -- Hexadecimal digits
   Hexadecimal_Digit_Set : constant Wide_Character_Set :=
     Decimal_Digit_Set or
     To_Set (Wide_Character_Range'(Wide_Character'Val (Character'Pos ('A')),
                                   Wide_Character'Val (Character'Pos ('F')))) or
     To_Set (Wide_Character_Range'(Wide_Character'Val (Character'Pos ('a')),
                                   Wide_Character'Val (Character'Pos ('f'))));

   -- Alphanumeric characters
   Alphanumeric_Set : constant Wide_Character_Set := Letter_Set or Decimal_Digit_Set;

   -- Special graphic characters
   Special_Set : constant Wide_Character_Set :=
     Graphic_Set - Alphanumeric_Set - To_Set (Wide_Character'Val (32));

   -- ISO 646 set
   ISO_646_Set : constant Wide_Character_Set :=
     To_Set (Wide_Character_Range'(Wide_Character'Val (0), Wide_Character'Val (127)));

   -- Case conversion mappings
   Lower_Case_Map : constant Wide_Character_Mapping :=
     To_Mapping ("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                 "abcdefghijklmnopqrstuvwxyz");

   Upper_Case_Map : constant Wide_Character_Mapping :=
     To_Mapping ("abcdefghijklmnopqrstuvwxyz",
                 "ABCDEFGHIJKLMNOPQRSTUVWXYZ");

   -- Basic map (identity for ASCII)
   Basic_Map : constant Wide_Character_Mapping := Identity;

end Ada.Strings.Wide_Maps.Wide_Constants;
