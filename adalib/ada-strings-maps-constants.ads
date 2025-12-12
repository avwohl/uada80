-- Ada.Strings.Maps.Constants for Z80
-- Predefined character sets and mappings

with Ada.Strings.Maps;

package Ada.Strings.Maps.Constants is
   pragma Preelaborate;

   -- Predefined Character Sets
   Control_Set           : constant Character_Set;
   Graphic_Set           : constant Character_Set;
   Letter_Set            : constant Character_Set;
   Lower_Set             : constant Character_Set;
   Upper_Set             : constant Character_Set;
   Basic_Set             : constant Character_Set;
   Decimal_Digit_Set     : constant Character_Set;
   Hexadecimal_Digit_Set : constant Character_Set;
   Alphanumeric_Set      : constant Character_Set;
   Special_Set           : constant Character_Set;
   ISO_646_Set           : constant Character_Set;

   -- Predefined Character Mappings
   Lower_Case_Map : constant Character_Mapping;
   Upper_Case_Map : constant Character_Mapping;
   Basic_Map      : constant Character_Mapping;

private

   -- Control characters: 0-31 and 127
   Control_Set : constant Character_Set :=
     To_Set ((Character_Range'(Character'Val (0), Character'Val (31)),
              Character_Range'(Character'Val (127), Character'Val (127))));

   -- Graphic characters: 32-126
   Graphic_Set : constant Character_Set :=
     To_Set (Character_Range'(Character'Val (32), Character'Val (126)));

   -- Lowercase letters
   Lower_Set : constant Character_Set :=
     To_Set (Character_Range'('a', 'z'));

   -- Uppercase letters
   Upper_Set : constant Character_Set :=
     To_Set (Character_Range'('A', 'Z'));

   -- All letters
   Letter_Set : constant Character_Set := Lower_Set or Upper_Set;

   -- Basic set (letters without diacritics - same as Letter_Set for ASCII)
   Basic_Set : constant Character_Set := Letter_Set;

   -- Decimal digits
   Decimal_Digit_Set : constant Character_Set :=
     To_Set (Character_Range'('0', '9'));

   -- Hexadecimal digits
   Hexadecimal_Digit_Set : constant Character_Set :=
     Decimal_Digit_Set or
     To_Set (Character_Range'('A', 'F')) or
     To_Set (Character_Range'('a', 'f'));

   -- Alphanumeric characters
   Alphanumeric_Set : constant Character_Set := Letter_Set or Decimal_Digit_Set;

   -- Special graphic characters (graphic minus alphanumeric minus space)
   Special_Set : constant Character_Set :=
     Graphic_Set - Alphanumeric_Set - To_Set (' ');

   -- ISO 646 set (standard ASCII printable plus control)
   ISO_646_Set : constant Character_Set :=
     To_Set (Character_Range'(Character'Val (0), Character'Val (127)));

   -- Case conversion mappings
   Lower_Case_Map : constant Character_Mapping :=
     To_Mapping ("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                 "abcdefghijklmnopqrstuvwxyz");

   Upper_Case_Map : constant Character_Mapping :=
     To_Mapping ("abcdefghijklmnopqrstuvwxyz",
                 "ABCDEFGHIJKLMNOPQRSTUVWXYZ");

   -- Basic map (same as identity for ASCII)
   Basic_Map : constant Character_Mapping := Identity;

end Ada.Strings.Maps.Constants;
