-- System.Val_Spec for Z80
-- Value specification constants

package System.Val_Spec is
   pragma Pure;

   -- Maximum string length for integer images
   Max_Integer_Image_Length : constant := 12;

   -- Maximum string length for Long_Long_Integer images
   Max_Long_Long_Integer_Image_Length : constant := 21;

   -- Maximum string length for Float images
   Max_Float_Image_Length : constant := 24;

   -- Maximum string length for Long_Float images
   Max_Long_Float_Image_Length : constant := 32;

   -- Allowable bases for based literals
   Min_Base : constant := 2;
   Max_Base : constant := 16;

   -- Default base for numeric conversion
   Default_Base : constant := 10;

   -- Character values for exponent markers
   Exponent_Char_Upper : constant Character := 'E';
   Exponent_Char_Lower : constant Character := 'e';

   -- Character values for based literal markers
   Based_Char : constant Character := '#';
   Based_Alt  : constant Character := ':';

   -- Check if character is numeric base marker
   function Is_Base_Marker (C : Character) return Boolean;

   -- Check if character is exponent marker
   function Is_Exponent_Marker (C : Character) return Boolean;

end System.Val_Spec;
