-- System.Digits for Z80
-- Digit precision constants for floating-point types

package System.Digits is
   pragma Pure;

   -- Float precision (32-bit IEEE single)
   Float_Digits : constant := 6;
   Float_Mantissa_Bits : constant := 24;
   Float_Min_Exponent : constant := -125;
   Float_Max_Exponent : constant := 128;

   -- Long_Float precision (48-bit z88dk format)
   Long_Float_Digits : constant := 11;
   Long_Float_Mantissa_Bits : constant := 40;
   Long_Float_Min_Exponent : constant := -125;
   Long_Float_Max_Exponent : constant := 128;

   -- Maximum decimal digits for output
   Max_Float_Decimal_Digits : constant := 9;
   Max_Long_Float_Decimal_Digits : constant := 15;

   -- Exponent field width
   Float_Exponent_Width : constant := 2;
   Long_Float_Exponent_Width : constant := 3;

end System.Digits;
