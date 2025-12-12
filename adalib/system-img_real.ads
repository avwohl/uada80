-- System.Img_Real for Z80
-- Real (floating-point) image conversion to strings

package System.Img_Real is
   pragma Pure;

   -- Create image of Float value
   procedure Image_Float
     (V    : Long_Long_Float;
      S    : in Out String;
      P    : out Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural);
   -- Stores the image of V into S.
   -- P is set to the index of the last character written.
   -- Fore is minimum characters before decimal point.
   -- Aft is digits after decimal point.
   -- Exp is exponent field width (0 for no exponent).

   -- Set image of Float value
   procedure Set_Image_Float
     (V    : Long_Long_Float;
      S    : in Out String;
      P    : in Out Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural);
   -- Writes starting at S(P+1).
   -- P is updated to point to the last character written.

end System.Img_Real;
