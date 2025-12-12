-- System.Img_Fixed for Z80
-- Fixed point image (string conversion)

package System.Img_Fixed is
   pragma Pure;

   -- Convert fixed point value to string
   procedure Image_Fixed
     (V    : Long_Long_Integer;
      S    : in out String;
      P    : out Natural;
      Num  : Long_Long_Integer;
      Den  : Long_Long_Integer;
      For0 : Natural;
      Aft0 : Natural);
   -- V is the scaled fixed value
   -- Num/Den is the small of the fixed type
   -- For0 is the Fore attribute
   -- Aft0 is the Aft attribute
   -- Result written to S, P returns last position

   -- Set fixed point image with specified precision
   procedure Set_Image_Fixed
     (V    : Long_Long_Integer;
      S    : in Out String;
      P    : in Out Natural;
      Num  : Long_Long_Integer;
      Den  : Long_Long_Integer;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural);

end System.Img_Fixed;
