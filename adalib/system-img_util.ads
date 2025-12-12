-- System.Img_Util for Z80
-- Common utilities for Image attribute implementations

package System.Img_Util is
   pragma Pure;

   -- Set leading blanks
   procedure Set_Blanks
     (S     : in Out String;
      Start : Natural;
      Count : Natural);
   -- Fill S(Start .. Start+Count-1) with blanks

   -- Set digit character
   procedure Set_Digit
     (S   : in Out String;
      Pos : Natural;
      Val : Natural);
   -- Set S(Pos) to character for digit value Val (0-15)

   -- Right-justify an integer in a field
   procedure Set_Integer_Width
     (S     : in Out String;
      P     : in Out Natural;
      Val   : Integer;
      Width : Natural);
   -- Output Val right-justified in Width positions

   -- Set sign character
   procedure Set_Sign
     (S        : in Out String;
      Pos      : Natural;
      Negative : Boolean);
   -- Set S(Pos) to '-' if Negative, ' ' otherwise

   -- Convert digit (0-15) to hex character
   function Hex_Digit (Val : Natural) return Character;

   -- Convert digit (0-9) to decimal character
   function Dec_Digit (Val : Natural) return Character;

end System.Img_Util;
