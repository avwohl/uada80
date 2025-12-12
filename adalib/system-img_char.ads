-- System.Img_Char for Z80
-- Character image (string conversion)

package System.Img_Char is
   pragma Pure;

   -- Convert Character to string
   procedure Image_Character
     (V : Character;
      S : in Out String;
      P : out Natural);
   -- For graphic characters: writes 'X' form
   -- For control characters: writes name (e.g., NUL, HT)

   -- Maximum width of Character'Image
   Max_Character_Image_Width : constant := 4;
   -- Longest names are 3 chars + potential quotes = 4

end System.Img_Char;
