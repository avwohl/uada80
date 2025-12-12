-- System.Img_Wchar for Z80
-- Wide_Character image (string conversion)

package System.Img_Wchar is
   pragma Pure;

   -- Convert Wide_Character to string
   procedure Image_Wide_Character
     (V : Wide_Character;
      S : in Out String;
      P : out Natural);
   -- For Latin-1 characters: 'X' form
   -- For others: ["xxxx"] hex form

   -- Maximum width of Wide_Character'Image
   Max_Wide_Character_Width : constant := 8;  -- ["xxxx"]

end System.Img_Wchar;
