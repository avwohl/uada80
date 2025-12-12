-- System.Img_Wchar body for Z80
-- Wide_Character image implementation

package body System.Img_Wchar is

   Hex_Digits : constant String := "0123456789ABCDEF";

   --------------------------
   -- Image_Wide_Character --
   --------------------------

   procedure Image_Wide_Character
     (V : Wide_Character;
      S : in Out String;
      P : out Natural)
   is
      Code : constant Natural := Wide_Character'Pos (V);
   begin
      if Code < 128 then
         -- ASCII characters: use character image format
         if Code < 32 or Code = 127 then
            -- Control characters - use hex form
            S (S'First .. S'First + 7) :=
              "[""" &
              Hex_Digits ((Code / 4096) mod 16 + 1) &
              Hex_Digits ((Code / 256) mod 16 + 1) &
              Hex_Digits ((Code / 16) mod 16 + 1) &
              Hex_Digits (Code mod 16 + 1) &
              """]";
            P := S'First + 7;
         else
            -- Graphic ASCII: 'X' form
            S (S'First) := ''';
            S (S'First + 1) := Character'Val (Code);
            S (S'First + 2) := ''';
            P := S'First + 2;
         end if;
      elsif Code < 256 then
         -- Latin-1 graphic characters: 'X' form
         if Code >= 160 then
            S (S'First) := ''';
            S (S'First + 1) := Character'Val (Code);
            S (S'First + 2) := ''';
            P := S'First + 2;
         else
            -- C1 control characters: hex form
            S (S'First .. S'First + 7) :=
              "[""" &
              Hex_Digits ((Code / 4096) mod 16 + 1) &
              Hex_Digits ((Code / 256) mod 16 + 1) &
              Hex_Digits ((Code / 16) mod 16 + 1) &
              Hex_Digits (Code mod 16 + 1) &
              """]";
            P := S'First + 7;
         end if;
      else
         -- Non-Latin-1: hex form ["xxxx"]
         S (S'First .. S'First + 7) :=
           "[""" &
           Hex_Digits ((Code / 4096) mod 16 + 1) &
           Hex_Digits ((Code / 256) mod 16 + 1) &
           Hex_Digits ((Code / 16) mod 16 + 1) &
           Hex_Digits (Code mod 16 + 1) &
           """]";
         P := S'First + 7;
      end if;
   end Image_Wide_Character;

end System.Img_Wchar;
