-- System.Img_LLU for Z80
-- Long_Long_Unsigned image (string conversion)

package System.Img_LLU is
   pragma Pure;

   type Long_Long_Unsigned is mod 2 ** 64;

   -- Convert Long_Long_Unsigned to string
   procedure Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in out String;
      P : out Natural);
   -- Writes image to S starting at S'First
   -- P returns last position written

   -- Set image with specified base
   procedure Set_Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in Out String;
      P : in Out Natural;
      Base : Natural := 10);

end System.Img_LLU;
