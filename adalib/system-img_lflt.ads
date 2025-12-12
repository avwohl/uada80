-- System.Img_LFlt for Z80
-- Long_Float image conversion (48-bit z88dk format)

package System.Img_LFlt is
   pragma Pure;

   -- Convert Long_Float to image
   procedure Image_Long_Float
     (V : Long_Float;
      S : in Out String;
      P : out Natural);

   -- Set Long_Float image with specified format
   procedure Set_Image_Long_Float
     (V   : Long_Float;
      S   : in Out String;
      P   : in Out Natural;
      Aft : Natural;
      Exp : Natural);

end System.Img_LFlt;
