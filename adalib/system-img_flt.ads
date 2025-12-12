-- System.Img_Flt for Z80
-- Float image conversion

package System.Img_Flt is
   pragma Pure;

   -- Convert Float to image
   procedure Image_Float
     (V : Float;
      S : in Out String;
      P : out Natural);

   -- Set Float image with specified format
   procedure Set_Image_Float
     (V   : Float;
      S   : in Out String;
      P   : in Out Natural;
      Aft : Natural;
      Exp : Natural);

end System.Img_Flt;
