-- System.Img_LLD for Z80
-- Image conversion for Long_Long_Integer (32-bit on Z80)

package System.Img_LLD is
   pragma Pure;

   type Long_Long_Integer is range -2 ** 31 .. 2 ** 31 - 1;

   -- Convert Long_Long_Integer to image string
   procedure Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in Out String;
      P : out Natural);

   -- Set integer with specified width
   procedure Set_Image_Long_Long_Integer
     (V     : Long_Long_Integer;
      S     : in Out String;
      P     : in Out Natural;
      Width : Natural);

end System.Img_LLD;
