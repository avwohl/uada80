-- System.Img_Bool body for Z80
-- Boolean image implementation

package body System.Img_Bool is

   -------------------
   -- Image_Boolean --
   -------------------

   procedure Image_Boolean
     (V : Boolean;
      S : in Out String;
      P : out Natural)
   is
   begin
      if V then
         S (S'First .. S'First + 3) := "TRUE";
         P := S'First + 3;
      else
         S (S'First .. S'First + 4) := "FALSE";
         P := S'First + 4;
      end if;
   end Image_Boolean;

end System.Img_Bool;
