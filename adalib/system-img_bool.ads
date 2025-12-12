-- System.Img_Bool for Z80
-- Boolean image (string conversion)

package System.Img_Bool is
   pragma Pure;

   -- Convert Boolean to string
   procedure Image_Boolean
     (V : Boolean;
      S : in out String;
      P : out Natural);
   -- Writes "TRUE" or "FALSE" to S starting at S'First
   -- P returns last position written

   -- Maximum width of Boolean'Image
   Max_Boolean_Image_Width : constant := 5;  -- "FALSE"

end System.Img_Bool;
