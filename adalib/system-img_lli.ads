-- System.Img_LLI for Z80
-- Long_Long_Integer image conversion to strings

package System.Img_LLI is
   pragma Pure;

   -- Create image of Long_Long_Integer value
   procedure Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in Out String;
      P : out Natural);
   -- Stores the image of V into S starting at S'First.
   -- P is set to the index of the last character written.

   -- Set image of Long_Long_Integer value
   procedure Set_Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in Out String;
      P : in Out Natural);
   -- Writes starting at S(P+1).
   -- P is updated to point to the last character written.

end System.Img_LLI;
