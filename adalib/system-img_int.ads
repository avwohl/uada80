-- System.Img_Int for Z80
-- Integer image conversion to strings

package System.Img_Int is
   pragma Pure;

   -- Create image of integer value
   procedure Image_Integer
     (V : Integer;
      S : in out String;
      P : out Natural);
   -- Stores the image of V into S starting at S'First.
   -- P is set to the index of the last character written.
   -- S must be large enough to hold the result.

   -- Set image of integer value (with leading space for non-negative)
   procedure Set_Image_Integer
     (V : Integer;
      S : in Out String;
      P : in Out Natural);
   -- Like Image_Integer but writes starting at S(P+1).
   -- P is updated to point to the last character written.

end System.Img_Int;
