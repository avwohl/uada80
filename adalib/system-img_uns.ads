-- System.Img_Uns for Z80
-- Unsigned integer image conversion to strings

with Interfaces;

package System.Img_Uns is
   pragma Pure;

   -- Create image of Unsigned value
   procedure Image_Unsigned
     (V : Interfaces.Unsigned_32;
      S : in Out String;
      P : out Natural);
   -- Stores the image of V into S starting at S'First.
   -- P is set to the index of the last character written.

   -- Set image of Unsigned value
   procedure Set_Image_Unsigned
     (V : Interfaces.Unsigned_32;
      S : in Out String;
      P : in Out Natural);
   -- Writes starting at S(P+1).
   -- P is updated to point to the last character written.

end System.Img_Uns;
