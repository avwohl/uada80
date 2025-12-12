-- System.Img_Enum for Z80
-- Enumeration image (string conversion)

package System.Img_Enum is
   pragma Pure;

   -- Generic enumeration image function
   -- Used by compiler-generated Image attribute implementations

   generic
      type Enum is (<>);
   procedure Image_Enumeration
     (V : Enum;
      S : in Out String;
      P : out Natural);
   -- Writes Enum'Image(V) to S starting at S'First
   -- P returns last position written

   -- Image using position and name table
   procedure Image_Enumeration_8
     (Pos    : Natural;
      Names  : String;
      Starts : String;
      S      : in Out String;
      P      : out Natural);
   -- Pos is enumeration position
   -- Names contains concatenated literal names
   -- Starts contains starting indices (as 8-bit values)

   procedure Image_Enumeration_16
     (Pos    : Natural;
      Names  : String;
      Starts : String;
      S      : in Out String;
      P      : out Natural);
   -- Same but Starts uses 16-bit indices

end System.Img_Enum;
