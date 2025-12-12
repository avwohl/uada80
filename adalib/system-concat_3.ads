-- System.Concat_3 for Z80
-- Three-string concatenation

package System.Concat_3 is
   pragma Pure;

   -- Concatenate three strings
   procedure Str_Concat_3
     (R      : out String;
      S1, S2 : String;
      S3     : String);

   -- Get result bounds for three-string concatenation
   procedure Str_Concat_Bounds_3
     (Lo, Hi : out Natural;
      S1, S2 : String;
      S3     : String);

end System.Concat_3;
