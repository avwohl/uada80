-- System.Concat_4 for Z80
-- String concatenation of 4 operands

package System.Concat_4 is
   pragma Pure;

   procedure Str_Concat_4
     (R              : out String;
      S1, S2, S3, S4 : String);

   procedure Str_Concat_Bounded_4
     (R              : out String;
      Last           : out Natural;
      S1, S2, S3, S4 : String);

end System.Concat_4;
