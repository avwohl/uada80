-- System.Concat_6 for Z80
-- String concatenation of 6 operands

package System.Concat_6 is
   pragma Pure;

   procedure Str_Concat_6
     (R                      : out String;
      S1, S2, S3, S4, S5, S6 : String);

   procedure Str_Concat_Bounded_6
     (R                      : out String;
      Last                   : out Natural;
      S1, S2, S3, S4, S5, S6 : String);

end System.Concat_6;
