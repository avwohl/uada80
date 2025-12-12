-- System.Concat_7 for Z80
-- String concatenation of 7 operands

package System.Concat_7 is
   pragma Pure;

   procedure Str_Concat_7
     (R                          : out String;
      S1, S2, S3, S4, S5, S6, S7 : String);

   procedure Str_Concat_Bounded_7
     (R                          : out String;
      Last                       : out Natural;
      S1, S2, S3, S4, S5, S6, S7 : String);

end System.Concat_7;
