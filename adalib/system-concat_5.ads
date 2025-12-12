-- System.Concat_5 for Z80
-- String concatenation of 5 operands

package System.Concat_5 is
   pragma Pure;

   procedure Str_Concat_5
     (R                  : out String;
      S1, S2, S3, S4, S5 : String);

   procedure Str_Concat_Bounded_5
     (R                  : out String;
      Last               : out Natural;
      S1, S2, S3, S4, S5 : String);

end System.Concat_5;
