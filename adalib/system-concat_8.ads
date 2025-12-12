-- System.Concat_8 for Z80
-- String concatenation of 8 operands

package System.Concat_8 is
   pragma Pure;

   procedure Str_Concat_8
     (R                              : out String;
      S1, S2, S3, S4, S5, S6, S7, S8 : String);

   procedure Str_Concat_Bounded_8
     (R                              : out String;
      Last                           : out Natural;
      S1, S2, S3, S4, S5, S6, S7, S8 : String);

end System.Concat_8;
