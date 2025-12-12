-- System.Concat_9 for Z80
-- String concatenation of 9 operands

package System.Concat_9 is
   pragma Pure;

   procedure Str_Concat_9
     (R                                  : out String;
      S1, S2, S3, S4, S5, S6, S7, S8, S9 : String);

   procedure Str_Concat_Bounded_9
     (R                                  : out String;
      Last                               : out Natural;
      S1, S2, S3, S4, S5, S6, S7, S8, S9 : String);

end System.Concat_9;
