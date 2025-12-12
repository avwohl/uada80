-- System.String_Ops_Concat_3 for Z80
-- Three-argument string concatenation

package System.String_Ops_Concat_3 is
   pragma Pure;

   -- Concatenate three strings
   procedure Str_Concat_3
     (R      : out String;
      S1, S2, S3 : String);
   -- R must be large enough to hold S1 & S2 & S3

   -- Concatenate four strings
   procedure Str_Concat_4
     (R          : out String;
      S1, S2, S3, S4 : String);

   -- Concatenate five strings
   procedure Str_Concat_5
     (R              : out String;
      S1, S2, S3, S4, S5 : String);

end System.String_Ops_Concat_3;
