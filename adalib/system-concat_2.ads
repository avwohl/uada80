-- System.Concat_2 for Z80
-- String concatenation of 2 operands

package System.Concat_2 is
   pragma Pure;

   -- Concatenate two strings
   procedure Str_Concat_2
     (R      : out String;
      S1, S2 : String);
   -- R must have length = S1'Length + S2'Length

   -- Bounded concatenate (returns actual length used)
   procedure Str_Concat_Bounded_2
     (R      : out String;
      Last   : out Natural;
      S1, S2 : String);
   -- Concatenates S1 & S2 into R up to R'Last
   -- Last returns final position used

end System.Concat_2;
