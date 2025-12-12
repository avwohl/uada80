-- System.String_Ops for Z80
-- Low-level string operations

package System.String_Ops is
   pragma Pure;

   -- String concatenation returning result bounds
   procedure Str_Concat_Bounds
     (Lo, Hi : out Natural;
      S1, S2 : String);
   -- Sets Lo and Hi to bounds for S1 & S2

   -- String concatenation into preallocated result
   procedure Str_Concat
     (R      : out String;
      S1, S2 : String);
   -- Concatenates S1 and S2 into R

   -- Three-way string concatenation
   procedure Str_Concat_3
     (R         : out String;
      S1, S2, S3 : String);
   -- Concatenates S1 & S2 & S3 into R

   -- Four-way string concatenation
   procedure Str_Concat_4
     (R              : out String;
      S1, S2, S3, S4 : String);
   -- Concatenates S1 & S2 & S3 & S4 into R

   -- Five-way string concatenation
   procedure Str_Concat_5
     (R                  : out String;
      S1, S2, S3, S4, S5 : String);
   -- Concatenates S1 & S2 & S3 & S4 & S5 into R

   -- Character to string conversion
   function Str_From_Char (C : Character) return String;
   -- Returns a 1-character string containing C

end System.String_Ops;
