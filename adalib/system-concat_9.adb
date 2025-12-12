-- System.Concat_9 body for Z80
-- String concatenation of 9 operands

package body System.Concat_9 is

   procedure Str_Concat_9
     (R                                  : out String;
      S1, S2, S3, S4, S5, S6, S7, S8, S9 : String)
   is
      Pos : Integer := R'First;
   begin
      for I in S1'Range loop R (Pos) := S1 (I); Pos := Pos + 1; end loop;
      for I in S2'Range loop R (Pos) := S2 (I); Pos := Pos + 1; end loop;
      for I in S3'Range loop R (Pos) := S3 (I); Pos := Pos + 1; end loop;
      for I in S4'Range loop R (Pos) := S4 (I); Pos := Pos + 1; end loop;
      for I in S5'Range loop R (Pos) := S5 (I); Pos := Pos + 1; end loop;
      for I in S6'Range loop R (Pos) := S6 (I); Pos := Pos + 1; end loop;
      for I in S7'Range loop R (Pos) := S7 (I); Pos := Pos + 1; end loop;
      for I in S8'Range loop R (Pos) := S8 (I); Pos := Pos + 1; end loop;
      for I in S9'Range loop R (Pos) := S9 (I); Pos := Pos + 1; end loop;
   end Str_Concat_9;

   procedure Str_Concat_Bounded_9
     (R                                  : out String;
      Last                               : out Natural;
      S1, S2, S3, S4, S5, S6, S7, S8, S9 : String)
   is
      Pos : Integer := R'First;
   begin
      for I in S1'Range loop exit when Pos > R'Last; R (Pos) := S1 (I); Pos := Pos + 1; end loop;
      for I in S2'Range loop exit when Pos > R'Last; R (Pos) := S2 (I); Pos := Pos + 1; end loop;
      for I in S3'Range loop exit when Pos > R'Last; R (Pos) := S3 (I); Pos := Pos + 1; end loop;
      for I in S4'Range loop exit when Pos > R'Last; R (Pos) := S4 (I); Pos := Pos + 1; end loop;
      for I in S5'Range loop exit when Pos > R'Last; R (Pos) := S5 (I); Pos := Pos + 1; end loop;
      for I in S6'Range loop exit when Pos > R'Last; R (Pos) := S6 (I); Pos := Pos + 1; end loop;
      for I in S7'Range loop exit when Pos > R'Last; R (Pos) := S7 (I); Pos := Pos + 1; end loop;
      for I in S8'Range loop exit when Pos > R'Last; R (Pos) := S8 (I); Pos := Pos + 1; end loop;
      for I in S9'Range loop exit when Pos > R'Last; R (Pos) := S9 (I); Pos := Pos + 1; end loop;
      Last := Pos - 1;
   end Str_Concat_Bounded_9;

end System.Concat_9;
