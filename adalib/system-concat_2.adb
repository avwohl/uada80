-- System.Concat_2 body for Z80
-- String concatenation of 2 operands

package body System.Concat_2 is

   -------------------
   -- Str_Concat_2 --
   -------------------

   procedure Str_Concat_2
     (R      : out String;
      S1, S2 : String)
   is
      Pos : Integer := R'First;
   begin
      for I in S1'Range loop
         R (Pos) := S1 (I);
         Pos := Pos + 1;
      end loop;

      for I in S2'Range loop
         R (Pos) := S2 (I);
         Pos := Pos + 1;
      end loop;
   end Str_Concat_2;

   ---------------------------
   -- Str_Concat_Bounded_2 --
   ---------------------------

   procedure Str_Concat_Bounded_2
     (R      : out String;
      Last   : out Natural;
      S1, S2 : String)
   is
      Pos : Integer := R'First;
   begin
      for I in S1'Range loop
         exit when Pos > R'Last;
         R (Pos) := S1 (I);
         Pos := Pos + 1;
      end loop;

      for I in S2'Range loop
         exit when Pos > R'Last;
         R (Pos) := S2 (I);
         Pos := Pos + 1;
      end loop;

      Last := Pos - 1;
   end Str_Concat_Bounded_2;

end System.Concat_2;
