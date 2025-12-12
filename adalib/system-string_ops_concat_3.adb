-- System.String_Ops_Concat_3 body for Z80
-- Three-argument string concatenation implementation

package body System.String_Ops_Concat_3 is

   ------------------
   -- Str_Concat_3 --
   ------------------

   procedure Str_Concat_3
     (R      : out String;
      S1, S2, S3 : String)
   is
      Pos : Integer := R'First;
   begin
      -- Copy S1
      for I in S1'Range loop
         R (Pos) := S1 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S2
      for I in S2'Range loop
         R (Pos) := S2 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S3
      for I in S3'Range loop
         R (Pos) := S3 (I);
         Pos := Pos + 1;
      end loop;
   end Str_Concat_3;

   ------------------
   -- Str_Concat_4 --
   ------------------

   procedure Str_Concat_4
     (R          : out String;
      S1, S2, S3, S4 : String)
   is
      Pos : Integer := R'First;
   begin
      -- Copy S1
      for I in S1'Range loop
         R (Pos) := S1 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S2
      for I in S2'Range loop
         R (Pos) := S2 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S3
      for I in S3'Range loop
         R (Pos) := S3 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S4
      for I in S4'Range loop
         R (Pos) := S4 (I);
         Pos := Pos + 1;
      end loop;
   end Str_Concat_4;

   ------------------
   -- Str_Concat_5 --
   ------------------

   procedure Str_Concat_5
     (R              : out String;
      S1, S2, S3, S4, S5 : String)
   is
      Pos : Integer := R'First;
   begin
      -- Copy S1
      for I in S1'Range loop
         R (Pos) := S1 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S2
      for I in S2'Range loop
         R (Pos) := S2 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S3
      for I in S3'Range loop
         R (Pos) := S3 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S4
      for I in S4'Range loop
         R (Pos) := S4 (I);
         Pos := Pos + 1;
      end loop;

      -- Copy S5
      for I in S5'Range loop
         R (Pos) := S5 (I);
         Pos := Pos + 1;
      end loop;
   end Str_Concat_5;

end System.String_Ops_Concat_3;
