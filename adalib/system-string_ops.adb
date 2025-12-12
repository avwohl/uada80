-- System.String_Ops body for Z80
-- Low-level string operations implementation

package body System.String_Ops is

   -----------------------
   -- Str_Concat_Bounds --
   -----------------------

   procedure Str_Concat_Bounds
     (Lo, Hi : out Natural;
      S1, S2 : String)
   is
   begin
      Lo := 1;
      Hi := S1'Length + S2'Length;
   end Str_Concat_Bounds;

   ----------------
   -- Str_Concat --
   ----------------

   procedure Str_Concat
     (R      : out String;
      S1, S2 : String)
   is
      Pos : Natural := R'First;
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
   end Str_Concat;

   ------------------
   -- Str_Concat_3 --
   ------------------

   procedure Str_Concat_3
     (R         : out String;
      S1, S2, S3 : String)
   is
      Pos : Natural := R'First;
   begin
      for I in S1'Range loop
         R (Pos) := S1 (I);
         Pos := Pos + 1;
      end loop;

      for I in S2'Range loop
         R (Pos) := S2 (I);
         Pos := Pos + 1;
      end loop;

      for I in S3'Range loop
         R (Pos) := S3 (I);
         Pos := Pos + 1;
      end loop;
   end Str_Concat_3;

   ------------------
   -- Str_Concat_4 --
   ------------------

   procedure Str_Concat_4
     (R              : out String;
      S1, S2, S3, S4 : String)
   is
      Pos : Natural := R'First;
   begin
      for I in S1'Range loop
         R (Pos) := S1 (I);
         Pos := Pos + 1;
      end loop;

      for I in S2'Range loop
         R (Pos) := S2 (I);
         Pos := Pos + 1;
      end loop;

      for I in S3'Range loop
         R (Pos) := S3 (I);
         Pos := Pos + 1;
      end loop;

      for I in S4'Range loop
         R (Pos) := S4 (I);
         Pos := Pos + 1;
      end loop;
   end Str_Concat_4;

   ------------------
   -- Str_Concat_5 --
   ------------------

   procedure Str_Concat_5
     (R                  : out String;
      S1, S2, S3, S4, S5 : String)
   is
      Pos : Natural := R'First;
   begin
      for I in S1'Range loop
         R (Pos) := S1 (I);
         Pos := Pos + 1;
      end loop;

      for I in S2'Range loop
         R (Pos) := S2 (I);
         Pos := Pos + 1;
      end loop;

      for I in S3'Range loop
         R (Pos) := S3 (I);
         Pos := Pos + 1;
      end loop;

      for I in S4'Range loop
         R (Pos) := S4 (I);
         Pos := Pos + 1;
      end loop;

      for I in S5'Range loop
         R (Pos) := S5 (I);
         Pos := Pos + 1;
      end loop;
   end Str_Concat_5;

   -------------------
   -- Str_From_Char --
   -------------------

   function Str_From_Char (C : Character) return String is
   begin
      return (1 => C);
   end Str_From_Char;

end System.String_Ops;
