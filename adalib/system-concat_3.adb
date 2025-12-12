-- System.Concat_3 body for Z80
-- Three-string concatenation implementation

package body System.Concat_3 is

   ------------------
   -- Str_Concat_3 --
   ------------------

   procedure Str_Concat_3
     (R      : out String;
      S1, S2 : String;
      S3     : String)
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

      -- Copy S3
      for I in S3'Range loop
         R (Pos) := S3 (I);
         Pos := Pos + 1;
      end loop;
   end Str_Concat_3;

   -------------------------
   -- Str_Concat_Bounds_3 --
   -------------------------

   procedure Str_Concat_Bounds_3
     (Lo, Hi : out Natural;
      S1, S2 : String;
      S3     : String)
   is
      Len : constant Natural := S1'Length + S2'Length + S3'Length;
   begin
      Lo := 1;
      if Len > 0 then
         Hi := Len;
      else
         Hi := 0;
      end if;
   end Str_Concat_Bounds_3;

end System.Concat_3;
