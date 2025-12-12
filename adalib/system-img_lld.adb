-- System.Img_LLD body for Z80
-- Image conversion for Long_Long_Integer

package body System.Img_LLD is

   ----------------------------
   -- Image_Long_Long_Integer --
   ----------------------------

   procedure Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in Out String;
      P : out Natural)
   is
      Buf : String (1 .. 12);
      Len : Natural := 0;
      Val : Long_Long_Integer := abs V;
   begin
      -- Build digits in reverse
      if Val = 0 then
         Buf (1) := '0';
         Len := 1;
      else
         while Val > 0 loop
            Len := Len + 1;
            Buf (Len) := Character'Val (Character'Pos ('0') + Integer (Val mod 10));
            Val := Val / 10;
         end loop;
      end if;

      P := S'First;

      -- Output sign
      if V < 0 then
         S (P) := '-';
      else
         S (P) := ' ';
      end if;
      P := P + 1;

      -- Output digits in correct order
      for I in reverse 1 .. Len loop
         S (P) := Buf (I);
         P := P + 1;
      end loop;

      P := P - 1;  -- Point to last written
   end Image_Long_Long_Integer;

   --------------------------------
   -- Set_Image_Long_Long_Integer --
   --------------------------------

   procedure Set_Image_Long_Long_Integer
     (V     : Long_Long_Integer;
      S     : in Out String;
      P     : in Out Natural;
      Width : Natural)
   is
      Buf   : String (1 .. 12);
      Len   : Natural := 0;
      Val   : Long_Long_Integer := abs V;
      Total : Natural;
      Pad   : Natural;
   begin
      -- Build digits in reverse
      if Val = 0 then
         Buf (1) := '0';
         Len := 1;
      else
         while Val > 0 loop
            Len := Len + 1;
            Buf (Len) := Character'Val (Character'Pos ('0') + Integer (Val mod 10));
            Val := Val / 10;
         end loop;
      end if;

      -- Calculate total width (sign + digits)
      Total := Len + 1;  -- Always include sign position
      if Width > Total then
         Pad := Width - Total;
      else
         Pad := 0;
      end if;

      -- Output padding
      for I in 1 .. Pad loop
         S (P) := ' ';
         P := P + 1;
      end loop;

      -- Output sign
      if V < 0 then
         S (P) := '-';
      else
         S (P) := ' ';
      end if;
      P := P + 1;

      -- Output digits in correct order
      for I in reverse 1 .. Len loop
         S (P) := Buf (I);
         P := P + 1;
      end loop;

      P := P - 1;  -- Point to last written
   end Set_Image_Long_Long_Integer;

end System.Img_LLD;
