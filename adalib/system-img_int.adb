-- System.Img_Int body for Z80
-- Integer image conversion to strings implementation

package body System.Img_Int is

   -------------------
   -- Image_Integer --
   -------------------

   procedure Image_Integer
     (V : Integer;
      S : in Out String;
      P : out Natural)
   is
      Buf   : String (1 .. 12);  -- Enough for 32-bit integer
      Pos   : Natural := Buf'Last;
      Value : Integer := abs V;
      Idx   : Natural := S'First;
   begin
      -- Convert digits (right to left)
      loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + Value mod 10);
         Value := Value / 10;
         exit when Value = 0;
         Pos := Pos - 1;
      end loop;

      -- Add leading space for non-negative or minus for negative
      if V >= 0 then
         S (Idx) := ' ';
      else
         S (Idx) := '-';
      end if;
      Idx := Idx + 1;

      -- Copy digits
      for I in Pos .. Buf'Last loop
         S (Idx) := Buf (I);
         Idx := Idx + 1;
      end loop;

      P := Idx - 1;
   end Image_Integer;

   -----------------------
   -- Set_Image_Integer --
   -----------------------

   procedure Set_Image_Integer
     (V : Integer;
      S : in Out String;
      P : in Out Natural)
   is
      Buf   : String (1 .. 12);
      Pos   : Natural := Buf'Last;
      Value : Integer := abs V;
   begin
      -- Convert digits (right to left)
      loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + Value mod 10);
         Value := Value / 10;
         exit when Value = 0;
         Pos := Pos - 1;
      end loop;

      -- Add sign
      if V < 0 then
         P := P + 1;
         S (P) := '-';
      end if;

      -- Copy digits
      for I in Pos .. Buf'Last loop
         P := P + 1;
         S (P) := Buf (I);
      end loop;
   end Set_Image_Integer;

end System.Img_Int;
