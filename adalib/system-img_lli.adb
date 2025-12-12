-- System.Img_LLI body for Z80
-- Long_Long_Integer image conversion to strings implementation

package body System.Img_LLI is

   -----------------------------
   -- Image_Long_Long_Integer --
   -----------------------------

   procedure Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in Out String;
      P : out Natural)
   is
      Buf   : String (1 .. 24);  -- Enough for 64-bit integer
      Pos   : Natural := Buf'Last;
      Value : Long_Long_Integer := abs V;
      Idx   : Natural := S'First;
   begin
      -- Convert digits (right to left)
      loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + Integer (Value mod 10));
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
   end Image_Long_Long_Integer;

   ---------------------------------
   -- Set_Image_Long_Long_Integer --
   ---------------------------------

   procedure Set_Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in Out String;
      P : in Out Natural)
   is
      Buf   : String (1 .. 24);
      Pos   : Natural := Buf'Last;
      Value : Long_Long_Integer := abs V;
   begin
      -- Convert digits (right to left)
      loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + Integer (Value mod 10));
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
   end Set_Image_Long_Long_Integer;

end System.Img_LLI;
