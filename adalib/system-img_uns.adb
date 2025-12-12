-- System.Img_Uns body for Z80
-- Unsigned integer image conversion to strings implementation

package body System.Img_Uns is

   use Interfaces;

   --------------------
   -- Image_Unsigned --
   --------------------

   procedure Image_Unsigned
     (V : Unsigned_32;
      S : in Out String;
      P : out Natural)
   is
      Buf   : String (1 .. 12);  -- Enough for 32-bit unsigned
      Pos   : Natural := Buf'Last;
      Value : Unsigned_32 := V;
      Idx   : Natural := S'First;
   begin
      -- Convert digits (right to left)
      loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + Integer (Value mod 10));
         Value := Value / 10;
         exit when Value = 0;
         Pos := Pos - 1;
      end loop;

      -- Add leading space (no sign for unsigned)
      S (Idx) := ' ';
      Idx := Idx + 1;

      -- Copy digits
      for I in Pos .. Buf'Last loop
         S (Idx) := Buf (I);
         Idx := Idx + 1;
      end loop;

      P := Idx - 1;
   end Image_Unsigned;

   ------------------------
   -- Set_Image_Unsigned --
   ------------------------

   procedure Set_Image_Unsigned
     (V : Unsigned_32;
      S : in Out String;
      P : in Out Natural)
   is
      Buf   : String (1 .. 12);
      Pos   : Natural := Buf'Last;
      Value : Unsigned_32 := V;
   begin
      -- Convert digits (right to left)
      loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + Integer (Value mod 10));
         Value := Value / 10;
         exit when Value = 0;
         Pos := Pos - 1;
      end loop;

      -- Copy digits
      for I in Pos .. Buf'Last loop
         P := P + 1;
         S (P) := Buf (I);
      end loop;
   end Set_Image_Unsigned;

end System.Img_Uns;
