-- System.Img_Decimal body for Z80
-- Decimal fixed point image implementation

package body System.Img_Decimal is

   -------------------
   -- Image_Decimal --
   -------------------

   procedure Image_Decimal
     (V     : Long_Long_Integer;
      S     : in Out String;
      P     : out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural)
   is
      Pos : Natural := S'First;
   begin
      Set_Image_Decimal (V, S, Pos, Scale, Fore, Aft, Exp);
      P := Pos;
   end Image_Decimal;

   -----------------------
   -- Set_Image_Decimal --
   -----------------------

   procedure Set_Image_Decimal
     (V     : Long_Long_Integer;
      S     : in Out String;
      P     : in Out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural)
   is
      pragma Unreferenced (Exp);

      Abs_V      : Long_Long_Integer := abs V;
      Scale_Fact : Long_Long_Integer := 1;
      Int_Part   : Long_Long_Integer;
      Frac_Part  : Long_Long_Integer;
      Int_Buf    : String (1 .. 20);
      Int_Len    : Natural := 0;
      Frac_Buf   : String (1 .. 20);
      Frac_Len   : Natural := 0;
      Pad_Len    : Natural;
   begin
      -- Compute 10**Scale
      if Scale > 0 then
         for I in 1 .. Scale loop
            Scale_Fact := Scale_Fact * 10;
         end loop;
      end if;

      -- Split into integer and fractional parts
      if Scale > 0 then
         Int_Part := Abs_V / Scale_Fact;
         Frac_Part := Abs_V mod Scale_Fact;
      else
         Int_Part := Abs_V;
         Frac_Part := 0;
      end if;

      -- Convert integer part
      if Int_Part = 0 then
         Int_Buf (1) := '0';
         Int_Len := 1;
      else
         declare
            Temp : Long_Long_Integer := Int_Part;
         begin
            while Temp > 0 loop
               Int_Len := Int_Len + 1;
               Int_Buf (Int_Len) := Character'Val
                 (Character'Pos ('0') + Natural (Temp mod 10));
               Temp := Temp / 10;
            end loop;
         end;
      end if;

      -- Convert fractional part with leading zeros
      Frac_Len := Scale;
      if Frac_Len > 0 then
         for I in reverse 1 .. Frac_Len loop
            Frac_Buf (I) := Character'Val
              (Character'Pos ('0') + Natural (Frac_Part mod 10));
            Frac_Part := Frac_Part / 10;
         end loop;
      end if;

      -- Output with Fore padding
      declare
         Sign_Width : constant Natural := (if V < 0 then 1 else 0);
         Total_Int  : constant Natural := Sign_Width + Int_Len;
      begin
         if Total_Int < Fore then
            Pad_Len := Fore - Total_Int;
            for I in 1 .. Pad_Len loop
               S (P) := ' ';
               P := P + 1;
            end loop;
         end if;
      end;

      -- Sign
      if V < 0 then
         S (P) := '-';
         P := P + 1;
      end if;

      -- Integer part (reversed)
      for I in reverse 1 .. Int_Len loop
         S (P) := Int_Buf (I);
         P := P + 1;
      end loop;

      -- Decimal point
      S (P) := '.';
      P := P + 1;

      -- Fractional part (truncated or padded to Aft digits)
      for I in 1 .. Natural'Min (Aft, Frac_Len) loop
         S (P) := Frac_Buf (I);
         P := P + 1;
      end loop;

      -- Pad with zeros if needed
      if Aft > Frac_Len then
         for I in Frac_Len + 1 .. Aft loop
            S (P) := '0';
            P := P + 1;
         end loop;
      end if;

      P := P - 1;  -- Point to last written character
   end Set_Image_Decimal;

end System.Img_Decimal;
