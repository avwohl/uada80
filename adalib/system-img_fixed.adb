-- System.Img_Fixed body for Z80
-- Fixed point image implementation

package body System.Img_Fixed is

   -----------------
   -- Image_Fixed --
   -----------------

   procedure Image_Fixed
     (V    : Long_Long_Integer;
      S    : in Out String;
      P    : out Natural;
      Num  : Long_Long_Integer;
      Den  : Long_Long_Integer;
      For0 : Natural;
      Aft0 : Natural)
   is
      Pos : Natural := S'First;
   begin
      Set_Image_Fixed (V, S, Pos, Num, Den, For0, Aft0, 0);
      P := Pos;
   end Image_Fixed;

   ---------------------
   -- Set_Image_Fixed --
   ---------------------

   procedure Set_Image_Fixed
     (V    : Long_Long_Integer;
      S    : in Out String;
      P    : in Out Natural;
      Num  : Long_Long_Integer;
      Den  : Long_Long_Integer;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural)
   is
      pragma Unreferenced (Exp);

      Abs_V     : Long_Long_Integer := abs V;
      Int_Part  : Long_Long_Integer;
      Frac_Part : Long_Long_Integer;
      Scale     : Long_Long_Integer := 1;
      Digits    : String (1 .. 20);
      Digit_Cnt : Natural := 0;
      Frac_Str  : String (1 .. Aft);
      Start_Pos : Natural;
   begin
      -- Compute scale for fractional part (10^Aft)
      for I in 1 .. Aft loop
         Scale := Scale * 10;
      end loop;

      -- Convert scaled value to integer and fractional parts
      -- Real value = V * Num / Den
      Int_Part := Abs_V * Num / Den;
      Frac_Part := ((Abs_V * Num) mod Den) * Scale / Den;

      -- Convert integer part to string
      if Int_Part = 0 then
         Digits (1) := '0';
         Digit_Cnt := 1;
      else
         declare
            Temp : Long_Long_Integer := Int_Part;
         begin
            while Temp > 0 loop
               Digit_Cnt := Digit_Cnt + 1;
               Digits (Digit_Cnt) := Character'Val
                 (Character'Pos ('0') + Natural (Temp mod 10));
               Temp := Temp / 10;
            end loop;
         end;
      end if;

      -- Calculate starting position for proper formatting
      Start_Pos := P;

      -- Add sign if negative
      if V < 0 then
         S (Start_Pos) := '-';
         Start_Pos := Start_Pos + 1;
      end if;

      -- Pad with spaces if needed for Fore
      declare
         Int_Width : constant Natural := Digit_Cnt + (if V < 0 then 1 else 0);
      begin
         if Int_Width < Fore then
            for I in 1 .. Fore - Int_Width loop
               S (P) := ' ';
               P := P + 1;
            end loop;
            Start_Pos := P;
            if V < 0 then
               S (Start_Pos) := '-';
               Start_Pos := Start_Pos + 1;
            end if;
         end if;
      end;

      -- Output integer part (reversed)
      for I in reverse 1 .. Digit_Cnt loop
         S (Start_Pos) := Digits (I);
         Start_Pos := Start_Pos + 1;
      end loop;

      -- Output decimal point
      S (Start_Pos) := '.';
      Start_Pos := Start_Pos + 1;

      -- Convert fractional part
      for I in reverse 1 .. Aft loop
         Frac_Str (I) := Character'Val
           (Character'Pos ('0') + Natural (Frac_Part mod 10));
         Frac_Part := Frac_Part / 10;
      end loop;

      -- Output fractional part
      for I in 1 .. Aft loop
         S (Start_Pos) := Frac_Str (I);
         Start_Pos := Start_Pos + 1;
      end loop;

      P := Start_Pos - 1;
   end Set_Image_Fixed;

end System.Img_Fixed;
