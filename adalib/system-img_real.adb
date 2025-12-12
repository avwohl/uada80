-- System.Img_Real body for Z80
-- Real (floating-point) image conversion to strings implementation

package body System.Img_Real is

   -----------------
   -- Image_Float --
   -----------------

   procedure Image_Float
     (V    : Long_Long_Float;
      S    : in Out String;
      P    : out Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural)
   is
      Idx : Natural := S'First;
   begin
      P := S'First - 1;
      Set_Image_Float (V, S, P, Fore, Aft, Exp);
   end Image_Float;

   ---------------------
   -- Set_Image_Float --
   ---------------------

   procedure Set_Image_Float
     (V    : Long_Long_Float;
      S    : in Out String;
      P    : in Out Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural)
   is
      Value      : Long_Long_Float := abs V;
      Exponent   : Integer := 0;
      Int_Part   : Integer;
      Frac_Part  : Long_Long_Float;
      Digit      : Integer;
      Start      : Natural := P + 1;
      Exp_Digits : String (1 .. 4) := "    ";
      Exp_Len    : Natural := 0;
   begin
      -- Handle sign
      if V < 0.0 then
         P := P + 1;
         S (P) := '-';
      else
         P := P + 1;
         S (P) := ' ';
      end if;

      -- Normalize for exponential format if needed
      if Exp > 0 and Value /= 0.0 then
         while Value >= 10.0 loop
            Value := Value / 10.0;
            Exponent := Exponent + 1;
         end loop;
         while Value < 1.0 and Value /= 0.0 loop
            Value := Value * 10.0;
            Exponent := Exponent - 1;
         end loop;
      end if;

      -- Output integer part with Fore padding
      Int_Part := Integer (Long_Long_Float'Truncation (Value));
      if Int_Part > 9 then Int_Part := 9; end if;

      -- Pad with spaces if needed
      declare
         Int_Digits : constant Natural := 1;  -- Simplified: always 1 digit
      begin
         for I in Int_Digits + 1 .. Fore loop
            P := P + 1;
            if P <= S'Last then
               S (P) := ' ';
            end if;
         end loop;
      end;

      P := P + 1;
      if P <= S'Last then
         S (P) := Character'Val (Character'Pos ('0') + Int_Part);
      end if;

      -- Output decimal point
      P := P + 1;
      if P <= S'Last then
         S (P) := '.';
      end if;

      -- Output fractional part
      Frac_Part := Value - Long_Long_Float (Int_Part);
      for I in 1 .. Aft loop
         Frac_Part := Frac_Part * 10.0;
         Digit := Integer (Long_Long_Float'Truncation (Frac_Part));
         if Digit > 9 then Digit := 9; end if;
         if Digit < 0 then Digit := 0; end if;
         P := P + 1;
         if P <= S'Last then
            S (P) := Character'Val (Character'Pos ('0') + Digit);
         end if;
         Frac_Part := Frac_Part - Long_Long_Float (Digit);
      end loop;

      -- Output exponent if needed
      if Exp > 0 then
         P := P + 1;
         if P <= S'Last then
            S (P) := 'E';
         end if;

         P := P + 1;
         if P <= S'Last then
            if Exponent >= 0 then
               S (P) := '+';
            else
               S (P) := '-';
               Exponent := abs Exponent;
            end if;
         end if;

         -- Build exponent digits
         Exp_Len := Exp;
         for I in reverse 1 .. Exp loop
            Exp_Digits (I) := Character'Val (Character'Pos ('0') + Exponent mod 10);
            Exponent := Exponent / 10;
         end loop;

         -- Output exponent digits
         for I in 1 .. Exp loop
            P := P + 1;
            if P <= S'Last then
               S (P) := Exp_Digits (I);
            end if;
         end loop;
      end if;
   end Set_Image_Float;

end System.Img_Real;
