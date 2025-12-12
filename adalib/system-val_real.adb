-- System.Val_Real body for Z80
-- Real (floating-point) value conversion from strings implementation

package body System.Val_Real is

   ---------------
   -- Scan_Real --
   ---------------

   function Scan_Real
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Float
   is
      P           : Integer := Ptr.all;
      Result      : Long_Long_Float := 0.0;
      Frac        : Long_Long_Float := 0.0;
      Frac_Div    : Long_Long_Float := 1.0;
      Exp_Value   : Integer := 0;
      Negative    : Boolean := False;
      Neg_Exp     : Boolean := False;
      In_Fraction : Boolean := False;
      In_Exponent : Boolean := False;
      Digit       : Integer;
   begin
      -- Skip leading whitespace
      while P <= Max and then P <= Str'Last and then Str (P) = ' ' loop
         P := P + 1;
      end loop;

      -- Check for sign
      if P <= Max and then P <= Str'Last then
         if Str (P) = '-' then
            Negative := True;
            P := P + 1;
         elsif Str (P) = '+' then
            P := P + 1;
         end if;
      end if;

      -- Parse number
      while P <= Max and then P <= Str'Last loop
         if Str (P) in '0' .. '9' then
            Digit := Character'Pos (Str (P)) - Character'Pos ('0');
            if In_Exponent then
               Exp_Value := Exp_Value * 10 + Digit;
            elsif In_Fraction then
               Frac_Div := Frac_Div * 10.0;
               Frac := Frac + Long_Long_Float (Digit) / Frac_Div;
            else
               Result := Result * 10.0 + Long_Long_Float (Digit);
            end if;
         elsif Str (P) = '.' and not In_Fraction and not In_Exponent then
            In_Fraction := True;
         elsif (Str (P) = 'E' or Str (P) = 'e') and not In_Exponent then
            In_Exponent := True;
            In_Fraction := False;
            -- Check for exponent sign
            if P < Max and then P < Str'Last then
               if Str (P + 1) = '-' then
                  Neg_Exp := True;
                  P := P + 1;
               elsif Str (P + 1) = '+' then
                  P := P + 1;
               end if;
            end if;
         elsif Str (P) = '_' then
            null;  -- Skip underscores
         else
            exit;
         end if;
         P := P + 1;
      end loop;

      Ptr.all := P;

      -- Combine mantissa and fraction
      Result := Result + Frac;

      -- Apply exponent
      if Neg_Exp then
         Exp_Value := -Exp_Value;
      end if;

      for I in 1 .. abs Exp_Value loop
         if Exp_Value > 0 then
            Result := Result * 10.0;
         else
            Result := Result / 10.0;
         end if;
      end loop;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end Scan_Real;

   ----------------
   -- Value_Real --
   ----------------

   function Value_Real (Str : String) return Long_Long_Float is
      Ptr : aliased Integer := Str'First;
   begin
      return Scan_Real (Str, Ptr'Access, Str'Last);
   end Value_Real;

end System.Val_Real;
