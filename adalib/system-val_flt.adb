-- System.Val_Flt body for Z80
-- Float value conversion

package body System.Val_Flt is

   ----------------
   -- Scan_Float --
   ----------------

   procedure Scan_Float
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer;
      Res : out Float)
   is
      Negative : Boolean := False;
      Result   : Float := 0.0;
      Digit    : Integer;
      Got_Dot  : Boolean := False;
      Frac_Div : Float := 1.0;
      Exp      : Integer := 0;
      Exp_Neg  : Boolean := False;
   begin
      -- Skip leading blanks
      while Ptr <= Max and then Str (Ptr) = ' ' loop
         Ptr := Ptr + 1;
      end loop;

      if Ptr > Max then
         raise Constraint_Error;
      end if;

      -- Check for sign
      if Str (Ptr) = '-' then
         Negative := True;
         Ptr := Ptr + 1;
      elsif Str (Ptr) = '+' then
         Ptr := Ptr + 1;
      end if;

      -- Parse integer and fractional parts
      while Ptr <= Max loop
         if Str (Ptr) = '.' then
            if Got_Dot then
               exit;
            end if;
            Got_Dot := True;
            Ptr := Ptr + 1;
         elsif Str (Ptr) in '0' .. '9' then
            Digit := Character'Pos (Str (Ptr)) - Character'Pos ('0');
            if Got_Dot then
               Frac_Div := Frac_Div * 10.0;
               Result := Result + Float (Digit) / Frac_Div;
            else
               Result := Result * 10.0 + Float (Digit);
            end if;
            Ptr := Ptr + 1;
         elsif Str (Ptr) = '_' then
            Ptr := Ptr + 1;  -- Skip underscores
         elsif Str (Ptr) = 'E' or else Str (Ptr) = 'e' then
            Ptr := Ptr + 1;
            -- Parse exponent
            if Ptr <= Max and then Str (Ptr) = '-' then
               Exp_Neg := True;
               Ptr := Ptr + 1;
            elsif Ptr <= Max and then Str (Ptr) = '+' then
               Ptr := Ptr + 1;
            end if;

            while Ptr <= Max and then Str (Ptr) in '0' .. '9' loop
               Exp := Exp * 10 + Character'Pos (Str (Ptr)) - Character'Pos ('0');
               Ptr := Ptr + 1;
            end loop;

            if Exp_Neg then
               Exp := -Exp;
            end if;
            exit;
         else
            exit;
         end if;
      end loop;

      -- Apply exponent
      if Exp > 0 then
         for I in 1 .. Exp loop
            Result := Result * 10.0;
         end loop;
      elsif Exp < 0 then
         for I in 1 .. (-Exp) loop
            Result := Result / 10.0;
         end loop;
      end if;

      if Negative then
         Res := -Result;
      else
         Res := Result;
      end if;
   end Scan_Float;

   -----------------
   -- Value_Float --
   -----------------

   function Value_Float (Str : String) return Float is
      Ptr : Integer := Str'First;
      Res : Float;
   begin
      Scan_Float (Str, Ptr, Str'Last, Res);

      -- Skip trailing blanks
      while Ptr <= Str'Last loop
         if Str (Ptr) /= ' ' then
            raise Constraint_Error;
         end if;
         Ptr := Ptr + 1;
      end loop;

      return Res;
   end Value_Float;

end System.Val_Flt;
