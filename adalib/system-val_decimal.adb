-- System.Val_Decimal body for Z80
-- Decimal fixed point value conversion implementation

package body System.Val_Decimal is

   ------------------
   -- Scan_Decimal --
   ------------------

   function Scan_Decimal
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Scale : Integer) return Long_Long_Integer
   is
      Pos       : Integer := Ptr.all;
      Negative  : Boolean := False;
      Int_Part  : Long_Long_Integer := 0;
      Frac_Part : Long_Long_Integer := 0;
      Frac_Len  : Natural := 0;
      C         : Character;
   begin
      -- Skip leading spaces
      while Pos <= Max and Str (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      -- Check for sign
      if Pos <= Max then
         if Str (Pos) = '-' then
            Negative := True;
            Pos := Pos + 1;
         elsif Str (Pos) = '+' then
            Pos := Pos + 1;
         end if;
      end if;

      -- Parse integer part
      while Pos <= Max loop
         C := Str (Pos);
         if C in '0' .. '9' then
            Int_Part := Int_Part * 10 +
              Long_Long_Integer (Character'Pos (C) - Character'Pos ('0'));
            Pos := Pos + 1;
         elsif C = '_' then
            Pos := Pos + 1;
         else
            exit;
         end if;
      end loop;

      -- Check for decimal point
      if Pos <= Max and Str (Pos) = '.' then
         Pos := Pos + 1;

         -- Parse fractional part
         while Pos <= Max loop
            C := Str (Pos);
            if C in '0' .. '9' then
               Frac_Part := Frac_Part * 10 +
                 Long_Long_Integer (Character'Pos (C) - Character'Pos ('0'));
               Frac_Len := Frac_Len + 1;
               Pos := Pos + 1;
            elsif C = '_' then
               Pos := Pos + 1;
            else
               exit;
            end if;
         end loop;
      end if;

      Ptr.all := Pos;

      -- Scale the result
      declare
         Result      : Long_Long_Integer;
         Scale_Fact  : Long_Long_Integer := 1;
         Frac_Scale  : Long_Long_Integer := 1;
      begin
         -- Compute 10**Scale
         if Scale > 0 then
            for I in 1 .. Scale loop
               Scale_Fact := Scale_Fact * 10;
            end loop;
         elsif Scale < 0 then
            for I in 1 .. (-Scale) loop
               Scale_Fact := Scale_Fact * 10;
            end loop;
         end if;

         -- Compute 10**Frac_Len
         for I in 1 .. Frac_Len loop
            Frac_Scale := Frac_Scale * 10;
         end loop;

         if Scale >= 0 then
            Result := Int_Part * Scale_Fact +
                      (Frac_Part * Scale_Fact) / Frac_Scale;
         else
            Result := Int_Part / Scale_Fact +
                      Frac_Part / (Frac_Scale * Scale_Fact);
         end if;

         if Negative then
            Result := -Result;
         end if;

         return Result;
      end;
   end Scan_Decimal;

   -------------------
   -- Value_Decimal --
   -------------------

   function Value_Decimal
     (Str   : String;
      Scale : Integer) return Long_Long_Integer
   is
      Pos    : aliased Integer := Str'First;
      Result : Long_Long_Integer;
   begin
      Result := Scan_Decimal (Str, Pos'Access, Str'Last, Scale);

      -- Skip trailing spaces
      while Pos <= Str'Last and Str (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      if Pos <= Str'Last then
         raise Constraint_Error;
      end if;

      return Result;
   end Value_Decimal;

end System.Val_Decimal;
