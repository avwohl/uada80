-- System.Val_Fixed body for Z80
-- Fixed point value conversion implementation

package body System.Val_Fixed is

   ----------------
   -- Scan_Fixed --
   ----------------

   function Scan_Fixed
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Num   : Long_Long_Integer;
      Den   : Long_Long_Integer) return Long_Long_Integer
   is
      Pos      : Integer := Ptr.all;
      Negative : Boolean := False;
      Int_Part : Long_Long_Integer := 0;
      Frac_Part : Long_Long_Integer := 0;
      Frac_Digits : Natural := 0;
      C        : Character;
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
            Int_Part := Int_Part * 10 + Long_Long_Integer (Character'Pos (C) - Character'Pos ('0'));
            Pos := Pos + 1;
         elsif C = '_' then
            Pos := Pos + 1;  -- Skip underscores
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
               Frac_Part := Frac_Part * 10 + Long_Long_Integer (Character'Pos (C) - Character'Pos ('0'));
               Frac_Digits := Frac_Digits + 1;
               Pos := Pos + 1;
            elsif C = '_' then
               Pos := Pos + 1;
            else
               exit;
            end if;
         end loop;
      end if;

      Ptr.all := Pos;

      -- Convert to scaled representation
      declare
         Scale_Factor : Long_Long_Integer := 1;
         Result       : Long_Long_Integer;
      begin
         -- Compute 10^Frac_Digits
         for I in 1 .. Frac_Digits loop
            Scale_Factor := Scale_Factor * 10;
         end loop;

         -- Result = (Int_Part * Scale_Factor + Frac_Part) * Den / (Num * Scale_Factor)
         Result := (Int_Part * Scale_Factor + Frac_Part) * Den / (Num * Scale_Factor);

         if Negative then
            Result := -Result;
         end if;

         return Result;
      end;
   end Scan_Fixed;

   -----------------
   -- Value_Fixed --
   -----------------

   function Value_Fixed
     (Str : String;
      Num : Long_Long_Integer;
      Den : Long_Long_Integer) return Long_Long_Integer
   is
      Pos : aliased Integer := Str'First;
      Result : Long_Long_Integer;
   begin
      Result := Scan_Fixed (Str, Pos'Access, Str'Last, Num, Den);

      -- Skip trailing spaces
      while Pos <= Str'Last and Str (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      -- Check that entire string was consumed
      if Pos <= Str'Last then
         raise Constraint_Error;
      end if;

      return Result;
   end Value_Fixed;

end System.Val_Fixed;
