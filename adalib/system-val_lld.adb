-- System.Val_LLD body for Z80
-- Value conversion for Long_Long_Integer

package body System.Val_LLD is

   --------------------------
   -- Scan_Long_Long_Integer --
   --------------------------

   procedure Scan_Long_Long_Integer
     (Str  : String;
      Ptr  : in Out Integer;
      Max  : Integer;
      Res  : out Long_Long_Integer)
   is
      Start    : Integer;
      Negative : Boolean := False;
      Digit    : Integer;
      Result   : Long_Long_Integer := 0;
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

      if Ptr > Max then
         raise Constraint_Error;
      end if;

      Start := Ptr;

      -- Scan digits
      while Ptr <= Max loop
         exit when Str (Ptr) < '0' or else Str (Ptr) > '9';

         Digit := Character'Pos (Str (Ptr)) - Character'Pos ('0');

         -- Check for overflow
         if Result > (Long_Long_Integer'Last - Long_Long_Integer (Digit)) / 10 then
            raise Constraint_Error;
         end if;

         Result := Result * 10 + Long_Long_Integer (Digit);
         Ptr := Ptr + 1;
      end loop;

      -- Must have at least one digit
      if Ptr = Start then
         raise Constraint_Error;
      end if;

      if Negative then
         Res := -Result;
      else
         Res := Result;
      end if;
   end Scan_Long_Long_Integer;

   -----------------------------
   -- Value_Long_Long_Integer --
   -----------------------------

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer is
      Ptr : Integer := Str'First;
      Res : Long_Long_Integer;
   begin
      Scan_Long_Long_Integer (Str, Ptr, Str'Last, Res);

      -- Skip trailing blanks
      while Ptr <= Str'Last loop
         if Str (Ptr) /= ' ' then
            raise Constraint_Error;
         end if;
         Ptr := Ptr + 1;
      end loop;

      return Res;
   end Value_Long_Long_Integer;

end System.Val_LLD;
