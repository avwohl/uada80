-- System.Val_Generic body for Z80
-- Generic value conversion utilities

package body System.Val_Generic is

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer;
      Res : out T)
   is
      Start    : Integer;
      Negative : Boolean := False;
      Digit    : Integer;
      Result   : T := 0;
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
         if Result > (T'Last - T (Digit)) / 10 then
            raise Constraint_Error;
         end if;

         Result := Result * 10 + T (Digit);
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
   end Scan;

   -----------
   -- Value --
   -----------

   function Value (Str : String) return T is
      Ptr : Integer := Str'First;
      Res : T;
   begin
      Scan (Str, Ptr, Str'Last, Res);

      -- Skip trailing blanks
      while Ptr <= Str'Last loop
         if Str (Ptr) /= ' ' then
            raise Constraint_Error;
         end if;
         Ptr := Ptr + 1;
      end loop;

      return Res;
   end Value;

   -------------------
   -- Checked_Value --
   -------------------

   function Checked_Value (Str : String; Lo, Hi : T) return T is
      Result : constant T := Value (Str);
   begin
      if Result < Lo or else Result > Hi then
         raise Constraint_Error;
      end if;
      return Result;
   end Checked_Value;

end System.Val_Generic;
