-- System.Val_LLU body for Z80
-- Long_Long_Unsigned value conversion implementation

package body System.Val_LLU is

   ------------------------------
   -- Scan_Long_Long_Unsigned --
   ------------------------------

   function Scan_Long_Long_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Unsigned
   is
      Pos    : Integer := Ptr.all;
      Result : Long_Long_Unsigned := 0;
      Base   : Long_Long_Unsigned := 10;
      Digit  : Natural;
      C      : Character;
      Based  : Boolean := False;
   begin
      -- Skip leading spaces
      while Pos <= Max and Str (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      if Pos > Max then
         raise Constraint_Error;
      end if;

      -- Parse initial digits
      while Pos <= Max loop
         C := Str (Pos);

         if C in '0' .. '9' then
            Digit := Character'Pos (C) - Character'Pos ('0');
         elsif C = '_' then
            Pos := Pos + 1;
            goto Continue;
         elsif C = '#' and not Based then
            -- Based literal
            Base := Result;
            if Base < 2 or Base > 16 then
               raise Constraint_Error;
            end if;
            Result := 0;
            Based := True;
            Pos := Pos + 1;
            goto Continue;
         elsif Based then
            if C in 'A' .. 'F' then
               Digit := Character'Pos (C) - Character'Pos ('A') + 10;
            elsif C in 'a' .. 'f' then
               Digit := Character'Pos (C) - Character'Pos ('a') + 10;
            elsif C = '#' then
               Pos := Pos + 1;
               exit;
            else
               exit;
            end if;
         else
            exit;
         end if;

         if Long_Long_Unsigned (Digit) >= Base then
            raise Constraint_Error;
         end if;

         Result := Result * Base + Long_Long_Unsigned (Digit);
         Pos := Pos + 1;

         <<Continue>>
      end loop;

      Ptr.all := Pos;
      return Result;
   end Scan_Long_Long_Unsigned;

   -------------------------------
   -- Value_Long_Long_Unsigned --
   -------------------------------

   function Value_Long_Long_Unsigned (Str : String) return Long_Long_Unsigned is
      Pos : aliased Integer := Str'First;
      Result : Long_Long_Unsigned;
   begin
      Result := Scan_Long_Long_Unsigned (Str, Pos'Access, Str'Last);

      -- Skip trailing spaces
      while Pos <= Str'Last and Str (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      if Pos <= Str'Last then
         raise Constraint_Error;
      end if;

      return Result;
   end Value_Long_Long_Unsigned;

end System.Val_LLU;
