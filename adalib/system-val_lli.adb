-- System.Val_LLI body for Z80
-- Long_Long_Integer value conversion from strings implementation

package body System.Val_LLI is

   ----------------------------
   -- Scan_Long_Long_Integer --
   ----------------------------

   function Scan_Long_Long_Integer
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Integer
   is
      P        : Integer := Ptr.all;
      Result   : Long_Long_Integer := 0;
      Negative : Boolean := False;
      Digit    : Integer;
      Base     : Long_Long_Integer := 10;
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

      -- First pass: check for base indicator
      declare
         Temp_Result : Long_Long_Integer := 0;
         Start       : constant Integer := P;
      begin
         while P <= Max and then P <= Str'Last and then Str (P) in '0' .. '9' loop
            Temp_Result := Temp_Result * 10 +
              Long_Long_Integer (Character'Pos (Str (P)) - Character'Pos ('0'));
            P := P + 1;
         end loop;

         if P <= Max and then P <= Str'Last and then Str (P) = '#' then
            Base := Temp_Result;
            P := P + 1;
         else
            -- Not a based literal, use result directly
            Result := Temp_Result;
            goto Done;
         end if;
      end;

      -- Read based number
      while P <= Max and then P <= Str'Last and then Str (P) /= '#' loop
         if Str (P) in '0' .. '9' then
            Digit := Character'Pos (Str (P)) - Character'Pos ('0');
         elsif Str (P) in 'A' .. 'F' then
            Digit := Character'Pos (Str (P)) - Character'Pos ('A') + 10;
         elsif Str (P) in 'a' .. 'f' then
            Digit := Character'Pos (Str (P)) - Character'Pos ('a') + 10;
         elsif Str (P) = '_' then
            P := P + 1;
            goto Skip;
         else
            exit;
         end if;

         if Long_Long_Integer (Digit) >= Base then
            raise Constraint_Error;
         end if;

         Result := Result * Base + Long_Long_Integer (Digit);
         P := P + 1;
         <<Skip>>
      end loop;

      -- Skip closing #
      if P <= Max and then P <= Str'Last and then Str (P) = '#' then
         P := P + 1;
      end if;

      <<Done>>
      Ptr.all := P;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end Scan_Long_Long_Integer;

   -----------------------------
   -- Value_Long_Long_Integer --
   -----------------------------

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer is
      Ptr : aliased Integer := Str'First;
   begin
      return Scan_Long_Long_Integer (Str, Ptr'Access, Str'Last);
   end Value_Long_Long_Integer;

end System.Val_LLI;
