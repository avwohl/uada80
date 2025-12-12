-- System.Val_Int body for Z80
-- Integer value conversion from strings implementation

package body System.Val_Int is

   ------------------
   -- Scan_Integer --
   ------------------

   function Scan_Integer
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Integer
   is
      P        : Integer := Ptr.all;
      Result   : Integer := 0;
      Negative : Boolean := False;
      Digit    : Integer;
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

      -- Check for base indicator (16#...# or 2#...# etc)
      declare
         Base : Integer := 10;
         Start : constant Integer := P;
      begin
         -- First, try to read a potential base
         while P <= Max and then P <= Str'Last and then Str (P) in '0' .. '9' loop
            Result := Result * 10 + (Character'Pos (Str (P)) - Character'Pos ('0'));
            P := P + 1;
         end loop;

         -- Check if this was actually a base specifier
         if P <= Max and then P <= Str'Last and then Str (P) = '#' then
            Base := Result;
            Result := 0;
            P := P + 1;

            -- Now read the actual number in the specified base
            while P <= Max and then P <= Str'Last and then Str (P) /= '#' loop
               if Str (P) in '0' .. '9' then
                  Digit := Character'Pos (Str (P)) - Character'Pos ('0');
               elsif Str (P) in 'A' .. 'F' then
                  Digit := Character'Pos (Str (P)) - Character'Pos ('A') + 10;
               elsif Str (P) in 'a' .. 'f' then
                  Digit := Character'Pos (Str (P)) - Character'Pos ('a') + 10;
               elsif Str (P) = '_' then
                  P := P + 1;
                  goto Continue;
               else
                  exit;
               end if;

               if Digit >= Base then
                  raise Constraint_Error;
               end if;

               Result := Result * Base + Digit;
               P := P + 1;
               <<Continue>>
            end loop;

            -- Skip closing #
            if P <= Max and then P <= Str'Last and then Str (P) = '#' then
               P := P + 1;
            end if;
         end if;
      end;

      Ptr.all := P;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end Scan_Integer;

   -------------------
   -- Value_Integer --
   -------------------

   function Value_Integer (Str : String) return Integer is
      Ptr : aliased Integer := Str'First;
   begin
      return Scan_Integer (Str, Ptr'Access, Str'Last);
   end Value_Integer;

end System.Val_Int;
