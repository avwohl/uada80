-- System.Val_Uns body for Z80
-- Unsigned integer value conversion from strings implementation

package body System.Val_Uns is

   use Interfaces;

   -------------------
   -- Scan_Unsigned --
   -------------------

   function Scan_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Unsigned_32
   is
      P      : Integer := Ptr.all;
      Result : Unsigned_32 := 0;
      Digit  : Unsigned_32;
      Base   : Unsigned_32 := 10;
   begin
      -- Skip leading whitespace
      while P <= Max and then P <= Str'Last and then Str (P) = ' ' loop
         P := P + 1;
      end loop;

      -- Skip optional plus sign
      if P <= Max and then P <= Str'Last and then Str (P) = '+' then
         P := P + 1;
      end if;

      -- First pass: check for base indicator
      declare
         Temp_Result : Unsigned_32 := 0;
      begin
         while P <= Max and then P <= Str'Last and then Str (P) in '0' .. '9' loop
            Temp_Result := Temp_Result * 10 +
              Unsigned_32 (Character'Pos (Str (P)) - Character'Pos ('0'));
            P := P + 1;
         end loop;

         if P <= Max and then P <= Str'Last and then Str (P) = '#' then
            Base := Temp_Result;
            P := P + 1;
         else
            Result := Temp_Result;
            goto Done;
         end if;
      end;

      -- Read based number
      while P <= Max and then P <= Str'Last and then Str (P) /= '#' loop
         if Str (P) in '0' .. '9' then
            Digit := Unsigned_32 (Character'Pos (Str (P)) - Character'Pos ('0'));
         elsif Str (P) in 'A' .. 'F' then
            Digit := Unsigned_32 (Character'Pos (Str (P)) - Character'Pos ('A') + 10);
         elsif Str (P) in 'a' .. 'f' then
            Digit := Unsigned_32 (Character'Pos (Str (P)) - Character'Pos ('a') + 10);
         elsif Str (P) = '_' then
            P := P + 1;
            goto Skip;
         else
            exit;
         end if;

         if Digit >= Base then
            raise Constraint_Error;
         end if;

         Result := Result * Base + Digit;
         P := P + 1;
         <<Skip>>
      end loop;

      -- Skip closing #
      if P <= Max and then P <= Str'Last and then Str (P) = '#' then
         P := P + 1;
      end if;

      <<Done>>
      Ptr.all := P;
      return Result;
   end Scan_Unsigned;

   --------------------
   -- Value_Unsigned --
   --------------------

   function Value_Unsigned (Str : String) return Unsigned_32 is
      Ptr : aliased Integer := Str'First;
   begin
      return Scan_Unsigned (Str, Ptr'Access, Str'Last);
   end Value_Unsigned;

end System.Val_Uns;
