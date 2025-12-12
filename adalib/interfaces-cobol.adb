-- Interfaces.COBOL body for Z80
-- Types for interfacing with COBOL implementation

package body Interfaces.COBOL is

   --------------
   -- To_COBOL --
   --------------

   function To_COBOL (Item : Character) return COBOL_Character is
   begin
      return COBOL_Character (Item);
   end To_COBOL;

   function To_COBOL (Item : String) return Alphanumeric is
      Result : Alphanumeric (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := COBOL_Character (Item (I));
      end loop;
      return Result;
   end To_COBOL;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Item : COBOL_Character) return Character is
   begin
      return Character (Item);
   end To_Ada;

   function To_Ada (Item : Alphanumeric) return String is
      Result : String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := Character (Item (I));
      end loop;
      return Result;
   end To_Ada;

   --------------
   -- To_COBOL --
   --------------

   procedure To_COBOL
     (Item   : String;
      Target : out Alphanumeric;
      Last   : out Natural)
   is
      Copy_Length : constant Natural :=
        Natural'Min (Item'Length, Target'Length);
   begin
      for I in 0 .. Copy_Length - 1 loop
         Target (Target'First + I) := COBOL_Character (Item (Item'First + I));
      end loop;

      -- Pad with spaces
      if Copy_Length < Target'Length then
         for I in Target'First + Copy_Length .. Target'Last loop
            Target (I) := COBOL_Character (' ');
         end loop;
      end if;

      Last := Target'First + Copy_Length - 1;
   end To_COBOL;

   ------------
   -- To_Ada --
   ------------

   procedure To_Ada
     (Item   : Alphanumeric;
      Target : out String;
      Last   : out Natural)
   is
      Copy_Length : constant Natural :=
        Natural'Min (Item'Length, Target'Length);
   begin
      for I in 0 .. Copy_Length - 1 loop
         Target (Target'First + I) := Character (Item (Item'First + I));
      end loop;

      Last := Target'First + Copy_Length - 1;
   end To_Ada;

   ----------------
   -- To_Display --
   ----------------

   function To_Display
     (Item   : Long_Binary;
      Format : Display_Format) return Numeric
   is
      Result : Numeric (1 .. Format'Length);
      Value  : Long_Binary := abs Item;
      Pos    : Natural := Format'Length;
      Digit  : Long_Binary;
   begin
      -- Initialize with zeros
      for I in Result'Range loop
         Result (I) := COBOL_Character ('0');
      end loop;

      -- Convert to digits (right to left)
      while Value > 0 and Pos >= 1 loop
         Digit := Value mod 10;
         Result (Pos) := COBOL_Character (Character'Val (Character'Pos ('0') + Integer (Digit)));
         Value := Value / 10;
         Pos := Pos - 1;
      end loop;

      -- Handle sign (last digit for signed)
      if Item < 0 and Result'Length > 0 then
         -- Encode sign in zone of last digit (using EBCDIC convention)
         declare
            Last_Digit : constant Integer :=
              Character'Pos (Character (Result (Result'Last))) - Character'Pos ('0');
         begin
            -- Negative: D zone (0xD0-0xD9 in EBCDIC, we use ASCII 'p'-'y')
            Result (Result'Last) := COBOL_Character (Character'Val (Character'Pos ('p') + Last_Digit));
         end;
      end if;

      return Result;
   end To_Display;

   ---------------
   -- To_Binary --
   ---------------

   function To_Binary (Item : Numeric) return Long_Binary is
      Result   : Long_Binary := 0;
      Negative : Boolean := False;
      Digit    : Integer;
      C        : Character;
   begin
      for I in Item'Range loop
         C := Character (Item (I));
         if C in '0' .. '9' then
            Digit := Character'Pos (C) - Character'Pos ('0');
            Result := Result * 10 + Long_Binary (Digit);
         elsif C in 'p' .. 'y' then
            -- Negative sign encoded in last digit
            Digit := Character'Pos (C) - Character'Pos ('p');
            Result := Result * 10 + Long_Binary (Digit);
            Negative := True;
         elsif C in 'A' .. 'I' then
            -- Positive sign encoded (EBCDIC style)
            Digit := Character'Pos (C) - Character'Pos ('A');
            Result := Result * 10 + Long_Binary (Digit);
         elsif C in 'J' .. 'R' then
            -- Negative sign encoded (EBCDIC style)
            Digit := Character'Pos (C) - Character'Pos ('J');
            Result := Result * 10 + Long_Binary (Digit);
            Negative := True;
         elsif C = '-' then
            Negative := True;
         elsif C = '+' or C = ' ' then
            null;  -- Skip
         else
            raise Conversion_Error;
         end if;
      end loop;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end To_Binary;

   ----------------
   -- To_Decimal --
   ----------------

   function To_Decimal
     (Item   : Packed_Decimal;
      Format : Display_Format) return Numeric
   is
      Result    : Numeric (1 .. Format'Length);
      Pos       : Natural := 1;
      High, Low : Byte;
      Negative  : Boolean := False;
   begin
      -- Initialize with zeros
      for I in Result'Range loop
         Result (I) := COBOL_Character ('0');
      end loop;

      -- Unpack digits (each byte has two BCD digits except last has digit+sign)
      for I in Item'Range loop
         High := (Item (I) / 16) and 16#0F#;
         Low  := Item (I) and 16#0F#;

         if I < Item'Last then
            -- Both are digits
            if Pos <= Result'Last then
               Result (Pos) := COBOL_Character (Character'Val (Character'Pos ('0') + Integer (High)));
               Pos := Pos + 1;
            end if;
            if Pos <= Result'Last then
               Result (Pos) := COBOL_Character (Character'Val (Character'Pos ('0') + Integer (Low)));
               Pos := Pos + 1;
            end if;
         else
            -- High is digit, Low is sign
            if Pos <= Result'Last then
               Result (Pos) := COBOL_Character (Character'Val (Character'Pos ('0') + Integer (High)));
            end if;
            -- Sign: C and F are positive, D and B are negative
            Negative := (Low = 16#D# or Low = 16#B#);
         end if;
      end loop;

      -- Encode sign if negative
      if Negative and Result'Length > 0 then
         declare
            Last_Digit : constant Integer :=
              Character'Pos (Character (Result (Result'Last))) - Character'Pos ('0');
         begin
            Result (Result'Last) := COBOL_Character (Character'Val (Character'Pos ('p') + Last_Digit));
         end;
      end if;

      return Result;
   end To_Decimal;

   ---------------
   -- To_Packed --
   ---------------

   function To_Packed
     (Item   : Numeric;
      Length : Natural) return Packed_Decimal
   is
      Result : Packed_Decimal (1 .. Length);
      Value  : Long_Binary;
      Pos    : Natural := Length;
      Sign   : Byte;
   begin
      -- Initialize with zeros
      for I in Result'Range loop
         Result (I) := 0;
      end loop;

      -- Determine sign and get absolute value
      Value := To_Binary (Item);
      if Value < 0 then
         Sign := 16#D#;  -- Negative
         Value := abs Value;
      else
         Sign := 16#C#;  -- Positive
      end if;

      -- Pack last byte: digit + sign
      if Pos >= 1 then
         Result (Pos) := Byte (Value mod 10) * 16 + Sign;
         Value := Value / 10;
         Pos := Pos - 1;
      end if;

      -- Pack remaining bytes: two digits each
      while Pos >= 1 loop
         Result (Pos) := Byte ((Value / 10) mod 10) * 16 + Byte (Value mod 10);
         Value := Value / 100;
         Pos := Pos - 1;
      end loop;

      return Result;
   end To_Packed;

   -----------
   -- Valid --
   -----------

   function Valid (Item : Numeric) return Boolean is
      C : Character;
   begin
      for I in Item'Range loop
         C := Character (Item (I));
         if not (C in '0' .. '9' or C in 'A' .. 'I' or C in 'J' .. 'R'
                 or C in 'p' .. 'y' or C = '+' or C = '-' or C = ' ')
         then
            return False;
         end if;
      end loop;
      return True;
   end Valid;

   function Valid
     (Item   : Packed_Decimal;
      Format : Display_Format) return Boolean
   is
      pragma Unreferenced (Format);
      High, Low : Byte;
   begin
      for I in Item'Range loop
         High := (Item (I) / 16) and 16#0F#;
         Low  := Item (I) and 16#0F#;

         if I < Item'Last then
            -- Both should be valid BCD digits (0-9)
            if High > 9 or Low > 9 then
               return False;
            end if;
         else
            -- High is digit, Low is sign
            if High > 9 then
               return False;
            end if;
            -- Valid signs: A-F (we accept C, D, F, B)
            if Low < 16#A# then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Valid;

end Interfaces.COBOL;
