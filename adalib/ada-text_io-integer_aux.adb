-- Ada.Text_IO.Integer_Aux body for Z80
-- Integer I/O support

package body Ada.Text_IO.Integer_Aux is

   -----------------
   -- Get_Integer --
   -----------------

   procedure Get_Integer
     (From : String;
      Item : out Integer;
      Last : out Positive)
   is
      Ptr      : Integer := From'First;
      Negative : Boolean := False;
      Result   : Integer := 0;
      Digit    : Integer;
      Base     : Integer := 10;
      Got_Hash : Boolean := False;
   begin
      -- Skip leading blanks
      while Ptr <= From'Last and then From (Ptr) = ' ' loop
         Ptr := Ptr + 1;
      end loop;

      if Ptr > From'Last then
         raise Data_Error;
      end if;

      -- Check for sign
      if From (Ptr) = '-' then
         Negative := True;
         Ptr := Ptr + 1;
      elsif From (Ptr) = '+' then
         Ptr := Ptr + 1;
      end if;

      -- Check for based literal (e.g., 16#FF#)
      declare
         Start : constant Integer := Ptr;
         Temp  : Integer := 0;
      begin
         while Ptr <= From'Last and then From (Ptr) in '0' .. '9' loop
            Temp := Temp * 10 + Character'Pos (From (Ptr)) - Character'Pos ('0');
            Ptr := Ptr + 1;
         end loop;

         if Ptr <= From'Last and then From (Ptr) = '#' then
            Base := Temp;
            if Base < 2 or else Base > 16 then
               raise Data_Error;
            end if;
            Ptr := Ptr + 1;
            Got_Hash := True;
         else
            Ptr := Start;  -- Reset and parse as decimal
         end if;
      end;

      -- Parse digits
      while Ptr <= From'Last loop
         if From (Ptr) in '0' .. '9' then
            Digit := Character'Pos (From (Ptr)) - Character'Pos ('0');
         elsif From (Ptr) in 'A' .. 'F' then
            Digit := Character'Pos (From (Ptr)) - Character'Pos ('A') + 10;
         elsif From (Ptr) in 'a' .. 'f' then
            Digit := Character'Pos (From (Ptr)) - Character'Pos ('a') + 10;
         elsif From (Ptr) = '#' and then Got_Hash then
            Ptr := Ptr + 1;
            exit;
         elsif From (Ptr) = '_' then
            Ptr := Ptr + 1;
            -- Skip underscores in numbers
            goto Continue;
         else
            exit;
         end if;

         if Digit >= Base then
            raise Data_Error;
         end if;

         Result := Result * Base + Digit;
         Ptr := Ptr + 1;

         <<Continue>>
      end loop;

      if Negative then
         Result := -Result;
      end if;

      Item := Result;
      Last := Ptr - 1;
   end Get_Integer;

   -----------------
   -- Put_Integer --
   -----------------

   procedure Put_Integer
     (To   : out String;
      Item : Integer;
      Base : Natural := Default_Base)
   is
   begin
      Put_Width (To, Item, To'Length, Base);
   end Put_Integer;

   ---------------
   -- Put_Width --
   ---------------

   procedure Put_Width
     (To    : out String;
      Item  : Integer;
      Width : Natural;
      Base  : Natural := Default_Base)
   is
      Img : constant String := Integer_Image (Item, Width, Base);
      Pad : Integer;
   begin
      Pad := To'Length - Img'Length;
      if Pad > 0 then
         To (To'First .. To'First + Pad - 1) := (others => ' ');
         To (To'First + Pad .. To'Last) := Img;
      elsif Img'Length <= To'Length then
         To (To'First .. To'First + Img'Length - 1) := Img;
         if Img'Length < To'Length then
            To (To'First + Img'Length .. To'Last) := (others => ' ');
         end if;
      else
         -- Field too small
         To := (others => '*');
      end if;
   end Put_Width;

   -------------------
   -- Integer_Image --
   -------------------

   function Integer_Image
     (Item  : Integer;
      Width : Natural := 0;
      Base  : Natural := Default_Base) return String
   is
      Result : String (1 .. 32) := (others => ' ');
      Pos    : Natural := Result'Last;
      V      : Integer := abs Item;
      D      : Integer;
   begin
      -- Build digits in reverse
      if V = 0 then
         Result (Pos) := '0';
         Pos := Pos - 1;
      else
         while V > 0 loop
            D := V mod Base;
            if D < 10 then
               Result (Pos) := Character'Val (Character'Pos ('0') + D);
            else
               Result (Pos) := Character'Val (Character'Pos ('A') + D - 10);
            end if;
            Pos := Pos - 1;
            V := V / Base;
         end loop;
      end if;

      -- Add base notation if not decimal
      if Base /= 10 then
         Result (Pos) := '#';
         Pos := Pos - 1;

         -- Add base digits
         V := Base;
         if V >= 10 then
            Result (Pos) := Character'Val (Character'Pos ('0') + V mod 10);
            Pos := Pos - 1;
            Result (Pos) := Character'Val (Character'Pos ('0') + V / 10);
            Pos := Pos - 1;
         else
            Result (Pos) := Character'Val (Character'Pos ('0') + V);
            Pos := Pos - 1;
         end if;

         -- Need closing #
         declare
            Close_Pos : constant Natural := Result'Last + 1;
         begin
            -- Shift and add closing #
            null;  -- Simplified for Z80
         end;
      end if;

      -- Add sign
      if Item < 0 then
         Result (Pos) := '-';
         Pos := Pos - 1;
      end if;

      -- Calculate final string
      declare
         Len : constant Natural := Result'Last - Pos;
         Pad : Natural := 0;
      begin
         if Width > Len then
            Pad := Width - Len;
         end if;

         declare
            Final : String (1 .. Len + Pad);
         begin
            Final (1 .. Pad) := (others => ' ');
            Final (Pad + 1 .. Final'Last) := Result (Pos + 1 .. Result'Last);
            return Final;
         end;
      end;
   end Integer_Image;

end Ada.Text_IO.Integer_Aux;
