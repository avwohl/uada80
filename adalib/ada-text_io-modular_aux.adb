-- Ada.Text_IO.Modular_Aux body for Z80
-- Modular type I/O support

package body Ada.Text_IO.Modular_Aux is

   ------------------
   -- Get_Unsigned --
   ------------------

   procedure Get_Unsigned
     (From : String;
      Item : out Unsigned;
      Last : out Positive)
   is
      Ptr    : Integer := From'First;
      Result : Unsigned := 0;
      Digit  : Natural;
      Base   : Natural := 10;
      Got_Hash : Boolean := False;
   begin
      -- Skip leading blanks
      while Ptr <= From'Last and then From (Ptr) = ' ' loop
         Ptr := Ptr + 1;
      end loop;

      if Ptr > From'Last then
         raise Data_Error;
      end if;

      -- Check for based literal
      declare
         Start : constant Integer := Ptr;
         Temp  : Natural := 0;
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
            Ptr := Start;
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
            goto Continue;
         else
            exit;
         end if;

         if Digit >= Base then
            raise Data_Error;
         end if;

         Result := Result * Unsigned (Base) + Unsigned (Digit);
         Ptr := Ptr + 1;

         <<Continue>>
      end loop;

      Item := Result;
      Last := Ptr - 1;
   end Get_Unsigned;

   ------------------
   -- Put_Unsigned --
   ------------------

   procedure Put_Unsigned
     (To   : out String;
      Item : Unsigned;
      Base : Natural := Default_Base)
   is
      Img : constant String := Unsigned_Image (Item, To'Length, Base);
      Pad : Integer;
   begin
      Pad := To'Length - Img'Length;
      if Pad > 0 then
         To (To'First .. To'First + Pad - 1) := (others => ' ');
         To (To'First + Pad .. To'Last) := Img;
      elsif Img'Length <= To'Length then
         To := Img (Img'First .. Img'First + To'Length - 1);
      else
         To := (others => '*');
      end if;
   end Put_Unsigned;

   --------------------
   -- Unsigned_Image --
   --------------------

   function Unsigned_Image
     (Item  : Unsigned;
      Width : Natural := 0;
      Base  : Natural := Default_Base) return String
   is
      Result : String (1 .. 16) := (others => ' ');
      Pos    : Natural := Result'Last;
      V      : Unsigned := Item;
      D      : Natural;
   begin
      -- Build digits in reverse
      if V = 0 then
         Result (Pos) := '0';
         Pos := Pos - 1;
      else
         while V > 0 loop
            D := Natural (V mod Unsigned (Base));
            if D < 10 then
               Result (Pos) := Character'Val (Character'Pos ('0') + D);
            else
               Result (Pos) := Character'Val (Character'Pos ('A') + D - 10);
            end if;
            Pos := Pos - 1;
            V := V / Unsigned (Base);
         end loop;
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
   end Unsigned_Image;

end Ada.Text_IO.Modular_Aux;
