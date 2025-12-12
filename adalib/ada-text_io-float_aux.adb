-- Ada.Text_IO.Float_Aux body for Z80
-- Float I/O support

package body Ada.Text_IO.Float_Aux is

   ---------------
   -- Get_Float --
   ---------------

   procedure Get_Float
     (From : String;
      Item : out Float;
      Last : out Positive)
   is
      Ptr      : Integer := From'First;
      Negative : Boolean := False;
      Result   : Float := 0.0;
      Frac     : Float;
      Exp      : Integer := 0;
      Digit    : Integer;
      Got_Dot  : Boolean := False;
      Frac_Div : Float := 1.0;
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

      -- Parse integer and fractional parts
      while Ptr <= From'Last loop
         if From (Ptr) = '.' then
            if Got_Dot then
               exit;
            end if;
            Got_Dot := True;
            Ptr := Ptr + 1;
         elsif From (Ptr) in '0' .. '9' then
            Digit := Character'Pos (From (Ptr)) - Character'Pos ('0');
            if Got_Dot then
               Frac_Div := Frac_Div * 10.0;
               Result := Result + Float (Digit) / Frac_Div;
            else
               Result := Result * 10.0 + Float (Digit);
            end if;
            Ptr := Ptr + 1;
         elsif From (Ptr) = 'E' or else From (Ptr) = 'e' then
            Ptr := Ptr + 1;
            -- Parse exponent
            declare
               Exp_Neg : Boolean := False;
            begin
               if Ptr <= From'Last and then From (Ptr) = '-' then
                  Exp_Neg := True;
                  Ptr := Ptr + 1;
               elsif Ptr <= From'Last and then From (Ptr) = '+' then
                  Ptr := Ptr + 1;
               end if;

               while Ptr <= From'Last and then From (Ptr) in '0' .. '9' loop
                  Exp := Exp * 10 + Character'Pos (From (Ptr)) - Character'Pos ('0');
                  Ptr := Ptr + 1;
               end loop;

               if Exp_Neg then
                  Exp := -Exp;
               end if;
            end;
            exit;
         else
            exit;
         end if;
      end loop;

      -- Apply exponent
      if Exp > 0 then
         for I in 1 .. Exp loop
            Result := Result * 10.0;
         end loop;
      elsif Exp < 0 then
         for I in 1 .. (-Exp) loop
            Result := Result / 10.0;
         end loop;
      end if;

      if Negative then
         Result := -Result;
      end if;

      Item := Result;
      Last := Ptr - 1;
   end Get_Float;

   ---------------
   -- Put_Float --
   ---------------

   procedure Put_Float
     (To   : out String;
      Item : Float;
      Aft  : Natural := Default_Aft;
      Exp  : Natural := Default_Exp)
   is
      Img : constant String := Float_Image (Item, 1, Aft, Exp);
      Pad : Integer;
   begin
      Pad := To'Length - Img'Length;
      if Pad > 0 then
         To (To'First .. To'First + Pad - 1) := (others => ' ');
         To (To'First + Pad .. To'Last) := Img;
      elsif Img'Length <= To'Length then
         To := Img;
      else
         -- Field too small
         To := (others => '*');
      end if;
   end Put_Float;

   -----------------
   -- Float_Image --
   -----------------

   function Float_Image
     (Item : Float;
      Fore : Natural := Default_Fore;
      Aft  : Natural := Default_Aft;
      Exp  : Natural := Default_Exp) return String
   is
      Result : String (1 .. 24) := (others => ' ');
      Pos    : Natural := 1;
      V      : Float := abs Item;
      E      : Integer := 0;
      D      : Integer;
      Frac   : Float;
   begin
      -- Handle sign
      if Item < 0.0 then
         Result (Pos) := '-';
         Pos := Pos + 1;
      end if;

      -- Normalize to scientific notation if Exp > 0
      if Exp > 0 and then V /= 0.0 then
         while V >= 10.0 loop
            V := V / 10.0;
            E := E + 1;
         end loop;
         while V < 1.0 loop
            V := V * 10.0;
            E := E - 1;
         end loop;
      end if;

      -- Integer part
      D := Integer (Float'Truncation (V));
      if D < 10 then
         Result (Pos) := Character'Val (Character'Pos ('0') + D);
         Pos := Pos + 1;
      else
         -- Multi-digit integer
         declare
            Buf : String (1 .. 10);
            Len : Natural := 0;
            Tmp : Integer := D;
         begin
            while Tmp > 0 loop
               Len := Len + 1;
               Buf (Len) := Character'Val (Character'Pos ('0') + Tmp mod 10);
               Tmp := Tmp / 10;
            end loop;
            for I in reverse 1 .. Len loop
               Result (Pos) := Buf (I);
               Pos := Pos + 1;
            end loop;
         end;
      end if;

      -- Decimal point and fractional part
      Result (Pos) := '.';
      Pos := Pos + 1;

      Frac := V - Float'Truncation (V);
      for I in 1 .. Aft loop
         Frac := Frac * 10.0;
         D := Integer (Float'Truncation (Frac));
         Result (Pos) := Character'Val (Character'Pos ('0') + D mod 10);
         Pos := Pos + 1;
         Frac := Frac - Float'Truncation (Frac);
      end loop;

      -- Exponent
      if Exp > 0 then
         Result (Pos) := 'E';
         Pos := Pos + 1;
         if E >= 0 then
            Result (Pos) := '+';
         else
            Result (Pos) := '-';
            E := -E;
         end if;
         Pos := Pos + 1;

         -- Exponent digits
         declare
            Buf : String (1 .. 4);
            Len : Natural := 0;
         begin
            if E = 0 then
               Buf (1) := '0';
               Len := 1;
            else
               while E > 0 loop
                  Len := Len + 1;
                  Buf (Len) := Character'Val (Character'Pos ('0') + E mod 10);
                  E := E / 10;
               end loop;
            end if;

            -- Pad exponent to Exp-1 digits
            while Len < Exp - 1 loop
               Result (Pos) := '0';
               Pos := Pos + 1;
               Len := Len + 1;
            end loop;

            for I in reverse 1 .. Len loop
               Result (Pos) := Buf (I);
               Pos := Pos + 1;
            end loop;
         end;
      end if;

      return Result (1 .. Pos - 1);
   end Float_Image;

end Ada.Text_IO.Float_Aux;
