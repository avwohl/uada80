-- Ada.Long_Long_Float_Text_IO body for Z80
-- Text I/O for Long_Long_Float type implementation

package body Ada.Long_Long_Float_Text_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Long_Long_Float;
      Width : Ada.Text_IO.Field := 0)
   is
      pragma Unreferenced (File, Width);
   begin
      -- Simplified stub
      Item := 0.0;
   end Get;

   procedure Get
     (Item  : out Long_Long_Float;
      Width : Ada.Text_IO.Field := 0)
   is
   begin
      Get (Ada.Text_IO.Current_Input, Item, Width);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : Long_Long_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
      pragma Unreferenced (File);
      Buffer : String (1 .. 30);
   begin
      Put (Buffer, Item, Aft, Exp);
      -- Output with padding
      declare
         First : Natural := Buffer'First;
      begin
         while First <= Buffer'Last and then Buffer (First) = ' ' loop
            First := First + 1;
         end loop;
         if First > Buffer'Last then
            First := Buffer'Last;
         end if;
         declare
            Str_Len : constant Natural := Buffer'Last - First + 1;
            Total   : constant Natural := Fore + 1 + Aft + Exp;
         begin
            if Total > Str_Len then
               for I in 1 .. Total - Str_Len loop
                  Ada.Text_IO.Put (' ');
               end loop;
            end if;
            Ada.Text_IO.Put (Buffer (First .. Buffer'Last));
         end;
      end;
   end Put;

   procedure Put
     (Item : Long_Long_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
   begin
      Put (Ada.Text_IO.Current_Output, Item, Fore, Aft, Exp);
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (From : String;
      Item : out Long_Long_Float;
      Last : out Positive)
   is
      Value       : Long_Long_Float := 0.0;
      Frac_Value  : Long_Long_Float := 0.0;
      Exp_Value   : Integer := 0;
      Negative    : Boolean := False;
      Neg_Exp     : Boolean := False;
      First       : Positive := From'First;
      In_Fraction : Boolean := False;
      In_Exponent : Boolean := False;
      Frac_Div    : Long_Long_Float := 1.0;
      Digit       : Integer;
   begin
      -- Skip leading spaces
      while First <= From'Last and then From (First) = ' ' loop
         First := First + 1;
      end loop;

      if First > From'Last then
         raise Ada.Text_IO.Data_Error;
      end if;

      -- Check for sign
      if From (First) = '-' then
         Negative := True;
         First := First + 1;
      elsif From (First) = '+' then
         First := First + 1;
      end if;

      Last := First;
      while Last <= From'Last loop
         if From (Last) in '0' .. '9' then
            Digit := Character'Pos (From (Last)) - Character'Pos ('0');
            if In_Exponent then
               Exp_Value := Exp_Value * 10 + Digit;
            elsif In_Fraction then
               Frac_Div := Frac_Div * 10.0;
               Frac_Value := Frac_Value + Long_Long_Float (Digit) / Frac_Div;
            else
               Value := Value * 10.0 + Long_Long_Float (Digit);
            end if;
         elsif From (Last) = '.' and not In_Fraction and not In_Exponent then
            In_Fraction := True;
         elsif (From (Last) = 'E' or From (Last) = 'e') and not In_Exponent then
            In_Exponent := True;
            In_Fraction := False;
            if Last < From'Last then
               if From (Last + 1) = '-' then
                  Neg_Exp := True;
                  Last := Last + 1;
               elsif From (Last + 1) = '+' then
                  Last := Last + 1;
               end if;
            end if;
         else
            exit;
         end if;
         Last := Last + 1;
      end loop;

      Last := Last - 1;

      -- Combine mantissa and fractional parts
      Item := Value + Frac_Value;

      -- Apply exponent
      if In_Exponent then
         if Neg_Exp then
            Exp_Value := -Exp_Value;
         end if;
         for I in 1 .. abs Exp_Value loop
            if Exp_Value > 0 then
               Item := Item * 10.0;
            else
               Item := Item / 10.0;
            end if;
         end loop;
      end if;

      if Negative then
         Item := -Item;
      end if;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Long_Long_Float;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
      Value     : Long_Long_Float := abs Item;
      Exponent  : Integer := 0;
      Pos       : Natural := To'First;
      Int_Part  : Long_Long_Float;
      Frac_Part : Long_Long_Float;
      Digit     : Integer;
   begin
      To := (others => ' ');

      -- Handle sign
      if Item < 0.0 then
         To (Pos) := '-';
         Pos := Pos + 1;
      end if;

      -- Normalize to scientific notation if needed
      if Exp > 0 then
         if Value /= 0.0 then
            while Value >= 10.0 loop
               Value := Value / 10.0;
               Exponent := Exponent + 1;
            end loop;
            while Value < 1.0 loop
               Value := Value * 10.0;
               Exponent := Exponent - 1;
            end loop;
         end if;
      end if;

      -- Output integer part
      Int_Part := Long_Long_Float'Truncation (Value);
      Digit := Integer (Int_Part);
      if Digit > 9 then Digit := 9; end if;
      To (Pos) := Character'Val (Character'Pos ('0') + Digit);
      Pos := Pos + 1;

      -- Output decimal point
      if Pos <= To'Last then
         To (Pos) := '.';
         Pos := Pos + 1;
      end if;

      -- Output fractional part
      Frac_Part := Value - Int_Part;
      for I in 1 .. Aft loop
         exit when Pos > To'Last;
         Frac_Part := Frac_Part * 10.0;
         Digit := Integer (Long_Long_Float'Truncation (Frac_Part));
         if Digit > 9 then Digit := 9; end if;
         if Digit < 0 then Digit := 0; end if;
         To (Pos) := Character'Val (Character'Pos ('0') + Digit);
         Frac_Part := Frac_Part - Long_Long_Float (Digit);
         Pos := Pos + 1;
      end loop;

      -- Output exponent if needed
      if Exp > 0 and Pos <= To'Last then
         To (Pos) := 'E';
         Pos := Pos + 1;
         if Pos <= To'Last then
            if Exponent >= 0 then
               To (Pos) := '+';
            else
               To (Pos) := '-';
               Exponent := abs Exponent;
            end if;
            Pos := Pos + 1;
         end if;
         -- Output exponent digits
         declare
            Exp_Str : String (1 .. Exp) := (others => '0');
            E_Pos   : Natural := Exp;
         begin
            while Exponent > 0 and E_Pos >= 1 loop
               Exp_Str (E_Pos) := Character'Val (Character'Pos ('0') + Exponent mod 10);
               Exponent := Exponent / 10;
               E_Pos := E_Pos - 1;
            end loop;
            for I in Exp_Str'Range loop
               exit when Pos > To'Last;
               To (Pos) := Exp_Str (I);
               Pos := Pos + 1;
            end loop;
         end;
      end if;
   end Put;

end Ada.Long_Long_Float_Text_IO;
