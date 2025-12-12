-- Ada.Float_Text_IO body for Z80
-- Floating-point text I/O implementation

with Ada.Text_IO;
with System;

package body Ada.Float_Text_IO is

   -- Runtime routines for float conversion
   procedure Rt_Float_To_String
     (Value : Float;
      Buf   : System.Address;
      Aft   : Integer;
      Exp   : Integer);
   pragma Import (C, Rt_Float_To_String, "_float_to_string");

   function Rt_String_To_Float (Buf : System.Address) return Float;
   pragma Import (C, Rt_String_To_Float, "_string_to_float");

   -- Internal buffer for formatting
   Format_Buffer : String (1 .. 32);

   ---------
   -- Put --
   ---------

   procedure Put
     (Item : Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
      Buffer : String (1 .. 32);
   begin
      Put (Buffer, Item, Aft, Exp);
      -- Trim and pad for Fore
      declare
         Start : Natural := 1;
         Len   : Natural := 0;
      begin
         -- Find start of number
         while Start <= Buffer'Last and then Buffer (Start) = ' ' loop
            Start := Start + 1;
         end loop;
         -- Find length
         Len := Buffer'Last - Start + 1;
         -- Pad with spaces for Fore
         for I in 1 .. Integer'Max (0, Fore - Len) loop
            Ada.Text_IO.Put (' ');
         end loop;
         -- Output the number
         Ada.Text_IO.Put (Buffer (Start .. Buffer'Last));
      end;
   end Put;

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
   begin
      -- Simplified: output to standard output
      Put (Item, Fore, Aft, Exp);
   end Put;

   procedure Put
     (To   : out String;
      Item : Float;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
      Temp : String (1 .. 32);
      Sign : Character := ' ';
      Int_Part : Integer;
      Frac_Part : Float;
      Digit : Integer;
      Pos : Integer := 1;
      Scale : Float := 1.0;
      Abs_Value : Float;
   begin
      -- Initialize output with spaces
      for I in To'Range loop
         To (I) := ' ';
      end loop;

      -- Handle sign
      if Item < 0.0 then
         Sign := '-';
         Abs_Value := -Item;
      else
         Abs_Value := Item;
      end if;

      -- Simple formatting without exponent
      if Exp = 0 then
         -- Get integer part
         Int_Part := Integer (Abs_Value);
         Frac_Part := Abs_Value - Float (Int_Part);

         -- Format integer part
         declare
            Int_Str : String (1 .. 10);
            Int_Pos : Integer := 10;
            N : Integer := Int_Part;
         begin
            if N = 0 then
               Int_Str (Int_Pos) := '0';
               Int_Pos := Int_Pos - 1;
            else
               while N > 0 loop
                  Int_Str (Int_Pos) := Character'Val (Character'Pos ('0') + (N mod 10));
                  N := N / 10;
                  Int_Pos := Int_Pos - 1;
               end loop;
            end if;

            -- Copy sign and integer part
            Pos := To'First;
            if Sign = '-' then
               To (Pos) := Sign;
               Pos := Pos + 1;
            end if;

            for I in Int_Pos + 1 .. 10 loop
               if Pos <= To'Last then
                  To (Pos) := Int_Str (I);
                  Pos := Pos + 1;
               end if;
            end loop;
         end;

         -- Add decimal point and fractional part
         if Pos <= To'Last and Aft > 0 then
            To (Pos) := '.';
            Pos := Pos + 1;

            for I in 1 .. Aft loop
               Frac_Part := Frac_Part * 10.0;
               Digit := Integer (Frac_Part);
               if Digit > 9 then
                  Digit := 9;
               end if;
               Frac_Part := Frac_Part - Float (Digit);
               if Pos <= To'Last then
                  To (Pos) := Character'Val (Character'Pos ('0') + Digit);
                  Pos := Pos + 1;
               end if;
            end loop;
         end if;
      else
         -- Scientific notation with exponent
         -- Normalize to have one digit before decimal
         declare
            Exponent : Integer := 0;
            Mantissa : Float := Abs_Value;
         begin
            if Mantissa /= 0.0 then
               while Mantissa >= 10.0 loop
                  Mantissa := Mantissa / 10.0;
                  Exponent := Exponent + 1;
               end loop;
               while Mantissa < 1.0 and Mantissa /= 0.0 loop
                  Mantissa := Mantissa * 10.0;
                  Exponent := Exponent - 1;
               end loop;
            end if;

            -- Output sign
            Pos := To'First;
            if Sign = '-' then
               To (Pos) := Sign;
               Pos := Pos + 1;
            end if;

            -- Output mantissa digit
            Digit := Integer (Mantissa);
            if Digit > 9 then
               Digit := 9;
            end if;
            To (Pos) := Character'Val (Character'Pos ('0') + Digit);
            Pos := Pos + 1;
            Mantissa := Mantissa - Float (Digit);

            -- Decimal point and fractional digits
            if Pos <= To'Last and Aft > 0 then
               To (Pos) := '.';
               Pos := Pos + 1;

               for I in 1 .. Aft loop
                  Mantissa := Mantissa * 10.0;
                  Digit := Integer (Mantissa);
                  if Digit > 9 then
                     Digit := 9;
                  end if;
                  Mantissa := Mantissa - Float (Digit);
                  if Pos <= To'Last then
                     To (Pos) := Character'Val (Character'Pos ('0') + Digit);
                     Pos := Pos + 1;
                  end if;
               end loop;
            end if;

            -- Exponent
            if Pos <= To'Last then
               To (Pos) := 'E';
               Pos := Pos + 1;
            end if;

            if Pos <= To'Last then
               if Exponent < 0 then
                  To (Pos) := '-';
                  Exponent := -Exponent;
               else
                  To (Pos) := '+';
               end if;
               Pos := Pos + 1;
            end if;

            -- Exponent digits (padded to Exp width)
            declare
               Exp_Str : String (1 .. 4);
               Exp_Pos : Integer := 4;
               E : Integer := Exponent;
            begin
               for I in Exp_Str'Range loop
                  Exp_Str (I) := '0';
               end loop;

               while E > 0 loop
                  Exp_Str (Exp_Pos) := Character'Val (Character'Pos ('0') + (E mod 10));
                  E := E / 10;
                  Exp_Pos := Exp_Pos - 1;
               end loop;

               -- Output exponent digits
               for I in 5 - Exp .. 4 loop
                  if Pos <= To'Last then
                     To (Pos) := Exp_Str (I);
                     Pos := Pos + 1;
                  end if;
               end loop;
            end;
         end;
      end if;
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (Item  : out Float;
      Width : Ada.Text_IO.Field := 0)
   is
      Buffer : String (1 .. 32);
      Pos    : Integer := 1;
      Ch     : Character;
   begin
      -- Skip leading spaces
      loop
         Ada.Text_IO.Get (Ch);
         exit when Ch /= ' ';
      end loop;

      -- Read number characters
      Buffer (Pos) := Ch;
      Pos := Pos + 1;

      while Pos <= Buffer'Last loop
         Ada.Text_IO.Get (Ch);
         exit when Ch = ' ' or Ch = Character'Val (10) or Ch = Character'Val (13);
         Buffer (Pos) := Ch;
         Pos := Pos + 1;
      end loop;

      -- Null-terminate
      if Pos <= Buffer'Last then
         Buffer (Pos) := Character'Val (0);
      end if;

      -- Parse the number
      Get (Buffer (1 .. Pos - 1), Item, Pos);
   end Get;

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Float;
      Width : Ada.Text_IO.Field := 0)
   is
   begin
      -- Simplified: read from standard input
      Get (Item, Width);
   end Get;

   procedure Get
     (From : String;
      Item : out Float;
      Last : out Positive)
   is
      Sign     : Float := 1.0;
      Result   : Float := 0.0;
      Frac     : Float := 0.0;
      Frac_Div : Float := 1.0;
      Exp      : Integer := 0;
      Exp_Sign : Integer := 1;
      Pos      : Integer := From'First;
      In_Frac  : Boolean := False;
      In_Exp   : Boolean := False;
   begin
      -- Skip leading spaces
      while Pos <= From'Last and then From (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      -- Check for sign
      if Pos <= From'Last then
         if From (Pos) = '-' then
            Sign := -1.0;
            Pos := Pos + 1;
         elsif From (Pos) = '+' then
            Pos := Pos + 1;
         end if;
      end if;

      -- Parse digits
      while Pos <= From'Last loop
         if From (Pos) >= '0' and From (Pos) <= '9' then
            if In_Exp then
               Exp := Exp * 10 + (Character'Pos (From (Pos)) - Character'Pos ('0'));
            elsif In_Frac then
               Frac_Div := Frac_Div * 10.0;
               Frac := Frac + Float (Character'Pos (From (Pos)) - Character'Pos ('0')) / Frac_Div;
            else
               Result := Result * 10.0 + Float (Character'Pos (From (Pos)) - Character'Pos ('0'));
            end if;
         elsif From (Pos) = '.' and not In_Frac and not In_Exp then
            In_Frac := True;
         elsif (From (Pos) = 'E' or From (Pos) = 'e') and not In_Exp then
            In_Exp := True;
            -- Check exponent sign
            if Pos < From'Last then
               if From (Pos + 1) = '-' then
                  Exp_Sign := -1;
                  Pos := Pos + 1;
               elsif From (Pos + 1) = '+' then
                  Pos := Pos + 1;
               end if;
            end if;
         else
            exit;
         end if;
         Pos := Pos + 1;
      end loop;

      -- Combine parts
      Result := Sign * (Result + Frac);

      -- Apply exponent
      Exp := Exp * Exp_Sign;
      while Exp > 0 loop
         Result := Result * 10.0;
         Exp := Exp - 1;
      end loop;
      while Exp < 0 loop
         Result := Result / 10.0;
         Exp := Exp + 1;
      end loop;

      Item := Result;
      Last := Pos - 1;
   end Get;

end Ada.Float_Text_IO;
