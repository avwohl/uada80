-- Ada.Text_IO.Complex_IO body for Z80
-- Text I/O for Complex numbers implementation

package body Ada.Text_IO.Complex_IO is

   use Complex_Types;

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Complex;
      Width : Field := 0)
   is
      pragma Unreferenced (File, Width);
   begin
      -- Simplified stub - real implementation needs parsing (re,im) or re+im*i
      Item := (Re => 0.0, Im => 0.0);
   end Get;

   procedure Get
     (Item  : out Complex;
      Width : Field := 0)
   is
   begin
      Get (Current_Input, Item, Width);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      pragma Unreferenced (Fore);
      Buffer : String (1 .. 60);
   begin
      Put (Buffer, Item, Aft, Exp);
      -- Find first non-space
      for I in Buffer'Range loop
         if Buffer (I) /= ' ' then
            Put (File, Buffer (I .. Buffer'Last));
            return;
         end if;
      end loop;
      Put (File, Buffer);
   end Put;

   procedure Put
     (Item : Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Put (Current_Output, Item, Fore, Aft, Exp);
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (From : String;
      Item : out Complex;
      Last : out Positive)
   is
      Re_Part : Real'Base := 0.0;
      Im_Part : Real'Base := 0.0;
      Pos     : Positive := From'First;
   begin
      -- Skip leading spaces
      while Pos <= From'Last and then From (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      -- Expect opening parenthesis
      if Pos <= From'Last and then From (Pos) = '(' then
         Pos := Pos + 1;
      end if;

      -- Parse real part (simplified)
      declare
         Start : constant Positive := Pos;
      begin
         while Pos <= From'Last and then
               (From (Pos) in '0' .. '9' or From (Pos) = '.' or
                From (Pos) = '-' or From (Pos) = '+' or
                From (Pos) = 'E' or From (Pos) = 'e')
         loop
            Pos := Pos + 1;
         end loop;
         if Pos > Start then
            Re_Part := Real'Base'Value (From (Start .. Pos - 1));
         end if;
      end;

      -- Skip comma
      while Pos <= From'Last and then (From (Pos) = ',' or From (Pos) = ' ') loop
         Pos := Pos + 1;
      end loop;

      -- Parse imaginary part
      declare
         Start : constant Positive := Pos;
      begin
         while Pos <= From'Last and then
               (From (Pos) in '0' .. '9' or From (Pos) = '.' or
                From (Pos) = '-' or From (Pos) = '+' or
                From (Pos) = 'E' or From (Pos) = 'e')
         loop
            Pos := Pos + 1;
         end loop;
         if Pos > Start then
            Im_Part := Real'Base'Value (From (Start .. Pos - 1));
         end if;
      end;

      -- Skip closing parenthesis
      if Pos <= From'Last and then From (Pos) = ')' then
         Pos := Pos + 1;
      end if;

      Item := (Re => Re_Part, Im => Im_Part);
      Last := Pos - 1;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Complex;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      Re_Str : String (1 .. 25);
      Im_Str : String (1 .. 25);
      Pos    : Natural := To'First;

      procedure Put_Float (S : out String; V : Real'Base) is
         Value    : Real'Base := abs V;
         Exponent : Integer := 0;
         Int_Part : Integer;
         Frac     : Real'Base;
         Idx      : Natural := S'First;
      begin
         S := (others => ' ');

         -- Handle sign
         if V < 0.0 then
            S (Idx) := '-';
            Idx := Idx + 1;
         end if;

         -- Normalize for exponent
         if Exp > 0 and Value /= 0.0 then
            while Value >= 10.0 loop
               Value := Value / 10.0;
               Exponent := Exponent + 1;
            end loop;
            while Value < 1.0 loop
               Value := Value * 10.0;
               Exponent := Exponent - 1;
            end loop;
         end if;

         -- Integer part
         Int_Part := Integer (Real'Base'Truncation (Value));
         if Int_Part > 9 then Int_Part := 9; end if;
         S (Idx) := Character'Val (Character'Pos ('0') + Int_Part);
         Idx := Idx + 1;

         -- Decimal point
         S (Idx) := '.';
         Idx := Idx + 1;

         -- Fractional digits
         Frac := Value - Real'Base (Int_Part);
         for I in 1 .. Aft loop
            exit when Idx > S'Last;
            Frac := Frac * 10.0;
            Int_Part := Integer (Real'Base'Truncation (Frac));
            if Int_Part > 9 then Int_Part := 9; end if;
            if Int_Part < 0 then Int_Part := 0; end if;
            S (Idx) := Character'Val (Character'Pos ('0') + Int_Part);
            Frac := Frac - Real'Base (Int_Part);
            Idx := Idx + 1;
         end loop;

         -- Exponent
         if Exp > 0 then
            if Idx <= S'Last then
               S (Idx) := 'E';
               Idx := Idx + 1;
            end if;
            if Idx <= S'Last then
               if Exponent >= 0 then
                  S (Idx) := '+';
               else
                  S (Idx) := '-';
                  Exponent := abs Exponent;
               end if;
               Idx := Idx + 1;
            end if;
            -- Exponent digits
            for I in 1 .. Exp loop
               exit when Idx > S'Last;
               S (Idx) := '0';
               Idx := Idx + 1;
            end loop;
            -- Fill in actual exponent from right
            Idx := Idx - 1;
            while Exponent > 0 and Idx >= S'First loop
               S (Idx) := Character'Val (Character'Pos ('0') + Exponent mod 10);
               Exponent := Exponent / 10;
               Idx := Idx - 1;
            end loop;
         end if;
      end Put_Float;

   begin
      To := (others => ' ');

      -- Opening parenthesis
      if Pos <= To'Last then
         To (Pos) := '(';
         Pos := Pos + 1;
      end if;

      -- Real part
      Put_Float (Re_Str, Item.Re);
      for I in Re_Str'Range loop
         exit when Pos > To'Last;
         exit when I > Re_Str'Last;
         if Re_Str (I) /= ' ' or I = Re_Str'First then
            To (Pos) := Re_Str (I);
            Pos := Pos + 1;
         end if;
      end loop;

      -- Comma
      if Pos <= To'Last then
         To (Pos) := ',';
         Pos := Pos + 1;
      end if;

      -- Imaginary part
      Put_Float (Im_Str, Item.Im);
      for I in Im_Str'Range loop
         exit when Pos > To'Last;
         exit when I > Im_Str'Last;
         if Im_Str (I) /= ' ' or I = Im_Str'First then
            To (Pos) := Im_Str (I);
            Pos := Pos + 1;
         end if;
      end loop;

      -- Closing parenthesis
      if Pos <= To'Last then
         To (Pos) := ')';
      end if;
   end Put;

end Ada.Text_IO.Complex_IO;
