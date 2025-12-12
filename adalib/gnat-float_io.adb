-- GNAT.Float_IO body for Z80
-- Simple float I/O implementation

with GNAT.IO;

package body GNAT.Float_IO is

   ---------
   -- Put --
   ---------

   procedure Put
     (Item : Float;
      Fore : Natural := 2;
      Aft  : Natural := 6;
      Exp  : Natural := 0)
   is
      pragma Unreferenced (Fore, Exp);
   begin
      GNAT.IO.Put (Image (Item, Aft));
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get (Item : out Float) is
      S    : String (1 .. 40);
      Last : Natural;
   begin
      GNAT.IO.Get_Line (S, Last);
      Item := Value (S (1 .. Last));
   end Get;

   -----------
   -- Image --
   -----------

   function Image
     (Value : Float;
      Aft   : Natural := 6) return String
   is
      Temp : String (1 .. 40);
      Idx  : Natural := 1;
      V    : Float := abs Value;
      Int  : Integer;
      Frac : Float;
   begin
      -- Sign
      if Value < 0.0 then
         Temp (Idx) := '-';
         Idx := Idx + 1;
      end if;

      -- Integer part
      Int := Integer (V);
      if Float (Int) > V then
         Int := Int - 1;
      end if;

      declare
         Int_Str : String (1 .. 15);
         I_Idx   : Natural := Int_Str'Last;
         I       : Integer := Int;
      begin
         if I = 0 then
            Temp (Idx) := '0';
            Idx := Idx + 1;
         else
            while I > 0 loop
               Int_Str (I_Idx) := Character'Val (Character'Pos ('0') + I mod 10);
               I_Idx := I_Idx - 1;
               I := I / 10;
            end loop;
            for J in I_Idx + 1 .. Int_Str'Last loop
               Temp (Idx) := Int_Str (J);
               Idx := Idx + 1;
            end loop;
         end if;
      end;

      -- Decimal point
      Temp (Idx) := '.';
      Idx := Idx + 1;

      -- Fractional part
      Frac := V - Float (Int);
      for I in 1 .. Aft loop
         Frac := Frac * 10.0;
         Int := Integer (Frac);
         if Int > 9 then
            Int := 9;
         end if;
         Temp (Idx) := Character'Val (Character'Pos ('0') + Int);
         Idx := Idx + 1;
         Frac := Frac - Float (Int);
      end loop;

      return Temp (1 .. Idx - 1);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (S : String) return Float is
      Idx      : Positive := S'First;
      Neg      : Boolean := False;
      Int_Part : Float := 0.0;
      Frac     : Float := 0.0;
      Frac_Div : Float := 1.0;
      Exp      : Integer := 0;
      Exp_Neg  : Boolean := False;
   begin
      -- Skip leading blanks
      while Idx <= S'Last and then S (Idx) = ' ' loop
         Idx := Idx + 1;
      end loop;

      -- Check sign
      if Idx <= S'Last and then S (Idx) = '-' then
         Neg := True;
         Idx := Idx + 1;
      elsif Idx <= S'Last and then S (Idx) = '+' then
         Idx := Idx + 1;
      end if;

      -- Integer part
      while Idx <= S'Last and then S (Idx) in '0' .. '9' loop
         Int_Part := Int_Part * 10.0 +
           Float (Character'Pos (S (Idx)) - Character'Pos ('0'));
         Idx := Idx + 1;
      end loop;

      -- Fractional part
      if Idx <= S'Last and then S (Idx) = '.' then
         Idx := Idx + 1;
         while Idx <= S'Last and then S (Idx) in '0' .. '9' loop
            Frac_Div := Frac_Div * 10.0;
            Frac := Frac +
              Float (Character'Pos (S (Idx)) - Character'Pos ('0')) / Frac_Div;
            Idx := Idx + 1;
         end loop;
      end if;

      Int_Part := Int_Part + Frac;

      -- Exponent
      if Idx <= S'Last and then (S (Idx) = 'E' or S (Idx) = 'e') then
         Idx := Idx + 1;
         if Idx <= S'Last and then S (Idx) = '-' then
            Exp_Neg := True;
            Idx := Idx + 1;
         elsif Idx <= S'Last and then S (Idx) = '+' then
            Idx := Idx + 1;
         end if;

         while Idx <= S'Last and then S (Idx) in '0' .. '9' loop
            Exp := Exp * 10 + (Character'Pos (S (Idx)) - Character'Pos ('0'));
            Idx := Idx + 1;
         end loop;

         if Exp_Neg then
            Exp := -Exp;
         end if;

         for I in 1 .. abs Exp loop
            if Exp > 0 then
               Int_Part := Int_Part * 10.0;
            else
               Int_Part := Int_Part / 10.0;
            end if;
         end loop;
      end if;

      if Neg then
         return -Int_Part;
      else
         return Int_Part;
      end if;
   end Value;

end GNAT.Float_IO;
