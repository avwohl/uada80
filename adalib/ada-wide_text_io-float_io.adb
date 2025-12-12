-- Ada.Wide_Text_IO.Float_IO body for Z80
-- Wide float I/O operations implementation

package body Ada.Wide_Text_IO.Float_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      pragma Unreferenced (Width);
      WC       : Wide_Character;
      Val      : Long_Float := 0.0;
      Frac     : Long_Float := 0.0;
      Frac_Div : Long_Float := 1.0;
      Exp      : Integer := 0;
      Neg      : Boolean := False;
      Exp_Neg  : Boolean := False;
   begin
      -- Skip leading whitespace
      loop
         Get (File, WC);
         exit when WC /= ' ';
      end loop;

      -- Check sign
      if WC = '-' then
         Neg := True;
         Get (File, WC);
      elsif WC = '+' then
         Get (File, WC);
      end if;

      -- Integer part
      while Wide_Character'Pos (WC) >= Wide_Character'Pos ('0') and
            Wide_Character'Pos (WC) <= Wide_Character'Pos ('9')
      loop
         Val := Val * 10.0 + Long_Float
           (Wide_Character'Pos (WC) - Wide_Character'Pos ('0'));
         begin
            Get (File, WC);
         exception
            when End_Error =>
               WC := Wide_Character'Val (0);
               exit;
         end;
      end loop;

      -- Fractional part
      if WC = '.' then
         Get (File, WC);
         while Wide_Character'Pos (WC) >= Wide_Character'Pos ('0') and
               Wide_Character'Pos (WC) <= Wide_Character'Pos ('9')
         loop
            Frac_Div := Frac_Div * 10.0;
            Frac := Frac + Long_Float
              (Wide_Character'Pos (WC) - Wide_Character'Pos ('0')) / Frac_Div;
            begin
               Get (File, WC);
            exception
               when End_Error =>
                  WC := Wide_Character'Val (0);
                  exit;
            end;
         end loop;
      end if;

      Val := Val + Frac;

      -- Exponent
      if WC = 'E' or WC = 'e' then
         Get (File, WC);
         if WC = '-' then
            Exp_Neg := True;
            Get (File, WC);
         elsif WC = '+' then
            Get (File, WC);
         end if;

         while Wide_Character'Pos (WC) >= Wide_Character'Pos ('0') and
               Wide_Character'Pos (WC) <= Wide_Character'Pos ('9')
         loop
            Exp := Exp * 10 +
              (Wide_Character'Pos (WC) - Wide_Character'Pos ('0'));
            begin
               Get (File, WC);
            exception
               when End_Error => exit;
            end;
         end loop;

         if Exp_Neg then
            Exp := -Exp;
         end if;

         for I in 1 .. abs Exp loop
            if Exp > 0 then
               Val := Val * 10.0;
            else
               Val := Val / 10.0;
            end if;
         end loop;
      end if;

      if Neg then
         Val := -Val;
      end if;

      Item := Num (Val);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Item  : out Num;
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
      Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      pragma Unreferenced (Fore);
      S : Wide_String (1 .. 40);
   begin
      Put (S, Item, Aft, Exp);
      for I in S'Range loop
         if S (I) /= ' ' then
            Put (File, S (I .. S'Last));
            return;
         end if;
      end loop;
      Put (File, "0.0");
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (Item : Num;
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
     (From : Wide_String;
      Item : out Num;
      Last : out Positive)
   is
      Idx      : Positive := From'First;
      Val      : Long_Float := 0.0;
      Frac     : Long_Float := 0.0;
      Frac_Div : Long_Float := 1.0;
      Neg      : Boolean := False;
   begin
      -- Skip leading blanks
      while Idx <= From'Last and then From (Idx) = ' ' loop
         Idx := Idx + 1;
      end loop;

      -- Check sign
      if Idx <= From'Last and then From (Idx) = '-' then
         Neg := True;
         Idx := Idx + 1;
      elsif Idx <= From'Last and then From (Idx) = '+' then
         Idx := Idx + 1;
      end if;

      -- Integer part
      while Idx <= From'Last loop
         exit when Wide_Character'Pos (From (Idx)) < Wide_Character'Pos ('0')
           or Wide_Character'Pos (From (Idx)) > Wide_Character'Pos ('9');
         Val := Val * 10.0 + Long_Float
           (Wide_Character'Pos (From (Idx)) - Wide_Character'Pos ('0'));
         Idx := Idx + 1;
      end loop;

      -- Fractional part
      if Idx <= From'Last and then From (Idx) = '.' then
         Idx := Idx + 1;
         while Idx <= From'Last loop
            exit when Wide_Character'Pos (From (Idx)) < Wide_Character'Pos ('0')
              or Wide_Character'Pos (From (Idx)) > Wide_Character'Pos ('9');
            Frac_Div := Frac_Div * 10.0;
            Frac := Frac + Long_Float
              (Wide_Character'Pos (From (Idx)) - Wide_Character'Pos ('0')) / Frac_Div;
            Idx := Idx + 1;
         end loop;
      end if;

      Val := Val + Frac;
      if Neg then
         Val := -Val;
      end if;

      Item := Num (Val);
      Last := Idx - 1;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out Wide_String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      pragma Unreferenced (Exp);
      Idx : Natural := To'First;
      V   : Long_Float := Long_Float (abs Item);
      Int : Integer;
   begin
      To := (others => ' ');

      if Item < 0.0 then
         To (Idx) := '-';
         Idx := Idx + 1;
      end if;

      Int := Integer (V);
      if Long_Float (Int) > V then
         Int := Int - 1;
      end if;

      -- Integer part
      declare
         Temp : Wide_String (1 .. 15);
         T    : Natural := Temp'Last;
         I    : Integer := Int;
      begin
         if I = 0 then
            To (Idx) := '0';
            Idx := Idx + 1;
         else
            while I > 0 loop
               Temp (T) := Wide_Character'Val
                 (Wide_Character'Pos ('0') + (I mod 10));
               T := T - 1;
               I := I / 10;
            end loop;
            for J in T + 1 .. Temp'Last loop
               if Idx <= To'Last then
                  To (Idx) := Temp (J);
                  Idx := Idx + 1;
               end if;
            end loop;
         end if;
      end;

      -- Decimal point
      if Idx <= To'Last then
         To (Idx) := '.';
         Idx := Idx + 1;
      end if;

      -- Fractional part
      V := V - Long_Float (Int);
      for I in 1 .. Natural (Aft) loop
         V := V * 10.0;
         Int := Integer (V);
         if Int > 9 then
            Int := 9;
         end if;
         if Idx <= To'Last then
            To (Idx) := Wide_Character'Val (Wide_Character'Pos ('0') + Int);
            Idx := Idx + 1;
         end if;
         V := V - Long_Float (Int);
      end loop;
   end Put;

end Ada.Wide_Text_IO.Float_IO;
