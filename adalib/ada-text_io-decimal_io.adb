-- Ada.Text_IO.Decimal_IO body for Z80
-- Generic decimal type I/O implementation

package body Ada.Text_IO.Decimal_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      pragma Unreferenced (Width);
      Buffer : String (1 .. 40);
      Last   : Natural := 0;
      C      : Character;
      E      : Boolean;
   begin
      loop
         Look_Ahead (File, C, E);
         exit when E or else C /= ' ';
         Get (File, C);
      end loop;

      while not E and then
        (C in '0' .. '9' or C = '.' or C = '-' or C = '+')
      loop
         Get (File, C);
         Last := Last + 1;
         Buffer (Last) := C;
         Look_Ahead (File, C, E);
      end loop;

      Get (Buffer (1 .. Last), Item, Last);
   end Get;

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
      S : String (1 .. 40);
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
     (From : String;
      Item : out Num;
      Last : out Positive)
   is
      Idx      : Positive := From'First;
      Neg      : Boolean := False;
      Int_Part : Long_Long_Integer := 0;
      Frac     : Long_Long_Integer := 0;
      Frac_Div : Long_Long_Integer := 1;
   begin
      while Idx <= From'Last and then From (Idx) = ' ' loop
         Idx := Idx + 1;
      end loop;

      if Idx <= From'Last and then From (Idx) = '-' then
         Neg := True;
         Idx := Idx + 1;
      elsif Idx <= From'Last and then From (Idx) = '+' then
         Idx := Idx + 1;
      end if;

      while Idx <= From'Last and then From (Idx) in '0' .. '9' loop
         Int_Part := Int_Part * 10 +
           Long_Long_Integer (Character'Pos (From (Idx)) - Character'Pos ('0'));
         Idx := Idx + 1;
      end loop;

      if Idx <= From'Last and then From (Idx) = '.' then
         Idx := Idx + 1;
         while Idx <= From'Last and then From (Idx) in '0' .. '9' loop
            Frac := Frac * 10 +
              Long_Long_Integer (Character'Pos (From (Idx)) - Character'Pos ('0'));
            Frac_Div := Frac_Div * 10;
            Idx := Idx + 1;
         end loop;
      end if;

      declare
         Value : constant Long_Long_Float :=
           Long_Long_Float (Int_Part) + Long_Long_Float (Frac) / Long_Long_Float (Frac_Div);
      begin
         if Neg then
            Item := Num (-Value);
         else
            Item := Num (Value);
         end if;
      end;

      Last := Idx - 1;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      pragma Unreferenced (Exp);
      Idx  : Natural := To'First;
      V    : Long_Long_Float := Long_Long_Float (abs Item);
      Int  : Long_Long_Integer;
   begin
      To := (others => ' ');

      if Item < 0.0 then
         To (Idx) := '-';
         Idx := Idx + 1;
      end if;

      Int := Long_Long_Integer (V);

      declare
         Temp : String (1 .. 20);
         T    : Natural := Temp'Last;
         I    : Long_Long_Integer := Int;
      begin
         if I = 0 then
            To (Idx) := '0';
            Idx := Idx + 1;
         else
            while I > 0 loop
               Temp (T) := Character'Val (Character'Pos ('0') + Integer (I mod 10));
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

      if Idx <= To'Last then
         To (Idx) := '.';
         Idx := Idx + 1;
      end if;

      V := V - Long_Long_Float (Int);
      for I in 1 .. Natural (Aft) loop
         V := V * 10.0;
         Int := Long_Long_Integer (V);
         if Int > 9 then
            Int := 9;
         end if;
         if Idx <= To'Last then
            To (Idx) := Character'Val (Character'Pos ('0') + Integer (Int));
            Idx := Idx + 1;
         end if;
         V := V - Long_Long_Float (Int);
      end loop;
   end Put;

end Ada.Text_IO.Decimal_IO;
