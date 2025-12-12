-- Ada.Text_IO.Modular_IO body for Z80
-- Generic modular type I/O implementation

package body Ada.Text_IO.Modular_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      pragma Unreferenced (Width);
      C     : Character;
      Value : Num := 0;
      E     : Boolean;
   begin
      -- Skip leading whitespace
      loop
         Look_Ahead (File, C, E);
         exit when E or else C /= ' ';
         Get (File, C);
      end loop;

      -- Read digits
      while not E and then C in '0' .. '9' loop
         Get (File, C);
         Value := Value * 10 + Num (Character'Pos (C) - Character'Pos ('0'));
         Look_Ahead (File, C, E);
      end loop;

      Item := Value;
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
     (File  : File_Type;
      Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
      S : String (1 .. 40);
   begin
      Put (S, Item, Base);

      -- Find start of number
      for I in S'Range loop
         if S (I) /= ' ' then
            declare
               Actual : constant String := S (I .. S'Last);
               Pad    : constant Integer := Integer (Width) - Actual'Length;
            begin
               for J in 1 .. Pad loop
                  Put (File, ' ');
               end loop;
               Put (File, Actual);
               return;
            end;
         end if;
      end loop;
      Put (File, "0");
   end Put;

   procedure Put
     (Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
   begin
      Put (Current_Output, Item, Width, Base);
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (From : String;
      Item : out Num;
      Last : out Positive)
   is
      Idx   : Positive := From'First;
      Value : Num := 0;
   begin
      -- Skip leading blanks
      while Idx <= From'Last and then From (Idx) = ' ' loop
         Idx := Idx + 1;
      end loop;

      -- Read digits
      while Idx <= From'Last and then From (Idx) in '0' .. '9' loop
         Value := Value * 10 + Num (Character'Pos (From (Idx)) - Character'Pos ('0'));
         Idx := Idx + 1;
      end loop;

      Item := Value;
      Last := Idx - 1;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Num;
      Base : Number_Base := Default_Base)
   is
      Temp   : String (1 .. 40);
      Idx    : Natural := Temp'Last;
      Value  : Num := Item;
      B      : constant Num := Num (Base);
      Digits : constant String := "0123456789ABCDEF";
   begin
      To := (others => ' ');

      if Value = 0 then
         To (To'Last) := '0';
         return;
      end if;

      while Value > 0 loop
         Temp (Idx) := Digits (Integer (Value mod B) + 1);
         Idx := Idx - 1;
         Value := Value / B;
      end loop;

      -- Copy right-justified
      declare
         Len   : constant Natural := Temp'Last - Idx;
         Start : constant Natural := To'Last - Len + 1;
      begin
         if Start >= To'First then
            To (Start .. To'Last) := Temp (Idx + 1 .. Temp'Last);
         end if;
      end;
   end Put;

end Ada.Text_IO.Modular_IO;
