-- Ada.Wide_Text_IO.Integer_IO body for Z80
-- Wide integer I/O operations implementation

package body Ada.Wide_Text_IO.Integer_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      pragma Unreferenced (Width);
      WC    : Wide_Character;
      Val   : Long_Long_Integer := 0;
      Neg   : Boolean := False;
      First : Boolean := True;
   begin
      -- Skip leading whitespace
      loop
         Get (File, WC);
         exit when WC /= ' ' and WC /= Wide_Character'Val (9);
      end loop;

      -- Check sign
      if WC = '-' then
         Neg := True;
         Get (File, WC);
      elsif WC = '+' then
         Get (File, WC);
      end if;

      -- Read digits
      loop
         if Wide_Character'Pos (WC) >= Wide_Character'Pos ('0') and
            Wide_Character'Pos (WC) <= Wide_Character'Pos ('9')
         then
            Val := Val * 10 + Long_Long_Integer
              (Wide_Character'Pos (WC) - Wide_Character'Pos ('0'));
            First := False;
         else
            exit;
         end if;
         begin
            Get (File, WC);
         exception
            when End_Error => exit;
         end;
      end loop;

      if First then
         raise Data_Error;
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
     (File  : File_Type;
      Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
      S : Wide_String (1 .. 40);
   begin
      Put (S, Item, Base);
      -- Find start and output with padding
      for I in S'Range loop
         if S (I) /= ' ' then
            declare
               Len : constant Natural := S'Last - I + 1;
               Pad : constant Integer := Integer (Width) - Len;
            begin
               for J in 1 .. Pad loop
                  Put (File, ' ');
               end loop;
               Put (File, S (I .. S'Last));
               return;
            end;
         end if;
      end loop;
      Put (File, "0");
   end Put;

   ---------
   -- Put --
   ---------

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
     (From : Wide_String;
      Item : out Num;
      Last : out Positive)
   is
      Idx : Positive := From'First;
      Val : Long_Long_Integer := 0;
      Neg : Boolean := False;
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

      -- Read digits
      while Idx <= From'Last loop
         exit when Wide_Character'Pos (From (Idx)) < Wide_Character'Pos ('0')
           or Wide_Character'Pos (From (Idx)) > Wide_Character'Pos ('9');
         Val := Val * 10 + Long_Long_Integer
           (Wide_Character'Pos (From (Idx)) - Wide_Character'Pos ('0'));
         Idx := Idx + 1;
      end loop;

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
      Base : Number_Base := Default_Base)
   is
      Temp : Wide_String (1 .. 40);
      Idx  : Natural := Temp'Last;
      V    : Long_Long_Integer := Long_Long_Integer (abs Item);
      B    : constant Long_Long_Integer := Long_Long_Integer (Base);
   begin
      To := (others => ' ');

      -- Convert digits
      loop
         declare
            D : constant Long_Long_Integer := V mod B;
         begin
            if D < 10 then
               Temp (Idx) := Wide_Character'Val
                 (Wide_Character'Pos ('0') + Integer (D));
            else
               Temp (Idx) := Wide_Character'Val
                 (Wide_Character'Pos ('A') + Integer (D) - 10);
            end if;
         end;
         Idx := Idx - 1;
         V := V / B;
         exit when V = 0;
      end loop;

      -- Add sign
      if Item < 0 then
         Temp (Idx) := '-';
         Idx := Idx - 1;
      end if;

      -- Copy to output right-justified
      declare
         Len   : constant Natural := Temp'Last - Idx;
         Start : constant Natural := To'Last - Len + 1;
      begin
         if Start >= To'First then
            To (Start .. To'Last) := Temp (Idx + 1 .. Temp'Last);
         end if;
      end;
   end Put;

end Ada.Wide_Text_IO.Integer_IO;
