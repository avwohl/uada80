-- GNAT.Integer_IO body for Z80
-- Simple integer I/O implementation

with GNAT.IO;

package body GNAT.Integer_IO is

   ---------
   -- Put --
   ---------

   procedure Put (Item : Integer; Width : Natural := 0; Base : Positive := 10) is
      S   : constant String := Image (Item, Base);
      Pad : constant Integer := Width - S'Length;
   begin
      for I in 1 .. Pad loop
         GNAT.IO.Put (' ');
      end loop;
      GNAT.IO.Put (S);
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get (Item : out Integer) is
      S    : String (1 .. 20);
      Last : Natural;
   begin
      GNAT.IO.Get_Line (S, Last);
      Item := Value (S (1 .. Last));
   end Get;

   -----------
   -- Image --
   -----------

   function Image (Value : Integer; Base : Positive := 10) return String is
      Temp   : String (1 .. 32);
      Idx    : Natural := Temp'Last;
      V      : Integer := abs Value;
      Digits : constant String := "0123456789ABCDEF";
   begin
      if V = 0 then
         return "0";
      end if;

      while V > 0 loop
         Temp (Idx) := Digits ((V mod Base) + 1);
         Idx := Idx - 1;
         V := V / Base;
      end loop;

      if Value < 0 then
         Temp (Idx) := '-';
         Idx := Idx - 1;
      end if;

      return Temp (Idx + 1 .. Temp'Last);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (S : String; Base : Positive := 10) return Integer is
      Result : Integer := 0;
      Neg    : Boolean := False;
      Idx    : Positive := S'First;
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

      -- Parse digits
      while Idx <= S'Last loop
         declare
            C : constant Character := S (Idx);
            D : Integer;
         begin
            if C in '0' .. '9' then
               D := Character'Pos (C) - Character'Pos ('0');
            elsif C in 'A' .. 'Z' then
               D := Character'Pos (C) - Character'Pos ('A') + 10;
            elsif C in 'a' .. 'z' then
               D := Character'Pos (C) - Character'Pos ('a') + 10;
            else
               exit;
            end if;

            exit when D >= Base;
            Result := Result * Base + D;
         end;
         Idx := Idx + 1;
      end loop;

      if Neg then
         return -Result;
      else
         return Result;
      end if;
   end Value;

end GNAT.Integer_IO;
