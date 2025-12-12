-- System.Img_Util body for Z80
-- Common utilities for Image attribute implementations

package body System.Img_Util is

   ----------------
   -- Set_Blanks --
   ----------------

   procedure Set_Blanks
     (S     : in Out String;
      Start : Natural;
      Count : Natural)
   is
   begin
      for I in Start .. Start + Count - 1 loop
         S (I) := ' ';
      end loop;
   end Set_Blanks;

   ---------------
   -- Set_Digit --
   ---------------

   procedure Set_Digit
     (S   : in Out String;
      Pos : Natural;
      Val : Natural)
   is
   begin
      if Val < 10 then
         S (Pos) := Character'Val (Character'Pos ('0') + Val);
      else
         S (Pos) := Character'Val (Character'Pos ('A') + Val - 10);
      end if;
   end Set_Digit;

   -----------------------
   -- Set_Integer_Width --
   -----------------------

   procedure Set_Integer_Width
     (S     : in Out String;
      P     : in Out Natural;
      Val   : Integer;
      Width : Natural)
   is
      Buf   : String (1 .. 12);
      Len   : Natural := 0;
      V     : Integer := abs Val;
      Start : Natural;
   begin
      -- Build digits in reverse
      if V = 0 then
         Buf (1) := '0';
         Len := 1;
      else
         while V > 0 loop
            Len := Len + 1;
            Buf (Len) := Character'Val (Character'Pos ('0') + V mod 10);
            V := V / 10;
         end loop;
      end if;

      -- Calculate total width (sign + digits)
      declare
         Total : constant Natural := Len + 1;  -- Always include sign position
         Pad   : Natural := 0;
      begin
         if Width > Total then
            Pad := Width - Total;
         end if;

         -- Output padding
         for I in 1 .. Pad loop
            S (P) := ' ';
            P := P + 1;
         end loop;

         -- Output sign
         if Val < 0 then
            S (P) := '-';
         else
            S (P) := ' ';
         end if;
         P := P + 1;

         -- Output digits in correct order
         for I in reverse 1 .. Len loop
            S (P) := Buf (I);
            P := P + 1;
         end loop;

         P := P - 1;  -- Point to last written
      end;
   end Set_Integer_Width;

   --------------
   -- Set_Sign --
   --------------

   procedure Set_Sign
     (S        : in Out String;
      Pos      : Natural;
      Negative : Boolean)
   is
   begin
      if Negative then
         S (Pos) := '-';
      else
         S (Pos) := ' ';
      end if;
   end Set_Sign;

   ---------------
   -- Hex_Digit --
   ---------------

   function Hex_Digit (Val : Natural) return Character is
   begin
      if Val < 10 then
         return Character'Val (Character'Pos ('0') + Val);
      else
         return Character'Val (Character'Pos ('A') + Val - 10);
      end if;
   end Hex_Digit;

   ---------------
   -- Dec_Digit --
   ---------------

   function Dec_Digit (Val : Natural) return Character is
   begin
      return Character'Val (Character'Pos ('0') + Val mod 10);
   end Dec_Digit;

end System.Img_Util;
