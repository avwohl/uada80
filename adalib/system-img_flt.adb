-- System.Img_Flt body for Z80
-- Float image conversion

package body System.Img_Flt is

   -----------------
   -- Image_Float --
   -----------------

   procedure Image_Float
     (V : Float;
      S : in Out String;
      P : out Natural)
   is
   begin
      P := S'First;
      Set_Image_Float (V, S, P, Aft => 6, Exp => 2);
   end Image_Float;

   ---------------------
   -- Set_Image_Float --
   ---------------------

   procedure Set_Image_Float
     (V   : Float;
      S   : in Out String;
      P   : in Out Natural;
      Aft : Natural;
      Exp : Natural)
   is
      Val    : Float := abs V;
      E      : Integer := 0;
      D      : Integer;
      Frac   : Float;
   begin
      -- Handle sign
      if V < 0.0 then
         S (P) := '-';
      else
         S (P) := ' ';
      end if;
      P := P + 1;

      -- Handle zero
      if Val = 0.0 then
         S (P) := '0';
         P := P + 1;
         S (P) := '.';
         P := P + 1;
         for I in 1 .. Aft loop
            S (P) := '0';
            P := P + 1;
         end loop;
         if Exp > 0 then
            S (P) := 'E';
            P := P + 1;
            S (P) := '+';
            P := P + 1;
            for I in 1 .. Exp loop
               S (P) := '0';
               P := P + 1;
            end loop;
         end if;
         P := P - 1;
         return;
      end if;

      -- Normalize for scientific notation
      if Exp > 0 then
         while Val >= 10.0 loop
            Val := Val / 10.0;
            E := E + 1;
         end loop;
         while Val < 1.0 loop
            Val := Val * 10.0;
            E := E - 1;
         end loop;
      end if;

      -- Integer part
      D := Integer (Float'Truncation (Val));
      if D >= 10 then
         S (P) := Character'Val (Character'Pos ('0') + D / 10);
         P := P + 1;
         S (P) := Character'Val (Character'Pos ('0') + D mod 10);
      else
         S (P) := Character'Val (Character'Pos ('0') + D);
      end if;
      P := P + 1;

      -- Decimal point
      S (P) := '.';
      P := P + 1;

      -- Fractional part
      Frac := Val - Float'Truncation (Val);
      for I in 1 .. Aft loop
         Frac := Frac * 10.0;
         D := Integer (Float'Truncation (Frac));
         S (P) := Character'Val (Character'Pos ('0') + D mod 10);
         P := P + 1;
         Frac := Frac - Float'Truncation (Frac);
      end loop;

      -- Exponent
      if Exp > 0 then
         S (P) := 'E';
         P := P + 1;
         if E >= 0 then
            S (P) := '+';
         else
            S (P) := '-';
            E := -E;
         end if;
         P := P + 1;

         -- Exponent digits with padding
         declare
            Buf : String (1 .. 4);
            Len : Natural := 0;
         begin
            if E = 0 then
               Buf (1) := '0';
               Len := 1;
            else
               while E > 0 loop
                  Len := Len + 1;
                  Buf (Len) := Character'Val (Character'Pos ('0') + E mod 10);
                  E := E / 10;
               end loop;
            end if;

            -- Pad to Exp digits
            for I in Len + 1 .. Exp loop
               S (P) := '0';
               P := P + 1;
            end loop;

            -- Output digits
            for I in reverse 1 .. Len loop
               S (P) := Buf (I);
               P := P + 1;
            end loop;
         end;
      end if;

      P := P - 1;  -- Point to last written
   end Set_Image_Float;

end System.Img_Flt;
