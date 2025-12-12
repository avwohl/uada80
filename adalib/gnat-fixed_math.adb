-- GNAT.Fixed_Math body for Z80
-- Fixed-point mathematics implementation

package body GNAT.Fixed_Math is

   -- Sine table for 0-90 degrees (every 5 degrees, scaled by 256)
   Sin_Table : constant array (0 .. 18) of Natural :=
     (0, 22, 44, 66, 87, 107, 125, 142, 157, 171,
      182, 191, 198, 203, 207, 209, 213, 215, 217);

   -------------------
   -- To_Fixed_8_8 --
   -------------------

   function To_Fixed_8_8 (Value : Integer) return Fixed_8_8 is
   begin
      return Value * Scale_8_8;
   end To_Fixed_8_8;

   function To_Fixed_8_8 (Int_Part : Integer; Frac_Hundredths : Natural) return Fixed_8_8 is
      Frac : constant Integer := (Integer (Frac_Hundredths) * Scale_8_8) / 100;
   begin
      if Int_Part >= 0 then
         return Int_Part * Scale_8_8 + Frac;
      else
         return Int_Part * Scale_8_8 - Frac;
      end if;
   end To_Fixed_8_8;

   ---------------------
   -- From_Fixed_8_8 --
   ---------------------

   function From_Fixed_8_8 (Value : Fixed_8_8) return Integer is
   begin
      if Value >= 0 then
         return Value / Scale_8_8;
      else
         return -((-Value) / Scale_8_8);
      end if;
   end From_Fixed_8_8;

   procedure From_Fixed_8_8 (Value : Fixed_8_8;
                              Int_Part : out Integer;
                              Frac_Hundredths : out Natural) is
      AV : constant Integer := abs Value;
   begin
      Int_Part := From_Fixed_8_8 (Value);
      Frac_Hundredths := ((AV mod Scale_8_8) * 100) / Scale_8_8;
   end From_Fixed_8_8;

   ---------
   -- Add --
   ---------

   function Add (A, B : Fixed_8_8) return Fixed_8_8 is
   begin
      return A + B;
   end Add;

   ---------
   -- Sub --
   ---------

   function Sub (A, B : Fixed_8_8) return Fixed_8_8 is
   begin
      return A - B;
   end Sub;

   ---------
   -- Mul --
   ---------

   function Mul (A, B : Fixed_8_8) return Fixed_8_8 is
      -- Need to handle overflow for Z80
      Result : Integer;
   begin
      Result := (A * B) / Scale_8_8;
      return Result;
   end Mul;

   ---------
   -- Div --
   ---------

   function Div (A, B : Fixed_8_8) return Fixed_8_8 is
   begin
      if B = 0 then
         if A >= 0 then
            return Fixed_8_8'Last;
         else
            return Fixed_8_8'First;
         end if;
      end if;
      return (A * Scale_8_8) / B;
   end Div;

   ------------
   -- Negate --
   ------------

   function Negate (A : Fixed_8_8) return Fixed_8_8 is
   begin
      return -A;
   end Negate;

   -------------
   -- Abs_Val --
   -------------

   function Abs_Val (A : Fixed_8_8) return Fixed_8_8 is
   begin
      if A < 0 then
         return -A;
      else
         return A;
      end if;
   end Abs_Val;

   ---------
   -- "<" --
   ---------

   function "<" (A, B : Fixed_8_8) return Boolean is
   begin
      return Integer (A) < Integer (B);
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (A, B : Fixed_8_8) return Boolean is
   begin
      return Integer (A) > Integer (B);
   end ">";

   ----------
   -- "<=" --
   ----------

   function "<=" (A, B : Fixed_8_8) return Boolean is
   begin
      return Integer (A) <= Integer (B);
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (A, B : Fixed_8_8) return Boolean is
   begin
      return Integer (A) >= Integer (B);
   end ">=";

   ----------
   -- Sqrt --
   ----------

   function Sqrt (A : Fixed_8_8) return Fixed_8_8 is
      Guess, Prev : Integer;
      N : Integer;
   begin
      if A <= 0 then
         return 0;
      end if;

      -- Scale for better precision
      N := A * Scale_8_8;  -- Now in 16.8 effectively
      Guess := N / 2;

      if Guess = 0 then
         Guess := 1;
      end if;

      for I in 1 .. 10 loop
         Prev := Guess;
         Guess := (Guess + N / Guess) / 2;
         exit when abs (Guess - Prev) <= 1;
      end loop;

      return Guess;
   end Sqrt;

   ------------
   -- Square --
   ------------

   function Square (A : Fixed_8_8) return Fixed_8_8 is
   begin
      return Mul (A, A);
   end Square;

   ---------
   -- Sin --
   ---------

   function Sin (Angle : Fixed_8_8) return Fixed_8_8 is
      Deg : Integer := (Angle * 360) / (Two_Pi);  -- Convert to degrees
      Sign : Integer := 1;
      Idx : Natural;
      Frac : Natural;
      V1, V2 : Natural;
   begin
      -- Normalize to 0-360
      Deg := Deg mod 360;
      if Deg < 0 then
         Deg := Deg + 360;
      end if;

      -- Handle quadrants
      if Deg > 180 then
         Sign := -1;
         Deg := Deg - 180;
      end if;

      if Deg > 90 then
         Deg := 180 - Deg;
      end if;

      -- Table lookup with interpolation
      Idx := Deg / 5;
      if Idx > 18 then
         Idx := 18;
      end if;

      Frac := Deg mod 5;
      V1 := Sin_Table (Idx);

      if Idx < 18 then
         V2 := Sin_Table (Idx + 1);
         V1 := V1 + ((V2 - V1) * Frac) / 5;
      end if;

      return Sign * Integer (V1);
   end Sin;

   ---------
   -- Cos --
   ---------

   function Cos (Angle : Fixed_8_8) return Fixed_8_8 is
   begin
      return Sin (Angle + Half_Pi);
   end Cos;

   ---------
   -- Tan --
   ---------

   function Tan (Angle : Fixed_8_8) return Fixed_8_8 is
      S : constant Fixed_8_8 := Sin (Angle);
      C : constant Fixed_8_8 := Cos (Angle);
   begin
      if Abs_Val (C) < 4 then  -- Near 90 degrees
         if S >= 0 then
            return Fixed_8_8'Last;
         else
            return Fixed_8_8'First;
         end if;
      end if;
      return Div (S * Scale_8_8, C);
   end Tan;

   -----------
   -- Atan2 --
   -----------

   function Atan2 (Y, X : Fixed_8_8) return Fixed_8_8 is
      Angle : Integer;
      AX : constant Integer := Abs_Val (X);
      AY : constant Integer := Abs_Val (Y);
   begin
      if X = 0 and Y = 0 then
         return 0;
      end if;

      -- Approximate atan using ratio
      if AX >= AY then
         if AX > 0 then
            Angle := (AY * 45 * Scale_8_8) / (AX * 360);
         else
            Angle := 0;
         end if;
      else
         Angle := Half_Pi / 4 - (AX * 45 * Scale_8_8) / (AY * 360);
      end if;

      -- Adjust for quadrant
      if X < 0 then
         Angle := Pi - Angle;
      end if;
      if Y < 0 then
         Angle := -Angle;
      end if;

      return Angle;
   end Atan2;

   ---------
   -- Exp --
   ---------

   function Exp (A : Fixed_8_8) return Fixed_8_8 is
      -- Taylor series approximation: e^x = 1 + x + x²/2 + x³/6 + ...
      Result : Integer := Scale_8_8;  -- 1.0
      Term : Integer := A;
      N : Integer := 1;
   begin
      if A > 4 * Scale_8_8 then  -- Overflow protection
         return Fixed_8_8'Last;
      end if;

      for I in 1 .. 8 loop
         Result := Result + Term;
         N := N + 1;
         Term := Mul (Term, A) / N;
         exit when Abs_Val (Term) < 1;
      end loop;

      return Result;
   end Exp;

   ---------
   -- Log --
   ---------

   function Log (A : Fixed_8_8) return Fixed_8_8 is
      -- Newton's method for ln(x)
      X : Integer;
      Y : Integer;
   begin
      if A <= 0 then
         return Fixed_8_8'First;
      end if;

      -- Normalize to range [1, 2] and count powers of 2
      X := A;
      Y := 0;

      while X > 2 * Scale_8_8 loop
         X := X / 2;
         Y := Y + Scale_8_8;  -- ln(2) ≈ 0.693
      end loop;

      while X < Scale_8_8 loop
         X := X * 2;
         Y := Y - Scale_8_8;
      end loop;

      -- Approximate ln for [1, 2] range
      -- ln(x) ≈ (x-1) - (x-1)²/2 for x near 1
      declare
         XM1 : constant Integer := X - Scale_8_8;
         Approx : Integer;
      begin
         Approx := XM1 - Mul (XM1, XM1) / (2 * Scale_8_8);
         -- Adjust Y by ln(2) factor
         Y := (Y * 177) / Scale_8_8 + Approx;  -- 177/256 ≈ 0.693 = ln(2)
      end;

      return Y;
   end Log;

   ---------
   -- Pow --
   ---------

   function Pow (Base, Exp : Fixed_8_8) return Fixed_8_8 is
   begin
      if Base <= 0 then
         return 0;
      end if;
      -- x^y = e^(y * ln(x))
      return GNAT.Fixed_Math.Exp (Mul (Exp, Log (Base)));
   end Pow;

   -----------
   -- Floor --
   -----------

   function Floor (A : Fixed_8_8) return Fixed_8_8 is
   begin
      if A >= 0 then
         return (A / Scale_8_8) * Scale_8_8;
      else
         return ((A - Scale_8_8 + 1) / Scale_8_8) * Scale_8_8;
      end if;
   end Floor;

   ----------
   -- Ceil --
   ----------

   function Ceil (A : Fixed_8_8) return Fixed_8_8 is
   begin
      if A >= 0 then
         return ((A + Scale_8_8 - 1) / Scale_8_8) * Scale_8_8;
      else
         return (A / Scale_8_8) * Scale_8_8;
      end if;
   end Ceil;

   -----------
   -- Round --
   -----------

   function Round (A : Fixed_8_8) return Fixed_8_8 is
   begin
      if A >= 0 then
         return ((A + Half) / Scale_8_8) * Scale_8_8;
      else
         return ((A - Half) / Scale_8_8) * Scale_8_8;
      end if;
   end Round;

   -----------
   -- Trunc --
   -----------

   function Trunc (A : Fixed_8_8) return Fixed_8_8 is
   begin
      return (A / Scale_8_8) * Scale_8_8;
   end Trunc;

   ----------
   -- Frac --
   ----------

   function Frac (A : Fixed_8_8) return Fixed_8_8 is
   begin
      return A - Trunc (A);
   end Frac;

   ---------
   -- Min --
   ---------

   function Min (A, B : Fixed_8_8) return Fixed_8_8 is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;

   ---------
   -- Max --
   ---------

   function Max (A, B : Fixed_8_8) return Fixed_8_8 is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;

   -----------
   -- Clamp --
   -----------

   function Clamp (Value, Low, High : Fixed_8_8) return Fixed_8_8 is
   begin
      if Value < Low then
         return Low;
      elsif Value > High then
         return High;
      else
         return Value;
      end if;
   end Clamp;

   ----------
   -- Lerp --
   ----------

   function Lerp (A, B : Fixed_8_8; T : Natural) return Fixed_8_8 is
   begin
      return A + ((B - A) * Integer (T)) / Scale_8_8;
   end Lerp;

   -----------
   -- Image --
   -----------

   function Image (A : Fixed_8_8) return String is
   begin
      return Image (A, 2);
   end Image;

   function Image (A : Fixed_8_8; Decimals : Natural) return String is
      Int_Part : Integer;
      Frac_Part : Natural;
      Result : String (1 .. 16);
      Pos : Natural := 1;
      Scale : Natural := 1;
      F : Natural;
   begin
      From_Fixed_8_8 (A, Int_Part, Frac_Part);

      -- Handle negative
      if A < 0 then
         Result (Pos) := '-';
         Pos := Pos + 1;
         Int_Part := abs Int_Part;
      end if;

      -- Integer part
      declare
         Int_Str : constant String := Integer'Image (Int_Part);
      begin
         for C of Int_Str (Int_Str'First + 1 .. Int_Str'Last) loop
            Result (Pos) := C;
            Pos := Pos + 1;
         end loop;
      end;

      if Decimals > 0 then
         Result (Pos) := '.';
         Pos := Pos + 1;

         -- Scale frac part
         for I in 1 .. Decimals loop
            Scale := Scale * 10;
         end loop;

         F := (Frac_Part * Scale) / 100;

         -- Pad with leading zeros
         declare
            Frac_Str : constant String := Natural'Image (F);
            Frac_Len : constant Natural := Frac_Str'Length - 1;
         begin
            for I in 1 .. Decimals - Frac_Len loop
               Result (Pos) := '0';
               Pos := Pos + 1;
            end loop;
            for C of Frac_Str (Frac_Str'First + 1 .. Frac_Str'Last) loop
               exit when Pos > 15;
               Result (Pos) := C;
               Pos := Pos + 1;
            end loop;
         end;
      end if;

      return Result (1 .. Pos - 1);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (S : String) return Fixed_8_8 is
      Int_Part : Integer := 0;
      Frac_Part : Natural := 0;
      Negative : Boolean := False;
      In_Frac : Boolean := False;
      Frac_Scale : Natural := 1;
      I : Natural := S'First;
   begin
      -- Skip whitespace
      while I <= S'Last and then (S (I) = ' ' or S (I) = Character'Val (9)) loop
         I := I + 1;
      end loop;

      if I > S'Last then
         return 0;
      end if;

      if S (I) = '-' then
         Negative := True;
         I := I + 1;
      elsif S (I) = '+' then
         I := I + 1;
      end if;

      while I <= S'Last loop
         if S (I) = '.' then
            In_Frac := True;
         elsif S (I) >= '0' and S (I) <= '9' then
            if In_Frac then
               if Frac_Scale < 100 then
                  Frac_Part := Frac_Part * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
                  Frac_Scale := Frac_Scale * 10;
               end if;
            else
               Int_Part := Int_Part * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
            end if;
         else
            exit;
         end if;
         I := I + 1;
      end loop;

      -- Normalize frac to hundredths
      while Frac_Scale < 100 loop
         Frac_Part := Frac_Part * 10;
         Frac_Scale := Frac_Scale * 10;
      end loop;

      if Negative then
         return -To_Fixed_8_8 (Int_Part, Frac_Part);
      else
         return To_Fixed_8_8 (Int_Part, Frac_Part);
      end if;
   end Value;

   ----------------
   -- Deg_To_Rad --
   ----------------

   function Deg_To_Rad (Degrees : Fixed_8_8) return Fixed_8_8 is
   begin
      -- radians = degrees * pi / 180
      return Mul (Degrees, Pi) / (180 * Scale_8_8 / Scale_8_8);
   end Deg_To_Rad;

   ----------------
   -- Rad_To_Deg --
   ----------------

   function Rad_To_Deg (Radians : Fixed_8_8) return Fixed_8_8 is
   begin
      -- degrees = radians * 180 / pi
      return Mul (Radians, 180 * Scale_8_8) / Pi;
   end Rad_To_Deg;

end GNAT.Fixed_Math;
