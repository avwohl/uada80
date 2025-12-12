-- System.Fat_Sflt body for Z80
-- Short_Float type attributes support implementation

package body System.Fat_Sflt is

   function Adjacent (X : Short_Float; Towards : Short_Float) return Short_Float is
   begin
      if X = Towards then return X;
      elsif X < Towards then return Succ (X);
      else return Pred (X);
      end if;
   end Adjacent;

   function Ceiling (X : Short_Float) return Short_Float is
      T : constant Short_Float := Short_Float'Truncation (X);
   begin
      if X > T then return T + 1.0; else return T; end if;
   end Ceiling;

   function Compose (Fraction : Short_Float; Exponent : Integer) return Short_Float is
   begin
      return Scaling (Fraction, Exponent - Short_Float_Mantissa_Size);
   end Compose;

   function Copy_Sign (Value, Sign : Short_Float) return Short_Float is
   begin
      if Sign >= 0.0 then return abs Value; else return -abs Value; end if;
   end Copy_Sign;

   function Exponent (X : Short_Float) return Integer is
      E   : Integer := 0;
      Val : Short_Float := abs X;
   begin
      if X = 0.0 then return 0; end if;
      while Val >= 2.0 loop Val := Val / 2.0; E := E + 1; end loop;
      while Val < 1.0 loop Val := Val * 2.0; E := E - 1; end loop;
      return E + 1;
   end Exponent;

   function Floor (X : Short_Float) return Short_Float is
      T : constant Short_Float := Short_Float'Truncation (X);
   begin
      if X < T then return T - 1.0; else return T; end if;
   end Floor;

   function Fraction (X : Short_Float) return Short_Float is
   begin
      if X = 0.0 then return 0.0; end if;
      return Scaling (X, -Exponent (X));
   end Fraction;

   function Leading_Part (X : Short_Float; Radix_Digits : Integer) return Short_Float is
      pragma Unreferenced (Radix_Digits);
   begin
      return X;
   end Leading_Part;

   function Machine (X : Short_Float) return Short_Float is
   begin
      return X;
   end Machine;

   function Machine_Rounding (X : Short_Float) return Short_Float is
   begin
      return Rounding (X);
   end Machine_Rounding;

   function Model (X : Short_Float) return Short_Float is
   begin
      return X;
   end Model;

   function Pred (X : Short_Float) return Short_Float is
      Eps : constant Short_Float := 1.0E-7;
   begin
      if X = 0.0 then return -Short_Float'Model_Small;
      else return X - abs (X) * Eps - Short_Float'Model_Small;
      end if;
   end Pred;

   function Remainder (X, Y : Short_Float) return Short_Float is
   begin
      return X - Short_Float'Truncation (X / Y) * Y;
   end Remainder;

   function Rounding (X : Short_Float) return Short_Float is
   begin
      if X >= 0.0 then return Short_Float'Truncation (X + 0.5);
      else return Short_Float'Truncation (X - 0.5);
      end if;
   end Rounding;

   function Scaling (X : Short_Float; Adjustment : Integer) return Short_Float is
      Result : Short_Float := X;
   begin
      if Adjustment > 0 then
         for I in 1 .. Adjustment loop Result := Result * 2.0; end loop;
      elsif Adjustment < 0 then
         for I in 1 .. abs Adjustment loop Result := Result / 2.0; end loop;
      end if;
      return Result;
   end Scaling;

   function Succ (X : Short_Float) return Short_Float is
      Eps : constant Short_Float := 1.0E-7;
   begin
      if X = 0.0 then return Short_Float'Model_Small;
      else return X + abs (X) * Eps + Short_Float'Model_Small;
      end if;
   end Succ;

   function Truncation (X : Short_Float) return Short_Float is
   begin
      if X >= 0.0 then return Short_Float (Integer (X - 0.5 + 0.5));
      else return Short_Float (Integer (X + 0.5 - 0.5));
      end if;
   end Truncation;

   function Unbiased_Rounding (X : Short_Float) return Short_Float is
      T : constant Short_Float := Short_Float'Truncation (X);
      D : constant Short_Float := abs (X - T);
   begin
      if D < 0.5 then return T;
      elsif D > 0.5 then
         if X >= 0.0 then return T + 1.0; else return T - 1.0; end if;
      else
         if Integer (T) mod 2 = 0 then return T;
         elsif X >= 0.0 then return T + 1.0;
         else return T - 1.0;
         end if;
      end if;
   end Unbiased_Rounding;

   function Machine_Radix return Integer is begin return 2; end Machine_Radix;
   function Machine_Mantissa return Integer is begin return Short_Float_Mantissa_Size; end Machine_Mantissa;
   function Machine_Emin return Integer is begin return -125; end Machine_Emin;
   function Machine_Emax return Integer is begin return 128; end Machine_Emax;

end System.Fat_Sflt;
