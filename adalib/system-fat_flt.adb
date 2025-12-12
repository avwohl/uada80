-- System.Fat_Flt body for Z80
-- Float type attributes support implementation

package body System.Fat_Flt is

   --------------
   -- Adjacent --
   --------------

   function Adjacent (X : Float; Towards : Float) return Float is
   begin
      if X = Towards then
         return X;
      elsif X < Towards then
         return Succ (X);
      else
         return Pred (X);
      end if;
   end Adjacent;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (X : Float) return Float is
      T : constant Float := Float'Truncation (X);
   begin
      if X > T then
         return T + 1.0;
      else
         return T;
      end if;
   end Ceiling;

   -------------
   -- Compose --
   -------------

   function Compose (Fraction : Float; Exponent : Integer) return Float is
   begin
      return Scaling (Fraction, Exponent - Float_Mantissa_Size);
   end Compose;

   ---------------
   -- Copy_Sign --
   ---------------

   function Copy_Sign (Value, Sign : Float) return Float is
   begin
      if Sign >= 0.0 then
         return abs Value;
      else
         return -abs Value;
      end if;
   end Copy_Sign;

   --------------
   -- Exponent --
   --------------

   function Exponent (X : Float) return Integer is
      E   : Integer := 0;
      Val : Float := abs X;
   begin
      if X = 0.0 then
         return 0;
      end if;

      while Val >= 2.0 loop
         Val := Val / 2.0;
         E := E + 1;
      end loop;

      while Val < 1.0 loop
         Val := Val * 2.0;
         E := E - 1;
      end loop;

      return E + 1;
   end Exponent;

   -----------
   -- Floor --
   -----------

   function Floor (X : Float) return Float is
      T : constant Float := Float'Truncation (X);
   begin
      if X < T then
         return T - 1.0;
      else
         return T;
      end if;
   end Floor;

   --------------
   -- Fraction --
   --------------

   function Fraction (X : Float) return Float is
   begin
      if X = 0.0 then
         return 0.0;
      end if;
      return Scaling (X, -Exponent (X));
   end Fraction;

   ------------------
   -- Leading_Part --
   ------------------

   function Leading_Part (X : Float; Radix_Digits : Integer) return Float is
   begin
      -- Simplified: just return X for now
      return X;
   end Leading_Part;

   -------------
   -- Machine --
   -------------

   function Machine (X : Float) return Float is
   begin
      return X;
   end Machine;

   ----------------------
   -- Machine_Rounding --
   ----------------------

   function Machine_Rounding (X : Float) return Float is
   begin
      return Rounding (X);
   end Machine_Rounding;

   -----------
   -- Model --
   -----------

   function Model (X : Float) return Float is
   begin
      return X;
   end Model;

   ----------
   -- Pred --
   ----------

   function Pred (X : Float) return Float is
      Eps : constant Float := 1.0E-7;  -- Approximate machine epsilon
   begin
      if X = 0.0 then
         return -Float'Model_Small;
      else
         return X - abs (X) * Eps - Float'Model_Small;
      end if;
   end Pred;

   ---------------
   -- Remainder --
   ---------------

   function Remainder (X, Y : Float) return Float is
      Q : Float;
   begin
      Q := Float'Truncation (X / Y);
      return X - Q * Y;
   end Remainder;

   --------------
   -- Rounding --
   --------------

   function Rounding (X : Float) return Float is
   begin
      if X >= 0.0 then
         return Float'Truncation (X + 0.5);
      else
         return Float'Truncation (X - 0.5);
      end if;
   end Rounding;

   -------------
   -- Scaling --
   -------------

   function Scaling (X : Float; Adjustment : Integer) return Float is
      Result : Float := X;
   begin
      if Adjustment > 0 then
         for I in 1 .. Adjustment loop
            Result := Result * 2.0;
         end loop;
      elsif Adjustment < 0 then
         for I in 1 .. abs Adjustment loop
            Result := Result / 2.0;
         end loop;
      end if;
      return Result;
   end Scaling;

   ----------
   -- Succ --
   ----------

   function Succ (X : Float) return Float is
      Eps : constant Float := 1.0E-7;
   begin
      if X = 0.0 then
         return Float'Model_Small;
      else
         return X + abs (X) * Eps + Float'Model_Small;
      end if;
   end Succ;

   ----------------
   -- Truncation --
   ----------------

   function Truncation (X : Float) return Float is
   begin
      if X >= 0.0 then
         return Float (Integer (X - 0.5 + 0.5));
      else
         return Float (Integer (X + 0.5 - 0.5));
      end if;
   end Truncation;

   -----------------------
   -- Unbiased_Rounding --
   -----------------------

   function Unbiased_Rounding (X : Float) return Float is
      T : constant Float := Float'Truncation (X);
      D : constant Float := abs (X - T);
   begin
      if D < 0.5 then
         return T;
      elsif D > 0.5 then
         if X >= 0.0 then
            return T + 1.0;
         else
            return T - 1.0;
         end if;
      else
         -- Exact 0.5 - round to even
         if Integer (T) mod 2 = 0 then
            return T;
         elsif X >= 0.0 then
            return T + 1.0;
         else
            return T - 1.0;
         end if;
      end if;
   end Unbiased_Rounding;

   -------------------
   -- Machine_Radix --
   -------------------

   function Machine_Radix return Integer is
   begin
      return 2;
   end Machine_Radix;

   ----------------------
   -- Machine_Mantissa --
   ----------------------

   function Machine_Mantissa return Integer is
   begin
      return Float_Mantissa_Size;
   end Machine_Mantissa;

   ------------------
   -- Machine_Emin --
   ------------------

   function Machine_Emin return Integer is
   begin
      return -125;  -- Typical for IEEE single
   end Machine_Emin;

   ------------------
   -- Machine_Emax --
   ------------------

   function Machine_Emax return Integer is
   begin
      return 128;
   end Machine_Emax;

end System.Fat_Flt;
