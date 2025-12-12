-- System.Fat_LFlt body for Z80
-- Long_Float type attributes support implementation

package body System.Fat_LFlt is

   --------------
   -- Adjacent --
   --------------

   function Adjacent (X : Long_Float; Towards : Long_Float) return Long_Float is
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

   function Ceiling (X : Long_Float) return Long_Float is
      T : constant Long_Float := Long_Float'Truncation (X);
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

   function Compose (Fraction : Long_Float; Exponent : Integer) return Long_Float is
   begin
      return Scaling (Fraction, Exponent - Long_Float_Mantissa_Size);
   end Compose;

   ---------------
   -- Copy_Sign --
   ---------------

   function Copy_Sign (Value, Sign : Long_Float) return Long_Float is
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

   function Exponent (X : Long_Float) return Integer is
      E   : Integer := 0;
      Val : Long_Float := abs X;
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

   function Floor (X : Long_Float) return Long_Float is
      T : constant Long_Float := Long_Float'Truncation (X);
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

   function Fraction (X : Long_Float) return Long_Float is
   begin
      if X = 0.0 then
         return 0.0;
      end if;
      return Scaling (X, -Exponent (X));
   end Fraction;

   ------------------
   -- Leading_Part --
   ------------------

   function Leading_Part (X : Long_Float; Radix_Digits : Integer) return Long_Float is
   begin
      return X;
   end Leading_Part;

   -------------
   -- Machine --
   -------------

   function Machine (X : Long_Float) return Long_Float is
   begin
      return X;
   end Machine;

   ----------------------
   -- Machine_Rounding --
   ----------------------

   function Machine_Rounding (X : Long_Float) return Long_Float is
   begin
      return Rounding (X);
   end Machine_Rounding;

   -----------
   -- Model --
   -----------

   function Model (X : Long_Float) return Long_Float is
   begin
      return X;
   end Model;

   ----------
   -- Pred --
   ----------

   function Pred (X : Long_Float) return Long_Float is
      Eps : constant Long_Float := 1.0E-10;
   begin
      if X = 0.0 then
         return -Long_Float'Model_Small;
      else
         return X - abs (X) * Eps - Long_Float'Model_Small;
      end if;
   end Pred;

   ---------------
   -- Remainder --
   ---------------

   function Remainder (X, Y : Long_Float) return Long_Float is
      Q : Long_Float;
   begin
      Q := Long_Float'Truncation (X / Y);
      return X - Q * Y;
   end Remainder;

   --------------
   -- Rounding --
   --------------

   function Rounding (X : Long_Float) return Long_Float is
   begin
      if X >= 0.0 then
         return Long_Float'Truncation (X + 0.5);
      else
         return Long_Float'Truncation (X - 0.5);
      end if;
   end Rounding;

   -------------
   -- Scaling --
   -------------

   function Scaling (X : Long_Float; Adjustment : Integer) return Long_Float is
      Result : Long_Float := X;
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

   function Succ (X : Long_Float) return Long_Float is
      Eps : constant Long_Float := 1.0E-10;
   begin
      if X = 0.0 then
         return Long_Float'Model_Small;
      else
         return X + abs (X) * Eps + Long_Float'Model_Small;
      end if;
   end Succ;

   ----------------
   -- Truncation --
   ----------------

   function Truncation (X : Long_Float) return Long_Float is
   begin
      if X >= 0.0 then
         return Long_Float (Long_Long_Integer (X - 0.5 + 0.5));
      else
         return Long_Float (Long_Long_Integer (X + 0.5 - 0.5));
      end if;
   end Truncation;

   -----------------------
   -- Unbiased_Rounding --
   -----------------------

   function Unbiased_Rounding (X : Long_Float) return Long_Float is
      T : constant Long_Float := Long_Float'Truncation (X);
      D : constant Long_Float := abs (X - T);
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
         if Long_Long_Integer (T) mod 2 = 0 then
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
      return Long_Float_Mantissa_Size;
   end Machine_Mantissa;

   ------------------
   -- Machine_Emin --
   ------------------

   function Machine_Emin return Integer is
   begin
      return -125;
   end Machine_Emin;

   ------------------
   -- Machine_Emax --
   ------------------

   function Machine_Emax return Integer is
   begin
      return 128;
   end Machine_Emax;

end System.Fat_LFlt;
