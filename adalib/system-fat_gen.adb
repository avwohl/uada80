-- System.Fat_Gen body for Z80
-- Generic floating-point attribute functions

package body System.Fat_Gen is

   --------------
   -- Adjacent --
   --------------

   function Adjacent (X, Towards : T) return T is
   begin
      if X = Towards then
         return X;
      elsif Towards > X then
         return Succ (X);
      else
         return Pred (X);
      end if;
   end Adjacent;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (X : T) return T is
      Trunc : constant T := Truncation (X);
   begin
      if X > 0.0 and then X /= Trunc then
         return Trunc + 1.0;
      else
         return Trunc;
      end if;
   end Ceiling;

   -------------
   -- Compose --
   -------------

   function Compose (Fraction : T; Exponent : Integer) return T is
   begin
      return Scaling (Fraction, Exponent - System.Fat_Gen.Exponent (Fraction));
   end Compose;

   ---------------
   -- Copy_Sign --
   ---------------

   function Copy_Sign (Value, Sign : T) return T is
   begin
      if Sign >= 0.0 then
         return abs Value;
      else
         return -(abs Value);
      end if;
   end Copy_Sign;

   --------------
   -- Exponent --
   --------------

   function Exponent (X : T) return Integer is
      E : Integer := 0;
      V : T := abs X;
   begin
      if V = 0.0 then
         return 0;
      end if;

      while V >= 2.0 loop
         V := V / 2.0;
         E := E + 1;
      end loop;

      while V < 1.0 loop
         V := V * 2.0;
         E := E - 1;
      end loop;

      return E + 1;  -- Ada defines exponent for 1.0 <= |fraction| < 2.0
   end Exponent;

   -----------
   -- Floor --
   -----------

   function Floor (X : T) return T is
      Trunc : constant T := Truncation (X);
   begin
      if X < 0.0 and then X /= Trunc then
         return Trunc - 1.0;
      else
         return Trunc;
      end if;
   end Floor;

   --------------
   -- Fraction --
   --------------

   function Fraction (X : T) return T is
      E : constant Integer := Exponent (X);
   begin
      if X = 0.0 then
         return 0.0;
      else
         return Scaling (X, -E);
      end if;
   end Fraction;

   ------------------
   -- Leading_Part --
   ------------------

   function Leading_Part (X : T; Radix_Digits : Integer) return T is
   begin
      if Radix_Digits <= 0 then
         raise Constraint_Error;
      end if;
      -- Simplified: return X unchanged
      return X;
   end Leading_Part;

   -------------
   -- Machine --
   -------------

   function Machine (X : T) return T is
   begin
      return X;
   end Machine;

   ----------------------
   -- Machine_Rounding --
   ----------------------

   function Machine_Rounding (X : T) return T is
   begin
      return Rounding (X);
   end Machine_Rounding;

   -----------
   -- Model --
   -----------

   function Model (X : T) return T is
   begin
      return X;
   end Model;

   ----------
   -- Pred --
   ----------

   function Pred (X : T) return T is
      Small : T;
   begin
      if X = 0.0 then
         -- Return smallest negative denormal
         return -T'Small;
      end if;

      -- Calculate ulp (unit in last place)
      Small := Scaling (1.0, Exponent (X) - T'Machine_Mantissa);
      return X - Small;
   end Pred;

   ---------------
   -- Remainder --
   ---------------

   function Remainder (X, Y : T) return T is
      Q : T;
   begin
      if Y = 0.0 then
         raise Constraint_Error;
      end if;

      Q := Unbiased_Rounding (X / Y);
      return X - Q * Y;
   end Remainder;

   --------------
   -- Rounding --
   --------------

   function Rounding (X : T) return T is
      Trunc : constant T := Truncation (X);
      Frac  : constant T := X - Trunc;
   begin
      if Frac >= 0.5 then
         return Trunc + 1.0;
      elsif Frac <= -0.5 then
         return Trunc - 1.0;
      else
         return Trunc;
      end if;
   end Rounding;

   -------------
   -- Scaling --
   -------------

   function Scaling (X : T; Adjustment : Integer) return T is
      Result : T := X;
   begin
      if Adjustment > 0 then
         for I in 1 .. Adjustment loop
            Result := Result * 2.0;
         end loop;
      elsif Adjustment < 0 then
         for I in 1 .. (-Adjustment) loop
            Result := Result / 2.0;
         end loop;
      end if;
      return Result;
   end Scaling;

   ----------
   -- Succ --
   ----------

   function Succ (X : T) return T is
      Small : T;
   begin
      if X = 0.0 then
         -- Return smallest positive denormal
         return T'Small;
      end if;

      -- Calculate ulp (unit in last place)
      Small := Scaling (1.0, Exponent (X) - T'Machine_Mantissa);
      return X + Small;
   end Succ;

   ----------------
   -- Truncation --
   ----------------

   function Truncation (X : T) return T is
   begin
      if X >= 0.0 then
         return T (Integer (X - 0.5 + 0.0001));
      else
         return T (Integer (X + 0.5 - 0.0001));
      end if;
   end Truncation;

   -----------------------
   -- Unbiased_Rounding --
   -----------------------

   function Unbiased_Rounding (X : T) return T is
      Trunc : constant T := Truncation (X);
      Frac  : constant T := abs (X - Trunc);
   begin
      if Frac > 0.5 then
         if X > 0.0 then
            return Trunc + 1.0;
         else
            return Trunc - 1.0;
         end if;
      elsif Frac < 0.5 then
         return Trunc;
      else
         -- Exactly 0.5: round to even
         declare
            Even_Trunc : constant T := Trunc;
            Int_Trunc  : constant Integer := Integer (Even_Trunc);
         begin
            if Int_Trunc mod 2 = 0 then
               return Trunc;
            else
               if X > 0.0 then
                  return Trunc + 1.0;
               else
                  return Trunc - 1.0;
               end if;
            end if;
         end;
      end if;
   end Unbiased_Rounding;

   -----------
   -- Valid --
   -----------

   function Valid (X : T) return Boolean is
      pragma Unreferenced (X);
   begin
      -- Software float always produces valid values
      return True;
   end Valid;

end System.Fat_Gen;
