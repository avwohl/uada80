-- Ada.Numerics.Aux body for Z80
-- Auxiliary numeric operations implementation
-- Uses Taylor series approximations

package body Ada.Numerics.Aux is

   Pi : constant Float := 3.14159265358979323846;

   ---------
   -- Sin --
   ---------

   function Sin (X : Float) return Float is
      XX    : Float := X;
      Term  : Float;
      Sum   : Float;
      N     : Integer;
   begin
      -- Reduce to [-Pi, Pi]
      while XX > Pi loop
         XX := XX - 2.0 * Pi;
      end loop;
      while XX < -Pi loop
         XX := XX + 2.0 * Pi;
      end loop;

      -- Taylor series: x - x^3/3! + x^5/5! - ...
      Term := XX;
      Sum := XX;
      for I in 1 .. 7 loop
         N := 2 * I;
         Term := -Term * XX * XX / Float (N * (N + 1));
         Sum := Sum + Term;
      end loop;

      return Sum;
   end Sin;

   ---------
   -- Cos --
   ---------

   function Cos (X : Float) return Float is
   begin
      return Sin (X + Pi / 2.0);
   end Cos;

   ---------
   -- Tan --
   ---------

   function Tan (X : Float) return Float is
      C : constant Float := Cos (X);
   begin
      if abs C < 1.0E-10 then
         raise Constraint_Error;
      end if;
      return Sin (X) / C;
   end Tan;

   ---------
   -- Exp --
   ---------

   function Exp (X : Float) return Float is
      Term : Float := 1.0;
      Sum  : Float := 1.0;
   begin
      for I in 1 .. 15 loop
         Term := Term * X / Float (I);
         Sum := Sum + Term;
         exit when abs Term < 1.0E-10;
      end loop;
      return Sum;
   end Exp;

   ---------
   -- Log --
   ---------

   function Log (X : Float) return Float is
      Y    : Float;
      Term : Float;
      Sum  : Float;
   begin
      if X <= 0.0 then
         raise Constraint_Error;
      end if;

      -- Use ln(x) = 2 * arctanh((x-1)/(x+1)) for x near 1
      Y := (X - 1.0) / (X + 1.0);
      Term := Y;
      Sum := Y;

      for I in 1 .. 20 loop
         Term := Term * Y * Y;
         Sum := Sum + Term / Float (2 * I + 1);
      end loop;

      return 2.0 * Sum;
   end Log;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Float) return Float is
      Guess : Float;
   begin
      if X < 0.0 then
         raise Constraint_Error;
      end if;
      if X = 0.0 then
         return 0.0;
      end if;

      -- Newton's method
      Guess := X / 2.0;
      for I in 1 .. 10 loop
         Guess := (Guess + X / Guess) / 2.0;
      end loop;

      return Guess;
   end Sqrt;

   ----------
   -- Asin --
   ----------

   function Asin (X : Float) return Float is
      Term : Float;
      Sum  : Float;
      XX   : constant Float := X * X;
   begin
      if abs X > 1.0 then
         raise Constraint_Error;
      end if;

      -- Taylor series
      Term := X;
      Sum := X;
      for I in 1 .. 10 loop
         Term := Term * XX * Float ((2 * I - 1) * (2 * I - 1)) /
                 Float (2 * I * (2 * I + 1));
         Sum := Sum + Term;
      end loop;

      return Sum;
   end Asin;

   ----------
   -- Acos --
   ----------

   function Acos (X : Float) return Float is
   begin
      return Pi / 2.0 - Asin (X);
   end Acos;

   ----------
   -- Atan --
   ----------

   function Atan (X : Float) return Float is
      Term : Float;
      Sum  : Float;
      XX   : Float := X;
   begin
      if abs X > 1.0 then
         if X > 0.0 then
            return Pi / 2.0 - Atan (1.0 / X);
         else
            return -Pi / 2.0 - Atan (1.0 / X);
         end if;
      end if;

      -- Taylor series
      Term := XX;
      Sum := XX;
      for I in 1 .. 15 loop
         Term := -Term * XX * XX;
         Sum := Sum + Term / Float (2 * I + 1);
      end loop;

      return Sum;
   end Atan;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Float) return Float is
   begin
      return (Exp (X) - Exp (-X)) / 2.0;
   end Sinh;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Float) return Float is
   begin
      return (Exp (X) + Exp (-X)) / 2.0;
   end Cosh;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Float) return Float is
      E2X : constant Float := Exp (2.0 * X);
   begin
      return (E2X - 1.0) / (E2X + 1.0);
   end Tanh;

   ---------
   -- Pow --
   ---------

   function Pow (X, Y : Float) return Float is
   begin
      if X <= 0.0 then
         if X = 0.0 then
            if Y > 0.0 then
               return 0.0;
            else
               raise Constraint_Error;
            end if;
         else
            raise Constraint_Error;
         end if;
      end if;
      return Exp (Y * Log (X));
   end Pow;

   -- Long_Float versions

   function Sin (X : Long_Float) return Long_Float is
   begin
      return Long_Float (Sin (Float (X)));
   end Sin;

   function Cos (X : Long_Float) return Long_Float is
   begin
      return Long_Float (Cos (Float (X)));
   end Cos;

   function Tan (X : Long_Float) return Long_Float is
   begin
      return Long_Float (Tan (Float (X)));
   end Tan;

   function Exp (X : Long_Float) return Long_Float is
   begin
      return Long_Float (Exp (Float (X)));
   end Exp;

   function Log (X : Long_Float) return Long_Float is
   begin
      return Long_Float (Log (Float (X)));
   end Log;

   function Sqrt (X : Long_Float) return Long_Float is
   begin
      return Long_Float (Sqrt (Float (X)));
   end Sqrt;

   function Pow (X, Y : Long_Float) return Long_Float is
   begin
      return Long_Float (Pow (Float (X), Float (Y)));
   end Pow;

end Ada.Numerics.Aux;
