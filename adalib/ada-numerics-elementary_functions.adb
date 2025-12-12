-- Ada.Numerics.Elementary_Functions body for Z80
-- Elementary math function implementations

with System;

package body Ada.Numerics.Elementary_Functions is

   -- Runtime math functions (from float48.asm / z88dk)
   function Rt_Sin (X : Float_Type) return Float_Type;
   pragma Import (C, Rt_Sin, "_FSIN48");

   function Rt_Cos (X : Float_Type) return Float_Type;
   pragma Import (C, Rt_Cos, "_FCOS48");

   function Rt_Tan (X : Float_Type) return Float_Type;
   pragma Import (C, Rt_Tan, "_FTAN48");

   function Rt_Exp (X : Float_Type) return Float_Type;
   pragma Import (C, Rt_Exp, "_FEXP48");

   function Rt_Log (X : Float_Type) return Float_Type;
   pragma Import (C, Rt_Log, "_FLOG48");

   function Rt_Sqrt (X : Float_Type) return Float_Type;
   pragma Import (C, Rt_Sqrt, "_FSQRT48");

   -- Pi constant for internal calculations
   Local_Pi : constant Float_Type := 3.14159_26535_89793;

   ---------
   -- Sin --
   ---------

   function Sin (X : Float_Type) return Float_Type is
   begin
      return Rt_Sin (X);
   end Sin;

   ---------
   -- Cos --
   ---------

   function Cos (X : Float_Type) return Float_Type is
   begin
      return Rt_Cos (X);
   end Cos;

   ---------
   -- Tan --
   ---------

   function Tan (X : Float_Type) return Float_Type is
   begin
      return Rt_Tan (X);
   end Tan;

   ---------
   -- Cot --
   ---------

   function Cot (X : Float_Type) return Float_Type is
      T : Float_Type := Tan (X);
   begin
      if T = 0.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      return 1.0 / T;
   end Cot;

   ------------
   -- Arcsin --
   ------------

   function Arcsin (X : Float_Type) return Float_Type is
   begin
      -- arcsin(x) = arctan(x / sqrt(1 - x^2))
      if X < -1.0 or X > 1.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      if X = 1.0 then
         return Local_Pi / 2.0;
      elsif X = -1.0 then
         return -Local_Pi / 2.0;
      end if;
      return Arctan (X / Sqrt (1.0 - X * X));
   end Arcsin;

   ------------
   -- Arccos --
   ------------

   function Arccos (X : Float_Type) return Float_Type is
   begin
      if X < -1.0 or X > 1.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      return Local_Pi / 2.0 - Arcsin (X);
   end Arccos;

   ------------
   -- Arctan --
   ------------

   function Arctan (X : Float_Type) return Float_Type is
      -- Simple polynomial approximation for arctan
      -- Valid for |x| <= 1
      X2 : Float_Type;
      Result : Float_Type;
      Reduced : Float_Type := X;
      Negate : Boolean := False;
      Add_Pi_2 : Boolean := False;
   begin
      -- Handle negative
      if X < 0.0 then
         Reduced := -X;
         Negate := True;
      end if;

      -- Reduce to [0, 1] range
      if Reduced > 1.0 then
         Reduced := 1.0 / Reduced;
         Add_Pi_2 := True;
      end if;

      -- Polynomial approximation
      X2 := Reduced * Reduced;
      Result := Reduced * (1.0 - X2 * (0.3333333 - X2 * (0.2 - X2 * 0.1428571)));

      if Add_Pi_2 then
         Result := Local_Pi / 2.0 - Result;
      end if;

      if Negate then
         Result := -Result;
      end if;

      return Result;
   end Arctan;

   function Arctan (Y : Float_Type; X : Float_Type) return Float_Type is
   begin
      if X > 0.0 then
         return Arctan (Y / X);
      elsif X < 0.0 then
         if Y >= 0.0 then
            return Arctan (Y / X) + Local_Pi;
         else
            return Arctan (Y / X) - Local_Pi;
         end if;
      else
         -- X = 0
         if Y > 0.0 then
            return Local_Pi / 2.0;
         elsif Y < 0.0 then
            return -Local_Pi / 2.0;
         else
            raise Ada.Numerics.Argument_Error;
         end if;
      end if;
   end Arctan;

   ------------
   -- Arccot --
   ------------

   function Arccot (X : Float_Type) return Float_Type is
   begin
      return Local_Pi / 2.0 - Arctan (X);
   end Arccot;

   function Arccot (Y : Float_Type; X : Float_Type) return Float_Type is
   begin
      return Local_Pi / 2.0 - Arctan (Y, X);
   end Arccot;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Float_Type) return Float_Type is
      E_X : Float_Type := Exp (X);
   begin
      return (E_X - 1.0 / E_X) / 2.0;
   end Sinh;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Float_Type) return Float_Type is
      E_X : Float_Type := Exp (X);
   begin
      return (E_X + 1.0 / E_X) / 2.0;
   end Cosh;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Float_Type) return Float_Type is
      E_2X : Float_Type := Exp (2.0 * X);
   begin
      return (E_2X - 1.0) / (E_2X + 1.0);
   end Tanh;

   ----------
   -- Coth --
   ----------

   function Coth (X : Float_Type) return Float_Type is
      T : Float_Type := Tanh (X);
   begin
      if T = 0.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      return 1.0 / T;
   end Coth;

   -------------
   -- Arcsinh --
   -------------

   function Arcsinh (X : Float_Type) return Float_Type is
   begin
      -- arcsinh(x) = ln(x + sqrt(x^2 + 1))
      return Log (X + Sqrt (X * X + 1.0));
   end Arcsinh;

   -------------
   -- Arccosh --
   -------------

   function Arccosh (X : Float_Type) return Float_Type is
   begin
      if X < 1.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      -- arccosh(x) = ln(x + sqrt(x^2 - 1))
      return Log (X + Sqrt (X * X - 1.0));
   end Arccosh;

   -------------
   -- Arctanh --
   -------------

   function Arctanh (X : Float_Type) return Float_Type is
   begin
      if X <= -1.0 or X >= 1.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      -- arctanh(x) = 0.5 * ln((1 + x) / (1 - x))
      return 0.5 * Log ((1.0 + X) / (1.0 - X));
   end Arctanh;

   -------------
   -- Arccoth --
   -------------

   function Arccoth (X : Float_Type) return Float_Type is
   begin
      if X >= -1.0 and X <= 1.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      -- arccoth(x) = 0.5 * ln((x + 1) / (x - 1))
      return 0.5 * Log ((X + 1.0) / (X - 1.0));
   end Arccoth;

   ---------
   -- Exp --
   ---------

   function Exp (X : Float_Type) return Float_Type is
   begin
      return Rt_Exp (X);
   end Exp;

   ---------
   -- Log --
   ---------

   function Log (X : Float_Type) return Float_Type is
   begin
      if X <= 0.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      return Rt_Log (X);
   end Log;

   function Log (X : Float_Type; Base : Float_Type) return Float_Type is
   begin
      if X <= 0.0 or Base <= 0.0 or Base = 1.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      -- log_base(x) = ln(x) / ln(base)
      return Log (X) / Log (Base);
   end Log;

   ----------
   -- "**" --
   ----------

   function "**" (Left : Float_Type; Right : Float_Type) return Float_Type is
   begin
      if Left < 0.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      if Left = 0.0 then
         if Right <= 0.0 then
            raise Ada.Numerics.Argument_Error;
         end if;
         return 0.0;
      end if;
      -- x^y = e^(y * ln(x))
      return Exp (Right * Log (Left));
   end "**";

   function "**" (Left : Float_Type; Right : Integer) return Float_Type is
      Result : Float_Type := 1.0;
      Base   : Float_Type := Left;
      N      : Integer := Right;
   begin
      if N < 0 then
         Base := 1.0 / Base;
         N := -N;
      end if;

      while N > 0 loop
         if N mod 2 = 1 then
            Result := Result * Base;
         end if;
         Base := Base * Base;
         N := N / 2;
      end loop;

      return Result;
   end "**";

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Float_Type) return Float_Type is
   begin
      if X < 0.0 then
         raise Ada.Numerics.Argument_Error;
      end if;
      return Rt_Sqrt (X);
   end Sqrt;

end Ada.Numerics.Elementary_Functions;
