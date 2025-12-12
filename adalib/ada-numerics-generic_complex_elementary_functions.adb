-- Ada.Numerics.Generic_Complex_Elementary_Functions body for Z80
-- Complex elementary mathematical functions implementation

with Ada.Numerics.Elementary_Functions;

package body Ada.Numerics.Generic_Complex_Elementary_Functions is

   package Elem is new Ada.Numerics.Elementary_Functions (Real);

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Complex) return Complex is
      R   : constant Real'Base := Modulus (X);
      Arg : constant Real'Base := Argument (X) / 2.0;
   begin
      return Compose_From_Polar (Elem.Sqrt (R), Arg);
   end Sqrt;

   ---------
   -- Log --
   ---------

   function Log (X : Complex) return Complex is
   begin
      return (Re => Elem.Log (Modulus (X)),
              Im => Argument (X));
   end Log;

   ---------
   -- Exp --
   ---------

   function Exp (X : Complex) return Complex is
      E_Re : constant Real'Base := Elem.Exp (Re (X));
   begin
      return (Re => E_Re * Elem.Cos (Im (X)),
              Im => E_Re * Elem.Sin (Im (X)));
   end Exp;

   function Exp (X : Imaginary) return Complex is
      Im_X : constant Real'Base := Real'Base (Im (X));
   begin
      return (Re => Elem.Cos (Im_X),
              Im => Elem.Sin (Im_X));
   end Exp;

   ----------
   -- "**" --
   ----------

   function "**" (Left : Complex; Right : Complex) return Complex is
   begin
      if Modulus (Left) = 0.0 then
         return (Re => 0.0, Im => 0.0);
      end if;
      return Exp (Right * Log (Left));
   end "**";

   function "**" (Left : Complex; Right : Real'Base) return Complex is
   begin
      if Modulus (Left) = 0.0 then
         return (Re => 0.0, Im => 0.0);
      end if;
      return Compose_From_Polar
        (Elem."**" (Modulus (Left), Right),
         Argument (Left) * Right);
   end "**";

   function "**" (Left : Real'Base; Right : Complex) return Complex is
   begin
      if Left = 0.0 then
         return (Re => 0.0, Im => 0.0);
      end if;
      return Exp (Right * (Re => Elem.Log (Left), Im => 0.0));
   end "**";

   ---------
   -- Sin --
   ---------

   function Sin (X : Complex) return Complex is
      -- sin(a+bi) = sin(a)cosh(b) + i*cos(a)sinh(b)
   begin
      return (Re => Elem.Sin (Re (X)) * Elem.Cosh (Im (X)),
              Im => Elem.Cos (Re (X)) * Elem.Sinh (Im (X)));
   end Sin;

   ---------
   -- Cos --
   ---------

   function Cos (X : Complex) return Complex is
      -- cos(a+bi) = cos(a)cosh(b) - i*sin(a)sinh(b)
   begin
      return (Re => Elem.Cos (Re (X)) * Elem.Cosh (Im (X)),
              Im => -Elem.Sin (Re (X)) * Elem.Sinh (Im (X)));
   end Cos;

   ---------
   -- Tan --
   ---------

   function Tan (X : Complex) return Complex is
   begin
      return Sin (X) / Cos (X);
   end Tan;

   ---------
   -- Cot --
   ---------

   function Cot (X : Complex) return Complex is
   begin
      return Cos (X) / Sin (X);
   end Cot;

   ------------
   -- Arcsin --
   ------------

   function Arcsin (X : Complex) return Complex is
      -- arcsin(z) = -i * log(iz + sqrt(1-z^2))
      Z_Sq   : constant Complex := X * X;
      One    : constant Complex := (Re => 1.0, Im => 0.0);
      Sqrt_P : constant Complex := Sqrt (One - Z_Sq);
      I_X    : constant Complex := (Re => -Im (X), Im => Re (X));  -- i*X
   begin
      return (Re => Im (Log (I_X + Sqrt_P)),
              Im => -Re (Log (I_X + Sqrt_P)));
   end Arcsin;

   ------------
   -- Arccos --
   ------------

   function Arccos (X : Complex) return Complex is
      -- arccos(z) = -i * log(z + sqrt(z^2 - 1))
      Z_Sq   : constant Complex := X * X;
      One    : constant Complex := (Re => 1.0, Im => 0.0);
      Sqrt_P : constant Complex := Sqrt (Z_Sq - One);
   begin
      return (Re => Im (Log (X + Sqrt_P)),
              Im => -Re (Log (X + Sqrt_P)));
   end Arccos;

   ------------
   -- Arctan --
   ------------

   function Arctan (X : Complex) return Complex is
      -- arctan(z) = (i/2) * log((1-iz)/(1+iz))
      I      : constant Complex := (Re => 0.0, Im => 1.0);
      One    : constant Complex := (Re => 1.0, Im => 0.0);
      I_X    : constant Complex := I * X;
      Num    : constant Complex := One - I_X;
      Den    : constant Complex := One + I_X;
      Log_R  : constant Complex := Log (Num / Den);
   begin
      return (Re => -Im (Log_R) / 2.0, Im => Re (Log_R) / 2.0);
   end Arctan;

   ------------
   -- Arccot --
   ------------

   function Arccot (X : Complex) return Complex is
      One : constant Complex := (Re => 1.0, Im => 0.0);
   begin
      return Arctan (One / X);
   end Arccot;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Complex) return Complex is
      -- sinh(a+bi) = sinh(a)cos(b) + i*cosh(a)sin(b)
   begin
      return (Re => Elem.Sinh (Re (X)) * Elem.Cos (Im (X)),
              Im => Elem.Cosh (Re (X)) * Elem.Sin (Im (X)));
   end Sinh;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Complex) return Complex is
      -- cosh(a+bi) = cosh(a)cos(b) + i*sinh(a)sin(b)
   begin
      return (Re => Elem.Cosh (Re (X)) * Elem.Cos (Im (X)),
              Im => Elem.Sinh (Re (X)) * Elem.Sin (Im (X)));
   end Cosh;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Complex) return Complex is
   begin
      return Sinh (X) / Cosh (X);
   end Tanh;

   ----------
   -- Coth --
   ----------

   function Coth (X : Complex) return Complex is
   begin
      return Cosh (X) / Sinh (X);
   end Coth;

   -------------
   -- Arcsinh --
   -------------

   function Arcsinh (X : Complex) return Complex is
      -- arcsinh(z) = log(z + sqrt(z^2 + 1))
      Z_Sq   : constant Complex := X * X;
      One    : constant Complex := (Re => 1.0, Im => 0.0);
      Sqrt_P : constant Complex := Sqrt (Z_Sq + One);
   begin
      return Log (X + Sqrt_P);
   end Arcsinh;

   -------------
   -- Arccosh --
   -------------

   function Arccosh (X : Complex) return Complex is
      -- arccosh(z) = log(z + sqrt(z^2 - 1))
      Z_Sq   : constant Complex := X * X;
      One    : constant Complex := (Re => 1.0, Im => 0.0);
      Sqrt_P : constant Complex := Sqrt (Z_Sq - One);
   begin
      return Log (X + Sqrt_P);
   end Arccosh;

   -------------
   -- Arctanh --
   -------------

   function Arctanh (X : Complex) return Complex is
      -- arctanh(z) = (1/2) * log((1+z)/(1-z))
      One : constant Complex := (Re => 1.0, Im => 0.0);
      Num : constant Complex := One + X;
      Den : constant Complex := One - X;
   begin
      return Log (Num / Den) / 2.0;
   end Arctanh;

   -------------
   -- Arccoth --
   -------------

   function Arccoth (X : Complex) return Complex is
      One : constant Complex := (Re => 1.0, Im => 0.0);
   begin
      return Arctanh (One / X);
   end Arccoth;

end Ada.Numerics.Generic_Complex_Elementary_Functions;
