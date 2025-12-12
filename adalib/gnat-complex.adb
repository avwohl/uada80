-- GNAT.Complex body for Z80
-- Complex number arithmetic implementation

package body GNAT.Complex is

   -- Approximate square root using Newton's method
   function Sqrt_Approx (X : Fixed_8_8) return Fixed_8_8 is
      Guess : Fixed_8_8;
      Prev  : Fixed_8_8;
   begin
      if X <= 0.0 then
         return 0.0;
      end if;

      Guess := X / 2.0;
      if Guess = 0.0 then
         Guess := 0.5;
      end if;

      for I in 1 .. 8 loop  -- 8 iterations for Z80
         Prev := Guess;
         Guess := (Guess + X / Guess) / 2.0;
         exit when abs (Guess - Prev) < 0.01;
      end loop;

      return Guess;
   end Sqrt_Approx;

   ----------
   -- Make --
   ----------

   function Make (Real, Imag : Fixed_8_8) return Complex is
   begin
      return (Re => Real, Im => Imag);
   end Make;

   function Make (Real : Fixed_8_8) return Complex is
   begin
      return (Re => Real, Im => 0.0);
   end Make;

   ---------------
   -- Real_Part --
   ---------------

   function Real_Part (C : Complex) return Fixed_8_8 is
   begin
      return C.Re;
   end Real_Part;

   ---------------
   -- Imag_Part --
   ---------------

   function Imag_Part (C : Complex) return Fixed_8_8 is
   begin
      return C.Im;
   end Imag_Part;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re + Right.Re, Im => Left.Im + Right.Im);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re - Right.Re, Im => Left.Im - Right.Im);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Complex) return Complex is
   begin
      -- (a+bi)(c+di) = (ac-bd) + (ad+bc)i
      return (Re => Left.Re * Right.Re - Left.Im * Right.Im,
              Im => Left.Re * Right.Im + Left.Im * Right.Re);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Complex) return Complex is
      Denom : Fixed_8_8;
   begin
      -- (a+bi)/(c+di) = (ac+bd)/(c²+d²) + (bc-ad)/(c²+d²)i
      Denom := Right.Re * Right.Re + Right.Im * Right.Im;

      if Denom = 0.0 then
         return Zero;  -- Division by zero
      end if;

      return (Re => (Left.Re * Right.Re + Left.Im * Right.Im) / Denom,
              Im => (Left.Im * Right.Re - Left.Re * Right.Im) / Denom);
   end "/";

   function "-" (C : Complex) return Complex is
   begin
      return (Re => -C.Re, Im => -C.Im);
   end "-";

   -- Scalar operations

   function "*" (Left : Fixed_8_8; Right : Complex) return Complex is
   begin
      return (Re => Left * Right.Re, Im => Left * Right.Im);
   end "*";

   function "*" (Left : Complex; Right : Fixed_8_8) return Complex is
   begin
      return (Re => Left.Re * Right, Im => Left.Im * Right);
   end "*";

   function "/" (Left : Complex; Right : Fixed_8_8) return Complex is
   begin
      if Right = 0.0 then
         return Zero;
      end if;
      return (Re => Left.Re / Right, Im => Left.Im / Right);
   end "/";

   ---------------
   -- Conjugate --
   ---------------

   function Conjugate (C : Complex) return Complex is
   begin
      return (Re => C.Re, Im => -C.Im);
   end Conjugate;

   ----------------------
   -- Modulus_Squared --
   ----------------------

   function Modulus_Squared (C : Complex) return Fixed_8_8 is
   begin
      return C.Re * C.Re + C.Im * C.Im;
   end Modulus_Squared;

   -------------
   -- Modulus --
   -------------

   function Modulus (C : Complex) return Fixed_8_8 is
   begin
      return Sqrt_Approx (Modulus_Squared (C));
   end Modulus;

   --------------
   -- Argument --
   --------------

   function Argument (C : Complex) return Fixed_8_8 is
      Pi : constant Fixed_8_8 := 3.14;
      Angle : Fixed_8_8;
   begin
      if C.Re = 0.0 and C.Im = 0.0 then
         return 0.0;
      end if;

      -- Approximate atan2 using simple quadrant logic
      if C.Re > 0.0 then
         if C.Im >= 0.0 then
            Angle := C.Im / (C.Re + Modulus (C));
         else
            Angle := -(-C.Im) / (C.Re + Modulus (C));
         end if;
      elsif C.Im >= 0.0 then
         Angle := Pi - C.Im / (-C.Re + Modulus (C));
      else
         Angle := -Pi + (-C.Im) / (-C.Re + Modulus (C));
      end if;

      return Angle;
   end Argument;

   -------------
   -- Is_Zero --
   -------------

   function Is_Zero (C : Complex) return Boolean is
   begin
      return C.Re = 0.0 and C.Im = 0.0;
   end Is_Zero;

   -------------
   -- Is_Real --
   -------------

   function Is_Real (C : Complex) return Boolean is
   begin
      return C.Im = 0.0;
   end Is_Real;

   ------------------
   -- Is_Imaginary --
   ------------------

   function Is_Imaginary (C : Complex) return Boolean is
   begin
      return C.Re = 0.0 and C.Im /= 0.0;
   end Is_Imaginary;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Complex) return Boolean is
   begin
      return Left.Re = Right.Re and Left.Im = Right.Im;
   end "=";

   ----------------
   -- From_Polar --
   ----------------

   function From_Polar (Modulus, Argument : Fixed_8_8) return Complex is
      -- Approximate sin/cos using Taylor series (first terms)
      Angle : constant Fixed_8_8 := Argument;
      Cos_A : Fixed_8_8;
      Sin_A : Fixed_8_8;
      A2    : Fixed_8_8;
   begin
      A2 := Angle * Angle;

      -- cos(x) ≈ 1 - x²/2
      Cos_A := 1.0 - A2 / 2.0;

      -- sin(x) ≈ x - x³/6
      Sin_A := Angle * (1.0 - A2 / 6.0);

      return (Re => Modulus * Cos_A, Im => Modulus * Sin_A);
   end From_Polar;

   ----------------
   -- Reciprocal --
   ----------------

   function Reciprocal (C : Complex) return Complex is
   begin
      return One / C;
   end Reciprocal;

   ------------
   -- Square --
   ------------

   function Square (C : Complex) return Complex is
   begin
      return C * C;
   end Square;

end GNAT.Complex;
