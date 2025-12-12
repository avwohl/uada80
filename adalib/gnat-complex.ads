-- GNAT.Complex for Z80
-- Complex number arithmetic using fixed-point (8.8)

package GNAT.Complex is
   pragma Pure;

   -- Using 8.8 fixed-point format for Z80
   type Fixed_8_8 is delta 1.0 / 256.0 range -128.0 .. 127.996;

   type Complex is record
      Re : Fixed_8_8;  -- Real part
      Im : Fixed_8_8;  -- Imaginary part
   end record;

   Zero : constant Complex := (Re => 0.0, Im => 0.0);
   One  : constant Complex := (Re => 1.0, Im => 0.0);
   I    : constant Complex := (Re => 0.0, Im => 1.0);  -- Imaginary unit

   function Make (Real, Imag : Fixed_8_8) return Complex;
   function Make (Real : Fixed_8_8) return Complex;  -- Pure real

   function Real_Part (C : Complex) return Fixed_8_8;
   function Imag_Part (C : Complex) return Fixed_8_8;

   -- Arithmetic
   function "+" (Left, Right : Complex) return Complex;
   function "-" (Left, Right : Complex) return Complex;
   function "*" (Left, Right : Complex) return Complex;
   function "/" (Left, Right : Complex) return Complex;
   function "-" (C : Complex) return Complex;  -- Unary minus

   -- Scalar operations
   function "*" (Left : Fixed_8_8; Right : Complex) return Complex;
   function "*" (Left : Complex; Right : Fixed_8_8) return Complex;
   function "/" (Left : Complex; Right : Fixed_8_8) return Complex;

   -- Properties
   function Conjugate (C : Complex) return Complex;
   function Modulus_Squared (C : Complex) return Fixed_8_8;
   function Modulus (C : Complex) return Fixed_8_8;  -- Approximate
   function Argument (C : Complex) return Fixed_8_8;  -- Approximate angle

   -- Predicates
   function Is_Zero (C : Complex) return Boolean;
   function Is_Real (C : Complex) return Boolean;
   function Is_Imaginary (C : Complex) return Boolean;

   -- Comparison
   function "=" (Left, Right : Complex) return Boolean;

   -- Conversion from polar form (approximate)
   function From_Polar (Modulus, Argument : Fixed_8_8) return Complex;

   -- Common operations
   function Reciprocal (C : Complex) return Complex;
   function Square (C : Complex) return Complex;

private

   pragma Inline (Make);
   pragma Inline (Real_Part);
   pragma Inline (Imag_Part);
   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline (Conjugate);

end GNAT.Complex;
