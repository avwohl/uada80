-- GNAT.Fixed_Math for Z80
-- Fixed-point mathematics using 8.8 and 16.16 formats

package GNAT.Fixed_Math is
   pragma Pure;

   -- 8.8 fixed-point: 8 bits integer, 8 bits fraction
   -- Range: -128.0 to +127.996 (step 1/256)
   subtype Fixed_8_8 is Integer range -32768 .. 32767;

   -- 16.16 fixed-point: 16 bits integer, 16 bits fraction (uses Long_Integer)
   -- For Z80, we'll use scaled integers

   -- Scale factors
   Scale_8_8  : constant := 256;   -- 2^8
   Scale_16_16 : constant := 65536; -- 2^16

   -- Conversion to/from integers
   function To_Fixed_8_8 (Value : Integer) return Fixed_8_8;
   function From_Fixed_8_8 (Value : Fixed_8_8) return Integer;

   -- Conversion to/from fixed with decimal places
   function To_Fixed_8_8 (Int_Part : Integer; Frac_Hundredths : Natural) return Fixed_8_8;
   procedure From_Fixed_8_8 (Value : Fixed_8_8;
                              Int_Part : out Integer;
                              Frac_Hundredths : out Natural);

   -- Basic arithmetic (8.8)
   function Add (A, B : Fixed_8_8) return Fixed_8_8;
   function Sub (A, B : Fixed_8_8) return Fixed_8_8;
   function Mul (A, B : Fixed_8_8) return Fixed_8_8;
   function Div (A, B : Fixed_8_8) return Fixed_8_8;

   -- Operators
   function "+" (A, B : Fixed_8_8) return Fixed_8_8 renames Add;
   function "-" (A, B : Fixed_8_8) return Fixed_8_8 renames Sub;
   function "*" (A, B : Fixed_8_8) return Fixed_8_8 renames Mul;
   function "/" (A, B : Fixed_8_8) return Fixed_8_8 renames Div;

   -- Unary operations
   function Negate (A : Fixed_8_8) return Fixed_8_8;
   function Abs_Val (A : Fixed_8_8) return Fixed_8_8;

   -- Comparison
   function "<" (A, B : Fixed_8_8) return Boolean;
   function ">" (A, B : Fixed_8_8) return Boolean;
   function "<=" (A, B : Fixed_8_8) return Boolean;
   function ">=" (A, B : Fixed_8_8) return Boolean;

   -- Mathematical functions (8.8)
   function Sqrt (A : Fixed_8_8) return Fixed_8_8;
   function Square (A : Fixed_8_8) return Fixed_8_8;

   -- Trigonometric functions (angle in degrees * 256)
   -- Results scaled by 256 (so 256 = 1.0)
   function Sin (Angle : Fixed_8_8) return Fixed_8_8;
   function Cos (Angle : Fixed_8_8) return Fixed_8_8;
   function Tan (Angle : Fixed_8_8) return Fixed_8_8;

   -- Inverse trig (returns degrees * 256)
   function Atan2 (Y, X : Fixed_8_8) return Fixed_8_8;

   -- Exponential/logarithmic (approximations)
   function Exp (A : Fixed_8_8) return Fixed_8_8;
   function Log (A : Fixed_8_8) return Fixed_8_8;
   function Pow (Base, Exp : Fixed_8_8) return Fixed_8_8;

   -- Rounding
   function Floor (A : Fixed_8_8) return Fixed_8_8;
   function Ceil (A : Fixed_8_8) return Fixed_8_8;
   function Round (A : Fixed_8_8) return Fixed_8_8;
   function Trunc (A : Fixed_8_8) return Fixed_8_8;

   -- Fractional part
   function Frac (A : Fixed_8_8) return Fixed_8_8;

   -- Min/Max
   function Min (A, B : Fixed_8_8) return Fixed_8_8;
   function Max (A, B : Fixed_8_8) return Fixed_8_8;
   function Clamp (Value, Low, High : Fixed_8_8) return Fixed_8_8;

   -- Linear interpolation (T is 0-256, where 256 = 1.0)
   function Lerp (A, B : Fixed_8_8; T : Natural) return Fixed_8_8;

   -- String conversion
   function Image (A : Fixed_8_8) return String;
   function Image (A : Fixed_8_8; Decimals : Natural) return String;
   function Value (S : String) return Fixed_8_8;

   -- Constants (pre-scaled for 8.8)
   Pi      : constant Fixed_8_8 := 804;   -- 3.14159 * 256
   Half_Pi : constant Fixed_8_8 := 402;   -- Pi/2
   Two_Pi  : constant Fixed_8_8 := 1608;  -- 2*Pi
   E       : constant Fixed_8_8 := 696;   -- 2.71828 * 256
   Sqrt_2  : constant Fixed_8_8 := 362;   -- 1.41421 * 256
   One     : constant Fixed_8_8 := 256;   -- 1.0
   Half    : constant Fixed_8_8 := 128;   -- 0.5
   Zero    : constant Fixed_8_8 := 0;

   -- Degree/Radian conversion helpers
   function Deg_To_Rad (Degrees : Fixed_8_8) return Fixed_8_8;
   function Rad_To_Deg (Radians : Fixed_8_8) return Fixed_8_8;

end GNAT.Fixed_Math;
