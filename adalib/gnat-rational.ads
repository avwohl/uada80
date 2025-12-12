-- GNAT.Rational for Z80
-- Rational number arithmetic

package GNAT.Rational is
   pragma Pure;

   type Rational is private;

   Zero : constant Rational;
   One  : constant Rational;

   function Make (Numerator : Integer; Denominator : Integer := 1) return Rational;
   --  Create rational (automatically reduces)

   function Numerator (R : Rational) return Integer;
   function Denominator (R : Rational) return Integer;

   -- Arithmetic
   function "+" (Left, Right : Rational) return Rational;
   function "-" (Left, Right : Rational) return Rational;
   function "*" (Left, Right : Rational) return Rational;
   function "/" (Left, Right : Rational) return Rational;
   function "-" (R : Rational) return Rational;  -- Unary minus
   function "abs" (R : Rational) return Rational;

   -- Integer arithmetic
   function "+" (Left : Rational; Right : Integer) return Rational;
   function "+" (Left : Integer; Right : Rational) return Rational;
   function "-" (Left : Rational; Right : Integer) return Rational;
   function "-" (Left : Integer; Right : Rational) return Rational;
   function "*" (Left : Rational; Right : Integer) return Rational;
   function "*" (Left : Integer; Right : Rational) return Rational;
   function "/" (Left : Rational; Right : Integer) return Rational;
   function "/" (Left : Integer; Right : Rational) return Rational;

   -- Comparison
   function "=" (Left, Right : Rational) return Boolean;
   function "<" (Left, Right : Rational) return Boolean;
   function ">" (Left, Right : Rational) return Boolean;
   function "<=" (Left, Right : Rational) return Boolean;
   function ">=" (Left, Right : Rational) return Boolean;

   -- Predicates
   function Is_Zero (R : Rational) return Boolean;
   function Is_Positive (R : Rational) return Boolean;
   function Is_Negative (R : Rational) return Boolean;
   function Is_Integer (R : Rational) return Boolean;

   -- Conversion
   function To_Integer (R : Rational) return Integer;  -- Truncates
   function Round (R : Rational) return Integer;
   function Floor (R : Rational) return Integer;
   function Ceiling (R : Rational) return Integer;

   -- Fixed-point conversion (8.8 format for Z80)
   function To_Fixed_8_8 (R : Rational) return Integer;
   function From_Fixed_8_8 (F : Integer) return Rational;

   -- String conversion
   function Image (R : Rational) return String;

   -- Utilities
   function Reciprocal (R : Rational) return Rational;
   function GCD (A, B : Integer) return Integer;
   function LCM (A, B : Integer) return Integer;

private

   type Rational is record
      Num : Integer := 0;
      Den : Integer := 1;
   end record;

   Zero : constant Rational := (Num => 0, Den => 1);
   One  : constant Rational := (Num => 1, Den => 1);

end GNAT.Rational;
