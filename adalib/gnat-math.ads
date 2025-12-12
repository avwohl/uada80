-- GNAT.Math for Z80
-- Basic math utilities

package GNAT.Math is
   pragma Pure;

   function Sign (X : Integer) return Integer;
   --  Return -1, 0, or 1 depending on sign of X

   function Sign (X : Float) return Float;
   --  Return -1.0, 0.0, or 1.0 depending on sign of X

   function Abs_Value (X : Integer) return Integer;
   --  Absolute value of Integer

   function Abs_Value (X : Float) return Float;
   --  Absolute value of Float

   function Min (A, B : Integer) return Integer;
   --  Minimum of two Integers

   function Max (A, B : Integer) return Integer;
   --  Maximum of two Integers

   function Min (A, B : Float) return Float;
   --  Minimum of two Floats

   function Max (A, B : Float) return Float;
   --  Maximum of two Floats

   function Clamp
     (Value : Integer;
      Low   : Integer;
      High  : Integer) return Integer;
   --  Clamp Value to [Low, High]

   function Clamp
     (Value : Float;
      Low   : Float;
      High  : Float) return Float;
   --  Clamp Value to [Low, High]

   function GCD (A, B : Natural) return Natural;
   --  Greatest common divisor

   function LCM (A, B : Positive) return Positive;
   --  Least common multiple

end GNAT.Math;
