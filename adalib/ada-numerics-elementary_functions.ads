-- Ada.Numerics.Elementary_Functions for Z80
-- Generic package providing elementary mathematical functions

with Ada.Numerics;

generic
   type Float_Type is digits <>;
package Ada.Numerics.Elementary_Functions is
   pragma Pure;

   -- Trigonometric functions (radians)
   function Sin (X : Float_Type) return Float_Type;
   function Cos (X : Float_Type) return Float_Type;
   function Tan (X : Float_Type) return Float_Type;
   function Cot (X : Float_Type) return Float_Type;

   -- Inverse trigonometric functions
   function Arcsin (X : Float_Type) return Float_Type;
   function Arccos (X : Float_Type) return Float_Type;
   function Arctan (X : Float_Type) return Float_Type;
   function Arctan (Y : Float_Type; X : Float_Type) return Float_Type;
   function Arccot (X : Float_Type) return Float_Type;
   function Arccot (Y : Float_Type; X : Float_Type) return Float_Type;

   -- Hyperbolic functions
   function Sinh (X : Float_Type) return Float_Type;
   function Cosh (X : Float_Type) return Float_Type;
   function Tanh (X : Float_Type) return Float_Type;
   function Coth (X : Float_Type) return Float_Type;

   -- Inverse hyperbolic functions
   function Arcsinh (X : Float_Type) return Float_Type;
   function Arccosh (X : Float_Type) return Float_Type;
   function Arctanh (X : Float_Type) return Float_Type;
   function Arccoth (X : Float_Type) return Float_Type;

   -- Exponential and logarithmic functions
   function Exp (X : Float_Type) return Float_Type;
   function Log (X : Float_Type) return Float_Type;
   function Log (X : Float_Type; Base : Float_Type) return Float_Type;

   -- Power functions
   function "**" (Left : Float_Type; Right : Float_Type) return Float_Type;
   function "**" (Left : Float_Type; Right : Integer) return Float_Type;

   -- Square root
   function Sqrt (X : Float_Type) return Float_Type;

end Ada.Numerics.Elementary_Functions;
