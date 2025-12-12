-- Ada.Numerics.Aux for Z80
-- Auxiliary numeric operations

package Ada.Numerics.Aux is
   pragma Pure;

   -- Float operations
   function Sin (X : Float) return Float;
   function Cos (X : Float) return Float;
   function Tan (X : Float) return Float;
   function Exp (X : Float) return Float;
   function Log (X : Float) return Float;
   function Sqrt (X : Float) return Float;
   function Asin (X : Float) return Float;
   function Acos (X : Float) return Float;
   function Atan (X : Float) return Float;
   function Sinh (X : Float) return Float;
   function Cosh (X : Float) return Float;
   function Tanh (X : Float) return Float;
   function Pow (X, Y : Float) return Float;

   -- Long_Float operations
   function Sin (X : Long_Float) return Long_Float;
   function Cos (X : Long_Float) return Long_Float;
   function Tan (X : Long_Float) return Long_Float;
   function Exp (X : Long_Float) return Long_Float;
   function Log (X : Long_Float) return Long_Float;
   function Sqrt (X : Long_Float) return Long_Float;
   function Pow (X, Y : Long_Float) return Long_Float;

end Ada.Numerics.Aux;
