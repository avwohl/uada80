-- System.Fat_Flt for Z80
-- Float type attributes support

package System.Fat_Flt is
   pragma Pure;

   -- Float representation parameters
   Float_Mantissa_Size : constant := 24;  -- Typical IEEE single precision
   Float_Exponent_Size : constant := 8;

   -- Attribute functions for Float
   function Adjacent (X : Float; Towards : Float) return Float;
   function Ceiling (X : Float) return Float;
   function Compose (Fraction : Float; Exponent : Integer) return Float;
   function Copy_Sign (Value, Sign : Float) return Float;
   function Exponent (X : Float) return Integer;
   function Floor (X : Float) return Float;
   function Fraction (X : Float) return Float;
   function Leading_Part (X : Float; Radix_Digits : Integer) return Float;
   function Machine (X : Float) return Float;
   function Machine_Rounding (X : Float) return Float;
   function Model (X : Float) return Float;
   function Pred (X : Float) return Float;
   function Remainder (X, Y : Float) return Float;
   function Rounding (X : Float) return Float;
   function Scaling (X : Float; Adjustment : Integer) return Float;
   function Succ (X : Float) return Float;
   function Truncation (X : Float) return Float;
   function Unbiased_Rounding (X : Float) return Float;

   -- Machine constants
   function Machine_Radix return Integer;
   function Machine_Mantissa return Integer;
   function Machine_Emin return Integer;
   function Machine_Emax return Integer;

end System.Fat_Flt;
