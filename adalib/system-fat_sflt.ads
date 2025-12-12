-- System.Fat_Sflt for Z80
-- Short_Float type attributes support
-- Note: On Z80 Short_Float is same as Float (32-bit)

package System.Fat_Sflt is
   pragma Pure;

   Short_Float_Mantissa_Size : constant := 24;

   function Adjacent (X : Short_Float; Towards : Short_Float) return Short_Float;
   function Ceiling (X : Short_Float) return Short_Float;
   function Compose (Fraction : Short_Float; Exponent : Integer) return Short_Float;
   function Copy_Sign (Value, Sign : Short_Float) return Short_Float;
   function Exponent (X : Short_Float) return Integer;
   function Floor (X : Short_Float) return Short_Float;
   function Fraction (X : Short_Float) return Short_Float;
   function Leading_Part (X : Short_Float; Radix_Digits : Integer) return Short_Float;
   function Machine (X : Short_Float) return Short_Float;
   function Machine_Rounding (X : Short_Float) return Short_Float;
   function Model (X : Short_Float) return Short_Float;
   function Pred (X : Short_Float) return Short_Float;
   function Remainder (X, Y : Short_Float) return Short_Float;
   function Rounding (X : Short_Float) return Short_Float;
   function Scaling (X : Short_Float; Adjustment : Integer) return Short_Float;
   function Succ (X : Short_Float) return Short_Float;
   function Truncation (X : Short_Float) return Short_Float;
   function Unbiased_Rounding (X : Short_Float) return Short_Float;

   function Machine_Radix return Integer;
   function Machine_Mantissa return Integer;
   function Machine_Emin return Integer;
   function Machine_Emax return Integer;

end System.Fat_Sflt;
