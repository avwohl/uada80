-- System.Fat_LFlt for Z80
-- Long_Float type attributes support

package System.Fat_LFlt is
   pragma Pure;

   Long_Float_Mantissa_Size : constant := 40;  -- Z88DK 48-bit float mantissa

   function Adjacent (X : Long_Float; Towards : Long_Float) return Long_Float;
   function Ceiling (X : Long_Float) return Long_Float;
   function Compose (Fraction : Long_Float; Exponent : Integer) return Long_Float;
   function Copy_Sign (Value, Sign : Long_Float) return Long_Float;
   function Exponent (X : Long_Float) return Integer;
   function Floor (X : Long_Float) return Long_Float;
   function Fraction (X : Long_Float) return Long_Float;
   function Leading_Part (X : Long_Float; Radix_Digits : Integer) return Long_Float;
   function Machine (X : Long_Float) return Long_Float;
   function Machine_Rounding (X : Long_Float) return Long_Float;
   function Model (X : Long_Float) return Long_Float;
   function Pred (X : Long_Float) return Long_Float;
   function Remainder (X, Y : Long_Float) return Long_Float;
   function Rounding (X : Long_Float) return Long_Float;
   function Scaling (X : Long_Float; Adjustment : Integer) return Long_Float;
   function Succ (X : Long_Float) return Long_Float;
   function Truncation (X : Long_Float) return Long_Float;
   function Unbiased_Rounding (X : Long_Float) return Long_Float;

   function Machine_Radix return Integer;
   function Machine_Mantissa return Integer;
   function Machine_Emin return Integer;
   function Machine_Emax return Integer;

end System.Fat_LFlt;
