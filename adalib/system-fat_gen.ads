-- System.Fat_Gen for Z80
-- Generic floating-point attribute functions

generic
   type T is digits <>;
package System.Fat_Gen is
   pragma Pure;

   -- Representation-oriented attributes
   function Adjacent (X, Towards : T) return T;
   function Ceiling (X : T) return T;
   function Compose (Fraction : T; Exponent : Integer) return T;
   function Copy_Sign (Value, Sign : T) return T;
   function Exponent (X : T) return Integer;
   function Floor (X : T) return T;
   function Fraction (X : T) return T;
   function Leading_Part (X : T; Radix_Digits : Integer) return T;
   function Machine (X : T) return T;
   function Machine_Rounding (X : T) return T;
   function Model (X : T) return T;
   function Pred (X : T) return T;
   function Remainder (X, Y : T) return T;
   function Rounding (X : T) return T;
   function Scaling (X : T; Adjustment : Integer) return T;
   function Succ (X : T) return T;
   function Truncation (X : T) return T;
   function Unbiased_Rounding (X : T) return T;

   -- Boolean predicates
   function Valid (X : T) return Boolean;

end System.Fat_Gen;
