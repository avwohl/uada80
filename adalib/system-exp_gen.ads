-- System.Exp_Gen for Z80
-- Generic exponentiation using binary algorithm

generic
   type T is private;
   One : T;
   with function "*" (L, R : T) return T is <>;
package System.Exp_Gen is
   pragma Pure;

   -- Compute Base ** Exponent using binary exponentiation
   function Exp (Base : T; Exponent : Natural) return T;

end System.Exp_Gen;
