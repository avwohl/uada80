-- System.Exp_Uns for Z80
-- Unsigned exponentiation

with Interfaces;

package System.Exp_Uns is
   pragma Pure;

   -- Unsigned exponentiation
   function Exp_Unsigned
     (Left  : Interfaces.Unsigned_32;
      Right : Natural) return Interfaces.Unsigned_32;
   -- Returns Left ** Right

end System.Exp_Uns;
