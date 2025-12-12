-- System.Exp_Mod for Z80
-- Modular exponentiation

with Interfaces;

package System.Exp_Mod is
   pragma Pure;

   -- Modular exponentiation for Unsigned_32
   function Exp_Modular
     (Left    : Interfaces.Unsigned_32;
      Right   : Natural;
      Modulus : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   -- Returns (Left ** Right) mod Modulus

   -- Modular exponentiation for Unsigned_16
   function Exp_Modular_16
     (Left    : Interfaces.Unsigned_16;
      Right   : Natural;
      Modulus : Interfaces.Unsigned_16) return Interfaces.Unsigned_16;

   -- Modular exponentiation for Unsigned_8
   function Exp_Modular_8
     (Left    : Interfaces.Unsigned_8;
      Right   : Natural;
      Modulus : Interfaces.Unsigned_8) return Interfaces.Unsigned_8;

end System.Exp_Mod;
