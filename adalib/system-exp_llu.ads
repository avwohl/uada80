-- System.Exp_LLU for Z80
-- Unsigned 64-bit exponentiation (32-bit on Z80)

package System.Exp_LLU is
   pragma Pure;

   type Long_Long_Unsigned is mod 2 ** 32;

   -- Exponentiation for Long_Long_Unsigned
   function Exp_Long_Long_Unsigned
     (Left  : Long_Long_Unsigned;
      Right : Natural) return Long_Long_Unsigned;

end System.Exp_LLU;
