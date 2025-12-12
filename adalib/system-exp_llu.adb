-- System.Exp_LLU body for Z80
-- Unsigned 64-bit exponentiation (32-bit on Z80)

package body System.Exp_LLU is

   ----------------------------
   -- Exp_Long_Long_Unsigned --
   ----------------------------

   function Exp_Long_Long_Unsigned
     (Left  : Long_Long_Unsigned;
      Right : Natural) return Long_Long_Unsigned
   is
      Result : Long_Long_Unsigned := 1;
      Base   : Long_Long_Unsigned := Left;
      Exp    : Natural := Right;
   begin
      -- Binary exponentiation
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Result * Base;
         end if;
         Exp := Exp / 2;
         if Exp > 0 then
            Base := Base * Base;
         end if;
      end loop;

      return Result;
   end Exp_Long_Long_Unsigned;

end System.Exp_LLU;
