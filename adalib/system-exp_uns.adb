-- System.Exp_Uns body for Z80
-- Unsigned exponentiation implementation

package body System.Exp_Uns is

   use Interfaces;

   ------------------
   -- Exp_Unsigned --
   ------------------

   function Exp_Unsigned
     (Left  : Unsigned_32;
      Right : Natural) return Unsigned_32
   is
      Result : Unsigned_32 := 1;
      Base   : Unsigned_32 := Left;
      Exp    : Natural := Right;
   begin
      -- Use binary exponentiation for efficiency
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Result * Base;
         end if;
         Base := Base * Base;
         Exp := Exp / 2;
      end loop;
      return Result;
   end Exp_Unsigned;

end System.Exp_Uns;
