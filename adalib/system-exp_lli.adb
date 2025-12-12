-- System.Exp_LLI body for Z80
-- Long_Long_Integer exponentiation implementation

package body System.Exp_LLI is

   ---------------------------
   -- Exp_Long_Long_Integer --
   ---------------------------

   function Exp_Long_Long_Integer
     (Left  : Long_Long_Integer;
      Right : Natural) return Long_Long_Integer
   is
      Result : Long_Long_Integer := 1;
      Base   : Long_Long_Integer := Left;
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
   end Exp_Long_Long_Integer;

end System.Exp_LLI;
