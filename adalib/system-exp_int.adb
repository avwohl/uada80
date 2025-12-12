-- System.Exp_Int body for Z80
-- Integer exponentiation implementation

package body System.Exp_Int is

   -----------------
   -- Exp_Integer --
   -----------------

   function Exp_Integer (Left : Integer; Right : Natural) return Integer is
      Result : Integer := 1;
      Base   : Integer := Left;
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
   end Exp_Integer;

end System.Exp_Int;
