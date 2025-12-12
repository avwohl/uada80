-- System.Exp_Gen body for Z80
-- Generic exponentiation using binary algorithm

package body System.Exp_Gen is

   ---------
   -- Exp --
   ---------

   function Exp (Base : T; Exponent : Natural) return T is
      Result : T := One;
      B      : T := Base;
      E      : Natural := Exponent;
   begin
      -- Binary exponentiation algorithm
      while E > 0 loop
         if E mod 2 = 1 then
            Result := Result * B;
         end if;
         E := E / 2;
         if E > 0 then
            B := B * B;
         end if;
      end loop;
      return Result;
   end Exp;

end System.Exp_Gen;
