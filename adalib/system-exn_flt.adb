-- System.Exn_Flt body for Z80
-- Float exponentiation implementation

package body System.Exn_Flt is

   ---------------
   -- Exn_Float --
   ---------------

   function Exn_Float (Left : Float; Right : Integer) return Float is
      Result : Float := 1.0;
      Base   : Float := Left;
      Exp    : Natural;
   begin
      if Right = 0 then
         return 1.0;
      end if;

      if Left = 0.0 then
         if Right < 0 then
            raise Constraint_Error;  -- 0 ** negative
         end if;
         return 0.0;
      end if;

      if Right < 0 then
         -- X ** (-N) = 1 / (X ** N)
         Exp := Natural (-Right);
         Base := 1.0 / Left;
      else
         Exp := Natural (Right);
      end if;

      -- Binary exponentiation
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Result * Base;
         end if;
         Base := Base * Base;
         Exp := Exp / 2;
      end loop;

      return Result;
   end Exn_Float;

end System.Exn_Flt;
