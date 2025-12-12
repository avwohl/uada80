-- System.Exn_LFlt body for Z80
-- Long_Float exponentiation implementation

package body System.Exn_LFlt is

   --------------------
   -- Exn_Long_Float --
   --------------------

   function Exn_Long_Float (Left : Long_Float; Right : Integer) return Long_Float is
      Result : Long_Float := 1.0;
      Base   : Long_Float := Left;
      Exp    : Natural;
   begin
      if Right = 0 then
         return 1.0;
      end if;

      if Left = 0.0 then
         if Right < 0 then
            raise Constraint_Error;
         end if;
         return 0.0;
      end if;

      if Right < 0 then
         Exp := Natural (-Right);
         Base := 1.0 / Left;
      else
         Exp := Natural (Right);
      end if;

      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Result * Base;
         end if;
         Base := Base * Base;
         Exp := Exp / 2;
      end loop;

      return Result;
   end Exn_Long_Float;

end System.Exn_LFlt;
