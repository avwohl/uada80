-- System.Exn_Int body for Z80
-- Integer exponentiation implementation

package body System.Exn_Int is

   -----------------
   -- Exn_Integer --
   -----------------

   function Exn_Integer (Left : Integer; Right : Natural) return Integer is
      Result : Integer := 1;
      Base   : Integer := Left;
      Exp    : Natural := Right;
   begin
      if Right = 0 then
         return 1;
      end if;

      if Left = 0 then
         return 0;
      end if;

      if Left = 1 then
         return 1;
      end if;

      if Left = -1 then
         if Right mod 2 = 0 then
            return 1;
         else
            return -1;
         end if;
      end if;

      -- Binary exponentiation with overflow check
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            -- Check for overflow before multiplication
            if Base > 0 and Result > 0 then
               if Result > Integer'Last / Base then
                  raise Constraint_Error;
               end if;
            elsif Base < 0 and Result < 0 then
               if Result < Integer'Last / Base then
                  raise Constraint_Error;
               end if;
            elsif Base < 0 and Result > 0 then
               if Result > Integer'First / Base then
                  raise Constraint_Error;
               end if;
            elsif Base > 0 and Result < 0 then
               if Result < Integer'First / Base then
                  raise Constraint_Error;
               end if;
            end if;

            Result := Result * Base;
         end if;

         Exp := Exp / 2;

         if Exp > 0 then
            -- Check for overflow in Base * Base
            if abs Base > Integer'Last / abs Base then
               if Exp = 1 and (Exp mod 2 = 0) then
                  null;  -- Last iteration, won't use Base
               else
                  raise Constraint_Error;
               end if;
            else
               Base := Base * Base;
            end if;
         end if;
      end loop;

      return Result;
   end Exn_Integer;

end System.Exn_Int;
