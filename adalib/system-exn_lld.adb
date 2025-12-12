-- System.Exn_LLD body for Z80
-- Long_Long_Integer exponentiation (32-bit on Z80)

package body System.Exn_LLD is

   ----------------------------
   -- Exn_Long_Long_Integer --
   ----------------------------

   function Exn_Long_Long_Integer
     (Left  : Long_Long_Integer;
      Right : Natural) return Long_Long_Integer
   is
      Result : Long_Long_Integer := 1;
      Base   : Long_Long_Integer := Left;
      Exp    : Natural := Right;
   begin
      -- Handle special cases
      if Exp = 0 then
         return 1;
      end if;

      if Left = 0 then
         return 0;
      end if;

      if Left = 1 then
         return 1;
      end if;

      if Left = -1 then
         if Exp mod 2 = 0 then
            return 1;
         else
            return -1;
         end if;
      end if;

      -- Binary exponentiation with overflow checking
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            -- Check for overflow before multiplication
            if Result > 0 and then Base > 0 then
               if Result > Long_Long_Integer'Last / Base then
                  raise Constraint_Error;
               end if;
            elsif Result < 0 and then Base < 0 then
               if Result < Long_Long_Integer'Last / Base then
                  raise Constraint_Error;
               end if;
            elsif Result > 0 and then Base < 0 then
               if Base < Long_Long_Integer'First / Result then
                  raise Constraint_Error;
               end if;
            elsif Result < 0 and then Base > 0 then
               if Result < Long_Long_Integer'First / Base then
                  raise Constraint_Error;
               end if;
            end if;

            Result := Result * Base;
         end if;

         Exp := Exp / 2;

         if Exp > 0 then
            -- Check for overflow before squaring
            if Base > 0 then
               if Base > 46340 then  -- sqrt(2^31-1) approximately
                  raise Constraint_Error;
               end if;
            else
               if Base < -46340 then
                  raise Constraint_Error;
               end if;
            end if;

            Base := Base * Base;
         end if;
      end loop;

      return Result;
   end Exn_Long_Long_Integer;

end System.Exn_LLD;
