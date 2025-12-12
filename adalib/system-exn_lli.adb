-- System.Exn_LLI body for Z80
-- Long_Long_Integer exponentiation implementation

package body System.Exn_LLI is

   ---------------------------
   -- Exn_Long_Long_Integer --
   ---------------------------

   function Exn_Long_Long_Integer
     (Left : Long_Long_Integer; Right : Natural) return Long_Long_Integer
   is
      Result : Long_Long_Integer := 1;
      Base   : Long_Long_Integer := Left;
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

      -- Binary exponentiation
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            -- Overflow check
            if Base > 0 and Result > 0 then
               if Result > Long_Long_Integer'Last / Base then
                  raise Constraint_Error;
               end if;
            elsif Base < 0 and Result < 0 then
               if Result < Long_Long_Integer'Last / Base then
                  raise Constraint_Error;
               end if;
            elsif Base < 0 and Result > 0 then
               if Result > Long_Long_Integer'First / Base then
                  raise Constraint_Error;
               end if;
            elsif Base > 0 and Result < 0 then
               if Result < Long_Long_Integer'First / Base then
                  raise Constraint_Error;
               end if;
            end if;

            Result := Result * Base;
         end if;

         Exp := Exp / 2;

         if Exp > 0 then
            if abs Base > Long_Long_Integer'Last / abs Base then
               raise Constraint_Error;
            end if;
            Base := Base * Base;
         end if;
      end loop;

      return Result;
   end Exn_Long_Long_Integer;

end System.Exn_LLI;
