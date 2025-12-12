-- System.Powten_Flt body for Z80
-- Power of 10 computation implementation

package body System.Powten_Flt is

   -----------
   -- Pow10 --
   -----------

   function Pow10 (N : Integer) return Float is
      Result : Float := 1.0;
      Abs_N  : Natural;
   begin
      if N = 0 then
         return 1.0;
      end if;

      Abs_N := abs N;

      -- Use precomputed table when possible
      if Abs_N <= Powten'Last then
         if N > 0 then
            return Powten (Abs_N);
         else
            return 1.0 / Powten (Abs_N);
         end if;
      end if;

      -- Compute larger powers iteratively
      if N > 0 then
         for I in 1 .. Abs_N loop
            Result := Result * 10.0;
         end loop;
      else
         for I in 1 .. Abs_N loop
            Result := Result / 10.0;
         end loop;
      end if;

      return Result;
   end Pow10;

end System.Powten_Flt;
