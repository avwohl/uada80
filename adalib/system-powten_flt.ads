-- System.Powten_Flt for Z80
-- Power of 10 computation for Float

package System.Powten_Flt is
   pragma Pure;

   -- Compute 10.0 ** N for Float
   function Pow10 (N : Integer) return Float;
   -- Returns 10.0 ** N
   -- N should be in reasonable range for Float precision

   -- Precomputed powers of 10
   Powten : constant array (0 .. 10) of Float :=
     (1.0, 10.0, 100.0, 1_000.0, 10_000.0,
      100_000.0, 1_000_000.0, 10_000_000.0,
      100_000_000.0, 1_000_000_000.0, 10_000_000_000.0);

end System.Powten_Flt;
