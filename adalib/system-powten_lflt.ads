-- System.Powten_LFlt for Z80
-- Power of 10 computation for Long_Float

package System.Powten_LFlt is
   pragma Pure;

   -- Compute 10.0 ** N for Long_Float
   function Pow10 (N : Integer) return Long_Float;

   -- Precomputed powers of 10 (higher precision)
   Powten : constant array (0 .. 15) of Long_Float :=
     (1.0, 10.0, 100.0, 1_000.0, 10_000.0,
      100_000.0, 1_000_000.0, 10_000_000.0,
      100_000_000.0, 1_000_000_000.0, 10_000_000_000.0,
      100_000_000_000.0, 1_000_000_000_000.0,
      10_000_000_000_000.0, 100_000_000_000_000.0,
      1_000_000_000_000_000.0);

end System.Powten_LFlt;
