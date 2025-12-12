-- System.Exn_LLD for Z80
-- Long_Long_Integer exponentiation (32-bit on Z80)

package System.Exn_LLD is
   pragma Pure;

   type Long_Long_Integer is range -2 ** 31 .. 2 ** 31 - 1;

   -- Exponentiation for Long_Long_Integer with checked overflow
   function Exn_Long_Long_Integer
     (Left  : Long_Long_Integer;
      Right : Natural) return Long_Long_Integer;

end System.Exn_LLD;
