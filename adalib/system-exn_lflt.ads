-- System.Exn_LFlt for Z80
-- Long_Float exponentiation

package System.Exn_LFlt is
   pragma Pure;

   -- Long_Float raised to integer power
   function Exn_Long_Float (Left : Long_Float; Right : Integer) return Long_Float;

end System.Exn_LFlt;
