-- System.Exn_Flt for Z80
-- Float exponentiation

package System.Exn_Flt is
   pragma Pure;

   -- Float raised to integer power
   function Exn_Float (Left : Float; Right : Integer) return Float;
   -- Returns Left ** Right

end System.Exn_Flt;
