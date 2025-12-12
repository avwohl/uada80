-- System.Exp_Int for Z80
-- Integer exponentiation

package System.Exp_Int is
   pragma Pure;

   -- Integer exponentiation
   function Exp_Integer (Left : Integer; Right : Natural) return Integer;
   -- Returns Left ** Right

end System.Exp_Int;
