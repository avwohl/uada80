-- System.Exp_LLI for Z80
-- Long_Long_Integer exponentiation

package System.Exp_LLI is
   pragma Pure;

   -- Long_Long_Integer exponentiation
   function Exp_Long_Long_Integer
     (Left  : Long_Long_Integer;
      Right : Natural) return Long_Long_Integer;
   -- Returns Left ** Right

end System.Exp_LLI;
