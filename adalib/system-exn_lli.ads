-- System.Exn_LLI for Z80
-- Long_Long_Integer exponentiation with overflow checking

package System.Exn_LLI is
   pragma Pure;

   -- Long_Long_Integer exponentiation with checked overflow
   function Exn_Long_Long_Integer
     (Left : Long_Long_Integer; Right : Natural) return Long_Long_Integer;
   -- Returns Left ** Right
   -- Raises Constraint_Error on overflow

end System.Exn_LLI;
