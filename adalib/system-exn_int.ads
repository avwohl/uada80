-- System.Exn_Int for Z80
-- Integer exponentiation with overflow checking

package System.Exn_Int is
   pragma Pure;

   -- Integer exponentiation with checked overflow
   function Exn_Integer (Left : Integer; Right : Natural) return Integer;
   -- Returns Left ** Right
   -- Raises Constraint_Error on overflow

end System.Exn_Int;
