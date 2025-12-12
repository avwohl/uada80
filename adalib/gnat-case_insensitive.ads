-- GNAT.Case_Insensitive for Z80
-- Case-insensitive comparison utilities

package GNAT.Case_Insensitive is
   pragma Pure;

   function Equal (Left, Right : String) return Boolean;
   --  Case-insensitive equality

   function Less (Left, Right : String) return Boolean;
   --  Case-insensitive less than

   function Hash (Key : String) return Natural;
   --  Case-insensitive hash

end GNAT.Case_Insensitive;
