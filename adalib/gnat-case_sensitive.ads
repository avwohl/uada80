-- GNAT.Case_Sensitive for Z80
-- Case-sensitive comparison utilities

package GNAT.Case_Sensitive is
   pragma Pure;

   function Equal (Left, Right : String) return Boolean;
   --  Case-sensitive equality

   function Less (Left, Right : String) return Boolean;
   --  Case-sensitive less than

   function Hash (Key : String) return Natural;
   --  Case-sensitive hash

end GNAT.Case_Sensitive;
