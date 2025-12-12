-- GNAT.Selection_Sort for Z80
-- Generic selection sort

package GNAT.Selection_Sort is
   pragma Pure;

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   procedure Sort (A : in Out Array_Type);
   --  Sort array using selection sort

end GNAT.Selection_Sort;
