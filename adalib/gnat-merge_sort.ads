-- GNAT.Merge_Sort for Z80
-- Generic merge sort (stable sort)

package GNAT.Merge_Sort is
   pragma Pure;

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   procedure Sort (A : in Out Array_Type);
   --  Sort array using merge sort (stable)

end GNAT.Merge_Sort;
