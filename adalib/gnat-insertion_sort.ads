-- GNAT.Insertion_Sort for Z80
-- Generic insertion sort (good for small arrays)

package GNAT.Insertion_Sort is
   pragma Pure;

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   procedure Sort (A : in out Array_Type);
   --  Sort array using insertion sort

   generic
      type Index_Type is (<>);
      with procedure Move (From, To : Index_Type);
      with function Less (Op1, Op2 : Index_Type) return Boolean;
   procedure Sort_Indexes (First, Last : Index_Type);
   --  Sort using index operations

end GNAT.Insertion_Sort;
