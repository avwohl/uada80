-- GNAT.Heap_Sort_G for Z80
-- Generic heap sort (alternative interface)

generic
   type Element is private;
   type Index is (<>);
   type Element_Array is array (Index range <>) of Element;
   with function "<" (Left, Right : Element) return Boolean is <>;
package GNAT.Heap_Sort_G is
   pragma Pure;

   -- Sort array in place
   procedure Sort (Data : in Out Element_Array);

end GNAT.Heap_Sort_G;
