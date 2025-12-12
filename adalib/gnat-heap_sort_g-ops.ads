-- GNAT.Heap_Sort_G.Ops for Z80
-- Additional heap sort operations

generic
package GNAT.Heap_Sort_G.Ops is

   procedure Sort_N (N : Natural);
   --  Sort N elements (same as Sort but different name)

   procedure Sift (N : Natural; Idx : Positive);
   --  Sift element at Idx down in heap of size N

end GNAT.Heap_Sort_G.Ops;
