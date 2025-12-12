-- GNAT.Heap_Sort_A for Z80
-- Heap sort with access-to-procedure parameters

package GNAT.Heap_Sort_A is
   pragma Preelaborate;

   type Move_Procedure is access procedure (From, To : Natural);
   type Lt_Function is access function (Op1, Op2 : Natural) return Boolean;

   procedure Sort (N : Natural; Move : Move_Procedure; Lt : Lt_Function);
   --  Sort N elements using heap sort algorithm
   --  Element 0 is used as temporary storage
   --  Elements 1 .. N are sorted

end GNAT.Heap_Sort_A;
