-- GNAT.Heap_Sort for Z80
-- Generic heap sort algorithm

package GNAT.Heap_Sort is
   pragma Pure;

   -- Sort an array using heap sort
   generic
      with procedure Move (From, To : Natural);
      -- Move element From to position To

      with function Lt (Op1, Op2 : Natural) return Boolean;
      -- Return True if element Op1 < element Op2
   procedure Sort (N : Natural);
   -- Sort N elements (indices 1 .. N)

end GNAT.Heap_Sort;
