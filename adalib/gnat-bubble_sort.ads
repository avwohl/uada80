-- GNAT.Bubble_Sort for Z80
-- Generic bubble sort algorithm

package GNAT.Bubble_Sort is
   pragma Pure;

   -- Sort procedure
   generic
      with procedure Move (From, To : Natural);
      -- Move element From to position To

      with function Lt (Op1, Op2 : Natural) return Boolean;
      -- Return True if element Op1 < element Op2
   procedure Sort (N : Natural);
   -- Sort N elements (indices 1 .. N)

end GNAT.Bubble_Sort;
