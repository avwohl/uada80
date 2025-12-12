-- GNAT.Bubble_Sort_G for Z80
-- Generic bubble sort

generic
   with procedure Move (From, To : Natural);
   with function Lt (Op1, Op2 : Natural) return Boolean;
package GNAT.Bubble_Sort_G is
   pragma Preelaborate;

   procedure Sort (N : Natural);
   --  Sort N elements using bubble sort algorithm
   --  Element 0 is used as temporary storage
   --  Elements 1 .. N are sorted

end GNAT.Bubble_Sort_G;
