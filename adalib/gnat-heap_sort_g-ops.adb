-- GNAT.Heap_Sort_G.Ops body for Z80
-- Additional heap sort operations implementation

package body GNAT.Heap_Sort_G.Ops is

   ------------
   -- Sort_N --
   ------------

   procedure Sort_N (N : Natural) is
   begin
      Sort (N);
   end Sort_N;

   ----------
   -- Sift --
   ----------

   procedure Sift (N : Natural; Idx : Positive) is
      C   : Positive := Idx;
      Son : Positive;
   begin
      Move (Idx, 0);

      loop
         Son := 2 * C;
         exit when Son > N;

         if Son < N and then Lt (Son, Son + 1) then
            Son := Son + 1;
         end if;

         exit when not Lt (C, Son);

         Move (Son, C);
         C := Son;
      end loop;

      Move (0, C);
   end Sift;

end GNAT.Heap_Sort_G.Ops;
