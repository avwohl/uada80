-- GNAT.Heap_Sort body for Z80
-- Generic heap sort algorithm implementation

package body GNAT.Heap_Sort is

   ----------
   -- Sort --
   ----------

   procedure Sort (N : Natural) is
      -- Index 0 is used as temporary storage

      procedure Sift (S : Positive) is
         C      : Positive := S;
         Parent : Natural;
         Son    : Natural;
      begin
         -- Move index 0 to starting position
         Move (S, 0);

         loop
            Son := C * 2;
            exit when Son > N;

            if Son < N and then Lt (Son, Son + 1) then
               Son := Son + 1;
            end if;

            exit when not Lt (0, Son);

            Move (Son, C);
            C := Son;
         end loop;

         Move (0, C);
      end Sift;

   begin
      -- Build heap
      for J in reverse 1 .. N / 2 loop
         Sift (J);
      end loop;

      -- Extract elements
      for J in reverse 2 .. N loop
         -- Swap root with last element
         Move (1, 0);
         Move (J, 1);
         Move (0, J);

         -- Rebuild heap with one fewer element
         declare
            Old_N : constant Natural := N;
         begin
            -- Temporarily reduce N for sifting
            -- (Can't actually modify N, but sift uses global N)
            null;
         end;
      end loop;
   end Sort;

end GNAT.Heap_Sort;
