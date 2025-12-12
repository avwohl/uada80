-- GNAT.Heap_Sort_A body for Z80
-- Heap sort implementation with access parameters

package body GNAT.Heap_Sort_A is

   ----------
   -- Sort --
   ----------

   procedure Sort (N : Natural; Move : Move_Procedure; Lt : Lt_Function) is

      procedure Sift (S : Positive) is
         C      : Positive := S;
         Parent : Positive;
         Son    : Positive;
      begin
         -- Save element at position S to position 0
         Move (S, 0);

         loop
            Son := 2 * C;
            exit when Son > N;

            -- Choose larger son
            if Son < N and then Lt (Son, Son + 1) then
               Son := Son + 1;
            end if;

            -- If parent >= son, we're done
            exit when not Lt (C, Son);

            -- Move son to parent position
            Move (Son, C);
            C := Son;
         end loop;

         -- Put saved element in final position
         Move (0, C);
      end Sift;

   begin
      if N <= 1 then
         return;
      end if;

      -- Build the heap
      for J in reverse 1 .. N / 2 loop
         Sift (J);
      end loop;

      -- Extract elements from heap
      for J in reverse 2 .. N loop
         -- Swap first and J-th elements
         Move (1, 0);
         Move (J, 1);
         Move (0, J);

         -- Restore heap property for 1 .. J-1
         declare
            C      : Positive := 1;
            Son    : Positive;
            Limit  : constant Natural := J - 1;
         begin
            Move (1, 0);
            loop
               Son := 2 * C;
               exit when Son > Limit;

               if Son < Limit and then Lt (Son, Son + 1) then
                  Son := Son + 1;
               end if;

               exit when not Lt (C, Son);

               Move (Son, C);
               C := Son;
            end loop;
            Move (0, C);
         end;
      end loop;
   end Sort;

end GNAT.Heap_Sort_A;
