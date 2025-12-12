-- GNAT.Bubble_Sort_A body for Z80
-- Bubble sort implementation with access parameters

package body GNAT.Bubble_Sort_A is

   ----------
   -- Sort --
   ----------

   procedure Sort (N : Natural; Move : Move_Procedure; Lt : Lt_Function) is
      Swapped : Boolean;
   begin
      if N <= 1 then
         return;
      end if;

      loop
         Swapped := False;

         for J in 1 .. N - 1 loop
            if Lt (J + 1, J) then
               -- Swap elements J and J+1 using position 0 as temp
               Move (J, 0);
               Move (J + 1, J);
               Move (0, J + 1);
               Swapped := True;
            end if;
         end loop;

         exit when not Swapped;
      end loop;
   end Sort;

end GNAT.Bubble_Sort_A;
