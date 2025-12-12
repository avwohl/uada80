-- GNAT.Bubble_Sort body for Z80
-- Generic bubble sort algorithm implementation

package body GNAT.Bubble_Sort is

   ----------
   -- Sort --
   ----------

   procedure Sort (N : Natural) is
      Swapped : Boolean;
   begin
      if N <= 1 then
         return;
      end if;

      loop
         Swapped := False;

         for I in 1 .. N - 1 loop
            if Lt (I + 1, I) then
               -- Swap using index 0 as temp
               Move (I, 0);
               Move (I + 1, I);
               Move (0, I + 1);
               Swapped := True;
            end if;
         end loop;

         exit when not Swapped;
      end loop;
   end Sort;

end GNAT.Bubble_Sort;
