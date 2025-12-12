-- GNAT.Bubble_Sort_G body for Z80
-- Generic bubble sort implementation

package body GNAT.Bubble_Sort_G is

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

         for J in 1 .. N - 1 loop
            if Lt (J + 1, J) then
               Move (J, 0);
               Move (J + 1, J);
               Move (0, J + 1);
               Swapped := True;
            end if;
         end loop;

         exit when not Swapped;
      end loop;
   end Sort;

end GNAT.Bubble_Sort_G;
