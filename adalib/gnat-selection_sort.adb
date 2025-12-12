-- GNAT.Selection_Sort body for Z80
-- Generic selection sort implementation

package body GNAT.Selection_Sort is

   ----------
   -- Sort --
   ----------

   procedure Sort (A : in Out Array_Type) is
      Min_Idx : Index_Type;
      Temp    : Element_Type;
   begin
      if A'Length <= 1 then
         return;
      end if;

      for I in A'First .. Index_Type'Pred (A'Last) loop
         Min_Idx := I;

         for J in Index_Type'Succ (I) .. A'Last loop
            if A (J) < A (Min_Idx) then
               Min_Idx := J;
            end if;
         end loop;

         if Min_Idx /= I then
            Temp := A (I);
            A (I) := A (Min_Idx);
            A (Min_Idx) := Temp;
         end if;
      end loop;
   end Sort;

end GNAT.Selection_Sort;
