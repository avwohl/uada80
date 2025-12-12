-- Ada.Containers.Generic_Anonymous_Array_Sort body for Z80
-- Insertion sort using accessor procedures

procedure Ada.Containers.Generic_Anonymous_Array_Sort
  (First, Last : Index_Type'Base)
is
   J : Index_Type'Base;
begin
   if Last <= First then
      return;
   end if;

   -- Insertion sort using Less and Swap
   for I in Index_Type'Succ (First) .. Last loop
      J := I;

      while J > First and then Less (J, Index_Type'Pred (J)) loop
         Swap (J, Index_Type'Pred (J));
         J := Index_Type'Pred (J);
      end loop;
   end loop;
end Ada.Containers.Generic_Anonymous_Array_Sort;
