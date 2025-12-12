-- Ada.Containers.Generic_Array_Sort body for Z80
-- Insertion sort implementation

procedure Ada.Containers.Generic_Array_Sort (Container : in Out Array_Type) is
   Temp : Element_Type;
   J    : Index_Type;
begin
   if Container'Length <= 1 then
      return;
   end if;

   -- Insertion sort
   for I in Index_Type'Succ (Container'First) .. Container'Last loop
      Temp := Container (I);
      J := I;

      -- Shift elements to the right until correct position found
      while J > Container'First and then Container (Index_Type'Pred (J)) > Temp loop
         Container (J) := Container (Index_Type'Pred (J));
         J := Index_Type'Pred (J);
      end loop;

      Container (J) := Temp;
   end loop;
end Ada.Containers.Generic_Array_Sort;
