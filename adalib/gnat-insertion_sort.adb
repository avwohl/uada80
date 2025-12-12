-- GNAT.Insertion_Sort body for Z80
-- Generic insertion sort implementation

package body GNAT.Insertion_Sort is

   ----------
   -- Sort --
   ----------

   procedure Sort (A : in Out Array_Type) is
      Key : Element_Type;
      J   : Index_Type;
   begin
      if A'Length <= 1 then
         return;
      end if;

      for I in Index_Type'Succ (A'First) .. A'Last loop
         Key := A (I);
         J := I;

         while J > A'First and then Key < A (Index_Type'Pred (J)) loop
            A (J) := A (Index_Type'Pred (J));
            J := Index_Type'Pred (J);
         end loop;

         A (J) := Key;
      end loop;
   end Sort;

   ------------------
   -- Sort_Indexes --
   ------------------

   procedure Sort_Indexes (First, Last : Index_Type) is
      J : Index_Type;
   begin
      if Index_Type'Pos (First) >= Index_Type'Pos (Last) then
         return;
      end if;

      for I in Index_Type'Succ (First) .. Last loop
         J := I;

         while Index_Type'Pos (J) > Index_Type'Pos (First)
           and then Less (J, Index_Type'Pred (J))
         loop
            Move (J, Index_Type'Pred (J));
            J := Index_Type'Pred (J);
         end loop;
      end loop;
   end Sort_Indexes;

end GNAT.Insertion_Sort;
