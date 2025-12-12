-- GNAT.Quick_Sort body for Z80
-- Generic quicksort implementation

package body GNAT.Quick_Sort is

   ----------
   -- Sort --
   ----------

   procedure Sort (A : in Out Array_Type) is

      procedure Quick (Lo, Hi : Index_Type) is
         Left   : Index_Type := Lo;
         Right  : Index_Type := Hi;
         Pivot  : Element_Type;
         Mid    : Index_Type;
         Temp   : Element_Type;
      begin
         if Index_Type'Pos (Lo) >= Index_Type'Pos (Hi) then
            return;
         end if;

         Mid := Index_Type'Val ((Index_Type'Pos (Lo) + Index_Type'Pos (Hi)) / 2);
         Pivot := A (Mid);

         while Index_Type'Pos (Left) <= Index_Type'Pos (Right) loop
            while A (Left) < Pivot loop
               Left := Index_Type'Succ (Left);
            end loop;

            while Pivot < A (Right) loop
               Right := Index_Type'Pred (Right);
            end loop;

            if Index_Type'Pos (Left) <= Index_Type'Pos (Right) then
               Temp := A (Left);
               A (Left) := A (Right);
               A (Right) := Temp;
               Left := Index_Type'Succ (Left);
               Right := Index_Type'Pred (Right);
            end if;
         end loop;

         if Index_Type'Pos (Lo) < Index_Type'Pos (Right) then
            Quick (Lo, Right);
         end if;

         if Index_Type'Pos (Left) < Index_Type'Pos (Hi) then
            Quick (Left, Hi);
         end if;
      end Quick;

   begin
      if A'Length <= 1 then
         return;
      end if;
      Quick (A'First, A'Last);
   end Sort;

   ------------------
   -- Sort_Indexes --
   ------------------

   procedure Sort_Indexes (First, Last : Index_Type) is

      procedure Quick (Lo, Hi : Index_Type) is
         Left  : Index_Type := Lo;
         Right : Index_Type := Hi;
         Mid   : Index_Type;
      begin
         if Index_Type'Pos (Lo) >= Index_Type'Pos (Hi) then
            return;
         end if;

         Mid := Index_Type'Val ((Index_Type'Pos (Lo) + Index_Type'Pos (Hi)) / 2);

         while Index_Type'Pos (Left) <= Index_Type'Pos (Right) loop
            while Less (Left, Mid) loop
               Left := Index_Type'Succ (Left);
            end loop;

            while Less (Mid, Right) loop
               Right := Index_Type'Pred (Right);
            end loop;

            if Index_Type'Pos (Left) <= Index_Type'Pos (Right) then
               Move (Left, Right);
               Left := Index_Type'Succ (Left);
               Right := Index_Type'Pred (Right);
            end if;
         end loop;

         if Index_Type'Pos (Lo) < Index_Type'Pos (Right) then
            Quick (Lo, Right);
         end if;

         if Index_Type'Pos (Left) < Index_Type'Pos (Hi) then
            Quick (Left, Hi);
         end if;
      end Quick;

   begin
      if Index_Type'Pos (First) >= Index_Type'Pos (Last) then
         return;
      end if;
      Quick (First, Last);
   end Sort_Indexes;

end GNAT.Quick_Sort;
