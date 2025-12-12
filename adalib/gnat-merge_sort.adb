-- GNAT.Merge_Sort body for Z80
-- Generic merge sort implementation

package body GNAT.Merge_Sort is

   ----------
   -- Sort --
   ----------

   procedure Sort (A : in Out Array_Type) is
      -- Use in-place merge sort with temporary storage
      -- Limited temp size for Z80 memory constraints

      Max_Temp : constant := 64;  -- Max temp array size for Z80

      procedure Merge (Lo, Mid, Hi : Index_Type) is
         Temp : Array_Type (1 .. Index_Type'Val (Max_Temp));
         I    : Index_Type := Lo;
         J    : Index_Type := Index_Type'Succ (Mid);
         K    : Natural := 1;
         Len  : constant Natural :=
           Index_Type'Pos (Hi) - Index_Type'Pos (Lo) + 1;
      begin
         if Len > Max_Temp then
            -- Fall back to simple merge for large arrays
            while Index_Type'Pos (I) <= Index_Type'Pos (Mid)
              and Index_Type'Pos (J) <= Index_Type'Pos (Hi)
            loop
               if not (A (J) < A (I)) then
                  I := Index_Type'Succ (I);
               else
                  -- Rotate elements
                  declare
                     T : constant Element_Type := A (J);
                     M : Index_Type := J;
                  begin
                     while Index_Type'Pos (M) > Index_Type'Pos (I) loop
                        A (M) := A (Index_Type'Pred (M));
                        M := Index_Type'Pred (M);
                     end loop;
                     A (I) := T;
                  end;
                  I := Index_Type'Succ (I);
                  J := Index_Type'Succ (J);
               end if;
            end loop;
         else
            -- Use temp array for efficiency
            while Index_Type'Pos (I) <= Index_Type'Pos (Mid)
              and Index_Type'Pos (J) <= Index_Type'Pos (Hi)
            loop
               if not (A (J) < A (I)) then
                  Temp (Index_Type'Val (K)) := A (I);
                  I := Index_Type'Succ (I);
               else
                  Temp (Index_Type'Val (K)) := A (J);
                  J := Index_Type'Succ (J);
               end if;
               K := K + 1;
            end loop;

            while Index_Type'Pos (I) <= Index_Type'Pos (Mid) loop
               Temp (Index_Type'Val (K)) := A (I);
               I := Index_Type'Succ (I);
               K := K + 1;
            end loop;

            while Index_Type'Pos (J) <= Index_Type'Pos (Hi) loop
               Temp (Index_Type'Val (K)) := A (J);
               J := Index_Type'Succ (J);
               K := K + 1;
            end loop;

            -- Copy back
            for N in 1 .. K - 1 loop
               A (Index_Type'Val (Index_Type'Pos (Lo) + N - 1)) :=
                 Temp (Index_Type'Val (N));
            end loop;
         end if;
      end Merge;

      procedure Merge_Sort (Lo, Hi : Index_Type) is
         Mid : Index_Type;
      begin
         if Index_Type'Pos (Lo) >= Index_Type'Pos (Hi) then
            return;
         end if;

         Mid := Index_Type'Val ((Index_Type'Pos (Lo) + Index_Type'Pos (Hi)) / 2);

         Merge_Sort (Lo, Mid);
         Merge_Sort (Index_Type'Succ (Mid), Hi);
         Merge (Lo, Mid, Hi);
      end Merge_Sort;

   begin
      if A'Length <= 1 then
         return;
      end if;
      Merge_Sort (A'First, A'Last);
   end Sort;

end GNAT.Merge_Sort;
