-- GNAT.Heap_Sort_G body for Z80
-- Generic heap sort (alternative interface) implementation

package body GNAT.Heap_Sort_G is

   ----------
   -- Sort --
   ----------

   procedure Sort (Data : in Out Element_Array) is
      procedure Sift_Down (Start, Stop : Index) is
         Root : Index := Start;
         Child : Index;
         Temp : Element;
      begin
         while Index'Pos (Root) * 2 + 1 - Index'Pos (Data'First) <=
               Index'Pos (Stop) - Index'Pos (Data'First)
         loop
            Child := Index'Val (Index'Pos (Root) * 2 + 1 -
                                Index'Pos (Data'First) + Index'Pos (Data'First));

            -- Use larger child
            if Child < Stop and then Data (Child) < Data (Index'Succ (Child)) then
               Child := Index'Succ (Child);
            end if;

            if Data (Root) < Data (Child) then
               -- Swap
               Temp := Data (Root);
               Data (Root) := Data (Child);
               Data (Child) := Temp;
               Root := Child;
            else
               return;
            end if;
         end loop;
      end Sift_Down;

      Temp : Element;
   begin
      if Data'Length <= 1 then
         return;
      end if;

      -- Build heap
      for I in reverse Data'First ..
               Index'Val ((Index'Pos (Data'Last) - Index'Pos (Data'First)) / 2 +
                         Index'Pos (Data'First))
      loop
         Sift_Down (I, Data'Last);
      end loop;

      -- Extract elements
      for I in reverse Index'Succ (Data'First) .. Data'Last loop
         -- Swap root with last
         Temp := Data (Data'First);
         Data (Data'First) := Data (I);
         Data (I) := Temp;

         -- Restore heap
         Sift_Down (Data'First, Index'Pred (I));
      end loop;
   end Sort;

end GNAT.Heap_Sort_G;
