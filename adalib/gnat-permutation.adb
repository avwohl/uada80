-- GNAT.Permutation body for Z80
-- Permutation generation implementation

package body GNAT.Permutation is

   ----------------------
   -- Next_Permutation --
   ----------------------

   function Next_Permutation (A : in Out Array_Type) return Boolean is
      I, J : Index_Type;
      Temp : Element_Type;
   begin
      if A'Length <= 1 then
         return False;
      end if;

      -- Find largest i where A(i) < A(i+1)
      I := Index_Type'Pred (A'Last);
      while I >= A'First loop
         exit when A (I) < A (Index_Type'Succ (I));
         if I = A'First then
            -- Last permutation - reverse to first
            declare
               L : Index_Type := A'First;
               R : Index_Type := A'Last;
            begin
               while L < R loop
                  Temp := A (L);
                  A (L) := A (R);
                  A (R) := Temp;
                  L := Index_Type'Succ (L);
                  R := Index_Type'Pred (R);
               end loop;
            end;
            return False;
         end if;
         I := Index_Type'Pred (I);
      end loop;

      -- Find largest j where A(i) < A(j)
      J := A'Last;
      while A (I) >= A (J) loop
         J := Index_Type'Pred (J);
      end loop;

      -- Swap A(i) and A(j)
      Temp := A (I);
      A (I) := A (J);
      A (J) := Temp;

      -- Reverse A(i+1) to A(last)
      declare
         L : Index_Type := Index_Type'Succ (I);
         R : Index_Type := A'Last;
      begin
         while L < R loop
            Temp := A (L);
            A (L) := A (R);
            A (R) := Temp;
            L := Index_Type'Succ (L);
            R := Index_Type'Pred (R);
         end loop;
      end;

      return True;
   end Next_Permutation;

   ---------------------------
   -- Sort_For_Permutation --
   ---------------------------

   procedure Sort_For_Permutation (A : in Out Array_Type) is
      Min_Idx : Index_Type;
      Temp    : Element_Type;
   begin
      -- Simple selection sort
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
   end Sort_For_Permutation;

   ---------------
   -- Factorial --
   ---------------

   function Factorial (N : Natural) return Natural is
      Result : Natural := 1;
   begin
      for I in 2 .. N loop
         Result := Result * I;
      end loop;
      return Result;
   end Factorial;

end GNAT.Permutation;
