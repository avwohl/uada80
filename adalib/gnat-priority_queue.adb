-- GNAT.Priority_Queue body for Z80
-- Generic priority queue implementation

package body GNAT.Priority_Queue is

   package body Bounded is

      procedure Sift_Up (Q : in Out Priority_Queue; Idx : Positive) is
         I    : Positive := Idx;
         P    : Positive;
         Temp : Element_Type;
      begin
         while I > 1 loop
            P := I / 2;
            if Priority (Q.Data (I)) < Priority (Q.Data (P)) then
               Temp := Q.Data (I);
               Q.Data (I) := Q.Data (P);
               Q.Data (P) := Temp;
               I := P;
            else
               exit;
            end if;
         end loop;
      end Sift_Up;

      procedure Sift_Down (Q : in Out Priority_Queue; Idx : Positive) is
         I     : Positive := Idx;
         L, R  : Natural;
         Min   : Positive;
         Temp  : Element_Type;
      begin
         loop
            L := I * 2;
            R := I * 2 + 1;
            Min := I;

            if L <= Q.Count and then
               Priority (Q.Data (L)) < Priority (Q.Data (Min))
            then
               Min := L;
            end if;

            if R <= Q.Count and then
               Priority (Q.Data (R)) < Priority (Q.Data (Min))
            then
               Min := R;
            end if;

            exit when Min = I;

            Temp := Q.Data (I);
            Q.Data (I) := Q.Data (Min);
            Q.Data (Min) := Temp;
            I := Min;
         end loop;
      end Sift_Down;

      -----------
      -- Clear --
      -----------

      procedure Clear (Q : out Priority_Queue) is
      begin
         Q.Count := 0;
      end Clear;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (Q : Priority_Queue) return Boolean is
      begin
         return Q.Count = 0;
      end Is_Empty;

      -------------
      -- Is_Full --
      -------------

      function Is_Full (Q : Priority_Queue) return Boolean is
      begin
         return Q.Count >= Max_Size;
      end Is_Full;

      ----------
      -- Size --
      ----------

      function Size (Q : Priority_Queue) return Natural is
      begin
         return Q.Count;
      end Size;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue (Q : in Out Priority_Queue; Item : Element_Type) is
      begin
         if Q.Count >= Max_Size then
            raise Constraint_Error with "priority queue overflow";
         end if;
         Q.Count := Q.Count + 1;
         Q.Data (Q.Count) := Item;
         Sift_Up (Q, Q.Count);
      end Enqueue;

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue (Q : in Out Priority_Queue; Item : out Element_Type) is
      begin
         if Q.Count = 0 then
            raise Constraint_Error with "priority queue is empty";
         end if;
         Item := Q.Data (1);
         Q.Data (1) := Q.Data (Q.Count);
         Q.Count := Q.Count - 1;
         if Q.Count > 0 then
            Sift_Down (Q, 1);
         end if;
      end Dequeue;

      -----------
      -- First --
      -----------

      function First (Q : Priority_Queue) return Element_Type is
      begin
         if Q.Count = 0 then
            raise Constraint_Error with "priority queue is empty";
         end if;
         return Q.Data (1);
      end First;

   end Bounded;

end GNAT.Priority_Queue;
