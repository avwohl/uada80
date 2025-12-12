-- GNAT.Queue body for Z80
-- Generic queue data structure implementation

package body GNAT.Queue is

   package body Bounded is

      -----------
      -- Clear --
      -----------

      procedure Clear (Q : out Queue) is
      begin
         Q.Head := 1;
         Q.Tail := 0;
         Q.Count := 0;
      end Clear;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (Q : Queue) return Boolean is
      begin
         return Q.Count = 0;
      end Is_Empty;

      -------------
      -- Is_Full --
      -------------

      function Is_Full (Q : Queue) return Boolean is
      begin
         return Q.Count >= Max_Size;
      end Is_Full;

      ----------
      -- Size --
      ----------

      function Size (Q : Queue) return Natural is
      begin
         return Q.Count;
      end Size;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue (Q : in Out Queue; Item : Element_Type) is
      begin
         if Q.Count >= Max_Size then
            raise Constraint_Error with "queue overflow";
         end if;
         Q.Tail := Q.Tail + 1;
         if Q.Tail > Max_Size then
            Q.Tail := 1;
         end if;
         Q.Data (Q.Tail) := Item;
         Q.Count := Q.Count + 1;
      end Enqueue;

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue (Q : in Out Queue; Item : out Element_Type) is
      begin
         if Q.Count = 0 then
            raise Constraint_Error with "queue underflow";
         end if;
         Item := Q.Data (Q.Head);
         Q.Head := Q.Head + 1;
         if Q.Head > Max_Size then
            Q.Head := 1;
         end if;
         Q.Count := Q.Count - 1;
      end Dequeue;

      -----------
      -- Front --
      -----------

      function Front (Q : Queue) return Element_Type is
      begin
         if Q.Count = 0 then
            raise Constraint_Error with "queue is empty";
         end if;
         return Q.Data (Q.Head);
      end Front;

      ----------
      -- Back --
      ----------

      function Back (Q : Queue) return Element_Type is
      begin
         if Q.Count = 0 then
            raise Constraint_Error with "queue is empty";
         end if;
         return Q.Data (Q.Tail);
      end Back;

   end Bounded;

end GNAT.Queue;
