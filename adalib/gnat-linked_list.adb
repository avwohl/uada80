-- GNAT.Linked_List body for Z80
-- Bounded singly linked list implementation

package body GNAT.Linked_List is

   function Allocate_Node (L : in Out List; Value : Integer) return Node_Index is
   begin
      for I in 1 .. Max_Nodes loop
         if not L.Nodes (I).In_Use then
            L.Nodes (I) := (Value => Value, Next => Null_Node, In_Use => True);
            return Node_Index (I);
         end if;
      end loop;
      return Null_Node;
   end Allocate_Node;

   procedure Free_Node (L : in Out List; Idx : Node_Index) is
   begin
      if Idx /= Null_Node then
         L.Nodes (Positive (Idx)).In_Use := False;
         L.Nodes (Positive (Idx)).Next := Null_Node;
      end if;
   end Free_Node;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (L : out List) is
   begin
      L.Nodes := (others => (Value => 0, Next => Null_Node, In_Use => False));
      L.Head := Null_Node;
      L.Tail := Null_Node;
      L.Count := 0;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List) return Boolean is
   begin
      return L.Head = Null_Node;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List) return Natural is
   begin
      return L.Count;
   end Length;

   -----------
   -- First --
   -----------

   function First (L : List) return Integer is
   begin
      if L.Head = Null_Node then
         return 0;
      end if;
      return L.Nodes (Positive (L.Head)).Value;
   end First;

   ----------
   -- Last --
   ----------

   function Last (L : List) return Integer is
   begin
      if L.Tail = Null_Node then
         return 0;
      end if;
      return L.Nodes (Positive (L.Tail)).Value;
   end Last;

   -------------
   -- Element --
   -------------

   function Element (L : List; Index : Positive) return Integer is
      Current : Node_Index := L.Head;
      I       : Positive := 1;
   begin
      while Current /= Null_Node loop
         if I = Index then
            return L.Nodes (Positive (Current)).Value;
         end if;
         Current := L.Nodes (Positive (Current)).Next;
         I := I + 1;
      end loop;
      return 0;
   end Element;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (L : in Out List; Value : Integer) is
      New_Node : constant Node_Index := Allocate_Node (L, Value);
   begin
      if New_Node = Null_Node then
         return;
      end if;

      L.Nodes (Positive (New_Node)).Next := L.Head;
      L.Head := New_Node;

      if L.Tail = Null_Node then
         L.Tail := New_Node;
      end if;

      L.Count := L.Count + 1;
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append (L : in Out List; Value : Integer) is
      New_Node : constant Node_Index := Allocate_Node (L, Value);
   begin
      if New_Node = Null_Node then
         return;
      end if;

      if L.Tail = Null_Node then
         L.Head := New_Node;
         L.Tail := New_Node;
      else
         L.Nodes (Positive (L.Tail)).Next := New_Node;
         L.Tail := New_Node;
      end if;

      L.Count := L.Count + 1;
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert (L : in Out List; Before : Positive; Value : Integer) is
      New_Node : Node_Index;
      Current  : Node_Index;
      Prev     : Node_Index := Null_Node;
      I        : Positive := 1;
   begin
      if Before = 1 then
         Prepend (L, Value);
         return;
      end if;

      New_Node := Allocate_Node (L, Value);
      if New_Node = Null_Node then
         return;
      end if;

      Current := L.Head;
      while Current /= Null_Node and I < Before loop
         Prev := Current;
         Current := L.Nodes (Positive (Current)).Next;
         I := I + 1;
      end loop;

      if Prev /= Null_Node then
         L.Nodes (Positive (New_Node)).Next := Current;
         L.Nodes (Positive (Prev)).Next := New_Node;
         L.Count := L.Count + 1;
      else
         Free_Node (L, New_Node);
      end if;
   end Insert;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (L : in Out List) is
      Old_Head : Node_Index;
   begin
      if L.Head = Null_Node then
         return;
      end if;

      Old_Head := L.Head;
      L.Head := L.Nodes (Positive (L.Head)).Next;

      if L.Head = Null_Node then
         L.Tail := Null_Node;
      end if;

      Free_Node (L, Old_Head);
      L.Count := L.Count - 1;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (L : in Out List) is
      Current : Node_Index;
      Prev    : Node_Index := Null_Node;
   begin
      if L.Head = Null_Node then
         return;
      end if;

      if L.Head = L.Tail then
         Free_Node (L, L.Head);
         L.Head := Null_Node;
         L.Tail := Null_Node;
         L.Count := 0;
         return;
      end if;

      Current := L.Head;
      while L.Nodes (Positive (Current)).Next /= Null_Node loop
         Prev := Current;
         Current := L.Nodes (Positive (Current)).Next;
      end loop;

      if Prev /= Null_Node then
         L.Nodes (Positive (Prev)).Next := Null_Node;
         L.Tail := Prev;
      end if;

      Free_Node (L, Current);
      L.Count := L.Count - 1;
   end Delete_Last;

   ------------
   -- Delete --
   ------------

   procedure Delete (L : in Out List; Index : Positive) is
      Current : Node_Index := L.Head;
      Prev    : Node_Index := Null_Node;
      I       : Positive := 1;
   begin
      if Index = 1 then
         Delete_First (L);
         return;
      end if;

      while Current /= Null_Node and I < Index loop
         Prev := Current;
         Current := L.Nodes (Positive (Current)).Next;
         I := I + 1;
      end loop;

      if Current /= Null_Node and Prev /= Null_Node then
         L.Nodes (Positive (Prev)).Next := L.Nodes (Positive (Current)).Next;

         if Current = L.Tail then
            L.Tail := Prev;
         end if;

         Free_Node (L, Current);
         L.Count := L.Count - 1;
      end if;
   end Delete;

   -----------
   -- Clear --
   -----------

   procedure Clear (L : out List) is
   begin
      Initialize (L);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (L : List; Value : Integer) return Boolean is
   begin
      return Find (L, Value) > 0;
   end Contains;

   ----------
   -- Find --
   ----------

   function Find (L : List; Value : Integer) return Natural is
      Current : Node_Index := L.Head;
      I       : Natural := 0;
   begin
      while Current /= Null_Node loop
         I := I + 1;
         if L.Nodes (Positive (Current)).Value = Value then
            return I;
         end if;
         Current := L.Nodes (Positive (Current)).Next;
      end loop;
      return 0;
   end Find;

   ------------------
   -- Reverse_List --
   ------------------

   procedure Reverse_List (L : in Out List) is
      Prev    : Node_Index := Null_Node;
      Current : Node_Index := L.Head;
      Next    : Node_Index;
   begin
      L.Tail := L.Head;

      while Current /= Null_Node loop
         Next := L.Nodes (Positive (Current)).Next;
         L.Nodes (Positive (Current)).Next := Prev;
         Prev := Current;
         Current := Next;
      end loop;

      L.Head := Prev;
   end Reverse_List;

   ----------------
   -- Begin_Iter --
   ----------------

   function Begin_Iter (L : List) return Iterator is
   begin
      return (Current => L.Head);
   end Begin_Iter;

   -------------
   -- Is_Done --
   -------------

   function Is_Done (I : Iterator) return Boolean is
   begin
      return I.Current = Null_Node;
   end Is_Done;

   -------------
   -- Current --
   -------------

   function Current (L : List; I : Iterator) return Integer is
   begin
      if I.Current = Null_Node then
         return 0;
      end if;
      return L.Nodes (Positive (I.Current)).Value;
   end Current;

   ----------
   -- Next --
   ----------

   procedure Next (I : in Out Iterator) is
   begin
      -- Note: This requires access to the list to work properly
      -- In a full implementation, the iterator would store a reference to the list
      -- For now, this is a simplified version
      I.Current := Null_Node;  -- Simplified - would need list access
   end Next;

end GNAT.Linked_List;
