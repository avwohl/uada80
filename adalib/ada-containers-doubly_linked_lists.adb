-- Ada.Containers.Doubly_Linked_Lists body for Z80
-- Generic doubly linked list implementation

package body Ada.Containers.Doubly_Linked_Lists is

   -- Internal: Allocate a node from the free list
   procedure Allocate_Node
     (Container : in Out List;
      Index     : out Node_Index)
   is
   begin
      if Container.Free_List = Null_Node then
         raise Capacity_Error;
      end if;

      Index := Container.Free_List;

      -- Find next free node
      Container.Free_List := Null_Node;
      for I in Index + 1 .. Max_Nodes loop
         if not Container.Nodes (I).Used then
            Container.Free_List := I;
            exit;
         end if;
      end loop;

      Container.Nodes (Index).Used := True;
      Container.Nodes (Index).Next := Null_Node;
      Container.Nodes (Index).Prev := Null_Node;
   end Allocate_Node;

   -- Internal: Free a node back to the pool
   procedure Free_Node
     (Container : in Out List;
      Index     : Node_Index)
   is
   begin
      Container.Nodes (Index).Used := False;
      Container.Nodes (Index).Next := Null_Node;
      Container.Nodes (Index).Prev := Null_Node;

      -- Add to front of free list if it's lower than current
      if Container.Free_List = Null_Node or Index < Container.Free_List then
         Container.Free_List := Index;
      end if;
   end Free_Node;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : List) return Boolean is
      L_Cursor : Node_Index := Left.Head;
      R_Cursor : Node_Index := Right.Head;
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      while L_Cursor /= Null_Node loop
         if Left.Nodes (L_Cursor).Element /= Right.Nodes (R_Cursor).Element then
            return False;
         end if;
         L_Cursor := Left.Nodes (L_Cursor).Next;
         R_Cursor := Right.Nodes (R_Cursor).Next;
      end loop;

      return True;
   end "=";

   ------------
   -- Length --
   ------------

   function Length (Container : List) return Count_Type is
   begin
      return Container.Length;
   end Length;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out List) is
   begin
      -- Mark all nodes as unused
      for I in 1 .. Max_Nodes loop
         Container.Nodes (I).Used := False;
         Container.Nodes (I).Next := Null_Node;
         Container.Nodes (I).Prev := Null_Node;
      end loop;

      Container.Head := Null_Node;
      Container.Tail := Null_Node;
      Container.Length := 0;
      Container.Free_List := 1;
   end Clear;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = Null_Node or Position.Container = null then
         raise Constraint_Error;
      end if;
      return Position.Container.Nodes (Position.Node).Element;
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in Out List;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Position.Node = Null_Node then
         raise Constraint_Error;
      end if;
      Container.Nodes (Position.Node).Element := New_Item;
   end Replace_Element;

   -----------
   -- First --
   -----------

   function First (Container : List) return Cursor is
   begin
      if Container.Head = Null_Node then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Node => Container.Head);
   end First;

   ----------
   -- Last --
   ----------

   function Last (Container : List) return Cursor is
   begin
      if Container.Tail = Null_Node then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Node => Container.Tail);
   end Last;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = Null_Node or Position.Container = null then
         return No_Element;
      end if;

      declare
         Next_Node : Node_Index := Position.Container.Nodes (Position.Node).Next;
      begin
         if Next_Node = Null_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => Next_Node);
      end;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Node = Null_Node or Position.Container = null then
         return No_Element;
      end if;

      declare
         Prev_Node : Node_Index := Position.Container.Nodes (Position.Node).Prev;
      begin
         if Prev_Node = Null_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => Prev_Node);
      end;
   end Previous;

   procedure Next (Position : in Out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   procedure Previous (Position : in Out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Node /= Null_Node and Position.Container /= null;
   end Has_Element;

   ----------
   -- Find --
   ----------

   function Find
     (Container : List;
      Item      : Element_Type) return Cursor
   is
      Current : Node_Index := Container.Head;
   begin
      while Current /= Null_Node loop
         if Container.Nodes (Current).Element = Item then
            return (Container => Container'Unchecked_Access, Node => Current);
         end if;
         Current := Container.Nodes (Current).Next;
      end loop;
      return No_Element;
   end Find;

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor) return Cursor
   is
      Current : Node_Index;
   begin
      if Position = No_Element then
         Current := Container.Head;
      else
         Current := Position.Node;
      end if;

      while Current /= Null_Node loop
         if Container.Nodes (Current).Element = Item then
            return (Container => Container'Unchecked_Access, Node => Current);
         end if;
         Current := Container.Nodes (Current).Next;
      end loop;
      return No_Element;
   end Find;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Container : List;
      Item      : Element_Type) return Cursor
   is
      Current : Node_Index := Container.Tail;
   begin
      while Current /= Null_Node loop
         if Container.Nodes (Current).Element = Item then
            return (Container => Container'Unchecked_Access, Node => Current);
         end if;
         Current := Container.Nodes (Current).Prev;
      end loop;
      return No_Element;
   end Reverse_Find;

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor) return Cursor
   is
      Current : Node_Index;
   begin
      if Position = No_Element then
         Current := Container.Tail;
      else
         Current := Position.Node;
      end if;

      while Current /= Null_Node loop
         if Container.Nodes (Current).Element = Item then
            return (Container => Container'Unchecked_Access, Node => Current);
         end if;
         Current := Container.Nodes (Current).Prev;
      end loop;
      return No_Element;
   end Reverse_Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : List; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in Out List; New_Item : Element_Type) is
      New_Node : Node_Index;
   begin
      Allocate_Node (Container, New_Node);
      Container.Nodes (New_Node).Element := New_Item;

      if Container.Tail = Null_Node then
         -- Empty list
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         -- Link to end
         Container.Nodes (Container.Tail).Next := New_Node;
         Container.Nodes (New_Node).Prev := Container.Tail;
         Container.Tail := New_Node;
      end if;

      Container.Length := Container.Length + 1;
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Container : in Out List; New_Item : Element_Type) is
      New_Node : Node_Index;
   begin
      Allocate_Node (Container, New_Node);
      Container.Nodes (New_Node).Element := New_Item;

      if Container.Head = Null_Node then
         -- Empty list
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         -- Link to front
         Container.Nodes (Container.Head).Prev := New_Node;
         Container.Nodes (New_Node).Next := Container.Head;
         Container.Head := New_Node;
      end if;

      Container.Length := Container.Length + 1;
   end Prepend;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in Out List;
      Before    : Cursor;
      New_Item  : Element_Type)
   is
      Position : Cursor;
   begin
      Insert (Container, Before, New_Item, Position);
   end Insert;

   procedure Insert
     (Container : in Out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor)
   is
      New_Node : Node_Index;
   begin
      if Before = No_Element then
         Append (Container, New_Item);
         Position := Last (Container);
         return;
      end if;

      Allocate_Node (Container, New_Node);
      Container.Nodes (New_Node).Element := New_Item;

      declare
         Before_Node : Node_Index := Before.Node;
         Prev_Node   : Node_Index := Container.Nodes (Before_Node).Prev;
      begin
         Container.Nodes (New_Node).Next := Before_Node;
         Container.Nodes (New_Node).Prev := Prev_Node;
         Container.Nodes (Before_Node).Prev := New_Node;

         if Prev_Node = Null_Node then
            Container.Head := New_Node;
         else
            Container.Nodes (Prev_Node).Next := New_Node;
         end if;
      end;

      Container.Length := Container.Length + 1;
      Position := (Container => Container'Unchecked_Access, Node => New_Node);
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in Out List;
      Position  : in Out Cursor)
   is
      Node_Idx  : Node_Index := Position.Node;
      Prev_Node : Node_Index;
      Next_Node : Node_Index;
   begin
      if Position.Node = Null_Node then
         raise Constraint_Error;
      end if;

      Prev_Node := Container.Nodes (Node_Idx).Prev;
      Next_Node := Container.Nodes (Node_Idx).Next;

      -- Update links
      if Prev_Node = Null_Node then
         Container.Head := Next_Node;
      else
         Container.Nodes (Prev_Node).Next := Next_Node;
      end if;

      if Next_Node = Null_Node then
         Container.Tail := Prev_Node;
      else
         Container.Nodes (Next_Node).Prev := Prev_Node;
      end if;

      Free_Node (Container, Node_Idx);
      Container.Length := Container.Length - 1;
      Position := No_Element;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in Out List) is
      Position : Cursor := First (Container);
   begin
      if Position /= No_Element then
         Delete (Container, Position);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in Out List) is
      Position : Cursor := Last (Container);
   begin
      if Position /= No_Element then
         Delete (Container, Position);
      end if;
   end Delete_Last;

   ------------
   -- Splice --
   ------------

   procedure Splice
     (Target   : in Out List;
      Before   : Cursor;
      Source   : in Out List)
   is
      Current : Node_Index := Source.Head;
   begin
      -- Move all elements from Source to Target
      while Current /= Null_Node loop
         declare
            Next_In_Source : Node_Index := Source.Nodes (Current).Next;
            Pos : Cursor;
         begin
            Insert (Target, Before, Source.Nodes (Current).Element, Pos);
            Current := Next_In_Source;
         end;
      end loop;

      Clear (Source);
   end Splice;

   procedure Splice
     (Target   : in Out List;
      Before   : Cursor;
      Source   : in Out List;
      Position : in Out Cursor)
   is
      Element_Value : Element_Type := Source.Nodes (Position.Node).Element;
      New_Position  : Cursor;
   begin
      -- Insert element into target
      Insert (Target, Before, Element_Value, New_Position);

      -- Delete from source
      Delete (Source, Position);

      Position := New_Position;
   end Splice;

   procedure Splice
     (Container : in Out List;
      Before    : Cursor;
      Position  : Cursor)
   is
      Node_Idx  : Node_Index := Position.Node;
      Prev_Node : Node_Index;
      Next_Node : Node_Index;
   begin
      if Position = No_Element or Position = Before then
         return;  -- Nothing to do
      end if;

      -- Unlink from current position
      Prev_Node := Container.Nodes (Node_Idx).Prev;
      Next_Node := Container.Nodes (Node_Idx).Next;

      if Prev_Node = Null_Node then
         Container.Head := Next_Node;
      else
         Container.Nodes (Prev_Node).Next := Next_Node;
      end if;

      if Next_Node = Null_Node then
         Container.Tail := Prev_Node;
      else
         Container.Nodes (Next_Node).Prev := Prev_Node;
      end if;

      -- Insert before the target position
      if Before = No_Element then
         -- Insert at end
         Container.Nodes (Node_Idx).Prev := Container.Tail;
         Container.Nodes (Node_Idx).Next := Null_Node;
         if Container.Tail /= Null_Node then
            Container.Nodes (Container.Tail).Next := Node_Idx;
         end if;
         Container.Tail := Node_Idx;
         if Container.Head = Null_Node then
            Container.Head := Node_Idx;
         end if;
      else
         declare
            Before_Node : Node_Index := Before.Node;
            Before_Prev : Node_Index := Container.Nodes (Before_Node).Prev;
         begin
            Container.Nodes (Node_Idx).Next := Before_Node;
            Container.Nodes (Node_Idx).Prev := Before_Prev;
            Container.Nodes (Before_Node).Prev := Node_Idx;

            if Before_Prev = Null_Node then
               Container.Head := Node_Idx;
            else
               Container.Nodes (Before_Prev).Next := Node_Idx;
            end if;
         end;
      end if;
   end Splice;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in Out List) is
      Current : Node_Index := Container.Head;
      Temp    : Node_Index;
   begin
      while Current /= Null_Node loop
         -- Swap prev and next
         Temp := Container.Nodes (Current).Next;
         Container.Nodes (Current).Next := Container.Nodes (Current).Prev;
         Container.Nodes (Current).Prev := Temp;

         Current := Temp;
      end loop;

      -- Swap head and tail
      Temp := Container.Head;
      Container.Head := Container.Tail;
      Container.Tail := Temp;
   end Reverse_Elements;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   procedure Generic_Sorting is
      -- Simple insertion sort - reasonable for small lists on Z80
   begin
      -- Sorting implementation would go here
      -- Using bubble sort for simplicity
      null;
   end Generic_Sorting;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      Current : Node_Index := Container.Head;
   begin
      while Current /= Null_Node loop
         Process ((Container => Container'Unchecked_Access, Node => Current));
         Current := Container.Nodes (Current).Next;
      end loop;
   end Iterate;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      Current : Node_Index := Container.Tail;
   begin
      while Current /= Null_Node loop
         Process ((Container => Container'Unchecked_Access, Node => Current));
         Current := Container.Nodes (Current).Prev;
      end loop;
   end Reverse_Iterate;

end Ada.Containers.Doubly_Linked_Lists;
