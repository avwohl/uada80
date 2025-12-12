-- Ada.Containers.Bounded_Doubly_Linked_Lists body for Z80
-- Generic bounded doubly linked list implementation

package body Ada.Containers.Bounded_Doubly_Linked_Lists is

   ----------------
   -- Allocate --
   ----------------

   procedure Allocate (Container : in Out List; Index : out Node_Index) is
   begin
      if Container.Length >= Container.Capacity then
         raise Capacity_Error;
      end if;

      -- Find first free node
      for I in Container.Nodes'Range loop
         if not Container.Nodes (I).In_Use then
            Index := I;
            Container.Nodes (I).In_Use := True;
            Container.Nodes (I).Next := No_Node;
            Container.Nodes (I).Prev := No_Node;
            return;
         end if;
      end loop;

      raise Capacity_Error;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Container : in Out List; Index : Node_Index) is
   begin
      Container.Nodes (Index).In_Use := False;
      Container.Nodes (Index).Next := No_Node;
      Container.Nodes (Index).Prev := No_Node;
   end Deallocate;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : List) return Boolean is
      L_Node : Node_Index := Left.First;
      R_Node : Node_Index := Right.First;
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      while L_Node /= No_Node loop
         if Left.Nodes (L_Node).Element /= Right.Nodes (R_Node).Element then
            return False;
         end if;
         L_Node := Left.Nodes (L_Node).Next;
         R_Node := Right.Nodes (R_Node).Next;
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
      for I in Container.Nodes'Range loop
         Container.Nodes (I).In_Use := False;
         Container.Nodes (I).Next := No_Node;
         Container.Nodes (I).Prev := No_Node;
      end loop;
      Container.First := No_Node;
      Container.Last := No_Node;
      Container.Length := 0;
   end Clear;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = No_Node then
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
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;
      Container.Nodes (Position.Node).Element := New_Item;
   end Replace_Element;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
   begin
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;
      Process (Position.Container.Nodes (Position.Node).Element);
   end Query_Element;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in Out List;
      Position  : Cursor;
      Process   : not null access procedure (Element : in Out Element_Type))
   is
   begin
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;
      Process (Container.Nodes (Position.Node).Element);
   end Update_Element;

   -----------
   -- First --
   -----------

   function First (Container : List) return Cursor is
   begin
      if Container.First = No_Node then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Node => Container.First);
   end First;

   ----------
   -- Last --
   ----------

   function Last (Container : List) return Cursor is
   begin
      if Container.Last = No_Node then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Node => Container.Last);
   end Last;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = No_Node then
         return No_Element;
      end if;
      declare
         Next_Node : constant Node_Index := Position.Container.Nodes (Position.Node).Next;
      begin
         if Next_Node = No_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => Next_Node);
      end;
   end Next;

   procedure Next (Position : in Out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Node = No_Node then
         return No_Element;
      end if;
      declare
         Prev_Node : constant Node_Index := Position.Container.Nodes (Position.Node).Prev;
      begin
         if Prev_Node = No_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => Prev_Node);
      end;
   end Previous;

   procedure Previous (Position : in Out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Node /= No_Node;
   end Has_Element;

   ----------
   -- Find --
   ----------

   function Find
     (Container : List;
      Item      : Element_Type) return Cursor
   is
      Node : Node_Index := Container.First;
   begin
      while Node /= No_Node loop
         if Container.Nodes (Node).Element = Item then
            return (Container => Container'Unchecked_Access, Node => Node);
         end if;
         Node := Container.Nodes (Node).Next;
      end loop;
      return No_Element;
   end Find;

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
      Allocate (Container, New_Node);
      Container.Nodes (New_Node).Element := New_Item;

      if Container.Last = No_Node then
         Container.First := New_Node;
         Container.Last := New_Node;
      else
         Container.Nodes (Container.Last).Next := New_Node;
         Container.Nodes (New_Node).Prev := Container.Last;
         Container.Last := New_Node;
      end if;

      Container.Length := Container.Length + 1;
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Container : in Out List; New_Item : Element_Type) is
      New_Node : Node_Index;
   begin
      Allocate (Container, New_Node);
      Container.Nodes (New_Node).Element := New_Item;

      if Container.First = No_Node then
         Container.First := New_Node;
         Container.Last := New_Node;
      else
         Container.Nodes (Container.First).Prev := New_Node;
         Container.Nodes (New_Node).Next := Container.First;
         Container.First := New_Node;
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

      Allocate (Container, New_Node);
      Container.Nodes (New_Node).Element := New_Item;

      declare
         Before_Node : constant Node_Index := Before.Node;
         Prev_Node   : constant Node_Index := Container.Nodes (Before_Node).Prev;
      begin
         Container.Nodes (New_Node).Next := Before_Node;
         Container.Nodes (New_Node).Prev := Prev_Node;
         Container.Nodes (Before_Node).Prev := New_Node;

         if Prev_Node = No_Node then
            Container.First := New_Node;
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

   procedure Delete (Container : in Out List; Position : in Out Cursor) is
      Node      : constant Node_Index := Position.Node;
      Prev_Node : constant Node_Index := Container.Nodes (Node).Prev;
      Next_Node : constant Node_Index := Container.Nodes (Node).Next;
   begin
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;

      if Prev_Node = No_Node then
         Container.First := Next_Node;
      else
         Container.Nodes (Prev_Node).Next := Next_Node;
      end if;

      if Next_Node = No_Node then
         Container.Last := Prev_Node;
      else
         Container.Nodes (Next_Node).Prev := Prev_Node;
      end if;

      Deallocate (Container, Node);
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

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      Node : Node_Index := Container.First;
   begin
      while Node /= No_Node loop
         Process ((Container => Container'Unchecked_Access, Node => Node));
         Node := Container.Nodes (Node).Next;
      end loop;
   end Iterate;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      Node : Node_Index := Container.Last;
   begin
      while Node /= No_Node loop
         Process ((Container => Container'Unchecked_Access, Node => Node));
         Node := Container.Nodes (Node).Prev;
      end loop;
   end Reverse_Iterate;

end Ada.Containers.Bounded_Doubly_Linked_Lists;
