-- Ada.Containers.Multiway_Trees body for Z80
-- Generic multiway tree container implementation

package body Ada.Containers.Multiway_Trees is

   procedure Allocate (Container : in Out Tree; Index : out Node_Index) is
   begin
      for I in Container.Nodes'Range loop
         if not Container.Nodes (I).In_Use then
            Index := I;
            Container.Nodes (I).In_Use := True;
            Container.Nodes (I).Parent := No_Node;
            Container.Nodes (I).First_Child := No_Node;
            Container.Nodes (I).Last_Child := No_Node;
            Container.Nodes (I).Next_Sibling := No_Node;
            Container.Nodes (I).Prev_Sibling := No_Node;
            return;
         end if;
      end loop;
      raise Capacity_Error;
   end Allocate;

   procedure Deallocate (Container : in Out Tree; Index : Node_Index) is
   begin
      Container.Nodes (Index).In_Use := False;
   end Deallocate;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Tree) return Boolean is
   begin
      return Left.Count = Right.Count;
      -- Full equality would require deep comparison
   end "=";

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Tree) return Boolean is
   begin
      return Container.Count = 0;
   end Is_Empty;

   ----------------
   -- Node_Count --
   ----------------

   function Node_Count (Container : Tree) return Count_Type is
   begin
      return Container.Count;
   end Node_Count;

   ------------------------
   -- Subtree_Node_Count --
   ------------------------

   function Subtree_Node_Count (Position : Cursor) return Count_Type is
      Count : Count_Type := 0;

      procedure Count_Nodes (N : Node_Index) is
         Child : Node_Index;
      begin
         if N = No_Node then
            return;
         end if;
         Count := Count + 1;
         Child := Position.Container.Nodes (N).First_Child;
         while Child /= No_Node loop
            Count_Nodes (Child);
            Child := Position.Container.Nodes (Child).Next_Sibling;
         end loop;
      end Count_Nodes;

   begin
      if Position.Node = No_Node then
         return 0;
      end if;
      Count_Nodes (Position.Node);
      return Count;
   end Subtree_Node_Count;

   -----------
   -- Depth --
   -----------

   function Depth (Position : Cursor) return Count_Type is
      D : Count_Type := 0;
      N : Node_Index := Position.Node;
   begin
      while N /= No_Node loop
         D := D + 1;
         N := Position.Container.Nodes (N).Parent;
      end loop;
      return D;
   end Depth;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Position : Cursor) return Boolean is
   begin
      return Position.Node /= No_Node and then
             Position.Container.Nodes (Position.Node).Parent = No_Node;
   end Is_Root;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Position : Cursor) return Boolean is
   begin
      return Position.Node /= No_Node and then
             Position.Container.Nodes (Position.Node).First_Child = No_Node;
   end Is_Leaf;

   ----------
   -- Root --
   ----------

   function Root (Container : Tree) return Cursor is
   begin
      if Container.Root = No_Node then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Node => Container.Root);
   end Root;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out Tree) is
   begin
      for I in Container.Nodes'Range loop
         Container.Nodes (I).In_Use := False;
      end loop;
      Container.Root := No_Node;
      Container.Count := 0;
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
     (Container : in Out Tree;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;
      Container.Nodes (Position.Node).Element := New_Item;
   end Replace_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Node /= No_Node;
   end Has_Element;

   ------------
   -- Parent --
   ------------

   function Parent (Position : Cursor) return Cursor is
   begin
      if Position.Node = No_Node then
         return No_Element;
      end if;
      declare
         P : constant Node_Index := Position.Container.Nodes (Position.Node).Parent;
      begin
         if P = No_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => P);
      end;
   end Parent;

   -----------------
   -- First_Child --
   -----------------

   function First_Child (Position : Cursor) return Cursor is
   begin
      if Position.Node = No_Node then
         return No_Element;
      end if;
      declare
         C : constant Node_Index := Position.Container.Nodes (Position.Node).First_Child;
      begin
         if C = No_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => C);
      end;
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child (Position : Cursor) return Cursor is
   begin
      if Position.Node = No_Node then
         return No_Element;
      end if;
      declare
         C : constant Node_Index := Position.Container.Nodes (Position.Node).Last_Child;
      begin
         if C = No_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => C);
      end;
   end Last_Child;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling (Position : Cursor) return Cursor is
   begin
      if Position.Node = No_Node then
         return No_Element;
      end if;
      declare
         S : constant Node_Index := Position.Container.Nodes (Position.Node).Next_Sibling;
      begin
         if S = No_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => S);
      end;
   end Next_Sibling;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling (Position : Cursor) return Cursor is
   begin
      if Position.Node = No_Node then
         return No_Element;
      end if;
      declare
         S : constant Node_Index := Position.Container.Nodes (Position.Node).Prev_Sibling;
      begin
         if S = No_Node then
            return No_Element;
         end if;
         return (Container => Position.Container, Node => S);
      end;
   end Previous_Sibling;

   procedure Next_Sibling (Position : in Out Cursor) is
   begin
      Position := Next_Sibling (Position);
   end Next_Sibling;

   procedure Previous_Sibling (Position : in Out Cursor) is
   begin
      Position := Previous_Sibling (Position);
   end Previous_Sibling;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Container : Tree; Parent : Cursor) return Count_Type is
      pragma Unreferenced (Container);
      Count : Count_Type := 0;
      Child : Node_Index;
   begin
      if Parent.Node = No_Node then
         return 0;
      end if;
      Child := Parent.Container.Nodes (Parent.Node).First_Child;
      while Child /= No_Node loop
         Count := Count + 1;
         Child := Parent.Container.Nodes (Child).Next_Sibling;
      end loop;
      return Count;
   end Child_Count;

   -----------------
   -- Child_Depth --
   -----------------

   function Child_Depth (Parent, Child : Cursor) return Count_Type is
      D : Count_Type := 0;
      N : Node_Index := Child.Node;
   begin
      while N /= No_Node and N /= Parent.Node loop
         D := D + 1;
         N := Child.Container.Nodes (N).Parent;
      end loop;
      if N = No_Node then
         return 0;
      end if;
      return D;
   end Child_Depth;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Container : in Out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor)
   is
      New_Node : Node_Index;
      Parent_Node : Node_Index;
   begin
      Allocate (Container, New_Node);
      Container.Nodes (New_Node).Element := New_Item;

      if Parent = No_Element then
         -- Insert as root
         if Container.Root /= No_Node then
            raise Constraint_Error;  -- Root already exists
         end if;
         Container.Root := New_Node;
      else
         Parent_Node := Parent.Node;
         Container.Nodes (New_Node).Parent := Parent_Node;

         if Before = No_Element then
            -- Append as last child
            declare
               Last : constant Node_Index := Container.Nodes (Parent_Node).Last_Child;
            begin
               if Last = No_Node then
                  Container.Nodes (Parent_Node).First_Child := New_Node;
               else
                  Container.Nodes (Last).Next_Sibling := New_Node;
                  Container.Nodes (New_Node).Prev_Sibling := Last;
               end if;
               Container.Nodes (Parent_Node).Last_Child := New_Node;
            end;
         else
            -- Insert before specified sibling
            declare
               Prev : constant Node_Index := Container.Nodes (Before.Node).Prev_Sibling;
            begin
               Container.Nodes (New_Node).Next_Sibling := Before.Node;
               Container.Nodes (New_Node).Prev_Sibling := Prev;
               Container.Nodes (Before.Node).Prev_Sibling := New_Node;
               if Prev = No_Node then
                  Container.Nodes (Parent_Node).First_Child := New_Node;
               else
                  Container.Nodes (Prev).Next_Sibling := New_Node;
               end if;
            end;
         end if;
      end if;

      Container.Count := Container.Count + 1;
      Position := (Container => Container'Unchecked_Access, Node => New_Node);
   end Insert_Child;

   procedure Insert_Child
     (Container : in Out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      New_Item  : Element_Type)
   is
      Position : Cursor;
   begin
      Insert_Child (Container, Parent, Before, New_Item, Position);
   end Insert_Child;

   -------------------
   -- Prepend_Child --
   -------------------

   procedure Prepend_Child
     (Container : in Out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type)
   is
      First : Cursor := First_Child (Parent);
   begin
      Insert_Child (Container, Parent, First, New_Item);
   end Prepend_Child;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Container : in Out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type)
   is
   begin
      Insert_Child (Container, Parent, No_Element, New_Item);
   end Append_Child;

   ---------------------
   -- Delete_Children --
   ---------------------

   procedure Delete_Children (Container : in Out Tree; Parent : Cursor) is
      Child : Node_Index;
      Next  : Node_Index;
   begin
      if Parent.Node = No_Node then
         return;
      end if;

      Child := Container.Nodes (Parent.Node).First_Child;
      while Child /= No_Node loop
         Next := Container.Nodes (Child).Next_Sibling;

         -- Recursively delete grandchildren
         declare
            Child_Cursor : Cursor := (Container => Container'Unchecked_Access, Node => Child);
         begin
            Delete_Children (Container, Child_Cursor);
         end;

         Deallocate (Container, Child);
         Container.Count := Container.Count - 1;
         Child := Next;
      end loop;

      Container.Nodes (Parent.Node).First_Child := No_Node;
      Container.Nodes (Parent.Node).Last_Child := No_Node;
   end Delete_Children;

   --------------------
   -- Delete_Subtree --
   --------------------

   procedure Delete_Subtree (Container : in Out Tree; Position : in Out Cursor) is
      Node : constant Node_Index := Position.Node;
      Parent_Node : Node_Index;
   begin
      if Node = No_Node then
         return;
      end if;

      -- Delete all children first
      Delete_Children (Container, Position);

      Parent_Node := Container.Nodes (Node).Parent;

      -- Unlink from siblings
      declare
         Prev : constant Node_Index := Container.Nodes (Node).Prev_Sibling;
         Next : constant Node_Index := Container.Nodes (Node).Next_Sibling;
      begin
         if Prev /= No_Node then
            Container.Nodes (Prev).Next_Sibling := Next;
         elsif Parent_Node /= No_Node then
            Container.Nodes (Parent_Node).First_Child := Next;
         end if;

         if Next /= No_Node then
            Container.Nodes (Next).Prev_Sibling := Prev;
         elsif Parent_Node /= No_Node then
            Container.Nodes (Parent_Node).Last_Child := Prev;
         end if;
      end;

      -- Handle root deletion
      if Container.Root = Node then
         Container.Root := No_Node;
      end if;

      Deallocate (Container, Node);
      Container.Count := Container.Count - 1;
      Position := No_Element;
   end Delete_Subtree;

   ----------
   -- Find --
   ----------

   function Find
     (Container : Tree;
      Item      : Element_Type) return Cursor
   is
      Result : Cursor := No_Element;

      procedure Search (N : Node_Index) is
         Child : Node_Index;
      begin
         if N = No_Node or Result /= No_Element then
            return;
         end if;

         if Container.Nodes (N).Element = Item then
            Result := (Container => Container'Unchecked_Access, Node => N);
            return;
         end if;

         Child := Container.Nodes (N).First_Child;
         while Child /= No_Node loop
            Search (Child);
            exit when Result /= No_Element;
            Child := Container.Nodes (Child).Next_Sibling;
         end loop;
      end Search;

   begin
      Search (Container.Root);
      return Result;
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Tree; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children
     (Parent  : Cursor;
      Process : not null access procedure (Position : Cursor))
   is
      Child : Node_Index;
   begin
      if Parent.Node = No_Node then
         return;
      end if;

      Child := Parent.Container.Nodes (Parent.Node).First_Child;
      while Child /= No_Node loop
         Process ((Container => Parent.Container, Node => Child));
         Child := Parent.Container.Nodes (Child).Next_Sibling;
      end loop;
   end Iterate_Children;

   ---------------------
   -- Iterate_Subtree --
   ---------------------

   procedure Iterate_Subtree
     (Position : Cursor;
      Process  : not null access procedure (Position : Cursor))
   is
      procedure Visit (N : Node_Index) is
         Child : Node_Index;
      begin
         if N = No_Node then
            return;
         end if;

         Process ((Container => Position.Container, Node => N));

         Child := Position.Container.Nodes (N).First_Child;
         while Child /= No_Node loop
            Visit (Child);
            Child := Position.Container.Nodes (Child).Next_Sibling;
         end loop;
      end Visit;

   begin
      Visit (Position.Node);
   end Iterate_Subtree;

end Ada.Containers.Multiway_Trees;
