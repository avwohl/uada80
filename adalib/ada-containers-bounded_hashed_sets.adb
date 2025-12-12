-- Ada.Containers.Bounded_Hashed_Sets body for Z80
-- Generic bounded hashed set implementation

package body Ada.Containers.Bounded_Hashed_Sets is

   function Bucket_For (Container : Set; Item : Element_Type) return Ada.Containers.Hash_Type is
   begin
      return Hash (Item) mod Container.Modulus;
   end Bucket_For;

   procedure Allocate (Container : in Out Set; Index : out Node_Index) is
   begin
      for I in Container.Nodes'Range loop
         if not Container.Nodes (I).In_Use then
            Index := I;
            Container.Nodes (I).In_Use := True;
            Container.Nodes (I).Next := No_Node;
            return;
         end if;
      end loop;
      raise Capacity_Error;
   end Allocate;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in Left.Nodes'Range loop
         if Left.Nodes (I).In_Use then
            if not Contains (Right, Left.Nodes (I).Element) then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Length --
   ------------

   function Length (Container : Set) return Count_Type is
   begin
      return Container.Length;
   end Length;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out Set) is
   begin
      for I in Container.Nodes'Range loop
         Container.Nodes (I).In_Use := False;
         Container.Nodes (I).Next := No_Node;
      end loop;
      Container.Buckets := (others => No_Node);
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

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
   begin
      for I in Container.Nodes'Range loop
         if Container.Nodes (I).In_Use then
            return (Container => Container'Unchecked_Access, Node => I);
         end if;
      end loop;
      return No_Element;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = No_Node then
         return No_Element;
      end if;

      for I in Position.Node + 1 .. Position.Container.Nodes'Last loop
         if Position.Container.Nodes (I).In_Use then
            return (Container => Position.Container, Node => I);
         end if;
      end loop;

      return No_Element;
   end Next;

   procedure Next (Position : in Out Cursor) is
   begin
      Position := Next (Position);
   end Next;

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

   function Find (Container : Set; Item : Element_Type) return Cursor is
      Bucket : constant Ada.Containers.Hash_Type := Bucket_For (Container, Item);
      Node   : Node_Index := Container.Buckets (Bucket);
   begin
      while Node /= No_Node loop
         if Equivalent_Elements (Container.Nodes (Node).Element, Item) then
            return (Container => Container'Unchecked_Access, Node => Node);
         end if;
         Node := Container.Nodes (Node).Next;
      end loop;
      return No_Element;
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in Out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      Bucket   : constant Ada.Containers.Hash_Type := Bucket_For (Container, New_Item);
      New_Node : Node_Index;
   begin
      Position := Find (Container, New_Item);
      if Position /= No_Element then
         Inserted := False;
         return;
      end if;

      Allocate (Container, New_Node);
      Container.Nodes (New_Node).Element := New_Item;
      Container.Nodes (New_Node).Next := Container.Buckets (Bucket);
      Container.Buckets (Bucket) := New_Node;
      Container.Length := Container.Length + 1;

      Position := (Container => Container'Unchecked_Access, Node => New_Node);
      Inserted := True;
   end Insert;

   procedure Insert (Container : in Out Set; New_Item : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include (Container : in Out Set; New_Item : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
   end Include;

   -------------
   -- Replace --
   -------------

   procedure Replace (Container : in Out Set; New_Item : Element_Type) is
      Position : constant Cursor := Find (Container, New_Item);
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      Container.Nodes (Position.Node).Element := New_Item;
   end Replace;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Set; Item : Element_Type) is
      Bucket    : constant Ada.Containers.Hash_Type := Bucket_For (Container, Item);
      Node      : Node_Index := Container.Buckets (Bucket);
      Prev_Node : Node_Index := No_Node;
   begin
      while Node /= No_Node loop
         if Equivalent_Elements (Container.Nodes (Node).Element, Item) then
            if Prev_Node = No_Node then
               Container.Buckets (Bucket) := Container.Nodes (Node).Next;
            else
               Container.Nodes (Prev_Node).Next := Container.Nodes (Node).Next;
            end if;
            Container.Nodes (Node).In_Use := False;
            Container.Nodes (Node).Next := No_Node;
            Container.Length := Container.Length - 1;
            return;
         end if;
         Prev_Node := Node;
         Node := Container.Nodes (Node).Next;
      end loop;
   end Exclude;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in Out Set; Item : Element_Type) is
   begin
      if not Contains (Container, Item) then
         raise Constraint_Error;
      end if;
      Exclude (Container, Item);
   end Delete;

   procedure Delete (Container : in Out Set; Position : in Out Cursor) is
   begin
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;
      Exclude (Container, Container.Nodes (Position.Node).Element);
      Position := No_Element;
   end Delete;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in Out Set; Source : Set) is
   begin
      for I in Source.Nodes'Range loop
         if Source.Nodes (I).In_Use then
            Include (Target, Source.Nodes (I).Element);
         end if;
      end loop;
   end Union;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection (Target : in Out Set; Source : Set) is
      I : Node_Index := 1;
   begin
      while I <= Target.Nodes'Last loop
         if Target.Nodes (I).In_Use then
            if not Contains (Source, Target.Nodes (I).Element) then
               Exclude (Target, Target.Nodes (I).Element);
            else
               I := I + 1;
            end if;
         else
            I := I + 1;
         end if;
      end loop;
   end Intersection;

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in Out Set; Source : Set) is
   begin
      for I in Source.Nodes'Range loop
         if Source.Nodes (I).In_Use then
            Exclude (Target, Source.Nodes (I).Element);
         end if;
      end loop;
   end Difference;

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference (Target : in Out Set; Source : Set) is
   begin
      for I in Source.Nodes'Range loop
         if Source.Nodes (I).In_Use then
            if Contains (Target, Source.Nodes (I).Element) then
               Exclude (Target, Source.Nodes (I).Element);
            else
               Include (Target, Source.Nodes (I).Element);
            end if;
         end if;
      end loop;
   end Symmetric_Difference;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
      for I in Subset.Nodes'Range loop
         if Subset.Nodes (I).In_Use then
            if not Contains (Of_Set, Subset.Nodes (I).Element) then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Subset;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Set) return Boolean is
   begin
      for I in Left.Nodes'Range loop
         if Left.Nodes (I).In_Use then
            if Contains (Right, Left.Nodes (I).Element) then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Overlap;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for I in Container.Nodes'Range loop
         if Container.Nodes (I).In_Use then
            Process ((Container => Container'Unchecked_Access, Node => I));
         end if;
      end loop;
   end Iterate;

end Ada.Containers.Bounded_Hashed_Sets;
