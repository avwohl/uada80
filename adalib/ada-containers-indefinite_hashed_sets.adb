-- Ada.Containers.Indefinite_Hashed_Sets body for Z80
-- Generic hashed set for indefinite types implementation

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Indefinite_Hashed_Sets is

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);

   function Bucket_For (Item : Element_Type) return Natural is
   begin
      return Natural (Hash (Item) mod Num_Buckets);
   end Bucket_For;

   procedure Allocate (Container : in Out Set; Index : out Node_Index) is
   begin
      for I in Container.Nodes'Range loop
         if not Container.Nodes (I).In_Use then
            Index := I;
            Container.Nodes (I).In_Use := True;
            Container.Nodes (I).Next := No_Node;
            Container.Nodes (I).Element := null;
            return;
         end if;
      end loop;
      raise Capacity_Error;
   end Allocate;

   procedure Deallocate (Container : in Out Set; Index : Node_Index) is
   begin
      if Container.Nodes (Index).Element /= null then
         Free (Container.Nodes (Index).Element);
      end if;
      Container.Nodes (Index).In_Use := False;
      Container.Nodes (Index).Next := No_Node;
   end Deallocate;

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
            if not Contains (Right, Left.Nodes (I).Element.all) then
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
         if Container.Nodes (I).In_Use then
            Deallocate (Container, I);
         end if;
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
      return Position.Container.Nodes (Position.Node).Element.all;
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
      Bucket : constant Natural := Bucket_For (Item);
      Node   : Node_Index := Container.Buckets (Bucket);
   begin
      while Node /= No_Node loop
         if Equivalent_Elements (Container.Nodes (Node).Element.all, Item) then
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
      Bucket   : constant Natural := Bucket_For (New_Item);
      New_Node : Node_Index;
   begin
      Position := Find (Container, New_Item);
      if Position /= No_Element then
         Inserted := False;
         return;
      end if;

      Allocate (Container, New_Node);
      Container.Nodes (New_Node).Element := new Element_Type'(New_Item);
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
      -- For sets, Include just ignores duplicates
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
      -- Replace element in place
      if Container.Nodes (Position.Node).Element /= null then
         Free (Container.Nodes (Position.Node).Element);
      end if;
      Container.Nodes (Position.Node).Element := new Element_Type'(New_Item);
   end Replace;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Set; Item : Element_Type) is
      Bucket    : constant Natural := Bucket_For (Item);
      Node      : Node_Index := Container.Buckets (Bucket);
      Prev_Node : Node_Index := No_Node;
   begin
      while Node /= No_Node loop
         if Equivalent_Elements (Container.Nodes (Node).Element.all, Item) then
            if Prev_Node = No_Node then
               Container.Buckets (Bucket) := Container.Nodes (Node).Next;
            else
               Container.Nodes (Prev_Node).Next := Container.Nodes (Node).Next;
            end if;
            Deallocate (Container, Node);
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
      Delete (Container, Container.Nodes (Position.Node).Element.all);
      Position := No_Element;
   end Delete;

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

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
      for I in Subset.Nodes'Range loop
         if Subset.Nodes (I).In_Use then
            if not Contains (Of_Set, Subset.Nodes (I).Element.all) then
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
            if Contains (Right, Left.Nodes (I).Element.all) then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Overlap;

end Ada.Containers.Indefinite_Hashed_Sets;
