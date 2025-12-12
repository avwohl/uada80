-- Ada.Containers.Indefinite_Hashed_Maps body for Z80
-- Generic hashed map for indefinite types implementation

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Indefinite_Hashed_Maps is

   procedure Free_Key is new Ada.Unchecked_Deallocation
     (Key_Type, Key_Access);
   procedure Free_Element is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);

   function Bucket_For (Key : Key_Type) return Natural is
   begin
      return Natural (Hash (Key) mod Num_Buckets);
   end Bucket_For;

   procedure Allocate (Container : in Out Map; Index : out Node_Index) is
   begin
      for I in Container.Nodes'Range loop
         if not Container.Nodes (I).In_Use then
            Index := I;
            Container.Nodes (I).In_Use := True;
            Container.Nodes (I).Next := No_Node;
            Container.Nodes (I).Key := null;
            Container.Nodes (I).Element := null;
            return;
         end if;
      end loop;
      raise Capacity_Error;
   end Allocate;

   procedure Deallocate (Container : in Out Map; Index : Node_Index) is
   begin
      if Container.Nodes (Index).Key /= null then
         Free_Key (Container.Nodes (Index).Key);
      end if;
      if Container.Nodes (Index).Element /= null then
         Free_Element (Container.Nodes (Index).Element);
      end if;
      Container.Nodes (Index).In_Use := False;
      Container.Nodes (Index).Next := No_Node;
   end Deallocate;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
      Pos : Cursor;
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in Left.Nodes'Range loop
         if Left.Nodes (I).In_Use then
            Pos := Find (Right, Left.Nodes (I).Key.all);
            if Pos = No_Element then
               return False;
            end if;
            if Left.Nodes (I).Element.all /= Element (Pos) then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      return Container.Length;
   end Length;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out Map) is
   begin
      for I in Container.Nodes'Range loop
         if Container.Nodes (I).In_Use then
            Deallocate (Container, I);
         end if;
      end loop;
      Container.Buckets := (others => No_Node);
      Container.Length := 0;
   end Clear;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;
      return Position.Container.Nodes (Position.Node).Key.all;
   end Key;

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

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Pos : constant Cursor := Find (Container, Key);
   begin
      if Pos = No_Element then
         raise Constraint_Error;
      end if;
      return Element (Pos);
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in Out Map;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;
      if Container.Nodes (Position.Node).Element /= null then
         Free_Element (Container.Nodes (Position.Node).Element);
      end if;
      Container.Nodes (Position.Node).Element := new Element_Type'(New_Item);
   end Replace_Element;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
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

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Bucket : constant Natural := Bucket_For (Key);
      Node   : Node_Index := Container.Buckets (Bucket);
   begin
      while Node /= No_Node loop
         if Equivalent_Keys (Container.Nodes (Node).Key.all, Key) then
            return (Container => Container'Unchecked_Access, Node => Node);
         end if;
         Node := Container.Nodes (Node).Next;
      end loop;
      return No_Element;
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= No_Element;
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      Bucket   : constant Natural := Bucket_For (Key);
      New_Node : Node_Index;
   begin
      Position := Find (Container, Key);
      if Position /= No_Element then
         Inserted := False;
         return;
      end if;

      Allocate (Container, New_Node);
      Container.Nodes (New_Node).Key := new Key_Type'(Key);
      Container.Nodes (New_Node).Element := new Element_Type'(New_Item);
      Container.Nodes (New_Node).Next := Container.Buckets (Bucket);
      Container.Buckets (Bucket) := New_Node;
      Container.Length := Container.Length + 1;

      Position := (Container => Container'Unchecked_Access, Node => New_Node);
      Inserted := True;
   end Insert;

   procedure Insert
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
      if not Inserted then
         Replace_Element (Container, Position, New_Item);
      end if;
   end Include;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : constant Cursor := Find (Container, Key);
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      Replace_Element (Container, Position, New_Item);
   end Replace;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Map; Key : Key_Type) is
      Bucket    : constant Natural := Bucket_For (Key);
      Node      : Node_Index := Container.Buckets (Bucket);
      Prev_Node : Node_Index := No_Node;
   begin
      while Node /= No_Node loop
         if Equivalent_Keys (Container.Nodes (Node).Key.all, Key) then
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

   procedure Delete (Container : in Out Map; Key : Key_Type) is
   begin
      if not Contains (Container, Key) then
         raise Constraint_Error;
      end if;
      Exclude (Container, Key);
   end Delete;

   procedure Delete (Container : in Out Map; Position : in Out Cursor) is
   begin
      if Position.Node = No_Node then
         raise Constraint_Error;
      end if;
      Delete (Container, Container.Nodes (Position.Node).Key.all);
      Position := No_Element;
   end Delete;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for I in Container.Nodes'Range loop
         if Container.Nodes (I).In_Use then
            Process ((Container => Container'Unchecked_Access, Node => I));
         end if;
      end loop;
   end Iterate;

end Ada.Containers.Indefinite_Hashed_Maps;
