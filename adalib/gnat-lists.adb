-- GNAT.Lists body for Z80
-- Simple doubly-linked list implementation

package body GNAT.Lists is

   function Allocate (L : in Out List) return Natural is
      Result : Natural;
   begin
      if L.Free = 0 or else L.Count >= Max_Elements then
         return 0;
      end if;

      Result := L.Free;
      L.Free := L.Free + 1;
      if L.Free > Max_Elements then
         L.Free := 0;
      end if;
      return Result;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (L : in Out List; Element : Element_Type) is
      New_Node : constant Natural := Allocate (L);
   begin
      if New_Node = 0 then
         return;  -- List full
      end if;

      L.Elements (New_Node) := Element;
      L.Next_Link (New_Node) := 0;
      L.Prev_Link (New_Node) := L.Tail;

      if L.Tail /= 0 then
         L.Next_Link (L.Tail) := New_Node;
      else
         L.Head := New_Node;
      end if;

      L.Tail := New_Node;
      L.Count := L.Count + 1;
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (L : in Out List; Element : Element_Type) is
      New_Node : constant Natural := Allocate (L);
   begin
      if New_Node = 0 then
         return;
      end if;

      L.Elements (New_Node) := Element;
      L.Prev_Link (New_Node) := 0;
      L.Next_Link (New_Node) := L.Head;

      if L.Head /= 0 then
         L.Prev_Link (L.Head) := New_Node;
      else
         L.Tail := New_Node;
      end if;

      L.Head := New_Node;
      L.Count := L.Count + 1;
   end Prepend;

   -----------
   -- First --
   -----------

   function First (L : List) return Element_Type is
   begin
      if L.Head = 0 then
         raise List_Empty;
      end if;
      return L.Elements (L.Head);
   end First;

   ----------
   -- Last --
   ----------

   function Last (L : List) return Element_Type is
   begin
      if L.Tail = 0 then
         raise List_Empty;
      end if;
      return L.Elements (L.Tail);
   end Last;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (L : in Out List) is
      Old_Head : constant Natural := L.Head;
   begin
      if Old_Head = 0 then
         return;
      end if;

      L.Head := L.Next_Link (Old_Head);
      if L.Head = 0 then
         L.Tail := 0;
      else
         L.Prev_Link (L.Head) := 0;
      end if;
      L.Count := L.Count - 1;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (L : in Out List) is
      Old_Tail : constant Natural := L.Tail;
   begin
      if Old_Tail = 0 then
         return;
      end if;

      L.Tail := L.Prev_Link (Old_Tail);
      if L.Tail = 0 then
         L.Head := 0;
      else
         L.Next_Link (L.Tail) := 0;
      end if;
      L.Count := L.Count - 1;
   end Delete_Last;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List) return Boolean is
   begin
      return L.Count = 0;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List) return Natural is
   begin
      return L.Count;
   end Length;

   -----------
   -- Clear --
   -----------

   procedure Clear (L : in Out List) is
   begin
      L.Head := 0;
      L.Tail := 0;
      L.Free := 1;
      L.Count := 0;
   end Clear;

   ------------------
   -- First_Cursor --
   ------------------

   function First_Cursor (L : List) return Cursor is
   begin
      return (Container => L'Unrestricted_Access, Index => L.Head);
   end First_Cursor;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Index = 0 then
         return (null, 0);
      end if;
      return (Position.Container,
              Position.Container.Next_Link (Position.Index));
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index > 0;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Container.Elements (Position.Index);
   end Element;

end GNAT.Lists;
