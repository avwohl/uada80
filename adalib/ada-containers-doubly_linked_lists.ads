-- Ada.Containers.Doubly_Linked_Lists for Z80
-- Generic doubly linked list container
--
-- Note: On Z80 with limited memory, lists have practical node limits.

with Ada.Containers;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Doubly_Linked_Lists is
   pragma Preelaborate;

   type List is tagged private;
   type Cursor is private;

   Empty_List : constant List;
   No_Element : constant Cursor;

   -- Comparison
   function "=" (Left, Right : List) return Boolean;

   -- Query operations
   function Length (Container : List) return Count_Type;
   function Is_Empty (Container : List) return Boolean;
   procedure Clear (Container : in Out List);

   -- Element access
   function Element (Position : Cursor) return Element_Type;
   procedure Replace_Element
     (Container : in Out List;
      Position  : Cursor;
      New_Item  : Element_Type);

   -- Query cursor operations
   function First (Container : List) return Cursor;
   function Last (Container : List) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in Out Cursor);
   procedure Previous (Position : in Out Cursor);
   function Has_Element (Position : Cursor) return Boolean;

   -- Search
   function Find
     (Container : List;
      Item      : Element_Type) return Cursor;

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor) return Cursor;

   function Reverse_Find
     (Container : List;
      Item      : Element_Type) return Cursor;

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor) return Cursor;

   function Contains (Container : List; Item : Element_Type) return Boolean;

   -- Insertion
   procedure Append (Container : in Out List; New_Item : Element_Type);
   procedure Prepend (Container : in Out List; New_Item : Element_Type);

   procedure Insert
     (Container : in Out List;
      Before    : Cursor;
      New_Item  : Element_Type);

   procedure Insert
     (Container : in Out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor);

   -- Deletion
   procedure Delete
     (Container : in Out List;
      Position  : in Out Cursor);

   procedure Delete_First (Container : in Out List);
   procedure Delete_Last (Container : in Out List);

   -- Splice - move elements between lists
   procedure Splice
     (Target   : in Out List;
      Before   : Cursor;
      Source   : in Out List);

   procedure Splice
     (Target   : in Out List;
      Before   : Cursor;
      Source   : in Out List;
      Position : in Out Cursor);

   procedure Splice
     (Container : in Out List;
      Before    : Cursor;
      Position  : Cursor);

   -- Reversal and sorting
   procedure Reverse_Elements (Container : in Out List);

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   procedure Generic_Sorting;

   -- Iteration
   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));

private

   -- Maximum nodes for Z80 implementation
   Max_Nodes : constant := 128;

   -- Node storage - uses array-based allocation to avoid heap fragmentation
   type Node_Index is range 0 .. Max_Nodes;
   Null_Node : constant Node_Index := 0;

   type Node_Type is record
      Element : Element_Type;
      Next    : Node_Index := Null_Node;
      Prev    : Node_Index := Null_Node;
      Used    : Boolean := False;
   end record;

   type Node_Array is array (1 .. Max_Nodes) of Node_Type;

   type List is tagged record
      Nodes     : Node_Array;
      Head      : Node_Index := Null_Node;
      Tail      : Node_Index := Null_Node;
      Length    : Count_Type := 0;
      Free_List : Node_Index := 1;  -- First free node
   end record;

   type Cursor is record
      Container : access constant List;
      Node      : Node_Index := Null_Node;
   end record;

   Empty_List : constant List := (Nodes => (others => (Element => <>, Next => Null_Node, Prev => Null_Node, Used => False)), Head => Null_Node, Tail => Null_Node, Length => 0, Free_List => 1);
   No_Element : constant Cursor := (Container => null, Node => Null_Node);

end Ada.Containers.Doubly_Linked_Lists;
