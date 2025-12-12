-- Ada.Containers.Bounded_Doubly_Linked_Lists for Z80
-- Generic bounded doubly linked list with compile-time capacity
--
-- More memory-efficient than unbounded lists, suitable for Z80

with Ada.Containers;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Bounded_Doubly_Linked_Lists is
   pragma Preelaborate;

   type List (Capacity : Count_Type) is tagged private;
   pragma Preelaborable_Initialization (List);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   No_Element : constant Cursor;

   function "=" (Left, Right : List) return Boolean;

   function Length (Container : List) return Count_Type;
   function Is_Empty (Container : List) return Boolean;
   procedure Clear (Container : in Out List);

   -- Element access
   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in Out List;
      Position  : Cursor;
      New_Item  : Element_Type);

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type));

   procedure Update_Element
     (Container : in Out List;
      Position  : Cursor;
      Process   : not null access procedure (Element : in Out Element_Type));

   -- Iteration
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

   function Contains (Container : List; Item : Element_Type) return Boolean;

   -- Modification
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

   procedure Delete (Container : in Out List; Position : in Out Cursor);
   procedure Delete_First (Container : in Out List);
   procedure Delete_Last (Container : in Out List);

   -- Iteration
   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));

private

   type Node_Index is range 0 .. Count_Type'Last;
   No_Node : constant Node_Index := 0;

   type Node is record
      Element : Element_Type;
      Next    : Node_Index := No_Node;
      Prev    : Node_Index := No_Node;
      In_Use  : Boolean := False;
   end record;

   type Node_Array is array (Node_Index range <>) of Node;

   type List (Capacity : Count_Type) is tagged record
      Nodes      : Node_Array (1 .. Node_Index (Capacity));
      First      : Node_Index := No_Node;
      Last       : Node_Index := No_Node;
      Free       : Node_Index := 1;
      Length     : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant List;
      Node      : Node_Index := No_Node;
   end record;

   No_Element : constant Cursor := (Container => null, Node => No_Node);

end Ada.Containers.Bounded_Doubly_Linked_Lists;
