-- Ada.Containers.Indefinite_Doubly_Linked_Lists for Z80
-- Generic doubly linked list for indefinite element types

with Ada.Containers;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Doubly_Linked_Lists is
   pragma Preelaborate;

   -- Maximum nodes for Z80
   Max_Nodes : constant := 64;

   type List is tagged private;
   type Cursor is private;

   No_Element : constant Cursor;
   Empty_List : constant List;

   function "=" (Left, Right : List) return Boolean;

   function Length (Container : List) return Count_Type;
   function Is_Empty (Container : List) return Boolean;
   procedure Clear (Container : in Out List);

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in Out List;
      Position  : Cursor;
      New_Item  : Element_Type);

   function First (Container : List) return Cursor;
   function Last (Container : List) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in Out Cursor);
   procedure Previous (Position : in Out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   function Find
     (Container : List;
      Item      : Element_Type) return Cursor;

   function Contains (Container : List; Item : Element_Type) return Boolean;

   procedure Append (Container : in Out List; New_Item : Element_Type);
   procedure Prepend (Container : in Out List; New_Item : Element_Type);

   procedure Insert
     (Container : in Out List;
      Before    : Cursor;
      New_Item  : Element_Type);

   procedure Delete (Container : in Out List; Position : in Out Cursor);
   procedure Delete_First (Container : in Out List);
   procedure Delete_Last (Container : in Out List);

   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor));

private

   type Element_Access is access Element_Type;

   type Node_Index is range 0 .. Max_Nodes;
   No_Node : constant Node_Index := 0;

   type Node is record
      Element : Element_Access := null;
      Next    : Node_Index := No_Node;
      Prev    : Node_Index := No_Node;
      In_Use  : Boolean := False;
   end record;

   type Node_Array is array (1 .. Max_Nodes) of Node;

   type List is tagged record
      Nodes  : Node_Array;
      First  : Node_Index := No_Node;
      Last   : Node_Index := No_Node;
      Length : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant List;
      Node      : Node_Index := No_Node;
   end record;

   No_Element : constant Cursor := (Container => null, Node => No_Node);

   Empty_List : constant List := (Nodes => (others => (Element => null, Next => No_Node, Prev => No_Node, In_Use => False)), First => No_Node, Last => No_Node, Length => 0);

end Ada.Containers.Indefinite_Doubly_Linked_Lists;
