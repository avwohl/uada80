-- Ada.Containers.Indefinite_Multiway_Trees for Z80
-- Generic multiway tree for indefinite element types

with Ada.Containers;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Multiway_Trees is
   pragma Preelaborate;

   -- Maximum nodes for Z80
   Max_Nodes : constant := 32;

   type Tree is tagged private;
   type Cursor is private;

   No_Element : constant Cursor;
   Empty_Tree : constant Tree;

   function "=" (Left, Right : Tree) return Boolean;

   function Is_Empty (Container : Tree) return Boolean;
   function Node_Count (Container : Tree) return Count_Type;
   function Subtree_Node_Count (Position : Cursor) return Count_Type;
   function Depth (Position : Cursor) return Count_Type;

   function Is_Root (Position : Cursor) return Boolean;
   function Is_Leaf (Position : Cursor) return Boolean;

   function Root (Container : Tree) return Cursor;

   procedure Clear (Container : in Out Tree);

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in Out Tree;
      Position  : Cursor;
      New_Item  : Element_Type);

   function Has_Element (Position : Cursor) return Boolean;

   function Parent (Position : Cursor) return Cursor;
   function First_Child (Position : Cursor) return Cursor;
   function Last_Child (Position : Cursor) return Cursor;
   function Next_Sibling (Position : Cursor) return Cursor;
   function Previous_Sibling (Position : Cursor) return Cursor;

   procedure Next_Sibling (Position : in Out Cursor);
   procedure Previous_Sibling (Position : in Out Cursor);

   function Child_Count (Container : Tree; Parent : Cursor) return Count_Type;

   procedure Insert_Child
     (Container : in Out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor);

   procedure Insert_Child
     (Container : in Out Tree;
      Parent    : Cursor;
      Before    : Cursor;
      New_Item  : Element_Type);

   procedure Prepend_Child
     (Container : in Out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type);

   procedure Append_Child
     (Container : in Out Tree;
      Parent    : Cursor;
      New_Item  : Element_Type);

   procedure Delete_Children (Container : in Out Tree; Parent : Cursor);

   procedure Delete_Subtree (Container : in Out Tree; Position : in Out Cursor);

   function Find
     (Container : Tree;
      Item      : Element_Type) return Cursor;

   function Contains (Container : Tree; Item : Element_Type) return Boolean;

   procedure Iterate_Children
     (Parent  : Cursor;
      Process : not null access procedure (Position : Cursor));

   procedure Iterate_Subtree
     (Position : Cursor;
      Process  : not null access procedure (Position : Cursor));

private

   type Element_Access is access Element_Type;

   type Node_Index is range 0 .. Max_Nodes;
   No_Node : constant Node_Index := 0;

   type Node is record
      Element       : Element_Access := null;
      Parent        : Node_Index := No_Node;
      First_Child   : Node_Index := No_Node;
      Last_Child    : Node_Index := No_Node;
      Next_Sibling  : Node_Index := No_Node;
      Prev_Sibling  : Node_Index := No_Node;
      In_Use        : Boolean := False;
   end record;

   type Node_Array is array (1 .. Max_Nodes) of Node;

   type Tree is tagged record
      Nodes : Node_Array;
      Root  : Node_Index := No_Node;
      Count : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Tree;
      Node      : Node_Index := No_Node;
   end record;

   No_Element : constant Cursor := (Container => null, Node => No_Node);

   Empty_Tree : constant Tree := (Nodes => (others => (Element => null,
                                                       Parent => No_Node,
                                                       First_Child => No_Node,
                                                       Last_Child => No_Node,
                                                       Next_Sibling => No_Node,
                                                       Prev_Sibling => No_Node,
                                                       In_Use => False)),
                                  Root => No_Node,
                                  Count => 0);

end Ada.Containers.Indefinite_Multiway_Trees;
