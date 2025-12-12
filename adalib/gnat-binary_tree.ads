-- GNAT.Binary_Tree for Z80
-- Simple binary search tree with bounded storage

package GNAT.Binary_Tree is
   pragma Preelaborate;

   Max_Nodes : constant := 32;  -- Maximum nodes for Z80 memory
   Max_Key_Length : constant := 16;
   Max_Value_Length : constant := 32;

   type Tree is private;

   procedure Initialize (T : out Tree);
   --  Initialize empty tree

   function Is_Empty (T : Tree) return Boolean;
   --  Check if tree is empty

   function Size (T : Tree) return Natural;
   --  Return number of nodes

   function Contains (T : Tree; Key : String) return Boolean;
   --  Check if key exists

   procedure Insert
     (T       : in Out Tree;
      Key     : String;
      Value   : String;
      Success : out Boolean);
   --  Insert key-value pair, Success=False if full or duplicate

   procedure Find
     (T     : Tree;
      Key   : String;
      Value : out String;
      Last  : out Natural;
      Found : out Boolean);
   --  Find value by key

   procedure Remove
     (T       : in Out Tree;
      Key     : String;
      Success : out Boolean);
   --  Remove node by key

   procedure Clear (T : out Tree);
   --  Clear all nodes

   function Minimum (T : Tree) return String;
   --  Return minimum key (empty if tree is empty)

   function Maximum (T : Tree) return String;
   --  Return maximum key (empty if tree is empty)

   -- Traversal support
   type Visitor is access procedure (Key, Value : String);

   procedure Traverse_InOrder (T : Tree; Visit : Visitor);
   --  Visit nodes in sorted order

   procedure Traverse_PreOrder (T : Tree; Visit : Visitor);
   --  Visit root before children

   procedure Traverse_PostOrder (T : Tree; Visit : Visitor);
   --  Visit children before root

private

   type Key_String is array (1 .. Max_Key_Length) of Character;
   type Value_String is array (1 .. Max_Value_Length) of Character;

   Null_Index : constant := 0;
   type Node_Index is range Null_Index .. Max_Nodes;

   type Node is record
      Key       : Key_String := (others => ASCII.NUL);
      Key_Len   : Natural := 0;
      Value     : Value_String := (others => ASCII.NUL);
      Value_Len : Natural := 0;
      Left      : Node_Index := Null_Index;
      Right     : Node_Index := Null_Index;
      In_Use    : Boolean := False;
   end record;

   type Node_Array is array (1 .. Max_Nodes) of Node;

   type Tree is record
      Nodes : Node_Array;
      Root  : Node_Index := Null_Index;
      Count : Natural := 0;
   end record;

end GNAT.Binary_Tree;
