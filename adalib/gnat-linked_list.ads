-- GNAT.Linked_List for Z80
-- Bounded singly linked list with integer data

package GNAT.Linked_List is
   pragma Preelaborate;

   Max_Nodes : constant := 64;  -- Maximum nodes for Z80

   type List is private;

   procedure Initialize (L : out List);
   --  Initialize empty list

   function Is_Empty (L : List) return Boolean;
   --  Check if list is empty

   function Length (L : List) return Natural;
   --  Return number of elements

   function First (L : List) return Integer;
   --  Get first element (0 if empty)

   function Last (L : List) return Integer;
   --  Get last element (0 if empty)

   function Element (L : List; Index : Positive) return Integer;
   --  Get element at index (0 if invalid)

   procedure Prepend (L : in Out List; Value : Integer);
   --  Add to front of list

   procedure Append (L : in Out List; Value : Integer);
   --  Add to end of list

   procedure Insert (L : in Out List; Before : Positive; Value : Integer);
   --  Insert before position

   procedure Delete_First (L : in Out List);
   --  Remove first element

   procedure Delete_Last (L : in Out List);
   --  Remove last element

   procedure Delete (L : in Out List; Index : Positive);
   --  Remove element at index

   procedure Clear (L : out List);
   --  Clear all elements

   function Contains (L : List; Value : Integer) return Boolean;
   --  Check if value exists

   function Find (L : List; Value : Integer) return Natural;
   --  Find index of value (0 if not found)

   procedure Reverse_List (L : in Out List);
   --  Reverse list in place

   -- Iteration support
   type Iterator is private;

   function Begin_Iter (L : List) return Iterator;
   function Is_Done (I : Iterator) return Boolean;
   function Current (L : List; I : Iterator) return Integer;
   procedure Next (I : in Out Iterator);

private

   type Node_Index is range 0 .. Max_Nodes;
   Null_Node : constant Node_Index := 0;

   type Node is record
      Value  : Integer := 0;
      Next   : Node_Index := Null_Node;
      In_Use : Boolean := False;
   end record;

   type Node_Array is array (1 .. Max_Nodes) of Node;

   type List is record
      Nodes : Node_Array;
      Head  : Node_Index := Null_Node;
      Tail  : Node_Index := Null_Node;
      Count : Natural := 0;
   end record;

   type Iterator is record
      Current : Node_Index := Null_Node;
   end record;

end GNAT.Linked_List;
