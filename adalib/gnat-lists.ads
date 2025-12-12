-- GNAT.Lists for Z80
-- Simple doubly-linked list generic

generic
   type Element_Type is private;
package GNAT.Lists is
   pragma Preelaborate;

   type List is private;

   Empty_List : constant List;

   procedure Append (L : in Out List; Element : Element_Type);
   --  Add element to end of list

   procedure Prepend (L : in Out List; Element : Element_Type);
   --  Add element to front of list

   function First (L : List) return Element_Type;
   --  Get first element (raises if empty)

   function Last (L : List) return Element_Type;
   --  Get last element (raises if empty)

   procedure Delete_First (L : in Out List);
   --  Remove first element

   procedure Delete_Last (L : in Out List);
   --  Remove last element

   function Is_Empty (L : List) return Boolean;
   --  Check if list is empty

   function Length (L : List) return Natural;
   --  Number of elements

   procedure Clear (L : in Out List);
   --  Remove all elements

   -- Iteration
   type Cursor is private;

   function First_Cursor (L : List) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Has_Element (Position : Cursor) return Boolean;
   function Element (Position : Cursor) return Element_Type;

   List_Empty : exception;

private
   Max_Elements : constant := 200;

   type Element_Array is array (1 .. Max_Elements) of Element_Type;
   type Link_Array is array (1 .. Max_Elements) of Natural;

   type List is record
      Elements : Element_Array;
      Next_Link : Link_Array := (others => 0);
      Prev_Link : Link_Array := (others => 0);
      Head      : Natural := 0;
      Tail      : Natural := 0;
      Free      : Natural := 1;
      Count     : Natural := 0;
   end record;

   Empty_List : constant List := (
      Elements => <>,
      Next_Link => (others => 0),
      Prev_Link => (others => 0),
      Head => 0, Tail => 0, Free => 1, Count => 0);

   type Cursor is record
      Container : access List;
      Index     : Natural := 0;
   end record;

end GNAT.Lists;
