-- GNAT.Sets for Z80
-- Generic set container

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
package GNAT.Sets is
   pragma Preelaborate;

   type Set is private;

   Empty_Set : constant Set;

   function Create return Set;
   --  Create empty set

   procedure Insert (S : in Out Set; Element : Element_Type);
   --  Add element to set

   procedure Delete (S : in Out Set; Element : Element_Type);
   --  Remove element from set

   function Contains (S : Set; Element : Element_Type) return Boolean;
   --  Check if element is in set

   function Size (S : Set) return Natural;
   --  Number of elements

   function Is_Empty (S : Set) return Boolean;
   --  True if set is empty

   procedure Clear (S : in Out Set);
   --  Remove all elements

   -- Set operations
   function Union (Left, Right : Set) return Set;
   function Intersection (Left, Right : Set) return Set;
   function Difference (Left, Right : Set) return Set;

   function Is_Subset (Left, Right : Set) return Boolean;

   -- Iteration
   type Cursor is private;

   function First (S : Set) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Has_Element (Position : Cursor) return Boolean;
   function Element (Position : Cursor) return Element_Type;

private
   Max_Elements : constant := 100;

   type Element_Array is array (1 .. Max_Elements) of Element_Type;

   type Set is record
      Elements : Element_Array;
      Count    : Natural := 0;
   end record;

   Empty_Set : constant Set := (Elements => <>, Count => 0);

   type Cursor is record
      Container : access Set;
      Index     : Natural := 0;
   end record;

end GNAT.Sets;
