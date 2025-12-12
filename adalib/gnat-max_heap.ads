-- GNAT.Max_Heap for Z80
-- Binary max-heap priority queue

package GNAT.Max_Heap is
   pragma Pure;

   Max_Size : constant := 32;  -- Maximum heap size for Z80

   type Heap is private;

   procedure Initialize (H : out Heap);
   --  Initialize empty heap

   function Is_Empty (H : Heap) return Boolean;
   --  Check if heap is empty

   function Is_Full (H : Heap) return Boolean;
   --  Check if heap is full

   function Size (H : Heap) return Natural;
   --  Return number of elements

   function Peek (H : Heap) return Integer;
   --  Return maximum element without removing (0 if empty)

   procedure Push (H : in Out Heap; Value : Integer);
   --  Add element to heap

   function Pop (H : in Out Heap) return Integer;
   --  Remove and return maximum element (0 if empty)

   procedure Clear (H : out Heap);
   --  Clear all elements

   function Contains (H : Heap; Value : Integer) return Boolean;
   --  Check if value exists in heap

   -- Array operations
   type Integer_Array is array (Positive range <>) of Integer;

   procedure Build_Heap (H : out Heap; Values : Integer_Array);
   --  Build heap from array

   procedure Heap_Sort_Descending (Values : in Out Integer_Array);
   --  Sort array using heap sort (descending order)

private

   type Heap_Array is array (1 .. Max_Size) of Integer;

   type Heap is record
      Data  : Heap_Array := (others => 0);
      Count : Natural := 0;
   end record;

end GNAT.Max_Heap;
