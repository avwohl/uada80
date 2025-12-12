-- GNAT.Priority_Queue for Z80
-- Generic priority queue (min-heap)

package GNAT.Priority_Queue is
   pragma Pure;

   generic
      type Element_Type is private;
      type Priority_Type is (<>);
      Max_Size : Positive := 32;
      with function Priority (E : Element_Type) return Priority_Type;
   package Bounded is

      type Priority_Queue is limited private;

      procedure Clear (Q : out Priority_Queue);
      --  Clear the queue

      function Is_Empty (Q : Priority_Queue) return Boolean;
      --  Check if queue is empty

      function Is_Full (Q : Priority_Queue) return Boolean;
      --  Check if queue is full

      function Size (Q : Priority_Queue) return Natural;
      --  Return current size

      procedure Enqueue (Q : in Out Priority_Queue; Item : Element_Type);
      --  Add item to queue

      procedure Dequeue (Q : in Out Priority_Queue; Item : out Element_Type);
      --  Remove highest priority item

      function First (Q : Priority_Queue) return Element_Type;
      --  Return highest priority item without removing

   private

      type Element_Array is array (1 .. Max_Size) of Element_Type;

      type Priority_Queue is record
         Data  : Element_Array;
         Count : Natural := 0;
      end record;

   end Bounded;

end GNAT.Priority_Queue;
