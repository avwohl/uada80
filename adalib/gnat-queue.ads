-- GNAT.Queue for Z80
-- Generic queue data structure

package GNAT.Queue is
   pragma Pure;

   generic
      type Element_Type is private;
      Max_Size : Positive := 64;  -- Default max size for Z80
   package Bounded is

      type Queue is limited private;

      procedure Clear (Q : out Queue);
      --  Clear the queue

      function Is_Empty (Q : Queue) return Boolean;
      --  Check if queue is empty

      function Is_Full (Q : Queue) return Boolean;
      --  Check if queue is full

      function Size (Q : Queue) return Natural;
      --  Return current size

      procedure Enqueue (Q : in Out Queue; Item : Element_Type);
      --  Add item to queue

      procedure Dequeue (Q : in Out Queue; Item : out Element_Type);
      --  Remove item from queue

      function Front (Q : Queue) return Element_Type;
      --  Return front item without removing

      function Back (Q : Queue) return Element_Type;
      --  Return back item without removing

   private

      type Element_Array is array (1 .. Max_Size) of Element_Type;

      type Queue is record
         Data  : Element_Array;
         Head  : Natural := 1;
         Tail  : Natural := 0;
         Count : Natural := 0;
      end record;

   end Bounded;

end GNAT.Queue;
