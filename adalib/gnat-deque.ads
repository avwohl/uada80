-- GNAT.Deque for Z80
-- Double-ended queue (deque) implementation

package GNAT.Deque is
   pragma Pure;

   Max_Size : constant := 64;  -- Maximum deque size for Z80

   type Deque is private;

   procedure Initialize (D : out Deque);
   --  Initialize empty deque

   function Is_Empty (D : Deque) return Boolean;
   --  Check if deque is empty

   function Is_Full (D : Deque) return Boolean;
   --  Check if deque is full

   function Size (D : Deque) return Natural;
   --  Return number of elements

   function Front (D : Deque) return Integer;
   --  Get front element (0 if empty)

   function Back (D : Deque) return Integer;
   --  Get back element (0 if empty)

   procedure Push_Front (D : in Out Deque; Value : Integer);
   --  Add to front

   procedure Push_Back (D : in Out Deque; Value : Integer);
   --  Add to back

   function Pop_Front (D : in Out Deque) return Integer;
   --  Remove and return front element (0 if empty)

   function Pop_Back (D : in Out Deque) return Integer;
   --  Remove and return back element (0 if empty)

   procedure Clear (D : out Deque);
   --  Clear all elements

   function Element (D : Deque; Index : Positive) return Integer;
   --  Get element at index (1 = front, 0 if invalid)

   function Contains (D : Deque; Value : Integer) return Boolean;
   --  Check if value exists

   procedure Reverse_Deque (D : in Out Deque);
   --  Reverse elements in place

private

   type Deque_Array is array (1 .. Max_Size) of Integer;

   type Deque is record
      Data  : Deque_Array := (others => 0);
      Head  : Positive := 1;  -- Index of front element
      Tail  : Natural := 0;   -- Index of back element (0 = empty)
      Count : Natural := 0;
   end record;

end GNAT.Deque;
