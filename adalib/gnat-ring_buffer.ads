-- GNAT.Ring_Buffer for Z80
-- Generic circular buffer

package GNAT.Ring_Buffer is
   pragma Pure;

   generic
      type Element_Type is private;
      Max_Size : Positive := 64;
   package Bounded is

      type Ring_Buffer is limited private;

      procedure Clear (B : out Ring_Buffer);
      --  Clear the buffer

      function Is_Empty (B : Ring_Buffer) return Boolean;
      --  Check if buffer is empty

      function Is_Full (B : Ring_Buffer) return Boolean;
      --  Check if buffer is full

      function Size (B : Ring_Buffer) return Natural;
      --  Return current size

      function Capacity (B : Ring_Buffer) return Natural;
      --  Return maximum capacity

      procedure Write (B : in Out Ring_Buffer; Item : Element_Type);
      --  Write item to buffer (overwrites oldest if full)

      procedure Read (B : in Out Ring_Buffer; Item : out Element_Type);
      --  Read oldest item from buffer

      function Peek (B : Ring_Buffer; Index : Positive := 1) return Element_Type;
      --  Peek at item (1 = oldest)

   private

      type Element_Array is array (0 .. Max_Size - 1) of Element_Type;

      type Ring_Buffer is record
         Data     : Element_Array;
         Read_Idx : Natural := 0;
         Write_Idx : Natural := 0;
         Count    : Natural := 0;
      end record;

   end Bounded;

end GNAT.Ring_Buffer;
