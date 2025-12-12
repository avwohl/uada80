-- GNAT.Stack for Z80
-- Generic stack data structure

package GNAT.Stack is
   pragma Pure;

   generic
      type Element_Type is private;
      Max_Size : Positive := 64;  -- Default max size for Z80
   package Bounded is

      type Stack is limited private;

      procedure Clear (S : out Stack);
      --  Clear the stack

      function Is_Empty (S : Stack) return Boolean;
      --  Check if stack is empty

      function Is_Full (S : Stack) return Boolean;
      --  Check if stack is full

      function Size (S : Stack) return Natural;
      --  Return current size

      procedure Push (S : in Out Stack; Item : Element_Type);
      --  Push item onto stack

      procedure Pop (S : in Out Stack; Item : out Element_Type);
      --  Pop item from stack

      function Top (S : Stack) return Element_Type;
      --  Return top item without removing

      function Peek (S : Stack; Depth : Positive := 1) return Element_Type;
      --  Return item at given depth (1 = top)

   private

      type Element_Array is array (1 .. Max_Size) of Element_Type;

      type Stack is record
         Data  : Element_Array;
         Count : Natural := 0;
      end record;

   end Bounded;

end GNAT.Stack;
