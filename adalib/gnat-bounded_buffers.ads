-- GNAT.Bounded_Buffers for Z80
-- Bounded buffer for inter-task communication

generic
   type Element is private;
package GNAT.Bounded_Buffers is
   pragma Preelaborate;

   type Bounded_Buffer (Size : Positive) is limited private;

   -- Insert element (blocks if full)
   procedure Insert (B : in Out Bounded_Buffer; Item : Element);

   -- Remove element (blocks if empty)
   procedure Remove (B : in Out Bounded_Buffer; Item : out Element);

   -- Non-blocking versions
   procedure Try_Insert
     (B       : in Out Bounded_Buffer;
      Item    : Element;
      Success : out Boolean);

   procedure Try_Remove
     (B       : in Out Bounded_Buffer;
      Item    : out Element;
      Success : out Boolean);

   -- Query
   function Count (B : Bounded_Buffer) return Natural;
   function Is_Empty (B : Bounded_Buffer) return Boolean;
   function Is_Full (B : Bounded_Buffer) return Boolean;

private

   type Element_Array is array (Positive range <>) of Element;

   type Bounded_Buffer (Size : Positive) is limited record
      Buffer : Element_Array (1 .. Size);
      Head   : Positive := 1;
      Tail   : Positive := 1;
      Count  : Natural := 0;
   end record;

end GNAT.Bounded_Buffers;
