-- System.Return_Stack for Z80
-- Secondary stack for function return values

package System.Return_Stack is
   pragma Preelaborate;

   -- Maximum secondary stack size
   Max_Stack_Size : constant := 512;

   -- Push data onto secondary stack
   procedure Push (Data : System.Address; Size : Natural);

   -- Pop data from secondary stack
   procedure Pop (Data : System.Address; Size : Natural);

   -- Allocate space on secondary stack, return address
   function Allocate (Size : Natural) return System.Address;

   -- Release space from secondary stack
   procedure Release (Size : Natural);

   -- Get current stack pointer
   function Get_Pointer return Natural;

   -- Set stack pointer (for block exit)
   procedure Set_Pointer (Ptr : Natural);

   -- Mark current stack level
   function Mark return Natural renames Get_Pointer;

   -- Release to marked level
   procedure Release_To_Mark (Mark : Natural);

end System.Return_Stack;
