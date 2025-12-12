-- System.Secondary_Stack for Z80
-- Secondary stack management

package System.Secondary_Stack is
   pragma Preelaborate;

   -- Default secondary stack size (limited on Z80)
   Default_Secondary_Stack_Size : constant := 256;

   -- Mark for secondary stack (for cleanup)
   type SS_Mark is private;

   -- Get current mark
   function SS_Mark return SS_Mark;

   -- Release to mark
   procedure SS_Release (M : SS_Mark);

   -- Allocate from secondary stack
   function SS_Allocate (Size : Natural) return System.Address;

   -- Get current usage
   function SS_Get_Max return Natural;

private

   type SS_Mark is new Natural;

end System.Secondary_Stack;
