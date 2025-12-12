-- System.Elaboration_Allocators for Z80
-- Memory allocators for elaboration time

package System.Elaboration_Allocators is
   pragma Preelaborate;

   -- Static allocation area for elaboration
   Elaboration_Pool_Size : constant := 1024;

   -- Allocate memory during elaboration
   function Allocate (Size : Natural) return System.Address;

   -- Get amount of memory used
   function Memory_Used return Natural;

   -- Get amount of memory remaining
   function Memory_Remaining return Natural;

   -- Reset allocator (for testing)
   procedure Reset;

end System.Elaboration_Allocators;
