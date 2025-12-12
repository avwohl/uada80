-- GNAT.Task_Lock for Z80
-- Simple task locking

package GNAT.Task_Lock is
   pragma Preelaborate;

   -- Acquire global task lock
   procedure Lock;

   -- Release global task lock
   procedure Unlock;

end GNAT.Task_Lock;
