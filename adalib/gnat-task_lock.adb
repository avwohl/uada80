-- GNAT.Task_Lock body for Z80
-- Simple task locking implementation

with System.Tasking;

package body GNAT.Task_Lock is

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      System.Tasking.Enter_Protected;
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      System.Tasking.Leave_Protected;
   end Unlock;

end GNAT.Task_Lock;
