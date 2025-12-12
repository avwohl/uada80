-- System.Tasking.Utilities for Z80
-- Tasking utilities

with System.Tasking;

package System.Tasking.Utilities is
   pragma Preelaborate;

   function Get_Current_Task return Task_Id;
   --  Return current task ID

   function Get_Task_Name (T : Task_Id) return String;
   --  Return task name

   function Get_Task_Priority (T : Task_Id) return Integer;
   --  Return task priority

   procedure Set_Task_Priority
     (T : Task_Id;
      Priority : Integer);
   --  Set task priority

   function Get_Task_State (T : Task_Id) return Task_State;
   --  Return task state

   procedure Yield;
   --  Yield to other tasks

   procedure Sleep (Duration_MS : Natural);
   --  Sleep for specified milliseconds

end System.Tasking.Utilities;
