-- System.Tasking.Debug for Z80
-- Tasking debug and diagnostic support

package System.Tasking.Debug is
   pragma Preelaborate;

   -- Task information record
   type Task_Info is record
      Id       : Task_Id;
      Priority : Task_Priority;
      State    : Task_State;
      Stage    : Stages.Activation_Stage;
   end record;

   -- Get information about all tasks
   procedure Get_All_Tasks
     (Info  : out array (Task_Id range <>) of Task_Info;
      Count : out Natural);

   -- Get information about a specific task
   function Get_Task_Info (T : Task_Id) return Task_Info;

   -- Debug output (sends to console if available)
   procedure Print_Task_State (T : Task_Id);
   procedure Print_All_Tasks;

   -- Statistics
   function Context_Switch_Count return Natural;
   function Timer_Tick_Count return Natural;

   -- Breakpoint support (for debugging)
   procedure Task_Breakpoint (T : Task_Id);

   -- Trace control
   Trace_Enabled : Boolean := False;
   procedure Enable_Trace;
   procedure Disable_Trace;

end System.Tasking.Debug;
