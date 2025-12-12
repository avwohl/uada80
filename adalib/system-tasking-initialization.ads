-- System.Tasking.Initialization for Z80
-- Runtime initialization for tasking

with System.Task_Primitives;

package System.Tasking.Initialization is
   pragma Preelaborate;

   -- Initialize the tasking runtime
   procedure Initialize_Runtime;

   -- Finalize the tasking runtime
   procedure Finalize_Runtime;

   -- Check if runtime is initialized
   function Runtime_Initialized return Boolean;

   -- Initialize the environment task (main program)
   procedure Initialize_Environment_Task;

   -- Called at program termination
   procedure Finalize_Environment_Task;

   -- Abort deferral (for protected regions)
   procedure Defer_Abort;
   procedure Undefer_Abort;
   function Abort_Deferred return Boolean;

   -- Task termination hooks
   type Termination_Handler is access procedure (T : Task_Id);

   procedure Set_Termination_Handler (Handler : Termination_Handler);
   procedure Call_Termination_Handler (T : Task_Id);

end System.Tasking.Initialization;
